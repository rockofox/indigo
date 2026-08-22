# SMT-Based Refinement Types

**Branch:** `feature/smt-refinements`
**Status:** In development

## Overview

Refinement types attach logical predicates to type definitions. The compiler verifies
these predicates at compile time so invalid values can never be constructed. The
original implementation ran a mini-interpreter over literal values only. This spec
describes the upgrade to SMT-based (Z3) verification, which extends coverage to
variables, arithmetic, and values flowing through branches.

## Syntax

```
type Age = (value: Int) satisfies (value >= 0)
```

The `satisfies` clause is a boolean expression that may reference the type's field
names directly. At a struct literal or cast site the compiler verifies the predicate
holds for the given field values.

## Verification Strategy

The compiler tries two strategies in order:

1. **SMT path** — translate the predicate and field values into an `SBV` symbolic
   query and ask Z3. This handles variables, arithmetic, and path conditions
   collected from surrounding `if`/`when` branches.
2. **Interpreter fallback** — if the SMT translation fails (e.g. unsupported
   expression), run the old mini-interpreter. This still covers fully-concrete
   literal values.

If neither can decide, the compiler emits an error unless the call site is marked
`unchecked` (see below).

### Verification Conditions

`CompilerState` carries two new fields:

```haskell
refinementVCs  :: [VerificationCondition]
smtAssumptions :: [SMTExpr]
```

As the compiler descends into struct literals and casts it calls `emitVC` to append
a `VerificationCondition`. After the full compilation pass, `dischargeVCs` sends
all accumulated VCs to Z3 in one batch. Failures are translated back to ordinary
`CompilerError` values and reported through the normal error renderer (including
LSP diagnostics).

### SMT Expression Language (`Compiler.SMT.Constraints`)

```haskell
data SMTSort = SortInt | SortBool | SortString | SortFloat

data SMTExpr
    = SMTVar   String SMTSort
    | SMTLit   SMTLiteral
    | SMTBinOp SMTBinOp SMTExpr SMTExpr
    | SMTUnary SMTUnaryOp SMTExpr
    | SMTLen   SMTExpr          -- len(s) for strings
    | SMTSize  SMTExpr          -- size(c) for collections

data VerificationCondition = VerificationCondition
    { vcConstraint      :: SMTExpr
    , vcFieldEnv        :: Map String SMTExpr   -- field name → value expression
    , vcAssumptions     :: [SMTExpr]            -- path conditions in scope
    , vcSourcePos       :: Position
    , vcRefinementSrc   :: String               -- original source text for errors
    , vcUnchecked       :: Bool
    , vcHasSymbolicVars :: Bool
    }
```

`Compiler.SMT` is compiled conditionally: the `SMT_ENABLED` CPP flag (set when
`sbv` and `z3` are available) gates the real SBV calls. Without it `dischargeVCs`
is a no-op, so the compiler still builds and the WASM playground target falls back
to interpreter-only checks.

### Path Conditions

When the SMT path processes a struct literal inside an `if`/`when` branch it can
use the branch condition as an assumption. Example:

```indigo
type Age = (value: Int) satisfies (value >= 0)

let clamp (n: Int) : Age =
    if n >= 0 then
        Age{value: n}   -- solver assumes n >= 0, so value >= 0 is provable
    else
        Age{value: 0}   -- concrete 0 >= 0 is trivially true
    end
```

Without path conditions the solver would treat `n` as an unconstrained symbolic
integer and the check would fail.

## `unchecked` Keyword

When a value comes from user input or another runtime source the solver cannot
prove the constraint statically. Use `unchecked` to opt in to a runtime check
instead:

```indigo
type Age = (value: Int) satisfies (value >= 0)

let main : IO<Unit> = do
    let raw = unsafeGetLine
    let n   = raw as Int
    let age = unchecked Age{value: n}
    println age.value
end
```

If the constraint is violated at runtime the program exits with an error. Using
`unchecked` without a genuine reason (i.e. when the value is actually a literal)
is a compiler warning.

## Value Types and Casts

A `value type` has exactly one field and can be constructed by casting any
compatible value. The cast is verified by the SMT solver:

```indigo
value type EvenNumber = (num: Int) satisfies ((num % 2) == 0)

let main : IO<Unit> = do
    println (12 as EvenNumber)   -- valid, Z3 proves 12 % 2 == 0
    # println (5 as EvenNumber)  -- error: Refinement failed ((num % 2) == 0)
end
```

## Supported Predicate Operations

| Category       | Operators / functions               |
|----------------|-------------------------------------|
| Arithmetic     | `+` `-` `*` `/` `%`                 |
| Comparison     | `==` `!=` `<` `<=` `>` `>=`         |
| Boolean        | `&&` `\|\|` `!`                     |
| String         | `len s` (character count)           |
| Collections    | `size c`                            |
| Path conds     | `if`/`when` branch guards           |

## Trait Refinements

`Trait` definitions may also carry a `satisfies` clause and `requiredProperties`.
Structs implementing the trait must satisfy the trait's refinement in addition to
their own. This is enforced in `Compiler.Traits` during impl resolution.

## Module Layout

```
lib/Compiler/
  Refinements.hs          -- transformRefinementExpr, runRefinement, extractLets
  SMT.hs                  -- dischargeVCs, exprToSMT (CPP-gated SBV calls)
  SMT/
    Constraints.hs        -- SMTExpr, SMTSort, VerificationCondition
  Structs.hs              -- calls exprToSMT / emitVC at struct-literal and cast sites
  State.hs                -- refinementVCs, smtAssumptions fields; emitVC helper
```

## Build Dependencies

`indigo.cabal` conditionally depends on `sbv` (the Haskell SBV binding). The
`flake.nix` adds `z3` to the dev shell when the flag is enabled. The WASM reactor
build explicitly disables `SMT_ENABLED`.

## Examples

| File | What it demonstrates |
|------|----------------------|
| `examples/refinements_basic.in` | `Age`, `Score`, `NonEmptyString` with `len` |
| `examples/refinements_subtyping.in` | Multi-field constraint, `BoundedPair` |
| `examples/refinements_strings.in` | String equality and length constraints |
| `examples/refinements_variables.in` | Variables bound by `let`, resolved at compile time |
| `examples/refinements_unchecked.in` | Runtime fallback with `unchecked` |
| `examples/refinements_failures.in` | Commented-out failing cases for each error type |
