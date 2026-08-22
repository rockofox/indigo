# BytecodeCompiler Refactor Design

**Date:** 2026-04-02
**Status:** Approved

## Context

`lib/BytecodeCompiler.hs` has grown to 2,096 lines, with a single `compileExpr` function spanning 954 lines and 88 pattern-match clauses. Every new language feature (generics, traits, refinement types, modules) has been added in-place. The file is hard to navigate, hard to extend, and tightly couples type inference, code generation, name resolution, and module loading.

The goal is to split it into focused submodules by expression category, clean up the most egregious code smells, and keep the public API identical so callers (`app/Main.hs`, tests) need no changes.

---

## Module Structure

```
lib/
  BytecodeCompiler.hs       -- thin orchestrator (~100 lines), re-exports everything
  Compiler/
    State.hs                -- CompilerState, Function, Let, CompilerError, cerror, initState*
    Types.hs                -- typeOf, safeTypeOf, typeCompatible, functionTypesAcceptable
    Literals.hs             -- IntLit, FloatLit, DoubleLit, StringLit, BoolLit, CharLit
    Functions.hs            -- FuncDef, FuncDec, FuncCall, Lambda, unsafe builtins
    Structs.hs              -- Struct, StructLit, StructAccess, TupleLit, TupleAccess
    Traits.hs               -- Trait, Impl, method resolution, functionsByTrait
    ControlFlow.hs          -- If, When, DoBlock, Pipeline, Let, variable shadowing
    Lists.hs                -- ListLit, ListAdd, ListConcat, ListPattern, ArrayAccess
    Modules.hs              -- Import, mangleExprForImport, module loading, buildModuleMap
    Refinements.hs          -- transformRefinementExpr, runRefinement, refinement checking
```

`BytecodeCompiler.hs` defines `compileExpr` as a dispatch table calling into submodule functions, then re-exports all types and functions for backward compatibility.

---

## Recursion Strategy

`compileExpr` is recursive and needed by nearly every submodule. To avoid circular imports, each submodule function receives it as a callback parameter.

```haskell
-- Shared type aliases (defined in Compiler.State)
type Compiler a   = StateT (CompilerState IO) IO a
type CompileExpr  = Parser.Expr -> Parser.Type -> Compiler [Instruction]

-- Example submodule signature
compileIf :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
```

### Import Graph (no cycles)

```
BytecodeCompiler   →  Compiler.{Literals, Functions, Structs, Traits, ControlFlow, Lists, Modules, Refinements}
                   →  Compiler.Types
                   →  Compiler.State
Compiler.Functions →  Compiler.{State, Types, Refinements}
Compiler.Structs   →  Compiler.{State, Types, Refinements}
Compiler.Traits    →  Compiler.{State, Types}
Compiler.ControlFlow → Compiler.{State, Types}
Compiler.Lists     →  Compiler.{State, Types}
Compiler.Modules   →  Compiler.State
Compiler.Refinements → Compiler.{State, Types}
Compiler.Types     →  Compiler.State
Compiler.State     →  (no project imports)
```

---

## Cleanup Items

These code improvements are bundled with the structural split.

### 1. Unsafe Builtins → Lookup Table (`Compiler/Functions.hs`)

Replace ~26 identical pattern-match clauses with a `Map`:

```haskell
unsafeBuiltins :: Map String [Instruction]
unsafeBuiltins = Map.fromList
  [ ("unsafeAdd", [Add]), ("unsafeSub", [Sub]), ("unsafeMul", [Mul])
  , ("unsafeDiv", [Div]), ("unsafeMod", [Mod]), ("unsafePow", [Pow])
  , ("unsafeGt",  [Gt]),  ("unsafeLt",  [Lt]),  ("unsafeGe",  [Ge])
  , ("unsafeLe",  [Le]),  ("unsafeEq",  [Eq]),  ("unsafeNeq", [Neq])
  , ("unsafeAnd", [And]), ("unsafeOr",  [Or])
  , ...
  ]
```

`compileFuncCall` checks this map first before entering the main resolution logic.

### 2. Shadow/Restore → Shared Helper (`Compiler/ControlFlow.hs`)

Both `compileDoBlock` and `compileWhen` contain identical logic to save and restore shadowed variables. Extract to:

```haskell
withShadowedVars
  :: [String]               -- variable names that may be shadowed
  -> Compiler [Instruction] -- body that may bind those names
  -> Compiler [Instruction] -- save instrs ++ body ++ restore instrs
```

### 3. Binary Op Helpers → Unified

`compileBinOp`, `compileBinOpSeq`, and `doBinOp` are three closely related functions with overlapping logic. Merge into a single `compileBinOp` with clear internal structure, eliminating the intermediate `compileBinOpSeq` indirection. The merged function must produce identical instructions to the current three — this is a structural cleanup, not a behavioral change.

### 4. `mangleExprForImport` → Use `traverseExprChildren`

The function has 26 manual pattern-match cases that replicate AST traversal. Replace the pass-through cases with `AST.traverseExprChildren` (added in commit `34535a7`), keeping only the cases that actually transform nodes.

### 5. `CompilerState` Context Fields → `contextStack`

`currentContext :: String` and `contextPath :: [String]` carry redundant information and must be kept in sync manually. Replace with:

```haskell
contextStack :: NonEmpty String
```

`currentContext` becomes a smart accessor `head contextStack`. One field, one invariant.

### 6. Error Handling — Mark Internal Errors

Replace bare `error "..."` calls (used for impossible states / compiler bugs) with a dedicated helper:

```haskell
internalError :: String -> a
internalError msg = errorWithoutStackTrace ("[BUG] " ++ msg)
```

User-facing errors continue to use `cerror`. This makes the distinction explicit in the code.

---

## Public API

`BytecodeCompiler.hs` re-exports everything currently exported. The module remains `module BytecodeCompiler where` (export-all). `app/Main.hs` and all tests need zero changes.

---

## Cabal Changes

Add to `other-modules` in the `library` stanza of `indigo.cabal`:

```cabal
other-modules:
  Compiler.State
  Compiler.Types
  Compiler.Literals
  Compiler.Functions
  Compiler.Structs
  Compiler.Traits
  Compiler.ControlFlow
  Compiler.Lists
  Compiler.Modules
  Compiler.Refinements
```

---

## Verification

1. `cabal build --ghc-options=-Werror` — no warnings, no missing patterns
2. `cabal test indigo-test` — all existing tests pass
3. `fourmolu --mode check .` — formatting clean
4. `hlint lib/ app/` — no lint issues
5. `cabal run indigo -- examples/fib.in` — end-to-end smoke test passes

No new tests are required. The existing suite is the correctness oracle for a structural refactor.
