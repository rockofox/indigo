module Documentation exposing (Topic(..), topicPath, topicTitle, topicCategory, view)


import Html exposing (Html, a, button, div, h2, h3, input, node, p, pre, code, section, span, text)
import Html.Attributes exposing (attribute, class, href, id)
import Html.Events exposing (onClick, onInput)


type Topic
    = Overview
    | FunctionsAndBindings
    | Comments
    | LiteralsAndTypes
    | OperatorsAndPipelines
    | ControlFlow
    | LambdasAndPartials
    | Lists
    | Tuples
    | ProductTypes
    | SumTypes
    | TraitsAndImpls
    | Generics
    | CastingAndValueTypes
    | RefinementTypes
    | PatternMatching
    | IOAndSequencing
    | ImportsAndModules
    | FFI
    | Prelude
    | StdFile
    | ProjectConfig


topicPath : Topic -> String
topicPath topic =
    case topic of
        Overview ->
            "/docs/overview"

        FunctionsAndBindings ->
            "/docs/functions-and-bindings"

        Comments ->
            "/docs/comments"

        LiteralsAndTypes ->
            "/docs/literals-and-types"

        OperatorsAndPipelines ->
            "/docs/operators-and-pipelines"

        ControlFlow ->
            "/docs/control-flow"

        LambdasAndPartials ->
            "/docs/lambdas-and-partials"

        Lists ->
            "/docs/lists"

        Tuples ->
            "/docs/tuples"

        ProductTypes ->
            "/docs/product-types"

        SumTypes ->
            "/docs/sum-types"

        TraitsAndImpls ->
            "/docs/traits-and-impls"

        Generics ->
            "/docs/generics"

        CastingAndValueTypes ->
            "/docs/casting-and-value-types"

        RefinementTypes ->
            "/docs/refinement-types"

        PatternMatching ->
            "/docs/pattern-matching"

        IOAndSequencing ->
            "/docs/io-and-sequencing"

        ImportsAndModules ->
            "/docs/imports-and-modules"

        FFI ->
            "/docs/ffi"

        Prelude ->
            "/docs/prelude"

        StdFile ->
            "/docs/stdlib-file"

        ProjectConfig ->
            "/docs/project-config"


topicTitle : Topic -> String
topicTitle topic =
    case topic of
        Overview ->
            "Overview"

        FunctionsAndBindings ->
            "Functions & Bindings"

        Comments ->
            "Comments"

        LiteralsAndTypes ->
            "Literals & Types"

        OperatorsAndPipelines ->
            "Operators & Pipelines"

        ControlFlow ->
            "Control Flow"

        LambdasAndPartials ->
            "Lambdas & Partials"

        Lists ->
            "Lists"

        Tuples ->
            "Tuples"

        ProductTypes ->
            "Product Types"

        SumTypes ->
            "Sum Types"

        TraitsAndImpls ->
            "Traits & Impls"

        Generics ->
            "Generics"

        CastingAndValueTypes ->
            "Casting & Value Types"

        RefinementTypes ->
            "Refinement Types"

        PatternMatching ->
            "Pattern Matching"

        IOAndSequencing ->
            "IO & Sequencing"

        ImportsAndModules ->
            "Imports & Modules"

        FFI ->
            "FFI"

        Prelude ->
            "Prelude"

        StdFile ->
            "File"

        ProjectConfig ->
            "Project Config"


topicCategory : Topic -> String
topicCategory topic =
    case topic of
        Overview ->
            "Getting Started"

        FunctionsAndBindings ->
            "Basics"

        Comments ->
            "Basics"

        LiteralsAndTypes ->
            "Basics"

        OperatorsAndPipelines ->
            "Basics"

        ControlFlow ->
            "Basics"

        LambdasAndPartials ->
            "Basics"

        Lists ->
            "Data & Types"

        Tuples ->
            "Data & Types"

        ProductTypes ->
            "Data & Types"

        SumTypes ->
            "Data & Types"

        TraitsAndImpls ->
            "Data & Types"

        Generics ->
            "Data & Types"

        CastingAndValueTypes ->
            "Data & Types"

        RefinementTypes ->
            "Data & Types"

        PatternMatching ->
            "Expressions"

        IOAndSequencing ->
            "Expressions"

        ImportsAndModules ->
            "Modules"

        FFI ->
            "Interop"

        Prelude ->
            "Standard Library"

        StdFile ->
            "Standard Library"

        ProjectConfig ->
            "Tooling"


topicList : List Topic
topicList =
    [ Overview
    , FunctionsAndBindings
    , Comments
    , LiteralsAndTypes
    , OperatorsAndPipelines
    , ControlFlow
    , LambdasAndPartials
    , Lists
    , Tuples
    , ProductTypes
    , SumTypes
    , TraitsAndImpls
    , Generics
    , CastingAndValueTypes
    , RefinementTypes
    , PatternMatching
    , IOAndSequencing
    , ImportsAndModules
    , FFI
    , Prelude
    , StdFile
    , ProjectConfig
    ]


previousTopic : Topic -> Maybe Topic
previousTopic topic =
    let
        findPrev prev items =
            case items of
                [] ->
                    Nothing

                curr :: rest ->
                    if curr == topic then
                        prev

                    else
                        findPrev (Just curr) rest
    in
    findPrev Nothing topicList


nextTopic : Topic -> Maybe Topic
nextTopic topic =
    let
        findNext items =
            case items of
                [] ->
                    Nothing

                curr :: rest ->
                    if curr == topic then
                        List.head rest

                    else
                        findNext rest
    in
    findNext topicList


view : Topic -> String -> (String -> msg) -> (String -> msg) -> Html msg
view currentTopic searchQuery onSearchChange onLoadCode =
    div [ class "docs-container" ]
        [ viewSidebar currentTopic searchQuery onSearchChange
        , div [ class "docs-content" ]
            [ div [ class "docs-header-bar" ]
                [ div [ class "docs-breadcrumb" ]
                    [ a [ href "/docs" ] [ text "Docs" ]
                    , span [ class "breadcrumb-sep" ] [ text "/" ]
                    , span [ class "breadcrumb-category" ] [ text (topicCategory currentTopic) ]
                    , span [ class "breadcrumb-sep" ] [ text "/" ]
                    , span [ class "breadcrumb-current" ] [ text (topicTitle currentTopic) ]
                    ]
                ]
            , case currentTopic of
                Overview ->
                    viewOverview onLoadCode

                FunctionsAndBindings ->
                    viewFunctionsAndBindings onLoadCode

                Comments ->
                    viewComments onLoadCode

                LiteralsAndTypes ->
                    viewLiteralsAndTypes onLoadCode

                OperatorsAndPipelines ->
                    viewOperatorsAndPipelines onLoadCode

                ControlFlow ->
                    viewControlFlow onLoadCode

                LambdasAndPartials ->
                    viewLambdasAndPartials onLoadCode

                Lists ->
                    viewLists onLoadCode

                Tuples ->
                    viewTuples onLoadCode

                ProductTypes ->
                    viewProductTypes onLoadCode

                SumTypes ->
                    viewSumTypes onLoadCode

                TraitsAndImpls ->
                    viewTraitsAndImpls onLoadCode

                Generics ->
                    viewGenerics onLoadCode

                CastingAndValueTypes ->
                    viewCastingAndValueTypes onLoadCode

                RefinementTypes ->
                    viewRefinementTypes onLoadCode

                PatternMatching ->
                    viewPatternMatching onLoadCode

                IOAndSequencing ->
                    viewIOAndSequencing onLoadCode

                ImportsAndModules ->
                    viewImportsAndModules onLoadCode

                FFI ->
                    viewFFI onLoadCode

                Prelude ->
                    viewPrelude onLoadCode

                StdFile ->
                    viewStdFile onLoadCode

                ProjectConfig ->
                    viewProjectConfig onLoadCode
            , viewPagination currentTopic
            ]
        ]


viewSidebar : Topic -> String -> (String -> msg) -> Html msg
viewSidebar currentTopic searchQuery onSearchChange =
    let
        matches query topic =
            if String.isEmpty (String.trim query) then
                True

            else
                let
                    q =
                        String.toLower (String.trim query)

                    title =
                        String.toLower (topicTitle topic)

                    cat =
                        String.toLower (topicCategory topic)
                in
                String.contains q title || String.contains q cat

        renderGroup category topics =
            let
                filtered =
                    List.filter (matches searchQuery) topics
            in
            if List.isEmpty filtered then
                text ""

            else
                div [ class "sidebar-group" ]
                    [ h3 [] [ text category ]
                    , div [ class "sidebar-links" ]
                        (List.map (\topic -> sidebarLink currentTopic topic (topicTitle topic)) filtered)
                    ]
    in
    node "details" [ class "docs-sidebar" ]
        [ node "summary" [ class "docs-sidebar-summary" ]
            [ div [ class "docs-summary-left" ]
                [ span [ class "docs-summary-badge" ] [ text (topicCategory currentTopic) ]
                , span [ class "docs-summary-title" ] [ text (topicTitle currentTopic) ]
                ]
            , span [ class "docs-summary-toggle" ] [ text "Browse Topics ▾" ]
            ]
        , div [ class "docs-sidebar-content" ]
            [ div [ class "docs-search-container" ]
                [ input
                    [ class "docs-search-input"
                    , attribute "placeholder" "Search topics..."
                    , attribute "value" searchQuery
                    , onInput onSearchChange
                    ]
                    []
                ]
            , renderGroup "Getting Started" [ Overview ]
            , renderGroup "Basics" [ FunctionsAndBindings, Comments, LiteralsAndTypes, OperatorsAndPipelines, ControlFlow, LambdasAndPartials ]
            , renderGroup "Data & Types" [ Lists, Tuples, ProductTypes, SumTypes, TraitsAndImpls, Generics, CastingAndValueTypes, RefinementTypes ]
            , renderGroup "Expressions" [ PatternMatching, IOAndSequencing ]
            , renderGroup "Modules & Interop" [ ImportsAndModules, FFI ]
            , renderGroup "Standard Library" [ Prelude, StdFile ]
            , renderGroup "Tooling" [ ProjectConfig ]
            ]
        ]


sidebarLink : Topic -> Topic -> String -> Html msg
sidebarLink currentTopic linkTopic label =
    a
        [ href (topicPath linkTopic)
        , class
            (if currentTopic == linkTopic then
                "active"

             else
                ""
            )
        ]
        [ text label ]


viewPagination : Topic -> Html msg
viewPagination topic =
    let
        prevHtml =
            case previousTopic topic of
                Just prev ->
                    a [ class "pagination-card prev-card", href (topicPath prev) ]
                        [ span [ class "pagination-dir" ] [ text "← Previous" ]
                        , span [ class "pagination-name" ] [ text (topicTitle prev) ]
                        ]

                Nothing ->
                    div [ class "pagination-card-empty" ] []

        nextHtml =
            case nextTopic topic of
                Just next ->
                    a [ class "pagination-card next-card", href (topicPath next) ]
                        [ span [ class "pagination-dir" ] [ text "Next →" ]
                        , span [ class "pagination-name" ] [ text (topicTitle next) ]
                        ]

                Nothing ->
                    div [ class "pagination-card-empty" ] []
    in
    div [ class "docs-pagination" ]
        [ prevHtml
        , nextHtml
        ]


viewCodeBlock : String -> (String -> msg) -> Bool -> String -> Html msg
viewCodeBlock code onLoadCode showTryButton language =
    div [ class "code-block-container" ]
        [ div [ class "code-block-header" ]
            [ span [ class "code-block-lang" ] [ text (if language == "c" then "C / FFI" else if language == "json" then "JSON" else "indigo") ]
            , if showTryButton then
                button
                    [ class "try-button"
                    , onClick (onLoadCode code)
                    , attribute "title" "Open and run in Playground"
                    ]
                    [ span [ class "try-icon" ] [ text "▶" ]
                    , span [ class "try-label" ] [ text "Run in playground" ]
                    ]

              else
                text ""
            ]
        , node "code-editor"
            [ attribute "code" code
            , attribute "read-only" "true"
            , attribute "auto-height" "true"
            , attribute "language" language
            ]
            []
        ]


playgroundNote : String -> Html msg
playgroundNote note =
    p [ class "playground-note" ] [ text note ]



viewOverview : (String -> msg) -> Html msg
viewOverview onLoadCode =
    section [ id "overview" ]
        [ h2 [] [ text "Overview" ]
        , p [] [ text "Indigo is an experimental functional language inspired by F# and Haskell. It compiles to bytecode for a custom VM, with targets for WebAssembly and a language server." ]
        , p [] [ text "Programs are written in .in files. A typical entry point is a main binding that returns IO." ]
        , viewCodeBlock """type Person = (name: String, age: Int)

let greet (p: Person): IO<Unit> = println ("Hello " ++ p.name)

let main: IO<Unit> = do
    let person = Person { name: "Lily", age: 22 }
    greet person
end""" onLoadCode True "indigo"
        , h3 [] [ text "What you will find here" ]
        , p [] [ text "These docs cover the full language surface: bindings, control flow, lists and tuples, product and sum types, traits, generics, casts, refinement types, pattern matching, modules, FFI, the prelude, and project config." ]
        , h3 [] [ text "Quick pointers" ]
        , p [] [ text "Functions use let (and optional :: signatures). Records use type Name = (fields...). Traits use trait / impl. Pattern matching uses when ... of ... end. IO sequences use do ... end and operators like |> , >> , and >>= ." ]
        , p [] [ text "Open any code sample with ▶ to load it in the playground. Features that need native libraries (FFI, File I/O) are documented without a playground run." ]
        ]


viewFunctionsAndBindings : (String -> msg) -> Html msg
viewFunctionsAndBindings onLoadCode =
    section [ id "functions-and-bindings" ]
        [ h2 [] [ text "Functions and bindings" ]
        , p [] [ text "Indigo does not separate constants from functions. Both use let. A binding with parameters is a function; one without is a value. Variable bindings can be untyped or explicitly typed with : Type." ]
        , viewCodeBlock """let name = "Rocko"
let age: Int = 25

let main = do
    println ("Name: " ++ name)
    println ("Age: " ++ ^age)
end""" onLoadCode True "indigo"
        , h3 [] [ text "Parameters and return types" ]
        , p [] [ text "Parameters can be written curried ((a: Int) (b: Int)), grouped with commas ((a: Int, b: Int)), or untyped (a b). Return types after : or -> are optional but recommended for clarity." ]
        , viewCodeBlock """let add_curried (a: Int) (b: Int): Int = a + b
let add_grouped (a: Int, b: Int): Int = a + b
let square x = x * x
let inc (x: Int) -> Int = x + 1

let main = do
    println (add_curried 2, 3)
    println (add_grouped 2, 3)
    println (square 4)
    println (inc 10)
end""" onLoadCode True "indigo"
        , h3 [] [ text "Type signatures" ]
        , p [] [ text "You can declare a signature with :: and implement the body with one or more let / name clauses." ]
        , viewCodeBlock """add :: Int -> Int -> Int
let add x y = x + y

let main = println (add 2, 3)""" onLoadCode True "indigo"
        , h3 [] [ text "Multiple clauses" ]
        , p [] [ text "Functions can be defined with several clauses that pattern-match on arguments." ]
        , viewCodeBlock """let fib (0: Int) : Int = 0
let fib (1: Int) : Int = 1
let fib (n: Int) : Int = (fib ((n) - 1)) + (fib ((n) - 2))

let main = println (fib 10)""" onLoadCode True "indigo"
        , h3 [] [ text "do blocks" ]
        , p [] [ text "Group sequential statements with do ... end. This is how multi-step IO and scoped lets are written." ]
        , viewCodeBlock """let println (s: String): IO<Unit> = do
    print s
    print "\\n"
end

let main = println "hi"
""" onLoadCode True "indigo"
        , h3 [] [ text "Custom operators" ]
        , p [] [ text "Identifiers may include operator characters. Backticks name operators as functions; free operators can also be defined as bindings." ]
        , viewCodeBlock """let double_plus (x: Int y: Int): Int = x + y + y

let main = println (double_plus 2, 3)""" onLoadCode True "indigo"
        , p [] [ text "Operators such as +, ==, and ++ are ordinary bindings in the prelude (often spelled with backticks when the name is pure operator characters). You can define similarly named helpers for domain-specific arithmetic." ]
        ]


viewComments : (String -> msg) -> Html msg
viewComments onLoadCode =
    section [ id "comments" ]
        [ h2 [] [ text "Comments" ]
        , p [] [ text "Line comments start with #. Block comments use /* */." ]
        , viewCodeBlock """# This is a single-line comment
/* This
 * is a multi-line
 * comment
 */

let main = println "ok"
""" onLoadCode True "indigo"
        ]


viewLiteralsAndTypes : (String -> msg) -> Html msg
viewLiteralsAndTypes onLoadCode =
    section [ id "literals-and-types" ]
        [ h2 [] [ text "Literals and primitive types" ]
        , p [] [ text "Indigo has integers, floats, doubles, characters, booleans, and strings (lists of characters)." ]
        , h3 [] [ text "Numbers" ]
        , viewCodeBlock """let main = do
    println 42
    println 3.14
    println 2.5d
end""" onLoadCode True "indigo"
        , p [] [ text "Integer literals are Int. A trailing f marks Float; a trailing d marks Double. Bare decimals are Float." ]
        , h3 [] [ text "Booleans" ]
        , viewCodeBlock """let main = do
    println True
    println False
end""" onLoadCode True "indigo"
        , h3 [] [ text "Characters and strings" ]
        , p [] [ text "Characters use single quotes or an integer with a c suffix. Strings use double quotes and are lists of Char." ]
        , viewCodeBlock """let main = do
    println 'A'
    println 65c
    println "hello"
end""" onLoadCode True "indigo"
        , h3 [] [ text "Built-in type names" ]
        , p [] [ text "Common base types include Int, Float, Double, Bool, Char, String, Any, Unit, and IO (often written IO or IO<T>). Trait constraints and generics appear elsewhere in these docs." ]
        , h3 [] [ text "Unit" ]
        , p [] [ text "Unit is the empty product: type Unit = (). Value Unit{} (or related constructors from the prelude) represents \"no data\"." ]
        ]


viewOperatorsAndPipelines : (String -> msg) -> Html msg
viewOperatorsAndPipelines onLoadCode =
    section [ id "operators-and-pipelines" ]
        [ h2 [] [ text "Operators and pipelines" ]
        , p [] [ text "Most operators are ordinary functions in the prelude. Precedence follows a built-in operator table." ]
        , h3 [] [ text "Arithmetic and comparison" ]
        , viewCodeBlock """let main = do
    println 1 + 2 * 3
    println 10 % 3
    println 2 ** 8
    println 3 < 5
    println 3 != 4
end""" onLoadCode True "indigo"
        , h3 [] [ text "Boolean logic" ]
        , viewCodeBlock """let main = do
    println True && False
    println True || False
    println !False
end""" onLoadCode True "indigo"
        , h3 [] [ text "Lists" ]
        , p [] [ text ": conses an element onto a list. ++ concatenates list-like values (including strings)." ]
        , viewCodeBlock """let main = do
    println 1 : [2, 3]
    println [1, 2] ++ [3, 4]
    println "hi" ++ "!"
end""" onLoadCode True "indigo"
        , h3 [] [ text "Pipeline" ]
        , p [] [ text "|> pipes a value into a function as its first argument." ]
        , viewCodeBlock """let double (x: Int): Int = x * 2

let main = println (5 |> double |> double)""" onLoadCode True "indigo"
        , h3 [] [ text "Flexible application" ]
        , p [] [ text "Prefix ^ flexible-evaluates an expression in contexts that otherwise expect a different form (for example string conversion sites)." ]
        , viewCodeBlock """let main = println (^42 ++ " is the answer")
""" onLoadCode True "indigo"
        , h3 [] [ text "Strict markers" ]
        , p [] [ text "Prefix $ forces strict evaluation of its operand." ]
        , viewCodeBlock """let main = do
    let n = $12
    println n
end""" onLoadCode True "indigo"
        , h3 [] [ text "Infix operators" ]
        , p [] [ text "Binary operators from the prelude (+, ==, &&, and friends) parse as infix with standard precedence." ]
        , viewCodeBlock """let main = do
    println (2 + 4)
    println (3 == 3)
    println (True && False)
end""" onLoadCode True "indigo"
        , h3 [] [ text "Monadic operators" ]
        , p [] [ text ">> sequences two monadic actions; >>= binds. See IO & Sequencing and the prelude Monad trait." ]
        ]


viewControlFlow : (String -> msg) -> Html msg
viewControlFlow onLoadCode =
    section [ id "control-flow" ]
        [ h2 [] [ text "Control flow" ]
        , h3 [] [ text "if then else" ]
        , p [] [ text "Conditionals use if condition then ... else ... . Branches can be single expressions or do blocks." ]
        , viewCodeBlock """let abs (n: Int): Int =
    if n < 0 then (0 - n) else n

let main = println (abs (0 - 7))""" onLoadCode True "indigo"
        , viewCodeBlock """let bottles (i: Int) : IO<Unit> = do
    if i > 0 then do
        println ^i ++ " bottles"
        bottles ((i) - 1)
    else do
        println "done"
    end
end

let main : IO<Unit> = bottles 3""" onLoadCode True "indigo"
        , h3 [] [ text "do ... end" ]
        , p [] [ text "do blocks sequence expressions. Local let bindings are scoped to the block. do is also used for trait/impl/external bodies." ]
        , viewCodeBlock """let main : IO<Unit> = do
    let x = 1
    let y = 2
    println (x + y)
end""" onLoadCode True "indigo"
        , h3 [] [ text "when expressions" ]
        , p [] [ text "Branching on patterns uses when value of ... end. Full details are under Pattern Matching." ]
        , viewCodeBlock """let label (n: Int): String = when n of
    0 -> "zero"
    1 -> "one"
    else -> "many"
end

let main = println (label 1)""" onLoadCode True "indigo"
        ]


viewLambdasAndPartials : (String -> msg) -> Html msg
viewLambdasAndPartials onLoadCode =
    section [ id "lambdas-and-partials" ]
        [ h2 [] [ text "Lambdas and partials" ]
        , h3 [] [ text "Lambda syntax" ]
        , p [] [ text "Anonymous functions use a backslash, argument patterns, ->, and a body." ]
        , viewCodeBlock """let main = do
    println (map (\\x -> x * 2), [1, 2, 3])
end""" onLoadCode True "indigo"
        , h3 [] [ text "Multi-argument lambdas" ]
        , viewCodeBlock """let main = do
    let add = \\x y -> x + y
    println (add 2, 3)
end""" onLoadCode True "indigo"
        , h3 [] [ text "Placeholders" ]
        , p [] [ text "The placeholder _ stands for an ignored binding or pattern slot. Void-style helpers may return _ when a value is discarded." ]
        , viewCodeBlock """let head ((x:xs): [Any]) : Any = x
let tail ((_:xs): [Any]) : [Any] = xs

let main = do
    println (head [10, 20, 30])
    println (tail [10, 20, 30])
end""" onLoadCode True "indigo"
        , h3 [] [ text "Passing functions" ]
        , p [] [ text "Named functions and lambdas are first-class and work with prelude helpers such as map, filter, and foldl." ]
        , viewCodeBlock """let square (x: Int): Int = x * x

let main = println (map square, [1, 2, 3, 4])""" onLoadCode True "indigo"
        ]


viewLists : (String -> msg) -> Html msg
viewLists onLoadCode =
    section [ id "lists" ]
        [ h2 [] [ text "Lists" ]
        , p [] [ text "Lists are ordered collections written with square brackets. The element type can be written as [T]." ]
        , h3 [] [ text "Literals and construction" ]
        , viewCodeBlock """let main = do
    println [1, 2, 3]
    println 0 : [1, 2, 3]
    println [] 
end""" onLoadCode True "indigo"
        , h3 [] [ text "Concatenation and length" ]
        , viewCodeBlock """let main = do
    println [1, 2] ++ [3]
    println (length [1, 2, 3, 4])
end""" onLoadCode True "indigo"
        , h3 [] [ text "Indexing and slicing" ]
        , p [] [ text "The prelude defines !! for indexing and slice for ranges. Strings are character lists, so the same tools apply." ]
        , viewCodeBlock """let main = do
    println ([10, 20, 30] !! 1)
    println (slice [0, 1, 2, 3, 4], 1, 4)
end""" onLoadCode True "indigo"
        , h3 [] [ text "map, filter, folds" ]
        , viewCodeBlock """let main = do
    println (map (\\x -> x + 1), [1, 2, 3])
    println (filter (\\x -> x > 2), [1, 2, 3, 4])
    println (foldl (\\acc x -> acc + x), 0, [1, 2, 3, 4])
end""" onLoadCode True "indigo"
        , h3 [] [ text "Destructuring" ]
        , p [] [ text "List patterns use [] , [x, y] , and cons patterns such as (x:xs) or (x:y:rest). See Pattern Matching." ]
        ]


viewTuples : (String -> msg) -> Html msg
viewTuples onLoadCode =
    section [ id "tuples" ]
        [ h2 [] [ text "Tuples" ]
        , p [] [ text "Tuples are fixed-size products of values that may have different types. They need no named type definition." ]
        , h3 [] [ text "Creating tuples" ]
        , viewCodeBlock """let main = do
    let point = (10, 20)
    let person = ("Alice", 30, True)
    println point
    println person
end""" onLoadCode True "indigo"
        , h3 [] [ text "Tuple types" ]
        , p [] [ text "Tuple types use parentheses with types: (Int, String) or (Int, String, Bool)." ]
        , h3 [] [ text "Access" ]
        , p [] [ text "Elements use zero-based numeric field access: t.0 , t.1 , ..." ]
        , viewCodeBlock """let main = do
    let point = (10, 20)
    println point.0
    println point.1
end""" onLoadCode True "indigo"
        , h3 [] [ text "Parameters and returns" ]
        , viewCodeBlock """let swap ((a, b): (Int, String)): (String, Int) = (b, a)

let main = do
    let result = swap (42, "hello")
    println result.0
    println result.1
end""" onLoadCode True "indigo"
        , h3 [] [ text "Pattern matching" ]
        , viewCodeBlock """let describe ((x, y): (Int, Int)): String =
    when (x, y) of
        (0, 0) -> "origin"
        (0, _) -> "on y-axis"
        (_, 0) -> "on x-axis"
        else -> "somewhere else"
    end

let main = println (describe (0, 5))""" onLoadCode True "indigo"
        , h3 [] [ text "Nested tuples" ]
        , viewCodeBlock """let main = do
    let nested = ((1, 2), (3, 4))
    println nested.0.1
    println nested.1.0
end""" onLoadCode True "indigo"
        , h3 [] [ text "Tuples vs lists" ]
        , p [] [ text "Tuples have fixed length and heterogeneous types. Lists have uniform element type and variable length. Prefer tuples for small fixed groupings; lists for sequences." ]
        ]


viewProductTypes : (String -> msg) -> Html msg
viewProductTypes onLoadCode =
    section [ id "product-types" ]
        [ h2 [] [ text "Product types" ]
        , p [] [ text "Product types (records / structs) group named fields. Define them with type Name = (field: Type, ...)." ]
        , viewCodeBlock """type Person = (name: String, age: Int)

let main = do
    let p = Person { name: "Ada", age: 36 }
    println p.name
    println p.age
end""" onLoadCode True "indigo"
        , h3 [] [ text "Construction and access" ]
        , p [] [ text "Construct with TypeName { field: expr, ... }. Read fields with dot syntax. Empty products use () and often Unit{} / TypeName{}." ]
        , h3 [] [ text "is clause" ]
        , p [] [ text "A type can declare trait implementations it carries with is Trait, OtherTrait." ]
        , viewCodeBlock """trait Printable = do
    print_value :: Self -> IO<Unit>
end

type Person = (name: String, age: Int) is Printable

impl Printable for Person = do
    print_value self = println self.name
end

let main = print_value (Person { name: "Ada", age: 36 })""" onLoadCode True "indigo"
        , h3 [] [ text "Generic product types" ]
        , viewCodeBlock """type Box<T> = (value: T)

let main = do
    let b = Box<Int> { value: 99 }
    println b.value
end""" onLoadCode True "indigo"
        , h3 [] [ text "Refinements" ]
        , p [] [ text "Attach logical constraints with satisfies (...). See Refinement Types." ]
        , viewCodeBlock """type Positive = (x: Int) satisfies (x > 0)

let main = println (Positive { x: 3 }).x""" onLoadCode True "indigo"
        , p [] [ text "For variant-style ADTs using | , see Sum Types. Traits are covered under Traits & Impls." ]
        ]


viewSumTypes : (String -> msg) -> Html msg
viewSumTypes onLoadCode =
    section [ id "sum-types" ]
        [ h2 [] [ text "Sum types" ]
        , p [] [ text "Sum types (tagged unions) list constructors after =, separated by | . Each constructor takes its fields in parentheses (no space before the '(')." ]
        , viewCodeBlock """type Shape = Circle(radius: Float) | Rect(width: Float, height: Float)

let areaCircle (c: Circle): Float = 3.14159 * c.radius * c.radius
let areaRect (r: Rect): Float = r.width * r.height

let main = do
    println (areaCircle (Circle { radius: 2.0 }))
    println (areaRect (Rect { width: 3.0, height: 4.0 }))
end""" onLoadCode True "indigo"
        , p [] [ text "Each constructor becomes its own product type that implements a trait named after the sum type, so you can still abstract over Shape when you need polymorphism." ]
        , h3 [] [ text "Simple tags" ]
        , viewCodeBlock """type Color = Red() | Green() | Blue()

let nameGreen (_: Green): String = "green"
let nameRed (_: Red): String = "red"

let main = println (nameGreen (Green {}))""" onLoadCode True "indigo"
        , h3 [] [ text "Traits as open sums" ]
        , p [] [ text "For open sets of cases (possibly across modules), use a trait plus separate types with impl (see Traits & Impls)." ]
        ]


viewTraitsAndImpls : (String -> msg) -> Html msg
viewTraitsAndImpls onLoadCode =
    section [ id "traits-and-impls" ]
        [ h2 [] [ text "Traits and implementations" ]
        , p [] [ text "Traits describe shared behavior. Implementations attach that behavior to concrete types." ]
        , h3 [] [ text "Declaring a trait" ]
        , viewCodeBlock """trait Animal = do
    makeNoise :: Self -> IO<Unit>
end

type Dog = (name: String)
type Cat = (name: String)

impl Animal for Dog = do
    makeNoise self = println "Woof"
end

impl Animal for Cat = do
    makeNoise self = println "Meow"
end

let main : IO<Unit> = do
    makeNoise (Dog { name: "Bello" })
    makeNoise (Cat { name: "Mauzi" })
end""" onLoadCode True "indigo"
        , h3 [] [ text "Marker traits" ]
        , p [] [ text "A trait body may be empty (or implementations may omit methods) when no methods are required." ]
        , viewCodeBlock """trait Pet

type Dog = ()
type Rock = ()

impl Pet for Dog

let pet (a: Pet) : IO<Unit> = println "petting"

let main = pet (Dog {})""" onLoadCode True "indigo"
        , h3 [] [ text "Required properties" ]
        , p [] [ text "Traits can require fields that implementing types must provide." ]
        , viewCodeBlock """trait Named = (name: String)

type Person = (name: String) is Named

type Person2 = (name: String, age: String) is Named

let main = println "ok"
""" onLoadCode True "indigo"
        , h3 [] [ text "Required properties and methods" ]
        , viewCodeBlock """trait Describable = (name: String, age: Int) do
    describe :: Self -> String
end

type Person = (name: String, age: Int) is Describable

impl Describable for Person = do
    describe self = self.name ++ " (" ++ (self.age as String) ++ ")"
end

let main = println (describe (Person { name: "Ada", age: 36 }))""" onLoadCode True "indigo"
        , h3 [] [ text "Self" ]
        , p [] [ text "In trait signatures, Self refers to the implementing type." ]
        , h3 [] [ text "Trait refinements" ]
        , p [] [ text "Traits may include satisfies constraints checked for implementing value construction. Combine with required fields as needed." ]
        , h3 [] [ text "is on the type" ]
        , p [] [ text "Prefer type Person = (...) is Trait so implementations and field requirements stay next to the type. Multiple traits: is Printable, Cloneable." ]
        ]


viewGenerics : (String -> msg) -> Html msg
viewGenerics onLoadCode =
    section [ id "generics" ]
        [ h2 [] [ text "Generics" ]
        , p [] [ text "Type parameters make functions, types, and traits reusable across concrete types." ]
        , h3 [] [ text "Generic functions" ]
        , viewCodeBlock """trait Number
impl Number for Int
impl Number for Float

let add<N: Number> (a: N b: N): N = do
  a + b
end

let main: IO<Unit> = do
  println add 1, 2
end""" onLoadCode True "indigo"
        , p [] [ text "Here add is generic over N constrained to Number. Calls succeed only when both arguments share a Number instance; the result is erased to Number at the call site in simple cases." ]
        , h3 [] [ text "Generic types" ]
        , viewCodeBlock """trait Number
impl Number for Int
impl Number for Float

type Container<T: Number> = (value: T)

let main: IO<Unit> = do
    let intContainer = Container<Int>{value: 42}
    let floatContainer = Container<Float>{value: 3.14}
    println intContainer.value
    println floatContainer.value
end""" onLoadCode True "indigo"
        , p [] [ text "Provide type arguments in angle brackets on literals: Container<Int>{value: 42}." ]
        , h3 [] [ text "Inferring type parameters" ]
        , viewCodeBlock """let identity<T> (x: T) : T = x

let main : IO<Unit> = println (identity "hello")""" onLoadCode True "indigo"
        , h3 [] [ text "Generic traits" ]
        , viewCodeBlock """trait Boxable<T> = do
    get :: Self -> T
end""" onLoadCode True "indigo"
        , h3 [] [ text "Trait constraints" ]
        , viewCodeBlock """trait Number
impl Number for Int
impl Number for Float

type Example<N: Number> = (content: N)

let main: IO<Unit> = do
    let x = Example<Int>{content: 42}
    let y = Example<Float>{content: 3.14}
    println x.content
    println y.content
end""" onLoadCode True "indigo"
        ]


viewCastingAndValueTypes : (String -> msg) -> Html msg
viewCastingAndValueTypes onLoadCode =
    section [ id "casting-and-value-types" ]
        [ h2 [] [ text "Casting and value types" ]
        , h3 [] [ text "as cast" ]
        , p [] [ text "The binary as operator converts a value to another type when the conversion is allowed." ]
        , viewCodeBlock """let main = do
    println (100 as Float)
    println ((3.5 as Int))
    println (42 as String)
end""" onLoadCode True "indigo"
        , h3 [] [ text "Value types" ]
        , p [] [ text "A value type wraps exactly one field and can be constructed via cast as well as the usual struct form." ]
        , viewCodeBlock """value type Meters = (n: Float)

let main = do
    let m = 3.0 as Meters
    println m.n
end""" onLoadCode True "indigo"
        , h3 [] [ text "With refinements" ]
        , p [] [ text "Casts into refined value types are checked (SMT on native builds; limited checks in WASM). See Refinement Types for unchecked." ]
        , viewCodeBlock """value type Positive = (x: Int) satisfies (x > 0)

let main = println ((5 as Positive).x)""" onLoadCode True "indigo"
        ]


viewRefinementTypes : (String -> msg) -> Html msg
viewRefinementTypes onLoadCode =
    section [ id "refinement-types" ]
        [ h2 [] [ text "Refinement types" ]
        , p [] [ text "Refinement types attach logical constraints to type definitions. The compiler uses an SMT solver (Z3) to verify constraints at compile time for literals, arithmetic, and values flowing through branches." ]
        , p [] [ text "Syntax: satisfies followed by a boolean predicate over the type's fields." ]
        , viewCodeBlock """type Age = (value: Int) satisfies (value >= 0)

let main: IO<Unit> = do
    let a = Age{value: 5}
    println a.value
end""" onLoadCode True "indigo"
        , h3 [] [ text "Compile-time validation" ]
        , p [] [ text "If a value provably violates the constraint, the compiler produces an error." ]
        , viewCodeBlock """type Age = (value: Int) satisfies (value >= 0)

let main: IO<Unit> = do
    let a = Age{value: 0 - 1}   # error: refinement failed
end""" onLoadCode False "indigo"
        , h3 [] [ text "Variables and path conditions" ]
        , p [] [ text "The SMT backend can track path conditions from if / when branches so refinements may be proven for variables (support is evolving). Prefer literals and arithmetic the solver can discharge today." ]
        , viewCodeBlock """type Age = (value: Int) satisfies (value >= 0)

# Prefer building refined values from proven literals, or from arithmetic
# the solver can discharge (e.g. 0, 1, x+1 when x is known non-negative).
let main: IO<Unit> = do
    let a = Age{value: 0}
    let b = Age{value: 1 + 2}
    println a.value
    println b.value
end""" onLoadCode True "indigo"
        , h3 [] [ text "Value types and casts" ]
        , p [] [ text "Single-field value types constructed with as are verified the same way." ]
        , viewCodeBlock """value type EvenNumber = (num: Int) satisfies ((num % 2) == 0)

let main: IO<Unit> = do
    println (12 as EvenNumber)
    # println (5 as EvenNumber)  # fails at compile time
end""" onLoadCode True "indigo"
        , h3 [] [ text "String and collection constraints" ]
        , p [] [ text "Use length(...) for strings and collections in predicates." ]
        , viewCodeBlock """type Username = (name: String) satisfies ((length name) > 0 && (length name) <= 20)
type Tag = (label: String) satisfies ((length label) > 0)

let main: IO<Unit> = do
    let u = Username{name: "alice"}
    let t = Tag{label: "indigo"}
    println u.name
    println t.label
end""" onLoadCode True "indigo"
        , h3 [] [ text "Equality constraints" ]
        , viewCodeBlock """type Person = (name: String) satisfies (name == "Alice")

let main: IO<Unit> = do
    let p = Person{name: "Alice"}
    println p.name
end""" onLoadCode True "indigo"
        , h3 [] [ text "Runtime fallback: unchecked" ]
        , p [] [ text "When a constraint cannot be proven (for example input from the user), unchecked defers the check to runtime. Violation exits with an error. Full unchecked support lands with the SMT refinements workstream; prefer path-conditioned construction on current mainline." ]
        , viewCodeBlock """type Age = (value: Int) satisfies (value >= 0)

let main: IO<Unit> = do
    let raw = $getLine
    let n   = raw as Int
    let age = unchecked Age{value: n}
    println age.value
end""" onLoadCode False "indigo"
        , h3 [] [ text "How it works" ]
        , p [] [ text "The compiler collects verification conditions for struct literals, casts, and call sites, then discharges them with Z3 via SBV. Failures surface as ordinary compiler errors (including through the LSP)." ]
        , p [] [ text "Supported in predicates: arithmetic (+ - * / %), comparisons, boolean logic (&& || !), len / size, and path conditions from branches." ]
        , playgroundNote "The WASM playground is built without full SMT support. Refinements there fall back to interpreter checks on literals only."
        ]


viewPatternMatching : (String -> msg) -> Html msg
viewPatternMatching onLoadCode =
    section [ id "pattern-matching" ]
        [ h2 [] [ text "Pattern matching" ]
        , p [] [ text "when expresses multi-way matching. Clauses use pattern -> body . An else clause covers remaining cases." ]
        , viewCodeBlock """let main = do
    let x = 5
    when x of
        1 -> println "one"
        2 -> println "two"
        3 -> println "three"
        else -> println "something else"
    end
end""" onLoadCode True "indigo"
        , h3 [] [ text "List patterns" ]
        , viewCodeBlock """let main = do
    let list = [1, 2, 3]
    when list of
        [] -> println "empty"
        [x] -> println "single"
        [x, y] -> println "two"
        (x:y:rest) -> println "three or more"
        else -> println "other"
    end
end""" onLoadCode True "indigo"
        , h3 [] [ text "String patterns" ]
        , p [] [ text "Strings are lists of characters, so list and cons patterns apply." ]
        , viewCodeBlock """let main = do
    let str = "hi"
    when str of
        [] -> println "empty"
        ['h', 'i'] -> println "matched hi"
        (c:rest) -> println "starts with something"
        else -> println "?"
    end
end""" onLoadCode True "indigo"
        , h3 [] [ text "Boolean and literals" ]
        , viewCodeBlock """let main = do
    when True of
        True -> println "yes"
        False -> println "no"
        else -> println "?"
    end
end""" onLoadCode True "indigo"
        , h3 [] [ text "Function clauses" ]
        , p [] [ text "Define multiple function heads with different patterns instead of a single when." ]
        , viewCodeBlock """let fact (0: Int): Int = 1
let fact (n: Int): Int = n * (fact ((n) - 1))

let main = println (fact 5)""" onLoadCode True "indigo"
        , h3 [] [ text "when as a function body" ]
        , viewCodeBlock """let show_list (xs: [Int]): String = when xs of
    [] -> "[]"
    [x] -> "[" ++ (x as String) ++ "]"
    else -> "..."
end

let main = println (show_list [9])""" onLoadCode True "indigo"
        , h3 [] [ text "Tuple patterns" ]
        , viewCodeBlock """let describe ((x, y): (Int, Int)): String = when (x, y) of
    (0, 0) -> "origin"
    (0, _) -> "on y-axis"
    (_, 0) -> "on x-axis"
    else -> "somewhere else"
end

let main = println (describe (0, 5))""" onLoadCode True "indigo"
        , h3 [] [ text "Struct fields" ]
        , p [] [ text "Match on extracted fields, or branch with if on p.x / p.y . Constructor-specific sum types are handled as separate types (see Sum Types)." ]
        ]


viewIOAndSequencing : (String -> msg) -> Html msg
viewIOAndSequencing onLoadCode =
    section [ id "io-and-sequencing" ]
        [ h2 [] [ text "IO and sequencing" ]
        , p [] [ text "Side-effecting computations return IO (or IO<T>). Program entry is typically main : IO<Unit>." ]
        , viewCodeBlock """let main : IO<Unit> = do
    println "Hello"
    println "world"
end""" onLoadCode True "indigo"
        , h3 [] [ text "IO wrapper" ]
        , p [] [ text "The prelude defines type IO<T> = (inner: T). Many actions return that shape; .inner extracts the pure payload when you already ran the effect in a do block." ]
        , h3 [] [ text "do sequencing" ]
        , p [] [ text "Lines in a do block that produce IO are sequenced left-to-right." ]
        , h3 [] [ text ">> and >>=" ]
        , p [] [ text ">> runs two monadic actions in order (sequence). >>= passes the unwrapped result of the left action into a function (bind)." ]
        , viewCodeBlock """let main : IO<Unit> = do
    println "first"
    println "second"
end""" onLoadCode True "indigo"
        , h3 [] [ text "mapM and friends" ]
        , p [] [ text "Prelude helpers such as mapM apply an IO-producing function across a list." ]
        , viewCodeBlock """let main : IO<Unit> = do
    mapM println, ["a", "b", "c"]
    nop
end""" onLoadCode True "indigo"
        , h3 [] [ text "Pipeline into effects" ]
        , viewCodeBlock """let main = "Hello from a pipe" |> println""" onLoadCode True "indigo"
        , playgroundNote "Prefer do-blocks in longer programs; monadic operators are available for compact composition."
        ]


viewImportsAndModules : (String -> msg) -> Html msg
viewImportsAndModules onLoadCode =
    section [ id "imports-and-modules" ]
        [ h2 [] [ text "Imports and modules" ]
        , p [] [ text "Larger programs split into modules. Imports pull names from other modules into scope." ]
        , h3 [] [ text "Module declarations" ]
        , viewCodeBlock """module MathUtils

let add (x: Int y: Int): Int = x + y""" onLoadCode False "indigo"
        , h3 [] [ text "Unqualified imports" ]
        , p [] [ text "import Module brings exported names into scope without a prefix. The from keyword is optional." ]
        , viewCodeBlock """import MathUtils
# or: import from MathUtils

let main = println (add 1, 2)""" onLoadCode False "indigo"
        , h3 [] [ text "Qualified imports" ]
        , viewCodeBlock """import qualified MathUtils

let main = println (MathUtils.add 1, 2)""" onLoadCode False "indigo"
        , h3 [] [ text "Aliases" ]
        , viewCodeBlock """import qualified MathUtils as M

let main = println (M.add 1, 2)""" onLoadCode False "indigo"
        , h3 [] [ text "Paths" ]
        , p [] [ text "Module names may include dots, slashes, and @ for hierarchical or path-like references. Std modules such as File are imported by name." ]
        , viewCodeBlock """import qualified File

let main = println "see File stdlib docs"
""" onLoadCode False "indigo"
        , playgroundNote "Multi-file modules are not loaded by the single-buffer playground; use local indigo with multiple files for full module workflows."
        ]


viewFFI : (String -> msg) -> Html msg
viewFFI onLoadCode =
    section [ id "ffi" ]
        [ h2 [] [ text "FFI" ]
        , p [] [ text "Indigo can call into dynamic libraries via external blocks. Declare the library name and foreign signatures; the runtime searches the OS library path and the current directory." ]
        , viewCodeBlock """// example.c
void greet(char *name) {
  printf("Hello, %s!\\n", name);
}

void printNumbers(int *numbers, int count) {
  for (int i = 0; i < count; i++) {
    printf("%d\\n", numbers[i]);
  }
}""" onLoadCode False "c"
        , pre [] [ code [] [ text """gcc -shared -fPIC example.c -o example.so
# macOS: gcc -dynamiclib example.c -o example.dylib""" ] ]
        , viewCodeBlock """external "example" = do
    greet :: String -> IO
    printNumbers :: [Int] -> Int -> IO
end

external "__default" = do
    puts :: [Char] -> IO
end

let main = do
    puts ['H', 'i', '!', '\\0']
    printNumbers [1, 2, 3, 4, 5], 5
    greet "Rocko"
end""" onLoadCode False "indigo"
        , p [] [ text "Use \"__default\" for symbols resolved from the default process / libc-style linkage where supported." ]
        , playgroundNote "FFI requires native shared libraries and is not available in the WASM playground."
        ]


viewPrelude : (String -> msg) -> Html msg
viewPrelude onLoadCode =
    section [ id "prelude" ]
        [ h2 [] [ text "Standard library: Prelude" ]
        , p [] [ text "share/std/prelude.in is loaded for normal programs. It defines core types, operators, list helpers, and IO utilities." ]
        , h3 [] [ text "Core types" ]
        , viewCodeBlock """# From prelude (already available)
# type IO<T> = (inner: T)
# type Unit = ()
# trait Optional<T>
# type Some<T> = (value: T) is Optional<T>
# type None = () is Optional<Any>
# trait Number
# trait Monad<T>""" onLoadCode False "indigo"
        , h3 [] [ text "Optional" ]
        , viewCodeBlock """let main = do
    let s = Some<Int>{value: 10}
    let n = None{}
    println s.value
    println "none-constructed"
end""" onLoadCode True "indigo"
        , p [] [ text "Prelude defines trait Optional<T>, constructors Some<T> / None, and maybe default, optional helpers. Use Some and None for optional values." ]
        , h3 [] [ text "List helpers" ]
        , p [] [ text "map, mapM, filter, foldl, foldr, sum, head, tail, length, last, slice, !! ." ]
        , viewCodeBlock """let main = do
    println (map (\\x -> x * 2), [1, 2, 3])
    println (sum [1, 2, 3, 4])
    println (head [9, 8, 7])
end""" onLoadCode True "indigo"
        , h3 [] [ text "Operators as functions" ]
        , p [] [ text "Arithmetic, comparison, boolean, modulo, and ++ are defined in the prelude as callable bindings (often via unsafe* primitives)." ]
        , h3 [] [ text "IO helpers" ]
        , p [] [ text "print, println, nop, return, and related helpers produce IO actions. times / timesM style helpers iterate." ]
        , viewCodeBlock """let main : IO<Unit> = do
    println "prelude println"
    nop
end""" onLoadCode True "indigo"
        , h3 [] [ text "Struct utilities" ]
        , p [] [ text "keys, update, mergeStructs, and !!! (string field access) work with record values when exposed by the runtime." ]
        ]


viewStdFile : (String -> msg) -> Html msg
viewStdFile onLoadCode =
    section [ id "stdlib-file" ]
        [ h2 [] [ text "Standard library: File" ]
        , p [] [ text "share/std/file.in provides basic file I/O as IO actions. Import the File module and call the wrappers around unsafe* FFI primitives." ]
        , viewCodeBlock """import qualified File

let main : IO<Unit> = do
    let path = "demo.txt"
    let fd_io = File.openFile path, "w"
    let fd = fd_io.inner
    File.writeFile fd, "hello\\n"
    File.closeFile fd

    let fd2_io = File.openFile path, "r"
    let fd2 = fd2_io.inner
    let content_io = File.readFile fd2, 1000
    println content_io.inner
    File.closeFile fd2
    nop
end""" onLoadCode False "indigo"
        , h3 [] [ text "Typical API" ]
        , p [] [ text "openFile path mode, readFile fd size, writeFile fd content, closeFile fd; each returns IO wrapping an Int or String payload via .inner." ]
        , h3 [] [ text "Sockets" ]
        , p [] [ text "share/std/sockets.in similarly wraps socket, bindSocket, listen, accept, connect, send, and recv. Import qualified Sockets for network examples (native only)." ]
        , playgroundNote "File and Sockets need the native runtime and OS facilities. They are not available in the browser playground."
        ]


viewProjectConfig : (String -> msg) -> Html msg
viewProjectConfig onLoadCode =
    section [ id "project-config" ]
        [ h2 [] [ text "Project config" ]
        , p [] [ text "Projects may include indigo.json. The compiler walks up from the current file looking for it and uses the first one found." ]
        , h3 [] [ text "Fields" ]
        , viewCodeBlock """{
  "main": "src/Main.in",
  "src": ["src", "lib"]
}""" onLoadCode False "json"
        , p [] [ text "main is an optional entry file, and src specifies optional extra source directories for module resolution." ]
        , h3 [] [ text "Minimal config" ]
        , viewCodeBlock """{}""" onLoadCode False "json"
        , p [] [ text "All fields are optional. {} uses defaults (no forced main, no extra source roots)." ]
        , h3 [] [ text "Discovery" ]
        , p [] [ text "Search starts in the directory of the file being compiled, then each parent until the filesystem root." ]
        , h3 [] [ text "Running indigo" ]
        , p [] [ text "With a Nix shell: cabal run indigo -- path/to/file.in runs a program. cabal run indigo starts the REPL. See the repository README for LSP and WASM build targets." ]
        ]
