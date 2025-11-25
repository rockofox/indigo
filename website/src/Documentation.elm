module Documentation exposing (view, Topic(..))

import Html exposing (Html, div, h2, h3, p, pre, code, section, text, a, button, node)
import Html.Attributes exposing (class, id, href, attribute)
import Html.Events exposing (onClick)

type Topic
    = FunctionsAndBindings
    | Comments
    | PatternMatching
    | Tuples
    | StructsAndTraits
    | FFI
    | Generics
    | RefinementTypes

view : Topic -> (String -> msg) -> Html msg
view currentTopic onLoadCode =
    div [ class "docs-container" ]
        [ viewSidebar
        , div [ class "docs-content" ]
            [ case currentTopic of
                FunctionsAndBindings -> viewFunctionsAndBindings onLoadCode
                Comments -> viewComments onLoadCode
                PatternMatching -> viewPatternMatching onLoadCode
                Tuples -> viewTuples onLoadCode
                StructsAndTraits -> viewStructsAndTraits onLoadCode
                FFI -> viewFFI onLoadCode
                Generics -> viewGenerics onLoadCode
                RefinementTypes -> viewRefinementTypes onLoadCode
            ]
        ]

viewSidebar : Html msg
viewSidebar =
    div [ class "docs-sidebar" ]
        [ h3 [] [ text "Language Basics" ]
        , sidebarLink "Functions & Bindings" "/docs/functions-and-bindings"
        , sidebarLink "Comments" "/docs/comments"
        , sidebarLink "Pattern Matching" "/docs/pattern-matching"
        , sidebarLink "Tuples" "/docs/tuples"
        , sidebarLink "Structs & Traits" "/docs/structs-and-traits"
        , h3 [] [ text "Advanced Features" ]
        , sidebarLink "FFI" "/docs/ffi"
        , sidebarLink "Generics" "/docs/generics"
        , sidebarLink "Refinement Types" "/docs/refinement-types"
        ]

sidebarLink : String -> String -> Html msg
sidebarLink label path =
    a [ href path ] [ text label ]

viewCodeBlock : String -> (String -> msg) -> Bool -> String -> Html msg
viewCodeBlock code onLoadCode showTryButton language =
    div [ class "code-block-container" ]
        [ if showTryButton then
            button [ class "try-button", onClick (onLoadCode code), attribute "title" "Try in Playground" ] [ text "▶" ]
          else
            text ""
        , node "code-editor"
            [ attribute "code" code
            , attribute "read-only" "true"
            , attribute "auto-height" "true"
            , attribute "language" language
            ] []
        ]

viewFunctionsAndBindings : (String -> msg) -> Html msg
viewFunctionsAndBindings onLoadCode =
    section [ id "functions-and-bindings" ]
        [ h2 [] [ text "Functions and bindings" ]
        , p [] [ text "Indigo doesn't differentiate between functions and bindings. They are both defined using the let keyword." ]
        , viewCodeBlock """let name = "Rocko"
let main = println "Hello " ++ name""" onLoadCode True "indigo"
        , p [] [ text "The above code defines a binding name and a function main. The function main is called when the program is run." ]
        , viewCodeBlock """let multiple_args (a: Int b: Int): Int = a + b
let main = print (multiple_args 2, 3)""" onLoadCode True "indigo"
        , p [] [ text "Functions can have multiple arguments. The above function adds two integers. Specifying the return type is optional, specifying parameter types is mandatory currently." ]
        , viewCodeBlock """let println (s: String): IO = do
    print s
    print "\\n"
end""" onLoadCode True "indigo"
        , p [] [ text "The above code shows the source code of the println function. As you can see here, the return type of IO actions is IO. Such functions can only used in functions that also return IO." ]
        , p [] [ text "Also, this example shows how it is possible to group statements using do and end." ]
        ]

viewComments : (String -> msg) -> Html msg
viewComments onLoadCode =
    section [ id "comments" ]
        [ h2 [] [ text "Comments" ]
        , viewCodeBlock """# This is a single-line comment
/* This
 * is 
 * a
 * multi-line
 * comment
 */""" onLoadCode True "indigo"
        , p [] [ text "Not a lot to explain here. Line comments use # and block comments /* */" ]
        ]

viewStructsAndTraits : (String -> msg) -> Html msg
viewStructsAndTraits onLoadCode =
    section [ id "structs-and-traits" ]
        [ h2 [] [ text "Structs and traits" ]
        , viewCodeBlock """struct Dog = (name: String)
struct Cat = (name: String)

let main: IO = do
    let bello = Dog { name : "Bello" }
    let mauzi = Cat { name : "Mauzi" }
    println name bello
    println name mauzi
end""" onLoadCode True "indigo"
        , p [] [ text "The above example shows simple data structures. The name method gets created automatically. Name collisions (shoud be) resolved automatically." ]
        , viewCodeBlock """trait Animal = do
    makeNoise :: Self -> IO
end

impl Animal for Dog = do
    makeNoise self = println "Woof"
end

impl Animal for Cat = do
    makeNoise self = println "Meow"
end

let main: IO = do
    makeNoise (Dog {})
    makeNoise (Cat {})
end""" onLoadCode True "indigo"
        , p [] [ text "The above example defines a trait called Animal with a method makeNoise." ]
        , p [] [ text "Then, two implementations of the Animal trait are provided for the types Dog and Cat. The implementation for Dog specifies that when makeNoise is called, it will print \"Woof,\" and the implementation for Cat will print \"Meow.\"" ]
        ]

viewFFI : (String -> msg) -> Html msg
viewFFI onLoadCode =
    section [ id "ffi" ]
        [ h2 [] [ text "FFI" ]
        , viewCodeBlock """// example.c
void greet(char *name) {
  printf("Hello, %s!\\n", name);
}

struct Point {
  float x;
  float y;
};

void printPoint(struct Point p) {
  printf("(%f, %f)\\n", p.x, p.y);
}

void printNumbers(int *numbers, int count) {
  for (int i = 0; i < count; i++) {
    printf("%d\\n", numbers[i]);
  }
}""" onLoadCode False "c"
        , pre [] [ code [] [ text """gcc -dynamiclib example.c -o example.dylib""" ] ]
        , viewCodeBlock """# example.in
external "sayhello" = do
    greet :: String -> IO
    printNumbers :: List{Int} -> Int -> IO
end
external "__default" = do
    puts :: List{Char} -> IO
end

let main = do
    puts ['H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\\0']
    printNumbers [1, 2, 3, 4, 5], 5
    greet "Rocko"
end""" onLoadCode False "indigo"
        , p [] [ text "Indigo can interact with dynamic libraries by declaring their function in an external block. The runtime will look for the specified library (+ the appropriate extension for dynamic libraries on your OS) in your OS's search path and ./" ]
        ]

viewPatternMatching : (String -> msg) -> Html msg
viewPatternMatching onLoadCode =
    section [ id "pattern-matching" ]
        [ h2 [] [ text "Pattern Matching" ]
        , p [] [ text "Indigo provides pattern matching through the when expression, which allows you to match values against patterns and execute different code based on the match." ]
        , viewCodeBlock """let x = 5
when x of
  1 -> println("x is one")
  2 -> println("x is two")
  3 -> println("x is three")
  else -> println("x is something else")
end""" onLoadCode True "indigo"
        , p [] [ text "The when expression starts with when followed by the expression to match, then of, followed by pattern branches. Each branch consists of a pattern, an arrow ->, and the expression to execute if the pattern matches. The else branch is optional and serves as a default case." ]
        , h3 [] [ text "List Pattern Matching" ]
        , p [] [ text "You can match against lists using various patterns:" ]
        , viewCodeBlock """let list = [1, 2, 3]
when list of
  [] -> println("empty list")
  [x] -> println("single element: " ++ show(x))
  [x, y] -> println("two elements: " ++ show(x) ++ ", " ++ show(y))
  (x:y:rest) -> println("three or more elements, first two: " ++ show(x) ++ ", " ++ show(y))
  else -> println("unexpected list")
end""" onLoadCode True "indigo"
        , p [] [ text "List patterns include: empty list [], single element [x], multiple elements [x, y], and cons patterns (x:y:rest) for matching the head and tail of a list." ]
        , h3 [] [ text "String Pattern Matching" ]
        , p [] [ text "Since strings are lists of characters, you can use list patterns to match strings:" ]
        , viewCodeBlock """let str = "hello"
when str of
  [] -> println("empty string")
  ['h', 'e', 'l', 'l', 'o'] -> println("matched 'hello'")
  (c:rest) -> println("starts with: " ++ show(c))
  else -> println("other string")
end""" onLoadCode True "indigo"
        , h3 [] [ text "Boolean Matching" ]
        , viewCodeBlock """let b = True
when b of
  True -> println("b is true")
  False -> println("b is false")
  else -> println("b is something else")
end""" onLoadCode True "indigo"
        , h3 [] [ text "Function Pattern Matching" ]
        , p [] [ text "Functions can also pattern match directly on their arguments by defining multiple function clauses with different patterns:" ]
        , viewCodeBlock """let fib (0: Int) : Int = 0
let fib (1: Int) : Int = 1
let fib (n: Int) : Int = (fib ((n) - 1)) + (fib ((n) - 2))

let main : IO = do
  println fib 12
end""" onLoadCode True "indigo"
        , p [] [ text "In the above example, the fib function has three definitions: one for when the argument is 0, one for when it's 1, and one for any other integer. The compiler will try to match the arguments against these patterns in order." ]
        , p [] [ text "Function pattern matching also works with list patterns:" ]
        , viewCodeBlock """let head ([]: [Any]) = 0
let head ((x:xs): [Any]) = x

let main : IO = do
  println head [1, 2, 3]
end""" onLoadCode True "indigo"
        , p [] [ text "This head function matches the empty list pattern first, then matches any non-empty list using a cons pattern, binding the first element to x and the rest to xs." ]
        , h3 [] [ text "Using when as a Function Body" ]
        , p [] [ text "The when expression can be used directly as a function body:" ]
        , viewCodeBlock """let main = when 5 of
  1 -> println("one")
  5 -> println("five")
  else -> println("other")
end""" onLoadCode True "indigo"
        , p [] [ text "This allows for concise pattern matching in function definitions." ]
        , h3 [] [ text "Tuple Pattern Matching" ]
        , p [] [ text "You can pattern match on tuples to extract their elements:" ]
        , viewCodeBlock """let t = (42, "hello")
when t of
  (x, y) -> do
    println x
    println y
  end
end""" onLoadCode True "indigo"
        , p [] [ text "Tuple patterns allow you to destructure tuples and bind their elements to variables. You can also match against specific tuple values:" ]
        , viewCodeBlock """let t = (1, "test")
when t of
  (1, "hello") -> println "matched (1, hello)"
  (2, "world") -> println "matched (2, world)"
  (x, y) -> println "matched other tuple"
end""" onLoadCode True "indigo"
        , p [] [ text "Functions can also pattern match on tuple arguments:" ]
        , viewCodeBlock """let fst :: (Int, String) -> Int
let fst (x, y) = x

let snd :: (Int, String) -> String
let snd (x, y) = y

let main: IO = do
  println (fst (42, "hello"))
  println (snd (42, "hello"))
end""" onLoadCode True "indigo"
        ]

viewTuples : (String -> msg) -> Html msg
viewTuples onLoadCode =
    section [ id "tuples" ]
        [ h2 [] [ text "Tuples" ]
        , p [] [ text "Tuples are fixed-size collections of values of potentially different types. They provide a way to group multiple values together without defining a custom struct." ]
        , h3 [] [ text "Creating Tuples" ]
        , p [] [ text "Tuples are created using parentheses with comma-separated values:" ]
        , viewCodeBlock """let main: IO = do
    let point = (10, 20)
    let person = ("Alice", 30, True)
    println point
    println person
end""" onLoadCode True "indigo"
        , p [] [ text "A tuple must have at least 2 elements. Single-element parentheses are treated as parenthesized expressions, not tuples." ]
        , h3 [] [ text "Tuple Types" ]
        , p [] [ text "Tuple types are specified using parentheses with type names:" ]
        , viewCodeBlock """let point: (Int, Int) = (10, 20)
let person: (String, Int, Bool) = ("Alice", 30, True)
let main: IO = do
    println point
    println person
end""" onLoadCode True "indigo"
        , p [] [ text "Each position in the tuple type corresponds to the type of the value at that position." ]
        , h3 [] [ text "Accessing Tuple Elements" ]
        , p [] [ text "Tuple elements are accessed using dot notation with zero-based numeric indices:" ]
        , viewCodeBlock """let main: IO = do
    let t = (42, "hello", True)
    println t.0
    println t.1
    println t.2
end""" onLoadCode True "indigo"
        , p [] [ text "The first element is accessed with .0, the second with .1, and so on. For nested tuple access, use intermediate variables:" ]
        , viewCodeBlock """let main: IO = do
    let t = ((1, 2), (3, 4))
    let first = t.0
    let second = t.1
    println first.0
    println first.1
    println second.0
    println second.1
end""" onLoadCode True "indigo"
        , h3 [] [ text "Tuples in Functions" ]
        , p [] [ text "Tuples can be used as function parameters and return types:" ]
        , viewCodeBlock """let swap :: (Int, String) -> (String, Int)
let swap (x, y) = (y, x)

let main: IO = do
    let result = swap (42, "hello")
    println result.0
    println result.1
end""" onLoadCode True "indigo"
        , p [] [ text "Functions can return tuples to provide multiple values:" ]
        , viewCodeBlock """let divide :: Int -> Int -> (Int, Int)
let divide a b = (a / b, a % b)

let main: IO = do
    let (quotient, remainder) = divide 10 3
    println quotient
    println remainder
end""" onLoadCode True "indigo"
        , h3 [] [ text "Tuple Pattern Matching" ]
        , p [] [ text "Tuples can be destructured using pattern matching in function definitions and when expressions:" ]
        , viewCodeBlock """let fst :: (Int, String) -> Int
let fst (x, y) = x

let main: IO = do
    println (fst (42, "test"))
end""" onLoadCode True "indigo"
        , p [] [ text "Pattern matching allows you to extract tuple elements directly in the function signature or when branches:" ]
        , viewCodeBlock """let main: IO = do
    let t = (1, "hello")
    when t of
        (1, "hello") -> println "matched (1, hello)"
        (2, "world") -> println "matched (2, world)"
        (x, y) -> do
            println "matched other: "
            println x
            println y
        end
    end
end""" onLoadCode True "indigo"
        , h3 [] [ text "Nested Tuples" ]
        , p [] [ text "Tuples can contain other tuples, allowing you to create more complex data structures:" ]
        , viewCodeBlock """let main: IO = do
    let nested = ((1, 2), (3, 4))
    let first = nested.0
    let second = nested.1
    println first.0
    println first.1
    println second.0
    println second.1
end""" onLoadCode True "indigo"
        , h3 [] [ text "Tuples vs Lists" ]
        , p [] [ text "Tuples differ from lists in several important ways:" ]
        , p [] [ text "• Tuples have a fixed size determined at compile time, while lists can have any length" ]
        , p [] [ text "• Tuple elements can be of different types, while list elements must all be the same type" ]
        , p [] [ text "• Tuples use numeric indices (.0, .1, etc.) for access, while lists use bracket notation ([0], [1], etc.)" ]
        , p [] [ text "• Tuples are best for grouping a known number of related values, while lists are for collections of unknown size" ]
        ]

viewGenerics : (String -> msg) -> Html msg
viewGenerics onLoadCode =
    section [ id "generics" ]
        [ h2 [] [ text "Generics" ]
        , p [] [ text "Indigo supports generic types on functions." ]
        , viewCodeBlock """trait Number
impl Number for Int
impl Number for Float

let add<N: Number> (a: N b: N): N = do
  a + b
end

let main: IO = do
  println add 1, 2
end""" onLoadCode True "indigo"
        , p [] [ text "In the above example, a function add is declared with a generic type parameter N constrained to the Number trait." ]
        , p [] [ text "This function takes two parameters of type N and returns a result of the same type." ]
        , p [] [ text "The call to add only suceeds if the given parameters are of the same Number type. The return type N gets type erased to Number." ]
        ]

viewRefinementTypes : (String -> msg) -> Html msg
viewRefinementTypes onLoadCode =
    section [ id "refinement-types" ]
        [ h2 [] [ text "Refinement Types" ]
        , p [] [ text "Refinement types allow you to add compile-time constraints to struct definitions. These constraints are checked when creating struct literals, ensuring that only valid values can be constructed." ]
        , p [] [ text "The syntax for refinement types uses the satisfies keyword followed by a boolean expression that references the struct's fields." ]
        , viewCodeBlock """struct Age = (value: Int) satisfies (value >= 0)

let main: IO = do
    let a = Age{value: 5}
end""" onLoadCode True "indigo"
        , p [] [ text "In the above example, the Age struct has a refinement that requires the value field to be greater than or equal to zero. When creating an Age struct literal, the compiler checks this constraint at compile time." ]
        , h3 [] [ text "Compile-Time Validation" ]
        , p [] [ text "If you try to create a struct literal that violates the refinement constraint, the compiler will produce an error:" ]
        , viewCodeBlock """struct Age = (value: Int) satisfies (value >= 0)

let main: IO = do
    let a = Age{value: 0 - 1}
end""" onLoadCode False "indigo"
        , p [] [ text "The above code will fail to compile with a refinement error, since the value -1 does not satisfy the constraint value >= 0." ]
        , h3 [] [ text "Equality Constraints" ]
        , p [] [ text "Refinements can also use equality checks to enforce specific values:" ]
        , viewCodeBlock """struct Person = (name: String) satisfies (name == "Alice")

let main: IO = do
    let p = Person{name: "Alice"}
end""" onLoadCode True "indigo"
        , p [] [ text "This example shows a Person struct that can only be created with the name \"Alice\". Any other name value will cause a compilation error." ]
        , h3 [] [ text "Field References" ]
        , p [] [ text "Field names from the struct can be referenced directly in the refinement expression. The refinement expression is evaluated with the struct literal's field values to determine if the constraint is satisfied." ]
        ]
