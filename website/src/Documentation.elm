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
    | ImportsAndModules
    | FFI
    | Generics
    | RefinementTypes

view : Topic -> (String -> msg) -> Html msg
view currentTopic onLoadCode =
    div [ class "docs-container" ]
        [ viewSidebar currentTopic
        , div [ class "docs-content" ]
            [ case currentTopic of
                FunctionsAndBindings -> viewFunctionsAndBindings onLoadCode
                Comments -> viewComments onLoadCode
                PatternMatching -> viewPatternMatching onLoadCode
                Tuples -> viewTuples onLoadCode
                StructsAndTraits -> viewStructsAndTraits onLoadCode
                ImportsAndModules -> viewImportsAndModules onLoadCode
                FFI -> viewFFI onLoadCode
                Generics -> viewGenerics onLoadCode
                RefinementTypes -> viewRefinementTypes onLoadCode
            ]
        ]

viewSidebar : Topic -> Html msg
viewSidebar currentTopic =
    div [ class "docs-sidebar" ]
        [ h3 [] [ text "Language Basics" ]
        , sidebarLink currentTopic FunctionsAndBindings "Functions & Bindings" "/docs/functions-and-bindings"
        , sidebarLink currentTopic Comments "Comments" "/docs/comments"
        , sidebarLink currentTopic PatternMatching "Pattern Matching" "/docs/pattern-matching"
        , sidebarLink currentTopic Tuples "Tuples" "/docs/tuples"
        , sidebarLink currentTopic StructsAndTraits "Structs & Traits" "/docs/structs-and-traits"
        , sidebarLink currentTopic ImportsAndModules "Imports & Modules" "/docs/imports-and-modules"
        , h3 [] [ text "Advanced Features" ]
        , sidebarLink currentTopic FFI "FFI" "/docs/ffi"
        , sidebarLink currentTopic Generics "Generics" "/docs/generics"
        , sidebarLink currentTopic RefinementTypes "Refinement Types" "/docs/refinement-types"
        ]

sidebarLink : Topic -> Topic -> String -> String -> Html msg
sidebarLink currentTopic linkTopic label path =
    a [ href path, class (if currentTopic == linkTopic then "active" else "") ] [ text label ]

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
        , h3 [] [ text "Generic Structs" ]
        , p [] [ text "Structs can have type parameters, allowing you to create reusable data structures that work with different types:" ]
        , viewCodeBlock """trait Number
impl Number for Int
impl Number for Float

struct Example<N: Number> = (content: N)

let main: IO = do
    let x = Example<Int>{content: 42}
    let y = Example<Float>{content: 3.14}
    println x.content
    println y.content
end""" onLoadCode True "indigo"
        , p [] [ text "In the above example, the Example struct has a generic type parameter N constrained to the Number trait. This means you can create Example instances with any type that implements Number, such as Int or Float." ]
        , p [] [ text "When creating a struct literal with type arguments, you specify the concrete type in angle brackets: Example<Int>{content: 42}. The compiler validates that the type argument (Int) satisfies the trait constraint (Number)." ]
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
        , h3 [] [ text "Generic Traits" ]
        , p [] [ text "Traits can also have type parameters:" ]
        , viewCodeBlock """trait Monad<T> = do
    bind :: Self -> (Any -> Self) -> Self
    return :: Any -> Self
end""" onLoadCode True "indigo"
        , p [] [ text "Generic traits allow you to define reusable abstractions that work with different types. The type parameter T can be used in the trait's method signatures." ]
        , h3 [] [ text "Required Properties" ]
        , p [] [ text "Traits can specify required properties that implementing structs must have. This ensures that any type implementing the trait has the necessary fields:" ]
        , viewCodeBlock """trait Printable = (name: String, age: Int)

struct Person = (name: String, age: Int) is Printable""" onLoadCode True "indigo"
        , p [] [ text "In the above example, the Printable trait requires that implementing structs have both a name field of type String and an age field of type Int. When you use the is clause, the compiler verifies that Person has these required fields with matching types." ]
        , p [] [ text "If a struct tries to implement a trait but is missing required properties or has type mismatches, the compiler will produce an error:" ]
        , viewCodeBlock """trait Printable = (name: String, age: Int)

# Error: Missing required property 'age' in struct Person for trait Printable
struct Person = (name: String) is Printable

# Error: Type mismatch - trait requires Int, struct has String
struct Person2 = (name: String, age: String) is Printable""" onLoadCode False "indigo"
        , h3 [] [ text "Trait Refinements" ]
        , p [] [ text "Traits can also have refinement types, similar to structs. These refinements are checked when creating struct literals that implement the trait:" ]
        , viewCodeBlock """trait PositiveNumber satisfies (x > 0)

struct PosInt = (x: Int) is PositiveNumber

let main: IO = do
    # Valid - 5 > 0
    let p = PosInt{x: 5}
    
    # Error - trait refinement failed (x > 0)
    # let p2 = PosInt{x: 0}
end""" onLoadCode True "indigo"
        , p [] [ text "When a struct implements a trait with a refinement, the compiler checks the trait's refinement constraint when creating struct literals. If the constraint is violated, compilation fails with an error indicating which trait's refinement was violated." ]
        , h3 [] [ text "Combining Required Properties and Refinements" ]
        , p [] [ text "Traits can have both required properties and refinements:" ]
        , viewCodeBlock """trait ValidPerson = (name: String, age: Int) satisfies (age > 0)

struct Person = (name: String, age: Int) satisfies (age > 0) is ValidPerson

let main: IO = do
    let p = Person{name: "Bob", age: 25}
    println p.name
end""" onLoadCode True "indigo"
        , p [] [ text "In this example, ValidPerson requires both name and age fields, and also has a refinement that age must be greater than 0. The Person struct must satisfy both the required properties and the refinement constraint." ]
        , h3 [] [ text "Using 'is' Clause" ]
        , p [] [ text "Structs can declare that they implement a trait directly in their definition using the is clause:" ]
        , viewCodeBlock """trait Printable = (name: String, age: Int)

struct Person = (name: String, age: Int) is Printable

let main: IO = do
    let p = Person{name: "Alice", age: 30}
    println p.name
end""" onLoadCode True "indigo"
        , p [] [ text "The is clause creates an implicit implementation of the trait. The compiler validates that the struct has all required properties specified by the trait. If the struct is missing required properties or has type mismatches, compilation fails:" ]
        , viewCodeBlock """trait Printable = (name: String, age: Int)

# Error: Missing required property 'age' in struct Person for trait Printable
struct Person = (name: String) is Printable""" onLoadCode False "indigo"
        , p [] [ text "Structs can implement multiple traits using the is clause by separating them with commas:" ]
        , viewCodeBlock """trait Printable = (name: String)
trait Cloneable

struct Person = (name: String) is Printable, Cloneable

let main: IO = do
    let p = Person{name: "Alice"}
    println p.name
end""" onLoadCode True "indigo"
        , p [] [ text "When using the is clause with traits that have refinements, the compiler checks trait refinements when creating struct literals, unless the struct has its own refinement that would cover the constraint." ]
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
        , p [] [ text "Multi-element tuples require at least 2 elements. Single-element tuples use a trailing comma syntax:" ]
        , viewCodeBlock """let main: IO = do
    let single = (42,)
    let double = (10, 20)
    println single.0
    println double.0
    println double.1
end""" onLoadCode True "indigo"
        , p [] [ text "Note that (1) is treated as a parenthesized expression (just the value 1), while (1,) is a single-element tuple. The trailing comma distinguishes single-element tuples from parenthesized expressions." ]
        , h3 [] [ text "Tuple Types" ]
        , p [] [ text "Tuple types are specified using parentheses with type names:" ]
        , viewCodeBlock """let point: (Int, Int) = (10, 20)
    let person: (String, Int, Bool) = ("Alice", 30, True)
    let single: (Int,) = (42,)
    let main: IO = do
    println point
    println person
    println single.0
end""" onLoadCode True "indigo"
        , p [] [ text "Each position in the tuple type corresponds to the type of the value at that position. Single-element tuple types also use the trailing comma syntax: (Int,) for a tuple containing a single Int." ]
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
        , p [] [ text "Indigo supports generic type parameters on functions, structs, and traits. This allows you to write reusable code that works with different types while maintaining type safety." ]
        , h3 [] [ text "Generic Functions" ]
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
        , h3 [] [ text "Generic Structs" ]
        , viewCodeBlock """trait Number
impl Number for Int
impl Number for Float

struct Container<T: Number> = (value: T)

let main: IO = do
    let intContainer = Container<Int>{value: 42}
    let floatContainer = Container<Float>{value: 3.14}
    println intContainer.value
    println floatContainer.value
end""" onLoadCode True "indigo"
        , p [] [ text "Structs can have generic type parameters with optional trait constraints. When creating a struct literal, you provide the type arguments in angle brackets: Container<Int>{value: 42}." ]
        , p [] [ text "The compiler validates that the type argument satisfies any trait constraints specified in the struct definition." ]
        , h3 [] [ text "Generic Traits" ]
        , viewCodeBlock """trait Monad<T> = do
    bind :: Self -> (Any -> Self) -> Self
    return :: Any -> Self
end""" onLoadCode True "indigo"
        , p [] [ text "Traits can also have type parameters, allowing you to define reusable abstractions that work with different types. The type parameters can be used in the trait's method signatures." ]
        , h3 [] [ text "Implementing Generic Traits" ]
        , viewCodeBlock """trait Monad<T> = do
    bind :: Self -> (Any -> Self) -> Self
    return :: Any -> Self
end

struct Optional = (value: Any)

impl Monad<Optional> for Optional = do
    bind Optional{value: x} f = f x
    bind None{} f = None{}
    return x = Optional{value: x}
end""" onLoadCode True "indigo"
        , p [] [ text "When implementing a generic trait, you provide the type arguments in angle brackets after the trait name: impl Monad<Optional> for Optional. The compiler validates that the number of type arguments matches the trait's generic parameters." ]
        , p [] [ text "The type arguments are substituted into the trait's method signatures during compilation, allowing the implementation to work with the specific types you've chosen." ]
        , h3 [] [ text "Trait Constraints" ]
        , p [] [ text "Type parameters can be constrained to traits using the colon syntax. This ensures that only types implementing the specified trait can be used:" ]
        , viewCodeBlock """trait Number
impl Number for Int
impl Number for Float

struct Example<N: Number> = (content: N)

let main: IO = do
    # Valid - Int implements Number
    let x = Example<Int>{content: 42}
    
    # Valid - Float implements Number
    let y = Example<Float>{content: 3.14}
    
    # Error - String does not implement Number
    # let z = Example<String>{content: "hello"}
end""" onLoadCode True "indigo"
        , p [] [ text "In the above example, the generic parameter N is constrained to the Number trait. This means you can only use types that implement Number (like Int or Float) when creating Example instances." ]
        ]

viewImportsAndModules : (String -> msg) -> Html msg
viewImportsAndModules onLoadCode =
    section [ id "imports-and-modules" ]
        [ h2 [] [ text "Imports and Modules" ]
        , p [] [ text "Indigo supports organizing code into modules and importing functionality from other modules. This allows you to structure larger programs and reuse code across multiple files." ]
        , h3 [] [ text "Module Declarations" ]
        , p [] [ text "A module declaration at the top of a file defines the module name. The module name is used when other files import from this module:" ]
        , viewCodeBlock """module Module2

let add (a: Int b: Int) : Int = a + b

let multiply (a: Int b: Int) : Int = a * b

let greet (name: String) : String = "Hello, " ++ name ++ "!" """ onLoadCode False "indigo"
        , p [] [ text "In the above example, Module2 is declared as the module name. All functions, structs, and traits defined in this file can be accessed from other modules using this name." ]
        , h3 [] [ text "Unqualified Imports" ]
        , p [] [ text "An unqualified import brings all names from a module into the current scope without a prefix:" ]
        , viewCodeBlock """import Module2

let main : IO = do
    let result = add 5, 3
    println result
end""" onLoadCode False "indigo"
        , p [] [ text "With an unqualified import, you can use functions directly by their name (e.g., add instead of Module2.add). However, this can lead to name conflicts if multiple modules define functions with the same name." ]
        , h3 [] [ text "Qualified Imports" ]
        , p [] [ text "A qualified import requires you to prefix imported names with the module name, preventing name conflicts:" ]
        , viewCodeBlock """import qualified Module2

let main : IO = do
    let result1 = Module2.add 5, 3
    let result2 = Module2.multiply 4, 7
    let message = Module2.greet "World"
    println result1
    println result2
    println message
end""" onLoadCode False "indigo"
        , p [] [ text "Qualified imports use the qualified keyword and require accessing functions through the module name (e.g., Module2.add). This is the recommended approach to avoid naming conflicts." ]
        , h3 [] [ text "Import Aliases" ]
        , p [] [ text "You can create an alias for a module when importing, which is useful for shortening long module names:" ]
        , viewCodeBlock """import qualified Module2 as M2

let main : IO = do
    let result = M2.add 5, 3
    println result
end""" onLoadCode False "indigo"
        , p [] [ text "The as keyword creates an alias for the module. You can use aliases with both qualified and unqualified imports. For qualified imports, the alias replaces the module name when accessing functions." ]
        , h3 [] [ text "Optional from Keyword" ]
        , p [] [ text "The from keyword in import statements is optional. Both of these are equivalent:" ]
        , viewCodeBlock """import Module2
import from Module2""" onLoadCode False "indigo"
        , p [] [ text "You can use either syntax; they behave identically." ]
        , h3 [] [ text "Module Paths" ]
        , p [] [ text "Module names can contain dots, slashes, and @ symbols, allowing you to organize modules in a hierarchical structure or reference files by path:" ]
        , viewCodeBlock """import qualified std.File
import qualified utils/helpers""" onLoadCode False "indigo"
        , p [] [ text "The compiler resolves module names to file paths, looking for corresponding .in files in the configured source directories." ]
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
        , h3 [] [ text "Value Structs" ]
        , p [] [ text "Value structs are a special kind of struct that have exactly one field and can be constructed using type casts. They provide a convenient way to create refined types that can be implicitly or explicitly converted from their underlying type." ]
        , viewCodeBlock """value struct EvenNumber = (num: Int) satisfies ((num % 2) == 0)

let main: IO = do
    # Valid - 12 is even
    println (12 as EvenNumber)
    
    # Error - 5 is not even, refinement fails
    # println (5 as EvenNumber)
    
    # Error - String cannot be cast to EvenNumber
    # println ("hello" as EvenNumber)
end""" onLoadCode True "indigo"
        , p [] [ text "In the above example, EvenNumber is a value struct with a single field num of type Int. The refinement ensures that only even numbers can be cast to EvenNumber. The cast expression 12 as EvenNumber creates an EvenNumber struct with num = 12, but only if the refinement passes at compile time." ]
        , p [] [ text "Value structs can also be defined without refinements, allowing any value of the field type to be cast:" ]
        , viewCodeBlock """value struct PositiveInt = (num: Int)

let main: IO = do
    let x = 42 as PositiveInt
    println x
end""" onLoadCode True "indigo"
        , p [] [ text "When casting to a value struct, the compiler checks:" ]
        , p [] [ text "• That the source type is compatible with the value struct's field type" ]
        , p [] [ text "• That the refinement constraint is satisfied (if a refinement is defined)" ]
        , p [] [ text "If either check fails, or if the refinement cannot be verified at compile time, the compilation will fail with an appropriate error message." ]
        , p [] [ text "Value structs are particularly useful for creating type-safe wrappers around primitive types, ensuring that only values meeting certain criteria can be used in specific contexts." ]
        ]
