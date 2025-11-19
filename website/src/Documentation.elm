module Documentation exposing (view, Topic(..))

import Html exposing (Html, div, h1, h2, h3, p, pre, code, section, text, a, button, node)
import Html.Attributes exposing (class, id, href, attribute)
import Html.Events exposing (onClick)

type Topic
    = FunctionsAndBindings
    | Comments
    | StructsAndTraits
    | FFI
    | Generics

view : Topic -> (String -> msg) -> Html msg
view currentTopic onLoadCode =
    div [ class "docs-container" ]
        [ viewSidebar
        , div [ class "docs-content" ]
            [ case currentTopic of
                FunctionsAndBindings -> viewFunctionsAndBindings onLoadCode
                Comments -> viewComments onLoadCode
                StructsAndTraits -> viewStructsAndTraits onLoadCode
                FFI -> viewFFI onLoadCode
                Generics -> viewGenerics onLoadCode
            ]
        ]

viewSidebar : Html msg
viewSidebar =
    div [ class "docs-sidebar" ]
        [ h3 [] [ text "Language Basics" ]
        , sidebarLink "Functions & Bindings" "/docs/functions-and-bindings"
        , sidebarLink "Comments" "/docs/comments"
        , sidebarLink "Structs & Traits" "/docs/structs-and-traits"
        , h3 [] [ text "Advanced Features" ]
        , sidebarLink "FFI" "/docs/ffi"
        , sidebarLink "Generics" "/docs/generics"
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
let main = println "Hello " : name""" onLoadCode True "indigo"
        , p [] [ text "The above code defines a binding name and a function main. The function main is called when the program is run." ]
        , viewCodeBlock """let multiple_args (a: Int b: Int) => Int = a + b
let main = print (multiple_args 2, 3)""" onLoadCode True "indigo"
        , p [] [ text "Functions can have multiple arguments. The above function adds two integers. Specifying the return type is optional, specifying parameter types is mandatory currently." ]
        , viewCodeBlock """let println (s: String) => IO = do
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

let main => IO = do
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

let main => IO = do
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

viewGenerics : (String -> msg) -> Html msg
viewGenerics onLoadCode =
    section [ id "generics" ]
        [ h2 [] [ text "Generics" ]
        , p [] [ text "Indigo supports generic types on functions." ]
        , viewCodeBlock """trait Number
impl Number for Int
impl Number for Float

let add<N: Number> (a: N b: N) => N = do
  a + b
end

let main => IO = do
  println add 1, 2
end""" onLoadCode True "indigo"
        , p [] [ text "In the above example, a function add is declared with a generic type parameter N constrained to the Number trait." ]
        , p [] [ text "This function takes two parameters of type N and returns a result of the same type." ]
        , p [] [ text "The call to add only suceeds if the given parameters are of the same Number type. The return type N gets type erased to Number." ]
        ]
