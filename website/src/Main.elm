port module Main exposing (main)

import Browser
import Html exposing (Html, div, text, h1, p, button, header, main_, section, nav, a)
import Html.Attributes exposing (class, id, href)
import Html.Events exposing (onClick)
import Json.Encode as E
import Json.Decode

-- PORTS

port runCode : String -> Cmd msg

-- MODEL

type alias Model =
    { code : String
    , output : String
    }

initialCode : String
initialCode =
    """let main : IO = do
    println "Hello, Indigo!"
end"""

examples : List (String, String)
examples =
    [ ("Hello World", """let main : IO = do
    println "Hello, Indigo!"
end""")
    , ("Fibonacci", """let fib (0: Int) : Int = 0
let fib (1: Int) : Int = 1
let fib (n: Int) : Int = (fib ((n) - 1)) + (fib ((n) - 2))

let main : IO = do
    println fib 12
end""")
    , ("Bottles", """let bottles (i: Int) : IO = do
    if i > 0 then do
        println ^i : " bottles of beer on the wall, " : ^i : " bottles of beer."
        println "Take one down and pass it around, " : ((i) - 1) as String : " bottles of beer on the wall.\\n"
        bottles (i)-1
    else do
        println "No more bottles of beer on the wall, no more bottles of beer."
        println "Go to the store and buy some more, 99 bottles of beer on the wall."
    end
end

let main : IO = do
    bottles 99
end""")
    , ("Structs & Traits", """struct Dog = (name: String)
struct Cat = (name: String)

trait Animal = do
    makeNoise :: Self -> IO
end

impl Animal for Dog = do
    makeNoise self = println "Woof"
end

impl Animal for Cat = do
    makeNoise self = println "Meow"
end

let main : IO = do
    let bello = Dog { name : "Bello" }
    let mauzi = Cat { name : "Mauzi" }
    makeNoise bello
    makeNoise mauzi
end""")
    ]

init : () -> ( Model, Cmd Msg )
init _ =
    ( { code = initialCode
      , output = ""
      }
    , Cmd.none
    )

-- UPDATE

type Msg
    = RunCode
    | CodeChanged String
    | LoadExample String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RunCode ->
            ( model, runCode model.code )

        CodeChanged newCode ->
            ( { model | code = newCode }, Cmd.none )

        LoadExample code ->
            ( { model | code = code }, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    div [ class "app-container" ]
        [ viewHeader
        , main_ [ class "main-content" ]
            [ viewHero
            , viewFeatures
            , viewPlayground model
            ]
        , viewFooter
        ]

viewHeader : Html Msg
viewHeader =
    header [ class "site-header" ]
        [ div [ class "container header-content" ]
            [ h1 [ class "logo" ] [ text "Indigo" ]
            , nav [ class "main-nav" ]
                [ a [ href "#" ] [ text "Home" ]
                , a [ href "#features" ] [ text "Features" ]
                , a [ href "#playground" ] [ text "Playground" ]
                , a [ href "https://github.com/rockofox/indigo" ] [ text "GitHub" ]
                ]
            ]
        ]

viewHero : Html Msg
viewHero =
    section [ class "hero" ]
        [ div [ class "container" ]
            [ h1 [] [ text "The Indigo Programming Language" ]
            , p [ class "subtitle" ] [ text "Modern. Functional. Clear." ]
            , div [ class "hero-buttons" ]
                [ a [ href "#playground", class "cta-button" ] [ text "Try Online" ]
                , a [ href "https://github.com/rockofox/indigo", class "secondary-button" ] [ text "View on GitHub" ]
                ]
            ]
        ]

viewFeatures : Html Msg
viewFeatures =
    section [ id "features", class "features-section" ]
        [ div [ class "container" ]
            [ h2 [] [ text "Features" ]
            , div [ class "features-grid" ]
                [ featureCard "Functional" "Pure functions, immutable data, and first-class functions."
                , featureCard "Typed" "Powerful type system"
                , featureCard "Pattern Matching" "Expressive pattern matching for control flow."
                , featureCard "Traits" "Ad-hoc polymorphism via traits."
                ]
            ]
        ]

featureCard : String -> String -> Html Msg
featureCard title description =
    div [ class "feature-card" ]
        [ Html.h3 [] [ text title ]
        , p [] [ text description ]
        ]

viewPlayground : Model -> Html Msg
viewPlayground model =
    section [ id "playground", class "playground-section" ]
        [ div [ class "container" ]
            [ h2 [] [ text "Try it out" ]
            , div [ class "examples-nav" ]
                (List.map (\(name, code) -> 
                    button [ class "example-btn", onClick (LoadExample code) ] [ text name ]
                ) examples)
            , div [ class "playground-container" ]
                [ div [ class "editor-pane" ]
                    [ Html.node "code-editor"
                        [ Html.Attributes.property "code" (E.string model.code)
                        , Html.Events.on "code-changed" (decodeCodeChanged CodeChanged)
                        ]
                        []
                    ]
                , div [ class "controls-pane" ]
                    [ button [ class "run-button", onClick RunCode ] [ text "Run ▶" ]
                    ]
                , div [ class "terminal-pane" ]
                    [ Html.node "xterm-display" [] []
                    ]
                ]
            ]
        ]

viewFooter : Html Msg
viewFooter =
    Html.footer [ class "site-footer" ]
        [ div [ class "container" ]
            [ p [] [ text "© 2025 Indigo Language. Open Source." ]
            ]
        ]

h2 : List (Html.Attribute msg) -> List (Html msg) -> Html msg
h2 attrs children =
    Html.h2 attrs children

decodeCodeChanged : (String -> msg) -> Json.Decode.Decoder msg
decodeCodeChanged toMsg =
    Json.Decode.field "detail" (Json.Decode.field "code" Json.Decode.string)
        |> Json.Decode.map toMsg



main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
