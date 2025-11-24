port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Documentation
import Html exposing (Html, a, button, div, h1, h2, header, main_, nav, p, section, text)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode as E
import Url
import Url.Parser as Parser exposing ((</>), Parser, map, oneOf, s, top)


-- PORTS


port runCode : String -> Cmd msg


-- MODEL


type Page
    = Home
    | Playground
    | Documentation Documentation.Topic


type Route
    = HomeRoute
    | PlaygroundRoute
    | DocsRoute Documentation.Topic


type alias Model =
    { code : String
    , output : String
    , page : Page
    , key : Nav.Key
    , url : Url.Url
    }


initialCode : String
initialCode =
    """let main : IO = do
    println "Hello, Indigo!"
end"""


examples : List ( String, String )
examples =
    [ ( "Hello World", """let main : IO = do
    println "Hello, Indigo!"
end""" )
    , ( "Fibonacci", """let fib (0: Int) : Int = 0
let fib (1: Int) : Int = 1
let fib (n: Int) : Int = (fib ((n) - 1)) + (fib ((n) - 2))

let main : IO = do
    println fib 12
end""" )
    , ( "Bottles", """let bottles (i: Int) : IO = do
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
end""" )
    , ( "Structs & Traits", """struct Dog = (name: String)
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
end""" )
    ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        model =
            { code = initialCode
            , output = ""
            , page = Home
            , key = key
            , url = url
            }
    in
    changeRouteTo (urlToRoute url) model


-- ROUTING


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map HomeRoute top
        , map PlaygroundRoute (s "playground")
        , map (DocsRoute Documentation.FunctionsAndBindings) (s "docs" </> s "functions-and-bindings")
        , map (DocsRoute Documentation.Comments) (s "docs" </> s "comments")
        , map (DocsRoute Documentation.PatternMatching) (s "docs" </> s "pattern-matching")
        , map (DocsRoute Documentation.StructsAndTraits) (s "docs" </> s "structs-and-traits")
        , map (DocsRoute Documentation.FFI) (s "docs" </> s "ffi")
        , map (DocsRoute Documentation.Generics) (s "docs" </> s "generics")
        , map (DocsRoute Documentation.RefinementTypes) (s "docs" </> s "refinement-types")
        , map (DocsRoute Documentation.FunctionsAndBindings) (s "docs")
        ]


urlToRoute : Url.Url -> Route
urlToRoute url =
    Maybe.withDefault HomeRoute (Parser.parse routeParser url)


changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case route of
        HomeRoute ->
            ( { model | page = Home }, Cmd.none )

        PlaygroundRoute ->
            ( { model | page = Playground }, Cmd.none )

        DocsRoute topic ->
            ( { model | page = Documentation topic }, Cmd.none )


-- UPDATE


type Msg
    = RunCode
    | CodeChanged String
    | LoadExample String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ChangeDocTopic Documentation.Topic
    | LoadAndRunCode String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RunCode ->
            ( model, runCode model.code )

        CodeChanged newCode ->
            ( { model | code = newCode }, Cmd.none )

        LoadExample code ->
            ( { model | code = code }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            changeRouteTo (urlToRoute url) { model | url = url }

        ChangeDocTopic topic ->
            let
                path =
                    case topic of
                        Documentation.FunctionsAndBindings ->
                            "/docs/functions-and-bindings"

                        Documentation.Comments ->
                            "/docs/comments"

                        Documentation.PatternMatching ->
                            "/docs/pattern-matching"

                        Documentation.StructsAndTraits ->
                            "/docs/structs-and-traits"

                        Documentation.FFI ->
                            "/docs/ffi"

                        Documentation.Generics ->
                            "/docs/generics"

                        Documentation.RefinementTypes ->
                            "/docs/refinement-types"
            in
            ( model, Nav.pushUrl model.key path )

        LoadAndRunCode code ->
            ( { model | code = code }, Nav.pushUrl model.key "/playground" )


-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Indigo Programming Language"
    , body =
        [ div [ class "app-container" ]
            [ viewHeader
            , main_ [ class "main-content" ]
                (case model.page of
                    Home ->
                        [ viewHero
                        , viewFeatures
                        ]

                    Playground ->
                        [ viewPlayground model ]

                    Documentation topic ->
                        [ Documentation.view topic LoadAndRunCode ]
                )
            , viewFooter
            ]
        ]
    }


viewHeader : Html Msg
viewHeader =
    header [ class "site-header" ]
        [ div [ class "container header-content" ]
            [ h1 [ class "logo" ] [ text "Indigo" ]
            , nav [ class "main-nav" ]
                [ a [ href "/" ] [ text "Home" ]
                , a [ href "/playground" ] [ text "Playground" ]
                , a [ href "/docs" ] [ text "Documentation" ]
                , a [ href "https://github.com/rockofox/indigo" ] [ text "GitHub" ]
                ]
            ]
        ]


viewHero : Html Msg
viewHero =
    section [ class "hero" ]
        [ div [ class "container" ]
            [ h1 [] [ text "Indigo" ]
            , p [ class "subtitle" ] [ text "Modern. Functional. Clear." ]
            , div [ class "hero-buttons" ]
                [ a [ href "/playground", class "cta-button" ] [ text "Try Online" ]
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
                (List.map
                    (\( name, code ) ->
                        button [ class "example-btn", onClick (LoadExample code) ] [ text name ]
                    )
                    examples
                )
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
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
