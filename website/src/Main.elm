port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Documentation
import Html exposing (Html, a, button, div, h1, h2, h3, header, input, main_, nav, p, pre, code, section, span, text)
import Html.Attributes exposing (attribute, class, href, id, title)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Json.Encode as E
import Url
import Url.Parser as Parser exposing ((</>), Parser, map, oneOf, s, top)


-- PORTS


port runCode : String -> Cmd msg

port copyToClipboard : String -> Cmd msg

port shareCode : String -> Cmd msg

port scrollToTop : () -> Cmd msg

port statusChanged : (String -> msg) -> Sub msg


-- MODEL


type Page
    = Home
    | Playground
    | Documentation Documentation.Topic


type Route
    = HomeRoute
    | PlaygroundRoute
    | DocsRoute Documentation.Topic


type alias Flags =
    { initialCode : String }


type PlaygroundView
    = SplitView
    | EditorViewTab
    | OutputViewTab


type alias Model =
    { code : String
    , status : String
    , page : Page
    , key : Nav.Key
    , url : Url.Url
    , docsSearchQuery : String
    , activeExample : String
    , mobileNavOpen : Bool
    , playgroundView : PlaygroundView
    }


defaultCode : String
defaultCode =
    """let main : IO<Unit> = do
    println "Hello, Indigo!"
end"""


examples : List ( String, String )
examples =
    [ ( "Hello World"
      , """let main : IO<Unit> = do
    println "Hello, Indigo!"
end"""
      )
    , ( "Fibonacci"
      , """let fib (0: Int) : Int = 0
let fib (1: Int) : Int = 1
let fib (n: Int) : Int = (fib ((n) - 1)) + (fib ((n) - 2))

let main : IO<Unit> = do
    println ("fib 10 = " ++ (fib 10) as String)
    println ("fib 12 = " ++ (fib 12) as String)
end"""
      )
    , ( "Pattern Matching"
      , """let sumList ([]: [Int]) : Int = 0
let sumList ((x:xs): [Int]) : Int = x + sumList xs

let describe (n: Int) : String =
    when n of
        0 -> "zero"
        1 -> "one"
        _ -> "many"
    end

let main : IO<Unit> = do
    let nums = [10, 20, 30, 40]
    println ("Sum of list: " ++ (sumList nums) as String)
    println ("Description of 1: " ++ describe 1)
end"""
      )
    , ( "Traits & Structs"
      , """type Dog = (name: String)
type Cat = (name: String)

trait Animal = do
    speak :: Self -> IO<Unit>
end

impl Animal for Dog = do
    speak self = println (self.name ++ " says: woof")
end

impl Animal for Cat = do
    speak self = println (self.name ++ " says: meow")
end

let main : IO<Unit> = do
    let buddy = Dog { name: "Buddy" }
    let luna = Cat { name: "Luna" }
    speak buddy
    speak luna
end"""
      )
    , ( "Pipelines"
      , """let double (x: Int) : Int = x * 2
let addTen (x: Int) : Int = x + 10

let main : IO<Unit> = do
    let result = 5 |> double |> addTen
    println ("(5 * 2) + 10 = " ++ result as String)

    let items = [1, 2, 3, 4]
    println ("Sum of [1, 2, 3, 4] = " ++ (items |> sum) as String)
end"""
      )
    , ( "99 Bottles"
      , """let bottles (i: Int) : IO<Unit> = do
    if i > 0 then do
        println (^i ++ " bottles of beer on the wall, " ++ ^i ++ " bottles of beer.")
        println ("Take one down and pass it around, " ++ ((i) - 1) as String ++ " bottles of beer on the wall.\\n")
        bottles (i - 1)
    else do
        println "No more bottles of beer on the wall, no more bottles of beer."
        println "Go to the store and buy some more, 99 bottles of beer on the wall."
    end
end

let main : IO<Unit> = do
    bottles 3
end"""
      )
    ]


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        startingCode =
            if String.isEmpty (String.trim flags.initialCode) then
                defaultCode

            else
                flags.initialCode

        model =
            { code = startingCode
            , status = "Ready"
            , page = Home
            , key = key
            , url = url
            , docsSearchQuery = ""
            , activeExample = "Hello World"
            , mobileNavOpen = False
            , playgroundView = EditorViewTab
            }
    in
    changeRouteTo (urlToRoute url) model


-- ROUTING


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map HomeRoute top
        , map PlaygroundRoute (s "playground")
        , map (DocsRoute Documentation.Overview) (s "docs" </> s "overview")
        , map (DocsRoute Documentation.FunctionsAndBindings) (s "docs" </> s "functions-and-bindings")
        , map (DocsRoute Documentation.Comments) (s "docs" </> s "comments")
        , map (DocsRoute Documentation.LiteralsAndTypes) (s "docs" </> s "literals-and-types")
        , map (DocsRoute Documentation.OperatorsAndPipelines) (s "docs" </> s "operators-and-pipelines")
        , map (DocsRoute Documentation.ControlFlow) (s "docs" </> s "control-flow")
        , map (DocsRoute Documentation.LambdasAndPartials) (s "docs" </> s "lambdas-and-partials")
        , map (DocsRoute Documentation.Lists) (s "docs" </> s "lists")
        , map (DocsRoute Documentation.Tuples) (s "docs" </> s "tuples")
        , map (DocsRoute Documentation.ProductTypes) (s "docs" </> s "product-types")
        , map (DocsRoute Documentation.ProductTypes) (s "docs" </> s "structs-and-traits")
        , map (DocsRoute Documentation.SumTypes) (s "docs" </> s "sum-types")
        , map (DocsRoute Documentation.TraitsAndImpls) (s "docs" </> s "traits-and-impls")
        , map (DocsRoute Documentation.Generics) (s "docs" </> s "generics")
        , map (DocsRoute Documentation.CastingAndValueTypes) (s "docs" </> s "casting-and-value-types")
        , map (DocsRoute Documentation.RefinementTypes) (s "docs" </> s "refinement-types")
        , map (DocsRoute Documentation.PatternMatching) (s "docs" </> s "pattern-matching")
        , map (DocsRoute Documentation.IOAndSequencing) (s "docs" </> s "io-and-sequencing")
        , map (DocsRoute Documentation.ImportsAndModules) (s "docs" </> s "imports-and-modules")
        , map (DocsRoute Documentation.FFI) (s "docs" </> s "ffi")
        , map (DocsRoute Documentation.Prelude) (s "docs" </> s "prelude")
        , map (DocsRoute Documentation.StdFile) (s "docs" </> s "stdlib-file")
        , map (DocsRoute Documentation.ProjectConfig) (s "docs" </> s "project-config")
        , map (DocsRoute Documentation.Overview) (s "docs")
        ]


urlToRoute : Url.Url -> Route
urlToRoute url =
    Maybe.withDefault HomeRoute (Parser.parse routeParser url)


changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case route of
        HomeRoute ->
            ( { model | page = Home, mobileNavOpen = False }, scrollToTop () )

        PlaygroundRoute ->
            ( { model | page = Playground, mobileNavOpen = False }, scrollToTop () )

        DocsRoute topic ->
            ( { model | page = Documentation topic, mobileNavOpen = False }, scrollToTop () )


-- UPDATE


type Msg
    = RunCode
    | CodeChanged String
    | SelectExample String String
    | CopyCode
    | ShareCode
    | ClearCode
    | ResetCode
    | StatusReceived String
    | DocsSearchChanged String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | LoadAndRunCode String
    | ToggleMobileNav
    | CloseMobileNav
    | SetPlaygroundView PlaygroundView


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RunCode ->
            ( { model | status = "Running...", playgroundView = OutputViewTab }, runCode model.code )

        CodeChanged newCode ->
            ( { model | code = newCode }, Cmd.none )

        SelectExample name code ->
            ( { model | code = code, activeExample = name, status = "Loaded " ++ name, playgroundView = EditorViewTab }, Cmd.none )

        CopyCode ->
            ( model, copyToClipboard model.code )

        ShareCode ->
            ( model, shareCode model.code )

        ClearCode ->
            ( { model | code = "", status = "Cleared editor" }, Cmd.none )

        ResetCode ->
            ( { model | code = defaultCode, activeExample = "Hello World", status = "Reset to default", playgroundView = EditorViewTab }, Cmd.none )

        StatusReceived newStatus ->
            ( { model | status = newStatus }, Cmd.none )

        DocsSearchChanged query ->
            ( { model | docsSearchQuery = query }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( { model | mobileNavOpen = False }, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( { model | mobileNavOpen = False }, Nav.load href )

        UrlChanged url ->
            changeRouteTo (urlToRoute url) { model | url = url, mobileNavOpen = False }

        LoadAndRunCode code ->
            ( { model | code = code, activeExample = "Snippet", status = "Running...", playgroundView = OutputViewTab, mobileNavOpen = False }
            , Cmd.batch
                [ Nav.pushUrl model.key "/playground"
                , runCode code
                ]
            )

        ToggleMobileNav ->
            ( { model | mobileNavOpen = not model.mobileNavOpen }, Cmd.none )

        CloseMobileNav ->
            ( { model | mobileNavOpen = False }, Cmd.none )

        SetPlaygroundView pView ->
            ( { model | playgroundView = pView }, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    statusChanged StatusReceived


-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        pageTitle =
            case model.page of
                Home ->
                    "Indigo Programming Language"

                Playground ->
                    "Playground - Indigo"

                Documentation topic ->
                    Documentation.topicTitle topic ++ " - Indigo Documentation"
    in
    { title = pageTitle
    , body =
        [ div [ class "app-container" ]
            [ viewHeader model
            , main_ [ class "main-content" ]
                (case model.page of
                    Home ->
                        [ viewHero model
                        , viewFeatures
                        , viewQuickstart
                        ]

                    Playground ->
                        [ viewPlayground model ]

                    Documentation topic ->
                        [ Documentation.view topic model.docsSearchQuery DocsSearchChanged LoadAndRunCode ]
                )
            , viewFooter
            ]
        ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    let
        currentPath =
            model.url.path

        isActive href =
            if String.startsWith "http" href then
                False

            else if href == "/" then
                currentPath == "/"

            else if href == "/docs" then
                currentPath == "/docs" || String.startsWith "/docs/" currentPath

            else
                currentPath == href || String.startsWith (href ++ "/") currentPath
    in
    header [ class "site-header" ]
        [ div [ class "container header-content" ]
            [ div [ class "header-brand" ]
                [ a [ href "/", class "logo-link", onClick CloseMobileNav ]
                    [ span [ class "logo" ] [ text "indigo" ]
                    ]
                , button
                    [ class ("mobile-nav-toggle" ++ (if model.mobileNavOpen then " is-open" else ""))
                    , onClick ToggleMobileNav
                    , attribute "aria-label" "Toggle navigation menu"
                    , attribute "aria-expanded" (if model.mobileNavOpen then "true" else "false")
                    ]
                    [ span [ class "nav-toggle-bar" ] []
                    , span [ class "nav-toggle-bar" ] []
                    , span [ class "nav-toggle-bar" ] []
                    ]
                ]
            , nav [ class ("main-nav" ++ (if model.mobileNavOpen then " is-open" else "")) ]
                [ a [ href "/", class (if isActive "/" then "active" else ""), onClick CloseMobileNav ] [ text "Home" ]
                , a [ href "/playground", class (if isActive "/playground" then "active" else ""), onClick CloseMobileNav ] [ text "Playground" ]
                , a [ href "/docs", class (if isActive "/docs" then "active" else ""), onClick CloseMobileNav ] [ text "Documentation" ]
                , a [ href "https://github.com/rockofox/indigo", attribute "target" "_blank", attribute "rel" "noopener noreferrer", class "nav-github" ] [ text "GitHub ↗" ]
                ]
            ]
        ]


viewHero : Model -> Html Msg
viewHero _ =
    section [ class "hero" ]
        [ div [ class "container hero-container" ]
            [ div [ class "hero-left" ]
                [ h1 [ class "hero-title" ] [ text "Indigo" ]
                , p [ class "subtitle" ]
                    [ text "An experimental functional programming language inspired by F# and Haskell. Compiles to native x86-64 binaries, bytecode for a custom VM, and WebAssembly for the browser."
                    ]
                , div [ class "hero-buttons" ]
                    [ a [ href "/playground", class "btn btn-primary" ] [ text "Try in Playground" ]
                    , a [ href "/docs/overview", class "btn btn-secondary" ] [ text "Read Documentation" ]
                    , a [ href "https://github.com/rockofox/indigo", class "btn btn-secondary", attribute "target" "_blank", attribute "rel" "noopener noreferrer" ] [ text "GitHub ↗" ]
                    ]
                ]
            , div [ class "hero-right" ]
                [ div [ class "hero-code-card" ]
                    [ div [ class "card-window-header" ]
                        [ span [ class "window-title" ] [ text "example.in" ]
                        , a [ href "/playground", class "hero-card-run" ] [ text "Open in playground ▶" ]
                        ]
                    , div [ class "card-editor-wrap" ]
                        [ Html.node "code-editor"
                            [ Html.Attributes.property "code"
                                (E.string """type Dog = (name: String)
trait Animal = do
    speak :: Self -> IO<Unit>
end

impl Animal for Dog = do
    speak self = println (self.name ++ " says: woof")
end

let main : IO<Unit> = do
    let buddy = Dog { name: "Buddy" }
    speak buddy
end""")
                            , Html.Attributes.attribute "read-only" "true"
                            , Html.Attributes.attribute "auto-height" "true"
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]


viewFeatures : Html Msg
viewFeatures =
    section [ id "features", class "features-section" ]
        [ div [ class "container" ]
            [ h2 [ class "section-title" ] [ text "Language features" ]
            , div [ class "features-grid" ]
                [ featureCard "Expressions and bindings" "Functions and constants both use let. Values are immutable and evaluated as expressions."
                , featureCard "Product and sum types" "Records and variants are declared with the type keyword, with pattern matching for destructuring."
                , featureCard "Traits and implementations" "Define shared method signatures with trait and implement them for concrete types with impl."
                , featureCard "Pattern matching" "Destructure constructors, lists, and tuples in multi-clause functions and when blocks."
                , featureCard "Native compilation and VM" "Compiles directly to native x86-64 binaries, bytecode for a custom stack VM, or WebAssembly for the browser."
                , featureCard "Language server and tools" "Includes a full Language Server Protocol (LSP) implementation, REPL, and code formatter."
                ]
            ]
        ]


viewQuickstart : Html Msg
viewQuickstart =
    section [ class "quickstart-section" ]
        [ div [ class "container" ]
            [ h2 [ class "section-title" ] [ text "Quick start" ]
            , p [ class "section-desc" ] [ text "Clone the repository and run with Cabal or Nix:" ]
            , pre [ class "cli-box" ]
                [ code []
                    [ text """git clone https://github.com/rockofox/indigo.git
cd indigo
cabal run indigo -- examples/fib.in
cabal run indigo -- -c examples/fib.in -o fib   # Compile native binary
cabal run indigo                               # Start REPL"""
                    ]
                ]
            ]
        ]


featureCard : String -> String -> Html Msg
featureCard title description =
    div [ class "feature-card" ]
        [ h3 [] [ text title ]
        , p [] [ text description ]
        ]


viewPlayground : Model -> Html Msg
viewPlayground model =
    section [ id "playground", class "playground-section" ]
        [ div [ class "container playground-wrapper" ]
            [ div [ class "playground-toolbar" ]
                [ div [ class "toolbar-examples-row" ]
                    [ span [ class "toolbar-label" ] [ text "Examples:" ]
                    , div [ class "examples-pills" ]
                        (List.map
                            (\( name, code ) ->
                                button
                                    [ class
                                        (if model.activeExample == name then
                                            "example-pill active"

                                         else
                                            "example-pill"
                                        )
                                    , onClick (SelectExample name code)
                                    ]
                                    [ text name ]
                            )
                            examples
                        )
                    ]
                , div [ class "toolbar-actions-row" ]
                    [ div [ class "status-indicator" ]
                        [ span [ class "status-dot" ] []
                        , span [ class "status-text" ] [ text model.status ]
                        ]
                    , div [ class "toolbar-buttons" ]
                        [ button [ class "btn-tool", onClick CopyCode, title "Copy code to clipboard" ]
                            [ text "Copy" ]
                        , button [ class "btn-tool", onClick ShareCode, title "Copy shareable link" ]
                            [ text "Share" ]
                        , button [ class "btn-tool", onClick ResetCode, title "Reset code" ]
                            [ text "Reset" ]
                        , button [ class "btn-run", onClick RunCode, title "Run (Ctrl+Enter)" ]
                            [ text "▶ Run" ]
                        ]
                    ]
                ]
            , div [ class "mobile-view-tabs" ]
                [ button
                    [ class (if model.playgroundView == EditorViewTab then "mobile-tab active" else "mobile-tab")
                    , onClick (SetPlaygroundView EditorViewTab)
                    ]
                    [ text "Editor (main.in)" ]
                , button
                    [ class (if model.playgroundView == OutputViewTab then "mobile-tab active" else "mobile-tab")
                    , onClick (SetPlaygroundView OutputViewTab)
                    ]
                    [ text "Output" ]
                , button
                    [ class (if model.playgroundView == SplitView then "mobile-tab active" else "mobile-tab")
                    , onClick (SetPlaygroundView SplitView)
                    ]
                    [ text "Split View" ]
                ]
            , div
                [ class
                    ("playground-split-view view-"
                        ++ (case model.playgroundView of
                                SplitView ->
                                    "split"

                                EditorViewTab ->
                                    "editor-only"

                                OutputViewTab ->
                                    "output-only"
                           )
                    )
                ]
                [ div [ class "pane editor-pane" ]
                    [ div [ class "pane-header" ]
                        [ span [ class "pane-tab active" ] [ text "main.in" ]
                        , button [ class "pane-quick-run", onClick RunCode, title "Run code" ] [ text "▶ Run" ]
                        ]
                    , div [ class "pane-body editor-body" ]
                        [ Html.node "code-editor"
                            [ Html.Attributes.property "code" (E.string model.code)
                            , Html.Events.on "code-changed" (decodeCodeChanged CodeChanged)
                            ]
                            []
                        ]
                    ]
                , div [ class "pane terminal-pane" ]
                    [ div [ class "pane-header" ]
                        [ span [ class "pane-tab active" ] [ text "Output" ]
                        , span [ class "pane-status" ] [ text model.status ]
                        ]
                    , div [ class "pane-body terminal-body" ]
                        [ Html.node "xterm-display" [] []
                        ]
                    ]
                ]
            ]
        ]


viewFooter : Html Msg
viewFooter =
    Html.footer [ class "site-footer" ]
        [ div [ class "container footer-content" ]
            [ div [ class "footer-left" ]
                [ p [ class "footer-title" ] [ text "Indigo" ]
                , p [ class "footer-desc" ] [ text "An experimental functional programming language inspired by F# and Haskell. Licensed under the MIT License." ]
                ]
            , div [ class "footer-right" ]
                [ a [ href "/docs/overview" ] [ text "Documentation" ]
                , a [ href "/playground" ] [ text "Playground" ]
                , a [ href "https://github.com/rockofox/indigo", attribute "target" "_blank", attribute "rel" "noopener noreferrer" ] [ text "GitHub ↗" ]
                , a [ href "https://github.com/rockofox/indigo/issues", attribute "target" "_blank", attribute "rel" "noopener noreferrer" ] [ text "Issues ↗" ]
                ]
            ]
        ]


decodeCodeChanged : (String -> msg) -> Json.Decode.Decoder msg
decodeCodeChanged toMsg =
    Json.Decode.field "detail" (Json.Decode.field "code" Json.Decode.string)
        |> Json.Decode.map toMsg


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


