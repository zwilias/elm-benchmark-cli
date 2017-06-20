module Benchmark.Runner.Node exposing (BenchmarkProgram, run)

import Benchmark exposing (Benchmark)
import Benchmark.Reporting as Report exposing (Report(..), Stats, Status(..))
import Console
import Json.Encode exposing (Value)
import Platform exposing (program)
import Process
import Task exposing (Task)


unsafeLoop : List a -> ( a, List a )
unsafeLoop l =
    case l of
        [] ->
            Debug.crash "nope"

        x :: xs ->
            ( x, xs ++ [ x ] )


type alias Model =
    { benchmark : Benchmark
    , emit : Value -> Cmd Msg
    , clock : List String
    }


type alias BenchmarkProgram =
    Program Never Model Msg


type Msg
    = Update Benchmark


init : (Value -> Cmd Msg) -> Benchmark -> ( Model, Cmd Msg )
init emit benchmark =
    let
        initialModel =
            { emit = emit
            , benchmark = benchmark
            , clock = [ "🕛", "🕐", "🕑", "🕒", "🕓", "🕔", "🕕", "🕖", "🕗", "🕘", "🕙", "🕚" ]
            }
    in
    initialModel
        ! [ step benchmark |> Maybe.withDefault Cmd.none
          , emit <| start benchmark
          ]


breakForRender : Task x a -> Task x a
breakForRender task =
    Task.andThen (\_ -> task) (Process.sleep 0)


step : Benchmark -> Maybe (Cmd Msg)
step =
    Benchmark.step
        >> Maybe.map breakForRender
        >> Maybe.map (Task.perform Update)


update : Msg -> Model -> ( Model, Cmd Msg )
update (Update benchmark) ({ emit } as model) =
    case step benchmark of
        Just cmd ->
            let
                ( show, clock ) =
                    unsafeLoop model.clock
            in
            { model | benchmark = benchmark, clock = clock } ! [ cmd, emit <| running show ]

        Nothing ->
            { model | benchmark = benchmark } ! [ emit <| done benchmark ]


running : String -> Value
running clock =
    Json.Encode.object
        [ ( "type", Json.Encode.string "running" )
        , ( "data", Json.Encode.string <| "\x0D\t" ++ clock ++ "  wait for it..." )
        ]


done : Benchmark -> Value
done benchmark =
    Json.Encode.object
        [ ( "type", Json.Encode.string "done" )
        , ( "data", Json.Encode.string <| summarize <| Report.fromBenchmark benchmark )
        ]


start : Benchmark -> Value
start bench =
    Json.Encode.object
        [ ( "type", Json.Encode.string "start" )
        , ( "data"
          , Json.Encode.string <|
                Console.bold "\n⏱  Running benchmarks...\n\n"
                    ++ (makePrettyIntro <| Report.fromBenchmark bench)
                    ++ "\n"
          )
        ]


indent : String -> String
indent =
    String.lines
        >> List.map (\s -> "    " ++ s)
        >> String.join "\n"


summarize : Report -> String
summarize report =
    case report of
        Benchmark name status ->
            name
                ++ (case status of
                        ToSize _ ->
                            Debug.crash "oops"

                        Pending _ _ _ ->
                            Debug.crash "oops"

                        Failure _ ->
                            "Failed"

                        Success stats ->
                            toString <| Report.operationsPerSecond stats
                   )

        Compare name left right ->
            "name"
                ++ "\n\t"
                ++ summarize left
                ++ "\t"
                ++ summarize right

        Group name reports ->
            name ++ "\n" ++ (List.map (summarize >> indent) reports |> String.join "\n")


makePrettyIntro : Report -> String
makePrettyIntro report =
    case report of
        Benchmark name _ ->
            name

        Compare name left right ->
            makePrettyIntro left
                ++ " vs. "
                ++ makePrettyIntro right
                ++ " - "
                ++ name

        Group name reports ->
            "→ "
                ++ name
                ++ "\n"
                ++ (List.map (\report -> "  ↳ " ++ makePrettyIntro report) reports |> String.join "\n")


run : (Value -> Cmd Msg) -> Benchmark -> BenchmarkProgram
run emit benchmarks =
    program
        { init = init emit benchmarks
        , update = update
        , subscriptions = always Sub.none
        }
