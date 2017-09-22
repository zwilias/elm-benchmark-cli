module Benchmark.Runner.Node exposing (Benchmark, BenchmarkProgram, compare, group, run, series, single)

import Benchmark.LowLevel as LowLevel
import Console
import Dict exposing (Dict)
import Json.Encode exposing (Value)
import Platform exposing (program)
import Process
import Task exposing (Task)


single : LowLevel.Benchmark -> Benchmark
single =
    Single


group : String -> List Benchmark -> Benchmark
group =
    Group


compare : String -> List LowLevel.Benchmark -> Benchmark
compare name benches =
    case benches of
        x :: y :: xs ->
            Compare name x (y :: xs)

        _ ->
            Debug.crash "you need to provide at least two benchmarks to compare"


series : String -> (a -> String) -> (a -> Benchmark) -> List a -> Benchmark
series name toDescription toBenchmark cases =
    List.map (\aCase -> ( toDescription aCase, toBenchmark aCase )) cases
        |> Dict.fromList
        |> Series name


type Benchmark
    = Single LowLevel.Benchmark
    | Compare String LowLevel.Benchmark (List LowLevel.Benchmark)
    | Group String (List Benchmark)
    | Series String (Dict String Benchmark)


maybeTaskList :
    (a -> Maybe (Task e a))
    -> List a
    -> Maybe (Task e (List a))
maybeTaskList toMaybeTask list =
    let
        tasks =
            List.map toMaybeTask list
    in
    if List.all ((==) Nothing) tasks then
        Nothing
    else
        List.map2
            (\maybeTask original ->
                Maybe.withDefault (Task.succeed original) maybeTask
            )
            tasks
            list
            |> Task.sequence
            |> Just


step : Benchmark -> Maybe (Task Never Benchmark)
step structure =
    case structure of
        Single bench ->
            LowLevel.step bench |> Maybe.map (Task.map Single)

        Compare name baseline cases ->
            case ( LowLevel.step baseline, maybeTaskList LowLevel.step cases ) of
                ( Nothing, Nothing ) ->
                    Nothing

                ( left, right ) ->
                    Task.map2 (Compare name)
                        (Maybe.withDefault (Task.succeed baseline) left)
                        (Maybe.withDefault (Task.succeed cases) right)
                        |> Just

        Group name entries ->
            case maybeTaskList step entries of
                Nothing ->
                    Nothing

                Just tasks ->
                    Task.map (Group name) tasks |> Just

        Series name entries ->
            let
                tasks =
                    Dict.toList entries
                        |> List.filterMap
                            (\( key, entry ) ->
                                entry
                                    |> step
                                    |> Maybe.map (Task.map ((,) key))
                            )
            in
            case tasks of
                [] ->
                    Nothing

                _ ->
                    List.foldr
                        (Task.map2
                            (\( key, value ) dict ->
                                Dict.insert key value dict
                            )
                        )
                        (Task.succeed entries)
                        tasks
                        |> Task.map (Series name)
                        |> Just


type alias Model =
    { benchmarks : Benchmark
    , emit : Value -> Cmd Msg
    }


type alias BenchmarkProgram =
    Program Never Model Msg


type Msg
    = Update Benchmark


init : (Value -> Cmd Msg) -> Benchmark -> ( Model, Cmd Msg )
init emit benchmarks =
    { emit = emit
    , benchmarks = benchmarks
    }
        ! [ stepCmd benchmarks |> Maybe.withDefault Cmd.none
          , emit <| running benchmarks
          , emit <| start benchmarks
          ]


breakForRender : Task x a -> Task x a
breakForRender task =
    Process.sleep 0 |> Task.andThen (always task)


stepCmd : Benchmark -> Maybe (Cmd Msg)
stepCmd benchmark =
    step benchmark
        |> Maybe.map breakForRender
        |> Maybe.map (Task.perform Update)


update : Msg -> Model -> ( Model, Cmd Msg )
update (Update benchmark) ({ emit } as model) =
    case stepCmd benchmark of
        Just cmd ->
            { model | benchmarks = benchmark } ! [ cmd, emit <| running benchmark ]

        Nothing ->
            { model | benchmarks = benchmark } ! [ emit <| done benchmark ]


type Progress
    = Sizing
    | InProgress ProgressStats
    | Invalid


type alias ProgressStats =
    { current : Float, total : Float, errors : Int }


combine :
    Progress
    -> Progress
    -> Progress
combine left right =
    case ( left, right ) of
        ( InProgress a, InProgress b ) ->
            InProgress
                { current = a.current + b.current
                , total = a.total + b.total
                , errors = a.errors + b.errors
                }

        ( Invalid, _ ) ->
            Invalid

        ( _, Invalid ) ->
            Invalid

        _ ->
            Sizing


stats : Benchmark -> Progress
stats structure =
    case structure of
        Single bench ->
            LowLevel.status bench |> statusToStats

        Compare _ baseLine cases ->
            List.foldr combine
                (LowLevel.status baseLine |> statusToStats)
                (List.map (LowLevel.status >> statusToStats) cases)

        Group _ reports ->
            List.foldr
                (\report acc_ ->
                    case acc_ of
                        Nothing ->
                            Just <| stats report

                        Just acc ->
                            Just <| combine acc (stats report)
                )
                Nothing
                reports
                |> Maybe.withDefault Invalid

        Series name entries ->
            Dict.foldl
                (\_ report acc_ ->
                    case acc_ of
                        Nothing ->
                            Just <| stats report

                        Just acc ->
                            Just <| combine acc (stats report)
                )
                Nothing
                entries
                |> Maybe.withDefault Invalid


statusToStats : LowLevel.Status -> Progress
statusToStats status =
    case status of
        LowLevel.ToSize total ->
            Sizing

        LowLevel.Pending total _ samples ->
            InProgress
                { current = min total (List.sum samples)
                , total = total
                , errors = 0
                }

        LowLevel.Failure _ ->
            InProgress { current = 0, total = 0, errors = 1 }

        LowLevel.Success _ samples ->
            let
                total : Float
                total =
                    List.sum samples
            in
            InProgress { current = total, total = total, errors = 0 }


running : Benchmark -> Value
running report =
    Json.Encode.object
        [ ( "type", Json.Encode.string "running" )
        , ( "data"
          , Json.Encode.string <| "\x0D" ++ progressBar 72 (stats report)
          )
        ]


progressBar : Int -> Progress -> String
progressBar width progress =
    case progress of
        Sizing ->
            "Sizing..."

        Invalid ->
            "Invalid benchmark structure."

        InProgress { current, total, errors } ->
            let
                done : Int
                done =
                    (current * 8 * toFloat width)
                        / total
                        |> floor

                toGo : Int
                toGo =
                    width - ceiling (toFloat done / 8)

                percentDone : Int
                percentDone =
                    (current * toFloat 100)
                        / total
                        |> floor

                error : String
                error =
                    if errors > 0 then
                        " " ++ toString errors ++ " errors"
                    else
                        ""
            in
            [ "▕"
            , String.repeat (done // 8) "█"
            , block (done % 8)
            , String.repeat toGo "·"
            , "▏"
            , String.padLeft 4 ' ' (toString percentDone ++ "%")
            , error
            ]
                |> String.concat


block : Int -> String
block i =
    case i of
        1 ->
            "▏"

        2 ->
            "▎"

        3 ->
            "▍"

        4 ->
            "▌"

        5 ->
            "▋"

        6 ->
            "▊"

        7 ->
            "▉"

        8 ->
            "█"

        _ ->
            ""


done : Benchmark -> Value
done report =
    Json.Encode.object
        [ ( "type", Json.Encode.string "done" )
        , ( "msg", Json.Encode.string <| "\x0D" ++ progressBar 72 (stats report) )
        , ( "data", encode report |> Maybe.withDefault (Json.Encode.object []) )
        ]


start : Benchmark -> Value
start report =
    Json.Encode.object
        [ ( "type", Json.Encode.string "start" )
        , ( "data"
          , Json.Encode.string <|
                Console.bold "\n⏱  Running benchmarks...\n\n"
                    ++ makePrettyIntro report
                    ++ "\n"
          )
        ]


indent : Int -> String -> String
indent level =
    (++) (String.repeat level "    ")


makePrettyIntro : Benchmark -> String
makePrettyIntro =
    makePrettyIntroLines >> String.join "\n"


makePrettyIntroLines : Benchmark -> List String
makePrettyIntroLines structure =
    case structure of
        Single benchmark ->
            [ LowLevel.name benchmark ]

        Compare comparison baseline cases ->
            ("Compare: " ++ comparison)
                :: (("→ " ++ LowLevel.name baseline) |> indent 1)
                :: List.map (LowLevel.name >> (++) "↝ " >> indent 1) cases

        Group group reports ->
            ("↳ " ++ group)
                :: List.concatMap
                    (makePrettyIntroLines >> List.map (indent 1))
                    reports

        Series name variations ->
            ("Series - " ++ name)
                :: (Dict.toList variations
                        |> List.concatMap
                            (\( name, variation ) ->
                                ("Variation: " ++ name)
                                    :: (variation |> makePrettyIntroLines |> List.map (indent 1))
                            )
                        |> List.map (indent 1)
                   )


encode : Benchmark -> Maybe Value
encode benchmark =
    case benchmark of
        Single bench ->
            encodeBench bench

        Compare name baseline cases ->
            Maybe.map
                (\baseObject ->
                    [ ( "name", Json.Encode.string name )
                    , ( "baseline", baseObject )
                    , ( "cases", Json.Encode.list (List.filterMap encodeBench cases) )
                    ]
                        |> Json.Encode.object
                )
                (encodeBench baseline)

        Group name members ->
            case List.filterMap encode members of
                [] ->
                    Nothing

                encodedMembers ->
                    [ ( "name", Json.Encode.string name )
                    , ( "entries", Json.Encode.list encodedMembers )
                    ]
                        |> Json.Encode.object
                        |> Just

        Series name variations ->
            case List.filterMap (mapTupleToMaybe encode) (Dict.toList variations) of
                [] ->
                    Nothing

                encodedEntries ->
                    [ ( "name", Json.Encode.string name )
                    , ( "variations", Json.Encode.object encodedEntries )
                    ]
                        |> Json.Encode.object
                        |> Just


mapTupleToMaybe : (b -> Maybe c) -> ( a, b ) -> Maybe ( a, c )
mapTupleToMaybe toMaybe ( a, b ) =
    toMaybe b |> Maybe.map ((,) a)


encodeBench : LowLevel.Benchmark -> Maybe Value
encodeBench bench =
    bench
        |> LowLevel.status
        |> encodeStats
        |> Maybe.map
            (\attrs ->
                ([ ( "name", LowLevel.name bench |> Json.Encode.string ) ]
                    ++ attrs
                )
                    |> Json.Encode.object
            )


encodeStats : LowLevel.Status -> Maybe (List ( String, Value ))
encodeStats status =
    case status of
        LowLevel.ToSize _ ->
            Nothing

        LowLevel.Pending _ _ _ ->
            Nothing

        LowLevel.Failure e ->
            [ ( "error", Json.Encode.string <| toString e ) ]
                |> Just

        LowLevel.Success runs samples ->
            [ ( "runs", Json.Encode.int runs )
            , ( "samples", Json.Encode.list (List.map Json.Encode.float samples) )
            ]
                |> Just


run : (Value -> Cmd Msg) -> Benchmark -> BenchmarkProgram
run emit benchmarks =
    program
        { init = init emit benchmarks
        , update = update
        , subscriptions = always Sub.none
        }
