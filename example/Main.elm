port module Main exposing (main)

import Benchmark.LowLevel exposing (benchmark3)
import Benchmark.Runner.Node as Benchmark exposing (Benchmark, BenchmarkProgram, run)
import Json.Encode exposing (Value)


main : BenchmarkProgram
main =
    run emit <| Benchmark.group "many" [ benchmarks (+) "(+)", benchmarks (-) "(-)" ]


doCompare : (Int -> Int -> Int) -> Int -> Benchmark
doCompare op size =
    let
        input =
            List.range 0 size
    in
    Benchmark.compare ("size: " ++ toString size)
        [ benchmark3 "foldl" List.foldl op 0 input
        , benchmark3 "foldr" List.foldr op 0 input
        ]


benchmarks : (Int -> Int -> Int) -> String -> Benchmark
benchmarks op desc =
    Benchmark.series ("folds : " ++ desc)
        (\s -> "size " ++ toString s)
        (doCompare op)
        [ 1, 100, 10000 ]


port emit : Value -> Cmd msg
