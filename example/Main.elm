port module Main exposing (main)

import Benchmark exposing (Benchmark, benchmark3)
import Benchmark.Runner.Node exposing (BenchmarkProgram, run)
import Json.Encode exposing (Value)


main : BenchmarkProgram
main =
    run emit benchmarks


doCompare : Int -> Benchmark
doCompare size =
    let
        input =
            List.range 0 size
    in
    Benchmark.compare ("size: " ++ toString size)
        (benchmark3 "foldl" List.foldl (+) 0 input)
        (benchmark3 "foldr" List.foldr (+) 0 input)


benchmarks : Benchmark
benchmarks =
    [ 1, 100, 10000 ]
        |> List.map doCompare
        |> Benchmark.describe "Benching folds"


port emit : Value -> Cmd msg
