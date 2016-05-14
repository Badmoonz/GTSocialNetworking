#load "../packages/FSharp.Charting.0.90.14/FSharp.Charting.fsx"

#load "GameEngine.fs"
#load "GameStatistic.fs"
#load "Strategies.fs"
#load "Plotting.fs"

open FSharp.Charting
open GameEngine
open Strategies
open Plotting


let testSnobFunction() = 
    let plot = functionChart snobFunction
    plot.ShowChart()

let test() = 
    let game = initGame {NodeCount = 1000; Seed = Some 100}
    let plot = neighBorCountDistributionChart game
    plot.ShowChart() |> ignore

testSnobFunction()

test()
