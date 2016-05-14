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


let testGurvitz() =
    let matrix =
        [|
            [|20.; 15.; 10.|]
            [|16.; 12.; 14.|]
            [|13.; 18.; 15.|]
        |]
        |> toProfitMatrix
    let start, result = gurvitz 0.5 matrix
    printfn "matrix : \n %A" matrix
    printfn "labda  : %f" 0.5
    printfn "Result : strategy #%i with result %0.1f , should be #3 with 15.5" (start + 1) result

let testNodesDistribution nodesCount =    
    let game = initGame {NodeCount = nodesCount; Seed = Some 1}
    neighBorCountDistributionChart game
    |> Chart.Show

let testGame choiceFunc nodesCount = 
    let game = initGame {NodeCount = nodesCount; Seed = None}
    let results = play choiceFunc game 50
    let results2 = play alwaysTrueChoiceFunc game 50

    Chart.Combine [
        retweetsStepWiseChart results ""
        notifiedStepWiseChart results ""
        retweetsStepWiseChart results2 "(оптимизм)"
        notifiedStepWiseChart results2 "(оптимизм)"

    ]
    |> Chart.WithLegend(true)
    |> Chart.Show

testGame (gurvitzChoiceFunc 0.1) 1000
//
//testGame (alwaysTrueChoiceFunc) 1000
//
//

let rnd = System.Random(4)
let rndChoiceFunc2 : ChoiceFunc = 
   snobFunction' >> (fun x -> rnd.NextDouble() <= System.Math.Max(0., x)  / 3.) 

testGame (rndChoiceFunc2) 1000
//
//
////testGurvitz()
//
//testNodesDistribution 1000


//testSnobFunction()
