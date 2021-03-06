﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open FSharp.Charting
open GameEngine
open Strategies
open Plotting

let testGame choiceFunc nodesCount = 
    let game = initGame {NodeCount = nodesCount; Seed = None}
    let results = play choiceFunc game 50
    Chart.Combine [
        retweetsStepWiseChart results ""
        notifiedStepWiseChart results ""
    ]
    |> Chart.WithLegend(true)
    |> Chart.Show

let rnd = System.Random()
let rndChoiceFunc : ChoiceFunc = 
   snobFunction' >> (fun x -> rnd.NextDouble() <= System.Math.Max(0., x)  / 2.)       


[<EntryPoint;System.STAThreadAttribute>]
let main argv = 
    testGame (gurvitzChoiceFunc 0.1) 1000

//    testGame (rndChoiceFunc) 1000

    0 // return an integer exit code
