module Strategies
open System
open GameEngine

type ProfitMatrix = float [,]

let toProfitMatrix (matrix : float [][]) : ProfitMatrix = Array2D.init matrix.Length matrix.[0].Length (fun i j -> matrix.[i].[j])

let calcProfitMatrix x1 : ProfitMatrix =
    let matrix = 
        [|
            [| x1 + 0.5 ; x1 - 1.|]
            [| - x1 / 2.; 0.     |]
        |] 
    toProfitMatrix matrix


let snobFunction x = 
    let y0 = 0.5 //значение функции в точке 0
    let yMax = 1.  // значение функции в точке xMax
    let xMax = 0.5  // точка перелома
    let yMin = -0.5 // значение функции в точке 1
    if x <= xMax
    then y0 + (yMax - y0) / xMax * x
    else yMax - (yMax - yMin) / (1. - xMax) * (x - xMax)


let snobFunction' (node : NodeState) =
    let x = (float)node.NotificationCount /(float)node.NeighborsCount
    snobFunction x

// lambda in [0,1]
let gurvitz lambda (matrix : ProfitMatrix) = 
    let iSlice = Array.init (Array2D.length1 matrix) (fun i -> matrix.[i,*])
    let optimistic = iSlice |> Array.map Array.max 
    let vald       = iSlice |> Array.map Array.min
    let map2 = Array.map2 (fun o v ->  lambda * o + (1. - lambda) * v) optimistic vald
//    printfn "gurvitz choice %A" map2
    map2 |> Array.mapi( fun i x -> i,x) |> Array.maxBy snd


/// Стратегии

let gurvitzChoiceFunc lambda : ChoiceFunc = 
    snobFunction' >> calcProfitMatrix >> (gurvitz lambda) >> (function | (0,_) -> true | _ -> false)

let alwaysTrueChoiceFunc : ChoiceFunc = 
   const_ true

let alwaysFalseChoiceFunc : ChoiceFunc = 
   const_ false

let rnd = System.Random()
let rndChoiceFunc : ChoiceFunc = 
   snobFunction' >> (fun x -> rnd.NextDouble() <= Math.Max(0., x)) 

