module Strategies
open GameEngine

type ProfitMatrix = float [,]

let profitMatrix x1 : ProfitMatrix = Array2D.init 2 2 (fun i j ->
    match (i,j) with
        | (0,0) -> x1 + 0.5
        | (0,1) -> x1 - 1.
        | (1,0) ->  - x1 / 2.
        | (1,1) -> 0.
        | _     -> 0.
    )

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