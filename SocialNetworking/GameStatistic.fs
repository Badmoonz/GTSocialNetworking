module GameStatistic
open GameEngine

let neighborCountDistribution (gameState : GameState) = 
    gameState |> Seq.map(fun x -> x.NeighborsCount) |> Seq.groupBy id |> Seq.sortBy fst |> Seq.map (fun (nc,items) -> string nc, Seq.length items) |> Seq.toArray
