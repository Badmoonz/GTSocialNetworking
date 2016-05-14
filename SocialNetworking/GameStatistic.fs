module GameStatistic
open GameEngine

let neighborCountDistribution (gameState : GameState) = 
    gameState |> Seq.map(fun x -> x.NeighborsCount) |> Seq.groupBy id |> Seq.sortBy fst |> Seq.map (fun (nc,items) -> string nc, Seq.length items) |> Seq.toArray


let retweetsStepWise (history : GameHistory) = 
    history |> Seq.map (Array.filter (fun state -> state.Tweeted) >> Array.length) |> Seq.toArray

let notifiedStepWise (history : GameHistory) = 
    history |> Seq.map (Array.filter (fun state -> state.NotificationCount > 0) >> Array.length) |> Seq.toArray

