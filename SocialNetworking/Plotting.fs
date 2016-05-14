module Plotting
open GameEngine
open GameStatistic
open FSharp.Charting

let neighBorCountDistributionChart (gameState : GameState) = 
    let items = gameState |> neighborCountDistribution
    Chart.Column(items, Name = @"Распределение числа соседей в графе")
    |> Chart.WithXAxis(Title = @"Число соседей")
    |> Chart.WithYAxis(Title = @"Число агентов")

let functionChart (f : float -> float) = 
    Array.init 100 (fun x -> (float)x / 100.) 
    |> Array.map (fun x -> x , f x)
    |> Chart.Line

        