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


let retweetsStepWiseChart (history : GameHistory) = 
    let data = retweetsStepWise history
    Chart.Line (data,Color=System.Drawing.Color.Red, XTitle = "Шаг", YTitle ="Число ретвитов")

let notifiedStepWiseChart (history : GameHistory) = 
    let data = notifiedStepWise history
    Chart.Line (data,Color=System.Drawing.Color.Blue, XTitle = "Шаг", YTitle ="Число оповещенных людей")
        