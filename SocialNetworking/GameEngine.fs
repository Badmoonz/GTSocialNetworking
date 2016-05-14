module GameEngine 
open System

type Graph = bool [,]

type GraphConfig = { NodeCount : int; Seed : int option }

type NodeState = {
        NodeId : int
        Tweeted : bool
        Neighbors : int Set
        NeighborsCount : int
        NotificationCount : int
   }

type ChoiceFunc = NodeState -> bool

type GameState  = NodeState array

type GameHistory = GameState List

type NodeUpdate = {
        Tweeted : bool
        NotificationCount : int
    }
type GameUpdate = NodeUpdate array

let defaultNodeUpdate  = {Tweeted  = false ; NotificationCount = 0}

let mergeNodeUpdates (update1 : NodeUpdate) (update2 : NodeUpdate) = 
    {Tweeted = update1.Tweeted || update2.Tweeted; NotificationCount = update1.NotificationCount + update2.NotificationCount} 
let mergeGameUpdates : GameUpdate -> GameUpdate -> GameUpdate = 
    Array.map2 mergeNodeUpdates
    
let initGraph  { NodeCount = nodesCount; Seed = seed } : Graph = 
    let rnd = match seed with | Some seed' -> System.Random(seed') | _ -> System.Random()
    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    let shuffle x =
        let a = Array.copy x
        Array.iteri (fun i _ -> swap a i (rnd.Next(i, Array.length a))) a
        a
    let rndDistribution : int Set array =
        [|0..(nodesCount - 1)|] 
        |> Array.map(fun i ->
           let neighborsCount = Math.Min(rnd.Next(0,10), Math.Max(0, nodesCount - 1 - i))
           let rndSeq = [|0..(nodesCount - 1)|] |> shuffle |> Seq.take neighborsCount |> Set.ofSeq
           rndSeq
        )
    printfn "generating rndDistribution complete"
    Array2D.init nodesCount nodesCount (fun i j->
        if i < j
        then
            if i + 1 = j 
            then true
            else rndDistribution.[i].Contains(j)
        else false
        )
let tuple2 a b = a,b 

let neighbors (graph : Graph) i = 
    let h = graph.[i,*] |> Seq.mapi tuple2 |> Seq.filter snd |> Seq.map fst |> Set.ofSeq
    let v = graph.[*,i] |> Seq.mapi tuple2 |> Seq.filter snd |> Seq.map fst |> Set.ofSeq
    h + v

let neighborsCount (graph : Graph) i = 
    let h = graph.[i,*] |> Seq.filter id |> Seq.length
    let v = graph.[*,i] |> Seq.filter id |> Seq.length
    h + v

let initGame config : GameState = 
    let graph = initGraph config
    Array.init config.NodeCount (fun id ->
        {
            NodeId = id
            Tweeted = false
            Neighbors = neighbors graph id
            NeighborsCount = neighborsCount graph id
            NotificationCount = 0
        })
let const_ x y = x

let evalNode (f : ChoiceFunc) (game : GameState) nodeId : GameUpdate = 
    let nodesCount = Array.length game
    let nodeState = game.[nodeId]
    if f nodeState
    then 
        Array.init nodesCount (fun nId -> 
            if nId = nodeId then {Tweeted = true; NotificationCount = 0}
            else {Tweeted = false; NotificationCount = if nodeState.Neighbors.Contains(nId) && not game.[nId].Tweeted  then 1 else 0}
            )
    else 
        Array.init nodesCount (const_ defaultNodeUpdate)

let evalGame (f : ChoiceFunc) (gameState : GameState) (nodesToAction : int array) : GameUpdate =
    let nodesCount = Array.length gameState
    let defaultGameUpdate = Array.init nodesCount (const_ defaultNodeUpdate)
    nodesToAction |> Array.map (evalNode f gameState) |> Array.fold mergeGameUpdates defaultGameUpdate

let applyNodeUpdate (nodeState : NodeState) (nodeUpdate : NodeUpdate) = 
    { nodeState with
        Tweeted = nodeState.Tweeted || nodeUpdate.Tweeted
        NotificationCount = nodeState.NotificationCount + nodeUpdate.NotificationCount
    }
let applyGameUpdate (gameState : GameState) (gameUpdate : GameUpdate) = 
    Array.map2 applyNodeUpdate gameState gameUpdate

let findMostConnectedNode (game : GameState) = 
    game |> Array.maxBy (fun x -> x.NeighborsCount)

let play (f : ChoiceFunc) (initGameState : GameState) maxIter : GameHistory = 
    let rec play' (gameState : GameState) (nodesToAction :  int array) iter (history : GameState list) = 
        printfn "[Step #%5i] nodesToAction : %5i" iter nodesToAction.Length
        if iter = maxIter || nodesToAction.Length = 0
        then
            if iter = maxIter 
            then
                printfn "Game stopped on maxIter"
            else 
                printfn "Information flow stop step %5i /%5i" iter maxIter
            gameState, history 
        else
            let gameUpdate = evalGame (if iter = 0 then const_ true else f) gameState nodesToAction
            let newGameState = applyGameUpdate gameState gameUpdate
            let newNodesToAction = gameUpdate |> Array.mapi(fun i x -> (i ,x)) |> Array.filter (fun (_ , x) -> x.NotificationCount > 0) |> Array.map fst
            let newHistory = gameState :: history
            play' newGameState newNodesToAction (iter + 1) newHistory
    
    let mostConnectedNode = findMostConnectedNode initGameState
    printfn "Game started with Node #%i  with %5i connections" mostConnectedNode.NodeId mostConnectedNode.NeighborsCount
    let gameResult, reveresedHistory = play' initGameState [|mostConnectedNode.NodeId|] 0 []
    let history = (gameResult :: reveresedHistory) |> List.rev
    history
