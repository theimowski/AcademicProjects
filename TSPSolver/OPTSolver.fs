module MetricTSP.OPTSolver

open System
open Common

/// The following algorithm is based on this publication:
/// M. J. Grimm - "A Simple Alghoritm for the Metric Travelling Salesman Problem" 
/// available under http://ipnpr.jpl.nasa.gov/progress_report/42-78/78I.PDF


/// represents node in a search tree
type private Node = {Path : seq<int>; Cost : float}

/// helper mutable value to keep the costs matrix
let mutable private costs : _[,] = null

/// for the given parent node finds all its children, that match the following criteria:
/// lower bound calculated for a child is smaller than the upper bound passed to function
/// child's lower bound is sum of the parent's cost and cost of replacing edge between vertices (u,v)
/// with two edges (u,v') and (v',v) where v' is vertex represented by the child node
/// upper bound is the cost of the path so far represented by the parent node
let private promisingChildren depth bound node =
    let boundDiff = bound - node.Cost

    let insertNode v i s =
        seq { yield! s |> Seq.take i; yield v; yield! s |> Seq.skip i } 

    let promisingChild i (curr, succ) =
        let childDiff = costs.[curr,depth] + costs.[depth,succ] - costs.[curr,succ]
        if (childDiff < boundDiff)
        then Some({Path = node.Path |> insertNode depth (i + 1); Cost = node.Cost + childDiff})
        else None

    node.Path
    |> Seq.pairwise
    |> Seq.mapi promisingChild
    |> Seq.choose id

/// recursively traverses the search tree and prunes non-promising branches
/// when the depth = size, update the current optimal solution (upper bound gets updated too)
let rec private solve node optNode depth size =
    if (size = depth)
    then 
        optNode := node
    else
        promisingChildren depth optNode.Value.Cost node
        |> Seq.sortBy (fun n -> n.Cost)
        |> Seq.takeWhile (fun c -> c.Cost < optNode.Value.Cost)
        |> Seq.iter (fun c -> solve c optNode (depth + 1) size)
   
/// creates initial path (0,1,2,0) as the starting point and calls the recursive function
let solveOpt input =
    costs <- input.Costs
    let size = input.Points.Length
    let path = [0;1;2;0]
    let node = {Path = path; Cost = path |> totalCost costs }
    let optNode = ref {Path = Seq.empty; Cost = Double.PositiveInfinity}

    solve node optNode 3 size

    createSolution input optNode.Value.Path

/// the alghoritm is data sensitive and preordering is required
/// this function orders the given input with the following policy:
/// first, find two furthest points and assign them index 0 and 1
/// assign next index to a point which is located as far as possible
/// from the already assigned points. stop when all points assigned
let preOrder input =
    let maxIndexes size (arr2d : _[,]) =  
        arr2d |> Seq.cast
        |> Seq.mapi (fun i x -> i, x)
        |> Seq.maxBy snd 
        |> fst
        |> (fun i -> i / size, i % size)

    let costs = input.Costs
    let size = input.Points.Length
    let first,second = costs |> maxIndexes size
    let left = Collections.Generic.List<_>(seq{0..size-1} |> Seq.filter (fun x -> x <> first && x <> second))
    let preOrdered = Collections.Generic.List<_>([first; second])
    while (not <| Seq.isEmpty left) do
        let next = 
            left 
            |> Seq.map (fun col -> col, (costs.[col,*] 
                                         |> Array.mapi (fun i v -> i,v)
                                         |> Array.filter (fun (i,v) -> preOrdered.Contains i)
                                         |> Array.map snd |> Array.min))
            |> Seq.maxBy snd
            |> fst
        preOrdered.Add next
        left.Remove next |> ignore
    let points = Array.init size (fun i -> input.Points.[preOrdered.[i]])
    createInput points