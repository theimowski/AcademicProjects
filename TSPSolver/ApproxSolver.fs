module MetricTSP.ApproxSolver

open Common


/// The following algorithm is based on this publication:
/// Vijay V. Vazirani - "Approximation Algorithms" 

/// Graph representation inspired by snippet: http://fssnip.net/av

type private Edge = int * int
type private Graph = int list * Edge list
type private Node = int * int list
type private AdjacencyGraph = Node list

/// Prim's alghoritm for finding minimal spanning tree
/// takes cost matrix as parameter and returns graph instance
let private primMST (costs : _[,]) : Graph =
    let size = Array2D.length1 costs
    let rec grow (vertices, edges) left visited =
        if left |> Set.isEmpty then
            (List.rev vertices, List.rev edges)
        else
            let (_,vertex) as edge = 
                visited
                |> Seq.collect(fun v -> costs.[v,*]
                                        |> Array.mapi (fun i c -> (v,i), c)
                                        |> Array.filter (fun ((_,i),_) -> not <| Set.contains i visited))
                |> Seq.minBy snd |> fst
            grow (vertex::vertices, edge::edges) (Set.remove vertex left) (Set.add vertex visited)

    grow ([0],[]) (Set(seq{1..size-1})) (Set.singleton 0)
    
/// takes graph and returns graph with doubled edges
let private doubleEdges ((vertices, edges) : Graph) : Graph=
    (vertices, edges |> List.collect (fun e -> [e;e]))

/// converts graph representation to adjacency graph representation
let private graph2AdjacencyGraph ((ns, es) : Graph) : AdjacencyGraph = 
    let nodeMap = ns |> List.map(fun n -> n, []) |> Map.ofList
    (nodeMap,es) 
    ||> List.fold(fun map (a,b) -> map |> Map.add a (b::map.[a]) |> Map.add b (a::map.[b]))
    |> Map.toList

/// returns nodes visited in dfs order
let private dfsTraverse (graph : Graph) =
    let adj = graph2AdjacencyGraph graph |> Map.ofList
    let first = List.head (fst graph)
    let visited = ref Set.empty
    let res = ref [first]

    let rec dfs u =
        visited := Set.add u !visited
        for v in adj.[u] do
            if (not <| Set.contains v !visited) then
                res := v::!res
                dfs v
    dfs first
    !res |> List.rev    

let private remove elem list =
    let index = list |> List.findIndex ((=) elem)
    list
    |> List.mapi (fun i el -> (i <> index, el)) 
    |> List.filter fst |> List.map snd
   
/// for the given edge and graph checks if the edge is a bridge
/// first removes the edge from the graph, outputs vertices in dfs order from the updated graph
/// and checks if any vertex was omitted comparing to the original graph
let private isBridge edge (vertices, edges) =
    let g' = (vertices, edges |> remove edge)
    let traversed = dfsTraverse g' |> Set.ofList
    Set.difference (vertices |> Set.ofList) traversed |> Set.isEmpty |> not

let private secondEnd firstEnd edge = 
    match edge with
    | v1,v2 when v1 = firstEnd -> v2
    | v1,v2 when v2 = firstEnd -> v1
    | _ -> failwith <| sprintf "%A is not at end of edge %A" firstEnd edge

/// Fleury's alghoritm for finding eulerian cycle
/// starting with an arbitrary vertex 
/// choose any edge that is not a bridge (if not possible choose any)
/// add the edge to the output cycle
let private fleuryEulerianCycle (vertices, edges) =
    let first = vertices |> List.head
    let rec cycle curr acc (vertices, edges) =
        if (List.isEmpty edges)
            then List.rev acc
        else 
            let adjEdges = edges |> List.filter (fun (v1,v2) -> curr = v1 || curr = v2)
            let edge, vertices' = 
                match adjEdges |> List.tryFind (fun e -> not <| isBridge e (vertices, edges)) with
                | Some e -> e, vertices
                | None -> List.head adjEdges, vertices |> remove curr
            let edges' = edges |> remove edge
            let next = secondEnd curr edge
            cycle next (edge::acc) (vertices', edges')
    cycle first [] (vertices, edges)

/// creates a tour from the eulerian cycle by 
/// outputing set of vertices in order the cycle visits them
let private cycleToTour edges =
    let start = List.head edges |> fst 
    let distinct = ref <| Set.singleton start
    let tour = ref [start]
    let rec visit curr edges =
        match edges with
        | [] -> ()
        | h::t -> 
            let next = secondEnd curr h
            if (not <| Set.contains next !distinct) 
            then 
                distinct := Set.add next !distinct
                tour := next :: !tour
            visit next t
    visit start edges
    start :: !tour |> List.rev

let solve2Approx input =
    let tour =
        input.Costs
        |> primMST
        |> doubleEdges
        |> fleuryEulerianCycle
        |> cycleToTour   

    createSolution input tour