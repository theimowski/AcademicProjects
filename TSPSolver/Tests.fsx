#r "bin/release/TSPSolver.dll"

open System
open System.IO
open System.Text.RegularExpressions

open MetricTSP.Common
open MetricTSP.OPTSolver
open MetricTSP.ApproxSolver

Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

let readFile file =
    let lines = File.ReadAllLines file
    let reg = Regex "(?<id>\d+) (?<x>\d+.\d+) (?<y>\d+.\d+)"
    let parsePoint line =
        reg.Match line |> (fun m -> {Id = Int32.Parse(m.Groups.["id"].Value)
                                     X = Double.Parse(m.Groups.["x"].Value.Replace(".", ","))
                                     Y = Double.Parse(m.Groups.["y"].Value.Replace(".", ","))})
    lines 
    |> Array.filter (fun l -> reg.IsMatch l)
    |> Array.map parsePoint


let generatePoints count seed =
    let rand = Random(seed)
    
    seq {1..count}
    |> Seq.map (fun i -> {Id = i; X = rand.NextDouble() * 100000.; Y = rand.NextDouble() * 100000.})
    |> Array.ofSeq

type Alghoritm = Optimal | Approximate2
type Result = {Alghoritm: Alghoritm; Solution : Solution; Time : TimeSpan}

let stopwatch fn = 
    let sw = Diagnostics.Stopwatch.StartNew()
    let res = fn()
    sw.Stop()
    res, sw.Elapsed

let runTest alg input =
    let solution,time = 
        stopwatch (fun () ->
            match alg with
            | Optimal -> input |> preOrder |> solveOpt
            | Approximate2 -> input |> solve2Approx)    
    {Alghoritm = alg; Solution = solution; Time = time}

let printResult result =
    let desc = 
        match result.Alghoritm with
            | Optimal -> "optimal solution"
            | Approximate2 -> "2 approx solution"
    printfn "%20s: %15f (time elapsed: %6.0f ms)" desc result.Solution.Cost result.Time.TotalMilliseconds


let compare input desc =
    let opt = runTest Optimal input 
    let approx2 = runTest Approximate2 input 

    printfn ""
    printfn "Comparing %s" desc
    printfn ""
    opt |> printResult
    approx2 |> printResult
    printfn "--------------------------------------------------"
    printfn "%20s: %15f" "2 approx ratio" (approx2.Solution.Cost/opt.Solution.Cost)
    printfn ""
    printfn ""
    printfn ""

let fileCompare file = 
    let input = file |> readFile |> createInput
    compare input (sprintf "file: %s" file)

let randomCompare count seed =
    let input = generatePoints count seed |> createInput
    compare input (sprintf "random case of size: %d (seed %d)" count seed)


// ----------------------------------------
// TESTS go here
    
let seeds = [1; 28; 14243; 23413123; 653462]
let sizes = [3; 10; 15; 20; 23; 26; 28; 30]

for size in sizes do
    for seed in seeds do
        randomCompare size seed
            
fileCompare "wi29.tsp"
fileCompare "dj38.tsp"


// this optimal branch and bound alghoritm is data sensitive
// for example this runs in about 3 seconds (30 points)
generatePoints 30 14243 |> createInput |> runTest Optimal

// while this already runs in more than 2 minutes (one point more)
generatePoints 31 14243 |> createInput |> runTest Optimal

// this yet runs in reasonable amount of time (about a minute)
// 734 cities in Uruguay
"uy734.tsp" |> readFile |> createInput |> runTest Approximate2


// additional test showing that preOrdering input for the optimal algorithm is crucial
let input = generatePoints 20 14243 |> createInput 
stopwatch (fun () -> solveOpt(preOrder input)) |> snd |> (fun ts -> ts.TotalMilliseconds) |> printfn "with preOrder: %f ms"
stopwatch (fun () -> solveOpt input) |> snd |> (fun ts -> ts.TotalMilliseconds) |> printfn "without preOrder: %f ms"