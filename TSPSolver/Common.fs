module MetricTSP.Common

open System

type Point = {Id : int; X : float; Y: float}

type Input = {Points : array<Point>; Costs : float[,]}

type Solution = {Tour : array<Point>; Cost : float}

let euclideanDistance first second =
    sqrt(pown (first.X - second.X) 2 + pown (first.Y - second.Y) 2)

/// counts total cost for the given costs matrix and tour as a sequence of vertices
let totalCost (costs: float[,]) tour =
    tour |> Seq.pairwise |> Seq.map (fun (node,succ) -> costs.[node, succ]) |> Seq.sum

/// writes all distances between given points to 2D array and returns Input instance
let createInput points = 
    {Points = points
     Costs = Array2D.init points.Length points.Length (fun r c -> euclideanDistance points.[r] points.[c])}

/// given the input and calculated tour as sequence of vertices 
/// calculates the total cost and creates Solution instance
let createSolution input tour =
    let size = input.Points.Length
    let optTour = Array.init size (fun i -> input.Points.[Seq.nth i tour])
    {Tour = optTour; Cost = tour |> totalCost input.Costs}