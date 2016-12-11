
open System

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open FSharpx.Collections

open FSharpz.Collections.Mutable

let rng = Random()
let max = pown 10 9

type PriorityQueue () =
    let mutable list  : int list = []

    [<Params (2000, 20000, 200000, 2000000)>]
    member val public Length = 0 with get, set

    [<Setup>]
    member self.SetupData() =
        list <- [for i in 2..self.Length -> rng.Next(1, max)]

    [<Benchmark>]
    member self.MaxPriorityQueue () = PriorityQueue<int>(list)

    [<Benchmark>]
    member self.MaxPriorityQueueInsertingFromStart () = 
        let pq = PriorityQueue<int>()
        list |> List.iter (fun e -> pq.Enqueue e)

    [<Benchmark>]
    member self.FSharpxPriorityQueueInsertingFromStart () = 
        let pq = PriorityQueue.empty true
        let rec insertIntoPriorityQueue xs (q: IPriorityQueue<IComparable>) =
            match xs with
            | [] -> ()
            | h::tail -> insertIntoPriorityQueue tail (q.Insert(h))
        insertIntoPriorityQueue list pq

[<EntryPoint>]
let main argv = 
    (BenchmarkSwitcher[|typeof<PriorityQueue>|]).Run argv
    |> string |> printfn "%s"
    0
