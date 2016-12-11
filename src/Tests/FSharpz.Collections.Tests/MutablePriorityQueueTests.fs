namespace FSharpz.Collections.Tests

open System

open Xunit
open FsCheck

open FSharpz.Collections.Mutable

module MutablePriorityQueueTests =

    let rec dequeueAll (pq : PriorityQueue<_>) xs =
        if pq.IsEmpty then xs else dequeueAll pq (pq.Dequeue()::xs)

    [<CustomComparison; StructuralEquality>]
    type Point = { X: int; 
                   Y: int }
                    interface IComparable<Point> with
                        member this.CompareTo other =
                            compare this.Y other.Y
                    interface IComparable with
                        member this.CompareTo(obj: obj) =
                            match obj with
                            | :? Point -> compare this.Y (unbox<Point> obj).Y
                            | _ -> invalidArg "obj" "Must be of type Point"

    [<Fact>]
    let ``Should Dequeue in descending order for Max PriorityQueue``() = 
        let intGenerator = Arb.generate<int>
        let samples = Gen.sample 500 100 intGenerator
        let pMax = new PriorityQueue<int>(samples)
        let dequeued = dequeueAll pMax []
        Assert.Equal<int list>(dequeued, (samples |> List.sort))

    [<Fact>]
    let ``Should Dequeue in ascending order for Min PriorityQueue``() =
        let intGenerator = Arb.generate<int>
        let samples = Gen.sample 500 100 intGenerator
        let pMin = new PriorityQueue<int>(samples, false)
        let dequeued = dequeueAll pMin []
        Assert.Equal<int list>(dequeued, (samples |> List.sortBy (fun x -> -x)))

    [<Fact>]
    let ``Should Dequeue in descending order for Max PriorityQueue and custom type``() = 
        let pointGenerator = Arb.generate<Point>
        let samples = Gen.sample 500 100 pointGenerator
        let samplesSorted = samples |> List.sort
        let pMax = new PriorityQueue<Point>(samples)
        let dequeued = dequeueAll pMax []
        for i in 0..dequeued.Length - 1 do
            Assert.Equal(dequeued.[i].Y, samplesSorted.[i].Y)

    [<Fact>]
    let ``Should Dequeue in ascending order for Min PriorityQueue and custom type``() =
        let pointGenerator = Arb.generate<Point>
        let samples = Gen.sample 500 100 pointGenerator
        let samplesSorted = samples |> List.sortBy (fun p -> -p.Y)
        let pMin = PriorityQueue<Point>(samples, false)
        let dequeued = dequeueAll pMin []
        for i in 0..dequeued.Length - 1 do
            Assert.Equal(dequeued.[i].Y, samplesSorted.[i].Y)

    [<Fact>]
    let ``Should shrink the heap when needed``() = 
        let intGenerator = Arb.generate<int>
        let samples = Gen.sample 500 100 intGenerator
        let pMax = PriorityQueue<int>(samples)
        for i in 0..50 do
            pMax.Dequeue() |> ignore
        Assert.Equal(49, pMax.Count)