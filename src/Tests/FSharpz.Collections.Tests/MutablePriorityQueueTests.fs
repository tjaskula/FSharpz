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
        let dequeuedDesc xs s = xs = (s |> List.sort)
        Assert.True(dequeuedDesc dequeued samples)

    [<Fact>]
    let ``Should Dequeue in ascending order for Min PriorityQueue``() =
        let intGenerator = Arb.generate<int>
        let samples = Gen.sample 500 100 intGenerator
        let pMin = new PriorityQueue<int>(samples, false)
        let dequeued = dequeueAll pMin []
        let dequeuedAsc xs s = xs = (s |> List.sortBy (fun x -> -x))
        Assert.True(dequeuedAsc dequeued samples)

    [<Fact>]
    let ``Should Dequeue in descending order for Max PriorityQueue and custom type``() = 
        let pointGenerator = Arb.generate<Point>
        let samples = Gen.sample 500 100 pointGenerator
        let pMax = new PriorityQueue<Point>(samples)
        let dequeued = dequeueAll pMax []
        let dequeuedDesc xs s = xs = (s |> List.sort)
        Assert.True(dequeuedDesc dequeued samples)

    [<Fact>]
    let ``Should Dequeue in ascending order for Min PriorityQueue and custom type``() =
        let pointGenerator = Arb.generate<Point>
        let samples = Gen.sample 500 100 pointGenerator
        let pMin = new PriorityQueue<Point>(samples, false)
        let dequeued = dequeueAll pMin []
        let dequeuedAsc xs s = xs = (s |> List.sortBy (fun p -> -p.Y))
        Assert.True(dequeuedAsc dequeued samples)