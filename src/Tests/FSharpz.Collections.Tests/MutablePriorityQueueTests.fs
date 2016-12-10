namespace FSharpz.Collections.Tests

open System

open Xunit
open FsCheck
open FsCheck.Xunit

open FSharpz.Collections.Mutable

module MutablePriorityQueueTests =

    let rec dequeueAll (pq : PriorityQueue<_>) xs =
        if pq.IsEmpty then xs else dequeueAll pq (pq.Dequeue()::xs)

    [<Fact>]
    let ``Should Dequeue in descending order for Max PriorityQueue``() = 
        let intGenerator = Arb.generate<int>
        let samples = Gen.sample 500 100 intGenerator
        let pMax = new PriorityQueue<int>(samples)
        let dequeued = dequeueAll pMax []
        let dequeuedDesc xs s = xs = (s |> List.sort)
        Assert.True(dequeuedDesc dequeued samples)

    [<Fact>]
    let ``Should Dequeue in descending order for Max PriorityQueue spec``() = 
        let intGenerator = Arb.generate<int>
        let samples = Gen.sample 500 100 intGenerator
        let samplesSorted = samples |> List.sortBy (fun x -> -x)
        let spec = 
            let dec = { new Command<PriorityQueue<int>, int>() with
                            override __.RunActual pq = pq
                            override __.RunModel m = samplesSorted.[m + 1]
                            override __.Post(pq, m) = pq.Dequeue() = m |@ sprintf "should have dequeued: %i" m
                            override __.ToString() = "dequeue" }
            { new ICommandGenerator<PriorityQueue<int>, int> with
                    member __.InitialActual = PriorityQueue<int>(samples)
                    member __.InitialModel = 0
                    member __.Next model = Gen.elements [dec] }
        
        Check.Quick (Command.toProperty spec)

    [<Fact>]
    let ``Should Dequeue in ascending order for Min PriorityQueue``() =
        let intGenerator = Arb.generate<int>
        let samples = Gen.sample 500 100 intGenerator
        let pMin = new PriorityQueue<int>(samples, false)
        let dequeued = dequeueAll pMin []
        let dequeuedAsc xs s = xs = (s |> List.sortBy (fun x -> -x))
        Assert.True(dequeuedAsc dequeued samples)