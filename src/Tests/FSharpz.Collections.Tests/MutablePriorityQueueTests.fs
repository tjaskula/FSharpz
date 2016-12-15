namespace FSharpz.Collections.Tests

open System

open Xunit
open FsCheck

open FSharpz.Collections.Mutable

module MutablePriorityQueueTests =

    type Edge = { DestinationVertexId: int; Distance: double }

    [<CustomComparison; StructuralEquality>]
    type Vertex = { Id: int; ShortestDistance: double; Edges: Edge list }
                    interface IComparable<Vertex> with
                            member this.CompareTo other =
                                compare this.ShortestDistance other.ShortestDistance
                    interface IComparable with
                        member this.CompareTo(obj: obj) =
                            match obj with
                            | :? Vertex -> compare this.ShortestDistance (unbox<Vertex> obj).ShortestDistance
                            | _ -> invalidArg "obj" "Must be of type Vertex"

    type Graph = { Vertices: Vertex list }

    let addEdge vertex e = { vertex with Edges = e::vertex.Edges }

    type Vertex with
        member this.AddEdge = addEdge this

    let addVertex graph v = { graph with Vertices = v::graph.Vertices }
    let getVertices graph = graph.Vertices
    let setSource vertexId graph = 
        { Vertices = graph.Vertices
                    |> List.map (fun e -> if e.Id = vertexId then {e with ShortestDistance = 0.0 } else e) }

    type Graph with
        member this.AddVertex = addVertex this
        member this.GetVertices () = getVertices this

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
        Assert.Equal<int list>(dequeued, (samples |> List.sortBy (~-)))

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

    [<Fact>]
    let ``Should find value with it's index``() = 
        let pq = PriorityQueue<int>([1; 8; 10; 7; 5; 6; 2; 3; 4; 9])
        Assert.Equal(None, pq.TryFind (fun e -> e = 0))
        Assert.Equal(Some(5, 1), pq.TryFind (fun e -> e = 1))
        Assert.Equal(Some(9, 5), pq.TryFind (fun e -> e = 5))
        Assert.Equal(Some(1, 9), pq.TryFind (fun e -> e = 9))
        Assert.Equal(Some(0, 10), pq.TryFind (fun e -> e = 10))

    [<Fact>]
    let ``Should update value``() = 
        let pq = PriorityQueue<int>([1; 8; 10; 7; 5; 6; 2; 3; 4; 9])
        let indx, _ = (pq.TryFind (fun e -> e = 5)).Value
        pq.Update indx 11
        Assert.Equal(Some(0, 11), pq.TryFind (fun e -> e = 11))
        let indx, _ = (pq.TryFind (fun e -> e = 11)).Value
        pq.Update indx 0
        Assert.Equal(Some(9, 0), pq.TryFind (fun e -> e = 0))

    [<Fact>]
    let ``Debug``() = 
        let rawGraph = Map.empty
                         .Add(1, [(9.0, 3); (7.0, 2); (14.0, 6)])
                         .Add(2, [(7.0, 1); (10.0, 3); (15.0, 4)])
                         .Add(3, [(9.0, 1); (2.0, 6); (11.0, 4); (10.0, 2)])
                         .Add(4, [(15.0, 2); (11.0, 3); (6.0, 5)])
                         .Add(5, [(6.0, 4); (9.0, 6)])
                         .Add(6, [(14.0, 1); (2.0, 3); (9.0, 5)])

        let makeEdge (distance, destVertexId) =
            { DestinationVertexId = destVertexId; Distance = distance}

        let makeVertex vertexId edges =
            { Id = vertexId;
              ShortestDistance = Double.PositiveInfinity;
              Edges = edges |> List.map makeEdge
            }

        let graph = rawGraph
                    |> Map.map makeVertex
                    |> Map.fold (fun (graph: Graph) _ v -> graph.AddVertex(v)) { Vertices = [] }
                    |> setSource 1

        let pq = PriorityQueue<Vertex>(graph.GetVertices(), false)

        while not pq.IsEmpty do
            let vertex = pq.Dequeue()
            for edge in vertex.Edges do
                let destinationId = edge.DestinationVertexId
                match pq.TryFind (fun e -> e.Id = destinationId) with
                | None -> ()
                | Some(indx, destination) ->
                    let newDistance = edge.Distance + vertex.ShortestDistance
                    if newDistance < destination.ShortestDistance then
                        let newDestination = { destination with ShortestDistance = newDistance }
                        pq.Update indx newDestination
                    else ()