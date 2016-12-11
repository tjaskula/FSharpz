namespace FSharpz.Collections

module Mutable =

    open System
    open Microsoft.FSharp.Core

    type PriorityQueue<'T when 'T : comparison>(values: seq<'T>, isDescending: bool) =
        let heap : System.Collections.Generic.List<'T> = System.Collections.Generic.List<'T>(values)
    
        let isGreater x y =
            if isDescending then x > y else x < y

        let isLower x y = not (isGreater x y)

        let mutable size = heap.Count

        let shrinkHeap() =
            let shouldShrink = size < heap.Count / 2
            if shouldShrink then heap.RemoveRange(size, heap.Count - size - 1)

        let parent i = (i - 1) / 2
        let leftChild i = 2 * i
        let rightChild i = 2 * i + 1

        let swap i maxIndex =
            let temp = heap.[i]
            heap.[i] <- heap.[maxIndex]
            heap.[maxIndex] <- temp

        let siftUp i =
            let mutable indx = i
            while indx > 0 && isLower heap.[parent indx] heap.[indx] do
                swap (parent indx) indx
                indx <- parent indx

        let rec siftDown i =
            let l = leftChild i
            let r = rightChild i
            let maxIndexLeft = if l < size && isGreater heap.[l] heap.[i] then l else i
            let maxIndex = if r < size && isGreater heap.[r] heap.[maxIndexLeft] then r else maxIndexLeft
            if i <> maxIndex then
                swap i maxIndex
                siftDown maxIndex
            else ()
    
        let build() =
            for i = size / 2 downto 0 do
                siftDown i
    
        do build()

        new (values) = PriorityQueue<'T>(values, true)
        new () = PriorityQueue<'T>([], true)

        member this.IsEmpty = size = 0

        member this.Count = size

        member this.Dequeue() =
            if this.IsEmpty then raise (new Exception("No more elements to dequeue"))
            let result = heap.[0]
            heap.[0] <- heap.[size - 1]
            // we limit the boundary but the last element stays in memory
            // we could use heap.Remove but it's O(n) operation so too slow
            size <- size - 1
            //shrinkHeap()
            siftDown 0
            result

        member this.Enqueue(p: 'T) =
            if heap.Count = size then
                heap.Add(p)
            else
                heap.[size] <- p
            size <- size + 1
            siftUp (size - 1)