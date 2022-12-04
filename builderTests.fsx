#load "external.fsx"
open External


type UselessBuilder() =
    member _.Bind(m, f) = f m
    member _.Return(x) = x

let useless = UselessBuilder()

let x = useless {
    let! a = 10
    return a
}

open System.Collections.Generic

type IEnumerable<'a> with
    // member this.Bind(m, f) = f m
    // member this.Return(x) = x
    member this.Yield(x) = x
    member this.Zero() = this
    member this.Combine(a, b) = Seq.append a [b]
    member this.Delay(f) = f()
    member this.Run(f) = f()

[1;2;3] {
    yield 4
    yield 5
    yield 6
}
