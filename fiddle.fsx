
type Vide<'v,'s,'c> = Vide of ('s option -> 'c -> 'v * 's)

type VideBuilder() =
    member inline _.Bind
        (
            Vide m: Vide<'v1,'s1,'c>,
            f: 'v1 -> Vide<'v2,'s2,'c>
        )
        =
        Vide <| fun s c ->
            let ms,fs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> Some ms, Some fs
            let mv,ms = m ms c
            let (Vide v) = f mv
            let vres,fs = v fs c
            vres, (ms,fs)
    member inline _.Return(x) =
        Vide <| fun s c -> x,None

let vide = VideBuilder()

let toStateMachine ctx (Vide vide: Vide<'v,'s,'c>)  =
    let mutable state = None
    fun () ->
        let value,newState = vide state ctx
        state <- Some newState
        value


// -----

let inline counter inclStart inc =
    Vide <| fun s c ->
        let s = s |> Option.defaultValue inclStart
        s, s + inc


// -----

let eval = 
    toStateMachine None <| vide {
        let! c1 = counter 0 1
        let! c2 = counter 100 10
        return c1 + c2
    }

// -----

eval()
