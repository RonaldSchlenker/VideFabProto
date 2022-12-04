
[<AutoOpen>]
module Vide.Wpf

open Vide
open System.Linq
open System.Windows
open System.Windows.Controls
open System.Windows.Markup
open System.Threading
open System.Diagnostics

let tryCast<'a> (x: obj) = if (x :? 'a) then Some(x :?> 'a) else None

// TODO: Trying to see things as recursive nodes, which is not always good
type Node = DependencyObject

type DependencyObject with
    member this.TryAddChild(child: Node) =
        this
        |> tryCast<IAddChild>
        |> Option.iter (fun x -> x.AddChild(child))
    member this.TryRemoveChild(child: Node) =
        // TODO: Crap!!
        do
            this
            |> tryCast<Panel>
            |> Option.iter (fun x -> x.Children.Remove(child :?> UIElement))
        do
            this
            |> tryCast<ContentControl>
            |> Option.iter (fun x -> x.Content <- null)

type UIElementCollection with
    member this.AsList() = this.Cast<Node>() |> Seq.toList

type Context =
    {
        node: Node
        mutable evaluateView: unit -> unit
        mutable elementsContext: ElementsContext
    }

and ElementsContext(parent: Node) =
    let mutable keptChildren = []
    let memory child =
        keptChildren <- child :: keptChildren
    let append child =
        do parent.TryAddChild child
    member _.AddChild(child: Node) =
        do memory child
        do append child
    member _.KeepChild(child: Node) =
        do memory child
    member _.GetObsoleteChildren() =
        let childNodes = 
            parent
            |> tryCast<Panel>
            |> Option.map (fun panel -> panel.Children.AsList())
            |> Option.defaultValue []
        childNodes |> List.except keptChildren

module Mutable =
    type MutableValue<'a>(init: 'a) =
        let mutable state = init
        member val EvaluateView = (fun () -> ()) with get,set
        member this.Set(value) = state <- value; this.EvaluateView()
        member this.Value
            with get() = state
            and set(value) = this.Set(value)

    let inline change op (mutVal: MutableValue<_>) x =
        mutVal.Value <- op mutVal.Value x
    
    let ofValue x =
        Vide <| fun s (c: Context) ->
            let s = s |> Option.defaultWith (fun () -> MutableValue(x))
            do s.EvaluateView <- c.evaluateView
            s, Some s
    
    //let list x =
    //    Vide <| fun s (c: Context) ->
    //        let s = s |> Option.defaultWith (fun () -> MutableValue(x))
    //        do s.EvaluateView <- c.evaluateView
    //        s, Some s

let inline ( += ) mutVal x = Mutable.change (+) mutVal x
let inline ( -= ) mutVal x = Mutable.change (-) mutVal x
let inline ( *= ) mutVal x = Mutable.change (*) mutVal x
let inline ( /= ) mutVal x = Mutable.change (/) mutVal x
let inline ( := ) (mutVal: Mutable.MutableValue<_>) x = mutVal.Value <- x

// TODO: Don't box values
type AttributeList = list<string * obj>

type NodeBuilderState<'n,'s when 'n :> DependencyObject> = option<'n> * option<'s>

type NodeSyncResult = Keep | DiscardAndCreateNew

type NodeBuilder<'n when 'n :> Node>(newNode, checkOrUpdateNode) =
    inherit VideBuilder()
    member val Attributes: AttributeList = [] with get, set
    //member val Events: EventList = [] with get, set
    member this.Run
        (Vide childVide: Vide<unit,'fs,Context>)
        : Vide<'n, NodeBuilderState<'n,'fs>, Context>
        =
        let syncAttrs (node: Node) =
            for name,value in this.Attributes do
                // TODO: Reflection!
                node.GetType().GetProperty(name).SetValue(node, value)
        // TODO
        //let syncEvents (node: Node) =
        //    for name,handler in this.Events do
        //         eventManager.RemoveListener(node, name)
        //         eventManager.AddListener(node, name, handler)
        Vide <| fun s (ctx: Context) ->
            let s,cs = separateStatePair s
            let node,cs =
                match s with
                | None ->
                    newNode ctx,cs
                | Some node ->
                    match checkOrUpdateNode node with
                    | Keep ->
                        ctx.elementsContext.KeepChild(node)
                        node,cs
                    | DiscardAndCreateNew ->
                        newNode ctx,None
            do syncAttrs node
            //do syncEvents node
            let childCtx =
                {
                    node = node
                    evaluateView = ctx.evaluateView
                    elementsContext = ElementsContext(node)
                }
            let cv,cs = childVide cs childCtx
            for x in childCtx.elementsContext.GetObsoleteChildren() do
                node.TryRemoveChild(x) |> ignore
                // we don'tneed this? Weak enough?
                // events.RemoveListener(node)
            node, Some (Some node, cs)
    member inline this.attrCond(name, value: obj) =
        do this.Attributes <- (name, value) :: this.Attributes
    // TODO
    //member this.on(name, handler: EventHandler) =
    //    failwith "TODO"
    //    //do this.Events <- (name, handler) :: this.Events
    //    //this

type RootBuilder<'n when 'n :> Node>(holder) =
    inherit NodeBuilder<'n>(
        (fun _ -> holder),
        (fun _ -> Keep))
    member inline _.Yield
        (x: Vide<unit,'s,Context>)
        : Vide<unit,'s,Context>
        =
        x

let inline bootstrapApp (holder: #Node) (v: Vide<_,'s,Context>) onEvaluated =
    let ctx =
        {
            node = holder
            evaluateView = fun () -> ()
            elementsContext = ElementsContext(holder)
        }
    let videMachine =
        VideMachine(
            None,
            ctx,
            RootBuilder(holder) { v },
            onEvaluated)
    do ctx.evaluateView <- videMachine.Eval
    videMachine

let startApp view =
    let thread = new Thread(fun () ->
        let window = Window()
        let app = Application(MainWindow = window)
        app.Startup.Add(fun _ ->
            let onEvaluated _ state =
                //currentState <- state |> Option.map (fun s -> s :> obj)
                Debug.WriteLine($"Evaluation done. New state: {state}")
            let videMachine = bootstrapApp window view onEvaluated
            videMachine.Eval()
            window.Show()
        )
        app.Run() |> ignore
    )

    thread.SetApartmentState(ApartmentState.STA)
    thread.Start()
