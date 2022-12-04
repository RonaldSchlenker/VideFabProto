namespace Vide

open System.Windows
open System.Windows.Controls
open Vide
open type Control

module Node =
    let inline create<'n,'b
            when 'n :> UIElement
            and 'b :> NodeBuilder<'n>
            >
        (createElement: unit -> 'n)
        createBuilder
        : 'b
        =
        let create ctx =
            let element = createElement()
            do ctx.elementsContext.AddChild(element)
            element
        let update (node: 'n) =
            match node.GetType() = typeof<'n> with
            | true -> Keep
            | false ->
                //console.log($"TODO: if/else detection? Expected node name: {tagName}, but was: {node.nodeName}")
                DiscardAndCreateNew
        createBuilder(create, update)

type ButtonBuilder(createNode, updateNode) =
    inherit NodeBuilder<Button>(createNode, updateNode)
type TextBlockBuilder(createNode, updateNode) =
    inherit NodeBuilder<TextBlock>(createNode, updateNode)
type StackPanelBuilder(createNode, updateNode) =
    inherit NodeBuilder<StackPanel>(createNode, updateNode)

// open type (why? -> We need always a new builder on property access)
type Widgets =
    // TODO: This is something special
    static member inline Nothing =
        NodeBuilder(
            (fun ctx -> UIElement()),
            (fun node -> Keep))
    static member inline Button = Node.create (fun () -> Button()) ButtonBuilder
    static member inline TextBlock text = Node.create (fun () -> TextBlock(Text = text)) TextBlockBuilder
    static member inline StackPanel = Node.create (fun () -> StackPanel()) StackPanelBuilder

    // TODO: Yield should work for strings

[<AutoOpen>]
module VideBuilderExtensions =
    type VideBuilder with
        member inline this.Bind
            (
                x: NodeBuilder<'n>,
                f: 'n -> Vide<'v,'s1,Context>
            ) : Vide<'v, NodeBuilderState<'n,unit> option * 's1 option, Context>
            =
            let v = x { () }
            this.Bind(v, f)
        member inline _.Yield<'n,'s,'c when 'n :> Node>
            (v: Vide<'n,'s,'c>)
            : Vide<unit,'s,'c>
            =
            v |> map ignore
        member inline _.Yield
            (x: NodeBuilder<'n>)
            : Vide<unit, NodeBuilderState<'n,unit>, Context>
            =
            x { () } |> map ignore
        member inline _.Yield
            (x: string)
            =
            Widgets.TextBlock(x) { () } |> map ignore
