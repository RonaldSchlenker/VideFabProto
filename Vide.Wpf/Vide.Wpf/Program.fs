open Vide
open type Api.Widgets

vide {
    let! count = Mutable.ofValue 0

    StackPanel {
        TextBlock {
            $"Count = {count.Value}"
        }

        Button.Click(fun _ -> count -= 1) { "dec" }
        Button.Click(fun _ -> count += 1) { "inc" }
    }
}
|> startApp
