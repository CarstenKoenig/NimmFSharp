open Nimm.Game


let runDirect coins =
    State.initWithCoins coins
    |> Direct.playGame

let runDependencyInjection =
    let console = new DependencyInjection.Console()
    let io = DependencyInjection.IO console
    let game = DependencyInjection.Gameplay io
    game.GameWithCoins

let runConsoleInterpreter coins =
    FreeMonads.Gameplay.gameWithCoins coins
    |> FreeMonads.Console.runConsole
    

[<EntryPoint>]
let main argv =
    runDirect 13
    // runDependencyInjection 13 |> ignore
    // runConsoleInterpreter 13 |> ignore
    0 // return an integer exit code
