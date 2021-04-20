open System
open Nimm.Game


/// direkte Umsetzung
let rec playGame state =
    System.Console.Clear ()
    printfn "NIMM die letzte Münze und Du hast gewonnen!"
    printfn "===========================================\n\n"
    printfn "%O" state
    printfn "-------------------------------------------\n\n"
    let actions = State.validActions state
    if actions.IsEmpty then () else
    for (i, action) in Seq.indexed actions do
        printfn "(%d): %O" (i+1) action
    let actionNumber = FreeMonads.Console.askForNumber actions.Length "Bitte Aktionsnummer eingeben:"
    let state' = State.act actions.[actionNumber-1] state
    playGame state'

let runGame coins =
    State.initWithCoins coins
    |> playGame

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
    // runGame 13
    runDependencyInjection 13 |> ignore
    // runConsoleInterpreter 13 |> ignore
    0 // return an integer exit code
