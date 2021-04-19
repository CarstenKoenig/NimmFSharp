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
    let actionNumber = Console.askForNumber actions.Length "Bitte Aktionsnummer eingeben:"
    let state' = State.act actions.[actionNumber-1] state
    playGame state'
    

[<EntryPoint>]
let main argv =
    //State.initWithCoins 13
    //|> playGame
    let won =
        Gameplay.gameWithCoins 21
        |> Console.runConsole
    0 // return an integer exit code
