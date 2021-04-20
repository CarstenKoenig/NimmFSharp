module Nimm.Game.Direct

open Nimm.Game

let private askForNumber =
    let console = DependencyInjection.Console()
    let io = DependencyInjection.IO console
    io.AskForNumber

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
    let actionNumber = askForNumber actions.Length "Bitte Aktionsnummer eingeben:"
    let state' = State.act actions.[actionNumber-1] state
    playGame state'
