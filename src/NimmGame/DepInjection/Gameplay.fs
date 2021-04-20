namespace Nimm.Game.DependencyInjection

open Nimm.Game

// Umsetzung des Spiels mit Hilfe der übergebenen IO Dependency
type Gameplay (io : IO) =
    
    member __.GameWithCoins (coins : CoinCount) =
        let outputState (state : State) =
            io.WriteText (state.ToString ())
            io.WriteText "------------------------------------\n\n"
        let inputAction (state : State) =
            let actions = Seq.mapi (fun ind act -> (ind + 1, act)) (State.validActions state) |> Seq.toArray
            for (i, act) in actions do
                io.WriteText (sprintf "(%d): %O" i act)
            let actionNr = io.AskForNumber actions.Length "Aktions-Nummer eingeben:"
            snd (actions.[actionNr-1])
        let rec run (state : State) = 
            io.Clear ()
            io.WriteText "NIMM die letzte Münze und Du hast gewonnen!"
            io.WriteText "===========================================\n\n"
            outputState state
            match state with
            | Won player -> 
                player
            | Turn _ -> 
                let action = inputAction state
                let nextState = State.act action state
                run nextState
        run (State.initWithCoins coins)

