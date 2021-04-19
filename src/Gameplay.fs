namespace Nimm.Game

module Gameplay =
    open Nimm.Game.IO
    
    /// Umsetzung des Spiels mit unserer freien Monade / Computational Expression
    let gameWithCoins (coins : CoinCount) : IOProgram<Player> =
        let outputState (state : State) : IOProgram<unit> =
            io {
                do! writeText (state.ToString ())
                do! writeText "------------------------------------\n\n"
            }
        let inputAction (state : State) : IOProgram<Action> =
            io {
                let actions = Seq.mapi (fun ind act -> (ind + 1, act)) (State.validActions state) |> Seq.toArray
                for (i, act) in actions do
                    do! writeText (sprintf "(%d): %O" i act)
                let! actionNr = readNr "Aktions-Nummer eingeben:" actions.Length
                return (snd (actions.[actionNr-1]))
            }
        let rec run (state : State) : IOProgram<Player> = 
            io {
                do! clear
                do! writeText "NIMM die letzte Münze und Du hast gewonnen!"
                do! writeText "===========================================\n\n"
                do! outputState state
                match state with
                | Won player -> 
                    return player
                | Turn _ -> 
                    let! action = inputAction state
                    let nextState = State.act action state
                    return! run nextState
            }
        run (State.initWithCoins coins)

