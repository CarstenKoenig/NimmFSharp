namespace Nimm.Game

/// Typ-Alias - Wieviele Münzen liegen noch auf dem Haufen?
type CoinCount = int

/// Union-Typ - entweder Player1 oder Player2
type Player = 
    | Player1 
    | Player2
    /// Überladung der ToString - Methode für unseren Datentyp
    override this.ToString() =
        match this with
        | Player1 -> "Spieler 1"
        | Player2 -> "Spieler 2"
    /// der jeweils andere Spieler
    member this.Opponent =
        match this with
        | Player1 -> Player2
        | Player2 -> Player1

/// mögliche Aktionen - Union-Type
type Action =
    | Take1Coin
    | Take2Coins
    | Take3Coins
    /// wieviele Münzen repräsentiert der Wert?
    member this.NrCoins : CoinCount =
        match this with
        | Take1Coin -> 1
        | Take2Coins -> 2
        | Take3Coins -> 3
    /// Überladung der ToString - Methode für unseren Datentyp
    override this.ToString() =
        if this.NrCoins > 1 then
            sprintf "Nimm %d Münzen." this.NrCoins 
        else
            "Nimm eine Münze."

/// Der Zustand unseres Spiels:
/// entweder hat ein Spieler gewonnen (dann sind keine Münzen mehr da)
/// oder es gibt noch Münzen und ein bestimmter spieler ist an der Reihe
type State =
    | Won of winner:Player
    | Turn of player:Player * coinCount : CoinCount
    override this.ToString () =
        match this with
        | Won winner ->
            sprintf "%O hat gewonnen!" winner
        | Turn (player, 1) -> 
            sprintf "%O ist am Zug und es ist noch genau eine Münze übrig" player
        | Turn (player, coinCount) -> 
            sprintf "%O ist am Zug und es sind noch %d Münzen übrig" player coinCount

/// sozusagen das zugehörige Modul zum Typ
module State =
    
    /// initialisiere eine Zustand mit einer bestimmten Menge (>= 1) Münzen, wo Spieler 1 am Zug ist
    let initWithCoins coinCount =
        if coinCount < 1 then failwith "mindestens eine Münze ist nötig für ein Spiel" else
        Turn (Player1, coinCount)

    /// welche Aktionen sind im Zustand möglich?
    let validActions state =
        match state with
        | Won _ -> []
        | Turn (_, coins) when coins >= 3 -> 
            [ Take1Coin; Take2Coins; Take3Coins ]
        | Turn (_, coins) when coins = 2 -> 
            [ Take1Coin; Take2Coins ]
        | Turn (_, coins) -> 
            [ Take1Coin ]

    /// prüft ob eine Aktion für den Zustand durchgeführt werden kann
    let isValidAction action state =
        validActions state
        |> Seq.contains action

    /// berechnet einen neuen Zustand aus einem alten auf den eine Aktion angewendet wird
    /// in der Regel ist der nächste Spieler dran und es gibt weniger Münzen
    /// oder ein Spieler hat gewonnen (hat die letzten Münze(n) genommen)
    /// wenn die Aktion ungültig war kommt der gleiche Zustand heraus
    let act (action : Action) state =
        match state with
        // falls ein Spieler am Zug ist und die Aktion gültig ist
        | Turn (current, coins) when isValidAction action state ->
            let remainingCoins = coins - action.NrCoins
            if remainingCoins = 0 then 
                Won current 
            else 
                Turn (current.Opponent, remainingCoins)
        // sonst: schon im Gewonnnen-Zustand / ungültige Aktion - alter Zustand wird erneut zurückgegeben
        | other -> other
