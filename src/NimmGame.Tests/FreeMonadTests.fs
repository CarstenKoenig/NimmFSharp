module Nimm.Game.FreeMonads.Tests


open Xunit
open Nimm.Game
open Nimm.Game.FreeMonads

[<Fact>]
let ``In a Game with 3 Coins player 1 wins by taking all 3`` () =
    let game = Gameplay.gameWithCoins 3
    let env = TestInterpreter.Environment.FromInputList ["3"]

    let result = TestInterpreter.runTest env game

    Assert.Equal(Player1, result)

[<Fact>]
let ``In a Game with 3 Coins if player 1 takes 3 the next-to-last output is 'Spieler 1 hat gewonnen!'`` ()=
    let game = Gameplay.gameWithCoins 3
    let env = TestInterpreter.Environment.FromInputList ["3"]

    let _ = TestInterpreter.runTest env game

    Assert.Equal("Spieler 1 hat gewonnen!", env.output.[env.output.Count - 2])
