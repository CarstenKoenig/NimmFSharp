namespace Nimm.Game

module Test =    
    open Nimm.Game

    type Environment =
        { 
            input : System.Collections.Generic.Stack<string>
            output : System.Collections.Generic.List<string>
        }
        static member FromInputList (inputs : seq<string>) =
            {
                input = System.Collections.Generic.Stack inputs
                output = System.Collections.Generic.List ()
            }

    /// Test-Interpreter, der Eingaben aus `Environment.input` zieht
    /// und Ausgaben alle an `Environment.output` anfügt (Clear wird as `<clear>` in die Ausgabe geschrieben)
    /// Schludert etwas mit dem ReadNr (prüft nicht >= 1 && <= max) für das Testen hier reicht es
    let runTest env prg =
        let rec go =
            function
            | Pure x -> x
            | Delayed del -> go (del ())
            | Free (Clear cont) ->
                env.output.Add "<clear>"
                |> cont
                |> go
            | Free (WriteText (text, cont)) ->
                env.output.Add text
                |> cont
                |> go
            | Free (ReadText (prompt, cont)) ->
                env.output.Add prompt
                env.input.Pop ()
                |> cont
                |> go
        go prg
