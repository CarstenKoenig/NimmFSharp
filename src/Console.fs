namespace Nimm.Game

module Console =    
    /// das ist ein sogenannter Interpreter der ein Programm (unsere freie Monade)
    /// ausführt - hier in dem die System-Konsole benutzt wird
    let rec runConsole =
        function
        | Pure x -> x
        | Delayed del -> runConsole (del ())
        | Free (Clear cont) ->
            System.Console.Clear ()
            |> cont
            |> runConsole
        | Free (WriteText (text, cont)) ->
            System.Console.WriteLine text
            |> cont
            |> runConsole
        | Free (ReadText (prompt, cont)) ->
            System.Console.Write (prompt + " ")
            System.Console.ReadLine ()
            |> cont
            |> runConsole

    let askForNumber max prompt =
        IO.askForNumber max prompt
        |> runConsole
