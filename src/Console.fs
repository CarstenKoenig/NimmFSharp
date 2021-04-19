namespace Nimm.Game

module Console =    

    /// Hilfsfunktion um den Benutzer eine Nummer zwischen 1 und max eingeben zu lassen
    let rec askForNumber max (prompt : string) =
        System.Console.Write prompt
        System.Console.Write " "
        let input = System.Console.ReadLine ()
        match System.Int32.TryParse input with
        | (true, i) when i <= 0 || i > max -> 
            printfn "bitte eine positive Zahl zwischen 1 und %d eingeben" max
            askForNumber max prompt
        | (true, i) -> i
        | (false, _) -> askForNumber max prompt

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
        | Free (ReadNr (prompt, max, cont)) ->
            askForNumber max prompt
            |> cont
            |> runConsole
