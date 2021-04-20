namespace Nimm.Game.DependencyInjection

type IDoIO =
    abstract WriteText : text:string -> unit
    abstract ReadText : prompt:string -> string
    abstract Clear : unit -> unit

type IO (implementation : IDoIO) =

    // unsere "Seiteneffekte" kapseln wir jetzt noch als schöne Funktionen:
    let writeText text = implementation.WriteText text
    let readText prompt = implementation.ReadText prompt
    let clear () = implementation.Clear()

    /// Hilfsfunktion um den Benutzer eine Nummer zwischen 1 und max eingeben zu lassen
    let rec askForNumber max (prompt : string) : int =
        let input = readText prompt
        match System.Int32.TryParse input with
        | (true, i) when i > 0 && i <= max -> 
            i
        | _ ->
            writeText (sprintf "bitte eine positive Zahl zwischen 1 und %d eingeben" max)
            askForNumber max prompt

    member __.Clear () = clear()
    member __.WriteText text = writeText text
    member __.AskForNumber max prompt = askForNumber max prompt
