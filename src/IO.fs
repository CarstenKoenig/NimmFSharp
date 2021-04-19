namespace Nimm.Game

// Schöne Einführung in freie Monaden mit F# von Mark Seemann:
// https://blog.ploeh.dk/2017/08/07/f-free-monad-recipe/

/// das ist die "F-Algebra" für die "Seiteneffekte", die wir haben wollen
/// F - steht für "Functor" - wir müssen uns also ein `map` überlegen
/// Die Idee ist, dass die Aktion als Wert darstellen und dann eine Continuation mit verwalten
/// das Verwalten wird und die "freie Monade" abnehmen die wir weiter unten definieren
type IOInstruction<'a> =
    | WriteText of text:string * cont:(unit -> 'a)
    | ReadNr of prompt:string * max:int * cont:(int -> 'a)
    | Clear of cont:(unit -> 'a)
    /// Funktor-Map - das "Ergebnis" jeweils mit `f` transformieren
    member this.map f =
        match this with
        | WriteText (text, cont) -> 
            WriteText (text, cont >> f)
        | ReadNr (prompt, max, cont) -> 
            ReadNr (prompt, max, cont >> f)
        | Clear cont -> 
            Clear (cont >> f)

/// Unsere "freie" Monade über die F-Algebra von oben
/// IOProgram<'res> beschreibt ein Programm, dass wenn es geflaufen ist ein 'res zurückgibt
type IOProgram<'a> =
    /// brauchen wir für den Computational-Expression Builder (dort für Delay) - sonst bekommen wir z.B. Probleme mit While / For
    | Delayed of (unit -> IOProgram<'a>)
    /// hier wird der rekursive Knoten geknüpft - das ermöglicht eine Bind-Operation - ein Wrapper für eine Instruktion deren Continuation ein neues Programm ist!
    | Free of IOInstruction<IOProgram<'a>>
    /// sozusagen das Ende der Rekursion oder für triviale Programme - enthält das Ergebnis, keine weiter Rekursion/Continuation
    | Pure of 'a
    /// Monadic-Bind!
    member this.bind f =
        match this with
        | Delayed del -> Delayed (fun () -> (del ()).bind f)
        | Free inst -> Free (inst.map (fun p -> p.bind f))
        | Pure inst -> f inst

module IO =

    /// Damit wird in F# der Syntax für Computational Expressions ermöglicht
    /// gebraucht wird ein "Builder" mit bestimmten Methoden
    /// anfangen kann man mit Bind/Return/ReturnFrom - dann sagt einen der Compiler jeweils was noch fehlt (bei Benutzung)
    /// siehe auch hier: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions
    type IOBuilder() = 
        member _.Bind (prg : IOProgram<_>, f) = prg.bind f
        member _.Return x = Pure x
        member _.ReturnFrom x = x
        member _.Zero () = Pure ()
        member _.Delay(f : unit -> IOProgram<'a>) : IOProgram<'a> = Delayed f
        member this.Combine(a,b) =
            this.Bind(a, fun _ -> b)
        member this.While (guard : unit -> bool, body : IOProgram<'a>) : IOProgram<unit> =
            if not (guard ()) then
                this.Zero ()
            else
                this.Bind (body, fun _ -> this.While (guard, body))
        member this.TryFinally(body, comp) =
            try
                this.ReturnFrom(body())
            finally
                comp()
        member this.Using (disp:#System.IDisposable, body) =
            let body' = fun () -> body disp
            this.TryFinally (body', fun () -> 
                match disp with
                | null -> ()
                | disp -> disp.Dispose())
        member this.For(seq:seq<'a>, body:('a -> IOProgram<'b>)) : IOProgram<unit> =
            this.Using (seq.GetEnumerator(), fun enum ->
                this.While(enum.MoveNext,
                    // Hier brauchen wir einen richtigen Delay-Daten-Konstruktor - mit dem "trivialen" `this.Delay f = f ()`
                    // würden wir hier zur Laufzeit ein Problem bekommen, weil dann `body enum.Current` vor dem ersten `enum.MoveNext()` aufgerufen würde
                    // Delay würde die `unit -> ..` ja sofort hier aufrufen
                    // Man kann das oft auch anders "hinfummeln" aber ich finde den Delay-Konstruktor in aller Regel am Zuverlässigsten/Einfachsten
                    this.Delay(fun () -> body enum.Current)))

    /// mit der Zeile wird dann der `io { ... }` Syntax ermöglicht
    let io = IOBuilder ()

    // unsere "Seiteneffekte" kapseln wir jetzt noch als schöne Funktionen:
    let writeText text = Free (WriteText (text, Pure))
    let readNr prompt max = Free (ReadNr (prompt, max, Pure))
    let clear = Free (Clear Pure)
