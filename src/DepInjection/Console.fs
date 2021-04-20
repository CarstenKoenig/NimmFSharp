namespace Nimm.Game.DependencyInjection

type Console () =    
    interface IDoIO with
        member this.Clear(): unit = 
            System.Console.Clear()
        member this.ReadText(prompt: string) = 
            System.Console.Write (prompt + " ")
            System.Console.ReadLine ()
        member this.WriteText(text: string) = 
            System.Console.WriteLine text
