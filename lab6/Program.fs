// Learn more about F# at http://fsharp.org

open System

[<AbstractClass>]
type Zwierze(nazwa) =
    let mutable nazwa : string = nazwa
    let mutable dataUr : DateTime = new DateTime(0L)
    
    member this.Opis() = printfn("opis zwierze")
    abstract member Odglos: unit -> unit
    default this.Odglos() = printfn("* odglosy zwierzęcia *")
    
    
    
type Lew(nazwa) =
    inherit Zwierze(nazwa)
    override this.Odglos() = printfn("Arrrr")
    member this.Opis() = printfn("Lew opis asdasdasd")
    

type Slon(nazwa) =
    inherit Zwierze(nazwa)
    override this.Odglos() = printfn("Turum")
    member this.Opis() = printfn("Słoń to największy ssak")
    
type LewIndyjski(nazwa) =
    inherit Lew(nazwa)
    member this.Opis() = printfn("Lew Indyjski  to ... opis asdasdasd")

[<EntryPoint>]
let main argv =
    let s1 = new Slon("Slonik1")
    let s2 = new Slon("Slonik2")
    let s3 = new Slon("Slonik3")
    let l1 = new Lew("Simba")
    let l2 = new LewIndyjski("Simba Copy")
    let lz: Zwierze list = [s1;s2;s3;l1;l2]
    
    List.iter(fun (x: Zwierze)  ->  x.Opis()) (lz)
    List.iter(fun (x: Zwierze)  ->  x.Odglos()) (lz)
    
    l2.Opis()
    l2.Odglos()
    
    
    0 // return an integer exit code
