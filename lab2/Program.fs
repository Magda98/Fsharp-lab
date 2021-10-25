// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

type LD = byte list

let rec zmnLDStr (n: LD) : string =
    match n with
    | [] -> ""
    | h::t -> h.ToString() + zmnLDStr(t)
        
        
let rec zmnStrInt (s: string) : LD =
    let lz = List.ofArray <|s.ToCharArray()
    let rec zamien = function
        | [] -> []
        | h::t -> ((byte h) - 0x30uy)::zamien t
    zamien lz

// list.map (for (x: char) -> (byte x) -0x30uy)    


let rec dodaj (n1 : LD, n2 : LD , nd : byte) : LD =
    match n1, n2, nd with
    | [], [], 0uy -> []
    | [], [], x -> [x]
    | [], h::t, _ -> let w = h + nd
                     let r = w % byte(10)
                     r::dodaj(n1, t, w/byte(10))
    | h::t, [], _ -> let w = h + nd
                     let r = w % byte(10)
                     r::dodaj(t, n2, w/byte(10))
    | h1::t1, h2::t2, _ -> let w = h1 + h2 + nd
                           let r = w % byte(10)
                           r::dodaj(t1, t2, w/byte(10))
    

//let rec pomnoz (n1: LD, n2: LD, nd: byte) : LD =
//    match n1, n2, nd with
//    | [], [], 0uy -> []
//    | h::t,
let rec mnozSkalar (w:byte) = function
    | [] -> []
    | h::t -> (w*h)::mnozSkalar w t
    
let rec mnozlisty (a: LD) (b: LD) (w: LD) =
    match b with
    | [] -> w
    | h::t -> let iloczyn = mnozSkalar h a
              let w1 = dodaj(w, iloczyn, byte(0))
              mnozlisty (0uy::a) t w1
              
let mnoz (n1 : LD, n2 : LD ) : LD =
    let cr = mnozlisty n1 n2 []
    List.rev(cr)
    
let rec odloz (x: list<int>, y : int) =
    match x with
    | _ -> y::x
   
let rec pobierz (x: list<int>) =
    let ls = List.rev x
    match ls with
    | [] -> []
    | h::t -> [h]

[<EntryPoint>]
let main argv =
    let liczba = "123"
    let wynik = zmnStrInt(liczba)
    printfn "%A" wynik
    let wynik1 = zmnLDStr(wynik)
    printfn "%s" wynik1
    
    let l1 = "320"
    let l2 = "123"
    
    let l1LD = zmnStrInt(l1)
    let l2LD = zmnStrInt(l2)
//    let w = List.rev(dodaj(List.rev(l1LD), List.rev(l2LD), byte(0)))
    let w = mnoz(List.rev(l1LD), List.rev(l2LD))
    printfn "%A" w
    
    
    
    0 // return an integer exit code
