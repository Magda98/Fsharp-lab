// Learn more about F# at http://fsharp.org

open System
/////////////////////////////////////
///
///
///

type Owca(r: string) as this=
    [<DefaultValue>]
    val mutable rasa : string
    do
        this.rasa <- r 



/////////////////////////////////////
exception InvalidArgumentException

type htow = int list

let sprzwdz(w: htow, n: int)=
    match w with
    | x::t -> if x > n then true else false
    | [] -> true
    
let wyloz(w: htow, n: int)=
    if sprzwdz(w, n) then n::w
    else  raise InvalidArgumentException

let rec wysokosc(w: htow)=
    match w with
    | h::t -> 1 + wysokosc(t)
    | [] -> 0
    
    
let rec sprawdzW(w: htow)=
    match w with
    | h::t -> if((List.filter(fun n -> n = h) t).Length = 0) then sprawdzW(t) else false
    | [] -> true



type moneta =
    | Drahma
    | Obol
    | Srebrnik
    | Lepton
    
let rec utworzListe ()=
    let wal = System.Console.ReadLine().Trim().Split(" ") |> List.ofArray
    List.map ( fun h -> match h with
                        | "D" | "d" -> Drahma 
                        | "O" | "o" -> Obol
                        | "S" | "s" -> Srebrnik
                        | "L" | "l" -> Lepton
                        | _ -> raise <| new System.ArgumentException() ) wal

let zlicz(l: list<moneta>) =
    List.fold( fun (d, o, s, l) w -> match w with
                                     | Drahma -> (d+1, o, s, l)
                                     | Obol -> (d, o+1, s, l)
                                     | Srebrnik -> (d, o, s+1, l)
                                     | Lepton -> (d, o, s, l+1)
                                     ) (0,0,0,0) l
[<EntryPoint>]
let main argv =
    
    let w = [2;3;4;5;6]
    printfn "%A" <| sprzwdz(w, 1)
    
    printfn "%A" <| wyloz(w, 1)
    
    printfn "%A" <| wysokosc(w)
    
    let l = [1;1;3;4;5;6;2;3;4;2;1]
    printfn "%A" <| sprawdzW(w)
    
    let l = utworzListe()
    
    printfn "%A" <| zlicz(l)
    0 // return an integer exit code
