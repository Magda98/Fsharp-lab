// Learn more about F# at http://fsharp.org

open System


let dodaj(l: list<int>) = List.fold(fun s e -> s + e) 0 l
    
let st(l: list<string>) = List.fold(fun x e -> x+e) "" l
let zam(l : list<int>) = st(List.map(fun e -> string(e)) l)

let pf(s: string) =
    match System.Double.TryParse(s) with
    | true, s -> Some(s)
    | false, _ -> None
let zamFloat(l: list<string>) = List.map(fun e -> pf(e)) l

let ptuple(t)=
    match t with
    |(a, b) -> printfn "%A : %A" a b
let wys(l1: float option list, l2: list<string>) = List.iter(fun e1  -> ptuple(e1)) (List.zip l1 l2)

let rec wyszukajMinimum(lst: list<int>, min: int)=
    match lst with
    | [] -> min
    | h::t when h < min -> wyszukajMinimum(t, h)
    | h::t when h >= min -> wyszukajMinimum(t, min)
     
let rec wyszukajMaximum(lst: list<int>, max: int)=
    match lst with
    | [] -> max
    | h::t when h > max -> wyszukajMaximum(t, h)
    | h::t when h <= max -> wyszukajMaximum(t, max)
    
[<EntryPoint>]
let main argv =
    let l = [1;2;3;4;5;6]
    let w = dodaj(l)
    printfn "%A" w
    
    let w1 = zam(l)
    printfn "%A" w1
    
    let t = ["113";"23";"sd"]
    let w2 = zamFloat t
    
    printfn "%A" w2
    
    let w3 = wys(w2, t)
    0 // return an integer exit code
