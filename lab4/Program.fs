// Learn more about F# at http://fsharp.org

open System

//rachunek 
type kodArt = string
type nazwaArt = string
type cena = int 
type rejestr = (kodArt * (nazwaArt * cena)) list
type sztukiA = int
type pozArt = sztukiA * kodArt
type zakupy = pozArt list
type infoArt = sztukiA*nazwaArt*cena
type infoLista = infoArt list
type rachunek = infoLista * cena
let Rejestr = [
    ("a1", ("ser biały", 3));
    ("a2", ("szprotki", 2));
    ("a3", ("sok", 4))
]
let zakupy = [
    (4, "a2");
    (1, "a1")
]
exception BLEDNY_KOD_ARTYKULU
let rec szukajArt u = function 
      | []  -> raise BLEDNY_KOD_ARTYKULU
      | (k, (n, c)) :: baza_ogon 
          -> if u = k then (n, c)
             else (szukajArt u baza_ogon)
let rec utworzRachunek = function
    | ([], _) -> ([], 0)
    | ((sztA, kA)::zak, rej) -> 
       let (nazA, cenA) = szukajArt kA rej
       let kosztA = sztA * cenA
       let (Rach, suma) = utworzRachunek(zak, rej)
       in ((sztA, nazA, kosztA)::Rach, kosztA + suma)
// rachunek koniec 

//biuro
type imieNazw = string
type telefon = int
type plec = string (* "Z" lub "M" *)
type rokUr = int
type hobby = string
type zainteresowania = hobby list
type daneOsoby = imieNazw * telefon * plec * rokUr * zainteresowania
type plik = daneOsoby list
type daneTwoje = plec * rokUr * zainteresowania
let rec czyElement x = function
  | y::ys -> x=y || (czyElement x ys)
  | [] -> false
let rec wspElement = function
  | (x::xs,ys) -> (czyElement x ys) || (wspElement(xs,ys))
  | ([], _) -> false (* istnieje wspólny element obu list *)
let zgodnosc = function
    | ((p,r,z),(_,_,p',r',z')) -> p <> p' && abs(r-r')<10 && wspElement(z,z')
let rec kandydaci = function
    | (_,[]) -> []
    | (dT:daneTwoje, (dO:daneOsoby)::xs) -> 
         if zgodnosc(dT,dO) 
           then let (a, b, _, _, _) = dO in (a, b)::kandydaci(dT,xs)
           else kandydaci(dT,xs)
//biuro koniec

let rec usunNieparzyste(lst: list<int>, i: int)=
    match lst with
    | [] -> []
    | h::t when i % 2 = 0 -> h::usunNieparzyste(t, i+1)
    | h::t when i%2 = 1 -> usunNieparzyste(t, i+1)
 
 
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
    
let rec polacz(lst: list<int>)=
    match lst with
    | [] -> []
    | h1::h2::t -> (h1,h2)::polacz(t)
    | h1::t -> [(h1, h1)]
    
let rec length(lst: List<int>)=
    match lst with
    | [] -> 0
    | h::t -> 1+length(t)
    
let rec krotnosc(x: list<int>, ys: int)=
    match x with
    | [] -> 0
    | h::t when h = ys -> 1+krotnosc(t, ys)
    | _::t -> krotnosc(t, ys)
let powtorzenia(x: list<int>, el: int)=
    let k = krotnosc(x, el)
    match k with
    | 0 -> false
    | 1 -> false
    | k when k > 1 -> true
    
let rec rev(lst: list<int>, lstr: list<int>)=
    match lst with
    | [] -> lstr
    | h::t -> rev(t, h::lstr)
let x(y)=
    y+1
let rec map(f, lst: list<int>)=
    match lst with
    | [] -> []
    | h::t -> f(h)::map(f, t)

let rec wyszukaj(lst: list<int>, poz)=
    match (lst, poz) with
    | [],_ -> []
    | h::t, 0 -> [h]
    | h::t, _ -> wyszukaj(t, poz-1)
    
let rec sprawdzCzyNieujemne(lst: list<int>)=
    match lst with
    | [] -> true
    | h::t when h >= 0 -> sprawdzCzyNieujemne(t)
    | h::t when h < 0 -> false
 
let rec wstaw(lst: list<int>, el:int, poz:int)=
    match (lst, poz) with
    | [], 0 -> [el]
    | [], _ -> []
    | h::t, 0 -> h::el::t
    | h::t, _ -> h::wstaw(t, el, poz-1)

//let rec sprawdzPowtorzeniaRejestr(rej: rejestr)=
//    match rej with
//    | ()
    
[<EntryPoint>]
let main argv =
//    let artykul1 = szukajArt "a38" Rejestr
//    let artykul2 = szukajArt "a3" Rejestr
//    let rachunek1 = utworzRachunek([], Rejestr)
//    let rachunek2 = utworzRachunek(zakupy, Rejestr)
//    printfn "%A" rachunek2
//    let O1 = ("iN1",997,"Z",75,["a"])
//    let O2 = ("iN2",998,"M",70,["b"])
//    let Plik = [O1;O2]
//    let a = kandydaci(("Z",80,["a"]), Plik)
//    let b = kandydaci(("M",70,["b"]), Plik)
//    let c = kandydaci(("M",70,["c";"a"]), Plik)
//    printfn "%A" c
    
    let wynik = usunNieparzyste([1;1;2;3;4;5;6;7;8;9;10], 1)
    printfn "%A" wynik
    
//    let wynik1 = wyszukajMaximum([10;12;1;4;5;6;1;25;6;4;3;2], -1)
//    printfn "%A" wynik1
    
//    let wynik2 = polacz([1;2;3;4;5;6;7;8;9])
//    printfn "%A" wynik2
    let l = [1;2;3;4;5;6;7;8;9;10;1;1;1;1]
    let wynik3 = length(l)
    printfn "%A" wynik3
    
    let wynik4 = krotnosc(l, 1)
    printfn "%A" wynik4
    
    let wynik5 = powtorzenia(l, 67)
    printfn "czy element się powtarza: %A" wynik5
    
    let wynik6 = map(x, [1;2;3;4;5;6;7;8])
    printfn "%A" wynik6
    
    let wynik7= rev(l, [])
    printfn "%A" wynik7
    
    let wynik8 = wyszukaj([1;2;3;4;5;6;7;8], 1)
    printfn "%A" wynik8
    
    let wynik9 = sprawdzCzyNieujemne([1;2;3;4;5;6;7;8])
    printfn "%A" wynik9
    
    let wynik10 = wstaw([1;2;3;4;5;6;7;8], 5, 2)
    printfn "%A" wynik10
    
    
    0 // return an integer exit code
