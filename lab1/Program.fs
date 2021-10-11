// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

let razyDwa x = 2 * x
    
let rec silnia x = 
    match x with
    | 0 -> 1
    | _ -> x * (silnia (x - 1)) ;;



let rec fun1 (s: float, g: float, i: int) =
    printf "podaj liczbę:"
    let p = Console.ReadLine()
    
    let x = float(p)
    match x with
    | 0.0 -> (s, (g ** (1.0/float(i))))
    | _ ->  fun1(s+x, x*g, i+1)
        

let rec znak (x: int, c: char)=
    match x with
    | 0 -> ()
    | x when x > 0 ->printf "%c" c
                     znak(x-1, c)
        
let rec pyramid (x: int, l: int )=
    match l with
    | l when l >= x -> printf " " 
    | l when l < x &&  x < System.Console.WindowHeight && x < System.Console.WindowWidth ->
        znak (l, '*')
        printfn " "
        pyramid(x, l+1)
        
    
let rec pyramid1 (x: int, l:int)=
     match l with
    | l when l >= x -> printf " " 
    | l when l < x &&  x < System.Console.WindowHeight && x < System.Console.WindowWidth ->
        znak (x-l-1, ' ')
        znak (l*2 + 1, '*')
        printfn " "
        pyramid1(x, l+1)

let rec podz(x: int, liczb: int, podzlist: list<int>) =
    match x with
    | 1 -> podz(x+1, liczb, podzlist)
    | x when x < liczb && liczb % x = 0 -> podz(x+1, liczb, x::podzlist)
    | x when x < liczb -> podz(x+1, liczb, podzlist)
    | x when x = liczb -> podzlist



exception UJEMNY_ARGUMENT_LUB_ZERO
let rec fun2 (x: int)=
    match x with
    | x when x > 0 && x % 4 = 1 -> true
    | x when x > 0 && x % 4 = 3 -> false
    | x when x > 0 && x % 2 = 0 -> fun2(x/2)
    | x when x <= 0  -> raise UJEMNY_ARGUMENT_LUB_ZERO

[<EntryPoint>]
let main argv =
//    let x = razyDwa 5
//    printfn "%i" x
    
//    printfn "%i" (silnia 5)
    
//    let wyn = fun1(0.0, 1.0, 0)
//    printf "suma: %f, średnia geometryczna: %f" <|| wyn
    
    
    pyramid (5, 1)
    printfn " "
    pyramid1(5, 0)
    printfn " "
//    let w1 = podz(1, 21, [])
    
//    printfn "%A" w1
    
//    let w2 = fun2(20)
//    printfn "%b" w2
    
    printf "Naciśnij klawisz..."
    ignore (System.Console.ReadKey(true))
    0 // return an integer exit code