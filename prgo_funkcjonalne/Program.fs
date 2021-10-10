let rec (^^) a b =
    match b with
    | 0 -> 1
    | _ -> a * (a ^^ (b - 1))

let rec suma m n =
    match n with
    | 0 -> 0
    | n when n > 0 -> m + (n - 1) + suma m (n - 1)


let rec tylkoMale (s: string, i: int) =
    match i with
    | -1 -> true
    | _ -> tylkoMale (s, i - 1) && System.Char.IsLower(s.[i])


exception UJEMNY_ARGUMENT

let rec mRn (m: int, n: int) =
    match n with
    | n when n < 0 -> raise UJEMNY_ARGUMENT
    | 0 -> 0
    | 1 -> m
    | n -> m + mRn (m, n - 1)



let rec wystOdItego (slowo: string, ch: char) =
    let i = (slowo.Length - 1)

    match i with
    | i when i < 0 -> 0
    | 0 -> if slowo.[i] = ch then 1 else 0
    | i ->
        if slowo.[i] = ch then
            1 + wystOdItego (slowo.[0..(i - 1)], ch)
        else
            0 + wystOdItego (slowo.[0..(i - 1)], ch)


let rec intersekcja (l1: list<int>, l2: list<int>, i: int, j: int) =
    match l1, l2, i, j with
    | [], [], _, _ -> []
    | _, [], _, _ -> []
    | [], _, _, _ -> []
    | _, _, -1, _ -> []
    | _, _, _, -1 -> intersekcja (l1, l2, i - 1, l2.Length - 1)
    | _, _, i, j ->
        if l1.[i] = l2.[j] then
            l1.[i] :: intersekcja (l1, l2, i, j - 1)
        else
            intersekcja (l1, l2, i, j - 1)


[<EntryPoint>]
let main _ =
    // let x = 5 ^^ 2
    // printf "%i \n" x

    // let k = suma 1 10
    // printfn "%i" k

    // let o = tylkoMale ("Abc", 2)
    // printfn "%b" o

    // let p = mRn (3, 2)
    // printfn "%i" p
    let x = [ 1; 2; 3; 4; 5; 6; 7; 8 ]
    let y = [ 5; 6; 7; 8 ]
    printfn "%A" (intersekcja (x, y, x.Length - 1, y.Length - 1))

    // let k = wystOdItego ("ksdsdf", 'k')

    // printfn "%i" k

    printf "Naciśnij klawisz..."
    ignore (System.Console.ReadKey(true))
    0
