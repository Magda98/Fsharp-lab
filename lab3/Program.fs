// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
let ulamekZwykly (s: string)=
    let k = s.IndexOf('.')
    let calk = s.Substring(0, k)
    let fp = s.IndexOf('(')
    let lp = s.IndexOf(')')
    let o = s.Substring(fp+1, lp-fp-1)
    let c = s.Substring(k+1, fp-k-1)
    let lo = o.Length
    let ns = new System.String('9', lo)
    let licznik = int(c+o) - int(c)
    let mianownik = 10.0**float(c.Length) * float(ns)
    (int(calk) ,int(licznik), int(mianownik))
    
let rec nwd(l: int, m: int)=
    match m with
    | 0 -> l
    | m when m > 0 -> nwd(m, l%m)
    
let skroc (c: int, l: int, m:int)=
    let dz = nwd(l, m)
    (c, l/dz, m/dz)


let rec p(z: string, k:  list<string>)=
    match k with
    | [] -> []
    | h::t -> z+h::p(z, t)

let rec permutacje(z : list<string>, z1 : list<string>)=
    match z with
    | [] -> []
    | h::t -> p(h,z1)@permutacje(t, z1)
    
let rec perm(k:  list<string>, k2:  list<string>, l:int)=
    match l with
    | 2 -> permutacje(k, k2)
    | l when l > 2 -> perm(permutacje(k, k2), k2, l-1)
        
[<EntryPoint>]
let main argv =
    let x = "0.33(3)"
    let calk, licznik, mianownik = ulamekZwykly(x)
    let w1 = skroc(calk, licznik, mianownik)
    printfn "%A" w1
    
    let z = ["a";"b";"c"]
    
    let w2 = perm(z,z,  4)
    printfn "%A" w2
    
    0 // return an integer exit code