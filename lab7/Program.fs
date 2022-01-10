// Learn more about F# at http://fsharp.org

open System

type prop =
    | Atom of string
    | Neg of prop
    | Alt of prop * prop
    | Kon of prop * prop
let Imp(p,q) = Alt(Neg p,q)
let rec show z =
    match z with
    | (Atom a) -> a
    | (Neg p) -> "(!" + (show p) + ")"
    | (Alt(p,q)) -> "(" + (show p) + " || " + (show q) + ")"
    | (Kon(p,q)) -> "(" + (show p) + " && " + (show q) + ")"

let rec nnf = function
    | (Atom a) -> Atom a
    | (Neg(Atom a)) -> Neg(Atom a)
    | (Neg(Neg p)) -> nnf p
    | (Neg(Alt(p,q))) -> nnf(Kon(Neg p,Neg q))
    | (Neg(Kon(p,q))) -> nnf (Alt(Neg p,Neg q))
    | (Kon(p,q)) -> Kon(nnf p, nnf q)
    | (Alt(p,q)) -> Alt(nnf p, nnf q)

let rec roz = function
    | (p,Kon(q,r)) -> Kon(roz(p,q), roz(p,r))
    | (Kon(q,r),p) -> Kon(roz(q,p), roz(r,p))
    | (p,q) -> Alt(p,q)

let rec cnf = function
    | (Alt(p,q)) -> roz(cnf p,cnf q)
    | (Kon(p,q)) -> Kon(cnf p,cnf q)
    | p -> p

exception NieCNF
let rec pos = function
    | (Atom a) -> [a]
    | (Neg (Atom _)) -> []
    | (Alt(p,q)) -> (pos p) @ (pos q)
    | _ -> raise NieCNF
let rec neg = function
    | (Atom _) -> []
    | (Neg (Atom a)) -> [a]
    | (Alt(p,q)) -> neg p @ neg q
    | _ -> raise NieCNF
let mem x l = List.exists (fun u -> u=x) l

let rec inter = function
    | ([],ys) -> []
    | (x::xs,ys) -> if mem x ys then x::inter(xs,ys)
                    else inter(xs,ys)
let rec taut = function
    | (Kon (p,q)) -> (taut p) && (taut q)
    | p -> ([] <> inter(pos p, neg p))
let check(s) = taut (cnf (nnf (s)))


[<EntryPoint>]
let main argv =
    printfn "Weryfikacja 1."
    let b = Atom "b"
    let z = Atom "z"
    let s = Atom "s"
    printfn "b = %s" (show b)
    let zal1 = Imp(z,b)
    let zal2 = Neg(Kon(s,b))
    let teza = Imp(z,Neg s)
    let twierdz = Imp(Kon(zal1,zal2),teza)
    printfn "twierdz = %s" (show twierdz)
    printfn "zal2 = %s" (show (nnf zal2))
    printfn "Kon(zal1,zal2) = %s" (show (cnf (nnf
    (Kon(zal1,zal2)))))
    let ctwierdz = cnf(nnf twierdz)
    printfn "ctwierdz = %s" (show ctwierdz)
    printfn "check = %A" (check twierdz)
    printfn ""
    
    printfn "Weryfikacja 2."
    let Ekw(p,q) = Alt(Kon(p,q),Kon(Neg p, Neg q))
    let p = Atom "p"
    let q = Atom "q"
    let L = Ekw(p,q)
    let P = Kon(Imp(p,q),Imp(q,p))
    let wzor = Ekw(L,P)
    printfn "wzor = %s" (show wzor)
    printfn "check = %A" (check wzor)
    
    let L2 = Ekw(p,q)
    let P2 = Kon(Imp(p,q),Imp(Neg p,Neg q))
    printfn "check = %A" (check (Ekw(L2,P2)))
    
    let L3 = Ekw(Kon(p,q),p)
    let P3 = Imp(p,q)
    printfn "check = %A" (check (Ekw(L3,P3)))
    
    let L4 = Alt(Kon(p,q),Kon(Neg p, Neg q))
    let P4 = Alt(Kon(p,q),Neg p)
    printfn "check = %A" (check (Ekw(L4,P4)))
    
    let a = Atom "a"
    let b = Atom "b"
    let x = Atom "x"
    let y = Atom "y"
    let z = Atom "z"
    let L5 = Alt(Kon(a,Ekw(x,y)),Kon(Neg a,Ekw(x,z)))
    let P5 = Ekw(x, Alt(Kon(a,y),Kon(Neg a, z)))
    let pw_wzor = Ekw(L5,P5)
    printfn "check = %A" (check pw_wzor)
    
    printfn ""
    printfn "Sterowanie logiczne"
    
    let OR_S(p,q) = Alt(p,q)
    let AND_S(p,q) = Kon(p,q)
    let NOT_S(p) = Neg p
    
    let START = Atom "START"
    let STAN = Atom "STAN"
    let STOP = Atom "STOP"
    let X1OR = START
    let X2OR = STAN
    let YOR = OR_S(X1OR, X2OR)
    let XNOT = STOP
    let YNOT = NOT_S(XNOT)
    let X1AND = YOR
    let X2AND = YNOT
    let YAND = AND_S(X1AND,X2AND)
    let OUT = YAND
    printfn "out = %A" (show OUT)
    
    let OUT_SPEC = Kon(Alt(START,STAN),Neg(STOP))
    let twierdz_prog = Ekw(OUT_SPEC, OUT)
    printfn "twierdz_prog = %s" (show twierdz_prog)
    printfn "check = %A" (check twierdz_prog)
    
    let StartStop(start,stan,stop) =
        let YOR = OR_S(start,stan)
        let YNOT = NOT_S(stop)
        in
        AND_S(YOR,YNOT)
    let OUT_SPEC = Kon(Alt(START,STAN),Neg(STOP))
    let twierdz_prog1 = Ekw(OUT_SPEC, StartStop(START,STAN,STOP))
    printfn "check = %A" (check twierdz_prog1)
    
    let IF_S(B,S1,S2) = Alt( Kon(B,S1), Kon(Neg B,S2) )
    
    let True = Atom "True"
    let TRUE = Alt(True, Neg True)
    let FALSE = Neg TRUE
    
    printfn ""
    printfn "EXOR"
    let EXOR_S(P,Q) = IF_S( Kon(Neg P,Neg Q), FALSE,
                        IF_S( Kon(Neg P,Q), TRUE,
                        IF_S( Kon(P,Neg Q), TRUE, FALSE)))
    
    let P = Atom "P"
    let Q = Atom "Q"
    let EXOR_SPEC = Alt( Kon(P,Neg Q), Kon(Neg P,Q) )
    show (EXOR_S(P,Q))
    let twierdzenie = Ekw(EXOR_S(P,Q), EXOR_SPEC)
    printfn "twierdzenie = %s" (show twierdzenie)
    printfn "check = %A" (check twierdzenie)
    
    printfn ""
    printfn "MULTIPLEKSER"
    let MUX_S(S,X0,X1) = Alt(Kon(Neg S,X0),Kon(S,X1))
    let MUX_S(S,X0,X1) =
        let YNOT = NOT_S(S)
        let YAND1 = AND_S(YNOT,X0)
        let YAND2 = AND_S(S,X1)
        in
        OR_S(YAND1,YAND2)
        
    let tw_multi = Ekw(MUX_S(P,Q,TRUE),Alt(P,Q))
    printfn "multiplekser = %s" (show tw_multi)
    printfn "check = %A" (check tw_multi)
    
    printfn ""
    printfn "ELEMENTARNE STEROWANIE KOMBINACYJNE"
    let G_S(a,b) = IF_S(Kon(Neg a,Neg b), TRUE,
                    IF_S( Kon(a, Neg b), TRUE,
                    IF_S( Kon(a,b), FALSE, FALSE)))
    let W_S(a,b) = IF_S(Kon(Neg a,Neg b), FALSE,
                    IF_S( Kon(a, Neg b), TRUE,
                    IF_S( Kon(a,b), TRUE, FALSE)))
    
    
    let a = Atom "a"
    let b = Atom "b"
    let GRZ_SPEC = Neg b
    let W_SPEC = a
    
    let grzalka = Ekw(G_S(a,b),GRZ_SPEC)
    printfn "grzalka = %A" (check grzalka)
    let wentylator = Ekw(W_S(a,b),W_SPEC)
    printfn "wentylator = %A" (check wentylator)
    
    
    0 // return an integer exit code
