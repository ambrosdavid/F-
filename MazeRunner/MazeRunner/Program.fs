open System
open System.ComponentModel
open System.Threading

let Grnd=30


module Generator=
    type RandomObject () =

        let mutable nextInt = System.Random().Next(1,4)
        member this.next=
            nextInt


    type Labirinto (grandezza:int) =
        let mutable my2dArray = Array2D.create grandezza grandezza false
        let mutable nextInt = System.Random()

        let mutable posIniziale=(nextInt.Next( (grandezza-1)/4 ,(grandezza-1)/2),1)

        let mutable soluzione=[]

        let mutable ris=""//stringa usata come buffer per stampare tutto in un colpo



        member this.grandezza = grandezza
        member this.getPosIniziale=posIniziale
        member this.getMatrice=my2dArray
        member this.getSoluzione=soluzione
        
        
        //false=muro
        //true=passaggio libero
        member this.generaLabirinto =
            printfn "Press arrows to play"

            let haSpazioGiu (x:(int*int))=
                if(fst(x)>1 && fst(x)<this.grandezza-2 && snd(x)<this.grandezza-2 && snd(x)>2) then 
                    if(my2dArray.[fst(x)+1,snd(x)]=false && my2dArray.[fst(x),snd(x)+1]=false && my2dArray.[fst(x),snd(x)-1]=false) then true else false
                else false
            let haSpazioDx (x:(int*int))=
                if(fst(x)>1 && fst(x)<this.grandezza-2 && snd(x)<this.grandezza-2 && snd(x)>2) then 
                    if(my2dArray.[fst(x)+1,snd(x)]=false && my2dArray.[fst(x),snd(x)+1]=false && my2dArray.[fst(x)-1,snd(x)]=false) then true else false
                else false

            let haSpazioSx (x:(int*int))=
                if(fst(x)>1 && fst(x)<this.grandezza-2 && snd(x)<this.grandezza-2 && snd(x)>2) then 
                    if(my2dArray.[fst(x)+1,snd(x)]=false && my2dArray.[fst(x),snd(x)-1]=false && my2dArray.[fst(x)-1,snd(x)]=false) then true else false
                else false

            let haSpazioSu (x:(int*int))=
                if(fst(x)>1 && fst(x)<this.grandezza-2 && snd(x)<this.grandezza-2 && snd(x)>2) then 
                    if(my2dArray.[fst(x)-1,snd(x)]=false && my2dArray.[fst(x),snd(x)-1]=false && my2dArray.[fst(x),snd(x)+1]=false) then true else false
                else false


            
            let rec remove i l =
                match i, l with
                | 0, x::xs -> xs
                | i, x::xs -> x::remove (i - 1) xs
                | i, [] -> failwith "index out of range"


            let rec insert v i l =
                match i, l with
                | 0, xs -> v::xs 
                | i, x::xs -> x::insert v (i - 1) xs 
                | i, [] -> failwith "index out of range"

            
            let inside_borders x=
                fst(x)<grandezza-2 && fst(x)>=1 && snd(x)>=1 && snd(x)<grandezza-2

            let prendiMuri x= 
                if(inside_borders (fst(x)+1,snd(x))) then
                    if (my2dArray.[fst(x)+1,snd(x)])= false then
                        
                        (fst(x)+1,snd(x))::[]     
                    else [] 
                else []
            let prendiMuri2 xy=
                if(inside_borders (fst(xy)-1,snd(xy))) then
                    if (my2dArray.[fst(xy)-1,snd(xy)])=false then (fst(xy)-1,snd(xy))::[] else []
                else []
            let prendiMuri3 xy=
                if(inside_borders (fst(xy),snd(xy)+1)) then
                    if (my2dArray.[fst(xy),snd(xy)+1])=false then (fst(xy),snd(xy)+1)::[] else []
                else []
            let prendiMuri4 xy=
                if(inside_borders (fst(xy),snd(xy)-1)) then
                    if (my2dArray.[fst(xy),snd(xy)-1])=false then (fst(xy),snd(xy)-1)::[]  else []
                else []
                
            let rec unzip l=
                match l with
                |[]->[]
                |x::xs->x@unzip xs
            
            let lista_muri pos=unzip ([prendiMuri pos]@[prendiMuri2 pos]@[prendiMuri3 pos]@[prendiMuri4 pos])
            
            
            //printfn "%A" (lista_muri (10,10))
            my2dArray.[fst(posIniziale),snd(posIniziale)+1]<-true
            let rec Prims lista=
                match lista with
                []->()
                |x::xs->let ris=nextInt.Next(0,(x::xs).Length)
                        let wall=lista.Item(ris)
                        
                        if (((my2dArray.[fst(wall)-1, snd(wall)]) && haSpazioGiu wall) || (((my2dArray.[fst(wall)+1,snd( wall)]) && haSpazioSu wall) || ((my2dArray.[fst(wall),snd( wall)-1]) && haSpazioDx wall)||(my2dArray.[fst(wall), snd(wall)+1]) && haSpazioSx wall) && inside_borders wall )then
                            my2dArray.[fst(wall), snd(wall)]<-true
                            let newList=(remove ris lista)@(lista_muri wall)
                            Prims (newList)
                        else Prims (remove ris lista)
                        
            lista_muri (fst(posIniziale),snd(posIniziale)+1) |> Prims
            let creaFine =
                let mutable r=nextInt.Next(0,grandezza-1)
                 
                while (my2dArray.[r,grandezza-3]=false) do
                    r<-nextInt.Next(0,grandezza-1)

                my2dArray.[r, grandezza-2]<-true
                my2dArray.[r, grandezza-1]<-true
            creaFine




let l=new Generator.Labirinto(Grnd)
l.generaLabirinto

module view=

    let mutable posizione=l.getPosIniziale
    let matrice=l.getMatrice

    let printMatrix x y=
            let mutable ris="           "
            let mutable y1=y
            let mutable x1=x
            System.Console.Clear()
            for i in 0 .. l.grandezza-1 do
                for j in 0 .. l.grandezza-1 do
                    let mutable isPercorso = false;
                    for m in 0..l.getSoluzione.Length-1 do
                        if(l.getSoluzione.Item(m)=(i,j) && not((i=x) && (j=y))) then 
                            isPercorso <- true
                            ris<-ris+"x " else ()
                    if (i=x && j=y )then ris<-ris+"|■" else if not(matrice.[i,j]) then ris<-(ris+"██") else if not(isPercorso) then ris<-(ris+"  ")
                    //if (i=x && j=y1)then printf "|■" else if not(matrice.[i,j]) then printf "██" else printf "  "
                    if(y1>l.grandezza-2 || x1<1||x1=l.grandezza-1)then
                        System.Console.Clear()
                        printfn "      /*       _\|/_                                                             "
                        printfn "               (o o)                                                             "
                        printfn "       +----oOO-{_}-OOo---------------------------------+                        "
                        printfn "       |                             __    _            |                        "
                        printfn "       |     ____ ___  ___ _      __/ /_  (_)__  _____  |                        "
                        printfn "       |    / __ `__ \/ _ \ | /| / / __ \/ / _ \/ ___/  |                        "
                        printfn "       |   / / / / / /  __/ |/ |/ / /_/ / /  __(__  )   |                        "
                        printfn "       |  /_/ /_/ /_/\___/|__/|__/_.___/_/\___/____/    |                        "
                        printfn "       |                                                |                        "
                        printfn "       +-----------------------------------------------*/                        "
                        
                        

                        y1<-snd(l.getPosIniziale)
                        x1<-fst(l.getPosIniziale)

                        posizione<-l.getPosIniziale
                //printf "\n"
                ris<-ris+"\n           "              
            printf "%s" ris


module BackTracking=

    type BackTracking (posIniziale:(int*int), matrice1:bool[,], grandezza)=

        
        let mutable matrice = Array2D.copy matrice1 //copio la matrice di true false iniziale per evitare di modificare quella originale
        
        let mutable matriceDiPos= Array2D.create grandezza grandezza (0,0)//creo una matrice di tuple inizialmente tutte (0,0), stessa grandezza del labirinto
        
        member this.trovaPercorsi  = //member vuol dire che è una funzione che posso chiamare da fuori e che mi fa tutte le sottofunzioni qui sotto

            let rec creaMatricePosizioni x y= //funzione che data una cella, da' la sua posizione a tutte le celle adiacenti che sono true
                matrice.[x,y]<-false //la prima cosa che viene fatta, è settare la cella corrente a false in modo da evitare, al ciclo successivo, di darle la posizione di un altra cella
                if ((not matrice.[x,y+1] && not matrice.[x,y-1] && not matrice.[x+1,y] && not matrice.[x-1,y]) || y>grandezza-3) then
                    ()//caso base, controlla se sono uscito fuori dai bordi 
                else if(matrice.[x,y+1]) then//***sono nella posizione x,y(passati come parametri), se la cella alla mia destra è true(chiamiamola cella Z), allora
                //vado sulla matriceDiPosizioni e nella cella Z metto la posizione della cella corrente
                //in questo modo uno che si trova nella cella Z ha la posizione della cella precedente a Z
                    matriceDiPos.[x,y+1]<-(x,y)
                    creaMatricePosizioni x (y+1)
                else()
                if(matrice.[x,y-1]) then //stessa cosa qua ma per cella di sinistra
                    matriceDiPos.[x,y-1]<-(x,y)
                    creaMatricePosizioni x (y-1)
                else ()
                if(matrice.[x+1,y]) then//per cella di sotto
                    matriceDiPos.[x+1,y]<-(x,y)
                    creaMatricePosizioni (x+1) y
                else ()
                if(matrice.[x-1,y]) then
                    matriceDiPos.[x-1,y]<-(x,y)
                    creaMatricePosizioni (x-1) y
                else ()
            creaMatricePosizioni (fst(posIniziale)) (snd(posIniziale))  

        member this.PercorsoRisolutivo=

            let posFinale= //trova la posizione finale del labirinto
                let mutable tmp=(grandezza+1,grandezza+1)//variabile volatile provisoria, settata a indici fuori dalla matrice
                for i in 0..l.grandezza-1 do //ciclo che in ogni riga dell ultima colonna cerca una cella che sia true, quando la trova, 
                //assegna a tmp il valore contenuto dentro la cella all'indice appena trovato della matriceDiPos
                    if(matrice.[i,l.grandezza-1]) then tmp <- matriceDiPos.[i,l.grandezza-2] else ()
                tmp //TMP DIVENTA QUINDI LA TUPLA(x,y) della posizione e cui punta la cella finale del nostro labirinto
                //quest'ultima cella finale, infatti contiene come valore, l'indice della cella precedente, che viene dato quindi a TMP

            let rec backTrackPuro pos= //funzione per trovare il percorso completo
                if(matriceDiPos.[(fst(pos)),(snd(pos))]<>(0,0))then //caso base:se mi trovo in una cella con VALORE (0,0), allora interrompi
                    backTrackPuro matriceDiPos.[(fst(pos)),(snd(pos))]//altrimenti prendi il valore contenuto nella cella corrente e fammi il backtraking della cella che si trova all indice di tale valore
                    matriceDiPos.[(fst(pos)),(snd(pos))]<-(1,1)// e settami il valore della cella corrente (-1,-1)
                else ()//caso base:esci dalla funzione
            backTrackPuro posFinale


        member this.stampaMatricePosizioni= // funzione per stampare la matrice di posizioni
            for i in 0..l.grandezza-1 do
                for j in 0..l.grandezza-1 do
                     if(matriceDiPos.[i,j]<>(0,0)) then printf "(%A,%A)" (fst(matriceDiPos.[i,j])) (snd(matriceDiPos.[i,j])) else printf "( , )"
                printf "\n"  
        
        member this.stampaBackTracking= //stampa il labirinto con disegnato il percorso risolutivo
            for i in 0..l.grandezza-1 do
                for j in 0..l.grandezza-1 do
                     if(matriceDiPos.[i,j]=(1,1)) then printf " O"
                     else if (matriceDiPos.[i,j]<>(0,0)) then printf "  "
                     else printf "██"
                printf "\n"

let bt=new BackTracking.BackTracking (l.getPosIniziale, l.getMatrice, Grnd)
bt.trovaPercorsi
//bt.stampaMatricePosizioni
bt.PercorsoRisolutivo
bt.stampaBackTracking
//bt.stampaMatricePosizioni




module Sprite=
    
    type Sprite (posIniziale:(int*int), matrice:bool[,])=
        let mutable posizione=posIniziale
        let matrice=matrice
        
        let mutable _sx=false;
        let mutable _dx=false;
        let mutable _su=false;
        let mutable _giu=false;



        member this.falseTutto =
            _sx<-false;
            _su<-false;
            _giu<-false;
            _dx<-false;


        member this.setGiu=

            if(matrice.[fst(posizione)+1,snd(posizione)]=true)then
                posizione<-(fst(posizione)+1,snd(posizione))
            else()
            view.printMatrix (fst(posizione)) (snd(posizione))

            
    
        member this.setSu=
            _su<-true
            if(matrice.[fst(posizione)-1,snd(posizione)]=true)then
                posizione<-(fst(posizione)-1,snd(posizione))
            else ()
            view.printMatrix (fst(posizione)) (snd(posizione))
            this.falseTutto
            

        member this.setDx=
            
            _dx<-true
            if((snd(posizione))=l.grandezza-1)then
                posizione<-(fst(posIniziale),snd(posIniziale))
            else
            if(matrice.[fst(posizione),snd(posizione)+1]=true)then
                posizione<-(fst(posizione),snd(posizione)+1)
            else()
            view.printMatrix (fst(posizione)) (snd(posizione))
            this.falseTutto

        member this.setSx=
        
            _sx<-true
            if(matrice.[fst(posizione),snd(posizione)-1]=true)then
                posizione<-(fst(posizione),snd(posizione)-1)
            else()
            view.printMatrix (fst(posizione)) (snd(posizione))
            this.falseTutto
        

        
    
let sprite = Sprite.Sprite(l.getPosIniziale, l.getMatrice)
//l.printMatrix (fst(l.getPosIniziale)) (snd(l.getPosIniziale))


module Control =

    let onKey (k: string): bool =
        match k with
        | (("DownArrow") | ("S")) ->
            ((sprite.setGiu)
             true)
        | "UpArrow" ->
            ((sprite.setSu)
             true)
        | "LeftArrow" ->
            ((sprite.setSx)
             true)
        | "RightArrow" ->
            ((sprite.setDx)
             true)

        | _ -> (false)

let rec reactiveKey() =
    async {
        let! key = Async.FromContinuations(fun (cont, _, _) ->
                       cont (Console.ReadKey())
                       reactiveKey())
        let keyName: string = key.Key.ToString()

        let needToRefresh = Control.onKey keyName
        if needToRefresh then
            ()//assurdo
    }
    |> Async.Start

reactiveKey()       

        
System.Threading.Thread.Sleep(-1);


        