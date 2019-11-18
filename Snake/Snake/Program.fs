// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System

let genRandomNumbers count =
    let rnd = System.Random()
    List.init count (fun _ -> rnd.Next (1,19))

type mela () =  
    let l = genRandomNumbers 2
    let mutable XX=(l.Head)
    let mutable YY=(l.Tail.Head)

    member this.setX x=
        XX<-x

    member this.setY y=
        YY<-y

    member this.getX=
        XX
    
    member this.getY=
        YY

    member this.createAgain=
        mela()
                  
type Scachiera ( larghezza , altezza)=
    let mutable my2dArray = Array2D.create larghezza altezza false
    member this.larghezza= larghezza
    member this.altezza = altezza
    member this.my2DArray = my2dArray

    member this.getGrandezza =
        this.larghezza

    member this.setPosMelaX x=
         x

    member this.setElement element x y=
        my2dArray.[x,y]<-element

    member this.getArray=
        my2dArray

    member this.printMatrix x y =
        //async{
        //while(true) do
        System.Console.Clear()
        
        for i in 0 .. larghezza-1 do
            for j in 0 .. altezza-1 do
                 //matrix.[i,j]<-false
                 if (i=x && j=y)then printf "|■" else if(my2dArray.[i,j])then printf "██" else printf "[]"
                 //printf "%A " my2dArray.[i,j]

            printf "\n"
        //do! Async.Sleep(150)
        //}   
    member this.clearMatrix x y  =

        //System.Console.Clear()
        
        for i in 0 .. larghezza-1 do
            for j in 0 .. altezza-1 do
                if(i=x && j=y) then
                 my2dArray.[i,j]<-true
                else my2dArray.[i,j]<-false
                 //printf "%A " my2dArray.[i,j]
            //printf "\n"

type Serpentone (x,y)=
    let mutable ArraySerpente = []
    

    let rec insert v i l =
        match i, l with
        | 0, xs -> v::xs 
        | i, x::xs -> x::insert v (i - 1) xs 
        | i, [] -> failwith "index out of range"

    let rec remove i l =
        match i, l with
        | 0, x::xs -> xs
        | i, x::xs -> x::remove (i - 1) xs
        | i, [] -> failwith "index out of range"

    let scachiera= Scachiera(19,19)
    let mutable _sx=false;
    let mutable _dx=false;
    let mutable _su=true;
    let mutable _giu=false;

    member this.x=x
    member this.y=y

    member this.su=_su;
    member this.giu=_giu;
    member this.sinistra=_sx
    member this.destra=_dx


    member this.falseTutto =
        _sx<-false;
        _su<-false;
        _giu<-false;
        _dx<-false;


    member this.setGiu=
        if not(_su)then
            this.falseTutto
            _giu <- true
        else ()
    
    member this.setSu=
        if not(_giu)then
            this.falseTutto
            _su <- true
        else ()

    member this.setDx=
        if not(_sx)then
            this.falseTutto
            _dx <- true
        else ()

    member this.setSx=
        if not(_dx)then
            this.falseTutto
            _sx <- true
        else()
    
   // member this.addTail=

    member this.getScachiera =
        scachiera

    member this.inizializzaSerpente=
            
        //scachiera.printMatrix
        async{
        //faccio partire il serpentone dal centro e gli salvo la gli indici in ArraySerpente
        scachiera.getArray.[this.y, this.x ]<-true
        ArraySerpente<-ArraySerpente@[(this.y,this.x)]
        printfn "%A l'ARRAY INIZIALE E" ArraySerpente
        //ArraySerpente<-insert (3,4) 1 ArraySerpente
        //ArraySerpente<-insert (7,4) 2 ArraySerpente
        //scachiera.getArray.[(fst (ArraySerpente.Item(1))) , (snd (ArraySerpente.Item(1)))]<-true
        
        
        let mutable indiceRighe = Array.create 20 0
        let mutable indiceColonne = Array.create 20 0
        

        for i in 0..0 do 
            indiceRighe.[i]<-(fst (ArraySerpente.Item(i)))
            indiceColonne.[i]<-(snd( ArraySerpente.Item(i)))
        
        let mela1=new mela()
        
        //printf "%A %A" mela1.getX mela1.getY
        let mutable x1=mela1.getX
        let mutable y1=mela1.getY
        scachiera.getArray.[x1,y1]<-true
          

        while(true)do
        if((fst (ArraySerpente.Item(0))=x1) && (snd (ArraySerpente.Item(0))=y1))then
            scachiera.getArray.[x1,y1]<-false
            ArraySerpente<-ArraySerpente@[(1,1)]
            for i in 0 .. ArraySerpente.Length-1 do
                
            let newMela=new mela()
            
            x1<-newMela.getX
            y1<-newMela.getY
            
        else scachiera.clearMatrix x1 y1
        for i in 1..ArraySerpente.Length-1 do
            if(ArraySerpente.Item(0)=ArraySerpente.Item(i)) then failwith "Warning: HAI PERSO brutto down" else ()
        
        

        
        (*let prevPos=(fst (ArraySerpente.Item(0)), snd( ArraySerpente.Item(0))) *)
        let prevPosArray=[for i in 0..ArraySerpente.Length-1 do yield (fst (ArraySerpente.Item(i)), snd( ArraySerpente.Item(i)))]


        //printfn "Prima while %A" ArraySerpente
        for i in 0..0 do 
            if(this.giu)then indiceRighe.[i]<-indiceRighe.[i]+1
            if(this.su)then indiceRighe.[i]<-indiceRighe.[i]-1
            if(this.destra)then indiceColonne.[i]<-indiceColonne.[i]+1
            if(this.sinistra)then indiceColonne.[i]<-indiceColonne.[i]-1
            //SINISTRA
            if( this.sinistra && indiceColonne.[i]<=scachiera.altezza)then
            

                scachiera.getArray.[indiceRighe.[i],indiceColonne.[i]]<-true
                ArraySerpente<-remove i ArraySerpente
                ArraySerpente<-insert ((indiceRighe.[i]),indiceColonne.[i]) 0 ArraySerpente
                //ArraySerpente.Item(i)<-((indiceRighe.[i]),indiceColonne.[i])

                //*scachiera.getArray.[indiceRighe.[i],indiceColonne.[i]+ArraySerpente.Length]<-false
      
            if(indiceColonne.[i]<=0)then
                indiceColonne.[i]<-scachiera.altezza
                ArraySerpente<-remove i ArraySerpente
                ArraySerpente<-insert (indiceRighe.[i],indiceColonne.[i]) i ArraySerpente
               // ArraySerpente.[i]<-((scachiera.altezza-1),indiceColonne.[i])
                //*scachiera.getArray.[indiceRighe.[0],0]<-false
            //DESTRA
            if( this.destra && indiceColonne.[i]<=scachiera.altezza-1)then
                scachiera.getArray.[indiceRighe.[i],indiceColonne.[i]]<-true
                ArraySerpente<-remove i ArraySerpente
                //ArraySerpente.[i]<-(indiceColonne.[i],indiceColonne.[i])
                ArraySerpente<-insert (indiceRighe.[i],indiceColonne.[i]) i ArraySerpente

                //*scachiera.getArray.[indiceRighe.[i],indiceColonne.[i]-ArraySerpente.Length]<-false
            else if(this.destra && indiceColonne.[i]>=scachiera.altezza)then
                //*scachiera.getArray.[indiceRighe.[i],indiceColonne.[i]-ArraySerpente.Length]<-false
                indiceColonne.[i]<-0
                //ArraySerpente.[i]<-(indiceRighe.[i],indiceColonne.[i])
                ArraySerpente<-remove i ArraySerpente
                ArraySerpente<-insert (indiceRighe.[i],0) i ArraySerpente
            //SU
            if( this.su && indiceRighe.[i]<=scachiera.altezza-1)then
       
                scachiera.getArray.[indiceRighe.[i],indiceColonne.[i]]<-true
                //ArraySerpente.[i]<-((indiceRighe.[i]),indiceColonne.[i])
                ArraySerpente<-remove i ArraySerpente
                ArraySerpente<-insert ((indiceRighe.[i]),indiceColonne.[i]) i ArraySerpente
                
                //*scachiera.getArray.[indiceRighe.[i]+ArraySerpente.Length,indiceColonne.[i]]<-false
                //scachiera.clearMatrix
            if(indiceRighe.[i]<=0)then
                indiceRighe.[i]<-scachiera.altezza-1
                //ArraySerpente.[i]<-((scachiera.altezza-1),indiceColonne.[i])
                ArraySerpente<-remove i ArraySerpente
                ArraySerpente<-insert ((scachiera.altezza-1),indiceColonne.[i]) i ArraySerpente
                //scachiera.getArray.[0,indiceColonne.[i]]<-false
                //scachiera.clearMatrix
            //GIU
            if( this.giu && indiceRighe.[i]<=scachiera.altezza-1)then
            

                scachiera.getArray.[indiceRighe.[i],indiceColonne.[i]]<-true
                //ArraySerpente.[i]<-((indiceRighe.[i]),indiceColonne.[i])
                ArraySerpente<-remove i ArraySerpente
                ArraySerpente<-insert ((indiceRighe.[i]),indiceColonne.[i]) i ArraySerpente


                //*scachiera.getArray.[indiceRighe.[i]-ArraySerpente.Length,indiceColonne.[i]]<-false
            
            else if(this.giu && indiceRighe.[i]>=scachiera.altezza-1)then
                //*scachiera.getArray.[indiceRighe.[i]-ArraySerpente.Length,indiceColonne.[i]]<-false
                indiceRighe.[i]<-0
                ArraySerpente<-remove i ArraySerpente
                //ArraySerpente.[i]<-((0),indiceColonne.[i])
                ArraySerpente<-insert (0,indiceColonne.[i]) i ArraySerpente
            
        
        //do! Async.Sleep(1000)
        
        for i in 1..ArraySerpente.Length-1 do
            ArraySerpente<-remove i ArraySerpente
            ArraySerpente<-insert (prevPosArray.Item(i-1)) i ArraySerpente
            scachiera.getArray.[fst( ArraySerpente.Item(i)),snd( ArraySerpente.Item(i))]<-true
        
        

        (*
        ArraySerpente<-remove 1 ArraySerpente
        ArraySerpente<-insert (prevPos) 1 ArraySerpente
        scachiera.getArray.[fst( ArraySerpente.Item(1)),snd( ArraySerpente.Item(1))]<-true
        *)
        //printfn "dopo while %A" ArraySerpente
        scachiera.printMatrix x1 y1
        
       // scachiera.printMatrix
        do! Async.Sleep(50)

        }

let serpentone = Serpentone(4, 4)
        
module Control =

    let onKey (k: string): bool =
        match k with
        | "DownArrow" ->
            ((serpentone.setGiu)
             true)
        | "UpArrow" ->
            ((serpentone.setSu)
             true)
        | "LeftArrow" ->
            ((serpentone.setSx)
             true)
        | "RightArrow" ->
            ((serpentone.setDx)
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
Async.Start (serpentone.inizializzaSerpente)

System.Threading.Thread.Sleep(-1);

