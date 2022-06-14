type Tree<'a> = Node of 'a * (Tree<'a> List)
type PosTree<'a> = PosNode of 'a*float*(PosTree<'a> List)
type Extent = (float*float) List

let moveTree (x':float) (PosNode(label, x, subtree):PosTree<'a>) =
    PosNode(label, x'+x, subtree)

let moveExtent (x':float) (e:Extent) =
    List.map (fun (a,b) -> (a+x',b+x')) e

let rec merge (ps:Extent) (qs:Extent) =
    match (ps, qs) with
    | ([], qs) -> qs
    | (ps, []) -> ps
    | ((p,_)::ps', (_,q)::qs') -> (p,q)::(merge ps' qs')

let mergelist (es: Extent List) = 
    List.fold merge [] es

let rmax (p:float) (q:float) = 
    if p>q then p else q

let rec fit (ps:Extent) (qs:Extent) = 
    match (ps, qs) with
    | ((_,p)::ps', (q,_)::qs') -> rmax(fit ps' qs') (p-q+1.0)
    | _ -> 0.0

let fitlistl (es:Extent List) : float List=
    let rec fitlistl' (acc: Extent) (es': Extent List)=
        match es' with
        | [] -> []
        | (e::es'') -> (fit acc e) |> fun x -> x::fitlistl' (merge acc (moveExtent x e)) es''
    fitlistl' [] es

let fitlistr (es:Extent List) : float List=
    let rec fitlistr' (acc: Extent) (es': Extent List)=
        match es' with
        | [] -> []
        | (e::es'') -> -(fit e acc) |> fun x -> x::fitlistr' (merge (moveExtent x e) acc) es''
    (List.rev (fitlistr' [] (List.rev es)))

let mean (x:float, y:float) =
    (x+y)/2.0

let rec fitlist (es:Extent List) = 
    List.map mean (List.zip (fitlistl es) (fitlistr es))

let design (tree: Tree<'a>) =
    let rec design' (Node(label, subtrees)) =
        let (trees, extents) = List.unzip (List.map design' subtrees)
        let positions = fitlist extents
        let ptrees = List.map (fun (v,t) -> moveTree v t) (List.zip (positions) (trees))
        let pextents = List.map (fun (v,e) -> moveExtent v e) (List.zip (positions) (extents))
        let resultextent = (0.0, 0.0) :: mergelist pextents
        let resulttree = PosNode(label, 0.0, ptrees)
        (resulttree, resultextent)
    fst (design' tree)

// example
let x = Node("A", [Node("B", []) ; Node("C", []) ; Node("D", []) ])
design x

let y = Node("A" ,[])
design y

// fitlistl [[0.0,0.0];[0.0,0.0];[0.0,0.0]]
// fitlistr [[0.0,0.0];[0.0,0.0];[0.0,0.0]]


// let (trees, extents) = ([], [])
// let positions = fitlist extents
// let ptrees = List.map (fun (v,t) -> moveTree v t) (List.zip (positions) (trees))
// let pextents = List.map (fun (v,e) -> moveExtent v e) (List.zip (positions) (extents))
// let resultextent = (0.0, 0.0) :: mergelist pextents
// let resulttree = PosNode("B", 0.0, ptrees)

let rec depthFirst (Node(x,ts)) = x :: (List.collect depthFirst ts)
depthFirst x
depthFirst y
