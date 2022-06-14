type Tree<'a> = Node of 'a * (Tree<'a> List)
type PosTree<'a> = PosNode of 'a*float*(PosTree<'a> List)
type Extend = (float*float) List

let moveTree (v:float) (PosNode(x, pos, cs): PosTree<'a>) =
    PosNode(x, pos+v, cs)

let moveExtend (v:float) (e:Extend) = 
    List.map (fun (a,b) -> (a+v, b+v)) e

let rec mergeExtend (ps:Extend) (qs:Extend) =
    match (ps, qs) with
    | ([], qs) -> qs
    | (ps, []) -> ps
    | ((p,_)::ps', (_,q)::qs') -> (p,q) :: mergeExtend ps' qs'

let mergeExtendList (es : Extend List) = 
    List.fold mergeExtend [] es

let rmax(p:float) (q:float) = 
    if p > q then p else q

let rec fit (e1:Extend) (e2:Extend) = 
    match (e1,e2) with
    | ((_, p)::ps, (q, _)::qs) -> rmax (fit ps qs) (p-q + 1.0)
    | _ -> 0.0

let fitlistl (es: Extend List):float List =
    let rec f(acc:Extend) (es': Extend List) = 
        match es' with
        | [] -> []
        | (e::es'') -> fit acc e |> fun x -> x::f(mergeExtend acc (moveExtend x e)) es''
    f [] es

let fitlistr (es: Extend List): float List = 
    let rec f(acc:Extend) (es': Extend List) =
        match es' with
        | [] -> []
        | (e::es'') -> -(fit e acc) |> fun x -> x::f (mergeExtend acc (moveExtend x e)) es''
    List.rev (f [] (List.rev es))

let mean (x:float, y:float) =
    (x+y)/2.0

let fitlist (es: Extend List): float List =
    List.map mean (List.zip (fitlistl es) (fitlistr es))

let rec blueprint (Node(x, xs)) = 
    List.unzip(List.map blueprint xs) |> fun(ts, es) ->
    let positions = fitlist es
    let ptrees = List.map (fun (v,t) -> moveTree v t) (List.zip positions ts)
    let pextends = List.map (fun (v,e) -> moveExtend v e) (List.zip positions es)
    let resultextent = (0.0, 0.0) :: mergeExtendList pextends
    let resulttree = PosNode(x, 0.0, ptrees)
    (resulttree, resultextent)

let designTree(t: Tree<'a>) : PosTree<'a> =
    fst (blueprint t)

let designExtends (t: Tree<'a>) :Extend =
    snd (blueprint t)

let extremes(t: Tree<'a>) : float*float =
    let (lefts, rights) = List.unzip ( designExtends t)
    List.min(lefts), List.max(rights)




