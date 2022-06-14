// For more information see https://aka.ms/fsharp-console-apps
2 + 4 + 8;;

let price = 125;;

System.Math.PI;;

let circleArea r = System.Math.PI *r * r;;
circleArea 3.0;;

circleArea 3;;

let daysOfMonth = function
    | 2 -> 28
    | 4|6|9|11 -> 30
    | _ -> 31
;;

daysOfMonth 3;;

let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1);;

fact 3;;

let rec power = function
    | (x,0) -> 1.0
    | (x,n) -> x * power(x, n-1);;

power (2,3)


27 % 12

116 % 36 

let even n = n % 2 = 0

even 9
10 % 2

2.0 ** 3

printf "hello world"

type BinTree<'a, 'b> =
    | Leaf of 'a
    | Node of BinTree<'a, 'b> * 'b * BinTree<'a, 'b>;;

let t1 = Node(Node(Leaf 1,"cd",Leaf 2),"ab",Leaf 3);;


type Tree<'a> = Node of 'a * (Tree<'a> List);;
type Extend = (float * float) List;;

let moveTree

let t1 = Node("A" , []);;
let t2 = Node("B", [t1;t1;t1]);;

printf "%A" t2

let rec isMember x y = 
    match y with
    | [] -> false
    | y::ys when y=x -> true
    | y::ys -> isMember x ys ;;

isMember 2 [1;2;3]


[[1];[2;3]] @ [[4]];;

[[1];[2;3]] @ [4];;