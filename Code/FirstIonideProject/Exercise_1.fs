// 1.1
let g n = n + 4
g 8

// 1.2
let h = function
    | (x,y) -> System.Math.Sqrt(x*x + y*y);;
h (2,2)

// 1.3

// 1.4
let rec f = function
    | 0 -> 0
    | n -> n + f(n-1);;
f 10

// 1.5
let rec fibonacci = function
    | 0 -> 0
    | 1 -> 1
    | n -> fibonacci(n-1) + fibonacci(n-2);;

fibonacci 4

// 1.6
let rec sum = function
    | (m,0) -> m 
    | (m,n) -> m+n+sum(m,n-1);;
sum (10,2)

// 1.7
// 

// 1.8
