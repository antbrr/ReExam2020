module Exam2020_2

//Part 1: Binary search trees

    type 'a bintree =
    | Leaf
    | Node of 'a bintree * 'a * 'a bintree

//1.1
    let rec insert (x: 'a) (t: 'a bintree) : 'a bintree when 'a : comparison =
        match t with
        |Leaf -> Node (Leaf, x, Leaf)
        |Node(tl, y, tr) when x <= y ->
            let left' = insert x tl
            Node(left',y, tr)
        |Node(tl, y, tr) ->
            let right' = insert x tr
            Node(tl, y, right')

//1.2
    let fromList (lst: 'a list) : 'a bintree =
        let rec aux acc lst' =
            match lst' with
            | [] -> acc
            | x :: xs -> aux (insert x acc) xs
        aux Leaf lst

//1.3

    let rec fold f acc t =
        match t with
        | Leaf -> acc
        | Node(tleft, x, tright) -> fold f (f (fold f acc tleft) x) tright
    
    let rec foldBack f acc t =
        match t with
        | Leaf -> acc
        | Node(tleft, x, tright) -> fold f (f (fold f acc tright) x) tleft

    let inOrder tree = foldBack (fun acc x -> x :: acc) [] tree

//1.4
    
    let rec map f tree = fold (fun acc x -> insert (f x) acc) Leaf tree

(* 2: Code Comprehension *)
    let rec foo =
        function
        | []                  -> []
        | [x]                 -> [x]
        | x::y::xs when x > y -> y :: (foo (x::xs))
        | x::xs               -> x :: foo xs

    let rec bar =
        function
        | []           -> true
        | [x]          -> true
        | x :: y :: xs -> x <= y && bar (y :: xs)
    let rec baz =
        function
        | []               -> []
        | lst when bar lst -> lst
        | lst              -> baz (foo lst)
     

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo,  bar, and baz?

    A: 
    foo: 'a list -> 'a list
    bar: 'a list -> bool
    baz: 'a list -> 'a list
    


    Q: What do functions ```bar```, and ```baz``` do 
       (not `foo`, we admit that it is a bit contrived)? 
       Focus on what they do rather than how they do it.

    A: 
    bar xs returns true if xs is in ascending order, false otherwise.
    
    baz xs returns xs' which is xs sorted in ascending order
   


    Q: What would be appropriate names for functions 
       foo, bar, and baz?
       
       These three functions implement bubble sort. This is not something you are
        strictly required to know.

    A: foo: foo could be called bubble (for bubble sort) or propagateLargest, or largestToEnd, or something similar.
       bar: isSortedInAscendingOrder
       baz: baz could be called sort or bubbleSort
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The functions foo and bar generate a warning during compilation: 
    'Warning: Incomplete pattern matches on this expression.' 
    
    Q: Why does this happen, and where? 

    A:Both foo and bar require that the argument lists are non-empty and the case where they
       are empty is not covered by the pattern match. The compiler warns that this case
       is not covered by either function.


    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: No it will not. The function baz checks if its input list is empty and it will never
       call either foo or bar with an empty list.

    *)

    let rec foo2 =
        function
        | []                  -> []
        | x::y::xs when x > y -> y :: (foo2 (x::xs))
        | x::xs               -> x :: foo2 xs
    
    let bar2 =
        function
        | []           -> true
        | [x]          -> true
        | x :: y :: xs -> x <= y && bar (y :: xs)

    let rec baz2 =
      function
      | lst when bar2 lst -> lst
      | lst               -> baz2 (foo2 lst)
    

(* Question 2.3 *) 

    (* Consider this alternative definition of *)

    let rec foo3 =
      function 
      | [x]                 -> [x]
      | x::xs               -> x :: foo3 xs
      | x::y::xs when x > y -> y :: (foo3 (x::xs))

    (*

    Q: Do the functions `foo` and `foo3` produce the same output for all possible inputs? 
       If yes, why; if no why not and provide a counter example.

    A: No, they do not. By swapping the second and third row in the pattern match the guard x > y is never run
       as matches are always checked in order. The function foo3 will always return the same list that you give it
       
   *)

(* Question 2.4 *)

    let bar3 ls = ls |> List.pairwise |> List.forall (fun (a, b) -> a <= b)

(* Question 2.5 *)

    (*

   Q: The function foo or baz is not tail recursive. Which one and why?
    
    A: foo is not tail recursive. This is because it has a cons operator chained on to the recursive call which has to be waited on to be evaluated
    making foo not tail recursive.
    
    Let me show you with the call 
    
    foo [3;2;1] ->
    
    2 :: (foo (3::[1])) ->
    
    2 :: 1 :: [3] ->
    
    2 :: 1 :: 3 ->
    
    [2;1;3] 
    
  *)

    (* ONLY implement the one that is NOT already tail recursive *)
    
    let fooTail xs =
        let rec aux c xs' =
            match xs' with
            | [] -> c []
            | [x] -> c [x]
            | x :: y :: xs when x > y -> aux (fun result -> c ( y :: result)) (x :: xs)
            | x :: xs -> aux (fun result -> c( x :: result)) xs
        aux id xs
    

(* 3: Big Integers *)

(* Question 3.1 *)

    type bigInt = B of int list

    let fromString (nums: string) =
        nums
        |> List.ofSeq
        |> List.map(fun elem -> int elem - int '0')
        |> B
    let toString (B lst) =
        lst
        |> List.map(fun elem -> string elem)
        |> String.concat ""
        
        

(* Question 3.2 *)

    let add (B lst1) (B lst2) =
        let rec aux lst1' lst2' carry =
            match lst1',lst2' with
            | [], [] ->
                if carry <> 0 then [carry] else []
            | [], y::ys ->
                ((y + carry) % 10) :: aux [] ys ((y+carry)/10)
            | x::xs, [] ->
                ((x + carry) % 10) :: aux xs [] ((x+carry)/10)
            | x::xs, y::ys ->
                (((x + y) + carry) % 10) :: aux xs ys (((x + y) + carry)/10)
        aux (List.rev lst1) (List.rev lst2) 0
        |> List.rev
        |> B
                

(* Question 3.3 *)

    let multSingle (B lst) (y: int) =
        let rec aux lst' carry =
            match lst' with
            | [] -> if carry <> 0 then [carry] else []
            | x :: xs -> (((y * x) + carry) % 10) :: aux xs (((y* x)+carry)/10)
        aux (List.rev lst) 0
        |> List.rev
        |> B
            

(* Question 3.4 *)

    let mult (lst1: bigInt) (B lst2) =
        let rec aux acc lst2' tail =
            match lst2' with
            | [] -> acc
            | x :: xs ->
                let (B next) = multSingle lst1 x 
                aux (add (B(next @ tail)) acc) xs (0 :: tail)
        aux B[0] (List.rev lst2) []
        

(* Question 3.5 *)

    let fact _ = failwith "not implemented"

(* 4: Lazy lists *)

    type 'a llist =
    | Cons of (unit -> ('a * 'a llist))

    let rec llzero = Cons (fun () -> (0, llzero))

(* Question 4.1 *)

    let step (Cons unitFunc) = unitFunc ()
    let cons (x: 'a) (l1: 'a llist) =
        Cons (fun () -> (x,l1))
     
        
        

(* Question 4.2 *)
    let init f =
        let rec aux (i: int) =
            Cons(fun () -> f i, aux (i + 1))
        aux 0

(* Question 4.3 *)
    let rec llmap f llst =
        Cons(fun () ->
            let (head,tail) = step llst
            f head, llmap f tail)
(* Question 4.4 *)
    let rec filter f llst =
         let (head,tail) = step llst
         if f head then
            Cons(fun () -> head, filter f tail)
         else filter f tail

(* Question 4.5 *)

    let takeFirst (n: int) ll =
        let rec aux acc counter ll' =
            match counter with
            | 0 -> List.rev(acc),ll'
            | x ->
                let(head,tail) = step ll'
                aux (head :: acc) (x-1) tail
        aux [] n ll
            
        

(* Question 4.6 *)

    let rec unfold generator st =
        Cons(fun () -> 
            let (elem, state) = generator st
            elem,(unfold generator state)
            )

    (* Consider the following two implementations of Fibonacci sequences fibll1 and fibll2: *)

    let fib x =
        let rec aux acc1 acc2 =
            function
            | 0 -> acc1
            | x -> aux acc2 (acc1 + acc2) (x - 1)

        aux 0 1 x




    let fibll1 = init fib
    let fibll2 = unfold (fun (acc1, acc2) -> (acc1, (acc2, acc1 + acc2))) (0, 1)
   
    (* 

    Q: Both fibll1 and fibll2 correctly calculate a lazy list of Fibonacci numbers. 
       Which of these two lazy lists is the most efficient implementation and why?
    
    A: fibll2 is fastest because it does not have to initialize the function every time. fibll1 is linear, whereas fibll2 is constant.
    
    *)
