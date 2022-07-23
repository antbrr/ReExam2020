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
    bar takes a list and returns true if the items in the list is in ascending order. Otherwise false.
    baz takes a list, checks if it is in ascending order, and if not it calls foo to do the ordering
   


    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: foo: sortList
       bar: isSorted
       baz: sortList2
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The functions foo and bar generate a warning during compilation: 
    'Warning: Incomplete pattern matches on this expression.' 
    
    Q: Why does this happen, and where? 

    A: This happens because they do not consider the case of the empty list. It happens at the function keyword at line 58 and 64.


    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: No, not unless we call the functions with an empty list

    *)

    let foo2 =
        function
        | []                  -> []
        | [x]                 -> [x]
        | x::y::xs when x > y -> y :: (foo (x::xs))
        | x::xs               -> x :: foo xs
    let bar2 =
        function
        | []           -> true
        | [x]          -> true
        | x :: y :: xs -> x <= y && bar (y :: xs)

    (* Uncomment code to run after you have written foo2 and bar2 *)
    let rec baz2 =
      function
      | lst when bar2 lst -> lst
      | lst               -> baz2 (foo2 lst)
    

(* Question 2.3 *) 

    (* Consider this alternative definition of *)

    let foo2copy =
        function
        | []                  -> []
        | [x]                 -> [x]
        | x::y::xs when x > y -> y :: (foo (x::xs))
        | x::xs               -> x :: foo xs
    let rec foo3 =
      function 
      | [x]                 -> [x]
      | x::xs               -> x :: foo3 xs
      | x::y::xs when x > y -> y :: (foo3 (x::xs))

    (*

    Q: Do the functions `foo` and `foo3` produce the same output for all possible inputs? 
       If yes, why; if no why not and provide a counter example.

    A: No they wont since in foo3 it always hits the x :: xs case. So lets consider this call
    
    foo2 [4;8;7]
    4 :: foo3 [8;7]
    4 :: 7 :: foo3(8::[])
    
    4 :: 7 :: 8
    
    [4;7;8]
    
    But in the other case foo3 [4;8;7]
    
   4 :: foo3 [8;7]
    4 :: 8 foo3[7]
    
    4 :: 8 :: 7
    
    [4;8;7]
    *)

(* Question 2.4 *)

    let bar3 ls = ls |> List.pairwise |> List.forall (fun (a, b) -> a <= b)

(* Question 2.5 *)

    (*

    Q: The function foo or baz is not tail recursive. Which one and why?
    
    A: foo is not tail recursive let me demonstrate
        foo [3;2;1]
        
        2 :: (foo (3 :: [1]))
        
        2 :: (foo [3;1]
        
        2 :: 1 :: 3 wtf
        
        
        

    *) (*let rec foo =
        function
        | []                  -> []
        | [x]                 -> [x]
        | x::y::xs when x > y -> y :: (foo (x::xs))
        | x::xs               -> x :: foo xs)*)

    (* ONLY implement the one that is NOT already tail recursive *)

    let fooTail _ = failwith "not implemented"
    let bazTail _ = failwith "not implemented"

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
            Cons(fun () -> head, tail)
         else filter f tail

(* Question 4.5 *)

    let takeFirst (x: int) llst =
        let rec aux acc counter =
            
        

(* Question 4.6 *)

    let unfold _ = failwith "not implemented"

    (* Consider the following two implementations of Fibonacci sequences fibll1 and fibll2: *)

    let fib x =
        let rec aux acc1 acc2 =
            function
            | 0 -> acc1
            | x -> aux acc2 (acc1 + acc2) (x - 1)

        aux 0 1 x

    (* Uncomment after you have implemented init and unfold *)

(*
    let fibll1 = init fib
    let fibll2 = unfold (fun (acc1, acc2) -> (acc1, (acc2, acc1 + acc2))) (0, 1)
  *)  
    (* 

    Q: Both fibll1 and fibll2 correctly calculate a lazy list of Fibonacci numbers. 
       Which of these two lazy lists is the most efficient implementation and why?
    
    A: <Your answer goes here>
    
    *)
