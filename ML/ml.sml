(*
Justin Harper
WSUID 10696738
*)


(*
in_list - 10%

This function should return true if the first argument is a member of the second argument and have type (''a * ''a list) -> bool. Explain in a comment why the type is (''a * ''a list) -> bool and not ('a * 'a list) -> bool
- in_list (1,[]);
false
- in_list (1,[1,2,3]);
true
- in_list ([1],[[1]]); 
true
- in_list ([1],[[3],[5]]); 
false 
- in_list ("c",["b","c","z"]);
true
*)

fun in_list(x,[]) = false | in_list(x,(y::z)) = 
    if x=y then true
    else in_list(x,z);

(*intersection - 10%

This function should return the intersection of two lists, and have type ''a list -> ''a list -> ''a list. Hint: Maybe you can make use of in_list?
- intersection [1] [1]; 
[1] 
- intersection [1,2,3] [1,2]; 
[1,2]
- intersection [[2,3],[1,2],[2,3]] [[1],[2,3]]; 
[[2,3]]         
Each value should appear in the output list only once, but the order does not matter.
*)

fun intersection(list1, list2) = 
    if null list1
        then []
    else if null list2
        then []
    else if in_list(hd(list1), list2)
        then hd(list1)::intersection(tl(list1), tl(list2))
    else intersection(t1(list1), tl(list2));

(*union - 10%

This function should return the union of two lists. Each value should appear in the output list only once but the order does not matter. In the intersection function above the two lists were supplied as separate arguments. Make union a function of a single argument with that argument being a tuple: it should have type (''a list * ''a list) -> ''a list.
- union ([1], [1]); 
[1]
- union ([1,2,3], [1,2]); 
[1,2,3]
- union ([[2,3],[1,2]], [[1],[2,3]]); 
[[1],[2,3],[1,2]]
*) 

fun union list1 [] = l1 | union list1(x::list2) =
    if in_list(x, (union list1 list2))
    then union list1 list2
    else x::union list1 list2;

(*
filter and reverse - 10%

filter takes as its first argument a one-argument function, called a predicate, which returns true or false, and as its second argument a list. It returns a list of all elements in the second argument that satisfy that predicate. The elements must appear in the result in the same order that they appear in the original list.
- filter (fn (x) => (x = 1)) [1,2,3];
[1] 
- filter (fn (x) => (x <= 3))[1,2,3,4]; 
[1,2,3]
For this problem (only) your implementation is required to be tail recursive, so you will need to define an auxiliary function that takes a parameter in which the result is accumulated. The function filter will simply call the auxiliary function with [] as the initial result. It is the auxiliary function that will be tail recursive. It turns out that in using the accumulating parameter technique, the result is produced in reverse order. So you also need to define the function reverse that reverses a list. reverse is also implemented as a tail-recursive function. In class when talking about Scheme, we defined revAppend and then reverse. Translate those definitions to ML.
We will have talked about tail recursion and accumulating parameters in class. I will show you how to write auxiliary functions as nested functions.
*)

fun filter f [] = [] | filter f(x::list) =
    if(f x) then x::(filter f list)
    else filter f list;

fun reverse [] = [] | reverse (x::xs) =
    (reverse a) @ [x];

(*
groupNl and groupNr - 10%

These functions take two arguments. The first is an integer and the second is a list. The idea is to produce a result in which the elements of the original list have been collected into sublists each containing N elements (where N is the integer argument). Thus the type of each of these is int -> 'a list -> 'a list list. The difference between the two functions is how they handle the left-over elements of the list when the integer doesn't divide the length of the list evenly. groupNl treats the initial elements of the list as the extras, thus the result starts with a list of between 1 and N elements and all the remaining elements of the result list contain N elements. groupNr does the opposite: the final group contains between 1 and N elements and all the rest contain N elements. Note: these functions are not required to be tail-recursive.
Examples:

- groupNl 2 [1, 2, 3, 4, 5]
[[1], [2, 3], [4, 5]]
- groupNr 2 [1, 2, 3, 4, 5]
[[1, 2], [3, 4], [5]]
*)
fun groupNr n l = 
    if(length l) <  n
        then [l]
    else
        let val (L,R) = (splitAt l n)
        in L::(groupNr n R) end'


fun groupNl n l = map reverse (reverse (groupNr n (reverse l)));

fun splitAt l n = (List.take(l,n), List.drop(l,n));

(*    
mergesort - 15%

This function takes two arguments. The first argument is a function called a comparator and takes as its argument a pair (two-tuple) and returns a boolean. The second argument is a list. The type of the sorting function is ('a * 'a -> bool) -> 'a list -> 'a list. The function mergesort returns a list consisting of the members of its second argument, ordered so that the comparator returns true on adjacent members.
- mergesort (fn (x,y) => (x <= y)) [1];
[1] 
- mergesort (fn (x,y) => (x <= y)) [3,2,1,2]; 
[1,2,2,3] 
- mergesort (fn (x,y) => (x >= y)) [3,2,1,2]; 
[3,2,2,1]
Recall the general structure of mergesort: split the list in 2 equal pieces. Recursively sort the pieces, then merge the two sorted sublists into a sorted result (as in the Scheme assignment).
*)

fun mergesort p [] = [] | mergesort p [x] = [x] | mergesort p lis =
    let
    fun merge p ([], a) = a | merge p (xs, []) = xs | merge p (x::xs,y::ys) = 
        if (p (x,y)) then (x::(merge p(xs, y::ys)))
        else(y::(merge p (x::xs,ys)))
        val half = (List.length (lis)) div 2
        in
            (merge p ((mergesort p (List.take(lis,half))), (mergesort p (list.drop (lis, half)))))
        end
        ;

(*
Practice with datatypes - 15%

Define an ML datatype datatype either = ImAString of string | ImAnInt of int.
Define an ML datatype named eitherTree for binary trees containing values of type either where data may be held at both interior and leaf nodes of the tree.
Define an ML function eitherSearch having type eitherTree -> int -> bool that returns true if the int is in the tree and false otherwise. The trick to getting this to type check is to realize that ImAnInt of int values and int values do not have the same type. But you can transform either into the other.
Define an ML function of no arguments, eitherTest that:

constructs an eitherTree with at least 5 int-containing leaves, at least 5 string-containing leaves, and at least 4 levels;
searches the tree using your eitherSearch function for an int that is present in the tree;
and, searches the tree using your eitherSearch function for a value that is notpresent in the tree.
*)

dataype either = ImAString of string | ImAnInt of int;

dataype eitherTree = NODE of eitherTree * eitherTree | LEAF of either;

fun eitherSearch (LEAF(ImAnInt a)) b = (a=b)
    | eitherSearch(LEAF(ImAString a)) b = false
    | eitherSearch(NODE (t1,t2)) y = (eitherSearch t1 b)
    orelse (eitherSearch t2 b);

fun eitherTest () =
    let
        val V1 = LEAF(ImAnInt 1)
        val V2 = LEAF(ImAnInt 2)
        val V3 = LEAF(ImAnInt 3)
        val V4 = LEAF(ImAnInt 4)
        val V5 = LEAF(ImAnInt 5)

        val V6 = LEAF(ImAnInt "a")
        val V7 = LEAF(ImAnInt "b")
        val V8 = LEAF(ImAnInt "c")
        val V9 = LEAF(ImAnInt "d")
        val V10 = LEAF(ImAnInt "e")

        val N1 = NODE (L1, L2)
        val N2 = NODE (N1, L3)
        val N3 = NODE (N2, L4)
        val N4 = NODE (N3, L5)

        in 
            (eitherSearch N4 1)
    end;


(*
treeToString - 15%

A polymorphic tree type, with data only at the leaves, in SML might be represented using
   datatype 'a Tree = LEAF of 'a | NODE of ('a Tree) list
Write a function treeToString: ('a -> string) -> 'a Tree -> string that returns a parenthesized string representing an arbitrary Tree. treeToString is invoked as treeToString f t where f is a function that converts data of type 'a to a string and t is an 'a Tree. The parenthesization rules implemented by treeToString are as follows:
For a LEAF node, the returned value is just (f the-data-in-the-leaf).
For a LIST node, concatenate the strings produced by treeToString on the elements of the list and surround the resulting string with parentheses.
For this function, you may use built-in functions map and String.concat in addition to the generally allowable functions listed above.
I suggest that you start by solving a simpler non-polymorphic problem using

   datatype Tree = LEAF of string | NODE of Tree list
Since the leaves in this simpler problem are already strings you don't need the function argument but you can see the overall structure of the solution. Then make the datatype polymorphic and add the function parameter.
Hint: The whole function is neatly expressed as two lines of code.

Here is some test data:

val L1a = LEAF "a"
val L1b = LEAF "b"
val L1c = LEAF "c"
val L2a = NODE [L1a, L1b, L1c]
val L2b = NODE [L1b, L1c, L1a]
val L3 = NODE [L2a, L2b, L1a, L1b]
val L4 = NODE [L1c, L1b, L3]
val L5 = NODE [L4]
val iL1a = LEAF 1
val iL1b = LEAF 2
val iL1c = LEAF 3
val iL2a = NODE [iL1a, iL1b, iL1c]
val iL2b = NODE [iL1b, iL1c, iL1a]
val iL3 = NODE [iL2a, iL2b, iL1a, iL1b]
val iL4 = NODE [iL1c, iL1b, iL3]
val iL5 = NODE [iL4]
treeToString String.toString L5 should produce "((cb((abc)(bca)ab)))" and treeToString Int.toString iL5 should produce "((32((123)(231)12)))".
Note that interactive SML systems typically do not print all of the contents of deeply nested data structures. So after evaluating the declaration for il5 the response may be something like

val iL5 = NODE [NODE [LEAF #,LEAF #,NODE #]] : int Tree
depending on what SML system you are using (in this case SMLofNJ).
Additional information about string manipulation: the ^ infix operator concatenates two strings, thus:

- "abc" ^ "def";
"abcdef"
and String.concat concatenates all of the strings in a list of strings (look at it's type!). Thus:
- String.concat ["abc", "def", "ghi"];
"abcdefghi"
*)

fun treeToString a (LEAF(x)) = (a x) | treeToString f(NODE(x)) = "(" ^ (String.concat (map (treeToString a) x)) ^ ")";

(*
Subsets - 5%

Function subsets is given a list, l, and returns a list of lists, each of the sublists being one of the 2^length(l) subsets of l. The subsets may be in any order. You may assume that all of the elements of the original list are distinct.
- subsets [1, 2, 3]
[[], [1], [2], [3], [1, 2], [1, 3], [2, 3], [1, 2, 3]]
*)

(*YA im not going to do that cs460 needs to get done tonight too*)










(*
Hints about using files containing ML code

In order to load files into the ML interactive system you have to use the function named use.
The function use has the following syntax assuming that your code is in a file in the current directory named ml.sml: You would see something like this in the output:

- use "ml.sml"; 
[opening file "ml.sml"]
...list of functions and types defined in your file
[closing file "ml.sml"] 
> val it = () : unit 
-
The effect of use is as if you had typed the content of the file into the system, so each val and fun declaration results in a line of output giving its type.
If the file is not located in the current working directory you should specify the full or relative path-name for the file. For example in Windows for loading a file present in the users directory in the C drive you would type the following at the prompt. Note the need to use double backslashes to represent a single backslash.

- use "c:\\users\\example.sml";
Alternatively you can change your current working directory to that having your files before invoking the ML interactive system. You can also load multiple files into the interactive system by using the following syntax
- use "file1"; use "file2";...; use "filen";
How to quit the ML environment

Control-Z followed by a newline in Windows or control-D in Linux will quit the interactive session.
*)