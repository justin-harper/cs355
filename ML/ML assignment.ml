fun in_list (x,[]) = false
    | in_list (x,(i::p)) = 
        if x=i 
            then true 
        else in_list(x,p);

fun intersection [] [] = []
	| intersection x [] = []
	| intersection [] x = []
 	| intersection(x::xs) (z) = if in_list (x,xs) then intersection xs z
 				    else if in_list (x,z) then x::intersection
 				    xs z else intersection xs z;

fun union ([], []) = []
  | union (l, []) = l
  | union ([], l) = l
  | union (a::b, c) = if in_list (a, c) then union (b,c)
                      else union(b,a::c);

fun filter f [] = []
    | filter f (x::list) = 
        if (f x) then x::(filter f list)
        else filter f list;

fun reverse [] = []
	| reverse (x::xs) = (reverse xs) @ [x];

(*helper function*)
fun splitAt l n = (List.take(l,n), List.drop(l,n));

fun groupNr n l = 
	if (length l) < n 
		then [l]
	else
		let val (L,R) = (splitAt l n)
		in L::(groupNr n R) end;

(*why not use the function we already defined?!*)
fun groupNl n l = map reverse (reverse (groupNr n (reverse l)));


(*
1. split the list in 2 equal pieces. 
2. Recursively sort the pieces, 
3. then merge the two sorted sublists into a sorted result*)
fun mergesort p [] = []
	|	mergesort p [z] = [z]
	|	mergesort p lst = let
			fun merge p ([], ys) = ys 
				|	merge p (xs, []) = xs
				|	merge p (x::xs,y::ys) = 
					if (p (x,y)) then(x::(merge p (xs, y::ys)))
					else(y::(merge p (x::xs,ys)))
				val half = (List.length (lst)) div 2 
			in
				(merge p ((mergesort p (List.take(lst,half))), (mergesort p (List.drop (lst, half))))) 
			end;

datatype either = ImAString of string | ImAnInt of int;


datatype eitherTree = NODE of eitherTree * eitherTree
		|	LEAF of either;


fun eitherSearch (LEAF(ImAnInt x)) y = (x=y)
  | eitherSearch(LEAF(ImAString x)) y = false
  | eitherSearch (NODE (t1,t2)) y = (eitherSearch t1 y)
  orelse (eitherSearch t2 y);



fun eitherTest () =
	let
		val L1 = LEAF(ImAnInt 1)
		val L2 = LEAF(ImAnInt 2)
		val L3 = LEAF(ImAnInt 3)
		val L4 = LEAF(ImAnInt 4)
		val L5 = LEAF(ImAnInt 5)
		val L6 = LEAF(ImAString "a")
		val L7 = LEAF(ImAString "b")
		val L8 = LEAF(ImAString "c")
		val L9 = LEAF(ImAString "d")
		val L10 = LEAF(ImAString "e")
		val N1 = NODE (L1,L2)
		val N2 = NODE (N1, L3)
		val N3 = NODE (N2, L4)
		val N4 = NODE (N3, L5)

   in
		not (eitherSearch N4 1)
   end;
fun treeToString f (LEAF(x)) = (f x)
	| treeToString f (NODE(x)) = "(" ^ (String.concat (map (treeToString f) x)) ^ ")";


(*ran out of time to do subsets.*)  