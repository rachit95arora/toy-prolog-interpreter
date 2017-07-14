type symbol = S of string * int;;
type variable = T of string;;
type term = V of variable | Node of symbol * (term list);;
type substitution = Q of variable * term;;
type clause = term * term list
exception TooDeep
let rec printTable l = match l with x::xs -> (printClause x);Printf.printf "NEXT CLAUSE \n\n\n\n"; (printTable xs);
| [] -> Printf.printf "EMPTY"
and printClause c = match c with (ter, terl) -> Printf.printf "Clause Start XXXXXXXXXXX\n"; (printTerm ter); Printf.printf "\nXXXXXXXXX MAPPED TO \n"; (printTerms terl);
and printTerms tl = match tl with [] -> Printf.printf ""; | x::xs -> (printTerm x);(Printf.printf "next term\n");(printTerms xs);
and printTerm t = match t with V(T(str)) ->  Printf.printf "Variable %s\n" str;
|Node(S(str,integ),children) -> Printf.printf "Symbol %s,%d\n Children\n" str integ; (printTerms children);Printf.printf "ChildrenOver\n";
;;
exception InvalidSubstition
exception NOT_UNIFIABLE
let rec checkAll l acc = match l with ((S(str,integ))::xs) ->
if((integ>=0) && (check str acc)) then (checkAll xs (str::acc)) else false | [] -> true
and
check stri acce = match acce with (x::xs) -> if(stri = x) then false else (check stri xs) | [] -> true;; 

let check_sig l = checkAll l [];;  
let rec checkLength l intg = match l with x::xs -> (checkLength xs (intg-1)) | [] -> (intg = 0);;
let rec snoc l a = match l with [] -> [a] | x::xs -> x::(snoc xs a);;
let rec snoclist l nl = match nl with x::xs -> snoclist (snoc l x) xs | [] -> l;;
let rec checkFromSignature (S(str,integ)) signature = match signature with ((S(str1,integ1))::l) ->
(
	if((str=str1) && (integ = integ1)) then true else (checkFromSignature (S(str,integ)) l)
)
| [] -> false;;

let rec wfterm term signature = match term with Node((S(strg,intg)),l) ->
( if((checkLength l intg) && (checkFromSignature (S(strg,intg)) signature)) then (wf_all_terms l signature) else false)
| V(T(str)) -> (not(str="")) 
and wf_all_terms l signature = match l with x::xs -> ((wfterm x signature) && (wf_all_terms xs signature)) | [] -> true;;

let fmax a x = if(a>x) then a else x;;

(* let pairUnion a x = match x with (c,d) ->(if(c) then x else a);;
let checklist llist elemt = (List.fold_left (pairUnion) (false,V(T(""))) (List.map (fun x -> ((x=elemt),elemt)) llist));;
let unionFvar a x = match x with (false,y) -> (y::a) | _-> a;;
let rec fvar a x = (List.fold_left unionFvar a (List.map (checklist a) x));;
let rec variable_set term = match term with V(x) -> [term]
| Node((S(strg,intg)),l) -> (List.fold_left fvar [] (List.map variable_set l));;
*)
let rec notAlreadyPresent a x = match a with v::vs -> ((not(v=x)) && (notAlreadyPresent vs x)) | [] -> true;; 
let takeUnion a x = if(notAlreadyPresent a x) then (x::a) else a;;
let rec unionUnions a x = (List.fold_left (takeUnion) a x);;
let rec variable_set term = match term with V(vd)-> [vd] | Node((S(strg,intg)),l) -> (List.fold_left (unionUnions) [] (List.map variable_set l));;
let rec height term = match term with Node((S(strg,intg)),l) -> (1+ (List.fold_left fmax 0 (List.map height l))) | V(x) -> 1;; 
let rec size term =  match term with Node((S(strg,intg)),l) ->(1 + (List.fold_left (+) 0 (List.map size l))) | V(x) -> 1;;

let rec checkSubstition sub = match sub with Q(T(str),termm) ->
(
	match termm with V(T(str1)) -> not(str = str1)
	| Node((S(strg,intg)),l) -> (checkAllSubstition (T(str)) l)
)
and checkAllSubstition (T(str)) l = match l with x::xs -> ((checkSubstition (Q((T(str)), x))) && (checkAllSubstition (T(str)) xs)) | [] -> true
;; 

let rec orList f l = match l with [] -> (false,[])
| x::xs -> (printClause x;match (f x) with (true,env) -> (true,env) | (false,env) -> (orList f xs));;

let rec andList f sl l = match l with [] -> (sl)
| x::xs -> (match (f sl x) with (true, sl1)-> (andList f (true,sl1) l) | (false,sl1) -> (false,[])
);;

let rec lookup key (bol,sl) = match key with V(T(str)) ->
(
	match sl with x::xs -> (match x with Q(T(str1),ter) ->if(str = str1) then ( lookup ter (bol,sl) ) else  (lookup key (bol,xs) ))
	|[]-> key
)
| _-> key;;
let lookuprev e k = lookup k e;;

let rec appendScope term = match term with V(T(str)) -> (V(T(str^"~"))) | Node((S(strg,intg)),[]) -> term | Node((S(strg,intg)),l) -> Node((S(strg,intg)),(List.map appendScope l));;
let appendToClause clausee= match clausee with (a,l) -> ((appendScope a), (List.map appendScope l));;
let rec copyTable table = match table with [] -> []
| x::xs -> (appendToClause x)::(copyTable xs);;

let addSubst t1 t2 sl = match t1 with V(var) -> (Q(var,t2))::sl | _-> (match t2 with V(var2) -> (Q(var2,t1)::sl));;

let rec substituteVar a x = match x with Q((T(scpe)),ter) -> if(checkSubstition x) then
(
	match a with (V(T(stry))) -> (if(stry = scpe) then ter else a)
	| Node((S(strg,intg)),l) -> Node((S(strg,intg)), (List.map (substMirror x) l))
) else raise InvalidSubstition
and substMirror x a = (substituteVar a x);;
let subst t sl = (List.fold_left substituteVar t sl);;

let rec mgu t1 t2 = 
(
	match t1 with
	Node(S(str,0),[]) -> (match t2 with Node(S(str1,x),l) -> if((str = str1) && (x=0) && (checkLength l 0)) then (true,[]) else (false,[]) | V(blah) -> (mgu t2 t1))

	| V(T(varname1)) -> (match t2 with
		V(T(varname2)) -> if(varname2 = varname1) then (true,[]) else (true,[(Q((T(varname1)), t2))])
		| _-> if(checkSubstition (Q(T(varname1), t2))) then (true,[(Q(T(varname1), t2))]) else (false,[])
		)

	| Node(S(str1,x1),l1) -> (match t2 with V(blah) -> (mgu t2 t1) | Node(S(str2,x2),l2) -> if((str1=str2) && (x1=x2)) then (mgu_list l1 l2 (true,[])) else (false,[]))
)
and mgu_list l1 l2 sl0 = match sl0 with (true,sl) -> (match l1 with x1::xs1 -> (match l2 with x2::xs2 ->
		let s0 = (mgu (subst x1 sl) (subst x2 sl)) in 
		(	match s0 with (true,s) ->	(mgu_list xs1 xs2 (true,(List.append sl s)))
			| (false,s) -> (false,[]) 
		)
		| [] -> (false,[])
	)
	| [] -> (true,sl)
)
| (false,sl) -> (false,[]);;

let unify sl t1 t2 = mgu_list [t1] [t2] sl;;

let decideTocontinue env = 
	let rea = read_line () in
		match rea with ";" -> (false,[])
		| "." -> (env)
		| _-> Printf.printf "Invalid Symbol\n"; env
;;
let getTargets term = List.rev (List.map (fun x -> V(x)) (variable_set term));;

let rec printNode value = match value with Node(S(str,integ),l) -> (if (integ>0) then (Printf.printf "%s(" str; printAllNodes l;Printf.printf ")";) else Printf.printf "%s" str;)
							| V(T(str)) -> Printf.printf "%s" str;
and printAllNodes vlist = match vlist with [] -> Printf.printf "";
								| (v::vs) -> (printNode v); Printf.printf ","; (printAllNodes vs);
;;

let printAssignment key value = match key with V(T(str)) ->
(
	match value with V(T(str1)) -> if(not(str=str1)) then (Printf.printf "%s = %s ,\n" str str1);
		| _-> Printf.printf "%s = " str; (printNode value);Printf.printf " ,\n";
)
|_-> Printf.printf "ERROR IN PRINT ASSIGNMENT\n";
;;

let printAssignment1 key value = match key with V(T(str)) ->
(
	match value with V(T(str1)) -> if(not(str=str1)) then (Printf.printf "%s = %s " str str1);
		| _-> Printf.printf "%s = " str; (printNode value);
)
|_-> Printf.printf "ERROR IN PRINT ASSIGNMENT\n";
;;

let rec printTargets env targets = 
	match targets with t::[] -> let value = (lookup t env) in ((printAssignment1 t value); )
	|t::ts -> let value = (lookup t env) in ((printAssignment t value); (printTargets env ts);)	
;;
let rec printTargets0 env targets = 
	match targets with t::ts -> (printTargets env targets);
	| [] -> Printf.printf "true";
;;
let rec isPureSymbol tree = match tree with (Node(S(str,integ),l)) -> List.fold_left (&&) true (List.map (isPureSymbol) l) | V(v) -> false;;
let rec checkNot term = match term with (Node(S("not",1),[li]))-> true |_-> false;;
let rec checkAllNot terms = List.fold_left (&&) true (List.map checkNot terms);;
let rec solve depth table env goals targets enableNot = 
	match goals with g::gs ->
	(
		if(depth=0) then (false,[])
		else(
			let newtable = (copyTable table) in
			(
			if(checkNot g) then
			(
				if(checkAllNot gs) then
				(
					if (List.fold_left (&&) true (List.map (isPureSymbol) (List.map (lookuprev env) (List.map (fun x -> V(x)) (variable_set g)))))
					then 
						match g with (Node(S("not",1),[li])) -> match (walkClauseList (solveGoal depth (newtable) env targets [] li true) newtable) with 
							(true,l) -> (false,[])
							| (false,l) -> (solve depth table env (gs) targets enableNot)
					else (false,[])
				)
				else (solve depth table env (snoc gs g) targets enableNot)
			)
			else walkClauseList (solveGoal depth (newtable) env targets gs g enableNot) newtable)
			)
	)
	| [] -> if(enableNot) then env else ((printTargets0 env targets);(decideTocontinue env))
and walkClauseList f clist = 
match clist with [] -> (false,[])
| c::cs -> (match (f c) with (true,env1) -> (true,env1)
			    |(false,env1) -> (walkClauseList f cs)  )	
and solveGoal depth table env targets goalL goal enableNot (head,body) = 
match (unify env goal head) with
	(false,env1) -> (false,[])
	| (true, env1) -> (solve (depth-1) table (true,env1) (List.append body goalL) targets enableNot)
;;

























(* let rec mgu (true,sl1) t1 t2 = 
(
	match (subst t1 sl1) with
	Node(S(str,0),[]) -> (match (subst t2 sl1) with Node(S(str1,x),l) -> if((str = str1) && (x=0) && (checkLength l 0)) then (true,[]) else (false,[]) | V(blah) -> (mgu ((true,sl1)) (V(blah)) (Node(S(str,0),[])) ) )

	| V(T(varname1)) -> (match (subst t2 sl1) with
		V(T(varname2)) -> if(varname2 = varname1) then (true,[]) else (true,[(Q((T(varname1)), t2))])
		| _-> if(checkSubstition (Q(T(varname1), t2))) then (true,[(Q(T(varname1), t2))]) else (false,[])
		)

	| Node(S(str1,x1),l1) -> (match (subst t2 sl1) with V(blah) -> (mgu ((true,sl1)) (V(blah)) (Node(S(str1,x1),l1))) | Node(S(str2,x2),l2) -> if((str1=str2) && (x1=x2)) then (mgu_list l1 l2 (true,sl1)) else (false,[]))
)
and mgu_list l1 l2 sl = match l1 with x1::xs1 -> (match l2 with x2::xs2 ->(match sl with (true,sl0)->(
	
	let s = (mgu sl x1 x2) in (match s with (true,s0) ->(mgu_list xs1 xs2 (true,(snoclist sl0 s0)))
		| (false,l) -> sl )
	
	)| (false,l) -> sl )
	
	|[] -> (false,[])
)
| [] -> sl;; *)






(*TEST CASES*)
(* let signature = [(S("f",3)); (S("g",2)); (S("h",2)); (S("t",1)); (S("A",0)); (S("B",0))];;
let a = Node((S("f",3)), [ Node((S("g",2)), [(V(T("x"))); (V(T("y")))]); Node((S("t",1)), [(V(T("z")))]); Node((S("B",0)),[]) ] );;
let b = Node (S ("f", 3),
 [Node (S ("g", 2), [V (T "x");  Node (S ("g", 2), [V (T "x");  Node (S ("g", 2), [V (T "x");   (V(T("m")))   ])    ])    ]); Node (S ("t", 1), [V (T "z")]);
  Node (S ("B", 0), [])])    ;; *)




(*
# sigma1;;
- : substitution = Q (T "x", V (T "Rachit Arora"))
# sigma2;;
- : substitution = Q (T "Rachit Arora", V (T "Alpha"))
# sigma3;;
- : substitution =
Q (T "z",
 Node (S ("f", 3),
  [Node (S ("g", 2), [V (T "x"); V (T "y")]); Node (S ("t", 1), [V (T "y")]);
   Node (S ("B", 0), [])]))
# sigma4;;
- : substitution = Q (T "y", V (T "This was y"))
# b;;
- : term =
Node (S ("f", 3),
 [Node (S ("g", 2),
   [V (T "x");
    Node (S ("g", 2), [V (T "x"); Node (S ("g", 2), [V (T "x"); V (T "y")])])]);
  Node (S ("t", 1), [V (T "z")]); Node (S ("B", 0), [])])
*)

(* 
val map : ('a -> 'b) -> 'a list -> 'b list
List.map f [a1; ...; an] applies function f to a1, ..., an, and builds the list [f a1; ...; f an] with the results returned by f. Not tail-recursive.

val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
List.fold_right f [a1; ...; an] b is f a1 (f a2 (... (f an b) ...)). Not tail-recursive. *)