(* 
	PROGETTO PR2 2017-18
	CAU FABRIZIO
	CORSO A
	MATRICOLA: 508700
	INTERPRETE OCAML
*)

(* TIPI *)
type ide = string
(* espressioni da valutare *) 
and exp = 
	| Eint of int 
	| Ebool of bool
	| Den of ide
	| Prod of exp * exp
	| Sum of exp * exp
	| Diff of exp * exp
	| Eq of exp * exp
	| Minus of exp
	| IsZero of exp
	| Or of exp * exp
	| And of exp * exp
	| Not of exp
	| Ifthenelse of exp * exp * exp
	| Let of ide * exp * exp
	| Fun of ide * exp			
	| Appl of exp * exp
	| ETree of tree
	| ApplyOver of exp * exp
	| Update of (ide list) * exp * exp
	| Select of (ide list) * exp * exp

and tree =
	| Empty								(* Albero vuoto *)
	| Node of ide * exp * tree * tree	(* Albero binario *)

(* tipi esprimibili *)
and evT =								
	| Int of int
	| Bool of bool
	| FunVal of ide * exp * evT env
	| Unbound
	| TreeVal of treeVal

and treeVal =
	| EmptyVal
	| NodeVal of ide * evT * treeVal * treeVal
	

(* Ambiente *)
and 't env = ide -> 't;;

let emptyenv (v : evT) = function x -> v;;		(* ambiente vuoto *)
let applyenv (r : evT env) (i : ide) = r i;;	(* ricerca nell'ambiente *)
let bind (r : evT env) (i : ide) (v : evT) = function x -> if x = i then v else applyenv r x;;

(*rts*)
(*type checking*)
let typecheck (s : string) (v : evT) : bool = match s with
	| "int" -> (match v with
		| Int(_) -> true
		| _ -> false)
	| "bool" -> (match v with
		| Bool(_) -> true
		| _ -> false)
	| _ -> failwith("not a valid type");;
	
(* typecheck albero *)
let rec checktree t = match t with		
	| ETree(Empty) -> true
	| ETree(Node(_,_,lt,rt)) -> (checktree(ETree(lt)) &&  checktree(ETree(rt)))
	| _ -> false;;	
	
(* funzioni primitive *)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n*u))
	else failwith("Type error")
and sum x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n+u))
	else failwith("Type error")
and diff x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n-u))
	else failwith("Type error")
and eq x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Bool(n=u))
	else failwith("Type error")
and minus x = if (typecheck "int" x) 
	then (match x with
	   	Int(n) -> Int(-n))
	else failwith("Type error")
and iszero x = if (typecheck "int" x)
	then (match x with
		Int(n) -> Bool(n=0))
	else failwith("Type error")
and vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		(Bool(b),Bool(e)) -> (Bool(b||e)))
	else failwith("Type error")
and et x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		(Bool(b),Bool(e)) -> Bool(b&&e))
	else failwith("Type error")
and non x = if (typecheck "bool" x)
	then (match x with
		Bool(true) -> Bool(false) |
		Bool(false) -> Bool(true))
	else failwith("Type error");;


(* Interprete *)
let rec eval (e : exp) (r : evT env) : evT = match e with
	| Eint n -> Int n
	| Ebool b -> Bool b
	| IsZero a -> iszero (eval a r)
	| Den i -> applyenv r i
	| Eq(a, b) -> eq (eval a r) (eval b r)
	| Prod(a, b) -> prod (eval a r) (eval b r)
	| Sum(a, b) -> sum (eval a r) (eval b r)
	| Diff(a, b) -> diff (eval a r) (eval b r)
	| Minus a -> minus (eval a r)
	| And(a, b) -> et (eval a r) (eval b r)
	| Or(a, b) -> vel (eval a r) (eval b r)
	| Not a -> non (eval a r)
	| Ifthenelse(a, b, c) ->
		let g = (eval a r) in
			if (typecheck "bool" g)
				then (if g = Bool(true) then (eval b r) else (eval c r))
				else failwith ("nonboolean guard")
	| Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r))
	| Fun(pf,b) -> FunVal(pf,b,r)
	| Appl(f,a) -> (match (eval f r) with
		| FunVal(pf,b,r1) -> (eval b (bind r1 pf (eval a r)))
		| _ -> failwith "appl error: no function found" )
	(* OPERAZIONI SU ALBERI *)
	(* Restuisce l'albero valutato *)
	| ETree(a) -> if checktree (ETree(a)) then
		(match ETree(a) with
			| ETree(Empty) -> TreeVal(EmptyVal)
			| ETree(Node(i,v,lt,rt)) -> 
				TreeVal(NodeVal(i,(eval v r),(match (eval (ETree(lt)) r) with 
					| TreeVal(n) -> n
					| _ -> failwith "error: not a node"
				),(match (eval (ETree(rt)) r) with
					| TreeVal(n) -> n
					| _ -> failwith "error: not a node")))
			| _ -> failwith "error: not a tree")
		else failwith "error: not a correct ETree" 
	(* Restituisce l'albero t applicando la funzione f al valore di ogni nodo dell'albero *)
	| ApplyOver(f,t) -> let evalF = (eval f r) in
		(match evalF with
			| FunVal(pf,b,r1) -> (match (eval t r) with
				| TreeVal(tv) -> TreeVal( let rec applFunToTree tv = match tv with
					| EmptyVal -> EmptyVal
					| NodeVal(i,v,lt,rt) -> NodeVal(i,(eval b (bind r1 pf v)),(applFunToTree lt),(applFunToTree rt))
					in applFunToTree tv)  
				| _ -> failwith "error: not a tree")
			| _ -> failwith "error: not a function")
	(* Restituisce l'albero t applicando la funzione f al nodo individuato dal cammino nella lista iList,
		altrimenti restituisce l'albero invariato *)
	| Update(iList,f,t) -> let evalF = (eval f r) in
		(match evalF with
			| FunVal(pf,b,r1) -> (match (eval t r) with
				| TreeVal(tv) -> TreeVal( let rec applFunToNode iList tv = match iList,tv with
					| ([],_) -> tv
					| (_,EmptyVal) -> EmptyVal
					| (nId::rest,NodeVal(i,v,lt,rt)) -> if(i=nId) then (match rest with
						| [] -> NodeVal(i,(eval b (bind r1 pf v)),lt,rt)
						| el::_ -> NodeVal(i,v,(applFunToNode rest lt),(applFunToNode rest rt))
						| _ -> tv)
						else tv
					in applFunToNode iList tv)
				| _ -> failwith "error: not a tree")
			| _ -> failwith "error: not a function")
	(* 	Restituisce il sottoalbero con radice il nodo individuato dal cammino nella lista iList 
		se e solo se la funzione di controllo f applicata al valore del nodo restituisce true *)	
	| Select(iList,f,t) -> let evalF = (eval f r) in
		(match evalF with
			| FunVal(pf,b,r1) -> (match (eval t r) with
				| TreeVal(tv) -> TreeVal( let rec findSubTree iList tv = match iList,tv with
					| ([],_) -> tv
					| (_,EmptyVal) -> EmptyVal
					| (nId::[],NodeVal(i,v,lt,rt)) ->								(* Ultimo elemento del cammino: se il tag del nodo coincide *)
						if (nId=i && ((eval b (bind r1 pf v))=Bool(true))) then tv	(* valuto la funzione e se questa è true restituisco il sottoalbero *)
						else EmptyVal												(* con radice quel nodo, altrimenti l'albero vuoto *)
					| (nId::rest,NodeVal(i,v,lt,rt)) ->						(* cammino ancora da leggere *)
						if nId=i then (match rest,lt,rt with
							| (nextNodeId::_,NodeVal(iLt,_,_,_),NodeVal(iRt,_,_,_)) ->	(* Tripla: prossimo nodo nel cammino, sottoalbero sx, sottoalbero dx *)
								if nextNodeId=iLt then (findSubTree rest lt)			(* Chiamo ricorsivamente la fun findSubTree sul primo nodo che trovo da sx, *)
								else if nextNodeId=iRt then (findSubTree rest rt)		(* se nessuno dei due è identificato dal cammino restituisco l'albero vuoto *)
								else EmptyVal
							| (nextNodeId::_,NodeVal(iLt,_,_,_),EmptyVal) ->		(* Tripla: prossimo nodo nel cammino, sottoalbero sx, sottoalbero dx vuoto *)
								if nextNodeId=iLt then (findSubTree rest lt)		(* controllo solo il sottoalbero sx, se non è identificato dal cammino *)
								else EmptyVal										(* restituisco l'albero vuoto *)
							| (nextNodeId::_,EmptyVal,NodeVal(iRt,_,_,_)) ->		(* Tripla: prossimo nodo nel cammino, sottoalbero sx vuoto, sottoalbero dx *)
								if nextNodeId=iRt then (findSubTree rest rt)		(* controllo solo il sottoalbero dx, se non è identificato dal cammino *)
								else EmptyVal										(* restituisco l'albero vuoto *)
							| _ -> tv)
						else EmptyVal
					in findSubTree iList tv)
				| _ -> failwith "error: not a tree")	
			| _ -> failwith "error: not a function")
	| _ -> failwith "error wrong expression";;	
