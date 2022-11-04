 (* 
	PROGETTO PR2 2017-18
	CAU FABRIZIO
	CORSO A
	MATRICOLA: 508700
	TEST INTERPRETE OCAML
*)

(* ======== TEST ======== *)

(* Albero con valori dei nodi interi *)
let t0 = ETree(									
			Node("a",Eint(10),
				(Node("b",Eint(5),
					(Node("d",Eint(43),
						(Node("h",Eint(2),
							(Node("p",Eint(32),
								(Empty),
								(Empty))),
							(Empty))),
						(Node("i",Eint(57),
							(Empty),
							(Empty))))),
					(Node("e",Eint(9),
						(Node("j",Eint(15),
							(Empty),
							(Empty))),
						(Node("k",Eint(99),
							(Node("q",Eint(11),
								(Empty),
								(Empty))),
							(Empty))))))),
				(Node("c",Eint(8),
					(Node("f",Eint(24),
						(Node("l",Eint(18),
							(Empty),
							(Node("r",Eint(61),
								(Empty),
								(Empty))))),
						(Node("m",Eint(34),
							(Empty),
							(Empty))))),
					(Node("g",Eint(76),
						(Node("n",Eint(7),
							(Empty),
							(Empty))),
						(Node("o",Eint(0),
							(Empty),
							(Empty)))))))));;

(* Albero con valori ed espressioni di tipo interi *)
let t1 = ETree(									
			Node("a",(Sum(Eint(3),Eint(7))),
				(Node("b",(Diff(Eint(50),Eint(45))),
					(Node("d",(Sum(Eint(40),Eint(3))),
						(Node("h",(Prod(Eint(1),Eint(2))),
							(Node("p",(Prod(Eint(4),Eint(8))),
								(Empty),
								(Empty))),
							(Empty))),
						(Node("i",(Minus(Minus(Sum(Eint(7),(Prod(Eint(5),Eint(10))))))),	   
							(Empty),
							(Empty))))),
					(Node("e",Eint(9),
						(Node("j",Eint(15),
							(Empty),
							(Empty))),
						(Node("k",Eint(99),
							(Node("q",Eint(11),
								(Empty),
								(Empty))),
							(Empty))))))),
				(Node("c",Eint(8),
					(Node("f",Eint(24),
						(Node("l",Eint(18),
							(Empty),
							(Node("r",Eint(61),
								(Empty),
								(Empty))))),
						(Node("m",Eint(34),
							(Empty),
							(Empty))))),
					(Node("g",Eint(76),
						(Node("n",Eint(7),
							(Empty),
							(Empty))),
						(Node("o",Eint(0),
							(Empty),
							(Empty)))))))));;

(* Creazione ambiente *)
let env0 = emptyenv Unbound;;


let f0 = Fun("x",Sum((Den("x")), Eint(1)));;	(* f(x) = x+1 *)

let f1 = Fun("x",Prod((Den("x")), Eint(10)));;	(* f(x) = x*10 *)

let f2 = Fun("x",Minus((Den("x"))));;			(* f(x) = -x *)

let f3 = Fun("x",IsZero(Den("x")));;			(* f(x) = (x=0) *)

let f4 = Fun("x",Eq((Den("x")), Eint(11)));;	(* f(x) = (x=11) *)

let f5 = Fun("x",Ebool(true));;					(* f(x) = true *)


let path0 = ["a";"b"];;					(* cammino breve *)
let path1 = ["a";"b";"e";"k";"q"];; 	(* cammino fino all'ultimo livello *)
let wrongpath0 = ["a";"b";"s";"l"];;	(* cammino inesistente *)
 
(* OPERAZIONI SU ALBERI: *) 

let test0 = eval (ApplyOver(f0,t0)) env0;;		(* albero t0: x+1 al valore di ogni nodo *)
 
let test1 = eval (ApplyOver(f1,t1)) env0;;		(* albero t1: x*10 al valore di ogni nodo *)

let test2 = eval (Update(path1,f1,t0)) env0;;	(* albero t0: moltiplico per 10 il valore del nodo indentificato dal path  *)

let test3 = eval (Select(path0,f4,t0)) env0;;	(* albero t0: restituisce il sottoalbero identificato dal path se il valore del nodo radice è uguale a 11 *)

let test4 = eval (Select(path1,f5,t0)) env0;;	(* albero t0: restituisce il sottoalbero identificato dal path *)

let test5 = eval t1 env0;;						(* albero t1 valutato *)

(* NOTA: Utilizzando il path "wrongpath0" (cammino inesistente negli alberi t0 e t1) nel caso di:
	Update: restituisce l'albero valutato ma invariato 
	Select: restituisce l'albero vuoto 
*)
