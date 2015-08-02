exception Illegal;;
type operation = Add | Mult | Sub_l | Sub_r | Div_l | Div_r;;
type nombre = Int of int | Op of int * operation * nombre * nombre;;

let rec distribute_acc g f a l acc =
	match l with
	[] -> (	)
	| x::t -> begin 
		try 
			g( acc@[f a x]@t)
		with Illegal -> ();
	end;
	distribute_acc g f a t (x::acc);;

let distribute g f a l = 
	distribute_acc g f a l [];;

let gv x =
	match x with
	| Int a -> a;
	| Op(r,_,_,_) -> r;;

let add x y = 
	Op(gv(x)+gv(y),Add,x,y);;
	
let sub_r x y = 
	if (gv(x) < gv(y)) then raise Illegal
	else Op(gv(x)-gv(y), Sub_r,x,y);;
let sub_l x y = 
	sub_r y x;;

let mult x y = 
	Op(gv(x)*gv(y),Mult, x,y);;
let div_r x y = 
	if (gv(y)=0) || (gv(x) mod gv(y) !=0) then raise Illegal
	else Op(gv(x)/gv(y),Div_r,x,y);;

let div_l x y = 
	div_r y x;;

(*
let explore_acc l acc =
	match l with
	[] -> ()
	| x::t -> distribute explore_acc add x t
*)
let rec print_nombre x =
	match x with
	Int a ->  a;
	| Op (res, op, op1, op2) -> 
		let (op_string,val1,val2) = match op with
		| Add -> " + ", print_nombre op1, print_nombre op2
		| Mult -> " * ", print_nombre op1, print_nombre op2
		| Sub_r -> " - ", print_nombre op1, print_nombre op2
		| Sub_l -> " - ", print_nombre op2, print_nombre op1
		| Div_r -> " / ", print_nombre op1, print_nombre op2
		| Div_l -> " / ", print_nombre op2, print_nombre op1
		in
			Printf.printf "%d %s %d = %d\n" val1 op_string val2 res;
		res
;;

let g l = 
	List.iter (fun x -> Printf.printf "%d " x) l;
	print_string "\n";;

let f = (+);;

let moins x y  =
	if (x < y) then raise Illegal
	else (x-y);;

distribute g f 1 [1;2;3;5];;
print_string "----------------\n";
distribute g moins 3 [1;2;3;5;0;2];;
print_string "----------------\n";;

let pn l = 
	List.iter (fun x -> print_int (print_nombre x)) l; 
	print_string " | \n";;

distribute pn div_r (Int 3) [Int 6;Int 2;Int 3;Int 5;Int 0;Int 1];;
print_string "----------------\n";
distribute pn div_l (Int 3) [Int 1;Int 2;Int 3;Int 5;Int 0;Int 2];;
print_string "----------------\n";
distribute pn div_r (Int 3) [Int 1;Int 3;Int 6];;
print_string "----------------\n";
distribute pn div_l (Int 3) [Int 1;Int 3;Int 6];;

print_string "----------------\n";
print_nombre (Int 4);;
print_string "----------------\n";
print_nombre (Op (10, Add,  (Op (8, Mult, Int 2, Int 4)),  
                            (Op (2, Add,  Int 1, Int 1))
                 )
             );;
