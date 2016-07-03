open Core.Std;;

exception Illegal;;
type operation = Add | Mult | Sub |  Div ;;
type nombre = Int of int | Op of int * operation * nombre * nombre;;
exception Trouve of nombre;;
(************************************************)
(* basic function                               *)
(************************************************)

(************************************************)
(* gv :                                         *)
(*     get value of the nombre                  *)
(************************************************)

let gv x =
	match x with
	| Int a -> a;
	| Op(r,_,_,_) -> r;;
(************************************************)
(* add :                                        *)
(*     add two numbers                          *)
(************************************************)


let add x y = 
        if gv(x) <= gv(y) then raise Illegal
        else Op(gv(x)+gv(y),Add,x,y);;
	
let sub x y = 
	if (gv(x) <= gv(y)) then raise Illegal
	else Op(gv(x)-gv(y), Sub,x,y);;


let mult x y = 
        if (gv(x) <= gv(y) || gv(x) = 1 || gv(y) = 1 ) then raise Illegal
        else Op(gv(x)*gv(y),Mult, x,y);;

let div x y = 
	if (gv(y)=0) || not (gv(x) mod gv(y) =0) then raise Illegal
	else Op(gv(x)/gv(y),Div,x,y);;

(************************************************)
(* print :                                      *)
(*     print the nombre recursively             *)
(************************************************)

let rec print_nombre x =
	match x with
	Int a ->  a;
	| Op (res, op, op1, op2) -> 
		let (op_string,val1,val2) = match op with
		| Add -> " + ", print_nombre op1, print_nombre op2
		| Mult -> " * ", print_nombre op1, print_nombre op2
		| Sub -> " - ", print_nombre op1, print_nombre op2
		| Div -> " / ", print_nombre op1, print_nombre op2
		in
			Printf.printf "%d %s %d = %d\n" val1 op_string val2 res;
		res
;;

let print_nb n =
	ignore (print_nombre n);;

let rec complexity n =
	match n with
	Int x -> 0
	| Op(_,_,x,y) -> 1 +  (complexity x) + (complexity y);;
(************************************************)
(* end basic function                           *)
(************************************************)

let goal = ref 0;;
let result = ref [];;

let distribute g f  l = 
	let rec distribute_acc g f  l acc =
		match l with
		[] -> (	)
		| x::t -> begin 
			try 
				g( acc@[f  x]@t)
			with 
			| Illegal -> ();
		end;
		distribute_acc g f  t (x::acc)
	in
	distribute_acc g f  l [];;

let rec explore l =
	List.iter l ~f:(fun x -> if (gv(x) = !goal) then 
				result := x :: !result
                            else result := !result) ;

        let  rec explore_acc l acc =
	match l with
	| [] -> ()
	| a::t -> 
	      begin
				distribute explore (fun x -> add a x) (t@acc) ;
				distribute explore (fun x -> mult a x) (t@acc) ;
				distribute explore (fun x -> sub a x) (t@acc) ;
				distribute explore (fun x -> div a x) (t@acc) ;
				explore_acc t (a::acc);
				
				
		  end
	in 
	explore_acc l [];;
	

let command = 
  Command.basic
    ~summary:"le compte est bon"
    ~readme:(fun ()-> "More detail")
    Command.Spec.(empty +> anon("suite" %: string) +> anon("cible" %: int))
    (fun s g ()  -> (Printf.printf "command %s -> %d\n" s g;
                goal := g;
                let l = List.map ~f:(fun x -> Int (int_of_string x)) 
                        (Str.split (Str.regexp ",") s) in 
                (
                        explore l ;

                List.iter ~f:(fun x -> 
                        print_string "TROUVE : complexity ="; 
                        print_int (complexity(x)); print_string "\n";  
                        print_nb x)  
                        (List.sort ~cmp:( fun x y -> compare (complexity x)
                        (complexity y)) !result) ;
                )
                )
    );;
let () = 
        Command.run ~version:"1.0" ~build_info:"RWO" command;;
