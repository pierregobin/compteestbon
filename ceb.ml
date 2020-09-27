open Core;;



exception Illegal;;
type operation = Add | Mult | Sub |  Div ;;
type nombre = Int of int | Op of int * operation * nombre * nombre;;
exception Trouve of nombre;;
(******************* *****************************)
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

let addcpt = ref 0
and subcpt = ref 0
and mulcpt = ref 0
and divcpt = ref 0

let add x y =
  if gv(x) <= gv(y) then raise Illegal
  else (
    addcpt := !addcpt+1;
    Op(gv(x)+gv(y),Add,x,y)
  )

let sub x y =
  if (gv(x) <= gv(y)) then raise Illegal
  else (
    subcpt := !subcpt+1;
    Op(gv(x)-gv(y), Sub,x,y)
  )


let mult x y =
  if (gv(x) <= gv(y) || gv(x) = 1 || gv(y) = 1 ) then raise Illegal
  else(
    mulcpt := !mulcpt+1;
    Op(gv(x)*gv(y),Mult, x,y)
  )

let div x y =
  if (gv(y)=0) || not (gv(x) mod gv(y) =0) then raise Illegal
  else (
    divcpt := !divcpt+1;
    Op(gv(x)/gv(y),Div,x,y)
  )

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
  ignore (print_nombre n : int);;

let rec complexity n =
  match n with
    Int _ -> 0
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
    (let open Command.Let_syntax in
     let%map_open s = anon (sequence ("n" %: int) )
     and          g = flag "cible" (required int) ~aliases:["c"] ~doc:"nombre a trouver"
     and          cmplx = flag "complexity" no_arg ~doc:"affiche complexite min et max"
     and          sol   = flag "solutions" ~aliases:["s"] no_arg  ~doc:"affiche les solutions"
     and          cpt   = flag "compteur" ~aliases:["p"] no_arg  ~doc:"affiche les compteurs"
     in
     fun     () ->
       Printf.printf "ceb.exe %s -> %d\n" (String.concat ~sep:"," (List.map ~f:(fun x -> string_of_int x) s)) g;
       goal := g;
       explore (List.map ~f:(fun x -> Int x) s ) ;

       let r = List.sort ~compare:(fun (a,_) (c,_) -> compare c a)
           ( List.map ~f:(fun x -> (complexity(x),x)) !result)
       in
       (
          try 
             let (_,s) = (List.nth_exn r (List.length r - 1)) in
         if sol then (
           List.iter  r ~f:(fun (c,x) ->  ( print_string "TROUVE : complexity =";
                                            Printf.printf "%d\n" c;                
                                            print_nb x;));
         ) else
              print_nb s;
             if cmplx then (
               print_endline "complexite";
               let (max,_) = List.nth_exn r 0
               and (min,_) = List.nth_exn r (List.length r - 1) in
               Printf.printf "min = %d, max = %d\n" min max;
             );
             if cpt then (
               Printf.printf "add = %d, sub = %d, mul = %d, div = %d\n"
                 !addcpt !subcpt !mulcpt !divcpt;
               Printf.printf "total = %d\n"(!addcpt + !subcpt + !divcpt
                                            + !mulcpt);
             )
           with  _ -> 
             Printf.printf "pas de solution\n\n";
       )
    );;

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command;;
