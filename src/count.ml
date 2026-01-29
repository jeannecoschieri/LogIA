open Ast

exception Unsat 
exception EmptyCNF

let assign f x = 
  let new_cnf = Cnf.fold 
                    (fun c acc ->
                      if Clause.mem x c then acc
                      else if Clause.cardinal c = 1 && Clause.mem (neg x) c then raise Unsat
                      else if Clause.mem (neg x) c then Cnf.add (Clause.remove (neg x) c) acc
                      else Cnf.add c acc)
                      f.cnf Cnf.empty in  
  {nb_var = f.nb_var - 1; nb_clause = Cnf.cardinal new_cnf; cnf = new_cnf}
      

              
let rec unit_propagate f m =  
  try let clause_min = Cnf.min_elt f.cnf in  
    if Clause.cardinal clause_min = 1 then 
      let x = Clause.min_elt clause_min in
      unit_propagate (assign {nb_var = f.nb_var; nb_clause = f.nb_clause - 1; cnf = (Cnf.remove clause_min f.cnf)} x) (x :: m)
    else f, m
  with Not_found -> f, m


let selectUnassignVariable f = 
  try Clause.choose (Cnf.choose f.cnf) 
with _ -> raise EmptyCNF
  

let have_empty f = 
  Cnf.exists Clause.is_empty f.cnf



let rec list_union l1 l2 = match l1,l2 with 
  |[], _ -> l2
  | _, [] -> l1 
  | h1 :: t1, _ -> if List.mem h1 l2 then list_union t1 l2 else h1 :: (list_union t1 l2)


(********************* BASIC *********************)

let rec aux_cdpll f n m l : (int * model list) = 
  let f', m' = unit_propagate f m in 
  if have_empty f' (* there is an empty clause in f' *)
    then 0, l
  else if Cnf.cardinal f'.cnf = 0 (* all clauses of f are satisfied with m as a model *)
    then int_of_float (2. ** (float_of_int (n - (List.length m')))), m' :: l 
  else begin
    let x = selectUnassignVariable f' in 
    let auxt = ref (0,l) in  (* if x is assign to true *)
    let auxf = ref (0,l) in  (* if x is assign to false *)
    (try 
      auxt := aux_cdpll (assign f' x) n (x :: m') l 
    with
      |Unsat -> ()
    );
    (try 
      auxf := aux_cdpll (assign f' (neg x)) n (neg x :: m') l
    with 
      |Unsat -> ()
    );
    fst !auxt + fst !auxf, list_union (snd !auxt) (snd !auxf)
  end



let cdpll f = aux_cdpll f f.nb_var [] []





(********************* PARTIAL *********************)


let rec unit_propagate_part f m lim =  
  if List.length m >= lim then f, m
  else try let clause_min = Cnf.min_elt f.cnf in  
    if Clause.cardinal clause_min = 1 then 
      let x = Clause.min_elt clause_min in
      unit_propagate_part (assign {nb_var = f.nb_var; nb_clause = f.nb_clause - 1; cnf = (Cnf.remove clause_min f.cnf)} x) (x :: m) lim
    else f, m
  with Not_found -> f, m



let rec aux_partial f n m l lim : (var * model list) = 
    let f', m' = unit_propagate_part f m lim in 
    if have_empty f' (* there is an empty clause in f' *)
      then 0, l
    else if Cnf.cardinal f'.cnf = 0 (* all clauses of f are satisfied with m as a model *)
      then int_of_float (2. ** (float_of_int (n - (List.length m')))), m' :: l 
    else
      if List.length m' >= lim then 0,l 
      else begin 
      let x = selectUnassignVariable f' in 
      let auxt = ref (0,l) in  
      let auxf = ref (0,l) in  
      (try 
        auxt := aux_partial (assign f' x) n (x :: m') l lim    (* if x is assign to true *)
      with
        |Unsat -> ()
      );
      (try 
        auxf := aux_partial (assign f' (neg x)) n (neg x :: m') l lim    (* if x is assign to false *)
      with 
        |Unsat -> ()
      );
      fst !auxt + fst !auxf, list_union (snd !auxt) (snd !auxf)
    end 

  let partial f lim = aux_partial f f.nb_var [] [] lim