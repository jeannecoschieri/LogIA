open Ast

exception Unsat 
exception EmptyCNF

let assign f x = 
  let new_cnf = Cnf.fold 
                    (fun c acc ->
                      if Clause.cardinal c = 1 && Clause.mem x c then acc
                      else if Clause.cardinal c = 1 && Clause.mem (neg x) c then raise Unsat
                      else if Clause.mem x c then acc
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




(********************* BASIC *********************)

let rec aux_cdpll f t m = 
  let f', m' = unit_propagate f m in 
  if have_empty f' (* there is an empty clause in f' *)
    then 0
  else if Cnf.cardinal f'.cnf = 0 (* all clauses of f are satisfied with m as a model *)
    then int_of_float (2. ** (float_of_int (f.nb_var - t)))
  else
    let x = selectUnassignVariable f' in 
    aux_cdpll (assign f' x) (t + 1) (x :: m') + aux_cdpll (assign f' (neg x)) (t + 1) (neg x :: m')



let cdpll f = aux_cdpll f 0 [] 




