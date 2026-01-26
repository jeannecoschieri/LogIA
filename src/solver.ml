open Ast


let rec unit_propagate f = 
  try let l = Cnf.elements f in
  let min = Cnf.min_elt f in  
    if Cnf.cardinal min = 1 then 
      unit_propagate (Cnf.remove min f)
    else f 
  with Not_found -> f
GROS CACA


let basic f = ()