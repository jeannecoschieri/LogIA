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



let rec aux_partial f n m l lim : (int * model list) = 
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




(********************* COMPONENT *********************)

module IntSet = Set.Make (Int) (* Set of integers *)
module IntMap = Map.Make (Int) 
type graph = IntSet.t IntMap.t (* A graph maps vertices to their neighbourhood *)

(* Return the set of variables appearing as literals in a clause c *)
let vars_of_clause (c: Clause.t): IntSet.t = 
  Clause.fold (fun lit acc -> IntSet.add (abs lit) acc) c IntSet.empty

(* Build the constraint graph of the formula f *)
let build_constraint_graph (f: Cnf.t): graph =
  let add_edge (x: int) (y: int) (g: graph): graph = (* Add edge (x, y) to g *)
    let add_one (a: int) (b: int) (g: graph): graph =  (* Add b to the neighbourhood of a *)
      let voisins = IntMap.find_opt a g |> Option.value ~default:IntSet.empty in
      IntMap.add a (IntSet.add b voisins) g
    in
    g |> add_one x y |> add_one y x
  in

  Cnf.fold
    (fun c g ->
       let vars = IntSet.elements (vars_of_clause c) in
       List.fold_left (* Iterate on each pair (x, y) of variables
                      in the clause c and add the corresponding edge *)
         (fun g x ->
            List.fold_left
              (fun g y -> if x = y then g else add_edge x y g)
              g vars)
         g vars)
    f IntMap.empty

(* Return the connected components of graph g *)
let connected_components (g: graph): IntSet.t list =
  (* Basic dfs, visited is global and comp is the current connected component *)
  let rec dfs (v: int) (visited: IntSet.t) (comp: IntSet.t): IntSet.t * IntSet.t =
    if IntSet.mem v visited then (visited, comp)
    else
      let visited = IntSet.add v visited in
      let comp = IntSet.add v comp in
      let voisins =
        IntMap.find_opt v g |> Option.value ~default:IntSet.empty
      in
      IntSet.fold
        (fun u (v, c) -> dfs u v c)
        voisins
        (visited, comp)
  in

  (* Run the dfs on each unvisited vertice, and store the found connected components in acc *)
  let rec loop (vars: int list) (visited: IntSet.t) (acc: IntSet.t list): IntSet.t list =
    match vars with
    | [] -> acc
    | v :: rest ->
        if IntSet.mem v visited then
          loop rest visited acc
        else
          let visited', comp = dfs v visited IntSet.empty in
          loop rest visited' (comp :: acc)
  in

  loop (List.map fst (IntMap.bindings g)) IntSet.empty []

(* Give the partition of subformulas such that variable sets are disjoints *)
let partition_cnf (f: t): t list =
  let graph = build_constraint_graph f.cnf in
  let components = connected_components graph in
  (* Build the constraint graph and calculate the connected components *)

  List.map
    (fun vars -> (* Iterate on each connected component *)
       let cnf_i =
         Cnf.filter (* Keep each clause such that all variables are in the component *)
           (fun clause ->
              let clause_vars = vars_of_clause clause in
              IntSet.subset clause_vars vars)
           f.cnf
       in
       { (* Build the subformula *)
         nb_var = IntSet.cardinal vars;
         nb_clause = Cnf.cardinal cnf_i;
         cnf = cnf_i;
       })
    components

(* Apply DPLL method with connected components preprocessing *)
let dpll_components (f: t): (int * model list) =
  (* Apply DPLL on each subformula *)
  let (numbers, models) = partition_cnf f |> List.map cdpll |> List.split
  in (
    List.fold_left (fun x y -> x*y) 1 numbers, (* Product of each number of models *)
    List.fold_left
    (* Concatenate each partial model for Fi with all those
    for Fj, j different from i, and take the list of all of those models *)
    (fun acc l ->
      List.concat_map
      (* Iterate on each l element to concatenate it to each element of acc *)
      (fun m ->
        List.map
        (fun m' -> m'@m)
        acc
      ) 
      l
    )
    [[]] models
  )




(********************* COSAT *********************)



(* to transform a formula of type t into a file .dimacs *)
let write_dimacs (filename : string) (f : t) =  
  let output = open_out filename in  
  Printf.fprintf output "p cnf %d %d\n" f.nb_var f.nb_clause;  
  Cnf.iter  
  (fun clause ->  
    Clause.iter (fun lit -> Printf.fprintf output "%d " lit) clause;  
    Printf.fprintf output "0\n")  
  f.cnf;  
  close_out output  



(* test with miniSAT if a formula of type t is sat or not *)
  let is_sat (f: t) : bool =
  let cnf_file = Filename.temp_file "component" ".cnf" in

  write_dimacs cnf_file f;

  let cmd =
    Printf.sprintf "minisat %s /dev/null 1> /dev/null 2> /dev/null" cnf_file
  in
  let code = Sys.command cmd in

  Sys.remove cnf_file;

  match code with
  | 10 -> true
  | 20 -> false
  | _  -> failwith "MiniSAT error - MiniSAT may not be installed"


let dpll_cosat (f: t): (int * model list) = 
  let part = partition_cnf f in 
  if List.for_all is_sat part then (* If each subformula is satisfiable *)
  let (numbers, models) = part |> List.map cdpll |> List.split
  in (
    List.fold_left (fun x y -> x*y) 1 numbers, (* Product of each number of models *)
    List.fold_left
    (* Concatenate each partial model for Fi with all those
    for Fj, j different from i, and take the list of all of those models *)
    (fun acc l ->
      List.concat_map
      (* Iterate on each l element to concatenate it to each element of acc *)
      (fun m ->
        List.map
        (fun m' -> m'@m)
        acc
      ) 
      l
    )
    [[]] models
  )
  else (* Formula is unsatisfiable *)
    0, []
