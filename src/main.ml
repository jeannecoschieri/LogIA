open Format
open Count


type execution_mode =
  | Cnf (** Solve a formula *)
  | Basic (** Count number of solutions of a formula *)
  | Partial of int  (** Count number of solutions given a limit *)
  | Components (** Same as basic but decompose the problem by components *)
  | Cosat (** decompose by components as Components and test each component with miniSAT *)
  | Sudoku of string (** Solve a sudoku given as a string *)


(** By default we solve a formula given in the dimacs format *)
let mode = ref Cnf

(** By default, CNF's output displays only true or false *)
let proof = ref false

(** Default solver. By doing the third part of the project, you might come with other solvers *)
module S = Dpll.DPLL(Dpll.DefaultChoice)

(** Handle files given on the command line *)
let handle_file : string -> unit = fun fname ->
  let p = Dimacs.parse_file fname in
  match S.solve p with
  | None -> printf "false@."
  | Some model ->
    let pp_list fmt list =
      let pp_sep fmt () = fprintf fmt "@ "
      in fprintf fmt "%a" (pp_print_list ~pp_sep pp_print_int) list in

    if !proof then printf "true: %a\n" pp_list model
    else printf "true@."



let handle_basic : string -> unit = fun fname ->
  let p = Dimacs.parse_file fname in
  let nb, l = cdpll p in
  if nb = 0 then printf "unsat \n"
  else begin
    print_string ("sat " ^ (string_of_int nb) ^ "\nAssignments that satisfy the formula : \n");
    let pp_list fmt list =
      let pp_sep fmt () = fprintf fmt "@ "
      in fprintf fmt "%a" (pp_print_list ~pp_sep pp_print_int) list in
    List.iter (fun model -> printf " -> %a\n" pp_list model) l
  end
    

let handle_partial n fname =
  let p = Dimacs.parse_file fname in
  let nb, l = partial p n in 
  if nb = 0 then printf "unsat \n"
  else begin
    print_string ("sat " ^ (string_of_int nb) ^ "\nAssignments that satisfy the formula : \n");
    let pp_list fmt list =
      let pp_sep fmt () = fprintf fmt "@ "
      in fprintf fmt "%a" (pp_print_list ~pp_sep pp_print_int) list in
    List.iter (fun model -> printf " -> %a\n" pp_list model) l
  end

let handle_comp : string -> unit = fun fname ->
  let p = Dimacs.parse_file fname in
  let nb, l = dpll_components p in
  if nb = 0 then printf "unsat \n"
  else begin
    print_string ("sat " ^ (string_of_int nb) ^ "\nAssignments that satisfy the formula : \n");
    let pp_list fmt list =
      let pp_sep fmt () = fprintf fmt "@ "
      in fprintf fmt "%a" (pp_print_list ~pp_sep pp_print_int) list in
    List.iter (fun model -> printf " -> %a\n" pp_list model) l
  end


  let handle_cosat : string -> unit = fun fname ->
    let p = Dimacs.parse_file fname in
    let nb, l = dpll_cosat p in
    if nb = 0 then printf "unsat \n"
    else begin
      print_string ("sat " ^ (string_of_int nb) ^ "\nAssignments that satisfy the formula : \n");
      let pp_list fmt list =
        let pp_sep fmt () = fprintf fmt "@ "
        in fprintf fmt "%a" (pp_print_list ~pp_sep pp_print_int) list in
      List.iter (fun model -> printf " -> %a\n" pp_list model) l
    end




let handle_sudoku : string -> unit = fun str ->
  let sudoku, solution = Sudoku.read str in
  let env, ast = Sudoku.to_cnf sudoku in
  match S.solve ast with
  | None ->
    raise @@ failwith "No answer. Probably your encoding is incorrect"
  | Some model ->
    let candidate = Sudoku.solution_of env model in
    if candidate <> solution then
      raise @@ failwith "You found an incorrect answer."

(** Specification of the options handle by the program. You are only allowed to ADD new options *)
let spec =
  let debug_flags =
    let fn acc l = acc ^ "\n        " ^ l in
    List.fold_left fn "\n      Available flags:" (Console.log_summary ())
  in
  let spec = Arg.align
      [ ( "--debug"
        , Arg.String (Console.set_debug true)
        , "<flags> Sets the given debugging flags" ^ debug_flags )
      ; ( "--sudoku"
        , Arg.String (fun s -> mode := Sudoku s)
        , " Solve a sudoku given on the command line as a string" )
      ; ( "--proof"
        , Arg.Unit (fun () -> proof := true)
        , " Display variable assignment in CNF mode" ) 
      ; ( "--basic"
        , Arg.Unit (fun () -> mode := Basic)
        , "Count number of models that satisfied the formula")
      ; ( "--partial"
        , Arg.String (fun n -> mode := Partial (int_of_string n))
        , "Compute a lower bound on the number of models given a limit on their size")
      ; ( "--comp"
        , Arg.Unit (fun () -> mode := Components)
        , "Same as basic but decompose the problem by components")
      ; ( "--cosat"
        , Arg.Unit (fun () -> mode := Cosat)
        , "Same as basic but decompose the problem by components and test staisfiability of each component with miniSAT ")]
  in
  spec

(** Entry point of the program *)
let _ =
  let open Console in
  let usage = Format.sprintf "Usage: %s [CMD] [FILE]" Sys.argv.(0) in
  let files = ref [] in
  Arg.parse spec (fun s -> files := s :: !files) usage;
  try
    begin
      match !mode with
      | Cnf -> List.iter handle_file !files
      | Sudoku str ->  handle_sudoku str
      | Basic -> List.iter handle_basic !files
      | Components -> List.iter handle_comp !files
      | Partial n -> List.iter (fun x -> handle_partial n x) !files
      | Cosat -> List.iter handle_cosat !files
    end
  with
  | Fatal(None,    msg) -> exit_with "%s" msg
  | Fatal(Some(p), msg) -> exit_with "[%a] %s" Location.print p msg
