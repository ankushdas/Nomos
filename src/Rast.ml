module RC = RastConfig
module C = Core
module S = Solver
module ClpS = S.Clp (S.Clp_std_options)

let q1 = ClpS.fresh_var ();;
let q2 = ClpS.fresh_var ();;
let () = ClpS.add_constr_list ~lower:2.0 ~upper:5.0 [(q1,1.0); (q2,1.0)];;
let () = ClpS.add_objective q2 1.0;;
let r = ClpS.first_solve ();;
let v1 = ClpS.get_solution q1;;
let v2 = ClpS.get_solution q2;;
print_string ((string_of_float v1) ^ " " ^ (string_of_float v2) ^ "\n");;
(*
let () =
  C.Command.run ~version:"1.0" ~build_info:"RWO" RC.rast_command;;*)