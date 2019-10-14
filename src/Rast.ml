module RC = RastConfig
module C = Core
(*module S = Solver

module Solver = S.Clp (S.Clp_std_options)*)

let () =
  C.Command.run ~version:"1.0" ~build_info:"RWO" RC.rast_command;;