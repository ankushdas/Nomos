module NC = NomosConfig
module C = Core

let () =
  C.Command.run ~version:"1.0" ~build_info:"RWO" NC.rast_command;;