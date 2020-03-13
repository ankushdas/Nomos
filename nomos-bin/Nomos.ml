module NC = Lib.NomosConfig
module C = Core

let () =
  C.Command.run ~version:"1.0" ~build_info:"RWO" NC.nomos_command;;
