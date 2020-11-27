module NC = Lib.NomosConfig
module C = Core

let () =
  C.Command.run ~version:"0.1" ~build_info:"unstable" NC.nomos_command;;
