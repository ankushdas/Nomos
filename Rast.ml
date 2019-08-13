module RC = RastConfig
module C = Core

let () =
  C.Command.run ~version:"1.0" ~build_info:"RWO" RC.command;;