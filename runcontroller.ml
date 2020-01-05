(** the module [Runcontroller] simply calls the
  [main] function in the [Controller] module.
    This is necessary to allow testing of the
    [Controller] module, which otherwise would
    automatically run the game when opened.*)
open Controller
let _ = main ()
