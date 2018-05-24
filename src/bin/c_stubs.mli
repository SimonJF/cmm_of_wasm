type cfunc
val cfunc_of_func : Ir.Func.t -> cfunc option
val cfuncs_of_funcs : Ir.Func.t list -> cfunc list
val header : module_name:string -> c_funcs:cfunc list -> string
val stub_file : module_name:string -> c_funcs:cfunc list -> string
