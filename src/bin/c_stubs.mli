type cfunc
val cfunc_of_func : module_name:string -> Ir.Func.t -> cfunc option
val cfuncs_of_funcs : module_name:string -> Ir.Func.t list -> cfunc list
val header : module_name:string -> c_funcs:cfunc list -> string
val stub_file : header_filename:string -> c_funcs:cfunc list -> string
