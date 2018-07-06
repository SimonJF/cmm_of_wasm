type c_export

val c_exports : module_name:string -> Ir.Stackless.module_ -> c_export list
val header : module_name:string -> exports:c_export list -> string
val stub_file : header_filename:string -> module_name:string -> exports:c_export list -> string
