type c_export

val c_exports : prefix:string -> Ir.Stackless.module_ -> c_export list
val header :
  prefix:string ->
  exports:c_export list ->
  ir_mod: Ir.Stackless.module_ ->
  string
val stub_file : header_filename:string -> prefix:string -> exports:c_export list -> string
