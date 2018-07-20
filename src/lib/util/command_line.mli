val generate_c : unit -> bool
val setup : unit -> unit
val dump_stackless : unit -> bool
val dump_cmm : unit -> bool
val dump_linear : unit -> bool
val verbose : unit -> bool
val cc : unit -> string
val dump_wasm : unit -> bool
val input_filename : unit -> string
val output_filename : unit -> string
val header_prefix_path : unit -> string
val rts_path : unit -> string
val rts_header : unit -> string
val initial_fuel : unit -> int
val keep_temp : unit -> bool
val prefix : unit -> string option
val colouring_allocator : unit -> bool
