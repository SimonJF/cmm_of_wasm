let gensym text =
    let counter = ref 0 in
    fun () ->
        let x = !counter in
        counter := x + 1;
        text ^ (string_of_int x)
