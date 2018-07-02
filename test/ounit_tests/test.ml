open OUnit
open Libwasm.Types

let assert_nativeint_equal n1 n2 =
  assert_equal
    ~printer:Nativeint.to_string
    ~cmp:Nativeint.equal
    n1 n2

let suite =
  "cmm_of_wasm: Util.Type_hashing" >::: [
    "void -> void" >:: (fun _ ->
      let expected = Nativeint.of_string "0x0000000000000000" in
      let func_ty = FuncType ([], []) in
      let actual = Util.Type_hashing.hash_function_type func_ty in
      assert_nativeint_equal expected actual
    );

    "void -> i32" >:: (fun _ ->
      let expected = Nativeint.of_string "0x0000000000000001" in
      let func_ty = FuncType ([], [I32Type]) in
      let actual = Util.Type_hashing.hash_function_type func_ty in
      assert_nativeint_equal expected actual
    );

    "void -> i64" >:: (fun _ ->
      let expected = Nativeint.of_string "0x0000000000000002" in
      let func_ty = FuncType ([], [I64Type]) in
      let actual = Util.Type_hashing.hash_function_type func_ty in
      assert_nativeint_equal expected actual
    );

    "void -> f32" >:: (fun _ ->
      let expected = Nativeint.of_string "0x0000000000000003" in
      let func_ty = FuncType ([], [F32Type]) in
      let actual = Util.Type_hashing.hash_function_type func_ty in
      assert_nativeint_equal expected actual
    );

    "void -> f64" >:: (fun _ ->
      let expected = Nativeint.of_string "0x0000000000000004" in
      let func_ty = FuncType ([], [F64Type]) in
      let actual = Util.Type_hashing.hash_function_type func_ty in
      assert_nativeint_equal expected actual
    );

    "i32 -> void" >:: (fun _ ->
      let expected = Nativeint.of_string "0x0000000000000008" in
      let func_ty = FuncType ([I32Type], []) in
      let actual = Util.Type_hashing.hash_function_type func_ty in
      assert_nativeint_equal expected actual
    );

    "i32 -> i32" >:: (fun _ ->
      let expected = Nativeint.of_string "0x0000000000000009" in
      let func_ty = FuncType ([I32Type], [I32Type]) in
      let actual = Util.Type_hashing.hash_function_type func_ty in
      assert_nativeint_equal expected actual
    );

    "i32 -> i64" >:: (fun _ ->
      let expected = Nativeint.of_string "0x000000000000000A" in
      let func_ty = FuncType ([I32Type], [I64Type]) in
      let actual = Util.Type_hashing.hash_function_type func_ty in
      assert_nativeint_equal expected actual
    );

    "i32, i64, f32, f64 -> i64" >:: (fun _ ->
      let expected = Nativeint.of_string "0x000000000000468A" in
      let func_ty = FuncType ([I32Type; I64Type; F32Type; F64Type], [I64Type]) in
      let actual = Util.Type_hashing.hash_function_type func_ty in
      assert_nativeint_equal expected actual
    );

    "i32, i32, f32 -> void" >:: (fun _ ->
      let expected = Nativeint.of_string "0x0000000000000648" in
      let func_ty = FuncType ([I32Type; I32Type; F32Type], []) in
      let actual = Util.Type_hashing.hash_function_type func_ty in
      assert_nativeint_equal expected actual
    );

    "unhashable: i32 * 21 -> void" >:: (fun _ ->
      assert_raises Util.Type_hashing.Unhashable (fun _ ->
        let arg_tys = Array.make 21 I32Type |> Array.to_list in
        let func_ty = FuncType (arg_tys, []) in
        Util.Type_hashing.hash_function_type func_ty
      ))
  ]

let _ = run_test_tt_main suite
