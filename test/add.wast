(module
  (func $add (param $lhs i64) (param $rhs i64) (result i64)
    get_local $lhs
    get_local $rhs
    i64.add)
  (export "add" (func $add))
  (func $simple_add (result i64)
    (i64.const 5)
    (i64.const 10)
    (i64.add)
  )
  (export "simple_add" (func $simple_add))
)
