(module
	(import "math" "sum" (func $sum (param i32 i32) (result i32)))
	(func (export "add_one") (param $x i32) (result i32)
	      local.get $x
	      i32.const 1
	      call $sum))