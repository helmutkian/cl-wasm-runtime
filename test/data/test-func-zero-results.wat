(module
	(type $test_t (func (param i32 i32)))
	(func $test_f (type $test_t) (param $x i32) (param $y i32))
	(export "test" (func $test_f)))