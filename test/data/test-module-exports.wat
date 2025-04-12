(module
	(func (export "function") (param i32 i64))
	(global (export "global") i32 (i32.const 7))
	(table (export "table") 0 funcref)
	(memory (export "memory") 1))