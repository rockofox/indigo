(module
 (type $none_=>_none (func))
 (type $i32_i32_=>_none (func (param i32 i32)))
 (type $i32_i32_i32_i32_=>_i32 (func (param i32 i32 i32 i32) (result i32)))
 (import "wasi_unstable" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
 (memory $0 1 1)
 (data (i32.const 8) "hello world\n")
 (export "memory" (memory $0))
 (start $main)
 (func $puts (param $0 i32) (param $1 i32)
  (i32.store
   (i32.const 0)
   (local.get $0)
  )
  (i32.store
   (i32.const 4)
   (local.get $1)
  )
  (drop
   (call $fd_write
    (i32.const 1)
    (i32.const 0)
    (i32.const 1)
    (i32.const 20)
   )
  )
 )
 (func $main
  (local $0 i32)
  (local.set $0
   (i32.const 8)
  )
  (call $puts
   (local.get $0)
   (i32.const 12)
  )
 )
)
