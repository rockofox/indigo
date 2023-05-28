(module
    ;; Import the required fd_write WASI function which will write the given io vectors to stdout
    ;; The function signature for fd_write is:
    ;; (File Descriptor, *iovs, iovs_len, nwritten) -> Returns number of bytes written
    (import "wasi_unstable" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))

    (memory 1)
    (export "memory" (memory 0))

    ;; Write 'hello world\n' to memory at an offset of 8 bytes
    ;; Note the trailing newline which is required for the text to appear
    (data (i32.const 8) "hello world\n")
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
    (func $main (export "_start")
        (local $0 i32)
        (local.set $0 (i32.const 8))

        ;; (call $fd_write
        ;;     (i32.const 1) ;; file_descriptor - 1 for stdout
        ;;     ;; (i32.const 0) ;; *iovs - The pointer to the iov array, which is stored at memory location 0
        ;;     (local.get $0)
        ;;     (i32.const 1) ;; iovs_len - We're printing 1 string stored in an iov - so one.
        ;;     (i32.const 20) ;; nwritten - A place in memory to store the number of bytes written
        ;; )
        ;; drop ;; Discard the number of bytes written from the top of the stack
        (call $puts
            (local.get $0)
            (i32.const 12)
        )
    )
)