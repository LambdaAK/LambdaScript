	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_foo                            ; -- Begin function foo
	.p2align	2
_foo:                                   ; @foo
; %bb.0:                                ; %entry
	mov	w0, #1                          ; =0x1
	ret
                                        ; -- End function
	.globl	_add                            ; -- Begin function add
	.p2align	2
_add:                                   ; @add
; %bb.0:                                ; %entry
	tst	w2, #0x1
	cneg	w8, w1, eq
	add	w0, w8, w0
	ret
                                        ; -- End function
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	mov	w0, #3                          ; =0x3
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
