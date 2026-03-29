	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_add                            ; -- Begin function add
	.p2align	2
_add:                                   ; @add
; %bb.0:                                ; %entry
	add	w0, w1, w0
	ret
                                        ; -- End function
	.globl	_factorial                      ; -- Begin function factorial
	.p2align	2
_factorial:                             ; @factorial
; %bb.0:                                ; %entry
	mov	x8, x0
	mov	w0, #1                          ; =0x1
	cbz	w8, LBB1_2
LBB1_1:                                 ; %else_2
                                        ; =>This Inner Loop Header: Depth=1
	mul	w0, w0, w8
	sub	w8, w8, #1
	cbnz	w8, LBB1_1
LBB1_2:                                 ; %merge_3
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
	mov	w0, #30                         ; =0x1e
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #5                          ; =0x5
	bl	_factorial
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
