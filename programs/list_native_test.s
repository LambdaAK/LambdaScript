	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_sum                            ; -- Begin function sum
	.p2align	2
_sum:                                   ; @sum
; %bb.0:                                ; %entry
	mov	w8, #0                          ; =0x0
	cbz	x0, LBB0_2
LBB0_1:                                 ; %swn_3
                                        ; =>This Inner Loop Header: Depth=1
	ldr	w9, [x0]
	ldr	x0, [x0, #8]
	add	w8, w8, w9
	cbnz	x0, LBB0_1
LBB0_2:                                 ; %swm_1
	mov	x0, x8
	ret
                                        ; -- End function
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	x0, #0                          ; =0x0
	mov	w19, #4                         ; =0x4
LBB1_1:                                 ; %lrb_2
                                        ; =>This Inner Loop Header: Depth=1
	mov	x20, x0
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	str	w19, [x0]
	str	x20, [x0, #8]
	sub	w19, w19, #1
	cmn	w19, #1
	b.ne	LBB1_1
; %bb.2:                                ; %lre_3
	bl	_sum
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x19, x0
	mov	w8, #4                          ; =0x4
	str	w8, [x0]
	str	xzr, [x0, #8]
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x20, x0
	mov	w8, #3                          ; =0x3
	str	w8, [x0]
	str	x19, [x0, #8]
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x19, x0
	mov	w8, #2                          ; =0x2
	str	w8, [x0]
	str	x20, [x0, #8]
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x20, x0
	mov	w8, #1                          ; =0x1
	str	w8, [x0]
	str	x19, [x0, #8]
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	str	wzr, [x0]
	str	x20, [x0, #8]
	bl	_sum
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
