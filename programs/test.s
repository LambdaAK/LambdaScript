	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_double_each                    ; -- Begin function double_each
	.p2align	2
_double_each:                           ; @double_each
	.cfi_startproc
; %bb.0:                                ; %entry
	cbz	x0, LBB0_2
; %bb.1:                                ; %swn_3
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	ldr	w8, [x0]
	ldr	x0, [x0, #8]
	lsl	w20, w8, #1
	bl	_double_each
	mov	x19, x0
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	str	w20, [x0]
	str	x19, [x0, #8]
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
LBB0_2:                                 ; %common.ret
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_print_list                     ; -- Begin function print_list
	.p2align	2
_print_list:                            ; @print_list
	.cfi_startproc
; %bb.0:                                ; %entry
	cbz	x0, LBB1_2
; %bb.1:                                ; %swn_3
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	ldr	w0, [x0]
	bl	_ls_int_to_str
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	b	_ls_println
LBB1_2:                                 ; %swm_1
	ret
	.cfi_endproc
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
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x19, x0
	mov	w8, #5                          ; =0x5
	str	w8, [x0]
	str	xzr, [x0, #8]
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x20, x0
	mov	w8, #4                          ; =0x4
	str	w8, [x0]
	str	x19, [x0, #8]
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x19, x0
	mov	w8, #3                          ; =0x3
	str	w8, [x0]
	str	x20, [x0, #8]
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x20, x0
	mov	w8, #2                          ; =0x2
	str	w8, [x0]
	str	x19, [x0, #8]
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	w8, #1                          ; =0x1
	str	w8, [x0]
	str	x20, [x0, #8]
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
