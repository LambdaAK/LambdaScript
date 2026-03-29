	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_fact_helper__lsn1              ; -- Begin function fact_helper__lsn1
	.p2align	2
_fact_helper__lsn1:                     ; @fact_helper__lsn1
; %bb.0:                                ; %entry
	cbz	w0, LBB0_2
LBB0_1:                                 ; %else_2
                                        ; =>This Inner Loop Header: Depth=1
	mul	w1, w1, w0
	sub	w0, w0, #1
	cbnz	w0, LBB0_1
LBB0_2:                                 ; %merge_3
	mov	x0, x1
	ret
                                        ; -- End function
	.globl	_fact                           ; -- Begin function fact
	.p2align	2
_fact:                                  ; @fact
; %bb.0:                                ; %entry
	mov	w1, #1                          ; =0x1
	b	_fact_helper__lsn1
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
	mov	w0, #5                          ; =0x5
	mov	w1, #1                          ; =0x1
	bl	_fact_helper__lsn1
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
