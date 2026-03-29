	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_even                           ; -- Begin function even
	.p2align	2
_even:                                  ; @even
; %bb.0:                                ; %entry
	cbz	w0, LBB0_3
LBB0_1:                                 ; %tailrecurse
                                        ; =>This Inner Loop Header: Depth=1
	cmp	w0, #1
	b.eq	LBB0_4
; %bb.2:                                ; %else_2.i
                                        ;   in Loop: Header=BB0_1 Depth=1
	sub	w0, w0, #2
	cbnz	w0, LBB0_1
LBB0_3:                                 ; %merge_3.loopexit
	mov	w0, #1                          ; =0x1
	ret
LBB0_4:                                 ; %merge_3.fold.split
	mov	w0, #0                          ; =0x0
	ret
                                        ; -- End function
	.globl	_odd                            ; -- Begin function odd
	.p2align	2
_odd:                                   ; @odd
; %bb.0:                                ; %entry
	cbz	w0, LBB1_2
; %bb.1:                                ; %else_2
	sub	w0, w0, #1
	b	_even
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
	mov	w0, #6                          ; =0x6
	bl	_even
	and	w0, w0, #0x1
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
