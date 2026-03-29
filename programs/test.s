	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_fib                            ; -- Begin function fib
	.p2align	2
_fib:                                   ; @fib
; %bb.0:                                ; %entry
	stp	x22, x21, [sp, #-48]!           ; 16-byte Folded Spill
	stp	x20, x19, [sp, #16]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #32]             ; 16-byte Folded Spill
	mov	x19, x0
	subs	w20, w0, #10
	b.hi	LBB0_2
; %bb.1:                                ; %entry
	mov	w8, #55                         ; =0x37
	mov	w9, #34                         ; =0x22
	mov	w10, #21                        ; =0x15
	mov	w11, #13                        ; =0xd
	mov	w12, #8                         ; =0x8
	mov	w13, #3                         ; =0x3
	mov	w14, #2                         ; =0x2
	mov	w0, #1                          ; =0x1
	mov	w15, w19
Lloh0:
	adrp	x16, lJTI0_0@PAGE
Lloh1:
	add	x16, x16, lJTI0_0@PAGEOFF
	adr	x17, LBB0_3
	ldrb	w1, [x16, x15]
	add	x17, x17, x1, lsl #2
	br	x17
LBB0_2:                                 ; %else_32
	sub	w0, w19, #1
	bl	_fib
	mov	x21, x0
	sub	w0, w19, #2
	bl	_fib
	add	w22, w0, w21
	sub	w0, w19, #3
	bl	_fib
	mov	x21, x0
	sub	w0, w19, #4
	bl	_fib
	add	w8, w21, w0
	add	w22, w22, w8
	sub	w0, w19, #5
	bl	_fib
	mov	x21, x0
	sub	w0, w19, #6
	bl	_fib
	add	w21, w21, w0
	sub	w0, w19, #7
	bl	_fib
	add	w8, w21, w0
	add	w22, w22, w8
	sub	w0, w19, #8
	bl	_fib
	mov	x21, x0
	sub	w0, w19, #9
	bl	_fib
	add	w19, w21, w0
	mov	x0, x20
	bl	_fib
	add	w8, w19, w0
	add	w8, w22, w8
LBB0_3:                                 ; %merge_33
	mov	x9, x8
LBB0_4:                                 ; %merge_30
	mov	x10, x9
LBB0_5:                                 ; %merge_27
	mov	x11, x10
LBB0_6:                                 ; %merge_24
	mov	x12, x11
LBB0_7:                                 ; %merge_21
	mov	x19, x12
LBB0_8:                                 ; %merge_18
	mov	x13, x19
LBB0_9:                                 ; %merge_15
	mov	x14, x13
LBB0_10:                                ; %merge_12
	mov	x0, x14
LBB0_11:                                ; %merge_3
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp], #48             ; 16-byte Folded Reload
	ret
	.loh AdrpAdd	Lloh0, Lloh1
	.section	__TEXT,__const
lJTI0_0:
	.byte	(LBB0_11-LBB0_3)>>2
	.byte	(LBB0_11-LBB0_3)>>2
	.byte	(LBB0_11-LBB0_3)>>2
	.byte	(LBB0_10-LBB0_3)>>2
	.byte	(LBB0_9-LBB0_3)>>2
	.byte	(LBB0_8-LBB0_3)>>2
	.byte	(LBB0_7-LBB0_3)>>2
	.byte	(LBB0_6-LBB0_3)>>2
	.byte	(LBB0_5-LBB0_3)>>2
	.byte	(LBB0_4-LBB0_3)>>2
	.byte	(LBB0_3-LBB0_3)>>2
                                        ; -- End function
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	mov	w0, #15                         ; =0xf
	bl	_fib
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
