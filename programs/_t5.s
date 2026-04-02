	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_even_val                       ; -- Begin function even_val
	.p2align	2
_even_val:                              ; @even_val
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x22, x21, [sp, #-48]!           ; 16-byte Folded Spill
	stp	x20, x19, [sp, #16]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #32]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 48
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	mov	x20, x0
	mov	w19, #0                         ; =0x0
LBB0_1:                                 ; %tailrecurse
                                        ; =>This Inner Loop Header: Depth=1
	mov	x0, x20
	bl	_ls_variant_tag
	cbz	w0, LBB0_5
; %bb.2:                                ; %swn_3
                                        ;   in Loop: Header=BB0_1 Depth=1
	mov	x0, x20
	bl	_ls_variant_tag
	mov	x21, x0
	mov	x0, x20
	bl	_ls_variant_payload
	cmp	w21, #1
	b.ne	LBB0_6
; %bb.3:                                ; %swm_4
                                        ;   in Loop: Header=BB0_1 Depth=1
	ldr	x20, [x0]
	mov	x0, x20
	bl	_ls_variant_tag
	mov	x21, x0
	mov	x0, x20
	bl	_ls_variant_payload
	cbnz	w21, LBB0_6
; %bb.4:                                ; %odd_val.exit
                                        ;   in Loop: Header=BB0_1 Depth=1
	ldr	x20, [x0]
	add	w19, w19, #2
	b	LBB0_1
LBB0_5:                                 ; %swm_1
	mov	x0, x19
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp], #48             ; 16-byte Folded Reload
	ret
LBB0_6:                                 ; %swf_3.i
	bl	_ls_abort
	brk	#0x1
	.cfi_endproc
                                        ; -- End function
	.globl	_odd_val                        ; -- Begin function odd_val
	.p2align	2
_odd_val:                               ; @odd_val
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	x19, x0
	bl	_ls_variant_tag
	mov	x20, x0
	mov	x0, x19
	bl	_ls_variant_payload
	cbnz	w20, LBB1_2
; %bb.1:                                ; %sws_2
	ldr	x0, [x0]
	bl	_even_val
	add	w0, w0, #1
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
LBB1_2:                                 ; %swf_3
	bl	_ls_abort
	brk	#0x1
	.cfi_endproc
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
	mov	w0, #0                          ; =0x0
	mov	x1, #0                          ; =0x0
	bl	_ls_variant_mk
	mov	x1, x0
	mov	w0, #0                          ; =0x0
	bl	_ls_variant_mk
	mov	x1, x0
	mov	w0, #1                          ; =0x1
	bl	_ls_variant_mk
	bl	_even_val
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
