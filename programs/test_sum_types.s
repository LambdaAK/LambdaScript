	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
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
	mov	w0, #0                          ; =0x0
	mov	x1, #0                          ; =0x0
	bl	_ls_variant_mk
	mov	x19, x0
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x1, x0
	mov	w8, #1                          ; =0x1
	str	w8, [x0]
	str	x19, [x0, #8]
	mov	w0, #1                          ; =0x1
	bl	_ls_variant_mk
	mov	x19, x0
	bl	_ls_variant_tag
	cbz	w0, LBB0_3
; %bb.1:                                ; %swn_3
	mov	x0, x19
	bl	_ls_variant_tag
	mov	x20, x0
	mov	x0, x19
	bl	_ls_variant_payload
	cmp	w20, #1
	b.ne	LBB0_4
; %bb.2:
	mov	w0, #1                          ; =0x1
LBB0_3:                                 ; %swm_1
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
LBB0_4:                                 ; %swf_5
	bl	_ls_abort
	brk	#0x1
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
