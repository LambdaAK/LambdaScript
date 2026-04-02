	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_unwrap_or__lsm627496531__ls_s0 ; -- Begin function unwrap_or__lsm627496531__ls_s0
	.p2align	2
_unwrap_or__lsm627496531__ls_s0:        ; @unwrap_or__lsm627496531__ls_s0
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	x19, x1
	mov	w0, #8                          ; =0x8
	bl	_ls_malloc
	mov	x1, x0
	str	x19, [x0]
Lloh0:
	adrp	x0, _unwrap_or__lsm627496531__ls_s1@PAGE
Lloh1:
	add	x0, x0, _unwrap_or__lsm627496531__ls_s1@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	_unwrap_or__lsm627496531__ls_s1 ; -- Begin function unwrap_or__lsm627496531__ls_s1
	.p2align	2
_unwrap_or__lsm627496531__ls_s1:        ; @unwrap_or__lsm627496531__ls_s1
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	x20, x1
	ldr	x19, [x0]
	mov	x0, x1
	bl	_ls_variant_tag
	cbz	w0, LBB1_3
; %bb.1:                                ; %swn_3.i
	mov	x0, x20
	bl	_ls_variant_tag
	mov	x19, x0
	mov	x0, x20
	bl	_ls_variant_payload
	cmp	w19, #1
	b.ne	LBB1_4
; %bb.2:
	ldr	x19, [x0]
LBB1_3:                                 ; %unwrap_or__lsm627496531.exit
	mov	x0, x19
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
LBB1_4:                                 ; %swf_5.i
	bl	_ls_abort
	brk	#0x1
	.cfi_endproc
                                        ; -- End function
	.globl	_unwrap_or__lsm627496531        ; -- Begin function unwrap_or__lsm627496531
	.p2align	2
_unwrap_or__lsm627496531:               ; @unwrap_or__lsm627496531
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	x19, x1
	mov	x20, x0
	mov	x0, x1
	bl	_ls_variant_tag
	cbz	w0, LBB2_3
; %bb.1:                                ; %swn_3
	mov	x0, x19
	bl	_ls_variant_tag
	mov	x20, x0
	mov	x0, x19
	bl	_ls_variant_payload
	cmp	w20, #1
	b.ne	LBB2_4
; %bb.2:
	ldr	x20, [x0]
LBB2_3:                                 ; %swm_1
	mov	x0, x20
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
LBB2_4:                                 ; %swf_5
	bl	_ls_abort
	brk	#0x1
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
	mov	w0, #4                          ; =0x4
	bl	_ls_malloc
	mov	x1, x0
	mov	w8, #7                          ; =0x7
	str	w8, [x0]
	mov	w0, #1                          ; =0x1
	bl	_ls_variant_mk
	mov	x19, x0
	mov	w0, #8                          ; =0x8
	bl	_ls_malloc
	mov	x1, x0
	str	x19, [x0]
	mov	w0, #1                          ; =0x1
	bl	_ls_variant_mk
	mov	x19, x0
	bl	_ls_variant_tag
	cbz	w0, LBB3_2
; %bb.1:                                ; %swn_3.i
	mov	x0, x19
	bl	_ls_variant_tag
	mov	x20, x0
	mov	x0, x19
	bl	_ls_variant_payload
	cmp	w20, #1
	b.ne	LBB3_3
LBB3_2:                                 ; %unwrap_or__lsm627496531.exit
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
LBB3_3:                                 ; %swf_5.i
	bl	_ls_abort
	brk	#0x1
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
