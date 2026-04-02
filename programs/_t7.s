	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_get_or__lsm173919979__ls_s0    ; -- Begin function get_or__lsm173919979__ls_s0
	.p2align	2
_get_or__lsm173919979__ls_s0:           ; @get_or__lsm173919979__ls_s0
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
	mov	w0, #4                          ; =0x4
	bl	_ls_malloc
	mov	x1, x0
	str	w19, [x0]
Lloh0:
	adrp	x0, _get_or__lsm173919979__ls_s1@PAGE
Lloh1:
	add	x0, x0, _get_or__lsm173919979__ls_s1@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	_get_or__lsm173919979__ls_s1    ; -- Begin function get_or__lsm173919979__ls_s1
	.p2align	2
_get_or__lsm173919979__ls_s1:           ; @get_or__lsm173919979__ls_s1
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
	ldr	w19, [x0]
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
	ldr	w19, [x0]
LBB1_3:                                 ; %get_or__lsm173919979.exit
	mov	x0, x19
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
LBB1_4:                                 ; %swf_5.i
	bl	_ls_abort
	brk	#0x1
	.cfi_endproc
                                        ; -- End function
	.globl	_get_or__lsm173919979           ; -- Begin function get_or__lsm173919979
	.p2align	2
_get_or__lsm173919979:                  ; @get_or__lsm173919979
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
	ldr	w20, [x0]
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
	mov	w0, #0                          ; =0x0
	mov	x1, #0                          ; =0x0
	bl	_ls_variant_mk
	mov	x19, x0
	mov	w0, #4                          ; =0x4
	bl	_ls_malloc
	mov	x1, x0
	mov	w8, #10                         ; =0xa
	str	w8, [x0]
	mov	w0, #1                          ; =0x1
	bl	_ls_variant_mk
	mov	x20, x0
	mov	w0, #0                          ; =0x0
	mov	x1, #0                          ; =0x0
	bl	_ls_variant_mk
	mov	x0, x20
	bl	_ls_variant_tag
	cbz	w0, LBB3_3
; %bb.1:                                ; %swn_3.i
	mov	x0, x20
	bl	_ls_variant_tag
	mov	x21, x0
	mov	x0, x20
	bl	_ls_variant_payload
	cmp	w21, #1
	b.ne	LBB3_9
; %bb.2:
	ldr	w21, [x0]
	b	LBB3_4
LBB3_3:
	mov	w21, #0                         ; =0x0
LBB3_4:                                 ; %get_or__lsm173919979.exit
	mov	x0, x19
	bl	_ls_variant_tag
	cbz	w0, LBB3_7
; %bb.5:                                ; %swn_3.i3
	mov	x0, x19
	bl	_ls_variant_tag
	mov	x20, x0
	mov	x0, x19
	bl	_ls_variant_payload
	cmp	w20, #1
	b.ne	LBB3_9
; %bb.6:
	ldr	w8, [x0]
	b	LBB3_8
LBB3_7:
	mov	w8, #100                        ; =0x64
LBB3_8:                                 ; %get_or__lsm173919979.exit10
	add	w0, w8, w21
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp], #48             ; 16-byte Folded Reload
	ret
LBB3_9:                                 ; %swf_5.i
	bl	_ls_abort
	brk	#0x1
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
