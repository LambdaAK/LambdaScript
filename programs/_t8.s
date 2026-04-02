	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_append__lsm928930968__ls_s0    ; -- Begin function append__lsm928930968__ls_s0
	.p2align	2
_append__lsm928930968__ls_s0:           ; @append__lsm928930968__ls_s0
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
	adrp	x0, _append__lsm928930968__ls_s1@PAGE
Lloh1:
	add	x0, x0, _append__lsm928930968__ls_s1@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	_append__lsm928930968__ls_s1    ; -- Begin function append__lsm928930968__ls_s1
	.p2align	2
_append__lsm928930968__ls_s1:           ; @append__lsm928930968__ls_s1
	.cfi_startproc
; %bb.0:                                ; %entry
	ldr	x0, [x0]
	b	_append__lsm928930968
	.cfi_endproc
                                        ; -- End function
	.globl	_append__lsm928930968           ; -- Begin function append__lsm928930968
	.p2align	2
_append__lsm928930968:                  ; @append__lsm928930968
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
	mov	x19, x1
	mov	x20, x0
	bl	_ls_variant_tag
	cbz	w0, LBB2_3
; %bb.1:                                ; %swn_3
	mov	x0, x20
	bl	_ls_variant_tag
	mov	x21, x0
	mov	x0, x20
	bl	_ls_variant_payload
	cmp	w21, #1
	b.ne	LBB2_4
; %bb.2:                                ; %swm_4
	ldr	w20, [x0]
	ldr	x0, [x0, #8]
	mov	x1, x19
	bl	_append__lsm928930968
	mov	x19, x0
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x1, x0
	str	w20, [x0]
	str	x19, [x0, #8]
	mov	w0, #1                          ; =0x1
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp], #48             ; 16-byte Folded Reload
	b	_ls_variant_mk
LBB2_3:                                 ; %common.ret
	mov	x0, x19
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp], #48             ; 16-byte Folded Reload
	ret
LBB2_4:                                 ; %swf_5
	bl	_ls_abort
	brk	#0x1
	.cfi_endproc
                                        ; -- End function
	.globl	_len__lsm348914431              ; -- Begin function len__lsm348914431
	.p2align	2
_len__lsm348914431:                     ; @len__lsm348914431
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
LBB3_1:                                 ; %tailrecurse
                                        ; =>This Inner Loop Header: Depth=1
	mov	x0, x20
	bl	_ls_variant_tag
	cbz	w0, LBB3_4
; %bb.2:                                ; %swn_3
                                        ;   in Loop: Header=BB3_1 Depth=1
	mov	x0, x20
	bl	_ls_variant_tag
	mov	x21, x0
	mov	x0, x20
	bl	_ls_variant_payload
	cmp	w21, #1
	b.ne	LBB3_5
; %bb.3:                                ; %swm_4
                                        ;   in Loop: Header=BB3_1 Depth=1
	ldr	x20, [x0, #8]
	add	w19, w19, #1
	b	LBB3_1
LBB3_4:                                 ; %swm_1
	mov	x0, x19
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp], #48             ; 16-byte Folded Reload
	ret
LBB3_5:                                 ; %swf_5
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
	mov	w0, #0                          ; =0x0
	mov	x1, #0                          ; =0x0
	bl	_ls_variant_mk
	mov	x19, x0
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x1, x0
	mov	w8, #2                          ; =0x2
	str	w8, [x0]
	str	x19, [x0, #8]
	mov	w20, #1                         ; =0x1
	mov	w0, #1                          ; =0x1
	bl	_ls_variant_mk
	mov	x19, x0
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x1, x0
	str	w20, [x0]
	str	x19, [x0, #8]
	mov	w0, #1                          ; =0x1
	bl	_ls_variant_mk
	mov	x1, x0
	bl	_append__lsm928930968
	bl	_len__lsm348914431
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
