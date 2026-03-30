	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_deep__lsm385162459__ls_s0      ; -- Begin function deep__lsm385162459__ls_s0
	.p2align	2
_deep__lsm385162459__ls_s0:             ; @deep__lsm385162459__ls_s0
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
	adrp	x0, _deep__lsm385162459__ls_s1@PAGE
Lloh1:
	add	x0, x0, _deep__lsm385162459__ls_s1@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	_deep__lsm385162459__ls_s1      ; -- Begin function deep__lsm385162459__ls_s1
	.p2align	2
_deep__lsm385162459__ls_s1:             ; @deep__lsm385162459__ls_s1
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
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x1, x0
	ldr	w8, [x20]
	str	w8, [x0]
	str	x19, [x0, #8]
Lloh2:
	adrp	x0, _deep__lsm385162459__ls_s2@PAGE
Lloh3:
	add	x0, x0, _deep__lsm385162459__ls_s2@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh2, Lloh3
	.cfi_endproc
                                        ; -- End function
	.globl	_deep__lsm385162459__ls_s2      ; -- Begin function deep__lsm385162459__ls_s2
	.p2align	2
_deep__lsm385162459__ls_s2:             ; @deep__lsm385162459__ls_s2
	.cfi_startproc
; %bb.0:                                ; %entry
	mov	x2, x1
	ldr	w8, [x0]
	ldr	x1, [x0, #8]
	mov	x0, x8
	b	_deep__lsm385162459
	.cfi_endproc
                                        ; -- End function
	.globl	_deep__lsm385162459             ; -- Begin function deep__lsm385162459
	.p2align	2
_deep__lsm385162459:                    ; @deep__lsm385162459
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
	cbz	w20, LBB3_2
LBB3_1:                                 ; %else_2
                                        ; =>This Inner Loop Header: Depth=1
	sub	w20, w20, #1
	mov	x0, x2
	blr	x19
	mov	x2, x0
	cbnz	w20, LBB3_1
LBB3_2:                                 ; %merge_3
	mov	x0, x2
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_inc                            ; -- Begin function inc
	.p2align	2
_inc:                                   ; @inc
; %bb.0:                                ; %entry
	add	w0, w0, #1
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
Lloh4:
	adrp	x1, _inc@PAGE
Lloh5:
	add	x1, x1, _inc@PAGEOFF
	mov	w0, #4                          ; =0x4
	mov	w2, #0                          ; =0x0
	bl	_deep__lsm385162459
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.loh AdrpAdd	Lloh4, Lloh5
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
