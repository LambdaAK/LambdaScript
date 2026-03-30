	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_apply__lsm509404921__ls_s0     ; -- Begin function apply__lsm509404921__ls_s0
	.p2align	2
_apply__lsm509404921__ls_s0:            ; @apply__lsm509404921__ls_s0
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
	adrp	x0, _apply__lsm509404921__ls_s1@PAGE
Lloh1:
	add	x0, x0, _apply__lsm509404921__ls_s1@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	_apply__lsm509404921__ls_s1     ; -- Begin function apply__lsm509404921__ls_s1
	.p2align	2
_apply__lsm509404921__ls_s1:            ; @apply__lsm509404921__ls_s1
	.cfi_startproc
; %bb.0:                                ; %entry
	ldr	x2, [x0]
	mov	x0, x1
	br	x2
	.cfi_endproc
                                        ; -- End function
	.globl	_apply__lsm509404921            ; -- Begin function apply__lsm509404921
	.p2align	2
_apply__lsm509404921:                   ; @apply__lsm509404921
	.cfi_startproc
; %bb.0:                                ; %entry
	mov	x2, x0
	mov	x0, x1
	br	x2
	.cfi_endproc
                                        ; -- End function
	.globl	_square                         ; -- Begin function square
	.p2align	2
_square:                                ; @square
; %bb.0:                                ; %entry
	mul	w0, w0, w0
	ret
                                        ; -- End function
	.globl	_h                              ; -- Begin function h
	.p2align	2
_h:                                     ; @h
; %bb.0:                                ; %entry
	mul	w0, w0, w0
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
	mov	w0, #25                         ; =0x19
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
