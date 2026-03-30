	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_add_three__ls_s0               ; -- Begin function add_three__ls_s0
	.p2align	2
_add_three__ls_s0:                      ; @add_three__ls_s0
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
	adrp	x0, _add_three__ls_s1@PAGE
Lloh1:
	add	x0, x0, _add_three__ls_s1@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	_add_three__ls_s1               ; -- Begin function add_three__ls_s1
	.p2align	2
_add_three__ls_s1:                      ; @add_three__ls_s1
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
	mov	w0, #8                          ; =0x8
	bl	_ls_malloc
	mov	x1, x0
	ldr	w8, [x20]
	stp	w8, w19, [x0]
Lloh2:
	adrp	x0, _add_three__ls_s2@PAGE
Lloh3:
	add	x0, x0, _add_three__ls_s2@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh2, Lloh3
	.cfi_endproc
                                        ; -- End function
	.globl	_add_three__ls_s2               ; -- Begin function add_three__ls_s2
	.p2align	2
_add_three__ls_s2:                      ; @add_three__ls_s2
; %bb.0:                                ; %entry
	ldp	w8, w9, [x0]
	add	w8, w8, w1
	add	w0, w8, w9
	ret
                                        ; -- End function
	.globl	_add_three                      ; -- Begin function add_three
	.p2align	2
_add_three:                             ; @add_three
; %bb.0:                                ; %entry
	add	w8, w1, w0
	add	w0, w8, w2
	ret
                                        ; -- End function
	.globl	_add__ls_s0                     ; -- Begin function add__ls_s0
	.p2align	2
_add__ls_s0:                            ; @add__ls_s0
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
Lloh4:
	adrp	x0, _add__ls_s1@PAGE
Lloh5:
	add	x0, x0, _add__ls_s1@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh4, Lloh5
	.cfi_endproc
                                        ; -- End function
	.globl	_add__ls_s1                     ; -- Begin function add__ls_s1
	.p2align	2
_add__ls_s1:                            ; @add__ls_s1
; %bb.0:                                ; %entry
	ldr	w8, [x0]
	add	w0, w8, w1
	ret
                                        ; -- End function
	.globl	_add                            ; -- Begin function add
	.p2align	2
_add:                                   ; @add
; %bb.0:                                ; %entry
	add	w0, w1, w0
	ret
                                        ; -- End function
	.globl	_apply__ls_s0                   ; -- Begin function apply__ls_s0
	.p2align	2
_apply__ls_s0:                          ; @apply__ls_s0
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
Lloh6:
	adrp	x0, _apply__ls_s1@PAGE
Lloh7:
	add	x0, x0, _apply__ls_s1@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh6, Lloh7
	.cfi_endproc
                                        ; -- End function
	.globl	_apply__ls_s1                   ; -- Begin function apply__ls_s1
	.p2align	2
_apply__ls_s1:                          ; @apply__ls_s1
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
	mov	w0, #12                         ; =0xc
	bl	_ls_malloc
	mov	x1, x0
	ldr	x8, [x20]
	str	x8, [x0]
	str	w19, [x0, #8]
Lloh8:
	adrp	x0, _apply__ls_s2@PAGE
Lloh9:
	add	x0, x0, _apply__ls_s2@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh8, Lloh9
	.cfi_endproc
                                        ; -- End function
	.globl	_apply__ls_s2                   ; -- Begin function apply__ls_s2
	.p2align	2
_apply__ls_s2:                          ; @apply__ls_s2
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
	ldr	x8, [x0]
	ldr	w1, [x0, #8]
	ldp	x9, x0, [x8]
	blr	x9
	ldp	x2, x0, [x0]
	mov	x1, x19
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	br	x2
	.cfi_endproc
                                        ; -- End function
	.globl	_apply                          ; -- Begin function apply
	.p2align	2
_apply:                                 ; @apply
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	x19, x2
	ldp	x8, x0, [x0]
	blr	x8
	ldp	x2, x0, [x0]
	mov	x1, x19
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	br	x2
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
	mov	w0, #4                          ; =0x4
	bl	_ls_malloc
	mov	x1, x0
	mov	w8, #1                          ; =0x1
	str	w8, [x0]
Lloh10:
	adrp	x0, _add_three__ls_s1@PAGE
Lloh11:
	add	x0, x0, _add_three__ls_s1@PAGEOFF
	bl	_ls_mkclos
	ldp	x8, x0, [x0]
	mov	w1, #2                          ; =0x2
	blr	x8
	ldp	x8, x0, [x0]
	mov	w1, #3                          ; =0x3
	blr	x8
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.loh AdrpAdd	Lloh10, Lloh11
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
