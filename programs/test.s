	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_foo__lsm768016045__ls_s0       ; -- Begin function foo__lsm768016045__ls_s0
	.p2align	2
_foo__lsm768016045__ls_s0:              ; @foo__lsm768016045__ls_s0
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
	adrp	x0, _foo__lsm768016045__ls_s1@PAGE
Lloh1:
	add	x0, x0, _foo__lsm768016045__ls_s1@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	_foo__lsm768016045__ls_s1       ; -- Begin function foo__lsm768016045__ls_s1
	.p2align	2
_foo__lsm768016045__ls_s1:              ; @foo__lsm768016045__ls_s1
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
	ldr	x8, [x20]
	stp	x8, x19, [x0]
Lloh2:
	adrp	x0, _foo__lsm768016045__ls_s2@PAGE
Lloh3:
	add	x0, x0, _foo__lsm768016045__ls_s2@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh2, Lloh3
	.cfi_endproc
                                        ; -- End function
	.globl	_foo__lsm768016045__ls_s2       ; -- Begin function foo__lsm768016045__ls_s2
	.p2align	2
_foo__lsm768016045__ls_s2:              ; @foo__lsm768016045__ls_s2
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
	mov	w0, #20                         ; =0x14
	bl	_ls_malloc
	mov	x1, x0
	ldr	x8, [x20]
	str	x8, [x0]
	ldr	x8, [x20, #8]
	str	x8, [x0, #8]
	str	w19, [x0, #16]
Lloh4:
	adrp	x0, _foo__lsm768016045__ls_s3@PAGE
Lloh5:
	add	x0, x0, _foo__lsm768016045__ls_s3@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh4, Lloh5
	.cfi_endproc
                                        ; -- End function
	.globl	_foo__lsm768016045__ls_s3       ; -- Begin function foo__lsm768016045__ls_s3
	.p2align	2
_foo__lsm768016045__ls_s3:              ; @foo__lsm768016045__ls_s3
	.cfi_startproc
; %bb.0:                                ; %entry
	sub	sp, sp, #48
	stp	x20, x19, [sp, #16]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #32]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 48
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	x19, x1
	ldp	x1, x8, [x0]
	str	x1, [sp, #8]                    ; 8-byte Folded Spill
	ldr	w1, [x0, #16]
	ldp	x9, x0, [x8]
	blr	x9
	ldp	x8, x0, [x0]
	mov	x1, x19
	blr	x8
	ldr	x1, [sp, #8]                    ; 8-byte Folded Reload
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #48
	br	x1
	.cfi_endproc
                                        ; -- End function
	.globl	_foo__lsm768016045              ; -- Begin function foo__lsm768016045
	.p2align	2
_foo__lsm768016045:                     ; @foo__lsm768016045
	.cfi_startproc
; %bb.0:                                ; %entry
	sub	sp, sp, #48
	stp	x20, x19, [sp, #16]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #32]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 48
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	x19, x3
	str	x0, [sp, #8]                    ; 8-byte Folded Spill
	ldp	x8, x0, [x1]
	mov	x1, x2
	blr	x8
	ldp	x8, x0, [x0]
	mov	x1, x19
	blr	x8
	ldr	x1, [sp, #8]                    ; 8-byte Folded Reload
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #48
	br	x1
	.cfi_endproc
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
Lloh6:
	adrp	x0, _add__ls_s1@PAGE
Lloh7:
	add	x0, x0, _add__ls_s1@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh6, Lloh7
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
	.globl	_inc                            ; -- Begin function inc
	.p2align	2
_inc:                                   ; @inc
; %bb.0:                                ; %entry
	add	w0, w0, #1
	ret
                                        ; -- End function
	.globl	_partially_applied__ls_s0       ; -- Begin function partially_applied__ls_s0
	.p2align	2
_partially_applied__ls_s0:              ; @partially_applied__ls_s0
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
Lloh8:
	adrp	x0, _partially_applied__ls_s1@PAGE
Lloh9:
	add	x0, x0, _partially_applied__ls_s1@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh8, Lloh9
	.cfi_endproc
                                        ; -- End function
	.globl	_partially_applied__ls_s1       ; -- Begin function partially_applied__ls_s1
	.p2align	2
_partially_applied__ls_s1:              ; @partially_applied__ls_s1
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
	ldr	w20, [x0]
Lloh10:
	adrp	x0, _add__ls_s0@PAGE
Lloh11:
	add	x0, x0, _add__ls_s0@PAGEOFF
	mov	x1, #0                          ; =0x0
	bl	_ls_mkclos
	ldp	x8, x0, [x0]
	mov	x1, x20
	blr	x8
	ldp	x8, x0, [x0]
	mov	x1, x19
	blr	x8
	add	w0, w0, #1
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.loh AdrpAdd	Lloh10, Lloh11
	.cfi_endproc
                                        ; -- End function
	.globl	_partially_applied              ; -- Begin function partially_applied
	.p2align	2
_partially_applied:                     ; @partially_applied
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
Lloh12:
	adrp	x0, _add__ls_s0@PAGE
Lloh13:
	add	x0, x0, _add__ls_s0@PAGEOFF
	mov	x1, #0                          ; =0x0
	bl	_ls_mkclos
	ldp	x8, x0, [x0]
	mov	x1, x20
	blr	x8
	ldp	x8, x0, [x0]
	mov	x1, x19
	blr	x8
	add	w0, w0, #1
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.loh AdrpAdd	Lloh12, Lloh13
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
Lloh14:
	adrp	x0, _add__ls_s0@PAGE
Lloh15:
	add	x0, x0, _add__ls_s0@PAGEOFF
	mov	x1, #0                          ; =0x0
	bl	_ls_mkclos
	ldp	x8, x0, [x0]
	mov	w1, #1                          ; =0x1
	blr	x8
	ldp	x8, x0, [x0]
	mov	w1, #2                          ; =0x2
	blr	x8
	add	w0, w0, #1
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.loh AdrpAdd	Lloh14, Lloh15
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
