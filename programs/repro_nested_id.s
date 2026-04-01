	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_id__lsm76640768__ls_s0         ; -- Begin function id__lsm76640768__ls_s0
	.p2align	2
_id__lsm76640768__ls_s0:                ; @id__lsm76640768__ls_s0
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
	adrp	x0, _id__lsm76640768__ls_s1@PAGE
Lloh1:
	add	x0, x0, _id__lsm76640768__ls_s1@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	_id__lsm76640768__ls_s1         ; -- Begin function id__lsm76640768__ls_s1
	.p2align	2
_id__lsm76640768__ls_s1:                ; @id__lsm76640768__ls_s1
; %bb.0:                                ; %entry
	ldr	w0, [x0]
	ret
                                        ; -- End function
	.globl	_id__lsm76640768                ; -- Begin function id__lsm76640768
	.p2align	2
_id__lsm76640768:                       ; @id__lsm76640768
; %bb.0:                                ; %entry
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
	mov	w0, #1                          ; =0x1
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
