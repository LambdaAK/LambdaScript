	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_apply                          ; -- Begin function apply
	.p2align	2
_apply:                                 ; @apply
	.cfi_startproc
; %bb.0:                                ; %entry
	mov	x2, x0
	mov	x0, x1
	br	x2
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
	mov	w0, #42                         ; =0x2a
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
