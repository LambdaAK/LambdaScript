	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_f__lsm76640768                 ; -- Begin function f__lsm76640768
	.p2align	2
_f__lsm76640768:                        ; @f__lsm76640768
; %bb.0:                                ; %entry
	mov	x0, x1
	ret
                                        ; -- End function
	.globl	_ff__lsm966650245               ; -- Begin function ff__lsm966650245
	.p2align	2
_ff__lsm966650245:                      ; @ff__lsm966650245
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
	mov	w0, #2                          ; =0x2
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
