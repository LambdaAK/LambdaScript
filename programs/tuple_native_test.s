	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_tuple_fst__lsm963247424        ; -- Begin function tuple_fst__lsm963247424
	.p2align	2
_tuple_fst__lsm963247424:               ; @tuple_fst__lsm963247424
; %bb.0:                                ; %entry
	ret
                                        ; -- End function
	.globl	_tuple_snd__lsm963247424        ; -- Begin function tuple_snd__lsm963247424
	.p2align	2
_tuple_snd__lsm963247424:               ; @tuple_snd__lsm963247424
; %bb.0:                                ; %entry
	mov	x0, x1
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
	mov	w0, #5                          ; =0x5
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #10                         ; =0xa
	bl	_ls_int_to_str
	bl	_ls_println
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
