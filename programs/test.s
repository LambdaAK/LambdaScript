	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_extend_list__lsm592479740      ; -- Begin function extend_list__lsm592479740
	.p2align	2
_extend_list__lsm592479740:             ; @extend_list__lsm592479740
	.cfi_startproc
; %bb.0:                                ; %entry
	cbz	x0, LBB0_2
; %bb.1:                                ; %swn_3
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
	ldr	w21, [x0]
	ldr	x0, [x0, #8]
	bl	_extend_list__lsm592479740
	mov	x19, x0
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x20, x0
	str	w21, [x0]
	str	x19, [x0, #8]
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	str	w21, [x0]
	str	x20, [x0, #8]
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp], #48             ; 16-byte Folded Reload
LBB0_2:                                 ; %common.ret
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_id__lsm966650245               ; -- Begin function id__lsm966650245
	.p2align	2
_id__lsm966650245:                      ; @id__lsm966650245
; %bb.0:                                ; %entry
	ret
                                        ; -- End function
	.globl	_map__lsm495454961__ls_s0       ; -- Begin function map__lsm495454961__ls_s0
	.p2align	2
_map__lsm495454961__ls_s0:              ; @map__lsm495454961__ls_s0
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
	adrp	x0, _map__lsm495454961__ls_s1@PAGE
Lloh1:
	add	x0, x0, _map__lsm495454961__ls_s1@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	b	_ls_mkclos
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	_map__lsm495454961__ls_s1       ; -- Begin function map__lsm495454961__ls_s1
	.p2align	2
_map__lsm495454961__ls_s1:              ; @map__lsm495454961__ls_s1
	.cfi_startproc
; %bb.0:                                ; %entry
	ldr	x0, [x0]
	b	_map__lsm495454961
	.cfi_endproc
                                        ; -- End function
	.globl	_map__lsm495454961              ; -- Begin function map__lsm495454961
	.p2align	2
_map__lsm495454961:                     ; @map__lsm495454961
	.cfi_startproc
; %bb.0:                                ; %entry
	cbz	x1, LBB4_2
; %bb.1:                                ; %swn_3
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
	mov	x19, x0
	ldr	w0, [x1]
	ldr	x20, [x1, #8]
	blr	x19
	mov	x21, x0
	mov	x0, x19
	mov	x1, x20
	bl	_map__lsm495454961
	mov	x19, x0
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	str	w21, [x0]
	str	x19, [x0, #8]
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp], #48             ; 16-byte Folded Reload
	ret
LBB4_2:
	mov	x0, #0                          ; =0x0
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_print_list                     ; -- Begin function print_list
	.p2align	2
_print_list:                            ; @print_list
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	x19, x0
	cbz	x19, LBB5_2
LBB5_1:                                 ; %swn_3
                                        ; =>This Inner Loop Header: Depth=1
	ldr	w0, [x19]
	ldr	x19, [x19, #8]
	bl	_ls_int_to_str
	bl	_ls_println
	cbnz	x19, LBB5_1
LBB5_2:                                 ; %swm_1
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
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
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x19, x0
	mov	w8, #5                          ; =0x5
	str	w8, [x0]
	str	xzr, [x0, #8]
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x20, x0
	mov	w8, #4                          ; =0x4
	str	w8, [x0]
	str	x19, [x0, #8]
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x19, x0
	mov	w8, #3                          ; =0x3
	str	w8, [x0]
	str	x20, [x0, #8]
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x20, x0
	mov	w8, #2                          ; =0x2
	str	w8, [x0]
	str	x19, [x0, #8]
	mov	w0, #16                         ; =0x10
	bl	_ls_malloc
	mov	x1, x0
	mov	w8, #1                          ; =0x1
	str	w8, [x0]
	str	x20, [x0, #8]
Lloh2:
	adrp	x0, _id__lsm966650245@PAGE
Lloh3:
	add	x0, x0, _id__lsm966650245@PAGEOFF
	bl	_map__lsm495454961
	bl	_extend_list__lsm592479740
	bl	_print_list
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.loh AdrpAdd	Lloh2, Lloh3
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
