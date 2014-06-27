	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
## BB#0:                                ## %entry
	pushq	%rax
	xorl	%edi, %edi
	movl	$10, %esi
	callq	_f
	movq	%rax, %rdi
	callq	_print
	xorl	%eax, %eax
	popq	%rdx
	ret

	.globl	_f
	.align	4, 0x90
_f:                                     ## @f
## BB#0:                                ## %entry
	cmpq	$1, %rsi
	jae	LBB1_2
## BB#1:                                ## %ok10
	movq	%rdi, %rax
	ret
LBB1_2:                                 ## %else11
	addq	%rsi, %rdi
	decq	%rsi
	jmp	_f                      ## TAILCALL

	.globl	_print
	.align	4, 0x90
_print:                                 ## @print
## BB#0:                                ## %entry
	pushq	%rax
	movq	%rdi, %rcx
	movq	%rcx, (%rsp)
	leaq	L_.str(%rip), %rdi
	xorl	%eax, %eax
	movq	%rcx, %rsi
	callq	_printf
	xorl	%eax, %eax
	popq	%rdx
	ret

	.globl	_create_array
	.align	4, 0x90
_create_array:                          ## @create_array
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%r14
Ltmp3:
	.cfi_def_cfa_offset 16
	pushq	%rbx
Ltmp4:
	.cfi_def_cfa_offset 24
	pushq	%rax
Ltmp5:
	.cfi_def_cfa_offset 32
Ltmp6:
	.cfi_offset %rbx, -24
Ltmp7:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	leaq	(,%rbx,8), %rdi
	callq	_malloc
	jmp	LBB3_1
	.align	4, 0x90
LBB3_2:                                 ## %body
                                        ##   in Loop: Header=BB3_1 Depth=1
	movq	%r14, -8(%rax,%rbx,8)
	decq	%rbx
LBB3_1:                                 ## %loop
                                        ## =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jg	LBB3_2
## BB#3:                                ## %end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	ret
	.cfi_endproc

	.section	__TEXT,__const
L_.str:                                 ## @.str
	.asciz	"%ld\n"


.subsections_via_symbols
