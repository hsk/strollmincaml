	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
## BB#0:                                ## %entry
	pushq	%rax
	movl	$1, %edi
	callq	_print_l
	xorl	%eax, %eax
	popq	%rdx
	ret


.subsections_via_symbols
