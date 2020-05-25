	.text
	.file	"c.c"
	.globl	clamp                   # -- Begin function clamp
	.p2align	4, 0x90
	.type	clamp,@function
clamp:                                  # @clamp
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movsd	%xmm0, -16(%rbp)
	movsd	%xmm1, -24(%rbp)
	movsd	%xmm2, -32(%rbp)
	movsd	-16(%rbp), %xmm0        # xmm0 = mem[0],zero
	movsd	-24(%rbp), %xmm1        # xmm1 = mem[0],zero
	ucomisd	%xmm0, %xmm1
	jbe	.LBB0_2
# %bb.1:
	movsd	-24(%rbp), %xmm0        # xmm0 = mem[0],zero
	movsd	%xmm0, -8(%rbp)
	jmp	.LBB0_5
.LBB0_2:
	movsd	-32(%rbp), %xmm0        # xmm0 = mem[0],zero
	movsd	-16(%rbp), %xmm1        # xmm1 = mem[0],zero
	ucomisd	%xmm0, %xmm1
	jbe	.LBB0_4
# %bb.3:
	movsd	-32(%rbp), %xmm0        # xmm0 = mem[0],zero
	movsd	%xmm0, -8(%rbp)
	jmp	.LBB0_5
.LBB0_4:
	movsd	-16(%rbp), %xmm0        # xmm0 = mem[0],zero
	movsd	%xmm0, -8(%rbp)
.LBB0_5:
	movsd	-8(%rbp), %xmm0         # xmm0 = mem[0],zero
	popq	%rbp
	retq
.Lfunc_end0:
	.size	clamp, .Lfunc_end0-clamp
	.cfi_endproc
                                        # -- End function
	.globl	Vec_new                 # -- Begin function Vec_new
	.p2align	4, 0x90
	.type	Vec_new,@function
Vec_new:                                # @Vec_new
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movl	$24, %eax
	movl	%eax, %edi
	movsd	%xmm0, -8(%rbp)
	movsd	%xmm1, -16(%rbp)
	movsd	%xmm2, -24(%rbp)
	callq	malloc
	movq	%rax, -32(%rbp)
	movsd	-8(%rbp), %xmm0         # xmm0 = mem[0],zero
	movq	-32(%rbp), %rax
	movsd	%xmm0, (%rax)
	movsd	-16(%rbp), %xmm0        # xmm0 = mem[0],zero
	movq	-32(%rbp), %rax
	movsd	%xmm0, 8(%rax)
	movsd	-24(%rbp), %xmm0        # xmm0 = mem[0],zero
	movq	-32(%rbp), %rax
	movsd	%xmm0, 16(%rax)
	movq	-32(%rbp), %rax
	addq	$32, %rsp
	popq	%rbp
	retq
.Lfunc_end1:
	.size	Vec_new, .Lfunc_end1-Vec_new
	.cfi_endproc
                                        # -- End function
	.globl	Vec_add                 # -- Begin function Vec_add
	.p2align	4, 0x90
	.type	Vec_add,@function
Vec_add:                                # @Vec_add
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rsi
	movsd	(%rsi), %xmm0           # xmm0 = mem[0],zero
	movq	-16(%rbp), %rsi
	addsd	(%rsi), %xmm0
	movq	-8(%rbp), %rsi
	movsd	8(%rsi), %xmm1          # xmm1 = mem[0],zero
	movq	-16(%rbp), %rsi
	addsd	8(%rsi), %xmm1
	movq	-8(%rbp), %rsi
	movsd	16(%rsi), %xmm2         # xmm2 = mem[0],zero
	movq	-16(%rbp), %rsi
	addsd	16(%rsi), %xmm2
	callq	Vec_new
	addq	$16, %rsp
	popq	%rbp
	retq
.Lfunc_end2:
	.size	Vec_add, .Lfunc_end2-Vec_add
	.cfi_endproc
                                        # -- End function
	.globl	Vec_sub                 # -- Begin function Vec_sub
	.p2align	4, 0x90
	.type	Vec_sub,@function
Vec_sub:                                # @Vec_sub
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rsi
	movsd	(%rsi), %xmm0           # xmm0 = mem[0],zero
	movq	-16(%rbp), %rsi
	subsd	(%rsi), %xmm0
	movq	-8(%rbp), %rsi
	movsd	8(%rsi), %xmm1          # xmm1 = mem[0],zero
	movq	-16(%rbp), %rsi
	subsd	8(%rsi), %xmm1
	movq	-8(%rbp), %rsi
	movsd	16(%rsi), %xmm2         # xmm2 = mem[0],zero
	movq	-16(%rbp), %rsi
	subsd	16(%rsi), %xmm2
	callq	Vec_new
	addq	$16, %rsp
	popq	%rbp
	retq
.Lfunc_end3:
	.size	Vec_sub, .Lfunc_end3-Vec_sub
	.cfi_endproc
                                        # -- End function
	.globl	Vec_mul                 # -- Begin function Vec_mul
	.p2align	4, 0x90
	.type	Vec_mul,@function
Vec_mul:                                # @Vec_mul
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movsd	%xmm0, -16(%rbp)
	movq	-8(%rbp), %rdi
	movsd	(%rdi), %xmm0           # xmm0 = mem[0],zero
	mulsd	-16(%rbp), %xmm0
	movq	-8(%rbp), %rdi
	movsd	8(%rdi), %xmm1          # xmm1 = mem[0],zero
	mulsd	-16(%rbp), %xmm1
	movq	-8(%rbp), %rdi
	movsd	16(%rdi), %xmm2         # xmm2 = mem[0],zero
	mulsd	-16(%rbp), %xmm2
	callq	Vec_new
	addq	$16, %rsp
	popq	%rbp
	retq
.Lfunc_end4:
	.size	Vec_mul, .Lfunc_end4-Vec_mul
	.cfi_endproc
                                        # -- End function
	.globl	Vec_multi               # -- Begin function Vec_multi
	.p2align	4, 0x90
	.type	Vec_multi,@function
Vec_multi:                              # @Vec_multi
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rsi
	movsd	(%rsi), %xmm0           # xmm0 = mem[0],zero
	movq	-16(%rbp), %rsi
	mulsd	(%rsi), %xmm0
	movq	-8(%rbp), %rsi
	movsd	8(%rsi), %xmm1          # xmm1 = mem[0],zero
	movq	-16(%rbp), %rsi
	mulsd	8(%rsi), %xmm1
	movq	-8(%rbp), %rsi
	movsd	16(%rsi), %xmm2         # xmm2 = mem[0],zero
	movq	-16(%rbp), %rsi
	mulsd	16(%rsi), %xmm2
	callq	Vec_new
	addq	$16, %rsp
	popq	%rbp
	retq
.Lfunc_end5:
	.size	Vec_multi, .Lfunc_end5-Vec_multi
	.cfi_endproc
                                        # -- End function
	.globl	Vec_dot                 # -- Begin function Vec_dot
	.p2align	4, 0x90
	.type	Vec_dot,@function
Vec_dot:                                # @Vec_dot
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rsi
	movsd	(%rsi), %xmm0           # xmm0 = mem[0],zero
	movq	-16(%rbp), %rsi
	mulsd	(%rsi), %xmm0
	movq	-8(%rbp), %rsi
	movsd	8(%rsi), %xmm1          # xmm1 = mem[0],zero
	movq	-16(%rbp), %rsi
	mulsd	8(%rsi), %xmm1
	addsd	%xmm1, %xmm0
	movq	-8(%rbp), %rsi
	movsd	16(%rsi), %xmm1         # xmm1 = mem[0],zero
	movq	-16(%rbp), %rsi
	mulsd	16(%rsi), %xmm1
	addsd	%xmm1, %xmm0
	popq	%rbp
	retq
.Lfunc_end6:
	.size	Vec_dot, .Lfunc_end6-Vec_dot
	.cfi_endproc
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function Vec_reflect
.LCPI7_0:
	.quad	-4611686018427387904    # double -2
	.text
	.globl	Vec_reflect
	.p2align	4, 0x90
	.type	Vec_reflect,@function
Vec_reflect:                            # @Vec_reflect
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-8(%rbp), %rdi
	movq	-16(%rbp), %rsi
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rcx
	movq	%rdi, -24(%rbp)         # 8-byte Spill
	movq	%rax, %rdi
	movq	%rsi, -32(%rbp)         # 8-byte Spill
	movq	%rcx, %rsi
	callq	Vec_dot
	movsd	.LCPI7_0(%rip), %xmm1   # xmm1 = mem[0],zero
	mulsd	%xmm0, %xmm1
	movq	-32(%rbp), %rdi         # 8-byte Reload
	movaps	%xmm1, %xmm0
	callq	Vec_mul
	movq	-24(%rbp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	Vec_add
	addq	$32, %rsp
	popq	%rbp
	retq
.Lfunc_end7:
	.size	Vec_reflect, .Lfunc_end7-Vec_reflect
	.cfi_endproc
                                        # -- End function
	.globl	Vec_length              # -- Begin function Vec_length
	.p2align	4, 0x90
	.type	Vec_length,@function
Vec_length:                             # @Vec_length
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	movsd	(%rdi), %xmm0           # xmm0 = mem[0],zero
	movq	-8(%rbp), %rdi
	mulsd	(%rdi), %xmm0
	movq	-8(%rbp), %rdi
	movsd	8(%rdi), %xmm1          # xmm1 = mem[0],zero
	movq	-8(%rbp), %rdi
	mulsd	8(%rdi), %xmm1
	addsd	%xmm1, %xmm0
	movq	-8(%rbp), %rdi
	movsd	16(%rdi), %xmm1         # xmm1 = mem[0],zero
	movq	-8(%rbp), %rdi
	mulsd	16(%rdi), %xmm1
	addsd	%xmm1, %xmm0
	callq	sqrt
	addq	$16, %rsp
	popq	%rbp
	retq
.Lfunc_end8:
	.size	Vec_length, .Lfunc_end8-Vec_length
	.cfi_endproc
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function Vec_normalize
.LCPI9_0:
	.quad	4487126258331716666     # double 1.0E-8
.LCPI9_1:
	.quad	4607182418800017408     # double 1
	.text
	.globl	Vec_normalize
	.p2align	4, 0x90
	.type	Vec_normalize,@function
Vec_normalize:                          # @Vec_normalize
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	Vec_length
	movsd	.LCPI9_0(%rip), %xmm1   # xmm1 = mem[0],zero
	movsd	%xmm0, -16(%rbp)
	movsd	-16(%rbp), %xmm0        # xmm0 = mem[0],zero
	ucomisd	%xmm1, %xmm0
	jbe	.LBB9_2
# %bb.1:
	movsd	.LCPI9_1(%rip), %xmm0   # xmm0 = mem[0],zero
	divsd	-16(%rbp), %xmm0
	movsd	%xmm0, -24(%rbp)
	movq	-8(%rbp), %rax
	movsd	(%rax), %xmm0           # xmm0 = mem[0],zero
	mulsd	-24(%rbp), %xmm0
	movq	-8(%rbp), %rax
	movsd	%xmm0, (%rax)
	movq	-8(%rbp), %rax
	movsd	8(%rax), %xmm0          # xmm0 = mem[0],zero
	mulsd	-24(%rbp), %xmm0
	movq	-8(%rbp), %rax
	movsd	%xmm0, 8(%rax)
	movq	-8(%rbp), %rax
	movsd	16(%rax), %xmm0         # xmm0 = mem[0],zero
	mulsd	-24(%rbp), %xmm0
	movq	-8(%rbp), %rax
	movsd	%xmm0, 16(%rax)
.LBB9_2:
	movq	-8(%rbp), %rax
	addq	$32, %rsp
	popq	%rbp
	retq
.Lfunc_end9:
	.size	Vec_normalize, .Lfunc_end9-Vec_normalize
	.cfi_endproc
                                        # -- End function
	.globl	Ray_new                 # -- Begin function Ray_new
	.p2align	4, 0x90
	.type	Ray_new,@function
Ray_new:                                # @Ray_new
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movl	$16, %eax
	movl	%eax, %ecx
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rcx, %rdi
	callq	malloc
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movq	-24(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-16(%rbp), %rax
	movq	-24(%rbp), %rcx
	movq	%rax, 8(%rcx)
	movq	-24(%rbp), %rax
	addq	$32, %rsp
	popq	%rbp
	retq
.Lfunc_end10:
	.size	Ray_new, .Lfunc_end10-Ray_new
	.cfi_endproc
                                        # -- End function
	.globl	Isect_new               # -- Begin function Isect_new
	.p2align	4, 0x90
	.type	Isect_new,@function
Isect_new:                              # @Isect_new
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$64, %rsp
	movl	$48, %eax
	movl	%eax, %r9d
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movsd	%xmm0, -40(%rbp)
	movq	%r8, -48(%rbp)
	movq	%r9, %rdi
	callq	malloc
	movq	%rax, -56(%rbp)
	movl	-4(%rbp), %r10d
	movq	-56(%rbp), %rax
	movl	%r10d, (%rax)
	movq	-16(%rbp), %rax
	movq	-56(%rbp), %rcx
	movq	%rax, 8(%rcx)
	movq	-24(%rbp), %rax
	movq	-56(%rbp), %rcx
	movq	%rax, 16(%rcx)
	movq	-32(%rbp), %rax
	movq	-56(%rbp), %rcx
	movq	%rax, 24(%rcx)
	movsd	-40(%rbp), %xmm0        # xmm0 = mem[0],zero
	movq	-56(%rbp), %rax
	movsd	%xmm0, 32(%rax)
	movq	-48(%rbp), %rax
	movq	-56(%rbp), %rcx
	movq	%rax, 40(%rcx)
	movq	-56(%rbp), %rax
	addq	$64, %rsp
	popq	%rbp
	retq
.Lfunc_end11:
	.size	Isect_new, .Lfunc_end11-Isect_new
	.cfi_endproc
                                        # -- End function
	.globl	Sphere_new              # -- Begin function Sphere_new
	.p2align	4, 0x90
	.type	Sphere_new,@function
Sphere_new:                             # @Sphere_new
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movl	$24, %eax
	movl	%eax, %ecx
	movsd	%xmm0, -8(%rbp)
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
	movq	%rcx, %rdi
	callq	malloc
	movq	%rax, -32(%rbp)
	movsd	-8(%rbp), %xmm0         # xmm0 = mem[0],zero
	movq	-32(%rbp), %rax
	movsd	%xmm0, (%rax)
	movq	-16(%rbp), %rax
	movq	-32(%rbp), %rcx
	movq	%rax, 8(%rcx)
	movq	-24(%rbp), %rax
	movq	-32(%rbp), %rcx
	movq	%rax, 16(%rcx)
	movq	-32(%rbp), %rax
	addq	$32, %rsp
	popq	%rbp
	retq
.Lfunc_end12:
	.size	Sphere_new, .Lfunc_end12-Sphere_new
	.cfi_endproc
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function Sphere_intersect
.LCPI13_0:
	.quad	4547007122018943789     # double 1.0E-4
.LCPI13_1:
	.quad	4591870180066957722     # double 0.10000000000000001
.LCPI13_2:
	.quad	4607182418800017408     # double 1
	.text
	.globl	Sphere_intersect
	.p2align	4, 0x90
	.type	Sphere_intersect,@function
Sphere_intersect:                       # @Sphere_intersect
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$112, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	-24(%rbp), %rcx
	movq	(%rcx), %rdi
	movq	-8(%rbp), %rcx
	movq	8(%rcx), %rsi
	callq	Vec_sub
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdi
	movq	-24(%rbp), %rax
	movq	8(%rax), %rsi
	callq	Vec_dot
	movsd	%xmm0, -48(%rbp)
	movq	-40(%rbp), %rdi
	movq	-40(%rbp), %rsi
	callq	Vec_dot
	xorps	%xmm1, %xmm1
	movq	-8(%rbp), %rax
	movsd	(%rax), %xmm2           # xmm2 = mem[0],zero
	movq	-8(%rbp), %rax
	mulsd	(%rax), %xmm2
	subsd	%xmm2, %xmm0
	movsd	%xmm0, -56(%rbp)
	movsd	-48(%rbp), %xmm0        # xmm0 = mem[0],zero
	mulsd	-48(%rbp), %xmm0
	subsd	-56(%rbp), %xmm0
	movsd	%xmm0, -64(%rbp)
	movaps	%xmm1, %xmm0
	subsd	-48(%rbp), %xmm0
	movsd	-64(%rbp), %xmm2        # xmm2 = mem[0],zero
	movsd	%xmm0, -80(%rbp)        # 8-byte Spill
	movaps	%xmm2, %xmm0
	movsd	%xmm1, -88(%rbp)        # 8-byte Spill
	callq	sqrt
	movsd	-80(%rbp), %xmm1        # 8-byte Reload
                                        # xmm1 = mem[0],zero
	subsd	%xmm0, %xmm1
	movsd	%xmm1, -72(%rbp)
	movsd	-64(%rbp), %xmm0        # xmm0 = mem[0],zero
	movsd	-88(%rbp), %xmm1        # 8-byte Reload
                                        # xmm1 = mem[0],zero
	ucomisd	%xmm1, %xmm0
	jbe	.LBB13_4
# %bb.1:
	movsd	.LCPI13_0(%rip), %xmm0  # xmm0 = mem[0],zero
	movsd	-72(%rbp), %xmm1        # xmm1 = mem[0],zero
	ucomisd	%xmm0, %xmm1
	jbe	.LBB13_4
# %bb.2:
	movsd	-72(%rbp), %xmm0        # xmm0 = mem[0],zero
	movq	-32(%rbp), %rax
	movsd	32(%rax), %xmm1         # xmm1 = mem[0],zero
	ucomisd	%xmm0, %xmm1
	jbe	.LBB13_4
# %bb.3:
	movq	-24(%rbp), %rax
	movq	(%rax), %rdi
	movq	-24(%rbp), %rax
	movq	8(%rax), %rax
	movsd	-72(%rbp), %xmm0        # xmm0 = mem[0],zero
	movq	%rdi, -96(%rbp)         # 8-byte Spill
	movq	%rax, %rdi
	callq	Vec_mul
	movq	-96(%rbp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	Vec_add
	movq	-32(%rbp), %rsi
	movq	%rax, 8(%rsi)
	movq	-32(%rbp), %rax
	movq	8(%rax), %rdi
	movq	-8(%rbp), %rax
	movq	8(%rax), %rsi
	callq	Vec_sub
	movq	%rax, %rdi
	callq	Vec_normalize
	movq	-32(%rbp), %rsi
	movq	%rax, 16(%rsi)
	movq	-8(%rbp), %rax
	movq	16(%rax), %rdi
	movq	-16(%rbp), %rax
	movq	-32(%rbp), %rsi
	movq	16(%rsi), %rsi
	movq	%rdi, -104(%rbp)        # 8-byte Spill
	movq	%rax, %rdi
	callq	Vec_dot
	movsd	.LCPI13_1(%rip), %xmm1  # xmm1 = mem[0],zero
	movsd	.LCPI13_2(%rip), %xmm2  # xmm2 = mem[0],zero
	callq	clamp
	movq	-104(%rbp), %rdi        # 8-byte Reload
	callq	Vec_mul
	movq	-32(%rbp), %rsi
	movq	%rax, 24(%rsi)
	movsd	-72(%rbp), %xmm0        # xmm0 = mem[0],zero
	movq	-32(%rbp), %rax
	movsd	%xmm0, 32(%rax)
	movq	-32(%rbp), %rax
	movl	(%rax), %ecx
	addl	$1, %ecx
	movq	-32(%rbp), %rax
	movl	%ecx, (%rax)
	movq	-24(%rbp), %rax
	movq	8(%rax), %rax
	movq	-32(%rbp), %rsi
	movq	%rax, 40(%rsi)
.LBB13_4:
	xorl	%eax, %eax
	addq	$112, %rsp
	popq	%rbp
	retq
.Lfunc_end13:
	.size	Sphere_intersect, .Lfunc_end13-Sphere_intersect
	.cfi_endproc
                                        # -- End function
	.globl	Plane_new               # -- Begin function Plane_new
	.p2align	4, 0x90
	.type	Plane_new,@function
Plane_new:                              # @Plane_new
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movl	$24, %eax
	movl	%eax, %ecx
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, %rdi
	callq	malloc
	movq	%rax, -32(%rbp)
	movq	-8(%rbp), %rax
	movq	-32(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-16(%rbp), %rax
	movq	-32(%rbp), %rcx
	movq	%rax, 8(%rcx)
	movq	-24(%rbp), %rax
	movq	-32(%rbp), %rcx
	movq	%rax, 16(%rcx)
	movq	-32(%rbp), %rax
	addq	$32, %rsp
	popq	%rbp
	retq
.Lfunc_end14:
	.size	Plane_new, .Lfunc_end14-Plane_new
	.cfi_endproc
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function Plane_intersect
.LCPI15_0:
	.quad	4547007122018943789     # double 1.0E-4
.LCPI15_1:
	.quad	4607182418800017408     # double 1
.LCPI15_2:
	.quad	4611686018427387904     # double 2
.LCPI15_3:
	.quad	4591870180066957722     # double 0.10000000000000001
.LCPI15_4:
	.quad	4602678819172646912     # double 0.5
.LCPI15_5:
	.quad	4627730092099895296     # double 25
.LCPI15_7:
	.quad	4585925428558828667     # double 0.040000000000000001
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4
.LCPI15_6:
	.quad	9223372036854775807     # double NaN
	.quad	9223372036854775807     # double NaN
	.text
	.globl	Plane_intersect
	.p2align	4, 0x90
	.type	Plane_intersect,@function
Plane_intersect:                        # @Plane_intersect
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$144, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	-8(%rbp), %rcx
	movq	(%rcx), %rdi
	movq	-8(%rbp), %rcx
	movq	8(%rcx), %rsi
	callq	Vec_dot
	xorps	%xmm1, %xmm1
	subsd	%xmm0, %xmm1
	movsd	%xmm1, -40(%rbp)
	movq	-24(%rbp), %rcx
	movq	8(%rcx), %rdi
	movq	-8(%rbp), %rcx
	movq	8(%rcx), %rsi
	callq	Vec_dot
	movsd	%xmm0, -48(%rbp)
	movq	-24(%rbp), %rcx
	movq	(%rcx), %rdi
	movq	-8(%rbp), %rcx
	movq	8(%rcx), %rsi
	callq	Vec_dot
	movsd	.LCPI15_0(%rip), %xmm1  # xmm1 = mem[0],zero
	xorps	%xmm2, %xmm2
	addsd	-40(%rbp), %xmm0
	divsd	-48(%rbp), %xmm0
	subsd	%xmm0, %xmm2
	movsd	%xmm2, -56(%rbp)
	movsd	-56(%rbp), %xmm0        # xmm0 = mem[0],zero
	ucomisd	%xmm1, %xmm0
	jbe	.LBB15_12
# %bb.1:
	movsd	-56(%rbp), %xmm0        # xmm0 = mem[0],zero
	movq	-32(%rbp), %rax
	movsd	32(%rax), %xmm1         # xmm1 = mem[0],zero
	ucomisd	%xmm0, %xmm1
	jbe	.LBB15_12
# %bb.2:
	movq	-24(%rbp), %rax
	movq	(%rax), %rdi
	movq	-24(%rbp), %rax
	movq	8(%rax), %rax
	movsd	-56(%rbp), %xmm0        # xmm0 = mem[0],zero
	movq	%rdi, -112(%rbp)        # 8-byte Spill
	movq	%rax, %rdi
	callq	Vec_mul
	movq	-112(%rbp), %rdi        # 8-byte Reload
	movq	%rax, %rsi
	callq	Vec_add
	movq	-32(%rbp), %rsi
	movq	%rax, 8(%rsi)
	movq	-8(%rbp), %rax
	movq	8(%rax), %rax
	movq	-32(%rbp), %rsi
	movq	%rax, 16(%rsi)
	movq	-16(%rbp), %rdi
	movq	-32(%rbp), %rax
	movq	16(%rax), %rsi
	callq	Vec_dot
	movsd	.LCPI15_3(%rip), %xmm1  # xmm1 = mem[0],zero
	movsd	.LCPI15_1(%rip), %xmm2  # xmm2 = mem[0],zero
	callq	clamp
	movsd	.LCPI15_1(%rip), %xmm1  # xmm1 = mem[0],zero
	movsd	.LCPI15_2(%rip), %xmm2  # xmm2 = mem[0],zero
	movsd	%xmm0, -64(%rbp)
	movq	-32(%rbp), %rax
	movq	8(%rax), %rax
	movsd	(%rax), %xmm0           # xmm0 = mem[0],zero
	movq	-32(%rbp), %rax
	movq	8(%rax), %rax
	movsd	(%rax), %xmm3           # xmm3 = mem[0],zero
	divsd	%xmm2, %xmm3
	movsd	%xmm0, -120(%rbp)       # 8-byte Spill
	movaps	%xmm3, %xmm0
	movsd	%xmm2, -128(%rbp)       # 8-byte Spill
	movsd	%xmm1, -136(%rbp)       # 8-byte Spill
	callq	floor
	movsd	-128(%rbp), %xmm1       # 8-byte Reload
                                        # xmm1 = mem[0],zero
	mulsd	%xmm0, %xmm1
	movsd	-120(%rbp), %xmm0       # 8-byte Reload
                                        # xmm0 = mem[0],zero
	subsd	%xmm1, %xmm0
	movsd	%xmm0, -72(%rbp)
	movq	-32(%rbp), %rax
	movq	8(%rax), %rax
	movsd	16(%rax), %xmm0         # xmm0 = mem[0],zero
	movq	-32(%rbp), %rax
	movq	8(%rax), %rax
	movsd	16(%rax), %xmm1         # xmm1 = mem[0],zero
	movsd	-128(%rbp), %xmm2       # 8-byte Reload
                                        # xmm2 = mem[0],zero
	divsd	%xmm2, %xmm1
	movsd	%xmm0, -144(%rbp)       # 8-byte Spill
	movaps	%xmm1, %xmm0
	callq	floor
	movsd	-128(%rbp), %xmm1       # 8-byte Reload
                                        # xmm1 = mem[0],zero
	mulsd	%xmm0, %xmm1
	movsd	-144(%rbp), %xmm0       # 8-byte Reload
                                        # xmm0 = mem[0],zero
	subsd	%xmm1, %xmm0
	movsd	%xmm0, -80(%rbp)
	movsd	-72(%rbp), %xmm0        # xmm0 = mem[0],zero
	movsd	-136(%rbp), %xmm1       # 8-byte Reload
                                        # xmm1 = mem[0],zero
	ucomisd	%xmm1, %xmm0
	jbe	.LBB15_4
# %bb.3:
	movsd	.LCPI15_1(%rip), %xmm0  # xmm0 = mem[0],zero
	movsd	-80(%rbp), %xmm1        # xmm1 = mem[0],zero
	ucomisd	%xmm0, %xmm1
	ja	.LBB15_6
.LBB15_4:
	movsd	.LCPI15_1(%rip), %xmm0  # xmm0 = mem[0],zero
	ucomisd	-72(%rbp), %xmm0
	jbe	.LBB15_7
# %bb.5:
	movsd	.LCPI15_1(%rip), %xmm0  # xmm0 = mem[0],zero
	ucomisd	-80(%rbp), %xmm0
	jbe	.LBB15_7
.LBB15_6:
	movsd	.LCPI15_4(%rip), %xmm0  # xmm0 = mem[0],zero
	mulsd	-64(%rbp), %xmm0
	movsd	%xmm0, -88(%rbp)
	jmp	.LBB15_8
.LBB15_7:
	movsd	-64(%rbp), %xmm0        # xmm0 = mem[0],zero
	movsd	%xmm0, -88(%rbp)
.LBB15_8:
	movsd	.LCPI15_5(%rip), %xmm0  # xmm0 = mem[0],zero
	movq	-32(%rbp), %rax
	movq	8(%rax), %rax
	movsd	16(%rax), %xmm1         # xmm1 = mem[0],zero
	movaps	.LCPI15_6(%rip), %xmm2  # xmm2 = [nan,nan]
	pand	%xmm2, %xmm1
	movsd	%xmm1, -96(%rbp)
	ucomisd	-96(%rbp), %xmm0
	jbe	.LBB15_10
# %bb.9:
	movsd	.LCPI15_1(%rip), %xmm0  # xmm0 = mem[0],zero
	movsd	.LCPI15_7(%rip), %xmm1  # xmm1 = mem[0],zero
	mulsd	-96(%rbp), %xmm1
	subsd	%xmm1, %xmm0
	movsd	%xmm0, -104(%rbp)
	jmp	.LBB15_11
.LBB15_10:
	xorps	%xmm0, %xmm0
	movsd	%xmm0, -104(%rbp)
.LBB15_11:
	movq	-8(%rbp), %rax
	movq	16(%rax), %rdi
	movsd	-88(%rbp), %xmm0        # xmm0 = mem[0],zero
	mulsd	-104(%rbp), %xmm0
	callq	Vec_mul
	movq	-32(%rbp), %rdi
	movq	%rax, 24(%rdi)
	movsd	-56(%rbp), %xmm0        # xmm0 = mem[0],zero
	movq	-32(%rbp), %rax
	movsd	%xmm0, 32(%rax)
	movq	-32(%rbp), %rax
	movl	(%rax), %ecx
	addl	$1, %ecx
	movq	-32(%rbp), %rax
	movl	%ecx, (%rax)
	movq	-24(%rbp), %rax
	movq	8(%rax), %rax
	movq	-32(%rbp), %rdi
	movq	%rax, 40(%rdi)
.LBB15_12:
	xorl	%eax, %eax
	addq	$144, %rsp
	popq	%rbp
	retq
.Lfunc_end15:
	.size	Plane_intersect, .Lfunc_end15-Plane_intersect
	.cfi_endproc
                                        # -- End function
	.globl	Env_intersect           # -- Begin function Env_intersect
	.p2align	4, 0x90
	.type	Env_intersect,@function
Env_intersect:                          # @Env_intersect
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-8(%rbp), %rdx
	movq	8(%rdx), %rdi
	movq	-8(%rbp), %rdx
	movq	(%rdx), %rsi
	movq	-16(%rbp), %rdx
	movq	-24(%rbp), %rcx
	callq	Sphere_intersect
	movq	-8(%rbp), %rcx
	movq	16(%rcx), %rdi
	movq	-8(%rbp), %rcx
	movq	(%rcx), %rsi
	movq	-16(%rbp), %rdx
	movq	-24(%rbp), %rcx
	movl	%eax, -28(%rbp)         # 4-byte Spill
	callq	Sphere_intersect
	movq	-8(%rbp), %rcx
	movq	24(%rcx), %rdi
	movq	-8(%rbp), %rcx
	movq	(%rcx), %rsi
	movq	-16(%rbp), %rdx
	movq	-24(%rbp), %rcx
	movl	%eax, -32(%rbp)         # 4-byte Spill
	callq	Sphere_intersect
	movq	-8(%rbp), %rcx
	movq	32(%rcx), %rdi
	movq	-8(%rbp), %rcx
	movq	(%rcx), %rsi
	movq	-16(%rbp), %rdx
	movq	-24(%rbp), %rcx
	movl	%eax, -36(%rbp)         # 4-byte Spill
	callq	Plane_intersect
	xorl	%r8d, %r8d
	movl	%eax, -40(%rbp)         # 4-byte Spill
	movl	%r8d, %eax
	addq	$48, %rsp
	popq	%rbp
	retq
.Lfunc_end16:
	.size	Env_intersect, .Lfunc_end16-Env_intersect
	.cfi_endproc
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function Env_new
.LCPI17_0:
	.quad	4607182418800017408     # double 1
.LCPI17_1:
	.quad	-4616189618054758400    # double -1
.LCPI17_2:
	.quad	4609434218613702656     # double 1.5
.LCPI17_3:
	.quad	-4611686018427387904    # double -2
.LCPI17_4:
	.quad	4602678819172646912     # double 0.5
.LCPI17_5:
	.quad	4614680912179589284     # double 3.3300000000000001
.LCPI17_6:
	.quad	4611686018427387904     # double 2
.LCPI17_7:
	.quad	4619184511806959780     # double 6.6600000000000001
.LCPI17_8:
	.quad	-4620693217682128896    # double -0.5
.LCPI17_9:
	.quad	4603372373515261968     # double 0.57699999999999996
	.text
	.globl	Env_new
	.p2align	4, 0x90
	.type	Env_new,@function
Env_new:                                # @Env_new
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$176, %rsp
	movl	$40, %eax
	movl	%eax, %edi
	callq	malloc
	movsd	.LCPI17_9(%rip), %xmm0  # xmm0 = mem[0],zero
	movq	%rax, -8(%rbp)
	movsd	%xmm0, -16(%rbp)        # 8-byte Spill
	movsd	-16(%rbp), %xmm1        # 8-byte Reload
                                        # xmm1 = mem[0],zero
	movsd	-16(%rbp), %xmm2        # 8-byte Reload
                                        # xmm2 = mem[0],zero
	callq	Vec_new
	xorps	%xmm0, %xmm0
	movsd	.LCPI17_8(%rip), %xmm1  # xmm1 = mem[0],zero
	movq	-8(%rbp), %rdi
	movq	%rax, (%rdi)
	xorps	%xmm2, %xmm2
	movsd	%xmm0, -24(%rbp)        # 8-byte Spill
	movaps	%xmm2, %xmm0
	movsd	%xmm1, -32(%rbp)        # 8-byte Spill
	callq	sin
	movsd	-24(%rbp), %xmm1        # 8-byte Reload
                                        # xmm1 = mem[0],zero
	movsd	%xmm0, -40(%rbp)        # 8-byte Spill
	movaps	%xmm1, %xmm0
	movsd	-32(%rbp), %xmm1        # 8-byte Reload
                                        # xmm1 = mem[0],zero
	movsd	-40(%rbp), %xmm2        # 8-byte Reload
                                        # xmm2 = mem[0],zero
	callq	Vec_new
	movsd	.LCPI17_0(%rip), %xmm0  # xmm0 = mem[0],zero
	xorps	%xmm1, %xmm1
	movsd	%xmm1, -48(%rbp)        # 8-byte Spill
	movsd	-48(%rbp), %xmm2        # 8-byte Reload
                                        # xmm2 = mem[0],zero
	movq	%rax, -56(%rbp)         # 8-byte Spill
	callq	Vec_new
	movsd	.LCPI17_4(%rip), %xmm0  # xmm0 = mem[0],zero
	movq	-56(%rbp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	Sphere_new
	movsd	.LCPI17_6(%rip), %xmm0  # xmm0 = mem[0],zero
	xorps	%xmm1, %xmm1
	movq	-8(%rbp), %rsi
	movq	%rax, 8(%rsi)
	movsd	.LCPI17_7(%rip), %xmm2  # xmm2 = mem[0],zero
	movsd	%xmm0, -64(%rbp)        # 8-byte Spill
	movaps	%xmm2, %xmm0
	movsd	%xmm1, -72(%rbp)        # 8-byte Spill
	callq	cos
	movsd	-64(%rbp), %xmm1        # 8-byte Reload
                                        # xmm1 = mem[0],zero
	movsd	%xmm0, -80(%rbp)        # 8-byte Spill
	movaps	%xmm1, %xmm0
	movsd	-72(%rbp), %xmm1        # 8-byte Reload
                                        # xmm1 = mem[0],zero
	movsd	-80(%rbp), %xmm2        # 8-byte Reload
                                        # xmm2 = mem[0],zero
	callq	Vec_new
	xorps	%xmm0, %xmm0
	movsd	.LCPI17_0(%rip), %xmm1  # xmm1 = mem[0],zero
	movsd	%xmm0, -88(%rbp)        # 8-byte Spill
	movsd	-88(%rbp), %xmm2        # 8-byte Reload
                                        # xmm2 = mem[0],zero
	movq	%rax, -96(%rbp)         # 8-byte Spill
	callq	Vec_new
	movsd	.LCPI17_0(%rip), %xmm0  # xmm0 = mem[0],zero
	movq	-96(%rbp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	Sphere_new
	movsd	.LCPI17_3(%rip), %xmm0  # xmm0 = mem[0],zero
	movsd	.LCPI17_4(%rip), %xmm1  # xmm1 = mem[0],zero
	movq	-8(%rbp), %rsi
	movq	%rax, 16(%rsi)
	movsd	.LCPI17_5(%rip), %xmm2  # xmm2 = mem[0],zero
	movsd	%xmm0, -104(%rbp)       # 8-byte Spill
	movaps	%xmm2, %xmm0
	movsd	%xmm1, -112(%rbp)       # 8-byte Spill
	callq	cos
	movsd	-104(%rbp), %xmm1       # 8-byte Reload
                                        # xmm1 = mem[0],zero
	movsd	%xmm0, -120(%rbp)       # 8-byte Spill
	movaps	%xmm1, %xmm0
	movsd	-112(%rbp), %xmm1       # 8-byte Reload
                                        # xmm1 = mem[0],zero
	movsd	-120(%rbp), %xmm2       # 8-byte Reload
                                        # xmm2 = mem[0],zero
	callq	Vec_new
	xorps	%xmm0, %xmm0
	movsd	.LCPI17_0(%rip), %xmm2  # xmm2 = mem[0],zero
	movsd	%xmm0, -128(%rbp)       # 8-byte Spill
	movsd	-128(%rbp), %xmm1       # 8-byte Reload
                                        # xmm1 = mem[0],zero
	movq	%rax, -136(%rbp)        # 8-byte Spill
	callq	Vec_new
	movsd	.LCPI17_2(%rip), %xmm0  # xmm0 = mem[0],zero
	movq	-136(%rbp), %rdi        # 8-byte Reload
	movq	%rax, %rsi
	callq	Sphere_new
	xorps	%xmm0, %xmm0
	movsd	.LCPI17_1(%rip), %xmm1  # xmm1 = mem[0],zero
	movq	-8(%rbp), %rsi
	movq	%rax, 24(%rsi)
	movsd	%xmm0, -144(%rbp)       # 8-byte Spill
	movsd	-144(%rbp), %xmm2       # 8-byte Reload
                                        # xmm2 = mem[0],zero
	callq	Vec_new
	xorps	%xmm0, %xmm0
	movsd	.LCPI17_0(%rip), %xmm1  # xmm1 = mem[0],zero
	movsd	%xmm0, -152(%rbp)       # 8-byte Spill
	movsd	-152(%rbp), %xmm2       # 8-byte Reload
                                        # xmm2 = mem[0],zero
	movq	%rax, -160(%rbp)        # 8-byte Spill
	callq	Vec_new
	movsd	.LCPI17_0(%rip), %xmm0  # xmm0 = mem[0],zero
	movsd	%xmm0, -168(%rbp)       # 8-byte Spill
	movsd	-168(%rbp), %xmm1       # 8-byte Reload
                                        # xmm1 = mem[0],zero
	movsd	-168(%rbp), %xmm2       # 8-byte Reload
                                        # xmm2 = mem[0],zero
	movq	%rax, -176(%rbp)        # 8-byte Spill
	callq	Vec_new
	movq	-160(%rbp), %rdi        # 8-byte Reload
	movq	-176(%rbp), %rsi        # 8-byte Reload
	movq	%rax, %rdx
	callq	Plane_new
	movq	-8(%rbp), %rdx
	movq	%rax, 32(%rdx)
	movq	-8(%rbp), %rax
	addq	$176, %rsp
	popq	%rbp
	retq
.Lfunc_end17:
	.size	Env_new, .Lfunc_end17-Env_new
	.cfi_endproc
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function color_of
.LCPI18_0:
	.quad	4643211215818981376     # double 256
.LCPI18_1:
	.quad	4607182418800017408     # double 1
	.text
	.globl	color_of
	.p2align	4, 0x90
	.type	color_of,@function
color_of:                               # @color_of
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	xorps	%xmm1, %xmm1
	movsd	.LCPI18_1(%rip), %xmm2  # xmm2 = mem[0],zero
	movsd	%xmm0, -16(%rbp)
	movsd	-16(%rbp), %xmm0        # xmm0 = mem[0],zero
	callq	clamp
	movsd	.LCPI18_0(%rip), %xmm1  # xmm1 = mem[0],zero
	mulsd	%xmm0, %xmm1
	cvttsd2si	%xmm1, %eax
	movl	%eax, -20(%rbp)
	cmpl	$256, -20(%rbp)         # imm = 0x100
	jne	.LBB18_2
# %bb.1:
	movl	$255, -4(%rbp)
	jmp	.LBB18_3
.LBB18_2:
	movl	-20(%rbp), %eax
	movl	%eax, -4(%rbp)
.LBB18_3:
	movl	-4(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
.Lfunc_end18:
	.size	color_of, .Lfunc_end18-color_of
	.cfi_endproc
                                        # -- End function
	.globl	print_col               # -- Begin function print_col
	.p2align	4, 0x90
	.type	print_col,@function
print_col:                              # @print_col
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	movsd	(%rdi), %xmm0           # xmm0 = mem[0],zero
	callq	color_of
	movq	-8(%rbp), %rdi
	movsd	8(%rdi), %xmm0          # xmm0 = mem[0],zero
	movl	%eax, -12(%rbp)         # 4-byte Spill
	callq	color_of
	movq	-8(%rbp), %rdi
	movsd	16(%rdi), %xmm0         # xmm0 = mem[0],zero
	movl	%eax, -16(%rbp)         # 4-byte Spill
	callq	color_of
	movabsq	$.L.str, %rdi
	movl	-12(%rbp), %esi         # 4-byte Reload
	movl	-16(%rbp), %edx         # 4-byte Reload
	movl	%eax, %ecx
	movb	$0, %al
	callq	printf
	movl	%eax, -20(%rbp)         # 4-byte Spill
	addq	$32, %rsp
	popq	%rbp
	retq
.Lfunc_end19:
	.size	print_col, .Lfunc_end19-print_col
	.cfi_endproc
                                        # -- End function
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function main
.LCPI20_0:
	.quad	4711630319722168320     # double 1.0E+7
.LCPI20_1:
	.quad	-4616189618054758400    # double -1
.LCPI20_2:
	.quad	4611686018427387904     # double 2
.LCPI20_3:
	.quad	4618441417868443648     # double 6
.LCPI20_4:
	.quad	4607182418800017408     # double 1
.LCPI20_5:
	.quad	4639481672377565184     # double 150
.LCPI20_6:
	.quad	4547007122018943789     # double 1.0E-4
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$208, %rsp
	movabsq	$.L.str.1, %rdi
	movl	$200, %eax
	movl	$0, -4(%rbp)
	movl	%eax, %esi
	movl	%eax, %edx
	movb	$0, %al
	callq	printf
	movl	%eax, -92(%rbp)         # 4-byte Spill
	callq	Env_new
	movq	%rax, -16(%rbp)
	movl	$0, -20(%rbp)
.LBB20_1:                               # =>This Loop Header: Depth=1
                                        #     Child Loop BB20_3 Depth 2
                                        #       Child Loop BB20_6 Depth 3
	cmpl	$300, -20(%rbp)         # imm = 0x12C
	jge	.LBB20_14
# %bb.2:                                #   in Loop: Header=BB20_1 Depth=1
	movl	$0, -24(%rbp)
.LBB20_3:                               #   Parent Loop BB20_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB20_6 Depth 3
	cmpl	$300, -24(%rbp)         # imm = 0x12C
	jge	.LBB20_13
# %bb.4:                                #   in Loop: Header=BB20_3 Depth=2
	xorps	%xmm0, %xmm0
	movsd	.LCPI20_2(%rip), %xmm1  # xmm1 = mem[0],zero
	movsd	.LCPI20_3(%rip), %xmm2  # xmm2 = mem[0],zero
	movsd	.LCPI20_4(%rip), %xmm3  # xmm3 = mem[0],zero
	movsd	.LCPI20_5(%rip), %xmm4  # xmm4 = mem[0],zero
	movl	$300, %eax              # imm = 0x12C
	movl	-24(%rbp), %ecx
	cvtsi2sdl	%ecx, %xmm5
	divsd	%xmm4, %xmm5
	subsd	%xmm3, %xmm5
	movsd	%xmm5, -32(%rbp)
	subl	-20(%rbp), %eax
	cvtsi2sdl	%eax, %xmm5
	divsd	%xmm4, %xmm5
	subsd	%xmm3, %xmm5
	movsd	%xmm5, -40(%rbp)
	callq	Vec_new
	movsd	.LCPI20_1(%rip), %xmm2  # xmm2 = mem[0],zero
	movsd	-32(%rbp), %xmm0        # xmm0 = mem[0],zero
	movsd	-40(%rbp), %xmm1        # xmm1 = mem[0],zero
	movq	%rax, -104(%rbp)        # 8-byte Spill
	callq	Vec_new
	movq	%rax, %rdi
	callq	Vec_normalize
	movq	-104(%rbp), %rdi        # 8-byte Reload
	movq	%rax, %rsi
	callq	Ray_new
	xorps	%xmm0, %xmm0
	movq	%rax, -48(%rbp)
	movsd	%xmm0, -112(%rbp)       # 8-byte Spill
	movsd	-112(%rbp), %xmm1       # 8-byte Reload
                                        # xmm1 = mem[0],zero
	movsd	-112(%rbp), %xmm2       # 8-byte Reload
                                        # xmm2 = mem[0],zero
	callq	Vec_new
	xorps	%xmm0, %xmm0
	movsd	%xmm0, -120(%rbp)       # 8-byte Spill
	movsd	-120(%rbp), %xmm1       # 8-byte Reload
                                        # xmm1 = mem[0],zero
	movsd	-120(%rbp), %xmm2       # 8-byte Reload
                                        # xmm2 = mem[0],zero
	movq	%rax, -128(%rbp)        # 8-byte Spill
	callq	Vec_new
	xorps	%xmm0, %xmm0
	movsd	%xmm0, -136(%rbp)       # 8-byte Spill
	movsd	-136(%rbp), %xmm1       # 8-byte Reload
                                        # xmm1 = mem[0],zero
	movsd	-136(%rbp), %xmm2       # 8-byte Reload
                                        # xmm2 = mem[0],zero
	movq	%rax, -144(%rbp)        # 8-byte Spill
	callq	Vec_new
	xorps	%xmm0, %xmm0
	movsd	%xmm0, -152(%rbp)       # 8-byte Spill
	movsd	-152(%rbp), %xmm1       # 8-byte Reload
                                        # xmm1 = mem[0],zero
	movsd	-152(%rbp), %xmm2       # 8-byte Reload
                                        # xmm2 = mem[0],zero
	movq	%rax, -160(%rbp)        # 8-byte Spill
	callq	Vec_new
	xorl	%edi, %edi
	movsd	.LCPI20_0(%rip), %xmm0  # xmm0 = mem[0],zero
	movq	-128(%rbp), %rsi        # 8-byte Reload
	movq	-144(%rbp), %rdx        # 8-byte Reload
	movq	-160(%rbp), %rcx        # 8-byte Reload
	movq	%rax, %r8
	callq	Isect_new
	movq	%rax, -56(%rbp)
	movq	-16(%rbp), %rdi
	movq	-48(%rbp), %rsi
	movq	-56(%rbp), %rdx
	callq	Env_intersect
	xorl	%r9d, %r9d
	movq	-56(%rbp), %rcx
	cmpl	(%rcx), %r9d
	movl	%eax, -164(%rbp)        # 4-byte Spill
	jge	.LBB20_11
# %bb.5:                                #   in Loop: Header=BB20_3 Depth=2
	movsd	.LCPI20_4(%rip), %xmm0  # xmm0 = mem[0],zero
	movq	-56(%rbp), %rax
	movq	24(%rax), %rax
	movq	%rax, -64(%rbp)
	movsd	%xmm0, -176(%rbp)       # 8-byte Spill
	movsd	-176(%rbp), %xmm1       # 8-byte Reload
                                        # xmm1 = mem[0],zero
	movsd	-176(%rbp), %xmm2       # 8-byte Reload
                                        # xmm2 = mem[0],zero
	callq	Vec_new
	movq	-56(%rbp), %rcx
	movq	24(%rcx), %rsi
	movq	%rax, %rdi
	callq	Vec_multi
	movq	%rax, -72(%rbp)
	movl	$1, -76(%rbp)
.LBB20_6:                               #   Parent Loop BB20_1 Depth=1
                                        #     Parent Loop BB20_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	cmpl	$4, -76(%rbp)
	jge	.LBB20_10
# %bb.7:                                #   in Loop: Header=BB20_6 Depth=3
	movsd	.LCPI20_6(%rip), %xmm0  # xmm0 = mem[0],zero
	movq	-56(%rbp), %rax
	movq	8(%rax), %rdi
	movq	-56(%rbp), %rax
	movq	16(%rax), %rax
	movq	%rdi, -184(%rbp)        # 8-byte Spill
	movq	%rax, %rdi
	callq	Vec_mul
	movq	-184(%rbp), %rdi        # 8-byte Reload
	movq	%rax, %rsi
	callq	Vec_add
	movq	-56(%rbp), %rsi
	movq	40(%rsi), %rdi
	movq	-56(%rbp), %rsi
	movq	16(%rsi), %rsi
	movq	%rax, -192(%rbp)        # 8-byte Spill
	callq	Vec_reflect
	movq	-192(%rbp), %rdi        # 8-byte Reload
	movq	%rax, %rsi
	callq	Ray_new
	movq	%rax, -88(%rbp)
	movq	-16(%rbp), %rdi
	movq	-88(%rbp), %rsi
	movq	-56(%rbp), %rdx
	callq	Env_intersect
	movl	-76(%rbp), %ecx
	movq	-56(%rbp), %rdx
	cmpl	(%rdx), %ecx
	movl	%eax, -196(%rbp)        # 4-byte Spill
	jge	.LBB20_9
# %bb.8:                                #   in Loop: Header=BB20_6 Depth=3
	movq	-64(%rbp), %rdi
	movq	-72(%rbp), %rax
	movq	-56(%rbp), %rcx
	movq	24(%rcx), %rsi
	movq	%rdi, -208(%rbp)        # 8-byte Spill
	movq	%rax, %rdi
	callq	Vec_multi
	movq	-208(%rbp), %rdi        # 8-byte Reload
	movq	%rax, %rsi
	callq	Vec_add
	movq	%rax, -64(%rbp)
	movq	-72(%rbp), %rdi
	movq	-56(%rbp), %rax
	movq	24(%rax), %rsi
	callq	Vec_multi
	movq	%rax, -72(%rbp)
.LBB20_9:                               #   in Loop: Header=BB20_6 Depth=3
	movl	-76(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -76(%rbp)
	jmp	.LBB20_6
.LBB20_10:                              #   in Loop: Header=BB20_3 Depth=2
	movq	-64(%rbp), %rdi
	callq	print_col
	jmp	.LBB20_12
.LBB20_11:                              #   in Loop: Header=BB20_3 Depth=2
	movq	-48(%rbp), %rax
	movq	8(%rax), %rax
	movsd	8(%rax), %xmm0          # xmm0 = mem[0],zero
	movq	-48(%rbp), %rax
	movq	8(%rax), %rax
	movsd	8(%rax), %xmm1          # xmm1 = mem[0],zero
	movq	-48(%rbp), %rax
	movq	8(%rax), %rax
	movsd	8(%rax), %xmm2          # xmm2 = mem[0],zero
	callq	Vec_new
	movq	%rax, %rdi
	callq	print_col
.LBB20_12:                              #   in Loop: Header=BB20_3 Depth=2
	movl	-24(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -24(%rbp)
	jmp	.LBB20_3
.LBB20_13:                              #   in Loop: Header=BB20_1 Depth=1
	movl	-20(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -20(%rbp)
	jmp	.LBB20_1
.LBB20_14:
	movl	-4(%rbp), %eax
	addq	$208, %rsp
	popq	%rbp
	retq
.Lfunc_end20:
	.size	main, .Lfunc_end20-main
	.cfi_endproc
                                        # -- End function
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%d %d %d\n"
	.size	.L.str, 10

	.type	.L.str.1,@object        # @.str.1
.L.str.1:
	.asciz	"P3\n%d %d\n255\n"
	.size	.L.str.1, 14


	.ident	"clang version 6.0.0-1ubuntu2 (tags/RELEASE_600/final)"
	.section	".note.GNU-stack","",@progbits
