	//
	// Test out arm math instructions
	//
	.global _start
	.align 2

_start:	mov X0, #1     // 1 = StdOut
        adr X1, helloworld // string to print
        mov X2, #13     // length of our string
        mov X16, #4     // MacOS write system call
        svc 0

// Setup the parameters to exit the program
// and then call Linux to do it.

        mov     X0, #0      // Use 0 return code
        mov     X16, #1     // Service command code 1 terminates this program
        svc     0           // Call MacOS to terminate the program


	movz	X22, #0x4444
	movk    X22, #0x3333, lsl 16
	movk    X22, #0x2222, lsl 32
	movk    X22, #0x1111, lsl 48

	;; madd	X21, X29, X30, X1
	;; mov	X21, SP
	;; mov	X21, #0xff230000
	;; msub	X21, X29, X30, X1
	;; mul	X1,X2,X3
	;; mvn	X1, X10, ror #4

	;; neg	X1, X10, asr #4
	;; neg	X1, X10, lsr #4
	;; negs	X1, X10, asr #4
	;; negs	X1, X10, lsr #4

	;; ngc	X1, X12
	;; ngcs	X1, X12

	;; nop

	;; orn	x26, x24, x1, ror #20

	;; orr	x10, x9, #0xff00
	;; orr	x11, x12, x11
	;; orr	x11, x12, x11, ror #8

	;; rbit	X1,X2
	;; ret	X12

	;; rev	X3,X4
	;; rev16	x5,x6
	;; rev32	x7,x8

	;; ror	x9,x10,x11
	;; ror	x12, x13, #4

	;; sbc	x15, x16, x17
	;; sbcs	x15, x16, x17

	;; sdiv	x18, x19, x20
	;; udiv	x21, x22, x23

	;; smaddl	x1,w2,w3,x4
	;; smulh	X5,X6,X7
	;; smull	X5,w6,w8

	;; stp	x1,x2,[x3], #32
	;; stp	x1, x2, [x3, #32]!
	;; stp	x1, x2, [x3, #32]

	;; str	x1,[x3], #32
	;; str	x1, [x3, #32]!
	;; str	x1, [x3, #32]
	;; str	x2, [x10, x7, sxtx #3]
	
	;; strb	w1,[x3], #32
	;; strb	w1, [x3, #32]!
	;; strb	w1, [x3, #32]
	;; strb	w2, [x10, x7, sxtx]
	
	;; strh	w1,[x3], #32
	;; strh	w1, [x3, #32]!
	;; strh	w1, [x3, #32]
	;; strh	w2, [x10, x7, sxtx]

	;; stur	X29, [X3, #-8]
	;; sturb	w29, [X3, #-8]
	;; sturh	w29, [X3, #-8]
	
	;; sub     X3, X4, X6
	;; sub     X3, X5, 135
	;; sub     X3, X5, 135, lsl 12
	;; sub     X2, X4, X6, lsl 3
	;; sub     X1, X12, X18, lsr 3
	;; sub     X1, X12, X18, asr 3

	;; subs     X3, X4, X6
	;; subs     X3, X5, 135
	;; subs     X3, X5, 135, lsl 12
	;; subs     X2, X4, X6, lsl 3
	;; subs     X1, X12, X18, lsr 3
	;; subs     X1, X12, X18, asr 3

	umaddl	X1, w2, w3, X4
	umnegl	x5, w6, w7
	umsubl	x8, w9, w10, x11
	umulh	x12,x13,x14
	umull	x15,w16,w17
	

helloworld:	.ascii "Hi there\n"
	.end
