	//
	// Test out case arm instructions
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

;; 1:	bl	1b
;; 	bl	2f
;; 	blr	X12
;; 	br	X13
;; 2:	brk	#1234

	;; cbnz	X3, 2b
	;; cbz	x4, 2b

	tbnz	X3, #56, 2f
	tbz	x4, #23, 2f
2:	tbnz	X3, #23, 2b
	tbz	x4, #56, 2b

helloworld:	.ascii "Hi there\n"
	.end
