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

//	casb	w6, w8, [x3]
	
//	casab	w10, w3, [X2]
//	casab	w10, w3, [sp]

//	casalb	w10, w3, [x5]

//	caslb	w10, w3, [x0]

//	cash	w6, w8, [x3]
	
//	casah	w10, w3, [X2]
//	casah	w10, w3, [sp]

//	casalh	w10, w3, [x5]

//	caslh	w10, w3, [x0]

	;; casp	x6, x7, x8,x9, [x3]
	
	;; caspa	x10,x11, x2,x3, [X2]
	;; caspa	x10,x11, x2,x3, [sp]

	;; caspal	x10,x11, x2,x3, [x5]

	;; caspl	x10,x11, x2,x3, [x0]

	cas	x6, x8, [x3]

	casa	x10, x3, [X2]
	casa	x10, x3, [sp]

	casal	x10, x3, [x5]
	casl	x10, x3, [x0]
	
	
helloworld:	.ascii "Hi there\n"
	.end
