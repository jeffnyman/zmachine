; Infocom ZIP interpreter for Apple II,

; The ZIP interpreter is copyrighted by Infocom, Inc.

	cpu	6502


iver1	equ	$0100	; interp for Z-machine version 1
			; only known to be used by Zork I releases 2 and 5

iver2	equ	$0200	; interp for Z-machine version 2
			; only known to be used by Zork I release 15
			; and Zork II release 7

; The following are interpreter revisions for Z-machine version 3
iver3	equ	$0300
iver3a	equ	$0301
iver3b	equ	$0302
iver3e	equ	$0305	; NOT YET SUPPORTED
iver3h	equ	$0308	; NOT YET SUPPORTED
iver3k	equ	$030b	; NOT YET SUPPORTED
iver3m	equ	$030d	; NOT YET SUPPORTED


	ifndef	iver
iver	equ	iver3b
	endif


	ifndef	rwtssz
	if	iver==iver1
rwtssz	equ	$0700
	else
rwtssz	equ	$0800
	endif
	endif


lc40	equ	0			; 40 column lower case patch


; define memory usage

zporg	equ	$7c			; origin of zero page usage
buffer	equ	$0200			; I/O buffer

	if	iver<iver3
stckmx	equ	180			; maximum size of stack in words
	else
stckmx	equ	224			; maximum size of stack in words
	endif

stcklc	equ	$03e8			; base address of stack (works down)
stklim	equ	stcklc-2*stckmx		; lower limit of stack

mainor	equ	$0800			; origin of main program
	
rwtsor	equ	$2400			; origin of RWTS routines
rwts	equ	rwtsor+$0500		; entry point of RWTS routines
rwtsen	equ	rwtsor+rwtssz

	if	iver<iver3a
vmtorg	equ	mainor+$1a00		; origin of virtual memory tables
	else
vmtorg	equ	rwtsen			; origin of virtual memory tables
	endif
vmtend	equ	vmtorg+$0200

	if	iver<iver3a
firflc	equ	rwtsen			; first location available
	else
firflc	equ	vmtend
	endif
lstflc	equ	$c000-1			; last potential location available

vmt1lc	equ	vmtorg+$0000		; virtual memory page tables
vmt2lc	equ	vmtorg+$0080
vmt3lc	equ	vmtorg+$0100
vmt4lc	equ	vmtorg+$0180



; Control characters

crchar	equ	$0d			; carriage return
lfchar	equ	$0a			; line feed
tbchar	equ	$09			; horizontal tab
ffchar	equ	$0c			; form feed


; Apple monitor ROM's zero page locations

wndlft	equ	$20			; screen window parameters
wndwdt	equ	$21
wndtop	equ	$22
wndbot	equ	$23

cursrh	equ	$24			; cursor position
cursrv	equ	$25

invflg	equ	$32			; inverse video flag

prompt	equ	$33			; line input prompt

cswl	equ	$36			; character output vector

rndloc	equ	$4e			; location randomized by keyboard input


; Apple firmware's screen hole locations

cur80h	equ	$057b			; (slot 3) cursor h for 80 column

prtflg	equ	$0779			; (slot 1) printer flags


; Apple hardware locations

rdc3rom	equ	$c017			; IIe and later: bit 7 set if
					; slot 3 ROM ($c3xx) enabled


; Apple peripheral card firmware

sl3fw	equ	$c300


; Apple ROM id location

romid	equ	$fbb3


; Apple monitor routines

vtab	equ	$fc22			; adjust video pointer after cursor move
home	equ	$fc58			; clear screen window
clreol	equ	$fc9c			; clear to end of line
rdkey	equ	$fd0c			; get a key from keyboard
getln1	equ	$fd6f			; get a line from keyboard
cout	equ	$fded			; output a char to current device
cout1	equ	$fdf0			; output a char to screen

; define zero page usage

	org	zporg

s7c	rmb	3			; subroutine used in iver<iver3
secptk	rmb	1			; number of sectors per track on disk

opcode	rmb	1			; opcode of current instruction
argcnt	rmb	1			; instruction arguments

arg1	rmb	2
arg2	rmb	2
arg3	rmb	2
arg4	rmb	2

prgidx	rmb	1			; PC low byte, index into page
prglpg	rmb	2			; PC logical page number
prgmpt	rmb	2			; PC mem loc of logical page
prgupd	rmb	1			; PC new page flag
prgppg	rmb	1			; PC physical page number

auxlpg	rmb	2			; AUX logical page number
auxidx	rmb	1			; AUX low byte, index into page
auxmpt	rmb	2			; AUX mem loc of logical page
auxupd	rmb	1			; AUX new page flag
auxppg	rmb	1			; AUX physical page number

glbvar	rmb	2			; pointer to global variables
locvar	rmb	30			; storage of local variables

swpmem	rmb	2			; address of first swappable page
frzmem	rmb	2			; address of first frozen page
frzpgs	rmb	1			; number of frozen pages
swppgs	rmb	1			; number of swappable phys. pages

mrupag	rmb	1			; phys. pg. # of most recently used page
lrupag	rmb	1			; phys. pg. # of least recently used page

vmtab1	rmb	2			; virtual memory table pointers
vmtab2	rmb	2
vmtab3	rmb	2
vmtab4	rmb	2

stkcnt	rmb	1			; # items on stack
stkpnt	rmb	2			; stack pointer
stkpsv	rmb	2			; stack ptr save during call
stkcsv	rmb	1			; stack cnt save during call

tmpmod	rmb	1			; string output temporary char. mode
prmmod	rmb	1			; string output perm. char. mode
pnybcn	rmb	1			; string output nybble counter
pnybbf	rmb	2			; string output nybble buffer

inword	rmb	6			; word to be packed

ld9	rmb	1

pkword	rmb	4			; packed word

lde	rmb	1
ldf	rmb	1
le0	rmb	1
le1	rmb	1

	if	iver>=iver2
sbwdpt	rmb	2
	endif

acb	rmb	2
acc	rmb	2
acd	rmb	2

	if	iver==iver1
rndbuf	rmb	4
	endif

mdflag	rmb	1			; negative arg count for mul/div

chrptr	rmb	1			; char out buffer pointer
chrpt2	rmb	1			; char out buffer pointer 2
lincnt	rmb	1			; output line counter
	
prcswl	rmb	2			; CSWL vector contents for printer

prgids	rmb	1			; prgidx save
prglps	rmb	2			; prglpg save

stltyp	rmb	1			; status line type (time vs. score)


; define offsets into game header

	org	0

hdrirl	rmb	1			; required interpreter release (should be 3)
hdrtyp	rmb	1			; game type flags (score/time, etc.)
hdrrel	rmb	2			; game release
hdrfrz	rmb	2			; log. addr. of end of frozen memory
hdrstr	rmb	2			; log. addr. of start of code
hdrvcb	rmb	2			; log. addr. of vocab. table
hdrthg	rmb	2			; log. addr. of thing table
hdrgbv	rmb	2			; log. addr. of global variables
hdrimp	rmb	2			; log. addr. of end of impure storage
hdrflg	rmb	2			; flags (script, etc.)
hdrser	rmb	6			; game serial no. (release data)
hdrsbw	rmb	2			; log. addr. of subword table
hdrcka	rmb	2			; half of last log. addr. to checksum
hdrckv	rmb	2			; expected checksum value


; define thing table offsets

	org	0

thgatt	rmb	4			; attribute bits
thgpar	rmb	1			; parent thing number
thgsib	rmb	1			; sibling thing number
thgchd	rmb	1			; child thing number
thgprp	rmb	2			; property list pointer

	include	zipmac.inc

; start of interpreter

	org	mainor

start:	cld				; very important

	lda	#$00			; clear our section of zero page
	ldx	#$80
l0805:	sta	$00,x
	ixbne	l0805

	ldx	#$ff			; init hardware stack
	txs

	if	iver<iver3
	dmovi2	$c100,prcswl
	else
	jsr	initsc			; init and clear screen window
	endif

	mov	#$00,prgupd,auxupd	; indicate no pages loaded

	mov	#$01,stkcnt		; init software stack
	dmovi	stcklc,stkpnt

	mov	#$ff,ld9

	dmovi	vmt1lc,vmtab1		; init virtual memory table pointers
	dmovi	vmt2lc,vmtab2
	dmovi	vmt3lc,vmtab3
	dmovi	vmt4lc,vmtab4

	ldy	#$00			; init virtual memory tables
	ldx	#$80

l084a:
	if	iver<iver3
	lda	#$00
	else
	lda	#$ff
	endif

	sta	(vmtab1),y
	sta	(vmtab2),y
	tya
	clc
	adc	#$01
	sta	(vmtab3),y
	tya
	sec
	sbc	#$01
	sta	(vmtab4),y
	iny
	dxbne	l084a
	dey
	lda	#$ff
	sta	(vmtab3),y

	mov	#$00,mrupag
	mov	#$7f,lrupag

	dmovi	firflc,frzmem		; init memory pointers

	if	iver<iver3
	jsr	s1e36
	endif

	dmov	frzmem,acc		; read log page 0 to first frozen page
	dmovi	$0000,acb

	jsr	drdbkf
	if	iver<iver3
	jcs	start
	endif

	ldy	#hdrfrz+1		; setup frozen storage page count
	lda	#$ff			; bump up to page boundary - 1
	sta	(frzmem),y
	dey
	lda	(frzmem),y
	sta	frzpgs
	inc	frzpgs

	lda	#$00			; read in rest of frozen memory
l0897:	add	,#$01
	tax
	adc	frzmem+1
	sta	acc+1
	mov	frzmem,acc
	txa
	cmpbe	frzpgs,l08b6
	pha
	sta	acb
	mov	#$00,acb+1

	jsr	drdbkf
	if	iver<iver3
	jcs	start
	endif

	pla
	jmp	l0897

l08b6:	ldy	#hdrtyp
	lda	(frzmem),y

	if	iver>=iver3b
	ora	#$20			; signal game that windows are available
	sta	(frzmem),y
	lda	(frzmem),y
	endif

	if	iver<iver3
	and	#$01
	eor	#$01
	beq	l090a
	else
	and	#$02			; setup for proper type of status line
	sta	stltyp
	endif

	ldy	#hdrstr+1		; init PC
	lda	(frzmem),y
	sta	prgidx
	dey
	lda	(frzmem),y
	sta	prglpg
	mov	#$00,prglpg+1

	ldy	#hdrgbv+1		; init global variable pointer
	lda	(frzmem),y
	sta	glbvar
	dey
	lda	(frzmem),y
	clc
	adc	frzmem+1
	sta	glbvar+1

	if	iver>=iver2
	ldy	#hdrsbw+1		; init sub-word table pointer
	lda	(frzmem),y
	sta	sbwdpt
	dey
	lda	(frzmem),y
	clc
	adc	frzmem+1
	sta	sbwdpt+1
	endif

	mov	#$00,swpmem		; swpmem := frzmem + 256 * frzpgs
	add	frzpgs,frzmem+1,swpmem+1

	jsr	fndmem			; determine nnumber of pages of memory
	sub	,swpmem+1		; swppgs := (maxmem - swpmem) / 256
	bcc	l090a			; if swppgs < 0 then fatal error
	tay
	iny
	sty	swppgs
	tay
	sty	lrupag
	lda	#$ff
	sta	(vmtab3),y

	if	iver==iver1
	sta	rndbuf
	sta	rndbuf+1
	sta	rndbuf+2
	sta	rndbuf+3

	lda	#$05
l0917:	pha
	jsr	getrnd
	pla
	sec
	sbc	#$01
	bne	l0917
	endif

	jmp	mnloop			; start the game!

l090a:	jsr	fatal


; class C instructions (implicit or no operand)

optab1:	fdb	oprtnt			; return with TRUE
	fdb	oprtnf			; return with FALSE
	fdb	oppsi			; print string immediate
	fdb	oppsic			; print string immediate, CRLF, return true
	fdb	opnull			; no-op
	fdb	opsvgm			; save game status to disk
	fdb	oprsgm			; restore game status from disk

	if	iver<iver3
	fdb	oprstg			; restart game
	else
	fdb	start			; restart game
	endif

	fdb	oprtnv			; return with value
	fdb	opdrop			; drop a word from the stack
	fdb	opends			; end the game
	fdb	opcrlf			; print CRLF

	if	iver>=iver3
	fdb	opprst			; print status line
	fdb	opcksm			; checksum the program
	endif

opmax1	equ	(*-optab1)/2


; class B instructions (single operand)

optab2:	fdb	optstz			; compare ARG1=0 (ARG1<>0)
	fdb	opgtsb			; get thing's sibling
	fdb	opgtch			; get thing's child
	fdb	opgtpr			; get thing's parent
	fdb	opgtpl			; get length of property (given addr)
	fdb	opinc			; increment variable
	fdb	opdec			; decrement variable
	fdb	oppsb			; print string at byte address
	fdb	opfatl
	fdb	opdstt			; destroy thing
	fdb	opprtn			; print thing name
	fdb	oprtn			; return
	fdb	opjump			; unconditional jump
	fdb	oppsw			; print string at word address
	fdb	opmove			; move var ARG1 to var
	fdb	opnot			; 1's complement
opmax2	equ	(*-optab2)/2


; class A instructions (variable number of operands, may use short form
; opcode)

optab3:	fdb	opfatl
	fdb	opmtch			; match ARG1 against ARG2, ARG3, or ARG4
	fdb	l0eb7			; ??? compare ARG1<=ARG2 (ARG1>ARG2)
	fdb	l0ecf			; ??? compare ARG1>=ARG2 (ARG1<ARG2)
	fdb	opdecb			; decrement variable and branch
	fdb	opincb			; increment variable and branch
	fdb	optint			; is thing ARG1 in thing ARG2
	fdb	l0f23
	fdb	opor			; logical OR
	fdb	opand			; logical AND
	fdb	optsta			; test thing attribute
	fdb	opseta			; set thing attribute
	fdb	opclra			; clear thing attribute
	fdb	l0f97			; move ARG2 into var ARG1
	fdb	opmovt			; move thing ARG1 into thing ARG2
	fdb	opgtwd			; get a word
	fdb	opgtby			; get a byte
	fdb	opgtp			; get thing property
	fdb	opgtpa			; get address of property
	fdb	opgtnp			; get next property
	fdb	opadd			; add
	fdb	opsub			; subtract
	fdb	opmul			; multiply
	fdb	opdiv			; divide
	fdb	oprmd			; remainder
	if	iver==iver1
	fdb	oppsbi			; print string at indexed byte address
	endif
opmax3	equ	(*-optab3)/2


; class D instructions (variable number of operands)

optab4:	fdb	opcall			; call procedure
	fdb	opptwd			; store a word
	fdb	opptby			; store a byte
	fdb	opptp			; store into thing property
	fdb	opgtln			; get a line of input
	fdb	opprch			; print a character
	fdb	opprnm			; print number
	fdb	oprndm			; generate random number
	fdb	oppush			; push ARG1 to stack
	fdb	oppull			; pull var from stack
	if	iver>=iver3b
	fdb	x_opsplw		; split widnow
	fdb	x_opsetw		; set window
	endif
opmax4	equ	(*-optab4)/2


mnloop:
	if	iver>=iver3a
	lda	d2004
	bne	l09a9
	ldy	#hdrflg+1
	lda	(frzmem),y
	and	#$01
	beq	l09a9
	jsr	s2067
l09a9:
	endif

	if	iver<iver3
	mov	prgidx,prgids
	dmov	prglpg,prglps
	endif

	mov	#$00,argcnt		; default no arguments

	jsr	ftprba			; get opcode
	sta	opcode

	cmpjl	#$80,opcgpa		; is it class A ($00-$7F)?
	cmpjl	#$b0,opcgpb		; how about class B ($80-$AF)?
	cmpbl	#$c0,opcgpc		; perhaps class C ($B0-$BF)?
;	JMP	OPCGPD			; nope, it's class D ($C0-$FF).


; process opcode group D ($C0-$FF)

opcgpd:	jsr	ftprba			; get operand specification byte

	ldx	#$00			; init operand count

l09af:	pha	 			; save the operand specification byte
	tay				;   in Y and on stack

	txa				; save operand count on stack
	pha

	tya				; get back operand specification byte
	and	#$c0			; look at top two bits

	jsreq	ftprwd,l09d7		; if they're 00, operand is word immed.
	cmpjse	#$80,gtvarp,l09d7	; 10? variable
	cmpjse	#$40,ftprby,l09d7	; 01? byte immediate

	pla				; must be 11, no more operands
	pla				; pull operand spec byte and count
	jmp	l09ed			; and finish up

l09d7:	pla				; get operand count back
	tax				; to use as index

	lda	acc			; store operand in proper ARG locatoin
	sta	arg1,x
	lda	acc+1
	sta	arg1+1,x

	inx				; increment ARG pointer
	inx
	inc	argcnt			; and count

	pla				; pull arg spec byte
	sec				; shift top two bits off left, while
	rol	a			; shifting 11 in from right (to
	sec				; indicate no more operands)
	rol	a

	jmp	l09af			; try for another

l09ed:	dmovi	optab4,acc		; assume class D
	lda	opcode			; but if it's $C0-$DF then it's class A
	cmpjl	#$e0,l0a98

	sbc	#$e0			; adjust to $00..$1F
	cmp	#opmax4			; make sure it's not illegal

	if	iver<iver3
	jge	opfatl
	else
	bge	l0a2b
	endif

godoit:	asl	a			; get address from table (base in ACC)
	tay				;   word indexed by A and execute

	if	iver<iver3

	lda	(acc),y
	sta	acb
	iny
	lda	(acc),y
	sta	acb+1
	jsr	s7c			; call a patch point in zero page, normally an rts
	jmp	(acb)

	else

	lda	(acc),y
	sta	dsptch+1
	iny
	lda	(acc),y
	sta	dsptch+2
dsptch:	jsr	dsptch
	jmp	mnloop

	endif

; process opcode group C ($80-$BF)

opcgpc:	sub	,#$b0			; adjust to $00..$0F
	cmp	#opmax1			; make sure it's not illegal

	if	iver<iver3
	jge	opfatl
	else
	bge	l0a2b
	endif

	pha				; save it temp.
	dmovi	optab1,acc		; get base address of proper table
	pla
	jmp	godoit

	if	iver>=iver3
l0a2b:	jsr	fatal			; oops!  illegal opcode
	endif


; process opcode group B ($80-$AF)

opcgpb:	and	#$30			; mask off operand type bits

	jsreq	ftprwd,l0a45		; 00?  then it's word immediate
	cmpjse	#$10,ftprby,l0a45	; 01?  byte immediate
	jsr	gtvarp			; must be 10, variable

l0a45:	mov	#$01,argcnt		; one argument
	dmov	acc,arg1

	lda	opcode			; adjust opcode to $00..$0F
	and	#$0f
	cmp	#opmax2			; make sure it's not illegal

	if	iver<iver3
	jge	opfatl
	else
	bge	l0a2b
	endif

	pha				; save tmep.
	dmovi	optab2,acc		; get appropriate table base addr
	pla
	jmp	godoit			; and go do it!


; process opcode group A ($00-$7F)

opcgpa:	and	#$40			; get type bit for ARG1
	jsreq	ftprby,l0a73		; 0:  byte immediate
	jsr	gtvarp			; 1:  variable/stack
l0a73:	dmov	acc,arg1		; save it

	lda	opcode			; get type bit for ARG2
	and	#$20
	jsreq	ftprby,l0a8a		; 0:  byte immediate
	jsr	gtvarp			; 1:  variable/stack
l0a8a:	dmov	acc,arg2		; save it

	mov	#$02,argcnt		; indicate two operands

	lda	opcode			; get opcode back
l0a98:	and	#$1f			; adjust to $00..$1F
	cmp	#opmax3			; make sure it's not illegal

	if	iver<iver3
	jge	opfatl
	else
	bge	l0a2b
	endif

	pha				; save temp.
	dmovi	optab3,acc		; get base addr of appropriate table
	pla
	jmp	godoit			; and go do it!


; fetch byte immediate into ACC

ftprby:	jsr	ftprba			; get a byte form program into A
	sta	acc			; sero-fill to 16 bits in ACC
	mov	#$00,acc+1
	rts				; return


; fetch word immediate into ACC

ftprwd:	jsr	ftprba			; get high byte from program into A
	pha				; save it temp.
	jsr	ftprba			; get low byte from program into A
	sta	acc			; store low byte
	pul	acc+1			; store high byte
	rts				; return


gtvra1:	tstabe	l0ad0			; fetch ACC from var in A, keep if stack
	jmp	gtvara


ptvra1:	tstabe	l0ad6			; store ACC into var in A, replace if stack
	jmp	ptvara

l0ad0:	jsr	pullwd			; read stack non-destructive
	jmp	pushwd

l0ad6:	dpsh	acc			; replace TOS w/ ACC
	jsr	pullwd
	dpul	acc
	jmp	pushwd


gtvarp:	jsr	ftprba			; fetch ACC from var ind. by program
	tstabe	l0b26
gtvara:	cmpbg	#$10,l0b02		; fetch ACC from var in A
	sub	,#$01
	asl	a
	tax
	lda	locvar,x
	sta	acc+1
	inx
	lda	locvar,x
	sta	acc
	rts

l0b02:	sub	,#$10
	asl	a
	sta	acb
	lda	#$00
	rol	a
	sta	acb+1
	dadd	glbvar,acb,acb
	ldy	#$00
	lda	(acb),y
	sta	acc+1
	iny
	lda	(acb),y
	sta	acc
	rts

l0b26:	jsr	pullwd
	rts


ptvrpz:	lda	#$00			; store 0 in var. ind. by program
ptvrpa:	sta	acc			; store byte in A in var. ind. by prog.
	mov	#$00,acc+1


ptvrp1:
	if	iver<iver3
	jsr	ptvarp
	jmp	mnloop
	else
	jmp	ptvarp			; unnecessary!!!
	endif

ptvarp:	dpsh	acc			; store ACC in var. ind. by program
	jsr	ftprba
	tax
	dpul	acc
	txa
ptvara:	tstaje	pushwd			; store ACC in var. in A
	cmpbg	#$10,l0b60
	deca
	asl	a
	tax
	lda	acc+1
	sta	locvar,x
	inx
	lda	acc
	sta	locvar,x
	rts

l0b60:	sub	,#$10
	asl	a
	sta	acb
	lda	#$00
	rol
	sta	acb+1
	dadd	glbvar,acb,acb
	ldy	#$00
	lda	acc+1
	sta	(acb),y
	iny
	lda	acc
	sta	(acb),y
	rts


predtr:	jsr	ftprba			; fetch first displacement byte
	tstabm	l0b9c			; complement condition if necessary
	bpl	l0b94

predfl:	jsr	ftprba			; fetch first displacement byte
	tstabp	l0b9c			; complement condition if necessary
;	BMI	L0B94

l0b94:	and	#$40			; branch not taken
	jsreq	ftprba			; fetch second displacement byte if
					; necessary and discard it
	rtop				; done

l0b9c:	tax				; branch take, save first disp. byte
	and	#$40			; do we need a second byte?
	beq	l0bad			; yes
	txa				; no, extend what we have w/ zeros
	and	#$3f
	sta	acc
	mov	#$00,acc+1
	jmp	l0bc3			; and go do it!

l0bad:	txa				; get rest of displacement
	and	#$3f
	pha
	jsr	ftprba
	sta	acc
	pul	acc+1
	and	#$20
	beq	l0bc3
	lda	acc+1
	ora	#$c0
	sta	acc+1

l0bc3:	dtstbe	acc,oprtnf		; if displacement = 0, return false
	ddec	acc
	dtstbe	acc,oprtnt		; if displacement = 1, return true
l0bda:	ddec	acc

	mov	acc+1,acb		; copy high byte of displacement to ACB
	asl	a			; and sign extend to 17 bits
	lda	#$00
	rol	a
	sta	acb+1

	add	prgidx,acc		; add low byte of displacement to PC
	bcc	l0bfc			; increment high 8 bits of displacement
	dinc	acb			;   if overflow
l0bfc:	sta	prgidx

	dtstbe	acb,l0c17		; if high 9 bits of disp. =0, all done

	clc				; add hgih 9 bits of disp. to PC log page
	lda	acb
	adc	prglpg
	sta	prglpg
	lda	acb+1
	adc	prglpg+1
	and	#$01			; mod 2^17
	sta	prglpg+1

	mov	#$00,prgupd		; indicate page chagne

					; all done
	if	iver<iver3
	jmp	mnloop
l0c17:	jmp	mnloop
	else
l0c17:	rts
	endif


oprtnt:	lda	#$01			; return true ($01)
l0c1a:	sta	arg1			; return byte in A
	mov	#$00,arg1+1		; make high byte of return value $00
	jmp	oprtn			; and do the return

oprtnf:	lda	#$00			; return false ($00)
	jmp	l0c1a


	if	iver<iver3
oppsi:	jsr	psi
	jmp	mnloop
	else
oppsi:
	endif
	

psi:	mov	prgidx,auxidx		; copy PC to AUX
	dmov	prglpg,auxlpg
	mov	#$00,auxupd		; indicate new log. page

	jsr	prntst			; print the string

	mov	auxidx,prgidx		; copy AUX back to PC
	dmov	auxlpg,prglpg
	mov	auxupd,prgupd
	dmov	auxmpt,prgmpt

	if	iver>=iver3
opnull:
	endif

	rts


oppsic:	jsr	psi			; print string immediate

	lda	#crchar			; print CRLF (could use JSR OPCRLF)
	jsr	bfchar
	lda	#lfchar
	jsr	bfchar
	
	jmp	oprtnt			; return true


	if	iver<iver3
opnull:	jmp	mnloop
	endif


oprtnv:	jsr	pullwd			; pull value off stack
	dmov	acc,arg1		; save it for posterity
	jmp	oprtn			; return with it


	if	iver<iver3
opdrop:	jsr	pullwd
	jmp	mnloop
	endif	


opcrlf:
	lda	#crchar			; print CRLF
	jsr	bfchar
	lda	#lfchar

	if	iver<iver3
	jsr	bfchar
	jmp	mnloop
	else
	jmp	bfchar			; implicit RTS
	endif


	if	iver>=iver3

	if	iver>iver3
ivmsg:	fcb	"INTERPRETER VERSION : "
	fcb	'@' + (iver & $ff)
	fcb	crchar,$ff
	endif

opcksm:
	if	iver>=iver3a
; message output loop clearly added by someone unfamiliar with codebase
	dmovi	ivmsg,acc
	ldy	#$00
l0cb9:	lda	(acc),y
	cmp	#$ff
	beq	l0cc8
	eor	#$80
	jsr	cout
	iny
	jmp	l0cb9
l0cc8:
	endif

	ldy	#hdrcka+1		; get checksum end log. address (word
	lda	(frzmem),y		;   index)
	sta	arg2
	dey
	lda	(frzmem),y
	sta	arg2+1

	mov	#$00,arg3,arg1,arg1+1,acc+1,arg4	; initialize everything

	mov	#arg4,l1807+1		; patch VM routine to swap  in all pages

	asl	arg2			; convert end address to byte index
	rol	arg2+1
	rol	arg3

	mov	#$40,acc		; start at log. address $00040
	jsr	setaxb

l0ca5:	jsr	ftaxba			; get a byte
	daddb2	arg1			;   and add it to checksum

	lda	auxidx			; compare AUX to end address
	cmpbn	arg2,l0ca5		; if not done, loop
	lda	auxlpg
	cmpbn	arg2+1,l0ca5
	lda	auxlpg+1
	cmpbn	arg3,l0ca5

	mov	#frzpgs,l1807+1		; unpatch VM routine

	ldy	#hdrckv+1		; compare computed vs. expected checksum
	lda	(frzmem),y
	cmpbn	arg1,l0cda
	dey
	lda	(frzmem),y
	cmpje	arg1+1,predtr

l0cda:	jmp	predfl

	endif


optstz:	dtstjn	arg1,predfl
l0ce6:	jmp	predtr

opgtsb:	lda	arg1			; get sibling of thing, predicate
	jsr	setupt
	ldy	#thgsib
	jmp	l0cfa

opgtch:	lda	arg1			; get child of thing, predicate
	jsr	setupt
	ldy	#thgchd
l0cfa:	lda	(acc),y
	pha
	sta	acc
	mov	#$00,acc+1
	jsr	ptvarp
	pla
	tstabn	l0ce6
	jmp	predfl

opgtpr:	lda	arg1			; get parent of thing
	jsr	setupt
	ldy	#thgpar
	lda	(acc),y
	sta	acc
	mov	#$00,acc+1
	jmp	ptvrp1

opgtpl:	dadd	arg1,frzmem,acc
	ddec	acc
	ldy	#$00
	jsr	gtplen
	add	,#$01
	jmp	ptvrpa


	if	iver<iver3

opinc:	jsr	incvar
	jmp	mnloop

opdec:	jsr	decvar
	jmp	mnloop
	
	endif


	if	iver>=iver3
opinc:
	endif

; increment variable ARG1
incvar:	lda	arg1
	jsr	gtvra1
	dinc	acc
l0d4e:	dpsh	acc
	lda	arg1
	jsr	ptvra1
	dpul	acc
	rts


	if	iver>=iver3
opdec:
	endif

; decrement variable ARG1

decvar:	lda	arg1
	jsr	gtvra1
	ddec	acc
	jmp	l0d4e


; print string at byte address in ARG1

oppsb:	dmov	arg1,acc		; set AUX to point to string at
oppsb2:	jsr	setaxb			;   byte address
	jmp	l0e9d			; and print it!


	if	iver==iver1
opfatl:	jmp	fatal
	elseif	iver==iver2
opfatl:	jsr	fatal
	endif


opdstt:
	if	iver<iver3
	jsr	dstthg
	jmp	mnloop
	endif

; destroy thing ARG1 (move to location 0)

dstthg:	lda	arg1
	jsr	setupt
	ldy	#thgpar
	lda	(acc),y
	rtseq
	tax
	dpsh	acc
	txa
	jsr	setupt
	ldy	#thgchd
	lda	(acc),y
	cmpbn	arg1,l0db7
	dpul	acb
	dpsh	acb
	ldy	#thgsib
	lda	(acb),y
	ldy	#thgchd
	sta	(acc),y
	jmp	l0dd2
l0db7:	jsr	setupt
	ldy	#thgsib
	lda	(acc),y
	cmpbn	arg1,l0db7
	dpul	acb
	dpsh	acb
	lda	(acb),y
	sta	(acc),y
l0dd2:	dpul	acc
	ldy	#thgpar
	lda	#$00
	sta	(acc),y
	iny				; to THGSIB
	sta	(acc),y
	rts


opprtn:	lda	arg1

	if	iver<iver3
	jsr	prtnam
	jmp	mnloop
	endif


; print thing name
prtnam:	jsr	setupt			; set up pointer to thing

	ldy	#thgprp			; get address of thing's property list
	lda	(acc),y
	sta	acb+1
	iny
	lda	(acc),y
	sta	acb
	dmov	acb,acc

	dinc	acc			; skip name length byte

	jsr	setaxb			; set AUX to point to it
	jmp	prntst			; and print it and return


oprtn:	dmov	stkpsv,stkpnt		; restore pre-call stack pointer, count
	mov	stkcsv,stkcnt

	jsr	pullwd			; are there any local variables to restore?
	lda	acc
	beq	l0e4c			; no, skip it

	dmovi	locvar-2,acb		; yes, calc. addr. of last var to restore
	mov	acc,acd
	asl	a
	daddb2	acb

l0e2f:	jsr	pullwd			; pull the value of the var
	ldy	#$01			; store it in the var
	lda	acc
	sta	(acb),y
	dey
	lda	acc+1
	sta	(acb),y
	ddec2	acb			; decrement the var pointer
	decbn	acd,l0e2f		; and the count and loop if more to do

l0e4c:	jsr	pullwd			; pull the PC log. page
	dmov	acc,prglpg

	jsr	pullwd			; pull the stack pointer save
	dmov	acc,stkpsv

	jsr	pullwd			; pull the stack count save and PC
	mov	acc+1,prgidx		; low byte
	mov	acc,stkcsv

	mov	#$00,prgupd		; indicate need to locate new page

	dmov	arg1,acc		; store the return value and return
	jmp	ptvrp1


; jump to address ARG1

opjump:	dmov	arg1,acc		; setup to jump into middle of
	ddec	acc			; predicate routine
	jmp	l0bda			; and do it!


oppsw:	dmov	arg1,acc		; set AUX to point to string at
	jsr	setaxw			;   word address

l0e9d:
	if	iver<iver3
	jsr	prntst			; and print it!
	jmp	mnloop
	else
	jmp	prntst			; and print it!
	endif


opmove:	lda	arg1			; get number of first variable
	jsr	gtvra1			; get its contents
	jmp	ptvrp1			; store into another variable


opnot:	d1comp	arg1,acc
	jmp	ptvrp1

l0eb7:	dmov	arg1,acc
	dmov	arg2,acb
	jsr	l16de
	bcc	l0f10
	jmp	predfl

l0ecf:	dmov	arg1,acb
	dmov	arg2,acc
	jsr	l16de
	bcc	l0f10
	jmp	predfl

opdecb:
	if	iver<iver3
	jsr	decvar
	else
	jsr	opdec
	endif
	dmov	arg2,acb
	jmp	l0f08

opincb:
	if	iver<iver3
	jsr	incvar
	else
	jsr	opinc
	endif
	dmov	acc,acb
	dmov	arg2,acc
l0f08:	jsr	l16de
	jcs	predfl
l0f10:	jmp	predtr

optint:	lda	arg1
	jsr	setupt
	ldy	#$04
	lda	arg2
	cmp	(acc),y
	beq	l0f10
	jmp	predfl

l0f23:	mov	arg2+1,acc+1
	and	arg1+1
	sta	acb+1
	mov	arg2,acc
	and	arg1
	sta	acb
	jsr	l16e9
	beq	l0f10
	jmp	predfl

opor:	dor	arg2,arg1,acc
	jmp	ptvrp1

opand:	dand	arg2,arg1,acc
	jmp	ptvrp1


; test attribute bit ARG2 of thing ARG1

optsta:	jsr	setupa
	lda	acb+1
	and	acd+1
	sta	acb+1
	lda	acb
	and	acd
	ora	acb+1
	bne	l0f10
	jmp	predfl


; set attribute bit ARG2 of thing ARG1

opseta:	jsr	setupa
	ldy	#$01
	lda	acb
	ora	acd
	sta	(acc),y
	dey
	lda	acb+1
	ora	acd+1
	sta	(acc),y
	rtop


; clear attribute bit ARG2 of thing ARG1

opclra:	jsr	setupa
	ldy	#$01
	lda	acd
	eor	#$ff
	and	acb
	sta	(acc),y
	dey
	lda	acd+1
	eor	#$ff
	and	acb+1
	sta	(acc),y
	rtop


l0f97:	dmov	arg2,acc
	lda	arg1
l0fa1:
	if	iver<iver3
	jsr	ptvra1
	jmp	mnloop
	else
	jmp	ptvra1
	endif


opmovt:	jsr	dstthg
	lda	arg1
	jsr	setupt
	dpsh	acc
	ldy	#thgpar
	lda	arg2
	sta	(acc),y
	jsr	setupt
	ldy	#thgchd
	lda	(acc),y
	tax
	lda	arg1
	sta	(acc),y
	dpul	acc
	txa
	beq	l0fd1
	ldy	#thgsib
	sta	(acc),y
l0fd1:
	rtop


opgtwd:	dasl	arg2
	dadd	arg2,arg1,acc
	jsr	setaxb
	jsr	ftaxwd
	jmp	ptvrp1

opgtby:	dadd	arg2,arg1,acc
	jsr	setaxb
	jsr	ftaxba
	sta	acc
	mov	#$00,acc+1
	jmp	ptvrp1


; get property ARG2 of thing ARG1

opgtp:	jsr	setupp
l100b:	jsr	gtpnum
	cmpbe	arg2,l103b
	jsrcs	advppt,l100b
	ldy	#hdrthg+1
	clc
	lda	(frzmem),y
	adc	frzmem
	sta	acb
	dey
	lda	(frzmem),y
	adc	frzmem+1
	sta	acb+1
	lda	arg2
	asl	a
	tay
	dey
	lda	(acb),y
	sta	acc
	dey
	lda	(acb),y
	sta	acc+1
	jmp	ptvrp1

l103b:	jsr	gtplen
	iny
	cmpbe	#$00,l105e
	cmpjsn	#$01,fatal
	lda	(acc),y
	sta	acb+1
	iny
	lda	(acc),y
	sta	acb
	dmov	acb,acc
	jmp	ptvrp1
l105e:	lda	(acc),y
	sta	acc
	mov	#$00,acc+1
	jmp	ptvrp1


; get address of property ARG2 of thing ARG1

opgtpa:	jsr	setupp
l106c:	jsr	gtpnum
	cmpbe	arg2,l107e
	jcc	ptvrpz
	jsr	advppt
	jmp	l106c
l107e:	dinc	acc
	clc
	tya
	adc	acc
	sta	acc
	bcc	l10be
	inc	acc+1
l10be:	dsub	acc,frzmem,acc
	jmp	ptvrp1


; get number of next property of thing ARG1 after property ARG2

opgtnp:	jsr	setupp
	lda	arg2
	beq	l10b7
l10a5:	jsr	gtpnum
	cmpbe	arg2,l10bd
	jcc	ptvrpz
	jsr	advppt
	jmp	l10a5
l10b7:	jsr	gtpnum
	jmp	ptvrpa
l10bd:	jsr	advppt
	jmp	l10b7


; add ARG1 and ARG2

opadd:	dadd	arg1,arg2,acc
	jmp	ptvrp1


; subtract ARG2 from ARG1

opsub:	dsub	arg1,arg2,acc
	jmp	ptvrp1


; multiply ARG1 by ARG2

opmul:	dmov	arg1,acc
	dmov	arg2,acb
	jsr	l15fb
	lda	acb+1
	bne	l1104
	lda	acb
	cmpbe	#$02,l1111
	cmpbe	#$04,l110d
l1104:	jsr	l1568
l1107:	jsr	l160a
	jmp	ptvrp1
l110d:	dasl	acc
l1111:	dasl	acc
	jmp	l1107


; divide ARG1 by ARG2

opdiv:	dmov	arg1,acc
	dmov	arg2,acb
	jsr	l15fb
	lda	acb+1
	bne	l1139
	lda	acb
	cmpbe	#$02,l1143
	cmpbe	#$04,l113f
l1139:	jsr	divide
	jmp	l1107
l113f:	dlsr	acc
l1143:	dlsr	acc
	jmp	l1107


; get remainder of ARG1 divided by ARG2

oprmd:	dmov	arg1,acc
	dmov	arg2,acb
	jsr	l15fb
	jsr	divide
	dmov	acb,acc
	jmp	ptvrp1


	if	iver==iver1
; print string at indexed byte address (arg+arg2)
oppsbi:	clc
	lda	arg2
	adc	arg1
	sta	acc
	lda	arg2+1
	adc	arg1+1
	sta	acc+1
	jmp	oppsb2
	endif


	if	iver>=iver3b
; these jumps are ridiculous; opcode jump table could jump directly
; to opsetw and opsplw
x_opsetw:
	jmp	opsetw

x_opsplw:
	jmp	opsplw
	endif


; test whether ARG1 is equal to any of the other args

opmtch:	ldx	argcnt
	dxbne	l1173
	jsr	fatal
l1173:	lda	arg1
	cmpbn	arg2,l117f
	lda	arg1+1
	cmpbe	arg2+1,l11a0
l117f:	dxbeq	l119d
	lda	arg1
	cmpbn	arg3,l118e
	lda	arg1+1
	cmpbe	arg3+1,l11a0
l118e:	dxbeq	l119d
	lda	arg1
	cmpbn	arg4,l1173
	lda	arg1+1
	cmpbe	arg4+1,l11a0
l119d:	jmp	predfl
l11a0:	jmp	predtr


; call procedure at addr. ARG1 and optionally pass ARG2, ARG3, and ARG4
; as arguments

opcall:	dts2bn	arg1,l11b4		; if argument 1 (call address/2) is
	dmovi	$0000,acc		; zero, just put zero in var
	jmp	ptvrp1			; these three lines could be replaced
					; with "DTS2BE PTVRPZ"

l11b4:	mov	stkcsv,acc		; push the stack count save and low byte
	mov	prgidx,acc+1		; of the PC
	jsr	pushwd

	dmov	stkpsv,acc		; push the stack pointer save
	jsr	pushwd

	dmov	prglpg,acc		; push the PC logical page
	jsr	pushwd

	mov	#$00,prgupd		; indicate need to search for new page

	dasl	arg1,prgidx		; make new PC := ARG1 * 2
	lda	#$00
	rol	a
	sta	prglpg+1

	jsr	ftprba			; get first byte of routine
	pha				; and save it

	tstabe	l1220			; if it's zero, no local variables

; push the local variables the routine will use

	ldx	#$00
l11f2:	pha
	lda	locvar,x
	sta	acc+1
	inx
	lda	locvar,x
	sta	acc
	dex
	txa
	pha
	jsr	pushwd
	jsr	ftprba
	pha
	jsr	ftprba
	sta	acc
	pul	acc+1
	pla
	tax
	lda	acc+1
	sta	locvar,x
	inx
	lda	acc
	sta	locvar,x
	inx
	pla
	sub	,#$01
	bne	l11f2

l1220:	mov	argcnt,acd		; do we pass any parameters?
	decbe	acd,l124c		; no

	mov	#$00,acb		; yes, copy them in
	mov	#$00,acc
l1230:	ldx	acb
	lda	arg2+1,x
	ldx	acc
	sta	locvar,x
	inc	acc
	ldx	acb
	lda	arg2,x
	ldx	acc
	sta	locvar,x
	inc	acc
	inc	acb
	inc	acb

	decbn	acd,l1230		; loop if more parameters to pass

l124c:	pul	acc			; get first porgram byte again
	jsr	pushwd			; and push it so return can restore
					; the local variables

	mov	stkcnt,stkcsv		; save the stack pointer and count
	dmov	stkpnt,stkpsv

	rtop


; store word ARG3 at log. addr. ARG2 (offset) * 2 + ARG1 (base)
; should have test to insure no overrun of end of frozen storage!

opptwd:	lda	arg2			; calculate logical address
	asl	a
	rol	arg2+1
	clc
	adc	arg1
	sta	acc
	lda	arg2+1
	adc	arg1+1
	sta	acc+1

	dadd	acc,frzmem,acc		; add base of frozen mem. to get phys. addr.

	ldy	#$00			; store the word
	lda	arg3+1
	sta	(acc),y
	iny
	lda	arg3
	sta	(acc),y

	rtop


; store byte ARG3 at log. addr. ARG2 (offset) + ARG1 (base)
; should have test to insure no overrun of end of frozen storage!

opptby:	lda	arg2			; calculate logical address
	clc
	adc	arg1
	sta	acc
	lda	arg2+1
	adc	arg1+1
	sta	acc+1

	dadd	acc,frzmem,acc		; add base of frozen mem. to get phys addr.

	ldy	#$00			; store the byte
	lda	arg3
	sta	(acc),y

	rtop


; store ARG3 as property of ARG2 of thing ARG1

opptp:	jsr	setupp			; setup for thing property operations

l12ac:	jsr	gtpnum			; get the property number
	cmp	arg2			; if it is the one, go do it!
	beq	l12be

	jsrcc	fatal			; oops! past it!

	jsr	advppt			; advance pointer
	jmp	l12ac			; and try again

; got the property we wand

l12be:	jsr	gtplen			; get property length
	iny
	cmp	#$00			; if it is byte sized, go store it
	beq	l12d7
	cmpjsn	#$01,fatal		; if it isn't word sized, fatal error

	lda	arg3+1			; yes, store high byte
	sta	(acc),y
	iny

	lda	arg3			; these three lines are unnecessary
	sta	(acc),y

	rtop

l12d7:	lda	arg3			; store low byte
	sta	(acc),y

	rtop


opgtln:	jsr	opprst
	if	iver==iver1
	jsr	getrnd
	endif
	dadd	arg1,frzmem,arg1
	dadd	arg2,frzmem,arg2
	jsr	getlin
	sta	acd+1
	mov	#$00,acd
	ldy	#$01
	lda	#$00
	sta	(arg2),y
	mov	#$02,le0
	mov	#$01,le1
l1310:	ldy	#$00
	lda	(arg2),y
	iny
	cmp	(arg2),y
	rtopeq

	lda	acd+1
	ora	acd
	rtopeq

	lda	acd
	cmpjse	#$06,l13ba
	lda	acd
	bne	l135c
	ldy	#$06
	ldx	#$00
l1332:	lda	#$00
	sta	inword,x
	inx
	dybne	l1332
	lda	le1
	ldy	le0
	iny
	iny
	iny
	sta	(arg2),y
	ldy	le1
	lda	(arg1),y
	jsr	l13f1
	bcs	l137a
	ldy	le1
	lda	(arg1),y
	jsr	l13e0
	bcc	l135c
	inc	le1
	dec	acd+1
	jmp	l1310
l135c:	lda	acd+1
	beq	l1382
	ldy	le1
	lda	(arg1),y
	jsr	l13da
	bcs	l1382
	ldy	le1
	lda	(arg1),y
	ldx	acd
	sta	inword,x
	dec	acd+1
	inc	acd
	inc	le1
	jmp	l1310
l137a:	sta	inword
	inc	acd
	dec	acd+1
	inc	le1
l1382:	lda	acd
	beq	l1310
	psh	acd+1
	ldy	le0
	iny
	iny
	lda	acd
	sta	(arg2),y
	jsr	crnwrd
	jsr	l141f
	ldy	le0
	lda	acb+1
	sta	(arg2),y
	iny
	lda	acb
	sta	(arg2),y
	iny
	iny
	iny
	sty	le0
	ldy	#$01
	lda	(arg2),y
	clc
	adc	#$01
	sta	(arg2),y
	pul	acd+1
	mov	#$00,acd
	jmp	l1310

l13ba:	lda	acd+1
	rtseq
	ldy	le1
	lda	(arg1),y
	jsr	l13da
	rtscs
	inc	le1
	dec	acd+1
	inc	acd
	jmp	l13ba

septab:	fcb	" .,?",crchar,lfchar,tbchar,ffchar

l13da:	jsr	l13f1
	rtscs
l13e0:	ldy	#$00
	ldx	#$08
l13e4:	cmp	septab,y
	beq	l13ef
	iny
	dxbne	l13e4
l13ed:	clc
	rts
l13ef:	sec
l13f0:	rts

l13f1:	pha
	jsr	gtvcba
	ldy	#$00
	lda	(acc),y
	tax
	pla
l13fb:	beq	l13ed
	iny
	cmp	(acc),y
	beq	l13ef
	dex
	jmp	l13fb

gtvcba:	ldy	#hdrvcb
	lda	(frzmem),y
	sta	acc+1
	iny
	lda	(frzmem),y
	sta	acc
	dadd	acc,frzmem,acc
	rts

l141f:	jsr	gtvcba
	ldy	#$00
	lda	(acc),y
	tay
	iny
	lda	(acc),y
	asl	a
	asl	a
	asl	a
	asl	a
	sta	acd
	iny
	lda	(acc),y
	sta	acb+1
	iny
	lda	(acc),y
	sta	acb
	iny
	tya
	add	,acc,acc
	bcc	l1445
	inc	acc+1
l1445:	ldy	#$00
	jmp	l1450

l144a:	lda	(acc),y
	cmpbg	pkword+1,l1470
l1450:	daddb1	acc,acd,acc

	if	iver<iver3

	sec
	lda	acb
	sbc	#$10
	sta	acb
	bcs	l144a
	dec	acb+1
	bpl	l144a

	else

	dsubb1	acb,#$10,acb
	lda	acb+1
	bmi	l1470
	bne	l144a
	lda	acb
	bne	l144a

	endif

l1470:	dsubb1	acc,acd,acc
	daddb1	acb,#$10,acb
	lda	acd
	lsr	a
	lsr	a
	lsr	a
	lsr	a
	sta	acd
l148e:	ldy	#$00
	lda	pkword+1
	cmp	(acc),y
	blt	l14d0
	bne	l14b4
	iny
	lda	pkword
	cmp	(acc),y
	blt	l14d0
	bne	l14b4
	ldy	#$02
	lda	pkword+3
	cmp	(acc),y
	blt	l14d0
	bne	l14b4
	iny
	lda	pkword+2
	cmp	(acc),y
	blt	l14d0
	beq	l14d7
l14b4:	daddb1	acc,acd,acc
	ddec	acb
	dts2bn	acb,l148e
l14d0:	mov	#$00,acb+1,acb
	rts
l14d7:	dsub	acc,frzmem,acb
	rts


; print ASCII character ARG1

opprch:	lda	arg1
	if	iver<iver3
	jsr	bfchar
	jmp	mnloop
	else
	jmp	bfchar
	endif


; print decimal number ARG1

opprnm:	dmov	arg1,acc
	if	iver<iver3
	jsr	prntnm
	jmp	mnloop
	else
	jmp	prntnm			; unnecessary
	endif


; print decimal number in ACC

prntnm:	lda	acc+1			; negative?
	jsrmi	l152e			;   yes, print '-' and negate
	mov	#$00,acd		; initialize digit count to 0
l150d:	dtstbe	acc,l1519		; if the remainder is zero, print it now
	dmovi	$000a,acb		; set up divisor of 10
	jsr	divide			; divide
	psh	acb			; push remainder onto stack
	inc	acd			; incrmeent digit count
	jmp	l150d			; do it again

l1519:	lda	acd			; is digit count zero?
	beq	l1529			;   yes, just print a '0' and return
l151d:	pla				; pull a digit off stack
	add	,#'0'			; convert to ASCII
	jsr	bfchar			; print it
	decbn	acd,l151d		; decrement digit count, loop if more
	rts				; return to caller

l1529:	lda	#'0'			; get code for '0'
	jmp	bfchar			; print it and return to caller

l152e:	lda	#'-'			; get code for '-'
	jsr	bfchar			; print it
	jmp	l1611			; negate the number, return


; get a random number from 1 to ARG1

oprndm:	dmov	arg1,acb		; save range
	jsr	getrnd			; get the "random" number
	jsr	divide			; divide by range
	dmov	acb,acc			; get the remainder
	dinc	acc			; increment it (base of result is 1)
	jmp	ptvrp1			; and store it


; push ARG1 on stack

oppush:	dmov	arg1,acc
	if	iver<iver3
	jsr	pushwd
	jmp	mnloop
	else
	jmp	pushwd
	endif


; pull stack into variable ARG1

oppull:	jsr	pullwd
	lda	arg1
	jmp	l0fa1


	if	iver==iver1
getrnd:	lda	#2
l1599:	pha
	ldy	#8
	ldx	#0
	lda	rndbuf,x
l15a0:	rol	a
	rol	a
	rol	a
	eor	rndbuf,x
	rol	a
	rol	a
	ldx	#0
	rol	rndbuf,x
	inx
	rol	rndbuf,x
	inx
	rol	rndbuf,x
	inx
	rol	rndbuf,x
	dey
	bne	l15a0
	pla
	sec
	sbc	#1
	bne	l1599
	lda	rndbuf+2
	sta	acc
	lda	rndbuf+3
	sta	acc+1
	lda	rndloc
	sta	rndbuf
	lda	rndloc+1
	sta	rndbuf+1
	rts
	endif


l1568:	dpsh	acd
	dmovi	$0000,acd
	ldx	#$10
l1578:	lda	acb
	clc
	and	#$01
	beq	l158b
	dadc	acc,acd,acd
l158b:	dror	acd
	dror	acb
	dxbne	l1578
	dmov	acb,acc
	dmov	acd,acb
	dpul	acd
	rts


; divide ACC by ACB, quotient to ACC, remainder to ACB

divide:	dpsh	acd
	dmov	acc,acd
	dmovi	$0000,acc
	ldx	#$11
l15c5:	sec
	lda	acc
	sbc	acb
	tay
	lda	acc+1
	sbc	acb+1
	bcc	l15d6
	sta	acc+1
	tya
	sta	acc
l15d6:	drol	acd
	drol	acc
	dxbne	l15c5
	clc
	dror	acc,acb
	dmov	acd,acc
	dpul	acd
	rts

l15fb:	mov	#$00,mdflag
	lda	acc+1
	jsr	l161f
	lda	acb+1
	jsr	l161f
	rts

l160a:	lda	mdflag
	and	#$01
	rtseq
l1611:	sec
	lda	#$00
	sbc	acc
	sta	acc
	lda	#$00
	sbc	acc+1
	sta	acc+1
	rts

l161f:	tstarp				; if positive, return
	inc	mdflag
	jmp	l1611


; setup stuff for thing attribute bit operations

setupa:	lda	arg1
	jsr	setupt
	lda	arg2
	cmpbl	#$10,l1643
	sub	,#$10
	dinc	acc
	dinc	acc
l1643:	sta	acb
	dmovi	$0001,acd
	sub	#$0f,acb
	tax
l1653:	beq	l165d
	dasl	acd
	dex
	jmp	l1653
l165d:	ldy	#$00
	lda	(acc),y
	sta	acb+1
	iny
	lda	(acc),y
	sta	acb
	rts


; setup stuff for thing property operations

setupp:	lda	arg1
	jsr	setupt
	ldy	#thgprp
	lda	(acc),y
	sta	acb+1
	iny
	lda	(acc),y
	sta	acb
	dadd	acb,frzmem,acc
	ldy	#$00
	lda	(acc),y
	asl	a
	tay
	iny
	rts


; get number of property pointed to by ACC

gtpnum:	lda	(acc),y
	and	#$1f
	rts


; get lenght of property pointed to by ACC

gtplen:	lda	(acc),y
	rept	5
	ror	a
	endm
	and	#$07
	rts


; advance ACC to point to next property

advppt:	jsr	gtplen
	tax
l16a1:	iny
	dxbpl	l16a1
	iny
	rts


; setup stuff for thing operations

setupt:	sta	acc
	mov	#$00,acc+1
	lda	acc
	rept	3
	dasl	acc
	endm
	add	,acc
	bcc	l16c3
	inc	acc+1
	clc
l16c3:	adc	#$35
	sta	acc
	bcc	l16cb
	inc	acc+1
l16cb:	ldy	#hdrthg+1
	lda	(frzmem),y
	clc
	adc	acc
	sta	acc
	dey
	lda	(frzmem),y
	adc	acc+1
	adc	frzmem+1
	sta	acc+1
	rts


l16de:	lda	acb+1
	eor	acc+1
	bpl	l16e9
	lda	acb+1
	cmp	acc+1
	rts
l16e9:	lda	acc+1
	cmpbn	acb+1,l16f3
	lda	acc
	cmp	acb
l16f3:	rts


pushwd:	ddec	stkpnt
	ldy	#$00
	lda	acc
	sta	(stkpnt),y
	ddec	stkpnt
	lda	acc+1
	sta	(stkpnt),y
	inc	stkcnt
	lda	stkcnt
	cmpjsg	#stckmx,fatal
	rts


	if	iver>=iver3
opdrop:
	endif

pullwd:	ldy	#$00
	lda	(stkpnt),y
	sta	acc+1
	dinc	stkpnt
	lda	(stkpnt),y
	sta	acc
	dinc	stkpnt
	dec	stkcnt
	jsreq	fatal
	rts


; fetch next byte from PC into A

ftprba:	lda	prgupd			; need to find a new page?
	beq	l1757			;   yes, go do it!

	ldy	prgidx			; get the byte
	lda	(prgmpt),y

	iny				; increment the low byte of the PC
	sty	prgidx
	rtsne				; return unless we've entered a new page

	ldy	#$00			; unnecessary!
	sty	prgupd			; indicate new page
	dinc	prglpg			; increment page number
	rts				; return

l1757:	lda	prglpg+1		; is the page we're looking for frozen?
	bne	l1761
	lda	prglpg
	cmpbl	frzpgs,l1778

l1761:	dmov	prglpg,acc		; no, see if it is swapped in
	jsr	fndpag
	sta	prgppg			; save phys. page no.
	bcs	l1788			; not found

; we have the swappable page, fix up the pointers, etc.

l1770:	jsr	mrkpag			; indicate that we're using this page

	clc				; add phys. page number to number
	lda	prgppg			; of frozen pages
	adc	frzpgs

; fix the memory pointers

l1778:	add	,frzmem+1,prgmpt+1	; add base of frozen memory
	mov	#$00,prgmpt

	mov	#$ff,prgupd		; indicate that we have the page
	jmp	ftprba			; and go get the byte

; we need to load the page from disk

l1788:	cmpbn	auxppg,l1790		; if we are about to load a new logical
	mov	#$00,auxupd		; page into the physical page AUX points
					; to, mark it as new page

l1790:	dmov	swpmem,acc		; setup to read the page
	add	prgppg,acc+1,acc+1
	dmov	prglpg,acb

	jsr	drdbkf			; read the page (die if error)
	if	iver<iver3
	jcs	start
	endif

	ldy	prgppg			; copy the new log. page number into
	lda	prglpg			; the VM table
	sta	(vmtab1),y
	lda	prglpg+1
	sta	(vmtab2),y

	tya
	jmp	l1770			; go fix up the pointers and fetch the byte


; set AUX to byte address in ACC

setaxb:	mov	acc,auxidx
	mov	acc+1,auxlpg
	mov	#$00,auxlpg+1
l17c4:	mov	#$00,auxupd
	rts


; set AUX to word address in ACC

setaxw:	lda	acc
	asl	a
	sta	auxidx
	lda	acc+1
	rol	a
	sta	auxlpg
	lda	#$00
	rol	a
	sta	auxlpg+1
	jmp	l17c4


	if	iver==iver1
l1846:	lda	#$00
	sta	auxupd
	lda	#$01
	jsr	fndpag
	sta	auxppg
	jsr	l1832
	jsr	mrkpag
	lda	auxppg
	clc
	adc	swpmem+1
	sta	auxmpt+1
	lda	swpmem
	sta	auxmpt
	rts
	endif


; fetch next word from AUX into ACC

ftaxwd:	jsr	ftaxba
	pha
	jsr	ftaxba
	sta	acc
	pul	acc+1
	rts


; fetch next byte from AUX into A

ftaxba:	lda	auxupd			; need to find a new page?
	beq	l1801			;   yes, go to it!

	ldy	auxidx			; get the byte
	lda	(auxmpt),y

	iny				; increment the low byte of AUX
	sty	auxidx
	rtsne				; return uness we've entered a new page

	ldy	#$00			; unnecessary!
	sty	auxupd			; indicate new page
	dinc	auxlpg			; increment page number
	rts				; return

l1801:	lda	auxlpg+1		; is the page we're looking for frozen?
	bne	l180b
	lda	auxlpg
l1807:	cmpbl	frzpgs,l1822

l180b:	dmov	auxlpg,acc		; no, see if it is swapped in
	jsr	fndpag
	sta	auxppg			; save phys. page no.
	if	iver==iver1
	bcs	l183a
	else
	bcs	l1832			; not found
	endif

; we have the swappable page, fix up the pointers, etc.

l181a:	jsr	mrkpag			; indicate that we're using this page

	clc				; add phys. page number to number of
	lda	auxppg			; frozen pages
	adc	frzpgs

; fix the memory pointers

l1822:	add	,frzmem+1,auxmpt+1	; add base of memory
	mov	#$00,auxmpt

	mov	#$ff,auxupd		; indicate that we have the page
	jmp	ftaxba			; and go get the byte

; we need to load the page from disk

l1832:
	if	iver==iver1
	cmprn	prgppg
	tax
	lda	#$00
	sta	prgupd
	txa
	rts
	else
	cmpbn	prgppg,l183a		; if we are about to load a new logical
	mov	#$00,prgupd		; page into the physical page the PC
					; points to, mark it as a new page
	endif

l183a:	dmov	swpmem,acc		; setup to read the page
	add	auxppg,acc+1,acc+1
	dmov	auxlpg,acb

	jsr	drdbkf			; read the page (die if error)
	if	iver<iver3
	jcs	start
	endif

	ldy	auxppg			; copy the new log. page number into
	lda	auxlpg			; the VM table
	sta	(vmtab1),y
	lda	auxlpg+1
	sta	(vmtab2),y

	tya
	jmp	l181a			; go fix up the pointers and fetch the byte


; we've just started using a new logical page, move it to the front of our list
; this makes least recently used pages first candidates to be removed

mrkpag:
	if	iver==iver1
	cmp	mrupag
	bne	l18fa
	lda	mrupag
	rts
l18fa:
	else
	cmpbe	mrupag,l1891
	endif

	ldx	mrupag
	sta	mrupag
	tay
	lda	(vmtab3),y
	sta	acc
	txa
	sta	(vmtab3),y
	lda	(vmtab4),y
	sta	acc+1
	lda	#$ff
	sta	(vmtab4),y
	ldy	acc+1
	lda	acc
	sta	(vmtab3),y
	txa
	tay
	lda	mrupag
	sta	(vmtab4),y
	lda	acc
	cmpbe	#$ff,l1892
	tay
	lda	acc+1
	sta	(vmtab4),y
l1891:	rts
l1892:	mov	acc+1,lrupag
	rts


; search virtual memory table for logical page # in ACC

fndpag:	ldx	swppgs
	ldy	#$00
	lda	acc
l189d:	cmp	(vmtab1),y
	bne	l18a9
	lda	acc+1
	cmp	(vmtab2),y
	beq	l18b1
	lda	acc
l18a9:	iny
	dxbne	l189d
	lda	lrupag
	sec
	rts
l18b1:	tya
	clc
	rts


; print string at AUX

prntst:	mov	#$00,prmmod,pnybcn
	mov	#$ff,tmpmod
donext:	jsr	getnyb
	rtscs
	sta	acd
	beq	dospac
	if	iver==iver1
	cmpbe	#$01,docrlf
	cmpbl	#$04,newmod
	cmpbl	#$06,newmdl
	elseif	iver==iver2
	cmpbe	#$01,dosbwd
	cmpbl	#$04,newmod
	cmpbl	#$06,newmdl
	else
	cmpbl	#$04,dosbwd
	cmpbl	#$06,newmod
	endif
	jsr	tstmod
	tstabn	l18e2
	lda	#$5b
l18d9:	add	,acd
l18dc:	jsr	bfchar
	jmp	donext
l18e2:	cmpbn	#$01,dospcl
	lda	#$3b
	jmp	l18d9

dospcl:	sub	acd,#$07
	bcc	doasci
	if	iver>iver1
	beq	docrlf
	endif
	tay
	if	iver>iver1
	dey
	endif
	lda	spclch,y
	jmp	l18dc

doasci:	jsr	getnyb
	rept	5
	asl	a
	endm
	pha
	jsr	getnyb
	sta	acd
	pla
	ora	acd

	if	iver==iver1
	cmp	#tbchar
	bne	l18dc
	lda	#' '
	endif

	jmp	l18dc


dospac:	lda	#' '
	jmp	l18dc

docrlf:	lda	#crchar
	jsr	bfchar
	lda	#lfchar
	jmp	l18dc


	if	iver>=iver3
	
newmod:	sub	,#$03
	tay
	jsr	tstmod
	bne	l192d
	sty	tmpmod
	jmp	donext
l192d:	sty	prmmod
	cmpbe	prmmod,l1937
	ldy	#$00
	sty	prmmod
l1937:	jmp	donext

	else

newmod:	jsr	tstmod
	add	,#$02
	adc	acd
	jsr	wrapmd
	sta	tmpmod
	jmp	donext

newmdl:	jsr	tstmod
	add	,acd
	jsr	wrapmd
	sta	prmmod
	jmp	donext

	endif


	if	iver>=iver2

	if	iver>iver2

l193a:	fcb	$00

dosbwd:	deca			; ver 3: 96 abbrevs
	rept	6		; first nybble 1..3 is base, 32*n-1
	asl	a
	endm
	sta	l193a
	jsr	getnyb
	asl	a
	adc	#$01
	adc	l193a

	elseif	iver==iver2

dosbwd:	jsr	getnyb		; ver 2: only 32 abbrevs
	asl	a
	adc	#$01

	endif

	tay
	lda	(sbwdpt),y
	sta	acc
	dey
	lda	(sbwdpt),y
	sta	acc+1
	psh	prmmod,pnybcn
	dpsh	pnybbf
	psh	auxidx
	dpsh	auxlpg
	jsr	setaxw
	jsr	prntst
	dpul	auxlpg
	pul	auxidx
	mov	#$00,auxupd
	dpul	pnybbf
	pul	pnybcn,prmmod
	mov	#$ff,tmpmod
	jmp	donext

	endif


	if	iver<iver3
wrapmd:	cmp	#$03
	bcc	wrapm1
	sec
	sbc	#$03
	jmp	wrapmd
wrapm1:	rts

	endif


spclch:	fcb	"0123456789"
	fcb	".,!?_#'"
	fcb	$22		; double quote
	fcb	"/"
	fcb	$5c		; backslash
	if	iver==iver1
	fcb	"|"
	endif
	fcb	"-:()"

tstmod:	lda	tmpmod
	bpl	l19b4
	lda	prmmod
	rts
l19b4:	ldy	#$ff
	sty	tmpmod
	rts


getnyb:	lda	pnybcn
	bpl	l19bf
	sec
	rts
l19bf:	bne	l19d6
	inc	pnybcn
	jsr	ftaxwd
	dmov	acc,pnybbf
	lda	pnybbf+1
	lsr	a
	lsr	a
	and	#$1f
	clc
	rts
l19d6:	decabn	l19f3
	mov	#$02,pnybcn
	lda	pnybbf+1
	lsr	a
	lda	pnybbf
	ror	a
	tay
	lda	pnybbf+1
	lsr	a
	lsr	a
	tya
	ror	a
	lsr	a
	lsr	a
	lsr	a
	and	#$1f
	clc
	rts
l19f3:	mov	#$00,pnybcn
	lda	pnybbf+1
	bpl	l19ff
	mov	#$ff,pnybcn
l19ff:	lda	pnybbf
	and	#$1f
	clc
	rts


; crunch word to compare with vocab table entries

crnwrd:
	if	iver<=iver2
	lda	#$00
	sta	prmmod
	endif

	ldx	#$00
	ldy	#$06
l1a09:	lda	#$05
	sta	pkword,x
	inx
	dybne	l1a09
	mov	#$06,acd+1
	mov	#$00,acb,acc
l1a1b:	ldx	acc
	inc	acc
	lda	inword,x
	sta	acd
	bne	l1a2a
	lda	#$05
	jmp	l1a52

l1a2a:
	if	iver==iver1
	cmp	#' '
	bne	l1a86
	lda	#$00
	jmp	l1a52
l1a86:	cmp	#$0d
	bne	l1a9e
	ldx	acc
	lda	inword,x
	cmp	#$0a
	beq	l1a97
	lda	#$0d
	jmp	l1a9e
l1a97:	inc	acc
	lda	#$01
	jmp	l1a52
l1a9e:
	endif

	if	iver<iver3

	lda	acb
	pha
	lda	acd
	jsr	tstchr
	sta	acb
	cmp	prmmod
	beq	l1a43
	ldx	acc
	lda	inword,x
	jsr	tstchr
	cmp	acb
	bne	l1a91
	sec
	sbc	prmmod
	add	,#$03
	jsr	wrapmd
	add	,#$03
	sta	acb+1
	lda	acb
	sta	prmmod
	pla
	sta	acb
	lda	acb+1
	ldx	acb
	sta	pkword,x
	inc	acb
	dec	acd+1
	jeq	l1aca
	lda	acb
	pha
	jmp	l1a43

l1a91:	lda	acb
	sec
	sbc	prmmod
	add	,#$03
	jsr	wrapmd
	tax
	inx
	pla
	sta	acb
	txa
	
	elseif	iver>=iver3
	
	lda	acd
	jsr	tstchr
	tstabe	l1a43
	add	,#$03

	endif

	ldx	acb
	sta	pkword,x
	inc	acb
	decje	acd+1,l1aca

	if	iver<iver3
	lda	acb
	pha
	endif

l1a43:
	if	iver<iver3
	pla
	sta	acb
	endif
	
	lda	acd
	jsr	tstchr
	decabp	l1a62
	sub	acd,#$5b
l1a52:	ldx	acb
	sta	pkword,x
	inc	acb
	decjn	acd+1,l1a1b
	jmp	l1aca
l1a62:	bne	l1a6c
	sub	acd,#$3b
	jmp	l1a52
l1a6c:	lda	acd
	jsr	l1a99
	bne	l1a52
	lda	#$06
	ldx	acb
	sta	pkword,x
	inc	acb
	decbe	acd+1,l1aca
	lda	acd
	rept	5
	lsr	a
	endm
	and	#$03
	ldx	acb
	sta	pkword,x
	inc	acb
	decbe	acd+1,l1aca
	lda	acd
	and	#$1f
	jmp	l1a52


l1a99:	ldx	#$24
l1a9b:	cmp	spclch,x
	beq	l1aa6
	dxbpl	l1a9b
	ldy	#$00
	rts
l1aa6:	txa
	if	iver==iver1
	add	,#$07
	else
	add	,#$08
	endif
	rts

tstchr:	cmpbl	#'a',l1ab6
	cmpbg	#'z'+1,l1ab6
	lda	#$00
	rts
l1ab6:	cmpbl	#'A',l1ac1
	cmpbg	#'Z'+1,l1ac1
	lda	#$01
	rts
l1ac1:	tstabe	l1ac9
	bmi	l1ac9
	lda	#$02
l1ac9:	rts

l1aca:	lda	pkword+1
	rept	4
	asl	a
	endm
	rol	pkword
	asl	a
	rol	pkword
	ldx	pkword
	stx	pkword+1
	ora	pkword+2
	sta	pkword
	lda	lde
	rept	4
	asl	a
	endm
	rol	pkword+3
	asl	a
	rol	pkword+3
	ldx	pkword+3
	stx	pkword+3
	ora	ldf
	sta	pkword+2
	lda	pkword+3
	ora	#$80
	sta	pkword+3
	rts


	if	iver<iver3
oprstg:	jsr	prntbf
	jmp	start
	endif


	if	iver==iver1
opends:	jsr	prntbf
	fcb	$00		; brk
	endif


; init output routine and screen window

	if	iver==iver3

initsc:	mov	#$c1,prcswl+1
	mov	#$01,wndtop
	mov	#$00,wndlft,l1ba0
	mov	#$28,wndwdt
	mov	#$18,wndbot
	mov	#$be,prompt
	mov	#$ff,invflg

	endif
	

	if	iver>=iver3b

s1b49:	sta	wndtop
	sta	wndtop
	rts

	endif


	if	iver>=iver3a

initsc:	mov	#$c1,prcswl+1
	lda	#$00
	if	iver>=iver3b
	jsr	s1b49
	else
	sta	wndtop
	endif
	lda	d2005
	beq	lx1b61
	lda	#$15
	jsr	cout
lx1b61:	mov	#$00,d2005
	if	iver==iver3a
	lda	#$00
	endif
	sta	wndlft
	sta	l1ba0
	mov	#$18,wndbot
	mov	#$be,prompt
	mov	#$ff,invflg
	jsr	ck80c
	mov	#$03,lincnt
	lda	#$01
	if	iver>=iver3b
	jsr	s1b49
	else
	sta	wndtop
	endif
	rts

	endif


	if	iver>=iver3b

d1b84:	fcb	$00
d1b85:	fcb	$00

s1b86:	lda	d2005
	beq	l1b90
	lda	#crchar
	jmp	l1b92
l1b90:	lda	#crchar+$80
l1b92:	jsr	cout
	rts


; split window
opsplw:	lda	arg1
	beq	l1bc3
	pha
	add	,#$01,wndbot
	sta	d1b85
	jsr	home
	jsr	s1b86
	mov	#24,wndbot
	pla
	add	,#$01
	if	iver>=iver3b
	jsr	s1b49
	else
	sta	wndtop
	endif
	mov	#1,cursrh,cur80h
	mov	#22,cursrv
	jsr	s1b86			; jsr/rts could be jmp
	rts

l1bc3:	lda	#$01
	jsr	s1b49
	mov	#$00,lincnt
	sta	d1b85
l1bcf:	rts
	

; set window
opsetw:	lda	arg1
	pha
	bne	l1be9
	mov	#$00,d1b84
	mov	#1,cursrh,cur80h
	mov	#22,cursrv
	pla
	jmp	l1bfc

l1be9:	pla
	cmp	#$01
	bne	l1bcf
	mov	#$01,d1b84
	mov	#$00,cursrh,cur80h,cursrv
l1bfc:	jsr	s1b86				; jsr/rts could be jmp
	rts

	endif


	if	iver==iver3

; clear the screen

clrscr:	jsr	home
	mov	wndtop,lincnt
	rts

	elseif	iver==iver3a

clrscr:	jsr	home
	mov	#$01,wndtop,wndtop,lincnt
	rts

	elseif	iver>=iver3b
	
clrscr:	jsr	home
	lda	d1b85
	bne	l1c0d
	lda	#$01
	jsr	s1b49
l1c0d:	mov	#1,lincnt
	rts

	endif



; find the highest usable page of memory

fndmem:	dmovi2	lstflc+$0100,acc
	ldy	#$00
l1b28:	dec	acc+1
	lda	(acc),y
	cmp	(acc),y
	bne	l1b28
	eor	#$ff
	sta	(acc),y
	cmp	(acc),y
	bne	l1b28
	eor	#$ff
	sta	(acc),y
	lda	acc+1
	rts


; buffer a character for output

bfchar:	ldx	chrptr			; get buffer pointer
	if	iver>=iver3a
	ldy	d2005
	endif

	cmpje	#crchar,prntbf	; if char is a CR, flush buffer
	cmpbl	#' ',l1b61		; if it is a control character, discard it
	cmpbl	#$60,l1b57		; if it is in 64 char subset, buffer it as is

	if	lc40
	bit	invflg			; if inverse, convert LC to UC
	bmi	l1b57
	else
	cmp	#$80			; is it in LC range ($60 <= char < $80)?
	bge	l1b57			;   (entirely superfluous test!)
	endif

	if	iver>=iver3a
	cpy	#$01
	beq	l1b57
	endif

	sub	,#$20			;   yes, convert o upper case

l1b57:	ora	#$80			; set high bit for Apple

	sta	buffer,x		; store it in buffer
	cpxbg	wndwdt,l1b64		; if buffer is full, print some of it

	inx				; increment pointer
l1b61:	stx	chrptr			; save pointer
	rts				; return

; find last space in buffer, if any

l1b64:	lda	#' '+$80		; load a space for comparison

l1b66:	cmp	buffer,x		; if this is one, we've got it
	beq	l1b70
	dxbne	l1b66			; no, loop if no character in buffer

	ldx	wndwdt			; no space... use last character

l1b70:	stx	chrpt2			; save pointer
	stx	chrptr

	jsr	prntbf			; print line up to this point

; move rest of line back to beginning of buffer

l1b77:	inc	chrpt2			; get pointer to next char
	ldx	chrpt2
	cpxrgt	wndwdt			; if it is past the last char, return

	lda	buffer,x		; get the character
	ldx	chrptr			; get the pointer to the new loc
	sta	buffer,x		; store the character there
	inc	chrptr			; and increment the pointer

	ldx	chrpt2			; unnecessary!
	jmp	l1b77			; try for another one


; output the buffer to the screen, and to the printer if enabled

outbuf:	ldy	#hdrflg+1
	lda	(frzmem),y
	and	#$01
	jsrne	prtbuf
	jsr	dspbuf
	rts


; output the buffer to the printer

	if	iver>=iver2
l1ba0:	fcb	$00			; printer initialization flag
	endif

prtbuf:	dpsh	cswl			; save our output vector

	if	iver>=iver3
	psh	cursrh			; save cursor column
	endif

	dmov	prcswl,cswl		; get vector for printer

	ldx	#$00			; start with position 0 in buffer

	if	iver>=iver2
	lda	l1ba0			; is printer initialized?
	bne	l1bd5			; yes, go print it
	inc	l1ba0			; no, but now will be
					; output ^I80N
	if	iver<iver3
	lda	#tbchar
	else
	lda	#tbchar+$80
	endif
	jsr	cout			;   (this sets printer width to 80
					;    characters, thereby disabling
					;    screen echo (we hope!))
	if	iver>=iver3a
	txa
	tay
	sub	prcswl+1,#$c1
	tax
	lda	#$91
	sta	prtflg,x

	else

	lda	#$91
	sta	prtflg

	endif

	lda	#$b8
	jsr	cout
	lda	#$b0
	jsr	cout
	lda	#$ce
	jsr	cout

	if	iver>=iver3a
	tya
	tax
	endif

	endif

l1bd5:	cpxbe	chrptr,l1be3		; are we done yet?

	lda	buffer,x		; no, get character
	jsr	cout			; and output it

	inx				; increment pointer
	jmp	l1bd5			; and go for another one

l1be3:	dmov	cswl,prcswl		; save print vector again (may have changed)

	if	iver>=iver3
	pul	cursrh			; restore cursor column
	if	iver>=iver3a
	sta	cur80h
	endif
	endif

	dpul	cswl			; restore display vector
	rts


; output the buffer to the display

dspbuf:	ldx	#$00			; start with position 0 in buffer

l1bf7:	cpxbe	chrptr,l1c05		; are we done yet?

	lda	buffer,x		; get the character and output it
	if	iver<iver3a
	jsr	cout1
	else
	jsr	cout
	endif

	inx				; increment pointer
	jmp	l1bf7			; and go for another one

l1c05:	ldx	#$00			; reset pointer to beginning
	stx	chrptr
	rts				; and return


morems:	fcb	"[MORE]"
mrmsln	equ	*-morems

prntbf:
	if	iver<iver3b
	inc	lincnt
	else
	lda	d1b84
	bne	l1d1f
	inc	lincnt
l1d1f:
	endif

	lda	lincnt
	cmpbl	wndbot,l1c40
	dmovi	morems,acc
	ldx	#mrmsln
	mov	#$3f,invflg
	jsr	shwmsg
	mov	#$ff,invflg
	jsr	rdkey

	if	iver<iver3a
	sub	cursrh,#$06,cursrh
	else
	mov	#$00,cursrh,cur80h
	endif

	jsr	clreol
	mov	wndtop,lincnt
	inc	lincnt
l1c40:	psh	chrptr
	jsr	outbuf
	pla
	cmpbe	wndwdt,l1c50
	lda	#crchar+$80
	if	iver<iver3a
	jsr	cout1
	else
	jsr	cout
	endif
l1c50:	ldy	#hdrflg+1
	lda	(frzmem),y
	and	#$01
	beq	l1c79
	dpsh	cswl
	dmov	prcswl,cswl
	lda	#crchar+$80
	jsr	cout
	dmov	cswl,prcswl
	dpul	cswl
l1c79:	ldx	#$00
	jmp	l1b61


	if	iver<=iver2
s1cc9:	jsr	home
	lda	wndtop
	sta	lincnt
	rts
	endif


scorms:	fcb	"SCORE:"
	if	iver>=iver3a
	fcb	" "
	endif
scmsln	equ	*-scorms

	if	iver>=iver3
timems:	fcb	"TIME:"
	if	iver>=iver3a
	fcb	" "
	endif
tmmsln	equ	*-timems

l1c89:	fcb	$00
	endif


opprst:
	jsr	outbuf			; print what's in the buffer

	if	iver>=iver3a
	ldy	d2005
	cpy	#0
	beq	l1daa
	lda	cur80h
	pha
	jmp	l1dad
	endif

l1daa:	psh	cursrh			; save the cursor position
l1dad:	psh	cursrv
	mov	#$00,cursrh		; home the cursor
	if	iver>=iver3a
	sta	cur80h
	endif
	sta	cursrv
	jsr	vtab
	mov	#$3f,invflg		; set inverse mode

	if	iver<iver3
	jsr	clreol
	endif

	lda	#$10			; get global var 0
	jsr	gtvra1

	if	iver>=iver3
	lda	acc			; is it save as last time?
	if	iver==iver3
	cmpbe	l1c89,l1cb8		; yes, don't print it
	endif
	sta	l1c89			; no, save for next time's compare
	endif

	jsr	prtnam			; output thing name
	jsr	dspbuf			; send it to display

	if	iver>=iver3
	jsr	clreol			; clear rest of line
	endif

	if	iver>=iver3a
	lda	d2005
	beq	l1cb8
	lda	#60
	sta	cursrh
	sta	cur80h
	jmp	l1de6
	endif

l1cb8:	mov	#$19,cursrh		; tab over
l1de6:

	if	iver>=iver3
	lda	stltyp			; score or time?
	bne	l1cdb			; time
	endif

	dmovi	scorms,acc		; score, print "SCORE:"
	ldx	#scmsln
	jsr	shwmsg
	if	iver<iver3a
	inc	cursrh			; one space
	endif
	lda	#$11			; get global var 1 (score)
	jsr	gtvra1
	jsr	prntnm			; output it as decimal number
	lda	#'/'			; separator


	if	iver>=iver3

	bne	l1d05			; always taken

l1cdb:	dmovi	timems,acc		; print "TIME:"
	ldx	#tmmsln
	jsr	shwmsg
	if	iver<iver3a
	inc	cursrh			; one space
	endif
	lda	#$11			; get global var 1 (time)
	jsr	gtvra1
	lda	acc			; is it zero?
	bne	l1cf5
	lda	#$18			; yes, make it 24:00
l1cf5:	cmpbm	#$0c,l1d00		; is it A.M. or P.M.?
	beq	l1d00
	sec				; P.M., convert to 1-12 range
	sbc	#$0c			;  by subtracting 12
	sta	acc
l1d00:	jsr	prntnm			; print out hours
	lda	#':'

	endif


l1d05:	jsr	bfchar			; print the separator
	lda	#$12			; get global var 2 (turns/minutes)
	jsr	gtvra1

	if	iver>=iver3

	lda	stltyp			; time?
	beq	l1d40			; no, go print turns

	lda	acc			; yes, are minutes < 10?
	cmpbg	#$0a,l1d1c		; no
	lda	#$b0			; yes, print a space (?)
	jsr	bfchar
l1d1c:	jsr	prntnm			; print the minutes
	lda	#$a0			; print a space
	jsr	bfchar
	lda	#$11			; get global var 1 (hours)
	jsr	gtvra1
	lda	acc			; is it A.M. or P.M.?
	cmpbp	#$0c,l1d33		; P.M.
	lda	#'A'+$80		; A.M.
	bne	l1d35
l1d33:	lda	#'P'+$80
l1d35:	jsr	bfchar			; print the 'A' or 'P'
	lda	#'M'+$80
	jsr	bfchar			; print the 'M'
	jmp	l1d43

	endif

l1d40:	jsr	prntnm			; print the score
l1d43:	jsr	dspbuf			; display the buffer

	if	iver>=iver3
	jsr	clreol			; clear out the line
	endif

	mov	#$ff,invflg		; back to normal video mode
	pul	cursrv,cursrh		; and the old cursor loc
	if	iver>=iver3a
	sta	cur80h
	endif
	jsr	vtab
	rts				; return to caller

shwmsg:	ldy	#$00
l1d59:	lda	(acc),y
	ora	#$80
	if	iver<iver3a
	jsr	cout1
	else
	jsr	cout
	endif
	iny
	dxbne	l1d59
	rts


getlin:	jsr	outbuf
	mov	wndtop,lincnt
	jsr	getln1
	inc	lincnt
	lda	#crchar+$80
	sta	buffer,x
	inx
	txa
	pha
	ldy	#hdrflg+1
	lda	(frzmem),y
	and	#$01
	beq	l1d8b
	txa
	sta	chrptr
	jsr	prtbuf
	mov	#$00,chrptr
l1d8b:	pla
	ldy	#$00
	cmp	(arg1),y
	blt	l1d94
	lda	(arg1),y
l1d94:	pha
	beq	l1db1
	tax
l1d98:	lda	buffer,y
	and	#$7f
	cmpbl	#'A',l1da7
	cmpbg	#'Z'+1,l1da7
	ora	#$20
l1da7:	iny
	sta	(arg1),y
	cmpbe	#crchar,l1db1
	dxbne	l1d98
l1db1:	pla
	rts


	if	iver<iver3
s1d90:	mov	#1,wndtop
	mov	#0,wndlft
	mov	#40,wndwdt
	mov	#24,wndbot
	mov	#'>',prompt
	mov	#$ff,invflg
	jsr	s1cc9			; jsr/rts could be combined to jmp
	rts
	endif


iob:	fcb	$01			; IOB type
iobslt:	fcb	$60			; Slot * 16
iobdrv:	fcb	$01			; Drive
	fcb	$00			; Volume
iobtrk:	fcb	$00			; Track
iobsct:	fcb	$00			; Sector
	fdb	dct			; Device Characteristics Table
iobbuf:	fdb	$0000			; I/O buffer
	fdb	$0000			; unused
iobcmd:	fcb	$00			; Command
	fcb	$00			; Status
	fcb	$00			; Actual volume
	fcb	$60			; Previous slot * 16
	fcb	$01			; Previous drive

dct:	fcb	$00,$01,$ef,$d8

diskio:	sta	iobcmd
	dmov	acc,iobbuf
	mov	#$03,iobtrk
	lda	acb
	ldx	acb+1
	sec
l1ddf:	sbc	secptk
	bcs	l1de7
	dxbmi	l1ded
	sec
l1de7:	inc	iobtrk
	jmp	l1ddf
l1ded:	add	,secptk,iobsct
	lda	#iob>>8
	ldy	#iob&$ff

	if	iver<iver3
	jsr	rwts
	rts
	else
	jmp	rwts
	endif


drdbuf:	dmovi	buffer,acc
drdnxt:	dinc	acb
drdblk:	lda	#$01

	if	iver<iver3
	jsr	diskio
	rts
	else
	jmp	diskio
	endif

	
	if	iver<iver3
drdbkf	equ	drdblk
	else
drdbkf:	jsr	drdblk
	jsrcs	fatal
	rts
	endif


dwrbuf:	dmovi	buffer,acc
dwrnxt:	dinc	acb

	if	iver<iver3
	lda	dct+2
	pha
	lda	dct+3
	pha
	lda	#$d8
	sta	dct+3
	lda	#$ef
	sta	dct+2
	endif

	lda	#$02

	if	iver<iver3
	jsr	diskio
	pla
	sta	dct+3
	pla
	sta	dct+2
	rts
	else
	jmp	diskio
	endif


	if	iver<iver3
s1e36:	jsr	s1d90		; jsr/rts could be jmp,
	rts			; but could just use jsr $1d90 instead of jsr s1e36
	endif


outmsg:	stx	acd
	ldy	#$00
	sty	acd+1
l1e2f:	ldy	acd+1
	lda	(acc),y
	jsr	bfchar
	inc	acd+1
	decbn	acd,l1e2f
	rts


ismsg:
	if	iver<iver3b
	fcb	"PLEASE "
	endif
	fcb	"INSERT SAVE DISKETTE,"
ismsgl	equ	*-ismsg

	if	iver==iver1

slmsg:	fcb	"    INTO  SLOT: "
slmsgl	equ	*-slmsg

sdmsg:	fcb	"DEFAULT = 6"
sdmsgl	equ	*-sdmsg

drmsg:	fcb	"         DRIVE: "
drmsgl	equ	*-drmsg

ddmsg:	fcb	"DEFAULT = 1"
ddmsgl	equ	*-ddmsg

	else	; iver>=iver2

msgofs:	fcb	$00

msgbas:

slmsg:	fcb	"SLOT     (1-7):"
slmsgl	equ	*-slmsg
slmsgo	equ	slmsg-msgbas

sldef:	fcb	"6"		; default
	if	iver>=iver3
	fcb	"18"		; range
	endif

drmsg:	fcb	"DRIVE    (1-2):"
drmsgl	equ	*-drmsg
drmsgo	equ	drmsg-msgbas

	if	(drmsgl<>slmsgl)
	error	"save/restore prompt message lengths must be identical"
	endif

drdef:	fcb	"2"		; default
	if	iver>=iver3
	fcb	"13"		; range
	endif

psmsg:	fcb	"POSITION (0-7):"
psmsgl	equ	*-psmsg
psmsgo	equ	psmsg-msgbas

	if	psmsgl <> slmsgl
	error	"save/restore prompt message lengths must be identical"
	endif

psdef:	fcb	"0"		; default
	if	iver>=iver3
	fcb	"08"		; range
	endif

	if	iver>=iver3a
c8msg:
	if	iver==iver3a
	fcb	"DO YOU WANT "
	endif
	fcb	"80 COLUMNS? (Y/N):"
	fcb	crchar,$ff
	endif

dfmsg:	fcb	"DEFAULT = "
dfmsgl	equ	*-dfmsg

	endif

prmsg:	fcb	"--- PRESS 'RETURN' "
	if	iver<iver3b
	fcb	"KEY "
	endif
	fcb	"TO BEGIN ---"
prmsgl	equ	*-prmsg

	if	iver>=iver3a
ptmsg:	fcb	"PRINTER SLOT "
	if	iver==iver3a
	fcb	"NUMBER? "
	endif
	fcb	"(0-7):"
	fcb	crchar
d2003:	fcb	$ff
d2004:	fcb	$00
d2005:	fcb	$00
	endif

	if	iver>=iver3a

ck80c:	lda	romid
	cmp	#$06
	bne	l2061
	lda	rdc3rom
	and	#$80
	bne	l2061
	mov	#$00,d2005
l2019:	dmovi	c8msg,acc
	jsr	home

; Another dumb string output routine. Sigh.
	ldy	#$00
l2026:	lda	(acc),y
	cmp	#$ff
	beq	l2035
	eor	#$80
	jsr	cout
	iny
	jmp	l2026

l2035:	jsr	rdkey
	tax
	cpx	#'n'+$80
	beq	l2061
	cpx	#'N'+$80
	beq	l2061
	cpx	#'y'+$80
	beq	l204d
	cpx	#'Y'+$80
	beq	l204d
	cpx	#crchar+$80
	bne	l2019

l204d:	jsr	home
	dmovi	sl3fw,cswl
	if	iver==iver3a
	lda	#crchar
	jsr	cout
	else
	jsr	s1b86
	endif
	mov	#$01,d2005
	rts

l2061:	mov	#$00,d2005
	rts


s2067:	lda	d2004
	bne	l209a
l206c:	dmovi	ptmsg,acc

; yet another dumb inline string print
; after the first ZIP v3, Infocom must have been overrun by newbies!
	ldy	#$00
l2076:	lda	(acc),y
	cmp	#$ff
	beq	l2085
	eor	#$80
	jsr	cout
	iny
	jmp	l2076

l2085:	jsr	rdkey
	sub	,#'0'+$80
	blt	l206c
	clc			; unnecessary
	cmp	#8
	bge	l206c
	add	,#$c0,prcswl+1
	inc	d2004
l209a:	rts

	endif


l1ebd:
	if	iver<iver3
	jsr	s1cc9
	else
	jsr	clrscr
	endif

	jsr	prntbf
	jsr	prntbf
	dmovi	ismsg,acc
	ldx	#ismsgl
	jsr	outmsg
l1f0b:	jsr	prntbf
	
	if	iver>iver1

l1ee1:	mov	#psmsgo,msgofs
	jsr	getnum

	if	iver<iver3
	cmp	#'0'
	blt	l1ee1
	cmp	#'8'
	bge	l1ee1
	endif

	sta	psdef
	jsr	bfchar

	endif

l1ef7:
	if	iver==iver1

	dmovi	slmsg,acc
	ldx	#slmsgl
	jsr	outmsg
	jsr	outbuf
	mov	#25,cursrh
	mov	#$3f,invflg
	dmovi	sdmsg,acc
	ldx	#sdmsgl
	jsr	shwmsg
	mov	#$ff,invflg
	jsr	rdkey
	pha
	mov	#25,cursrh
	jsr	clreol
	pla
	cmp	#crchar+$80
	bne	l1f4c
	lda	#'6'+$80
	jmp	l1f54
l1f4c:	cmp	#'1'+$80
	blt	L1f0b
	cmp	#'8'+$80
	bge	L1f0b
l1f54:

	else

	mov	#slmsgo,msgofs
	jsr	getnum

	if	iver<iver3
	cmp	#'1'
	blt	l1ef7
	cmp	#'8'
	bge	l1ef7
	endif

	endif

	tax
	and	#$07
	rept	4
	asl	a
	endm
	sta	iobslt
	txa
	if	iver==iver1
	and	#$7f
	else
	sta	sldef
	endif
	jsr	bfchar

; get drive number
l1f18:
	if	iver==iver1

	jsr	prntbf
	dmovi	drmsg,acc
	ldx	#drmsgl
	jsr	outmsg
	jsr	outbuf
	mov	#25,cursrh
	mov	#$3f,invflg
	dmovi	ddmsg,acc
	ldx	#ddmsgl
	jsr	shwmsg
	mov	#$ff,invflg
	jsr	rdkey
	pha
	mov	#25,cursrh
	jsr	clreol
	pla
	cmp	#crchar+$80
	bne	l1fa5
	lda	#'1'+$80
	jmp	l1fad
l1fa5:	cmp	#'1'+$80
	bcc	l1f18
	cmp	#'3'+$80
	bcs	l1f18

	else

	mov	#drmsgo,msgofs
	jsr	getnum

	if	iver<iver3
	cmp	#'1'
	bcc	l1f18
	cmp	#'3'
	bge	l1f18
	endif

	endif

l1fad:	tax
	and	#$03
	sta	iobdrv
	txa
	if	iver==iver1
	and	#$7f
	else
	sta	drdef
	endif
	jsr	bfchar

l1f12:	jsr	prntbf
	dmovi	prmsg,acc
	ldx	#prmsgl
	jsr	outmsg
	jsr	outbuf
	jsr	rdkey
	cmpbn	#crchar+$80,l1f12

	if	iver>iver1
	mov	#$ff,acb,acb+1

	lda	psdef
	and	#$07
	beq	l1f48
	tay
l1f3a:	daddb2	acb,#$40
	dybne	l1f3a
	endif

l1f48:	jsr	prntbf		; jsr/rts could be jmp

	if	iver==iver1
	mov	#$ff,acb,acb+1
	endif
	
	rts

	if	iver>=iver2

getnum:	jsr	prntbf

	dmovi	msgbas,acc
	daddb2	acc,msgofs
	ldx	#slmsgl
	jsr	outmsg
	jsr	outbuf
	mov	#25,cursrh
	if	iver>=iver3a
	sta	cur80h
	endif
	mov	#$3f,invflg

	dmovi	dfmsg,acc
	ldx	#dfmsgl
	jsr	shwmsg
	dmovi	sldef,acc
	daddb2	acc,msgofs
	ldx	#$01
	jsr	shwmsg
	mov	#$ff,invflg
	jsr	rdkey
	pha
	mov	#$19,cursrh
	if	iver>=iver3a
	sta	cur80h
	endif
	jsr	clreol
	pla

	if	iver>=iver3
	ldy	msgofs
	endif
	
	cmpbn	#crchar+$80,l1fb3

	if	iver<iver3
	ldy	msgofs
	endif

	lda	sldef,y
l1fb3:	and	#$7f

	if	iver>=iver3
	cmp	sldef+1,y
	blt	getnum
	cmp	sldef+2,y
	bge	getnum
	endif

	rts

	endif


rgmsg:
	if	iver<iver3b
	fcb	"PLEASE "
	endif
	fcb	"RE-INSERT GAME DISKETTE,"
rgmsgl	equ	*-rgmsg

pr2ms:	fcb	"--- PRESS 'RETURN' "
	if	iver<iver3b
	fcb	"KEY "
	endif
	fcb	"TO CONTINUE ---"
pr2msl	equ	*-pr2ms

l2005:	lda	iobslt
	cmpbn	#$60,l2040
	lda	iobdrv
	cmpbn	#$01,l2040
	jsr	prntbf
	dmovi	rgmsg,acc
	ldx	#rgmsgl
	jsr	outmsg

l2023:	jsr	prntbf
	dmovi	pr2ms,acc
	ldx	#pr2msl
	jsr	outmsg
	jsr	outbuf
	jsr	rdkey
	cmpbn	#crchar+$80,l2023
	jsr	prntbf
l2040:	mov	#$60,iobslt
	mov	#$01,iobdrv
	rts


opsvgm:	jsr	l1ebd			; setup for disk I/O

	if	iver<iver3

	ldx	#$00
	ldy	#hdrirl
	lda	(frzmem),y
	sta	buffer,x
	inx

	else

	ldx	#$00			; copy game release # to buffer
	ldy	#hdrrel
	lda	(frzmem),y
	sta	buffer,x
	inx
	iny
	lda	(frzmem),y
	sta	buffer,x
	inx

	endif

	dmovi	prgidx,acc		; copy PC to buffer
	ldy	#$03
	jsr	svgmmv

	dmovi	locvar,acc		; copy local variables to buffer
	ldy	#$1e
	jsr	svgmmv

	dmovi	stkcnt,acc		; copy SP and SP save to buffer
	ldy	#$06
	jsr	svgmmv

	jsr	dwrbuf			; write it out
	bcs	svgmfl			; fail if error

	ldx	#$00			; copy lowest 256 bytes of stack
	dmovi	stklim,acc		; to buffer
	ldy	#$00
	jsr	svgmmv

	jsr	dwrbuf			; write it out
	bcs	svgmfl			; fail if error

	ldx	#$00			; copy rest bytes of stack
	dmovi	stklim+$0100,acc	; to buffer
	ldy	#(stckmx*2)-$0100
	jsr	svgmmv

	jsr	dwrbuf			; write it out
	bcs	svgmfl			; fail if error

	dmov	frzmem,acc		; figure out how many pages of
	ldy	#hdrimp			; impure storage there are tobe
	lda	(frzmem),y		; written out, and set up for first
	sta	acd
	inc	acd			; one

l20c3:	jsr	dwrnxt			; write one page of impure storage
	bcs	svgmfl			; fail if error
	inc	acc+1			; increment buffer address
	decbn	acd,l20c3		; decrement page count, loop if more

	jsr	dwrnxt			; write final page
	bcs	svgmfl			; fail if error

	jsr	l2005			; make sure we have game disk
	jmp	predtr			; return true (no error)

svgmfl:	jsr	l2005			; make sure we have game disk
	jmp	predfl			; return false (error)


svgmmv:	dey				; copy memory into buffer to write
	lda	(acc),y
	sta	buffer,x
	inx
	cpybn	#$00,svgmmv		; if more, loop
	rts				; no, return


oprsgm:	jsr	l1ebd			; setup for disk I/O

	jsr	drdbuf			; read in a bufferful
	jcs	rsgmfl			; fail if error

	if	iver<iver3

	ldx	#$00
	ldy	#hdrirl
	lda	(frzmem),y
	cmp	buffer,x
	jne	rsgmfl

	else

	ldx	#$00			; check release of game, fail if wrong
	ldy	#hdrrel
	lda	(frzmem),y
	cmp	buffer,x
	bne	l210a
	inx

	iny
	lda	(frzmem),y
	cmp	buffer,x
	beq	l210d
l210a:	jmp	rsgmfl

	endif

l210d:	ldy	#hdrflg+1		; preserve SCRIPT flag
	lda	(frzmem),y
	sta	mdflag

	inx				; restore PC
	dmovi	prgidx,acc
	ldy	#$03
	jsr	rsgmmv
	mov	#$00,prgupd

	dmovi	locvar,acc		; restore local variables
	ldy	#$1e
	jsr	rsgmmv

	dmovi	stkcnt,acc		; restore SP and SP save
	ldy	#$06
	jsr	rsgmmv

	jsr	drdbuf			; read a bufferful
	bcs	rsgmfl			; fail if error

	ldx	#$00			; restore first 256 bytes of stack
	dmovi	stklim,acc
	ldy	#$00
	jsr	rsgmmv

	jsr	drdbuf			; read a bufferful
	bcs	rsgmfl			; fail if error

	ldx	#$00			; restore rest of stack
	dmovi	stklim+$0100,acc
	ldy	#(stckmx*2)-$0100
	jsr	rsgmmv

	dmov	frzmem,acc		; figure out how many pages of
	ldy	#hdrimp			; impure storage there are to be
	lda	(frzmem),y		; read in, and set up to read first
	sta	acd
	inc	acd			; one

l2177:	jsr	drdnxt			; read in next page of impure storage
	bcs	rsgmfl			; fail if error
	inc	acc+1			; increment buffer pointer
	decbn	acd,l2177		; decrement page count, loop if more

	lda	mdflag			; restore SCRIPT flag
	ldy	#hdrflg+1
	sta	(frzmem),y

	jsr	l2005			; make sure we have game disk
	jmp	predtr			; return true (no error)

rsgmfl:	jsr	l2005			; make sure we have game disk
	jmp	predfl			; return false (error)


rsgmmv:	dey				; copy buffer to memory (read)
	lda	buffer,x
	sta	(acc),y
	inx
	cpybn	#$00,rsgmmv
	rts


	if	iver>=iver3

; a really awful PRNG

getrnd:	inc	rndloc			; get a 'random' number
	inc	rndloc+1
	dmov	rndloc,acc
	rts

	endif


	if	iver==iver1

fatal:	fcb	$00

	fcb	$00,$00,$00,$00,$00,$00,$00,$00		; unused?
	fcb	$b6,$19,$00,$00,$02,$00,$01,$01
	fcb	$85,$e4,$a9,$1e,$85,$e5,$a2

; $21c5..$21ff is duplicate of $20c5..$20ff

	fcb	$23,$20,$6c
	fcb	$1e,$20,$36,$1c,$20,$0c,$fd,$c9
	fcb	$8d,$d0,$e6,$20,$8d,$1c,$a9,$ff
	fcb	$85,$e2,$85,$e3,$60,$50,$4c,$45
	fcb	$41,$53,$45,$20,$52,$45,$2d,$49
	fcb	$4e,$53,$45,$52,$54,$20,$47,$41
	fcb	$4d,$45,$20,$44,$49,$53,$4b,$45
	fcb	$54,$54,$45,$2c,$2d,$2d,$2d,$20

	else

endmsg:	fcb	"-- END OF SESSION --"
enmsln	equ	*-endmsg


	if	iver<iver3
opends:	jsr	prntbf
	dmovi	endmsg,acc
	ldx	#enmsln
	jsr	outmsg
	jsr	prntbf
	jmp	*			; halt
	endif


	if	iver<iver3
ftlmsg:	fcb	"ZORK INTERNAL ERROR!"
	else
ftlmsg:	fcb	"INTERNAL ERROR #"
	endif
ftmsln	equ	*-ftlmsg


	if	iver<iver3

fatal:	fcb	$00			; brk instruction

; a really awful PRNG
getrnd:	rol	rndloc+1
	lda	rndloc
	sta	acc
	lda	rndloc+1
	sta	acc+1
	rts

	fillto	$21fc,$00
	fcb	$fc,$19			; unused?
	fcb	$00,$00

	else

opfatl:
fatal:	jsr	prntbf			; flush anything left in buffer

	dmovi	ftlmsg,acc		; output fatal message
	ldx	#ftmsln
	jsr	outmsg

	dpul2	acc			; output address where error detected
	jsr	prntnm

	if	iver>=iver3b
	jmp	opends
	fcb	$00,$00,$00,$00,$00,$00,$00,$00
	fcb	$00,$00
	endif

opends:	jsr	prntbf			; flush anything left in buffer

	dmovi	endmsg,acc		; output end of session message
	ldx	#enmsln
	jsr	outmsg

	jsr	prntbf			; flush the buffer

halt:	jmp	halt			; die horribly

	; junk
	if	iver==iver3a

	fcb	$00,$00,$00,$00,$00,$00,$00
	fcb	$00,$00,$72,$1b,$00,$00,$02,$00
	fcb	$01,$01,$d0,$2d,$20,$81,$1c,$a9
	fcb	$29,$85,$e6,$a9,$21,$85,$e7,$a2
	fcb	$1f,$20,$b6,$1e,$20,$81,$1c,$a9
	fcb	$48,$85,$e6,$a9,$21,$85,$e7,$a2
	fcb	$26,$20,$b6,$1e,$20,$f5,$1b,$20
	fcb	$0c,$fd,$c9,$8d,$d0,$e6,$20,$81
	fcb	$1c,$a9,$60,$8d,$41,$1e,$a9,$01
	fcb	$8d,$42,$1e,$60,$20,$20,$20,$a2
	fcb	$00,$a0,$02,$b1,$ba,$9d,$00,$02
	fcb	$e8,$c8,$b1,$ba,$9d,$00,$02,$e8
	fcb	$a9,$8a,$85,$e6,$a9,$00,$85,$e7
	fcb	$a0,$03,$20,$00,$00,$a9,$9a,$85
	fcb	$e6,$a9,$00,$85,$e7,$a0,$1e,$20
	fcb	$00,$00,$a9,$c8,$85,$e6,$a9,$00
	fcb	$85,$e7,$a0,$06,$20,$00,$00,$20
	fcb	$a3,$1e,$b0,$00,$a2,$00,$a9,$28
	fcb	$85,$e6,$a9,$02,$85,$e7,$a0,$00

	elseif	iver==iver3b

	fcb	$00,$00,$00,$00,$00,$00,$00,$00
	fcb	$ee,$1b,$00,$00,$02,$00,$01,$01
	fcb	$e7,$a2,$18,$20,$52,$1f,$20,$18
	fcb	$1d,$a9

	endif

	endif

	endif

	end	start
