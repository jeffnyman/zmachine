C Data base compiler for DUNGEON
C
C COPYRIGHT 1980, 1990, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA.
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK
C
C 29-Jun-92	RMS	Removed extraneous declaration from INITFL.
C 30-Jun-92	RMS	Removed DISPOSE, INITIALSIZE from OPEN.
C		RMS	Changed WRITE (5,) to TYPE.
C
C Declarations
C
	LOGICAL FUNCTION INITFL(X)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	PARAMETER DIRMAX=15
	CHARACTER*2 STRDIR(DIRMAX),INDIR
	INTEGER CDIR(DIRMAX)
C
C Functions and data
C
	DATA STRDIR/'N ','NE','E ','SE','S ','SW','W ','NW',
	1 'U ','D ','LA','LN','EN','EX','CR'/
	DATA CDIR/XNORTH,XNE,XEAST,XSE,XSOUTH,XSW,XWEST,XNW,
	1 XUP,XDOWN,XLAUN,XLAND,XENTER,XEXIT,XCROSS/

C DBCOMP, PAGE 2
C
C Start initialization proper.
C
	TYPE 1,VMAJ,VMIN,VEDIT			! announce start.
1	FORMAT(/' Data Base Compiler for Dungeon V',I1,'.',I1,A)
C
	INITFL=.FALSE.				! assume init fails.
	RECNO=1					! init record number.
C
C Now start data base file processing.
C
	OPEN (UNIT=1,NAME='dtext.txt',TYPE='OLD',READONLY,ERR=1900)
C
	OPEN (UNIT=DBCH,NAME='dtext',TYPE='NEW',
	1	FORM='UNFORMATTED',ACCESS='DIRECT',RECORDSIZE=RECLNT,
	2	ERR=1950)
C
	TYPE 185
185	FORMAT(/' Creating new "DTEXT.DAT"...')
C
C Return here to read next section header.
C
80	READ (1,90) I,INBUF			! read section header.
90	FORMAT(I5,A)
C
	IF(I.EQ.0) GO TO 80			! ignore 0.
	IF(I.LT.0) GO TO 1000			! -1 is end file.
	TYPE 125,I,INBUF(1:40)
125	FORMAT(' Initializing section #',I2,': ',A)
	IF(I.LE.8) GO TO 100			! text section?
	GO TO (400,500,600,700,800,900,950),(I-8)
	CALL BUG(12,I)				! invalid section no.
C
C Text section.  Write text out to db file, note initial record
c number in appropriate array.
C
100	PREVJ=0					! force start of record.
110	READ(1,90) J,INBUF			! read line.
	IF(J.EQ.0) GO TO 110			! ignore 0.
	IF(J.LT.0) GO TO 80			! -1 is end section.
	IF(J.EQ.PREVJ) GO TO 300		! if cont, write out.
	GO TO (210,220,230,240,250,260,270,280),I
	CALL BUG(12,I)
C
C Section 1-- random remarks.
C
210	IF(J.GT.MMAX) CALL BUG(13,J)		! too many remarks?
	IF(RTEXT(J).NE.0) CALL BUG(14,J)	! duplicate remark?
	RTEXT(J)=-RECNO				! record start of record.
	MLNT=MAX0(MLNT,J)
	MBASE=MAX0(MBASE,J)
	GO TO 300				! go output line.
C
C Section 2-- melee messages
C
220	K=J+MBASE				! offset by end of randoms.
	IF(K.GT.MMAX) CALL BUG(13,K)
	IF(RTEXT(K).NE.0) CALL BUG(14,K)
	RTEXT(K)=-RECNO
	MLNT=MAX0(MLNT,K)			! update length counter.
	GO TO 300
C
C Section 3-- long room descriptions
C
230	IF(J.GT.RMAX) CALL BUG(15,J)		! too  many rooms?
	IF(RDESC1(J).NE.0) CALL BUG(16,J)	! duplicate room?
	RDESC1(J)=-RECNO
	GO TO 300
C
C Section 4-- short room descriptions
C
240	IF(J.GT.RMAX) CALL BUG(17,J)
	IF(RDESC2.EQ.0) RDESC2=-RECNO+1		! record base
	GO TO 300
C
C Section 5-- long object descriptions
C
250	IF(J.GT.OMAX) CALL BUG(19,J)		! too many objects?
	IF(ODESC1(J).NE.0) CALL BUG(20,J)	! duplicate objects?
	ODESC1(J)=-RECNO
	GO TO 300
C
C Section 6-- short object descriptions
C
260	IF(J.GT.OMAX) CALL BUG(21,J)
	IF(ODESC2(J).NE.0) CALL BUG(22,J)
	ODESC2(J)=-RECNO
	GO TO 300
C
C Section 7-- untouched object descriptions
C
270	IF(J.GT.OMAX) CALL BUG(23,J)
	IF(ODESCO(J).NE.0) CALL BUG(24,J)
	ODESCO(J)=-RECNO
	GO TO 300
C
C Section 8-- reading material
C
280	IF(J.GT.OMAX) CALL BUG(25,J)
	IF(OREAD(J).NE.0) CALL BUG(26,J)
	OREAD(J)=-RECNO
	GO TO 300
C
C Here to write out current line of text.
C
300	CALL TXCRYP(RECNO,INBUF)		! encrypt valid line
	WRITE(DBCH,REC=RECNO) J,INBUF		! write out record no, line
	RECNO=RECNO+1				! on to next record
	PREVJ=J
	GO TO 110

C DBCOMP, PAGE 3
C
C Section 9-- room data
C
400	READ(1,90) J,INBUF			! get room no.
	IF(J.LT.0) GO TO 80			! -1 is end of section.
	IF(J.EQ.0) GO TO 400			! 0 is ignored.
	IF(J.GT.RMAX) CALL BUG(28,J)		! too many rooms?
	RLNT=MAX0(RLNT,J)			! count rooms.
C
	READ(1,425) RACTIO(J),RVAL(J),RFLAG(J)	! read room numerics.
425	FORMAT(2I6,O7)
	IF((RFLAG(J).AND.REND).EQ.0)
	1	MXSCOR=MXSCOR+RVAL(J)		! update best score.
	IF((RFLAG(J).AND.REND).NE.0)
	1	EGMXSC=EGMXSC+RVAL(J)		! if endgame, update best.
	PREVX=0					! no prev entry.
C
C Here to loop on exit descriptions
C
405	READ(1,415) INDIR,XTYPE,XROOM1		! read next exit.
415	FORMAT(A,2I6)
	IF(INDIR.EQ.'  ') GO TO 400		! blank line ends exits.
	DO 410 K=1,DIRMAX			! look up dir in table.
	  IF(INDIR.EQ.STRDIR(K)) GO TO 420	! if found, ok.
410	CONTINUE
	CALL BUG(29,J)				! invalid direction.
C
C Have translated direction, validate other fields.
C
420	IF((XTYPE.LE.0).OR.(XTYPE.GT.(XFMASK+1))) CALL BUG(30,XTYPE)
	IF(XROOM1.GT.RMAX) CALL BUG(31,XROOM1)
	IF(REXIT(J).EQ.0) REXIT(J)=XLNT		! record first.
	IF((XLNT+XELNT(XTYPE)).GT.XXMAX) CALL BUG(32,XLNT)
	TRAVEL(XLNT)=CDIR(K)+((XTYPE-1)*XFSHFT)+XROOM1+XLFLAG
	IF(PREVX.NE.0) TRAVEL(PREVX)=TRAVEL(PREVX)-XLFLAG
	GO TO (480,470,460,450),XTYPE		! do unique processing.
	CALL BUG(30,XTYPE)
C
C Door entry-- read object,applicable,string
C
450	READ(1,455) XOBJ,XACTIO,INBUF
455	FORMAT(2I6,A)
	IF((XOBJ.LE.0).OR.(XOBJ.GT.OMAX)) CALL BUG(34,XOBJ)
	GO TO 468
C
C Conditional exit entry-- read flag,applicable,string
C
460	READ(1,465) XOBJ,XACTIO,INBUF
465	FORMAT(2I6,A)
	IF((XOBJ.LE.0).OR.(XOBJ.GT.FMAX)) CALL BUG(35,XOBJ)
468	TRAVEL(XLNT+2)=(XACTIO*XASHFT)+XOBJ
	GO TO 478
C
C No exit entry-- read string
C
470	READ(1,475) INBUF
475	FORMAT(A)
478	IF(INBUF(1:1).EQ.' ') GO TO 480		! blank line?
	CALL TXCRYP(RECNO,INBUF)		! encrypt record.
	WRITE(DBCH,REC=RECNO) RECNO,INBUF	! no, write out.
	TRAVEL(XLNT+1)=-RECNO
	RECNO=RECNO+1
C
C Common processing, advance over entry.
C
480	PREVX=XLNT				! record prev entry.
	XLNT=XLNT+XELNT(XTYPE)
	GO TO 405

C DBCOMP, PAGE 4
C
C Section 11-- global objects
C Section 10-- normal objects
C
600	STRBIT=OLNT				! record start of stars.
500	READ(1,90) J,INBUF			! get obj number.
	IF(J.LT.0) GO TO 80			! -1 is end section.
	IF(J.EQ.0) GO TO 500			! 0 is ignored.
	IF(J.GT.OMAX) CALL BUG(27,J)
	OLNT=MAX0(OLNT,J)
C
	READ(1,560) OROOM(J),OCAN(J),OACTIO(J),OFLAG1(J),
	1	OFLAG2(J),OFVAL(J),OTVAL(J),OSIZE(J),OCAPAC(J)
560	FORMAT(3I6,2O7,4I6)
C
	IF(OROOM(J).LT.0)
	1	OROOM(J)=IABS(OROOM(J))*HFACTR	! if puzz obj, to hyper.
	MXSCOR=MXSCOR+OFVAL(J)+OTVAL(J)		! update best score.
	GO TO 500
C
C Section 12-- villains
C
700	READ(1,710) J,K,L,INBUF			! read villain no.
710	FORMAT(3I5,A)
	IF(J.LT.0) GO TO 80
	IF(J.EQ.0) GO TO 700
	VLNT=VLNT+1
	IF(VLNT.GT.VMAX) CALL BUG(36,VLNT)	! too many villains?
	VILLNS(VLNT)=J				! no, add to list.
	VBEST(VLNT)=K				! note best weapon.
	VMELEE(VLNT)=L				! note melee.
	GO TO 700
C
C Section 13-- clock events
C
800	READ(1,90) J,INBUF			! read event no.
	IF(J.LT.0) GO TO 80
	IF(J.EQ.0) GO TO 800
	IF(J.GT.CMAX) CALL BUG(37,J)
	CLNT=MAX0(CLNT,J)
C
	READ(1,890) CACTIO(J),CTICK(J),CFLAG(J),CCNCEL(J)
890	FORMAT(2I6,2L2)
	GO TO 800
C
C Section 14-- adventurers
C
900	READ(1,90) J,INBUF			! read adv no.
	IF(J.LT.0) GO TO 80
	IF(J.EQ.0) GO TO 900
	IF(J.GT.AMAX) CALL BUG(38,J)
	ALNT=MAX0(ALNT,J)
C
	READ(1,990) AROOM(J),AOBJ(J),AACTIO(J),ASTREN(J)
990	FORMAT(4I6)
	GO TO 900
C
C Section 15-- multi-room objects
C
950	READ(1,955) J,K,INBUF
955	FORMAT(2I5,A)
	IF(J.LT.0) GO TO 80			! -1 is end section.
	IF(J.EQ.0) GO TO 950
	R2LNT=R2LNT+1				! get next table slot.
	IF(R2LNT.GT.R2MAX) CALL BUG(33,R2LNT)	! too many?
	O2(R2LNT)=J
	R2(R2LNT)=K
	GO TO 950				! onward.

C DBCOMP, PAGE 5
C
C Initialization is complete.
C Write out new index file for next time around.
C
1000	J=-1					! force end of last rec.
	WRITE(DBCH,REC=RECNO) J,INBUF		! write guard rec.
	RECNO=RECNO+1
	CLOSE (UNIT=1)				! close txt file.
C
	OPEN (UNIT=1,NAME='dindx',TYPE='NEW',
	1	FORM='FORMATTED',ACCESS='SEQUENTIAL',ERR=1925)
	TYPE 1005
1005	FORMAT(/' Creating new "DINDX.DAT"...')
C
	WRITE(1,1030) VMAJ,VMIN
	WRITE(1,1025) VEDIT
	WRITE(1,1030) MXSCOR,STRBIT,EGMXSC
	WRITE(1,1030) RLNT,RDESC2,RDESC1,REXIT,RACTIO,RVAL,RFLAG
	WRITE(1,1030) XLNT,TRAVEL
	WRITE(1,1030) OLNT,ODESC1,ODESC2,ODESCO,OACTIO,OFLAG1,OFLAG2,
	1	OFVAL,OTVAL,OSIZE,OCAPAC,OROOM,OADV,OCAN,OREAD
	WRITE(1,1030) R2LNT,O2,R2
	WRITE(1,1030) CLNT,CTICK,CACTIO
	WRITE(1,1035) CFLAG,CCNCEL
	WRITE(1,1030) VLNT,VILLNS,VPROB,VOPPS,VBEST,VMELEE
	WRITE(1,1030) ALNT,AROOM,ASCORE,AVEHIC,AOBJ,AACTIO,ASTREN,AFLAG
	WRITE(1,1030) MBASE,MLNT,RTEXT
C
	CLOSE (UNIT=1)
	CLOSE (UNIT=DBCH)
C
1025	FORMAT(1X,A)
1030	FORMAT(1X,I6)
1035	FORMAT(1X,L2)

C DBCOMP, PAGE 6
C
C The internal data base is now established.
C Print summary and exit-- INITFL succeeds.
C
	INITFL=.TRUE.
C
	TYPE 1050,RLNT,RMAX,XLNT,XXMAX,OLNT,OMAX,MLNT,MMAX,
	1	VLNT,VMAX,ALNT,AMAX,CLNT,CMAX,R2LNT,R2MAX
1050	FORMAT(' Used:'/1X,I5,' OF',I5,' rooms'/
	11X,I5,' OF',I5,' exits'/
	21X,I5,' OF',I5,' objects'/
	31X,I5,' OF',I5,' messages'/
	41X,I5,' OF',I5,' villains'/
	51X,I5,' OF',I5,' adventurers'/
	61X,I5,' OF',I5,' clock events'/
	71X,I5,' OF',I5,' room2 slots')
	TYPE 1150,MXSCOR,EGMXSC,RECNO,RDESC2,MBASE,STRBIT
1150	FORMAT(' Max score =',I5/' EG score =',I5/
	1' Max RECNO = ',I5/' RDESC2 base =',I5/
	1' Melee start =',I5/' Star mask =',I7/)
C
	RETURN
C
C Errors-- INITFL fails.
C
1900	TYPE 910				! dtext.txt error
	RETURN
1925	TYPE 920				! dindx.dat error
	RETURN
1950	TYPE 960				! dtext.dat error
	RETURN
910	FORMAT(' I can''t open "DTEXT.TXT".')
920	FORMAT(' I can''t open "DINDX.DAT".')
960	FORMAT(' I can''t open "DTEXT.DAT".')
C
	END

C
C BUG  - Subroutine to report program error
C
	SUBROUTINE BUG(I,J)
	IMPLICIT INTEGER (A-Z)
C
	TYPE 100,I,J
	CALL EXIT
C
100	FORMAT(' Fatal program error ',I2,', parameter = ',I6)
	END
C
C GAME - Dummy subroutine
C
	SUBROUTINE GAME
	IMPLICIT INTEGER (A-Z)
C
	RETURN
	END
