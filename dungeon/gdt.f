C Game debugging tool for DUNGEON
C
C COPYRIGHT 1980, 1990, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA.
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK
C
C Declarations
C
	SUBROUTINE GDT
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	PARAMETER (DBGMAX=38)			! number of debug commands
	CHARACTER*2 CMD,DBGCMD(DBGMAX),DBGSML(DBGMAX)
	INTEGER ARGTYP(DBGMAX)
	LOGICAL VALID1,VALID2,VALID3
C
C Equivalanced array definitions
C
	INTEGER EQR(RMAX,5)
	EQUIVALENCE (EQR(1,1),RDESC1(1))
	INTEGER EQO(OMAX,14)
	EQUIVALENCE (EQO(1,1),ODESC1(1))
	INTEGER EQC(CMAX,2)
	EQUIVALENCE (EQC(1,1),CTICK(1))
	INTEGER EQV(VMAX,5)
	EQUIVALENCE (EQV(1,1),VILLNS(1))
	INTEGER EQA(AMAX,7)
	EQUIVALENCE (EQA(1,1),AROOM(1))
C
C Functions and data
C
	VALID1(A1,L1)=(A1.GT.0).AND.(A1.LE.L1)
	VALID2(A1,A2,L1)=VALID1(A1,L1).AND.VALID1(A2,L1).AND.
	1	(A1.LE.A2)
	VALID3(A1,L1,A2,L2)=VALID1(A1,L1).AND.VALID1(A2,L2)
	DATA DBGCMD/'DR','DO','DA','DC','DX','DH','DL','DV','DF','DS',
	1	'AF','HE','NR','NT','NC','ND','RR','RT','RC','RD',
	2	'TK','EX','AR','AO','AA','AC','AX','AV','D2','DN',
	3	'AN','DM','DT','AH','DP','PD','DZ','AZ'/
	DATA DBGSML/'dr','do','da','dc','dx','dh','dl','dv','df','ds',
	1	'af','he','nr','nt','nc','nd','rr','rt','rc','rd',
	2	'tk','ex','ar','ao','aa','ac','ax','av','d2','dn',
	3	'an','dm','dt','ah','dp','pd','dz','az'/
	DATA ARGTYP/  2 ,  2 ,  2 ,  2 ,  2 ,  0 ,  0 ,  2 ,  2 ,  0 ,
	1	  1 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,
	2	  1 ,  0 ,  3 ,  3 ,  3 ,  3 ,  1 ,  3 ,  2 ,  2 ,
	3	  1 ,  2 ,  1 ,  0 ,  0 ,  0 ,  0 ,  1 /

C GDT, PAGE 2
C
C First, validate that the caller is an implementer.
C
	IF(GDTFLG.NE.0) GO TO 2000		! if ok, skip.
	WRITE(OUTCH,100)			! not an implementer.
	RETURN					! boot him off
C
100	FORMAT(' You are not an authorized user.')

c GDT, PAGE 2A
C
C Here to get next command.
C
2000	WRITE(OUTCH,200)			! output prompt.
	READ(INPCH,210,ERR=2200,END=31000) CMD	! get command.
	IF(CMD.EQ.'  ') GO TO 2000		! ignore blanks.
	DO 2100 I=1,DBGMAX			! look it up.
	  IF((CMD.EQ.DBGCMD(I)).OR.(CMD.EQ.DBGSML(I))) GO TO 2300
2100	CONTINUE				! found?
2200	WRITE(OUTCH,220)			! no, lose.
	GO TO 2000
C
200	FORMAT(' GDT> ',$)
210	FORMAT(A2)
220	FORMAT(' ?')
230	FORMAT(2I6)
240	FORMAT(I6)
225	FORMAT(' Limits:   ',$)
235	FORMAT(' Entry:    ',$)
245	FORMAT(' Idx,Ary:  ',$)
c
2300	GO TO (2400,2500,2600,2700),ARGTYP(I)+1	! branch on arg type.
	GO TO 2200				! illegal type.
C
2700	WRITE(OUTCH,245)			! type 3, request array coords.
	READ(INPCH,230,ERR=2200,END=2000) J,K
	GO TO 2400
C
2600	WRITE(OUTCH,225)			! type 2, read bounds.
	READ(INPCH,230,ERR=2200,END=2000) J,K
	IF(K.EQ.0) K=J
	GO TO 2400
C
2500	WRITE(OUTCH,235)			! type 1, read entry no.
	READ(INPCH,240,ERR=2200,END=2000) J
2400	GO TO (10000,11000,12000,13000,14000,15000,16000,17000,18000,
	1 19000,20000,21000,22000,23000,24000,25000,26000,27000,28000,
	2 29000,30000,31000,32000,33000,34000,35000,36000,37000,38000,
	3 39000,40000,41000,42000,43000,44000,45000,46000,47000),I
	GO TO 2200				! what???

C GDT, PAGE 3
C
C DR-- Display Rooms
C
10000	IF(.NOT.VALID2(J,K,RLNT)) GO TO 2200	! args valid?
	WRITE(OUTCH,300)			! col hdrs.
	DO 10100 I=J,K
	  WRITE(OUTCH,310) I,(EQR(I,L),L=1,5)
10100	CONTINUE
	GO TO 2000
C
300	FORMAT(' RM#  DESC1  EXITS ACTION  VALUE  FLAGS')
310	FORMAT(1X,I3,4(1X,I6),1X,O6)
C
C DO-- Display Objects
C
11000	IF(.NOT.VALID2(J,K,OLNT)) GO TO 2200	! args valid?
	WRITE(OUTCH,320)			! col hdrs
	DO 11100 I=J,K
	  WRITE(OUTCH,330) I,(EQO(I,L),L=1,14)
11100	CONTINUE
	GO TO 2000
C
320	FORMAT(' OB# DESC1 DESC2 DESCO ACT FLAGS1 FLAGS2 FV TV',
	1 '  SIZE CAPAC  ROOM ADV CON  READ')
330	FORMAT(1X,I3,3I6,I4,2O7,2I3,2I6,I6,2I4,I6)
C
C DA-- Display Adventurers
C
12000	IF(.NOT.VALID2(J,K,ALNT)) GO TO 2200	! args valid?
	WRITE(OUTCH,340)
	DO 12100 I=J,K
	  WRITE(OUTCH,350) I,(EQA(I,L),L=1,7)
12100	CONTINUE
	GO TO 2000
C
340	FORMAT(' AD#   ROOM  SCORE  VEHIC OBJECT ACTION  STREN  FLAGS')
350	FORMAT(1X,I3,6(1X,I6),1X,O6)
C
C DC-- Display Clock Events
C
13000	IF(.NOT.VALID2(J,K,CLNT)) GO TO 2200	! args valid?
	WRITE(OUTCH,360)
	DO 13100 I=J,K
	  WRITE(OUTCH,370) I,(EQC(I,L),L=1,2),CFLAG(I),CCNCEL(I)
13100	CONTINUE
	GO TO 2000
C
360	FORMAT(' CL#   TICK ACTION  FLAG  CANCEL')
370	FORMAT(1X,I3,1X,I6,1X,I6,5X,L1,5X,L1)
C
C DX-- Display Exits
C
14000	IF(.NOT.VALID2(J,K,XLNT)) GO TO 2200	! args valid?
	WRITE(OUTCH,380)			! col hdrs.
	DO 14100 I=J,K,10			! ten per line.
	  L=MIN0(I+9,K)				! compute end of line.
	  WRITE(OUTCH,390) I,L
	  DO 14050 L1=I,L			! loop through data
	    IF(TRAVEL(L1).GE.0) WRITE(OUTCH,391) TRAVEL(L1)
	    IF(TRAVEL(L1).LT.0) WRITE(OUTCH,392) TRAVEL(L1)
14050	  CONTINUE
	WRITE(OUTCH,393)
14100	CONTINUE
	GO TO 2000
C
380	FORMAT('     RANGE CONTENTS')
390	FORMAT(1X,I4,'-',I4,1X,$)
391	FORMAT('+',O7,$)
392	FORMAT('+',I7,$)
393	FORMAT('+')
C
C DH-- Display Hacks
C
15000	WRITE(OUTCH,400) THFPOS,THFFLG,THFACT,SWDACT,SWDSTA
	GO TO 2000
C
400	FORMAT(' THFPOS=',I6,', THFFLG=',L2,', THFACT=',L2/
	1' SWDACT=',L2,', SWDSTA=',I2)
C
C DL-- Display Lengths
C
16000	WRITE(OUTCH,410) RLNT,XLNT,OLNT,CLNT,VLNT,ALNT,MLNT,R2LNT,
	1	MBASE,STRBIT
	GO TO 2000
C
410	FORMAT(' R=',I6,', X=',I6,', O=',I6,', C=',I6/
	1' V=',I6,', A=',I6,', M=',I6,', R2=',I5/
	2' MBASE=',I6,', STRBIT=',I6)
C
C DV-- Display Villains
C
17000	IF(.NOT.VALID2(J,K,VLNT)) GO TO 2200	! args valid?
	WRITE(OUTCH,420)			! col hdrs
	DO 17100 I=J,K
	  WRITE(OUTCH,430) I,(EQV(I,L),L=1,5)
17100	CONTINUE
	GO TO 2000
C
420	FORMAT(' VL# OBJECT   PROB   OPPS   BEST  MELEE')
430	FORMAT(1X,I3,5(1X,I6))
C
C DF-- Display Flags
C
18000	IF(.NOT.VALID2(J,K,FMAX)) GO TO 2200	! args valid?
	DO 18100 I=J,K
	  WRITE(OUTCH,440) I,FLAGS(I)
18100	CONTINUE
	GO TO 2000
C
440	FORMAT(' Flag #',I2,' = ',L1)
C
C DS-- Display State
C
19000	WRITE(OUTCH,450) PRSA,PRSO,PRSI,PRSWON,PRSCON
	WRITE(OUTCH,460) WINNER,HERE,TELFLG
	WRITE(OUTCH,470) MOVES,DEATHS,RWSCOR,MXSCOR,MXLOAD,LTSHFT,BLOC,
	1	MUNGRM,HS,EGSCOR,EGMXSC
	WRITE(OUTCH,475) FROMDR,SCOLRM,SCOLAC
	GO TO 2000
C
450	FORMAT(' Parse vector=',3(1X,I6),1X,L6,1X,I6)
460	FORMAT(' Play vector= ',2(1X,I6),1X,L6)
470	FORMAT(' State vector=',7(1X,I6)/14X,4(1X,I6))
475	FORMAT(' Scol vector= ',1X,O6,2(1X,I6))

C GDT, PAGE 4
C
C AF-- Alter Flags
C
20000	IF(.NOT.VALID1(J,FMAX)) GO TO 2200	! entry no valid?
	WRITE(OUTCH,480) FLAGS(J)		! type old, get new.
	READ(INPCH,490,ERR=2200,END=2000) FLAGS(J)
	GO TO 2000
C
480	FORMAT(' Old=',L2,6X,'New= ',$)
490	FORMAT(L1)
C
C 21000-- Help
C
21000	WRITE(OUTCH,900)
	GO TO 2000
C
900	FORMAT(' Valid commands are:'/' AA- Alter ADVS'/
	1' AC- Alter CEVENT'/' AF- Alter FINDEX'/' AH- Alter HERE'/
	2' AN- Alter switches'/' AO- Alter OBJCTS'/' AR- Alter ROOMS'/
	3' AV- Alter VILLS'/' AX- Alter EXITS'/
	3' AZ- Alter PUZZLE'/' DA- Display ADVS'/
	4' DC- Display CEVENT'/' DF- Display FINDEX'/' DH- Display HACKS'/
	5' DL- Display lengths'/' DM- Display RTEXT'/
	6' DN- Display switches'/
	6' DO- Display OBJCTS'/' DP- Display parser'/
	6' DR- Display ROOMS'/' DS- Display state'/' DT- Display text'/
	7' DV- Display VILLS'/' DX- Display EXITS'/' DZ- Display PUZZLE'/
	8' D2- Display ROOM2'/' EX- Exit'/' HE- Type this message'/
	9' NC- No cyclops'/' ND- No deaths'/' NR- No robber'/
	1' NT- No troll'/' PD- Program detail'/
	1' RC- Restore cyclops'/' RD- Restore deaths'/
	2' RR- Restore robber'/' RT- Restore troll'/' TK- Take.')
C
C NR-- No Robber
C
22000	THFFLG=.FALSE.				! disable robber.
	THFACT=.FALSE.
	CALL NEWSTA(THIEF,0,0,0,0)		! vanish thief.
	WRITE(OUTCH,500)
	GO TO 2000
C
500	FORMAT(' No robber.')
C
C NT-- No Troll
C
23000	TROLLF=.TRUE.
	CALL NEWSTA(TROLL,0,0,0,0)
	WRITE(OUTCH,510)
	GO TO 2000
C
510	FORMAT(' No troll.')
C
C NC-- No Cyclops
C
24000	CYCLOF=.TRUE.
	CALL NEWSTA(CYCLO,0,0,0,0)
	WRITE(OUTCH,520)
	GO TO 2000
C
520	FORMAT(' No cyclops.')
C
C ND-- Immortality Mode
C
25000	DBGFLG=1
	WRITE(OUTCH,530)
	GO TO 2000
C
530	FORMAT(' No deaths.')
C
C RR-- Restore Robber
C
26000	THFACT=.TRUE.
	WRITE(OUTCH,540)
	GO TO 2000
C
540	FORMAT(' Restored robber.')
C
C RT-- Restore Troll
C
27000	TROLLF=.FALSE.
	CALL NEWSTA(TROLL,0,MTROL,0,0)
	WRITE(OUTCH,550)
	GO TO 2000
C
550	FORMAT(' Restored troll.')
C
C RC-- Restore Cyclops
C
28000	CYCLOF=.FALSE.
	MAGICF=.FALSE.
	CALL NEWSTA(CYCLO,0,MCYCL,0,0)
	WRITE(OUTCH,560)
	GO TO 2000
C
560	FORMAT(' Restored cyclops.')
C
C RD-- Mortal Mode
C
29000	DBGFLG=0
	WRITE(OUTCH,570)
	GO TO 2000
C
570	FORMAT(' Restored deaths.')

C GDT, PAGE 5
C
C TK-- Take
C
30000	IF(.NOT.VALID1(J,OLNT)) GO TO 2200	! valid object?
	CALL NEWSTA(J,0,0,0,WINNER)		! yes, take object.
	WRITE(OUTCH,580)			! tell.
	GO TO 2000
C
580	FORMAT(' Taken.')
C
C EX-- Goodbye
C
31000	RETURN
C
C AR-- Alter Room Entry
C
32000	IF(.NOT.VALID3(J,RLNT,K,5)) GO TO 2200	! indices valid?
	WRITE(OUTCH,590) EQR(J,K)		! type old, get new.
	READ(INPCH,600,ERR=2200,END=2000) EQR(J,K)
	GO TO 2000
C
590	FORMAT(' Old= ',I6,6X,'New= ',$)
600	FORMAT(I6)
C
C AO-- Alter Object Entry
C
33000	IF(.NOT.VALID3(J,OLNT,K,14)) GO TO 2200	! indices valid?
	WRITE(OUTCH,590) EQO(J,K)
	READ(INPCH,600,ERR=2200,END=2000) EQO(J,K)
	GO TO 2000
C
C AA-- Alter Advs Entry
C
34000	IF(.NOT.VALID3(J,ALNT,K,7)) GO TO 2200	! indices valid?
	WRITE(OUTCH,590) EQA(J,K)
	READ(INPCH,600,ERR=2200,END=2000) EQA(J,K)
	GO TO 2000
C
C AC-- Alter Clock Events
C
35000	IF(.NOT.VALID3(J,CLNT,K,4)) GO TO 2200	! indices valid?
	IF(K.EQ.3) GO TO 35500			! flags entry?
	IF(K.EQ.4) GO TO 35600			! cancel entry?
	WRITE(OUTCH,590) EQC(J,K)
	READ(INPCH,600,ERR=2200,END=2000) EQC(J,K)
	GO TO 2000
C
35500	WRITE(OUTCH,480) CFLAG(J)
	READ(INPCH,490,ERR=2200,END=2000) CFLAG(J)
	GO TO 2000
C
35600	WRITE(OUTCH,480) CCNCEL(J)
	READ(INPCH,490,ERR=2200,END=2000) CCNCEL(J)
	GO TO 2000

C GDT, PAGE 6
C
C AX-- Alter Exits
C
36000	IF(.NOT.VALID1(J,XLNT)) GO TO 2200	! entry no valid?
	IF(TRAVEL(J).LT.0) GO TO 36100		! string entry?
	WRITE(OUTCH,610) TRAVEL(J)
	READ(INPCH,620,ERR=2200,END=2000) TRAVEL(J)
	GO TO 2000
C
36100	WRITE(OUTCH,590) TRAVEL(J)
	READ(INPCH,600,ERR=2200,END=2000) TRAVEL(J)
	GO TO 2000
C
610	FORMAT(' Old= ',O6,6X,'New= ',$)
620	FORMAT(O6)
C
C AV-- Alter Villains
C
37000	IF(.NOT.VALID3(J,VLNT,K,5)) GO TO 2200	! indices valid?
	WRITE(OUTCH,590) EQV(J,K)
	READ(INPCH,600,ERR=2200,END=2000) EQV(J,K)
	GO TO 2000
C
C D2-- Display Room2 List
C
38000	IF(.NOT.VALID2(J,K,R2LNT)) GO TO 2200
	DO 38100 I=J,K
	  WRITE(OUTCH,630) I,R2(I),O2(I)
38100	CONTINUE
	GO TO 2000
C
630	FORMAT(' #',I2,'   Room=',I6,'   Obj=',I6)
C
C DN-- Display Switches
C
39000	IF(.NOT.VALID2(J,K,SMAX)) GO TO 2200	! valid?
	DO 39100 I=J,K
	  WRITE(OUTCH,640) I,SWITCH(I)
39100	CONTINUE
	GO TO 2000
C
640	FORMAT(' Switch #',I2,' = ',I6)
C
C AN-- Alter Switches
C
40000	IF(.NOT.VALID1(J,SMAX)) GO TO 2200	! valid entry?
	WRITE(OUTCH,590) SWITCH(J)
	READ(INPCH,600,ERR=2200,END=2000) SWITCH(J)
	GO TO 2000
C
C DM-- Display Messages
C
41000	IF(.NOT.VALID2(J,K,MLNT)) GO TO 2200	! valid limits?
	WRITE(OUTCH,380)
	DO 41100 I=J,K,10
	  L=MIN0(I+9,K)
	  WRITE(OUTCH,650) I,L,(RTEXT(L1),L1=I,L)
41100	CONTINUE
	GO TO 2000
C
650	FORMAT(1X,I4,'-',I4,10(1X,I6))
C
C DT-- Display Text
C
42000	CALL RSPEAK(J)
	GO TO 2000
C
C AH-- Alter Here
C
43000	WRITE(OUTCH,590) HERE
	READ(INPCH,600,ERR=2200,END=2000) HERE
	AROOM(PLAYER)=HERE
	GO TO 2000
C
C DP-- Display Parser State
C
44000	WRITE(OUTCH,660)
	1	OFLAG,OACT,OPREP1,OOBJ1,OPREP,ONAME,OPREP2,OOBJ2,
	2	LASTIT,ACT,OBJ1,OBJ2,PREP1,PREP2,SYN,
	3	BUNLNT,BUNSUB,BUNVEC
	GO TO 2000
C
660	FORMAT(' ORPHS= ',5I7,' "',A,'" ',2I7/' IT=    ',I7/
	1' PV=    ',5I7/' SYN=   ',2O7,4I7/15X,O7,4I7/
	2' BUNCH= ',7I7/22X,5I7)
C
C PD-- Program Detail
C
45000	WRITE(OUTCH,610) PRSFLG			! type old, get new.
	READ(INPCH,620,ERR=2200,END=2000) PRSFLG
	GO TO 2000
C
C DZ-- Display Puzzle Room
C
46000	DO 46100 I=1,64,8			! display puzzle
	  WRITE(OUTCH,670) (CPVEC(J),J=I,I+7)
46100	CONTINUE
	GO TO 2000
C
670	FORMAT(2X,8I3)
C
C AZ-- Alter Puzzle Room
C
47000	IF(.NOT.VALID1(J,64)) GO TO 2200	! valid entry?
	WRITE(OUTCH,590) CPVEC(J)		! output old,
	READ(OUTCH,600) CPVEC(J)		! get new.
	GO TO 2000
C
	END
