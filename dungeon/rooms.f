C Room processors for DUNGEON
C
C COPYRIGHT 1980, 1990, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA.
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK
C
C 11-Sep-94	RMS	Fixed bugs in Tree, Slide, Cave2, Magnet, object
C			substitution.  Added Puzzle Anteroom.
C 30-Jun-92	RMS	Changed file names to lower case.
C
C RAPPLI- Room routines
C
C Declarations
C
	SUBROUTINE RAPPLI(RI)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL QOPEN,QON,QHERE,PROB,F
	LOGICAL MOVETO,LIT,RMDESC,QEMPTY
C
C Functions and data
C
	QOPEN(R)=(OFLAG2(R).AND.OPENBT).NE.0
	QON(R)=(OFLAG1(R).AND.ONBT).NE.0
C
	IF(RI.EQ.0) RETURN			! return if naught.
	GO TO (  1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000,
	1 10000,11000,12000,13000,14000,15000,16000,17000,18000,19000,
	2 20000,21000,22000,23000,24000,25000,26000,27000,28000,29000,
	3 30000,31000,32000,33000,34000,35000,36000,37000,38000,39000,
	4 40000,41000,42000,43000,44000,45000,46000,47000,48000,49000,
	5 50000,51000,52000,53000,54000,55000,56000,57000,58000,59000,
	7 60000,61000,62000,63000,64000),RI
	CALL BUG(1,RI)

C RAPPLI, PAGE 2
C
C R1--	East of house
C
1000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=13					! assume closed.
	IF(QOPEN(WINDO)) I=12			! if open, ajar.
	CALL RSPSUB(11,I)			! describe.
	RETURN
C
C R2--	Kitchen
C
2000	IF(PRSA.NE.LOOKW) GO TO 2100		! look?
	I=13					! assume closed.
	IF(QOPEN(WINDO)) I=12			! if open, ajar.
	CALL RSPSUB(14,I)			! describe.
	RETURN
C
2100	IF((PRSA.NE.WALKIW).OR.DEADF.OR..NOT.BROC1F.OR.BROC2F) RETURN
	CFLAG(CEVBRO)=.TRUE.			! send for brochure.
	CTICK(CEVBRO)=3
	RETURN
C
C R3--	Living Room
C
3000	IF(PRSA.NE.LOOKW) GO TO 3500		! look?
	I=15					! assume no hole.
	IF(MAGICF) I=16				! if magicf, cyclops hole.
	CALL RSPEAK(I)				! describe.
	I=17+ORRUG				! assume initial state.
	IF(QOPEN(DOOR)) I=I+2			! door open?
	CALL RSPEAK(I)				! describe.
	RETURN
C
C Not a look word, reevaluate trophy case.
C
3500	IF((PRSA.NE.TAKEW).AND.((PRSA.NE.PUTW).OR.(PRSI.NE.TCASE)))
	1	RETURN				! take or put in?
	ASCORE(WINNER)=RWSCOR			! score trophy case.
	DO 3600 I=1,OLNT			! retain raw score as well.
	  J=I					! find out if in case.
3550	  J=OCAN(J)				! trace ownership.
	  IF(J.EQ.0) GO TO 3600
	  IF(J.NE.TCASE) GO TO 3550		! do all levels.
	  ASCORE(WINNER)=ASCORE(WINNER)+OTVAL(I)
3600	CONTINUE
	CALL SCRUPD(0)				! see if endgame trig.
	RETURN

C RAPPLI, PAGE 3
C
C R4--	Cellar
C
4000	IF(PRSA.NE.LOOKW) GO TO 4500		! look?
	CALL RSPEAK(21)				! describe cellar.
	IF(QOPEN(DOOR)) CALL RSPEAK(623)	! describe trap door if open.
	RETURN
C
4500	IF(PRSA.NE.WALKIW) RETURN		! walkin?
	IF((OFLAG2(DOOR).AND.(OPENBT+TCHBT)).NE.OPENBT) RETURN
	OFLAG2(DOOR)=(OFLAG2(DOOR).OR.TCHBT).AND. .NOT.OPENBT
	CALL RSPEAK(22)				! slam and bolt door.
	RETURN
C
C R5--	Grating Room
C
5000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(23)				! describe.
	I=24					! assume locked.
	IF(GRUNLF) I=26				! unlocked?
	IF(QOPEN(GRATE)) I=25			! open?
	CALL RSPEAK(I)				! describe grate.
	RETURN
C
C R6--	Clearing
C
6000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(27)				! describe.
	I=0					! assume no grating.
	IF(RVCLR.NE.0) I=28			! leaves moved?
	IF(QOPEN(GRATE)) I=29			! grate open?
	CALL RSPEAK(I)				! describe grate.
	RETURN

C RAPPLI, PAGE 4
C
C R7--	Reservoir south
C
7000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=31					! assume full.
	IF(LWTIDF) I=32				! if low tide, empty.
	CALL RSPEAK(I)				! describe.
	CALL RSPEAK(33)				! describe exits.
	RETURN
C
C R8--	Reservoir
C
8000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=34					! assume full.
	IF(LWTIDF) I=35				! if low tide, emtpy.
	CALL RSPEAK(I)				! describe.
	RETURN
C
C R9--	Reservoir north
C
9000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=36					! you get the idea.
	IF(LWTIDF) I=37
	CALL RSPEAK(I)
	CALL RSPEAK(38)
	RETURN
C
C R10--	Glacier Room
C
10000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(39)				! basic description.
	I=0					! assume no changes.
	IF(GLACMF) I=40				! partial melt?
	IF(GLACRF) I=41				! complete melt?
	CALL RSPEAK(I)				! describe.
	RETURN
C
C R11--	Forest Room
C
11000	IF(PRSA.NE.WALKIW) RETURN
	CFLAG(CEVFOR)=.TRUE.			! if walk in, birdie.
	CTICK(CEVFOR)=-1
	RETURN
C
C R12--	Mirror Room
C
12000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(42)				! describe.
	IF(MIRRMF) CALL RSPEAK(43)		! if broken, nasty remark.
	RETURN

C RAPPLI, PAGE 5
C
C R13--	Cave2
C
13000	IF(PRSA.NE.WALKIW) RETURN		! walkin?
	IF(PROB(50,20).OR.(OADV(CANDL).NE.WINNER).OR.
	1	.NOT.QON(CANDL)) RETURN		! blow em out?
	OFLAG1(CANDL)=OFLAG1(CANDL).AND. .NOT. ONBT	! yes.
	CALL RSPEAK(47)				! tell of winds.
	CFLAG(CEVCND)=.FALSE.			! halt candle countdown.
	IF(.NOT.LIT(HERE)) CALL RSPEAK(406)	! now pitch black.
	RETURN
C
C R14--	Boom Room
C
14000	J=ODESC2(CANDL)				! assume candle.
	IF((OADV(CANDL).EQ.WINNER).AND.QON(CANDL)) GO TO 14100
	J=ODESC2(TORCH)				! assume torch.
	IF((OADV(TORCH).EQ.WINNER).AND.QON(TORCH)) GO TO 14100
	J=ODESC2(MATCH)
	IF((OADV(MATCH).EQ.WINNER).AND.QON(MATCH)) GO TO 14100
	RETURN					! safe
C
14100	IF((PRSA.NE.TRNONW).AND.(PRSA.NE.BURNW))
	1	GO TO 14200			! turn on or burn?
	CALL RSPSUB(294,J)			! boom!
	CALL JIGSUP(44)
	RETURN
C
14200	IF(PRSA.NE.WALKIW) RETURN		! walkin?
	CALL RSPSUB(295,J)			! boom!
	CALL JIGSUP(44)
	RETURN
C
C R15--	No-objs
C
15000	EMPTHF=.TRUE.				! assume true.
	DO 15100 I=1,OLNT			! see if carrying.
	  IF(OADV(I).EQ.WINNER) EMPTHF=.FALSE.
15100	CONTINUE
C
	IF((HERE.NE.BSHAF).OR.(.NOT.LIT(HERE))) RETURN
	CALL SCRUPD(LTSHFT)			! score light shaft.
	LTSHFT=0				! never again.
	RETURN

C RAPPLI, PAGE 6
C
C R16--	Machine Room
C
16000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=46					! assume lid closed.
	IF(QOPEN(MACHI)) I=12			! if open, open.
	CALL RSPSUB(45,I)			! describe.
	RETURN
C
C R17--	Bat Room
C
17000	IF(PRSA.NE.LOOKW) GO TO 17500		! look?
	CALL RSPEAK(48)				! describe room.
	IF(OADV(GARLI).EQ.WINNER) CALL RSPEAK(49) ! bat holds nose.
	RETURN
C
17500	IF((PRSA.NE.WALKIW).OR.(OADV(GARLI).EQ.WINNER)
	1	.OR.DEADF) RETURN		! garlic or dead?
	CALL RSPEAK(50)				! time to fly, jack.
	F=MOVETO(BATDRP(RND(9)+1),WINNER)	! select random dest.
	F=RMDESC(0)				! new room description.
	PRSCON=0				! kill parser.
	RETURN
C
C R18--	Dome Room
C
18000	IF(PRSA.NE.LOOKW) GO TO 18500		! look?
	CALL RSPEAK(51)				! describe.
	IF(DOMEF) CALL RSPEAK(52)		! if rope, describe.
	RETURN
C
18500	IF(PRSA.EQ.LEAPW) CALL JIGSUP(53)	! did he jump???
	RETURN
C
C R19--	Torch Room
C
19000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(54)				! describe.
	IF(DOMEF) CALL RSPEAK(55)		! if rope, describe.
	RETURN
C
C R20--	Carousel Room
C
20000	IF(PRSA.NE.LOOKW) GO TO 20500		! look?
	CALL RSPEAK(56)				! describe.
	IF(.NOT.(CAROFF.OR.DEADF)) CALL RSPEAK(57) ! if not flipped, spin.
	RETURN
C
20500	IF((PRSA.EQ.WALKIW).AND.CAROZF.AND..NOT.DEADF)
	1	CALL JIGSUP(58)			! walked in, zoom, not dead.
	RETURN

C RAPPLI, PAGE 7
C
C R21--	Land of the Living Dead
C
21000	IF(PRSA.NE.LOOKW) GO TO 21100		! look?
	CALL RSPEAK(59)				! describe.
	IF(.NOT.LLDF) CALL RSPEAK(60)		! if not vanished, ghosts.
	RETURN
C
21100	IF(LLDF.OR.(PRSA.NE.RINGW).OR.(PRSO.NE.BELL))
	1	GO TO 21200			! ring bell?
	EXORBF=.TRUE.				! set exorcism bell flag.
	CALL NEWSTA(BELL,0,0,0,0)		! vanish bell.
	CALL NEWSTA(HBELL,967,HERE,0,0)		! insert hot bell.
	IF(LASTIT.EQ.BELL) LASTIT=HBELL		! fix last it reference.
	IF(.NOT.QON(CANDL).OR.(OADV(CANDL).NE.WINNER))
	1	GO TO 21150			! carrying lit candles?
	CALL NEWSTA(CANDL,968,HERE,0,0)		! drop and go out.
	OFLAG1(CANDL)=OFLAG1(CANDL).AND..NOT.ONBT
	CFLAG(CEVCND)=.FALSE.			! disable candle timer.
21150	CFLAG(CEVXB)=.TRUE.			! start bell timer.
	CTICK(CEVXB)=6
	CFLAG(CEVXBH)=.TRUE.			! start cooling timer.
	CTICK(CEVXBH)=20
	RETURN
C
21200	IF(.NOT.EXORBF.OR.EXORCF.OR.(OADV(CANDL).NE.WINNER).OR.
	1	((OFLAG1(CANDL).AND.ONBT).EQ.0)) GO TO 21300
	EXORCF=.TRUE.				! set exorcism candle flag.
	CALL RSPEAK(969)
	CFLAG(CEVXB)=.FALSE.			! turn off bell timer.
	CFLAG(CEVXC)=.TRUE.			! turn on candle timer.
	CTICK(CEVXC)=3
	RETURN
C
21300	IF(.NOT.EXORCF.OR.(PRSA.NE.READW).OR.(PRSO.NE.BOOK))
	1	GO TO 21400			! read book?
	CALL NEWSTA(GHOST,63,0,0,0)		! exorcism complete.
	LLDF=.TRUE.				! set flag.
	CFLAG(CEVXC)=.FALSE.			! turn off candle timer.
	RETURN
C
21400	IF(PRSA.NE.EXORCW) RETURN		! trying exorcism?
	IF(LLDF) GO TO 21600			! trying again?
	IF((OADV(BELL).EQ.WINNER).AND.(OADV(BOOK).EQ.WINNER).AND.
	1	(OADV(CANDL).EQ.WINNER).AND.QON(CANDL)) GO TO 21500
	CALL RSPEAK(62)				! not equipped.
	RETURN
C
21500	CALL RSPEAK(1044)			! must do it the hard way.
	RETURN
C
21600	CALL JIGSUP(61)				! twice, exorcise you.
	RETURN

C RAPPLI, PAGE 7A
C
C R22--	Land of the Living Dead interior
C
22000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(64)				! describe.
	IF(ONPOLF) CALL RSPEAK(65)		! on pole?
	RETURN
C
C R23--	Dam Room
C
23000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(66)				! describe.
	I=67
	IF(LWTIDF) I=68
	CALL RSPEAK(I)				! describe reservoir.
	CALL RSPEAK(69)				! describe panel.
	IF(GATEF) CALL RSPEAK(70)		! bubble is glowing.
	RETURN
C
C R24--	Tree Room
C
24000	IF(PRSA.NE.LOOKW) GO TO 24300		! look?
	CALL RSPEAK(660)			! describe.
	I=661					! set flag for below.
	DO 24200 J=1,OLNT			! describe obj in fore3.
	  IF(.NOT.QHERE(J,FORE3).OR.(J.EQ.FTREE)) GO TO 24200
	  CALL RSPEAK(I)			! set stage,
	  I=0
	  CALL RSPSUB(502,ODESC2(J))		! describe.
24200	CONTINUE
	RETURN
C
24300	IF(PRSA.NE.WALKIW) GO TO 24400		! walk in?
	CFLAG(CEVFOR)=.TRUE.			! start forest noise timer.
	CTICK(CEVFOR)=-1
	RETURN
C
24400	IF((PRSA.NE.DROPW).AND.(PRSA.NE.THROWW).AND.(PRSA.NE.SHAKEW))
	1	RETURN				! drop, throw, shake?
	DO 24600 I=1,OLNT			! loop through objects
	  IF((I.EQ.TTREE).OR.(I.EQ.NEST).OR.
	1	.NOT.QHERE(I,HERE)) GO TO 24600 !  is it here?
	  IF(I.EQ.EGG) GO TO 24500		! egg?
	  CALL NEWSTA(I,0,FORE3,0,0)		! no, drop to forest floor.
	  CALL RSPSUB(659,ODESC2(I))
	  GO TO 24600
C
24500	  CALL NEWSTA(EGG,0,0,0,0)		! vanish egg.
	  CALL NEWSTA(BEGG,658,FORE3,0,0)	! insert broken egg.
	  IF(LASTIT.EQ.EGG) LASTIT=BEGG		! fix last it reference.
	  OTVAL(BEGG)=2
	  IF(OCAN(CANAR).NE.EGG) GO TO 24550	! canary inside?
	  OTVAL(BCANA)=1
	  GO TO 24600
24550	  CALL NEWSTA(BCANA,0,0,0,0)		! no, vanish broken canary.
24600	CONTINUE
	RETURN

C RAPPLI, PAGE 8
C
C R25--	Cyclops Room
C
25000	IF(PRSA.NE.LOOKW) GO TO 25100		! look?
	CALL RSPEAK(606)			! describe.
	I=607					! assume basic state.
	IF(RVCYC.GT.0) I=608			! >0?  hungry.
	IF(RVCYC.LT.0) I=609			! <0?  thirsty.
	IF(CYCLOF) I=610			! asleep?
	IF(MAGICF) I=611			! gone?
	CALL RSPEAK(I)				! describe.
	RETURN
C
25100	IF((PRSA.NE.WALKIW).OR.(RVCYC.EQ.0).OR.DEADF) RETURN
	CFLAG(CEVCYC)=.TRUE.			! walked in, restart timer.
	CTICK(CEVCYC)=-1
	RETURN
C
C R26--	Bank Box Room
C
26000	IF(PRSA.NE.WALKIW) RETURN		! surprise him.
	DO 26100 I=1,8,2			! scolrm depends on
	  IF(FROMDR.EQ.SCOLDR(I)) SCOLRM=SCOLDR(I+1)
26100	CONTINUE				! entry direction.
	RETURN
C
C R27--	Treasure Room
C
27000	IF((PRSA.NE.WALKIW).OR.DEADF.OR..NOT.THFACT)
	1	RETURN				! walkin, thief active?
	IF(OROOM(THIEF).NE.HERE)
	1	CALL NEWSTA(THIEF,82,HERE,0,0)	! no, materialize him.
	THFPOS=HERE				! reset search pattern.
	OFLAG2(THIEF)=OFLAG2(THIEF).OR.FITEBT	! he's angry.
C
C Vanish everything in room.
C
	J=0					! assume nothing to vanish.
	DO 27200 I=1,OLNT
	  IF((I.EQ.CHALI).OR.(I.EQ.THIEF).OR..NOT.QHERE(I,HERE))
	1	GO TO 27200			! here?
	  J=83					! flag byebye.
	  OFLAG1(I)=OFLAG1(I).AND..NOT.VISIBT	! away it goes.
27200	CONTINUE
	CALL RSPEAK(J)				! describe.
	RETURN
C
C R28--	Cliff Room
C
28000	DEFLAF=OADV(RBOAT).NE.WINNER		! true if not carrying.
	RETURN

C RAPPLI, PAGE 9
C
C R29--	Rivr4 Room
C
29000	IF(BUOYF.OR.(OADV(BUOY).NE.WINNER)) RETURN
	CALL RSPEAK(84)				! give hint,
	BUOYF=.TRUE.				! then disable.
	RETURN
C
C R30--	Overfalls
C
30000	IF(PRSA.NE.LOOKW) CALL JIGSUP(85)	! over you go.
	RETURN
C
C R31--	Slide Ledge
C
31000	IF(PRSA.NE.WALKIW) RETURN		! walk in?
	CFLAG(CEVSLI)=.FALSE.			! disable slippery rope.
	RETURN
C
C R32--	Slide
C
32000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(1012)			! describe.
	IF(TTIE.EQ.0) RETURN			! timber tied?
	IF(OROOM(TTIE).EQ.HERE) CALL RSPSUB(1013,ODESC2(TTIE))
	RETURN
C
C R33--	Aragain Falls
C
33000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(96)				! describe.
	I=97					! assume no rainbow.
	IF(RAINBF) I=98				! got one?
	CALL RSPEAK(I)				! describe.
	RETURN

C RAPPLI, PAGE 10
C
C R34--	Ledge Room
C
34000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(100)			! describe.
	I=102					! assume safe room ok.
	IF((RFLAG(MSAFE).AND.RMUNG).NE.0) I=101	! if munged, room gone.
	CALL RSPEAK(I)				! describe.
	RETURN
C
C R35--	Safe Room
C
35000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(104)			! describe.
	I=105					! assume ok.
	IF(SAFEF) I=106				! blown?
	CALL RSPEAK(I)				! describe.
	RETURN
C
C R36--	Magnet Room
C
36000	IF(PRSA.NE.LOOKW) GO TO 36500		! look?
	CALL RSPEAK(107)			! describe.
	RETURN
C
36500	IF((PRSA.NE.WALKIW).OR.DEADF.OR..NOT.CAROFF) RETURN	! walkin?
	IF(CAROZF) GO TO 36600			! zoom?
	IF(WINNER.EQ.PLAYER) CALL RSPEAK(108)	! no, spin his compass.
	RETURN
C
36600	I=58					! spin his insides.
	IF(WINNER.NE.PLAYER) I=99		! spin robot.
	CALL JIGSUP(I)				! dead.
	RETURN
C
C R37--	Cage Room
C
37000	IF(CAGESF) F=MOVETO(CAGER,WINNER)	! if solved, move.
	RETURN

C RAPPLI, PAGE 11
C
C R38--	Mirror D Room
C
38000	IF(PRSA.EQ.LOOKW) CALL LOOKTO(FDOOR,MRG,0,682,681)
	RETURN
C
C R39--	Mirror G Room
C
39000	IF(PRSA.EQ.WALKIW) CALL JIGSUP(685)
	RETURN
C
C R40--	Mirror C Room
C
40000	IF(PRSA.EQ.LOOKW) CALL LOOKTO(MRG,MRB,683,0,681)
	RETURN
C
C R41--	Mirror B Room
C
41000	IF(PRSA.EQ.LOOKW) CALL LOOKTO(MRC,MRA,0,0,681)
	RETURN
C
C R42--	Mirror A Room
C
42000	IF(PRSA.EQ.LOOKW) CALL LOOKTO(MRB,0,0,684,681)
	RETURN

C RAPPLI, PAGE 12
C
C R43--	Mirror C East/West
C
43000	IF(PRSA.EQ.LOOKW) CALL EWTELL(HERE,683)
	RETURN
C
C R44--	Mirror B East/West
C
44000	IF(PRSA.EQ.LOOKW) CALL EWTELL(HERE,686)
	RETURN
C
C R45--	Mirror A East/West
C
45000	IF(PRSA.EQ.LOOKW) CALL EWTELL(HERE,687)
	RETURN
C
C R46--	Inside Mirror
C
46000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(688)			! describe
C
C Now describe pole state.
C
C Cases 1,2--	MDIR=270 & MLOC=MRB, pole is up or in hole
C Cases 3,4--	MDIR=0 V MDIR=180, pole is up or in channel
C Case 5--	Pole is up
C
	I=689					! assume case 5.
	IF((MDIR.EQ.270).AND.(MLOC.EQ.MRB))
	1	I=690+MIN0(POLEUF,1)		! cases 1,2.
	IF(MOD(MDIR,180).EQ.0)
	1	I=692+MIN0(POLEUF,1)		! cases 3,4.
	CALL RSPEAK(I)				! describe pole.
	CALL RSPSUB(694,695+(MDIR/45))		! describe arrow.
	RETURN

C RAPPLI, PAGE 13
C
C R47--	Mirror Eye Room
C
47000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=704					! assume beam stop.
	DO 47100 J=1,OLNT
	  IF(QHERE(J,HERE).AND.(J.NE.RBEAM)) GO TO 47200
47100	CONTINUE
	I=703
47200	CALL RSPSUB(I,ODESC2(J))		! describe beam.
	CALL LOOKTO(MRA,0,0,0,0)		! look north.
	RETURN
C
C R48--	Inside Crypt
C
48000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=46					! crypt is open/closed.
	IF(QOPEN(TOMB)) I=12
	CALL RSPSUB(705,I)
	RETURN
C
C R49--	South Corridor
C
49000	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL RSPEAK(706)			! describe.
	I=46					! odoor is open/closed.
	IF(QOPEN(ODOOR)) I=12
	IF(LCELL.EQ.4) CALL RSPSUB(707,I)	! describe odoor if there.
	RETURN
C
C R50--	Behind Door
C
50000	IF(PRSA.NE.WALKIW) GO TO 50100		! walk in?
	CFLAG(CEVFOL)=.TRUE.			! master follows.
	CTICK(CEVFOL)=-1
	RETURN
C
50100	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=46					! qdoor is open/closed.
	IF(QOPEN(QDOOR)) I=12
	CALL RSPSUB(708,I)
	RETURN

C RAPPLI, PAGE 14
C
C R51--	Front Door
C
51000	IF(PRSA.EQ.WALKIW) CTICK(CEVFOL)=0	! if exits, kill follow.
	IF(PRSA.NE.LOOKW) RETURN		! look?
	CALL LOOKTO(0,MRD,709,0,0)		! describe south.
	I=46					! panel is open/closed.
	IF(CFLAG(CEVINQ).AND.(CTICK(CEVINQ).NE.0)) I=12	! open if inq in prog.
	J=46					! qdoor is open/closed.
	IF(QOPEN(QDOOR)) J=12
	CALL RSPSB2(710,I,J)
	RETURN
C
C R52--	North Corridor
C
52000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=46
	IF(QOPEN(CDOOR)) I=12			! cdoor is open/closed.
	CALL RSPSUB(711,I)
	RETURN
C
C R53--	Parapet
C
53000	IF(PRSA.EQ.LOOKW) CALL RSPSUB(712,712+PNUMB)
	RETURN
C
C R54--	Cell
C
54000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=721					! cdoor is open/closed.
	IF(QOPEN(CDOOR)) I=722
	CALL RSPEAK(I)
	I=46					! odoor is open/closed.
	IF(QOPEN(ODOOR)) I=12
	IF(LCELL.EQ.4) CALL RSPSUB(723,I)	! describe.
	RETURN
C
C R55--	Prison Cell
C
55000	IF(PRSA.EQ.LOOKW) CALL RSPEAK(724)	! look?
	RETURN
C
C R56--	Nirvana Cell
C
56000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=46					! odoor is open/closed.
	IF(QOPEN(ODOOR)) I=12
	CALL RSPSUB(725,I)
	RETURN

C RAPPLI, PAGE 15
C
C R57--	Nirvana and end of game
C
57000	IF(PRSA.NE.WALKIW) RETURN		! walkin?
	CALL RSPEAK(726)
	CALL SCORE(.FALSE.)
	CALL EXIT
C
C R58--	Tomb Room
C
58000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=46					! tomb is open/closed.
	IF(QOPEN(TOMB)) I=12
	CALL RSPSUB(792,I)
	RETURN
C
C R59--	Puzzle Side Room
C
59000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=861					! assume door closed.
	IF(CPOUTF) I=862			! open?
	CALL RSPEAK(I)				! describe.
	RETURN
C
C R60--	Puzzle Room
C
60000	IF(PRSA.NE.LOOKW) RETURN		! look?
	IF(CPUSHF) GO TO 60100			! started puzzle?
	CALL RSPEAK(868)			! no, describe.
	IF((OFLAG2(WARNI).AND.TCHBT).NE.0) CALL RSPEAK(869)
	RETURN
C
60100	CALL CPINFO(880,CPHERE)			! describe room.
	RETURN

C RAPPLI, PAGE 16
C
C R61--	Palantir Room
C
61000	IF(PRSA.NE.LOOKW) GO TO 62400		! look?
	CALL RSPEAK(1015)
	I=699					! string is south.
	GO TO 62100				! join common code.
C
C R62--	Prm Room
C
62000	IF(PRSA.NE.LOOKW) GO TO 62400		! look?
	CALL RSPEAK(1016)			! string is north.
	I=695
C
62100	IF(PLOOKF) GO TO 62400			! thru window? skip door.
	CALL RSPSUB(1017,I)
	I=1018					! assume lid open.
	IF(.NOT.QOPEN(HERE-PRM+PLID1)) I=1019	! closed.
	CALL RSPEAK(I)				! describe lock.
	DO 62200 I=1,OLNT			! loop through objects.
	  IF(OCAN(I).NE.(HERE-PRM+PKH1)) GO TO 62200
	  CALL RSPSUB(1020,ODESC2(I))		! object in keyhole.
	  GO TO 62300
62200	CONTINUE
C
62300	IF(QOPEN(PDOOR)) CALL RSPEAK(1042)	! door open?
	IF(.NOT.MATF) GO TO 62400		! mat under door?
	CALL RSPEAK(1021)
	IF((MATOBJ.NE.0).AND.((HERE.EQ.PALRM).OR.QOPEN(PDOOR)))
	1	CALL RSPSUB(1022,ODESC2(MATOBJ))	! obj on mat?
	GO TO 62400
C
62400	PLOOKF=.FALSE.				! clear window flag.
	IF(PRSO.EQ.0) RETURN			! any object?
	IF((PRSA.NE.TAKEW).OR..NOT.QEMPTY(HERE-PRM+PKH1).OR.
	1  ((PRSO.NE.SCREW).AND.(PRSO.NE.STICK).AND.
	2   (PRSO.NE.PKEY).AND.(PRSO.NE.KEYS))) GO TO 62500
	IF(.NOT.PTOUCF) GO TO 62450		! touched?
	IF(QOPEN(HERE-PRM+PLID1)) CALL RSPEAK(1043)   	! lid closes.
	OFLAG2(HERE-PRM+PLID1)=OFLAG2(HERE-PRM+PLID1).AND..NOT.OPENBT
62450	PTOUCF=.TRUE.				! touched now.
C
62500	OFLAG1(SCREW)=OFLAG1(SCREW).AND..NOT.NDSCBT
	IF((OCAN(SCREW).EQ.PKH1).OR.(OCAN(SCREW).EQ.PKH2))
	1	OFLAG1(SCREW)=OFLAG1(SCREW).OR.NDSCBT
	OFLAG1(STICK)=OFLAG1(STICK).AND..NOT.NDSCBT
	IF((OCAN(STICK).EQ.PKH1).OR.(OCAN(STICK).EQ.PKH2))
	1	OFLAG1(STICK)=OFLAG1(STICK).OR.NDSCBT
	OFLAG1(PKEY)=OFLAG1(PKEY).AND..NOT.NDSCBT
	IF((OCAN(PKEY).EQ.PKH1).OR.(OCAN(PKEY).EQ.PKH2))
	1	OFLAG1(PKEY)=OFLAG1(PKEY).OR.NDSCBT
	OFLAG1(KEYS)=OFLAG1(KEYS).AND..NOT.NDSCBT
	IF((OCAN(KEYS).EQ.PKH1).OR.(OCAN(KEYS).EQ.PKH2))
	1	OFLAG1(KEYS)=OFLAG1(KEYS).OR.NDSCBT
	IF((OROOM(MAT).NE.PRM).AND.(OROOM(MAT).NE.PALRM)) MATF=.FALSE.
	OFLAG1(MAT)=OFLAG1(MAT).AND..NOT.NDSCBT
	IF(.NOT.MATF) RETURN
	OFLAG1(MAT)=OFLAG1(MAT).OR.NDSCBT
	CALL NEWSTA(MAT,0,HERE,0,0)
	RETURN

C RAPPLI, PAGE 17
C
C R63--	Inslide
C
63000	DO 63100 I=1,OLNT			! loop through objects
	  IF(.NOT.QHERE(I,HERE).OR.
	1	((OFLAG1(I).AND.TAKEBT).EQ.0)) GO TO 63100
	  CALL NEWSTA(I,0,CELLA,0,0)		! drop to cellar,
	  IF(I.EQ.WATER) CALL NEWSTA(I,0,0,0,0)	! unless water
	  CALL RSPSUB(1011,ODESC2(I))
63100	CONTINUE
	RETURN
C
C R64--	Puzzle Anteroom
C
64000	IF(PRSA.NE.LOOKW) RETURN		! look?
	I=1068					! not blocked.
	IF(CPVEC(10).NE.0) I=1069		! blocked.
	CALL RSPEAK(I)				! describe.
	RETURN
C
	END

C LOOKTO--	Describe view in mirror hallway
C
C Declarations
C
	SUBROUTINE LOOKTO(NRM,SRM,NT,ST,HT)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
C
	CALL RSPEAK(HT)				! describe hall.
	CALL RSPEAK(NT)				! describe north view.
	CALL RSPEAK(ST)				! describe south view.
	DIR=0					! assume no direction.
	IF(IABS(MLOC-HERE).NE.1) GO TO 200	! mirror to n or s?
	IF(MLOC.EQ.NRM) DIR=695
	IF(MLOC.EQ.SRM) DIR=699			! dir=n/s.
	IF(MOD(MDIR,180).NE.0) GO TO 100	! mirror n-s?
	CALL RSPSUB(847,DIR)			! yes, he sees panel
	CALL RSPSB2(848,DIR,DIR)		! and narrow rooms.
	GO TO 200
C
100	M1=MRHERE(HERE)				! which mirror?
	MRBF=0					! assume intact.
	IF(((M1.EQ.1).AND..NOT.MR1F).OR.
	1  ((M1.EQ.2).AND..NOT.MR2F)) MRBF=1	! broken?
	CALL RSPSUB(849+MRBF,DIR)		! describe.
	IF((M1.EQ.1).AND.MROPNF) CALL RSPEAK(823+MRBF)
	IF(MRBF.NE.0) CALL RSPEAK(851)
C
200	I=0					! assume no more to do.
	IF((NT.EQ.0).AND.((DIR.EQ.0).OR.(DIR.EQ.699))) I=852
	IF((ST.EQ.0).AND.((DIR.EQ.0).OR.(DIR.EQ.695))) I=853
	IF((NT+ST+DIR).EQ.0) I=854
	IF(HT.NE.0) CALL RSPEAK(I)		! describe halls.
	RETURN
C
	END

C EWTELL--	Describe e/w narrow rooms
C
C Declarations
C
	SUBROUTINE EWTELL(RM,ST)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL M1
C
C Note that we are east or west of mirror, and
C mirror must be n-s.
C
	M1=(MDIR+(MOD(RM-MRAE,2)*180)).EQ.180
	I=MOD(RM-MRAE,2)			! get basic e/w flag.
	IF((M1.AND..NOT.MR1F).OR.(.NOT.M1.AND..NOT.MR2F))
	1	I=I+2				! mirror broken?
	CALL RSPEAK(819+I)
	IF(M1.AND.MROPNF) CALL RSPEAK(823+(I/2))
	CALL RSPEAK(825)
	CALL RSPEAK(ST)
	RETURN
C
	END
