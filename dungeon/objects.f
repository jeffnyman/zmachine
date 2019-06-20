C Object processors for DUNGEON
C
C COPYRIGHT 1980, 1990, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA.
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK
C
C 20-Oct-94	RMS	Fixed bugs in water, black book, me, leak.
C 29-Sep-94	RMS	Fixed bugs in KILL MASTER, palantir, dial button,
C			well, slide, bat, global brochure, granite wall,
C			bottle, leaves, broken lamp, beam, robot, thief,
C			troll, me, subscripting, object substitution.
C			Added features to heads, coke bottles, balloon,
C			bucket, stove.
C 30-Jan-94	RMS	Fixed bugs from MS-DOS port.
C 25-Jan-94	RMS	Added ground = sand at sandy beach.
C 30-Jun-92	RMS	Changed file names to lower case.
C
C OAPPLI- Object action routines
C
C Declarations
C
	LOGICAL FUNCTION OAPPLI(RI,ARG)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL SOBJS,NOBJS
	LOGICAL QOPEN,QON,LIT,WASLIT
	LOGICAL MOVETO,RMDESC,CLOCKD
	LOGICAL THIEFP,CYCLOP,TROLLP,BALLOP
	LOGICAL QEMPTY,F,OPNCLS
	PARAMETER (MXSMP=99)
C
C Functions and data
C
	QOPEN(R)=(OFLAG2(R).AND.OPENBT).NE.0
	QON(R)=(OFLAG1(R).AND.ONBT).NE.0
C
	IF(RI.EQ.0) GO TO 10			! zero is false app.
	IF(RI.LE.MXSMP) GO TO 100		! simple object?
	ODO2=0
	ODI2=0
	IF((PRSO.NE.0).AND.(PRSO.LE.OMAX)) ODO2=ODESC2(PRSO)
	IF(PRSI.NE.0) ODI2=ODESC2(PRSI)
	AV=AVEHIC(WINNER)
	FLOBTS=FLAMBT+LITEBT+ONBT
	OAPPLI=.TRUE.
	WASLIT=LIT(HERE)
C
	GO TO (2000,5000,10000,11000,12000,15000,18000,
	1 19000,20000,22000,25000,26000,32000,35000,39000,40000,
	2 45000,47000,48000,49000,50000,51000,52000,54000,55000,
	3 56000,57000,58000,59000,60000,61000,62000),
	3	(RI-MXSMP)
	CALL BUG(6,RI)
C
C Return here to declare false result.
C
10	OAPPLI=.FALSE.
	RETURN
C
C Return here to test for light source change.
C
50	IF(WASLIT.AND..NOT.LIT(HERE)) CALL RSPEAK(406)
	RETURN
C
C Simple objects, processed externally.
C
100	IF(RI.LT.32) OAPPLI=SOBJS(RI,ARG)
	IF(RI.GE.32) OAPPLI=NOBJS(RI,ARG)
	RETURN

C OAPPLI, PAGE 3
C
C O100--	Machine
C
2000	IF(HERE.NE.MMACH) GO TO 10		! not here? f
	OAPPLI=OPNCLS(MACHI,123,124)		! handle opn/cls.
	RETURN
C
C O101--	Water
C
5000	IF(PRSA.NE.GTHROW) GO TO 5025		! go through?
	CALL RSPEAK(331+RND(3))			! joke.
	RETURN
C
5025	IF((PRSO.EQ.WATER).OR.(PRSO.EQ.GWATE)) GO TO 5100
	CALL RSPEAK(561)			! water is ind obj,
	RETURN					! punt.
C
5100	IF(PRSA.NE.TAKEW) GO TO 5400		! take water?
	IF(PRSI.NE.0) GO TO 5200		! from x?
	IF((OADV(BOTTL).EQ.WINNER).AND.(OCAN(PRSO).NE.BOTTL))
	1	GO TO 5500			! take, have bottle -> put.
	IF(OCAN(PRSO).NE.BOTTL) GO TO 5150	! water in bottle?
	IF(OADV(BOTTL).NE.WINNER) GO TO 5125	! already have bottle?
	CALL RSPEAK(103)			! yes, already have water.
	RETURN
C
5125	PRSO=BOTTL				! take bottle.
	GO TO 10				! do normal take.
C
5150	IF(OCAN(PRSO).EQ.0) GO TO 5300		! if not inside, take.
	PRSI=OCAN(PRSO)				! make into take from.
	GO TO 5250				! check for open.
C
5200	IF(OCAN(PRSO).NE.PRSI) GO TO 5250	! is it inside?
	CALL RSPEAK(1038)			! not in that.
	RETURN
C
5250	IF(QOPEN(PRSI)) GO TO 5300		! is it open?
	CALL RSPSUB(525,ODI2)			! no, not open.
	RETURN
C
5300	IF(AV.NE.0) GO TO 5800			! if in vehicle, put there.
	I=615					! assume player.
	IF(WINNER.NE.PLAYER) I=1081
	CALL RSPEAK(I)				! slips thru fingers.
	RETURN
C
5400	IF(PRSA.NE.PUTW) GO TO 5700		! put water in x?
	IF(PRSI.EQ.BOTTL) GO TO 5500		! in bottle?
	IF(((OFLAG2(PRSI).AND.VEHBT).NE.0).OR.
	1  ((AV.NE.0).AND.(PRSI.EQ.AV))) GO TO 5450	! in veh?
	CALL RSPSUB(297,ODI2)			! wont go elsewhere.
	CALL NEWSTA(PRSO,0,0,0,0)		! vanish water.
	RETURN
C
5450	CALL NEWSTA(WATER,0,0,PRSI,0)		! water into vehicle.
	CALL RSPSUB(296,ODESC2(PRSI))		! describe.
	RETURN
C
5500	IF(QOPEN(BOTTL)) GO TO 5550		! bottle open?
	CALL RSPEAK(612)			! no, lose.
	RETURN
C
5550	IF(QEMPTY(BOTTL)) GO TO 5600		! open, empty?
	CALL RSPEAK(613)			! no, already full.
	RETURN
C
5600	CALL NEWSTA(WATER,614,0,BOTTL,0)	! take water to bottle.
	RETURN
C
5700	IF((PRSA.NE.DROPW).AND.(PRSA.NE.POURW))
	1	GO TO 5900			! drop, pour?
	IF(AV.NE.0) GO TO 5800			! into vehicle?
	CALL NEWSTA(PRSO,133,0,0,0)		! no, vanishes.
	RETURN
C
5800	CALL NEWSTA(WATER,0,0,AV,0)		! water into his vehicle.
	CALL RSPSUB(296,ODESC2(AV))		! describe.
	RETURN
C
5900	IF(PRSA.NE.THROWW) GO TO 10		! last chance, throw?
	CALL NEWSTA(PRSO,132,0,0,0)		! vanishes.
	RETURN

C OAPPLI, PAGE 4
C
C O102--	Leaf pile
C
10000	IF(PRSA.NE.BURNW) GO TO 10500		! burn?
	IF(QOPEN(GRATE).OR.(RVCLR.NE.0)) GO TO 10050
	RVCLR=1					! leaves moved.
	CALL NEWSTA(GRATE,30,HERE,0,0)		! grating appears.
10050	IF(OADV(PRSO).EQ.PLAYER) GO TO 10100	! was he carrying?
	CALL NEWSTA(LEAVE,158,0,0,0)		! no, burn leaves.
	RETURN
C
10100	CALL NEWSTA(LEAVE,0,HERE,0,0)		! drop leaves.
	IF(HERE.EQ.MTREE) CALL NEWSTA(LEAVE,0,FORE3,0,0)
	CALL JIGSUP(159)			! burn him.
	RETURN
C
10500	IF(PRSA.NE.MOVEW) GO TO 10600		! move?
	CALL RSPEAK(2)				! done.
10550	IF(QOPEN(GRATE).OR.(RVCLR.NE.0)) RETURN	! done before?
	RVCLR=1					! leaves moved.
	CALL NEWSTA(GRATE,30,HERE,0,0)		! grating appears.
	RETURN
C
10600	IF(PRSA.NE.TAKEW) GO TO 10700		! take?
	OAPPLI=.FALSE.				! don't handle here.
	GO TO 10550				! make grate visible.
C
10700	IF((PRSA.NE.LOOKUW).OR.QOPEN(GRATE).OR.(RVCLR.NE.0)) GO TO 10
	CALL RSPEAK(344)			! look under?
	RETURN
C
C O103--	Troll, done externally.
C
11000	OAPPLI=TROLLP(ARG)			! troll processor.
	GO TO 50				! go see if now dark.
C
C O104--	Rusty knife.
C
12000	IF(PRSA.NE.TAKEW) GO TO 12100		! take?
	IF(OADV(SWORD).EQ.WINNER) CALL RSPEAK(160) ! pulse sword.
	GO TO 10
C
12100	IF((((PRSA.NE.ATTACW).AND.(PRSA.NE.KILLW)).OR.
	1	(PRSI.NE.RKNIF)).AND.
	2  (((PRSA.NE.SWINGW).AND.(PRSA.NE.THROWW)).OR.
	3	(PRSO.NE.RKNIF))) GO TO 10
	CALL NEWSTA(RKNIF,0,0,0,0)		! kill knife.
	CALL JIGSUP(161)			! kill him.
	RETURN

C OAPPLI, PAGE 5
C
C O105--	Glacier
C
15000	IF(PRSA.NE.THROWW) GO TO 15500		! throw?
	IF(PRSO.NE.TORCH) GO TO 15400		! torch?
	CALL NEWSTA(ICE,169,0,0,0)		! melt ice.
	ODESC1(TORCH)=174			! mung torch.
	ODESC2(TORCH)=173
	OFLAG1(TORCH)=OFLAG1(TORCH).AND. .NOT.FLOBTS
	CALL NEWSTA(TORCH,0,STREA,0,0)		! move torch.
	GLACRF=.TRUE.				! glacier gone.
	IF(.NOT.LIT(HERE)) CALL RSPEAK(170)	! in dark?
	RETURN
C
15400	CALL RSPEAK(171)			! joke if not torch.
	GO TO 10				! don't handle.
C
15500	IF((PRSA.NE.MELTW).OR.(PRSO.NE.ICE)) GO TO 10
	IF((OFLAG1(PRSI).AND.FLOBTS).EQ.FLOBTS) GO TO 15600
	CALL RSPSUB(298,ODI2)			! cant melt with that.
	RETURN
C
15600	GLACMF=.TRUE.				! partial melt.
	IF(PRSI.NE.TORCH) GO TO 15700		! melt with torch?
	ODESC1(TORCH)=174			! mung torch.
	ODESC2(TORCH)=173
	OFLAG1(TORCH)=OFLAG1(TORCH).AND. .NOT.FLOBTS
15700	CALL JIGSUP(172)			! drown.
	RETURN
C
C O106--	Black book
C
18000	IF((PRSA.NE.OPENW).OR.(PRSO.NE.BOOK)) GO TO 18100	! open book?
	CALL RSPEAK(180)			! joke.
	RETURN
C
18100	IF(PRSA.NE.CLOSEW) GO TO 18200		! close?
	CALL RSPEAK(181)
	RETURN
C
18200	IF(PRSA.NE.BURNW) GO TO 10		! burn?
	CALL NEWSTA(BOOK,0,0,0,0)		! vanish book.
	CALL JIGSUP(182)			! fatal joke.
	RETURN

C OAPPLI, PAGE 6
C
C O107--	Candles
C
19000	IF(ORCAND.NE.0) GO TO 19100		! first ref?
	ORCAND=1				! yes, candles are
	CFLAG(CEVCND)=.TRUE.
	CTICK(CEVCND)=50			! burning when seen.
C
19100	IF(PRSI.EQ.CANDL) GO TO 10		! ignore ind refs.
	IF(PRSA.NE.TRNOFW) GO TO 19200		! turn off?
	I=513					! assume off.
	IF(QON(CANDL)) I=514			! if on, different.
	CFLAG(CEVCND)=.FALSE.			! disable countdown.
	OFLAG1(CANDL)=OFLAG1(CANDL).AND. .NOT.ONBT
	CALL RSPEAK(I)
	GO TO 50				! go see if now dark.
C
19200	IF((PRSA.NE.BURNW).AND.(PRSA.NE.TRNONW)) GO TO 10
	IF((OFLAG1(CANDL).AND.LITEBT).NE.0) GO TO 19300
	CALL RSPEAK(515)			! candles too short.
	RETURN
C
19300	IF(PRSI.NE.0) GO TO 19400		! any flame?
	CALL RSPEAK(516)			! no, lose.
	CALL ORPHAN(-1,ACT,PREP1,CANDL,2,' ',0,0) ! orphan "light candle with"
	PRSWON=.FALSE.
	PRSCON=0
	RETURN
C
19400	IF((PRSI.NE.MATCH).OR. .NOT.QON(MATCH)) GO TO 19500
	I=517					! assume off.
	IF(QON(CANDL)) I=518			! if on, joke.
	OFLAG1(CANDL)=OFLAG1(CANDL).OR.ONBT	! lite candles.
	CFLAG(CEVCND)=.TRUE.			! resume countdown.
	CALL RSPEAK(I)
	RETURN
C
19500	IF((PRSI.NE.TORCH).OR. .NOT.QON(TORCH)) GO TO 19600
	IF(QON(CANDL)) GO TO 19700		! already on?
	CALL NEWSTA(CANDL,521,0,0,0)		! no, vaporize.
	RETURN
C
19600	CALL RSPEAK(519)			! cant light with that.
	RETURN
C
19700	CALL RSPEAK(520)			! already on.
	RETURN
C
C O108--	Matches
C
20000	IF((PRSA.NE.TRNONW).OR.(PRSO.NE.MATCH)) GO TO 20500
	IF(ORMTCH.NE.0) GO TO 20100		! any matches left?
	CALL RSPEAK(183)			! no, lose.
	RETURN
C
20100	ORMTCH=ORMTCH-1				! decrement no matches.
	OFLAG1(MATCH)=OFLAG1(MATCH).OR.FLOBTS
	CFLAG(CEVMAT)=.TRUE.
	CTICK(CEVMAT)=2				! countdown.
	CALL RSPEAK(184)
	RETURN
C
20500	IF((PRSA.NE.TRNOFW).OR.((OFLAG1(MATCH).AND.ONBT).EQ.0))
	1	GO TO 10			! extinguish?
	OFLAG1(MATCH)=OFLAG1(MATCH).AND. .NOT.FLOBTS
	CTICK(CEVMAT)=0
	CALL RSPEAK(185)
	GO TO 50				! go see if now dark.
C
C O109--	Cyclops, processed externally.
C
22000	OAPPLI=CYCLOP(ARG)			! cyclops
	GO TO 50				! go see if now dark.
C
C O110--	Thief, processed externally.
C
25000	OAPPLI=THIEFP(ARG)
	GO TO 50				! go see if now dark.
C
C O111--	Window
C
26000	OAPPLI=OPNCLS(WINDO,208,209)		! open/cls window.
	RETURN
C
C O112--	Pile of bodies
C
32000	IF(PRSA.NE.TAKEW) GO TO 32500		! take?
	CALL RSPEAK(228)			! cant.
	RETURN
C
32500	IF((PRSA.NE.BURNW).AND.(PRSA.NE.MUNGW)) GO TO 10
	IF(ONPOLF) RETURN			! burn or mung?
	ONPOLF=.TRUE.				! set head on pole.
	CALL NEWSTA(HPOLE,0,LLD2,0,0)
	CALL JIGSUP(229)			! beheaded.
	RETURN
C
C O113--	Vampire bat
C
35000	CALL RSPEAK(50)				! time to fly, jack.
	F=MOVETO(BATDRP(RND(9)+1),WINNER)	! select random dest.
	F=RMDESC(0)
	PRSCON=0				! disable parser.
	RETURN

C OAPPLI, PAGE 7
C
C O114--	Stick
C
39000	IF(PRSA.NE.WAVEW) GO TO 10		! wave?
	IF(HERE.EQ.MRAIN) GO TO 39500		! on rainbow?
	IF((HERE.EQ.POG).OR.(HERE.EQ.FALLS)) GO TO 39200
	CALL RSPEAK(244)			! nothing happens.
	RETURN
C
39200	OFLAG1(POT)=OFLAG1(POT).OR.VISIBT	! make gold visible.
	RAINBF=.NOT. RAINBF			! complement rainbow.
	I=245					! assume off.
	IF(RAINBF) I=246			! if on, solid.
	CALL RSPEAK(I)				! describe.
	RETURN
C
39500	RAINBF=.FALSE.				! on rainbow,
	CALL JIGSUP(247)			! take a fall.
	RETURN
C
C O115--	Balloon, handled externally.
C
40000	OAPPLI=BALLOP(ARG)
	RETURN
C
C O116--	Heads
C
45000	IF(PRSA.NE.HELLOW) GO TO 45100		! hello heads?
	CALL RSPEAK(633)			! truly bizarre.
	RETURN
C
45100	IF((PRSA.NE.KILLW).AND.(PRSA.NE.MUNGW).AND.
	1  (PRSA.NE.RUBW).AND.(PRSA.NE.OPENW).AND.
	2  (PRSA.NE.TAKEW).AND.(PRSA.NE.BURNW).AND.(PRSA.NE.SPINW).AND.
	3  (PRSA.NE.ATTACW).AND.(PRSA.NE.KICKW)) GO TO 10
	CALL RSPEAK(260)			! bad news for player
	I=ROBADV(WINNER,0,LCASE,0)+ROBRM(HERE,100,0,LCASE,0)
	IF(I.NE.0) CALL NEWSTA(LCASE,0,LROOM,0,0) ! if robbed, make large case.
	CALL JIGSUP(261)			! kill him.
	RETURN

C OAPPLI, PAGE 8
C
C O117--	Sphere
C
47000	IF(CAGESF.OR.(PRSA.NE.TAKEW)) GO TO 47600 ! take?
	IF(WINNER.NE.PLAYER) GO TO 47500	! robot take?
	CALL RSPEAK(263)			! no, drop cage.
	IF(OROOM(ROBOT).NE.HERE) GO TO 47200	! robot here?
	F=MOVETO(CAGED,WINNER)			! yes, move into cage.
	CALL NEWSTA(ROBOT,0,CAGED,0,0)		! move robot.
	AROOM(AROBOT)=CAGED
	OFLAG1(ROBOT)=OFLAG1(ROBOT).OR.NDSCBT	! don't describe robot.
	CFLAG(CEVSPH)=.TRUE.
	CTICK(CEVSPH)=10			! get out in 10 or else.
	RETURN
C
47200	CALL NEWSTA(SPHER,0,0,0,0)		! you're dead.
	RFLAG(CAGER)=RFLAG(CAGER).OR.RMUNG	! mung cage room.
	RDESC1(CAGER)=147
	CALL JIGSUP(148)			! mung player.
	RETURN
C
47500	CALL NEWSTA(SPHER,0,0,0,0)		! robot tried,
	CALL NEWSTA(ROBOT,264,0,0,0)		! kill him.
	CALL NEWSTA(CAGE,0,HERE,0,0)		! insert mangled cage.
	GO TO 50				! go see if now dark.
C
47600	IF(PRSA.NE.LOOKIW) GO TO 10		! look in?
	OAPPLI=NOBJS(OACTIO(PALAN),ARG)		! do palantir function.
	RETURN
C
C O118--	Geometrical buttons
C
48000	IF(PRSA.NE.PUSHW) GO TO 10		! push?
	I=PRSO-SQBUT+1				! get button index.
	IF((I.LE.0).OR.(I.GE.4)) GO TO 10	! a button?
	IF(WINNER.NE.PLAYER) GO TO (48100,48200,48300),I
	CALL JIGSUP(265)			! you pushed, you die.
	RETURN
C
48100	I=267					! square, speed up.
	IF(CAROZF) I=266
	CAROZF=.TRUE.
	CALL RSPEAK(I)
	RETURN
C
48200	I=266					! round, slow down.
	IF(CAROZF) I=268
	CAROZF=.FALSE.
	CALL RSPEAK(I)
	RETURN
C
48300	CAROFF=.NOT.CAROFF			! triangle, flip carousel.
	IF(OROOM(IRBOX).NE.CAROU) GO TO 48400	! iron box in carousel?
	CALL RSPEAK(269)			! yes, thump.
	OFLAG1(IRBOX)=OFLAG1(IRBOX).XOR.VISIBT	! complement visibility.
	IF(CAROFF) RFLAG(CAROU)=RFLAG(CAROU).AND. .NOT.RSEEN
	RETURN
C
48400	CALL RSPEAK(232)			! click.
	RETURN
C
C O119--	Flask function
C
49000	IF(PRSA.EQ.OPENW) GO TO 49100		! open?
	IF((PRSA.NE.MUNGW).AND.(PRSA.NE.THROWW)) GO TO 10
	CALL NEWSTA(FLASK,270,0,0,0)		! kill flask.
49100	RFLAG(HERE)=RFLAG(HERE).OR.RMUNG	! mung room.
	RDESC1(HERE)=271
	CALL JIGSUP(272)			! poisoned.
	RETURN
C
C O120--	Bucket function
C
50000	IF(ARG.EQ.1) GO TO 10			! read in?
	IF(ARG.EQ.2) GO TO 50400		! read out?
	IF(PRSA.NE.BURNW) GO TO 50100		! burn?
	CALL RSPEAK(928)			! can't.
	RETURN
C
50100	IF(PRSA.NE.KICKW) GO TO 10		! kick?
	CALL JIGSUP(1067)			! dead.
	RETURN
C
50400	IF((OCAN(WATER).NE.BUCKE).OR.BUCKTF) GO TO 50500
	BUCKTF=.TRUE.				! bucket at top.
	CFLAG(CEVBUC)=.TRUE.
	CTICK(CEVBUC)=100			! start countdown.
	CALL NEWSTA(BUCKE,290,TWELL,0,0)	! reposition bucket.
	GO TO 50900				! finish up.
C
50500	IF((OCAN(WATER).EQ.BUCKE).OR..NOT.BUCKTF) GO TO 10
	BUCKTF=.FALSE.
	CALL NEWSTA(BUCKE,291,BWELL,0,0)	! bucket at bottom.
50900	IF(AV.NE.BUCKE) RETURN			! not in bucket?
	F=MOVETO(OROOM(BUCKE),WINNER)		! move adventurer.
	F=RMDESC(0)				! describe room.
	RETURN

C OAPPLI, PAGE 9
C
C O121--	Eatme cake
C
51000	IF((PRSA.NE.EATW).OR.(PRSO.NE.ECAKE).OR.
	1	(HERE.NE.ALICE)) GO TO 10	! eat cake in aliceroom?
	CALL NEWSTA(ECAKE,273,0,0,0)		! vanish cake.
	OFLAG1(ROBOT)=OFLAG1(ROBOT).AND. .NOT.VISIBT	! vanish robot.
	DO 51100 I=1,OLNT			! make objects big.
	  IF((OROOM(I).NE.ALICE).OR.(OSIZE(I).EQ.10000))
	1	GO TO 51100
	  OSIZE(I)=OSIZE(I)*64
	  OROOM(I)=ALISM
51100	CONTINUE
	OAPPLI=MOVETO(ALISM,WINNER)		! move to alice small.
	RETURN
C
C O122--	Icings
C
52000	IF(PRSA.NE.READW) GO TO 52200		! read?
	I=274					! cant read.
	IF(PRSI.NE.0) I=275			! through something?
	IF(PRSI.EQ.BOTTL) I=276			! through bottle?
	IF(PRSI.EQ.FLASK) I=277+(PRSO-ORICE)	! through flask?
	CALL RSPEAK(I)				! read flask.
	RETURN
C
52200	IF((PRSA.NE.THROWW).OR.(PRSO.NE.RDICE).OR.(PRSI.NE.POOL))
	1	GO TO 52300			! throw rdice at pool?
	CALL NEWSTA(POOL,280,0,0,0)		! vanish pool.
	OFLAG1(SAFFR)=OFLAG1(SAFFR).OR.VISIBT	! materialize spices.
	RETURN
C
52300	IF((HERE.NE.ALICE).AND.(HERE.NE.ALISM).AND.(HERE.NE.ALITR))
	1	GO TO 10			! in wonderland?
	IF(((PRSA.NE.EATW).AND.(PRSA.NE.THROWW)).OR.
	1	(PRSO.NE.ORICE)) GO TO 52400	! throw orange ice?
	CALL NEWSTA(ORICE,0,0,0,0)		! vanish orange ice.
	RFLAG(HERE)=RFLAG(HERE).OR.RMUNG	! vanish room.
	RDESC1(HERE)=281
	CALL JIGSUP(282)			! vanish adventurer.
	RETURN
C
52400	IF((PRSA.NE.EATW).OR.(PRSO.NE.BLICE))
	1	GO TO 10			! eat blue ice?
	CALL NEWSTA(BLICE,283,0,0,0)		! vanish blue ice.
	IF(HERE.NE.ALISM) GO TO 52500		! in reduced room?
	OFLAG1(ROBOT)=OFLAG1(ROBOT).OR.VISIBT	! materialize robot.
	DO 52450 I=1,OLNT			! enlarge world.
	  IF((OROOM(I).NE.HERE).OR.(OSIZE(I).EQ.10000))
	1	GO TO 52450
	  OROOM(I)=ALICE
	  OSIZE(I)=OSIZE(I)/64
52450	CONTINUE
	OAPPLI=MOVETO(ALICE,WINNER)		! return
	RETURN
C
52500	CALL JIGSUP(284)			! enlarged in wrong room.
	RETURN
C
C O123--	Brick
C
54000	IF(PRSA.NE.BURNW) GO TO 10		! burn?
	CALL NEWSTA(BRICK,0,0,0,0)		! vanish brick.
	CALL JIGSUP(150)			! boom!
	RETURN
C
C O124--	Myself
C
55000	IF((PRSA.NE.GIVEW).OR.
	1 ((OFLAG2(PRSO).AND.NOCHBT).NE.0)) GO TO 55100	! give?
	IF(PRSO.NE.WATER) GO TO 55050		! water?
	CALL NEWSTA(WATER,615,0,0,0)		! slips through fingers.
	RETURN
C
55050	CALL NEWSTA(PRSO,2,0,0,PLAYER)		! done.
	RETURN
C
55100	IF(PRSA.NE.TAKEW) GO TO 55200		! take?
	CALL RSPEAK(286)			! joke.
	RETURN
C
55200	IF(((PRSA.NE.KILLW).AND.(PRSA.NE.MUNGW))
	1	.OR.(PRSO.NE.OPLAY)) GO TO 10
	WINNER=PLAYER				! can't kill someone else.
	CALL JIGSUP(287)			! kill, no joke.
	RETURN

C OAPPLI, PAGE 10
C
C O125--	Panels inside mirror
C
56000	IF(PRSA.NE.PUSHW) GO TO 10		! push?
	IF(POLEUF.NE.0) GO TO 56100		! short pole up?
	I=731					! no, wont budge.
	IF(MOD(MDIR,180).EQ.0) I=732		! diff msg if n-s.
	CALL RSPEAK(I)				! tell wont move.
	RETURN
C
56100	IF(MLOC.NE.MRG) GO TO 56200		! in gdn room?
	CALL RSPEAK(733)			! you lose.
	CALL JIGSUP(685)
	RETURN
C
56200	I=831					! rotate l or r.
	IF((PRSO.EQ.RDWAL).OR.(PRSO.EQ.YLWAL)) I=830
	CALL RSPEAK(I)				! tell direction.
	MDIR=MOD(MDIR+45+(270*(I-830)),360)	! calculate new dir.
	CALL RSPSUB(734,695+(MDIR/45))		! tell new dir.
	IF(WDOPNF) CALL RSPEAK(730)		! if panel open, close.
	WDOPNF=.FALSE.
	RETURN					! done.
C
C O126--	Ends inside mirror
C
57000	IF(PRSA.NE.PUSHW) GO TO 10		! push?
	IF(MOD(MDIR,180).EQ.0) GO TO 57100	! mirror n-s?
	CALL RSPEAK(735)			! no, wont budge.
	RETURN
C
57100	IF(PRSO.NE.PINDR) GO TO 57300		! push pine wall?
	IF(((MLOC.EQ.MRC).AND.(MDIR.EQ.180)).OR.
	1  ((MLOC.EQ.MRD).AND.(MDIR.EQ.0)).OR.
	2   (MLOC.EQ.MRG)) GO TO 57200		! in view of gdn?
	CALL RSPEAK(736)			! no, opens.
	WDOPNF=.TRUE.				! indicate open.
	CFLAG(CEVPIN)=.TRUE.			! time opening.
	CTICK(CEVPIN)=5
	RETURN
C
57200	CALL RSPEAK(737)			! gdn sees you, die.
	CALL JIGSUP(685)
	RETURN
C
57300	NLOC=MLOC-1				! new loc if south.
	IF(MDIR.EQ.0) NLOC=MLOC+1		! new loc if north.
	IF((NLOC.GE.MRA).AND.(NLOC.LE.MRD)) GO TO 57400
	CALL RSPEAK(738)			! have reached end.
	RETURN
C
57400	I=699					! assume south.
	IF(MDIR.EQ.0) I=695			! north.
	J=739					! assume smooth.
	IF(POLEUF.NE.0) J=740			! pole up, wobbles.
	CALL RSPSUB(J,I)			! describe.
	MLOC=NLOC
	IF(MLOC.NE.MRG) RETURN			! now in gdn room?
C
	IF(POLEUF.NE.0) GO TO 57500		! pole up, gdn sees.
	IF(MROPNF.OR.WDOPNF) GO TO 57600	! door open, gdn sees.
	IF(MR1F.AND.MR2F) RETURN		! mirrors intact, ok.
	CALL RSPEAK(742)			! mirrors broken, die.
	CALL JIGSUP(743)
	RETURN
C
57500	CALL RSPEAK(741)			! pole up, die.
	CALL JIGSUP(743)
	RETURN
C
57600	CALL RSPEAK(744)			! door open, die.
	CALL JIGSUP(743)
	RETURN

C OAPPLI, PAGE 11
C
C O127--	Global guardians
C
58000	IF((PRSA.NE.ATTACW).AND.(PRSA.NE.KILLW).AND.
	1  (PRSA.NE.MUNGW)) GO TO 58100		! aggressive?
	CALL JIGSUP(745)			! lose.
	RETURN
C
58100	IF(PRSA.NE.HELLOW) GO TO 10		! hello?
	CALL RSPEAK(746)			! no reply.
	RETURN
C
C O128--	Global master
C
59000	IF(((PRSA.NE.ATTACW).AND.(PRSA.NE.KILLW).AND.(PRSA.NE.MUNGW))
	1	.OR.(PRSO.NE.MASTER).OR.(PRSI.EQ.MASTER))
	2	GO TO 59100			! kill master?
	WINNER=PLAYER				! rebounds on player.
	CALL JIGSUP(747)			! bad idea.
	RETURN
C
59100	IF(PRSA.NE.TAKEW) GO TO 10		! take?
	CALL RSPEAK(748)			! joke.
	RETURN
C
C O129--	Numeral five
C
60000	IF(PRSA.NE.TAKEW) GO TO 10		! take five?
	CALL RSPEAK(419)			! time passes.
	DO 60100 I=1,3				! wait a while.
	  IF(CLOCKD(X)) RETURN
60100	CONTINUE
	RETURN
C
C O130--	Crypt function
C
61000	IF(.NOT.ENDGMF) GO TO 45000		! if not eg, die.
	IF(PRSA.NE.OPENW) GO TO 61100		! open?
	I=793
	IF(QOPEN(TOMB)) I=794
	CALL RSPEAK(I)
	OFLAG2(TOMB)=OFLAG2(TOMB).OR.OPENBT	! now tomb with view.
	RETURN
C
61100	IF(PRSA.NE.CLOSEW) GO TO 45000		! close?
	I=795
	IF(QOPEN(TOMB)) I=796
	CALL RSPEAK(I)
	OFLAG2(TOMB)=OFLAG2(TOMB).AND..NOT.OPENBT
	IF(HERE.NE.CRYPT) RETURN
	CFLAG(CEVSTE)=.TRUE.
	CTICK(CEVSTE)=3				! if in crypt, start eg.
	RETURN

C OAPPLI, PAGE 12
C
C O131--	Global ladder
C
62000	IF((CPVEC(CPHERE+1).EQ.-2).OR.(CPVEC(CPHERE-1).EQ.-3))
	1	GO TO 62100			! ladder here?
	CALL RSPEAK(865)			! no, lose.
	RETURN
C
62100	IF((PRSA.EQ.CLMBW).OR.(PRSA.EQ.CLMBUW)) GO TO 62200
	CALL RSPEAK(866)			! climb it?
	RETURN
C
62200	IF((CPHERE.EQ.10).AND.(CPVEC(CPHERE+1).EQ.-2))
	1	GO TO 62300			! at exit?
	CALL RSPEAK(867)			! no, hit your head.
	RETURN
C
62300	F=MOVETO(CPANT,WINNER)			! to anteroom.
	F=RMDESC(3)				! describe.
	RETURN
C
	END

C SOBJS-	Simple objects processor
C
C Declarations
C
	LOGICAL FUNCTION SOBJS(RI,ARG)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL MOVETO,OPNCLS,LIT,WASLIT
	LOGICAL F,QOPEN
C
C Functions and data
C
	QOPEN(R)=(OFLAG2(R).AND.OPENBT).NE.0
C
	ODO2=0
	ODI2=0
	IF((PRSO.NE.0).AND.(PRSO.LE.OMAX)) ODO2=ODESC2(PRSO)
	IF(PRSI.NE.0) ODI2=ODESC2(PRSI)
	AV=AVEHIC(WINNER)
	SOBJS=.TRUE.
	WASLIT=LIT(HERE)
C
	GO TO (1000,3000,4000,6000,7000,8000,9000,
	1 13000,14000,16000,17000,
	2 21000,23000,24000,27000,28000,29000,30000,
	3 31000,33000,34000,36000,37000,38000,
	4 41000,42000,43000,44000,46000,
	5 53000,56000)
	6	RI
	CALL BUG(6,RI)
C
C Return here to declare false result.
C
10	SOBJS=.FALSE.
	RETURN
C
C Return here to test for light source change.
C
50	IF(WASLIT.AND..NOT.LIT(HERE)) CALL RSPEAK(406)
	RETURN

C SOBJS, PAGE 3
C
C O1--	Gunk
C
1000	IF(OCAN(GUNK).EQ.0) GO TO 10		! not inside? f
	CALL NEWSTA(GUNK,122,0,0,0)		! falls apart.
	RETURN
C
C O2--	Trophy case
C
3000	IF(PRSA.NE.TAKEW) GO TO 10		! take?
	CALL RSPEAK(128)			! cant.
	RETURN
C
C O3--	Bottle
C
4000	IF((PRSA.NE.THROWW).OR.(PRSO.NE.BOTTL)) GO TO 4100	! throw?
	CALL NEWSTA(BOTTL,129,0,0,0)		! breaks.
	RETURN
C
4100	IF(PRSA.NE.MUNGW) GO TO 10		! mung?
	CALL NEWSTA(BOTTL,131,0,0,0)		! breaks.
	RETURN

C SOBJS, PAGE 4
C
C O4--	Rope
C
6000	IF((HERE.EQ.DOME).OR.(HERE.EQ.SLIDE).OR.(PRSI.EQ.0).OR.
	1  (PRSI.EQ.TIMBE).OR.(PRSI.EQ.COFFI)) GO TO 6100
	IF(PRSA.EQ.TIEW) CALL RSPEAK(135)	! tie, cant do it.
6050	DOMEF=.FALSE.				! not tied in dome.
	TTIE=0					! not tied to timber.
	OFLAG1(TIMBE)=OFLAG1(TIMBE).AND..NOT.NDSCBT
	OFLAG1(COFFI)=OFLAG1(COFFI).AND..NOT.NDSCBT
	ODESC1(TIMBE)=1032			! restore timber, coffin
	ODESC1(COFFI)=1033
	OFLAG1(ROPE)=OFLAG1(ROPE).AND..NOT.NDSCBT
	OFLAG2(ROPE)=OFLAG2(ROPE).AND..NOT.CLMBBT
	RETURN					! rope not climbable
C
6100	IF((PRSA.NE.CLMBDW).OR.(HERE.NE.CPANT)) GO TO 6200
	IF(TTIE.EQ.0) GO TO 6150		! climb down, tied?
	IF(OROOM(TTIE).NE.HERE) GO TO 6150	! tied here?
	CALL RSPSUB(1028,ODESC2(TTIE))		! yes, tumbles after you.
	CALL NEWSTA(ROPE,0,CPUZZ,0,0)		! now in puzzle room.
	CALL NEWSTA(TTIE,0,CPUZZ,0,0)
	GO TO 10				! not handled here.
C
6150	CALL RSPEAK(1029)			! not tied.
	GO TO 10				! not handled here.
C
6200	IF(PRSA.NE.TIEW) GO TO 6400		! tie rope?
	IF(PRSI.NE.RAILI) GO TO 6300		! to railing?
	IF(DOMEF.OR.(TTIE.NE.0)) GO TO 6250	! already tied?
	DOMEF=.TRUE.				! no, now tied.
	CALL NEWSTA(ROPE,137,DOME,0,0)		! put in dome room.
6225	OFLAG1(ROPE)=OFLAG1(ROPE).OR.NDSCBT
	OFLAG2(ROPE)=OFLAG2(ROPE).OR.CLMBBT	! now climbable
	RETURN
C
6250	CALL RSPEAK(136)			! already tied.
	RETURN
C
6300	IF((PRSI.NE.TIMBE).AND.(PRSI.NE.COFFI)) GO TO 10
	IF(DOMEF.OR.(TTIE.NE.0)) GO TO 6250	! already done?
	IF(OROOM(PRSI).NE.0) GO TO 6350		! target on ground?
	CALL RSPEAK(1025)			! too clumsy.
	RETURN
C
6350	CALL RSPSUB(961,ODI2)			! now tied to object.
	TTIE=PRSI
	IF(PRSI.EQ.TIMBE) ODESC1(TIMBE)=1030	! change description
	IF(PRSI.EQ.COFFI) ODESC1(COFFI)=1031	! of target.
	IF(HERE.EQ.CPANT) CALL RSPEAK(1056)	! room-specific words.
	IF(HERE.EQ.SLIDE) CALL RSPEAK(339)
	IF(HERE.EQ.SLIDE) OFLAG1(PRSI)=OFLAG1(PRSI).OR.NDSCBT
	CALL NEWSTA(ROPE,0,HERE,0,0)		! put rope in room.
	GO TO 6225				! rope now climbable.
C
6400	IF(PRSA.NE.UNTIEW) GO TO 6600		! untie rope?
	IF(.NOT.DOMEF.AND.(TTIE.EQ.0)) GO TO 6500 ! tied to obj or railing?
	CALL RSPEAK(139)			! report and then
	GO TO 6050				! clean up all status.
C
6500	CALL RSPEAK(134)			! not tied to anything.
	RETURN
C
6600	IF(DOMEF.OR.(PRSA.NE.DROPW).OR.
	1	(HERE.NE.DOME)) GO TO 6700	! drop & untied from dome?
	CALL NEWSTA(ROPE,140,MTORC,0,0)		! yes, drop.
	RETURN
C
6700	IF((PRSA.NE.TAKEW).OR..NOT.DOMEF) GO TO 6800
	CALL RSPEAK(141)			! take & tied.
	RETURN
C
6800	IF((PRSA.NE.TAKEW).OR.(TTIE.EQ.0)) GO TO 10
	CALL RSPSUB(926,ODESC2(TTIE))		! take & tied.
	RETURN
C
C O5--	Sword
C
7000	IF((PRSA.EQ.TAKEW).AND.(WINNER.EQ.PLAYER))
	1	SWDACT=.TRUE.			! turn on demon.
	GO TO 10
C
C O6--	Lantern
C
8000	IF((PRSA.NE.THROWW).OR.(PRSO.NE.LAMP)) GO TO 8100	! throw?
	CALL NEWSTA(LAMP,0,0,0,0)		! kill lamp,
	CALL NEWSTA(BLAMP,142,HERE,0,0)		! replace with broken.
	IF(HERE.EQ.MTREE) CALL NEWSTA(BLAMP,0,FORE3,0,0)
	IF(LASTIT.EQ.LAMP) LASTIT=BLAMP		! fix last it reference.
	CFLAG(CEVLNT)=.FALSE.			! turn off timer.
	GO TO 50				! go see if now dark.
C
8100	IF(PRSA.EQ.TRNONW) CFLAG(CEVLNT)=.TRUE.
	IF(PRSA.EQ.TRNOFW) CFLAG(CEVLNT)=.FALSE.
	GO TO 10
C
C O7--	Rug
C
9000	IF(PRSA.NE.RAISEW) GO TO 9100		! raise?
	CALL RSPEAK(143)			! cant
	RETURN
C
9100	IF(PRSA.NE.TAKEW) GO TO 9200		! take?
	CALL RSPEAK(144)			! cant
	RETURN
C
9200	IF(PRSA.NE.MOVEW) GO TO 9300		! move?
	CALL RSPEAK(145+ORRUG)
	ORRUG=1
	OFLAG1(DOOR)=OFLAG1(DOOR).OR.VISIBT	! reveal door.
	RETURN
C
9300	IF((PRSA.NE.LOOKUW).OR.(ORRUG.NE.0).OR.
	1	QOPEN(DOOR)) GO TO 10		! look under rug?
	CALL RSPEAK(345)
	RETURN

C SOBJS, PAGE 5
C
C O8--	Skeleton
C
13000	I=ROBRM(HERE,100,LLD2,0,0)+ROBADV(WINNER,LLD2,0,0)
	CALL RSPEAK(162)			! curses.
	RETURN
C
C O9--	Mirror
C
14000	IF(MIRRMF.OR.(PRSA.NE.RUBW)) GO TO 14500
	MROOM=HERE.XOR.1			! calculate new rm.
	DO 14100 I=1,OLNT			! interchange objs.
	  IF(OROOM(I).EQ.HERE) OROOM(I)=-1
	  IF(OROOM(I).EQ.MROOM) OROOM(I)=HERE
	  IF(OROOM(I).EQ.-1) OROOM(I)=MROOM
14100	CONTINUE
	F=MOVETO(MROOM,WINNER)
	CALL RSPEAK(163)			! shake world.
	RETURN
C
14500	IF((PRSA.NE.LOOKW).AND.(PRSA.NE.LOOKIW).AND.
	1	(PRSA.NE.EXAMIW)) GO TO 14600
	I=164					! mirror ok.
	IF(MIRRMF) I=165			! mirror dead.
	CALL RSPEAK(I)
	RETURN
C
14600	IF(PRSA.NE.TAKEW) GO TO 14700		! take?
	CALL RSPEAK(166)			! joke.
	RETURN
C
14700	IF((PRSA.NE.MUNGW).AND.(PRSA.NE.THROWW)) GO TO 10
	I=167					! mirror breaks.
	IF(MIRRMF) I=168			! mirror already broken.
	MIRRMF=.TRUE.
	BADLKF=.TRUE.
	CALL RSPEAK(I)
	RETURN

C SOBJS, PAGE 6
C
C O10--	Dumbwaiter
C
16000	IF(PRSA.NE.RAISEW) GO TO 16100		! raise?
	IF(CAGETF) GO TO 16400			! already at top?
	CALL NEWSTA(TBASK,175,TSHAF,0,0)	! no, raise basket.
	CALL NEWSTA(FBASK,0,BSHAF,0,0)
	IF(HERE.EQ.TSHAF) LASTIT=TBASK		! fix last it reference.
	IF(HERE.EQ.BSHAF) LASTIT=FBASK
	CAGETF=.TRUE.				! at top.
	RETURN
C
16100	IF(PRSA.NE.LOWERW) GO TO 16200		! lower?
	IF(.NOT.CAGETF) GO TO 16400		! already at bottom?
	CALL NEWSTA(TBASK,176,BSHAF,0,0)	! no, lower basket.
	CALL NEWSTA(FBASK,0,TSHAF,0,0)
	IF(HERE.EQ.TSHAF) LASTIT=FBASK		! fix last it reference.
	IF(HERE.EQ.BSHAF) LASTIT=TBASK
	CAGETF=.FALSE.
	GO TO 50				! go see if now dark.
C
16200	IF((PRSO.NE.FBASK).AND.(PRSI.NE.FBASK)) GO TO 16300
	CALL RSPEAK(130)			! wrong basket.
	RETURN
C
16300	IF(PRSA.NE.TAKEW) GO TO 10		! take?
	CALL RSPEAK(177)			! joke.
	RETURN
C
16400	CALL RSPEAK(125+RND(3))			! dummy.
	RETURN
C
C O11--	Ghost
C
17000	IF(PRSO.NE.GHOST) GO TO 17100		! direct object?
	CALL RSPEAK(178)			! joke.
	RETURN
C
17100	CALL RSPEAK(179)			! joke.
	GO TO 10				! don't handle.

C SOBJS, PAGE 7
C
C O12--	Tube
C
21000	IF((PRSA.NE.PUTW).OR.(PRSI.NE.TUBE)) GO TO 21100
	CALL RSPEAK(186)			! cant put back in.
	RETURN
C
21100	IF(PRSA.NE.SQUEEW) GO TO 10		! squeeze?
	IF(QOPEN(PRSO)) GO TO 21200		! tube open?
	CALL RSPEAK(909)			! no, can't do it.
	RETURN
C
21200	IF(OCAN(PUTTY).EQ.PRSO) GO TO 21300	! putty inside?
	CALL RSPEAK(910)			! no, doesn't work.
	RETURN
C
21300	CALL NEWSTA(PUTTY,911,0,0,WINNER)	! putty now in hand.
	RETURN
C
C O13--	Chalice
C
23000	IF((PRSA.NE.TAKEW).OR.(OCAN(PRSO).NE.0).OR.
	1	(OROOM(PRSO).NE.TREAS).OR.(OROOM(THIEF).NE.TREAS).OR.
	2	((OFLAG2(THIEF).AND.FITEBT).EQ.0).OR.
	3	.NOT. THFACT) GO TO 10
	CALL RSPEAK(204)			! cant take.
	RETURN
C
C O14--	Painting
C
24000	IF(PRSA.NE.MUNGW) GO TO 10		! mung?
	CALL RSPEAK(205)			! destroy painting.
	OFVAL(PRSO)=0
	OTVAL(PRSO)=0
	ODESC1(PRSO)=207
	ODESC2(PRSO)=206
	RETURN

C SOBJS, PAGE 8
C
C O15--	Bolt
C
27000	IF(PRSA.NE.TURNW) GO TO 27600		! turn bolt?
	IF(PRSI.NE.WRENC) GO TO 27500		! with wrench?
	IF(GATEF) GO TO 27100			! proper button pushed?
	CALL RSPEAK(210)			! no, lose.
	RETURN
C
27100	IF(LWTIDF) GO TO 27200			! low tide now?
	LWTIDF=.TRUE.				! no, empty dam.
	CALL RSPEAK(211)
	OFLAG2(COFFI)=OFLAG2(COFFI).AND. .NOT.SCRDBT
	OFLAG1(TRUNK)=OFLAG1(TRUNK).OR.VISIBT	! materialize trunk.
	RFLAG(RESER)=(RFLAG(RESER).OR.RLAND)
	1	.AND..NOT.(RWATER+RSEEN)	! keep thief away.
	RETURN
C
27200	LWTIDF=.FALSE.				! yes, fill dam.
	CALL RSPEAK(212)
	IF(OROOM(TRUNK).EQ.RESER) OFLAG1(TRUNK)=OFLAG1(TRUNK)
	1	.AND. .NOT.VISIBT
	RFLAG(RESER)=(RFLAG(RESER).OR.RWATER) .AND..NOT.RLAND
	RETURN
C
27500	CALL RSPSUB(299,ODI2)			! not with that.
	RETURN
C
27600	IF(PRSA.NE.OILW) GO TO 10		! oil?
	CALL RSPEAK(906)			! trouble.
	RETURN
C
C O16--	Grating
C
28000	IF((PRSA.NE.OPENW).AND.(PRSA.NE.CLOSEW)) GO TO 10
	IF(GRUNLF) GO TO 28100			! unlocked?
	CALL RSPEAK(214)			! no, locked.
	RETURN
C
28100	I=215					! unlocked, view frm below.
	IF(HERE.EQ.CLEAR) I=216			! view from clearing
	SOBJS=OPNCLS(GRATE,I,885)		! open/close.
	RFLAG(MGRAT)=RFLAG(MGRAT).AND. .NOT.RLIGHT	! set light/dark.
	IF(.NOT.QOPEN(GRATE)) GO TO 50		! if not open, done.
	RFLAG(MGRAT)=RFLAG(MGRAT).OR.RLIGHT	! now lit.
	RVCLR=1					! leaves shoved aside.
	CALL NEWSTA(GRATE,0,CLEAR,0,0)		! grating in clearing.
	RETURN
C
C O17--	Trap door
C
29000	IF(HERE.NE.LROOM) GO TO 29100		! from living room?
	SOBJS=OPNCLS(DOOR,218,219)		! open/close.
	RETURN
C
29100	IF(HERE.NE.CELLA) GO TO 10		! from cellar?
	IF((PRSA.NE.OPENW).OR.QOPEN(DOOR)) GO TO 29200
	CALL RSPEAK(220)			! cant open closed door.
	RETURN
C
29200	SOBJS=OPNCLS(DOOR,0,22)			! normal open/close.
	RETURN
C
C O18--	Durable door
C
30000	I=0					! assume no appl.
	IF(PRSA.EQ.OPENW) I=221			! open?
	IF(PRSA.EQ.BURNW) I=222			! burn?
	IF(PRSA.EQ.MUNGW) I=223+RND(3)		! mung?
	IF(I.EQ.0) GO TO 10
	CALL RSPEAK(I)
	RETURN
C
C O19--	Master switch
C
31000	IF(PRSA.NE.TURNW) GO TO 10		! turn?
	IF(PRSI.NE.SCREW) GO TO 31500		! with screwdriver?
	IF(QOPEN(MACHI)) GO TO 31600		! lid up?
	CALL RSPEAK(226)			! no, activate.
	IF(OCAN(COAL).NE.MACHI) GO TO 31400	! coal inside?
	CALL NEWSTA(COAL,0,0,0,0)		! kill coal,
	CALL NEWSTA(DIAMO,0,0,MACHI,0)		! replace with diamond.
	RETURN
C
31400	DO 31450 I=1,OLNT			! kill noncoal objects.
	  IF(OCAN(I).NE.MACHI) GO TO 31450	! inside machine?
	  CALL NEWSTA(I,0,0,0,0)		! kill object and contents.
	  CALL NEWSTA(GUNK,0,0,MACHI,0)		! reduce to gunk.
31450	CONTINUE
	RETURN
C
31500	CALL RSPSUB(300,ODI2)			! cant turn with that.
	RETURN
C
31600	CALL RSPEAK(227)			! lid is up.
	RETURN

C SOBJS, PAGE 9
C
C O20--	Leak
C
33000	IF((PRSO.NE.LEAK).OR.(PRSA.NE.PLUGW).OR.(RVMNT.LE.0))
	1	GO TO 10			! plug active leak?
	IF(PRSI.NE.PUTTY) GO TO 33100		! with putty?
	RVMNT=-1				! disable leak.
	CTICK(CEVMNT)=0
	CALL RSPEAK(577)
	RETURN
C
33100	CALL RSPSUB(301,ODI2)			! cant with that.
	RETURN
C
C O21--	Drowning buttons
C
34000	IF(PRSA.NE.PUSHW) GO TO 10		! push?
	GO TO (34100,34200,34300,34400),(PRSO-RBUTT+1)
	GO TO 10				! not a button.
C
34100	RFLAG(HERE)=RFLAG(HERE).XOR.RLIGHT	! red, zap lights.
	I=230
	IF((RFLAG(HERE).AND.RLIGHT).NE.0) I=231
	CALL RSPEAK(I)
	GO TO 50				! go see if now dark.
C
34200	GATEF=.TRUE.				! yellow, release gate.
	CALL RSPEAK(232)
	RETURN
C
34300	GATEF=.FALSE.				! brown, interlock gate.
	CALL RSPEAK(232)
	RETURN
C
34400	IF(RVMNT.NE.0) GO TO 34500		! blue, leak already started?
	CALL RSPEAK(233)			! no, start leak.
	RVMNT=1
	CFLAG(CEVMNT)=.TRUE.
	CTICK(CEVMNT)=-1
	RFLAG(HERE)=RFLAG(HERE).OR.RFILL	! water present.
	OFLAG1(LEAK)=OFLAG1(LEAK).OR.VISIBT	! bring on the leak.
	RETURN
C
34500	CALL RSPEAK(234)			! button jammed.
	RETURN
C
C O22--	Inflatable boat
C
36000	IF(PRSA.NE.INFLAW) GO TO 10		! inflate?
	IF(OROOM(IBOAT).NE.0) GO TO 36100	! in room?
	CALL RSPEAK(235)			! no, joke.
	RETURN
C
36100	IF(PRSI.NE.PUMP) GO TO 36200		! with pump?
	CALL NEWSTA(IBOAT,0,0,0,0)		! kill defl boat,
	CALL NEWSTA(RBOAT,236,HERE,0,0)		! repl with inf.
	IF(LASTIT.EQ.IBOAT) LASTIT=RBOAT	! fix last it reference.
	DEFLAF=.FALSE.
	RETURN
C
36200	I=237					! jokes.
	IF(PRSI.NE.LUNGS) I=303
	CALL RSPSUB(I,ODI2)
	RETURN
C
C O23--	Deflated boat
C
37000	IF(PRSA.NE.INFLAW) GO TO 37100		! inflate?
	CALL RSPEAK(238)			! joke.
	RETURN
C
37100	IF(PRSA.NE.PLUGW) GO TO 10		! plug?
	IF(PRSI.NE.PUTTY) GO TO 33100		! with putty?
	CALL NEWSTA(IBOAT,239,OROOM(DBOAT),OCAN(DBOAT),OADV(DBOAT))
	CALL NEWSTA(DBOAT,0,0,0,0)		! kill defl boat, repl.
	IF(LASTIT.EQ.DBOAT) LASTIT=IBOAT	! fix last it reference.
	RETURN

C SOBJS, PAGE 10
C
C O24--	Rubber boat
C
38000	IF(ARG.NE.0) GO TO 10			! dismiss readin, out.
	IF((PRSA.NE.BOARDW).OR.(OADV(STICK).NE.WINNER)) GO TO 38100
	CALL NEWSTA(RBOAT,0,0,0,0)		! kill infl boat,
	CALL NEWSTA(DBOAT,240,HERE,0,0)		! repl with dead.
	IF(LASTIT.EQ.RBOAT) LASTIT=DBOAT	! fix last it reference.
	DEFLAF=.TRUE.
	GO TO 50				! go see if now dark.
C
38100	IF(PRSA.NE.INFLAW) GO TO 38200		! inflate?
	CALL RSPEAK(367)			! yes, joke.
	RETURN
C
38200	IF(PRSA.NE.DEFLAW) GO TO 10		! deflate?
	IF(AV.EQ.RBOAT) GO TO 38300		! in boat?
	IF(OROOM(RBOAT).EQ.0) GO TO 38400	! on ground?
	CALL NEWSTA(RBOAT,0,0,0,0)		! kill infl boat,
	CALL NEWSTA(IBOAT,241,HERE,0,0)		! repl with defl.
	IF(LASTIT.EQ.RBOAT) LASTIT=IBOAT	! fix last it reference.
	DEFLAF=.TRUE.
	GO TO 50				! go see if now dark.
C
38300	CALL RSPEAK(242)			! in boat.
	RETURN
C
38400	CALL RSPEAK(243)			! not on ground.
	RETURN
C
C O25--	Braided rope (also balloon receptacle, cloth bag)
C
41000	IF((PRSA.NE.TIEW).OR.(PRSO.NE.BROPE).OR.
	1	((PRSI.NE.HOOK1).AND.(PRSI.NE.HOOK2)))
	2	GO TO 41100			! tie to hook?
	BTIEF=PRSI				! record location.
	ODESC1(BTIEF)=1072			! change description.
	CFLAG(CEVBAL)=.FALSE.			! stall ascent.
	CALL RSPEAK(248)
	RETURN
C
41100	IF((PRSA.NE.UNTIEW).OR.(PRSO.NE.BROPE)) GO TO 41300
	IF(BTIEF.NE.0) GO TO 41200		! tied up?
	CALL RSPEAK(249)			! no, joke.
	RETURN
C
41200	CALL RSPEAK(250)
	ODESC1(BTIEF)=1073			! restore description.
	BTIEF=0					! untie.
	CFLAG(CEVBAL)=.TRUE.
	CTICK(CEVBAL)=3				! restart clock.
	RETURN
C
41300	IF((PRSA.NE.FINDW).AND.(PRSA.NE.EXAMIW)) GO TO 41400
	CALL RSPSUB(1063,ODO2)			! describe.
	RETURN
C
41400	IF(PRSA.NE.TAKEW) GO TO 10		! take?
	CALL RSPSUB(1064,ODO2)			! can't.
	IF(PRSO.EQ.BROPE) CALL RSPEAK(1065)	! rope can be tied.
	RETURN
C
C O26--	Safe
C
42000	I=0					! assume unprocessed.
	IF(PRSA.EQ.TAKEW) I=251			! take?
	IF((PRSA.EQ.OPENW).AND.SAFEF) I=253	! open after blast?
	IF((PRSA.EQ.OPENW).AND..NOT.SAFEF) I=254 ! open before blast?
	IF((PRSA.EQ.CLOSEW).AND.SAFEF) I=253	! close after?
	IF((PRSA.EQ.CLOSEW).AND..NOT.SAFEF) I=255
	IF(I.EQ.0) GO TO 10
	CALL RSPEAK(I)
	RETURN
C
C O27--	Fuse
C
43000	IF(PRSA.NE.BURNW) GO TO 10		! burn?
	CALL RSPEAK(256)
	CFLAG(CEVFUS)=.TRUE.
	CTICK(CEVFUS)=2				! start countdown.
	RETURN
C
C O28--	Gnome
C
44000	IF((PRSA.NE.GIVEW).AND.(PRSA.NE.THROWW)) GO TO 44500
	IF(OTVAL(PRSO).EQ.0) GO TO 44100	! treasure?
	CALL RSPSUB(257,ODO2)			! yes, get door.
	CALL NEWSTA(PRSO,0,0,0,0)
	CALL NEWSTA(GNOME,0,0,0,0)		! vanish gnome.
	GNODRF=.TRUE.
	GO TO 50				! go see if now dark.
C
44100	IF((PRSO.NE.BRICK).OR.(OCAN(FUSE).NE.BRICK).OR.
	1	(CTICK(CEVFUS).EQ.0)) GO TO 44200 ! a bomb?
	CALL NEWSTA(GNOME,927,0,0,0)		! gnome leaves.
	CALL NEWSTA(BRICK,0,HERE,0,0)		! brick on floor.
	CFLAG(CEVVLG)=.FALSE.			! turn off gnome clocks.
	CFLAG(CEVGNO)=.FALSE.
	RETURN
C
44200	CALL RSPSUB(258,ODO2)			! no, lose object.
	CALL NEWSTA(PRSO,0,0,0,0)
	GO TO 50				! go see if now dark.
C
44500	CALL RSPEAK(259)			! nervous gnome.
	IF(GNOMEF) RETURN
	CFLAG(CEVGNO)=.TRUE.
	CTICK(CEVGNO)=5				! schedule byebye.
	GNOMEF=.TRUE.
	RETURN
C
C O29--	Coke bottles
C
46000	IF((PRSA.NE.THROWW).AND.(PRSA.NE.MUNGW)) GO TO 10
	CALL NEWSTA(COKES,262,0,0,0)		! mung bottles.
	IF(PRSI.NE.COKES) RETURN		! with cokes?
	CALL RSPSUB(1066,ODO2)			! kill direct object, too.
	CALL NEWSTA(PRSO,0,0,0,0)
	RETURN

C SOBJS, PAGE 11
C
C O30--	Robot
C
53000	IF(PRSA.NE.GIVEW) GO TO 53200		! give?
	IF(PRSO.NE.WATER) GO TO 53100		! water?
	CALL NEWSTA(WATER,1081,0,0,0)		! slips through fingers.
	RETURN
C
53100	CALL NEWSTA(PRSO,0,0,0,AROBOT)		! put on robot.
	CALL RSPSUB(302,ODO2)
	RETURN
C
53200	IF(((PRSA.NE.MUNGW).AND.(PRSA.NE.THROWW)).OR.
	1  ((PRSO.NE.ROBOT).AND.(PRSI.NE.ROBOT))) GO TO 10
	CALL NEWSTA(ROBOT,285,0,0,0)		! kill robot.
	GO TO 50				! go see if now dark.
C
C O31--	Grue
C
56000	IF(PRSA.NE.EXAMIW) GO TO 56100		! examine?
	CALL RSPEAK(288)
	RETURN
C
56100	IF(PRSA.NE.FINDW) GO TO 10		! find?
	CALL RSPEAK(289)
	RETURN
C
	END

C NOBJS-	New objects processor
C
C Declarations
C
	LOGICAL FUNCTION NOBJS(RI,ARG)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL QOPEN,MOVETO,F,RMDESC
	LOGICAL QHERE,OPNCLS,MIRPAN
	LOGICAL LIT,WASLIT,QEMPTY
C
C Functions and data
C
	QOPEN(R)=(OFLAG2(R).AND.OPENBT).NE.0
C
	ODO2=0
	ODI2=0
	IF((PRSO.NE.0).AND.(PRSO.LE.OMAX)) ODO2=ODESC2(PRSO)
	IF(PRSI.NE.0) ODI2=ODESC2(PRSI)
	AV=AVEHIC(WINNER)
	NOBJS=.TRUE.
	WASLIT=LIT(HERE)
C
	GO TO (1000,2000,3000,4000,5000,6000,7000,8000,9000,
	1 10000,11000,12000,13000,14000,15000,16000,17000,
	2 18000,19000,20000,21000,22000,23000,24000,25000,
	3 26000,27000,28000,29000,30000,31000,32000,33000,
	4 34000,35000,36000,37000,38000,39000,40000,41000,
	5 42000,43000,44000,45000,46000),
	6	(RI-31)
	CALL BUG(6,RI)
C
C Return here to declare false result.
C
10	NOBJS=.FALSE.
	RETURN
C
C Return here to test for light source change.
C
50	IF(WASLIT.AND..NOT.LIT(HERE)) CALL RSPEAK(406)
	RETURN

C NOBJS, PAGE 3
C
C O32--	Bills
C
1000	IF(PRSA.NE.EATW) GO TO 1100		! eat?
	CALL RSPEAK(639)			! joke.
	RETURN
C
1100	IF(PRSA.EQ.BURNW) CALL RSPEAK(640)	! burn?  joke.
	GO TO 10				! let it be handled.
C
C O33--	Screen of light
C
2000	TARGET=SCOL				! target is scol.
2100	IF(PRSO.NE.TARGET) GO TO 2400		! prso eq target?
	IF((PRSA.NE.PUSHW).AND.(PRSA.NE.MOVEW).AND.
	1	(PRSA.NE.TAKEW).AND.(PRSA.NE.RUBW)) GO TO 2200
	CALL RSPEAK(673)			! hand passes thru.
	RETURN
C
2200	IF((PRSA.NE.KILLW).AND.(PRSA.NE.ATTACW).AND.
	1	(PRSA.NE.MUNGW)) GO TO 2400	! aggressive?
	CALL RSPSUB(674,ODI2)			! passes thru.
	RETURN
C
2400	IF((PRSA.NE.THROWW).OR.(PRSI.NE.TARGET)) GO TO 10
	IF(HERE.EQ.BKBOX) GO TO 2600		! thru scol?
	CALL NEWSTA(PRSO,0,BKBOX,0,0)		! no, thru wall.
	CALL RSPSUB(675,ODO2)			! ends up in box room.
	CTICK(CEVSCL)=0				! cancel alarm.
	SCOLRM=0				! reset scol room.
	GO TO 50				! go see if now dark.
C
2600	IF(SCOLRM.EQ.0) GO TO 2900		! tried to go thru?
	CALL NEWSTA(PRSO,0,SCOLRM,0,0)		! success.
	CALL RSPSUB(676,ODO2)			! ends up somewhere.
	CTICK(CEVSCL)=0				! cancel alarm.
	SCOLRM=0				! reset scol room.
	GO TO 50				! go see if now dark.
C
2900	CALL RSPEAK(213)			! cant do it.
	RETURN

C NOBJS, PAGE 4
C
C O34--	Gnome of Zurich
C
3000	IF((PRSA.NE.GIVEW).AND.(PRSA.NE.THROWW)) GO TO 3200
	IF(OTVAL(PRSO).NE.0) GO TO 3100		! throw a treasure?
	IF((PRSO.NE.BRICK).OR.(OCAN(FUSE).NE.BRICK).OR.
	1	(CTICK(CEVFUS).EQ.0)) GO TO 3050 ! a bomb?
	CALL NEWSTA(ZGNOM,931,0,0,0)		! gnome leaves.
	CALL NEWSTA(BRICK,0,HERE,0,0)		! brick on floor.
	CFLAG(CEVZGO)=.FALSE.			! stop gnome timers.
	CFLAG(CEVZGI)=.FALSE.
	RETURN
C
3050	CALL NEWSTA(PRSO,641,0,0,0)		! no, go pop.
	RETURN
C
3100	CALL NEWSTA(PRSO,0,0,0,0)		! yes, bye bye treasure.
	CALL RSPSUB(642,ODO2)
	CALL NEWSTA(ZGNOM,0,0,0,0)		! bye bye gnome.
	CFLAG(CEVZGO)=.FALSE.			! cancel exit.
	F=MOVETO(BKENT,WINNER)			! now in bank entrance.
	RETURN
C
3200	IF((PRSA.NE.ATTACW).AND.(PRSA.NE.KILLW).AND.
	1	(PRSA.NE.MUNGW)) GO TO 3300	! aggressive?
	CALL NEWSTA(ZGNOM,643,0,0,0)		! vanish gnome.
	CFLAG(CEVZGO)=.FALSE.			! cancel exit.
	RETURN
C
3300	CALL RSPEAK(644)			! gnome is impatient.
	RETURN
C
C O35--	Egg
C
4000	IF((PRSA.NE.OPENW).OR.(PRSO.NE.EGG)) GO TO 4500
	IF(.NOT.QOPEN(EGG)) GO TO 4100		! open already?
	CALL RSPEAK(649)			! yes.
	RETURN
C
4100	IF(PRSI.NE.0) GO TO 4200		! with something?
	CALL RSPEAK(650)			! no, cant.
	RETURN
C
4200	IF(PRSI.NE.HANDS) GO TO 4300		! with hands?
	CALL RSPEAK(651)			! not recommended.
	RETURN
C
4300	I=652					! mung message.
	IF(((OFLAG1(PRSI).AND.TOOLBT).NE.0).OR.
	1	((OFLAG2(PRSI).AND.WEAPBT).NE.0)) GO TO 4600
	I=653					! novelty 1.
	IF((OFLAG2(PRSO).AND.FITEBT).NE.0) I=654 ! novelty 2.
	OFLAG2(PRSO)=OFLAG2(PRSO).OR.FITEBT
	CALL RSPSUB(I,ODI2)
	RETURN
C
4500	IF(PRSA.NE.MUNGW) GO TO 10		! mung?
	I=655					! you blew it.
4600	CALL NEWSTA(BEGG,I,OROOM(EGG),OCAN(EGG),OADV(EGG))
	CALL NEWSTA(EGG,0,0,0,0)		! vanish egg.
	IF(LASTIT.EQ.EGG) LASTIT=BEGG		! fix last it reference.
	OTVAL(BEGG)=2				! bad egg has value.
	IF(OCAN(CANAR).NE.EGG) GO TO 4700	! was canary inside?
	CALL RSPEAK(ODESCO(BCANA))		! yes, describe result.
	OTVAL(BCANA)=1
	RETURN
C
4700	CALL NEWSTA(BCANA,0,0,0,0)		! no, vanish it.
	RETURN

C NOBJS, PAGE 5
C
C O36--	Canaries, good and bad
C
5000	IF(PRSA.NE.WINDW) GO TO 10		! wind em up?
	IF(PRSO.EQ.CANAR) GO TO 5100		! right one?
	CALL RSPEAK(645)			! no, bad news.
	RETURN
C
5100	IF(.NOT.SINGSF.AND.((HERE.EQ.MTREE).OR.
	1	((HERE.GE.FORE1).AND.(HERE.LT.CLEAR))))
	2	GO TO 5200			! first song in for?
	CALL RSPEAK(646)			! no, mediocre news.
	RETURN
C
5200	SINGSF=.TRUE.				! sang song.
	CALL NEWSTA(BAUBL,647,HERE,0,0)		! place bauble.
	IF(HERE.EQ.MTREE) CALL NEWSTA(BAUBL,0,FORE3,0,0)
	RETURN
C
C O37--	White cliffs
C
6000	IF((PRSA.NE.CLMBW).AND.(PRSA.NE.CLMBUW).AND.
	1	(PRSA.NE.CLMBDW)) GO TO 10	! climb?
	CALL RSPEAK(648)			! oh yeah?
	RETURN
C
C O38--	Wall 
C
7000	IF((IABS(HERE-MLOC).NE.1).OR.(MRHERE(HERE).NE.0).OR.
	1	.NOT.ENDGMF) GO TO 7100		! mirror wall in endgame?
	IF(PRSA.NE.PUSHW) GO TO 10		! pushed?
	CALL RSPEAK(860)			! pushed mirror wall.
	RETURN
C
7100	IF((RFLAG(HERE).AND.RNWALL).EQ.0) GO TO 10
	CALL RSPEAK(662)			! no wall.
	RETURN

C NOBJS, PAGE 6
C
C O39--	Global bird
C
8000	IF(PRSA.NE.FINDW) GO TO 8100		! find?
	CALL RSPEAK(666)
	RETURN
C
8100	IF(PRSA.NE.EXAMIW) GO TO 10		! examine?
	CALL RSPEAK(667)
	RETURN
C
C O40--	Puzzle/Scol walls
C
9000	IF(HERE.NE.CPUZZ) GO TO 9500		! puzzle walls?
	IF(PRSA.NE.PUSHW) GO TO 10		! push?
	DO 9100 I=1,8,2				! locate wall.
	  IF(PRSO.EQ.CPWL(I)) GO TO 9200
9100	CONTINUE
	CALL BUG(80,PRSO)			! what?
C
9200	J=CPWL(I+1)				! get directional offset.
	NXT=CPHERE+J				! get next state.
	WL=CPVEC(NXT)				! get c(next state).
	GO TO (9300,9300,9300,9250,9350),(WL+4)	! process.
C
9250	CALL RSPEAK(876)			! clear corridor.
	RETURN
C
9300	IF(CPVEC(NXT+J).EQ.0) GO TO 9400	! movable, room to move?
9350	CALL RSPEAK(877)			! immovable, no room.
	RETURN
C
9400	I=878					! assume first push.
	IF(CPUSHF) I=879			! not?
	CPUSHF=.TRUE.
	CPVEC(NXT+J)=WL				! move wall.
	CPVEC(NXT)=0				! vacate next state.
	CALL CPGOTO(NXT)			! onward.
	CALL CPINFO(I,NXT)			! describe.
	CALL PRINCR(.TRUE.,HERE)		! print rooms contents.
	RETURN
C
9500	IF(HERE.NE.SCOLAC) GO TO 9700		! in scol active room?
	DO 9600 I=1,12,3
	  TARGET=SCOLWL(I+1)			! assume target.
	  IF(SCOLWL(I).EQ.HERE) GO TO 2100	! treat if found.
9600	CONTINUE
C
9700	IF(HERE.NE.BKBOX) GO TO 10		! in box room?
	TARGET=WNORT
	GO TO 2100

C NOBJS, PAGE 7
C
C O41--	Short pole
C
10000	IF(PRSA.NE.RAISEW) GO TO 10100		! lift?
	I=749					! assume up.
	IF(POLEUF.EQ.2) I=750			! already up?
	CALL RSPEAK(I)
	POLEUF=2				! pole is raised.
	RETURN
C
10100	IF((PRSA.NE.LOWERW).AND.(PRSA.NE.PUSHW)) GO TO 10
	IF(POLEUF.NE.0) GO TO 10200		! already lowered?
	CALL RSPEAK(751)			! cant do it.
	RETURN
C
10200	IF(MOD(MDIR,180).NE.0) GO TO 10300	! mirror n-s?
	POLEUF=0				! yes, lower into
	CALL RSPEAK(752)			! channel.
	RETURN
C
10300	IF((MDIR.NE.270).OR.(MLOC.NE.MRB)) GO TO 10400
	POLEUF=0				! lower into hole.
	CALL RSPEAK(753)
	RETURN
C
10400	CALL RSPEAK(753+POLEUF)			! poleuf = 1 or 2.
	POLEUF=1				! now on floor.
	RETURN
C
C O42--	Mirror switch
C
11000	IF(PRSA.NE.PUSHW) GO TO 10		! push?
	IF(MRPSHF) GO TO 11300			! already pushed?
	CALL RSPEAK(756)			! button goes in.
	DO 11100 I=1,OLNT			! blocked?
	  IF(QHERE(I,MREYE).AND.(I.NE.RBEAM)) GO TO 11200
11100	CONTINUE
	CALL RSPEAK(757)			! nothing in beam.
	RETURN
C
11200	CFLAG(CEVMRS)=.TRUE.			! mirror opens.
	CTICK(CEVMRS)=7
	MRPSHF=.TRUE.
	MROPNF=.TRUE.
	RETURN
C
11300	CALL RSPEAK(758)			! mirror already open.
	RETURN

C NOBJS, PAGE 8
C
C O43--	Beam function
C
12000	IF((PRSA.NE.TAKEW).OR.(PRSO.NE.RBEAM)) GO TO 12100
	CALL RSPEAK(759)			! take beam, joke.
	RETURN
C
12100	I=PRSO					! assume blk with dirobj.
	IF((PRSA.EQ.PUTW).AND.(PRSI.EQ.RBEAM)) GO TO 12200
	IF((PRSA.NE.MUNGW).OR.(PRSO.NE.RBEAM).OR.
	1	(PRSI.EQ.0)) GO TO 10		! break beam with x?
	I=PRSI
12200	IF(OADV(I).NE.WINNER) GO TO 12300	! carrying?
	CALL NEWSTA(I,0,HERE,0,0)		! drop obj.
	CALL RSPSUB(760,ODESC2(I))
	RETURN
C
12300	J=761					! assume not in room.
	IF(QHERE(I,HERE)) J=762			! in room?
	CALL RSPSUB(J,ODESC2(I))		! describe.
	RETURN
C
C O44--	Bronze door
C
13000	IF((HERE.EQ.NCELL).OR.((LCELL.EQ.4).AND.
	1	((HERE.EQ.CELL).OR.(HERE.EQ.SCORR))))
	2	GO TO 13100
	CALL RSPEAK(763)			! door not there.
	RETURN
C
13100	IF(.NOT.OPNCLS(ODOOR,764,765)) GO TO 10	! open/close?
	IF((HERE.EQ.NCELL).AND.QOPEN(ODOOR))
	1	CALL RSPEAK(766)		! descr view.
	RETURN
C
C O45--	Quiz door
C
14000	IF((PRSA.NE.OPENW).AND.(PRSA.NE.CLOSEW)) GO TO 14100
	CALL RSPEAK(767)			! door wont move.
	RETURN
C
14100	IF(PRSA.NE.KNOCKW) GO TO 10		! knock?
	IF(INQSTF) GO TO 14200			! tried it already?
	INQSTF=.TRUE.				! start inquisition.
	CFLAG(CEVINQ)=.TRUE.
	CTICK(CEVINQ)=2
	QUESNO=RND(8)				! select question.
	NQATT=0
	CORRCT=0
	CALL RSPEAK(768)			! announce rules.
	CALL RSPEAK(769)
	CALL RSPEAK(770+QUESNO)			! ask question.
	RETURN
C
14200	CALL RSPEAK(798)			! no reply.
	RETURN
C
C O46--	Locked door
C
15000	IF(PRSA.NE.OPENW) GO TO 10		! open?
	CALL RSPEAK(778)			! cant.
	RETURN
C
C O47--	Cell door
C
16000	NOBJS=OPNCLS(CDOOR,779,780)		! open/close?
	RETURN

C NOBJS, PAGE 9
C
C O48--	Dialbutton
C
17000	IF(PRSA.NE.PUSHW) GO TO 10		! push?
	CALL RSPEAK(809)			! click.
	IF(QOPEN(CDOOR)) CALL RSPEAK(810)	! close cell door.
	OFLAG2(CDOOR)=OFLAG2(CDOOR).AND. .NOT.OPENBT
	OFLAG2(ODOOR)=OFLAG2(ODOOR).AND. .NOT.OPENBT
	IF(LCELL.EQ.PNUMB) RETURN		! any change?
C
	DO 17100 I=1,OLNT			! relocate old to hyper.
	  IF((OROOM(I).EQ.CELL).AND.((OFLAG1(I).AND.DOORBT).EQ.0))
	1	CALL NEWSTA(I,0,LCELL*HFACTR,0,0)
	  IF(OROOM(I).EQ.(PNUMB*HFACTR))
	1	CALL NEWSTA(I,0,CELL,0,0)	! move in new hyper.
17100	CONTINUE
C
	OFLAG1(ODOOR)=OFLAG1(ODOOR).AND. .NOT.VISIBT
	IF(PNUMB.EQ.4) OFLAG1(ODOOR)=OFLAG1(ODOOR).OR.VISIBT
C
	IF(AROOM(PLAYER).NE.CELL) GO TO 17400	! player in cell?
	IF(LCELL.NE.4) GO TO 17200		! in right cell?
	OFLAG1(ODOOR)=OFLAG1(ODOOR).OR. VISIBT
	F=MOVETO(NCELL,PLAYER)			! yes, moveto ncell.
	GO TO 17400
17200	F=MOVETO(PCELL,PLAYER)			! no, moveto pcell.
C
17400	LCELL=PNUMB
	RETURN

C NOBJS, PAGE 10
C
C O49--	Dial indicator
C
18000	IF(PRSA.NE.SPINW) GO TO 18100		! spin?
	PNUMB=RND(8)+1				! whee!
	CALL RSPSUB(797,712+PNUMB)
	RETURN
C
18100	IF((PRSA.NE.MOVEW).AND.(PRSA.NE.PUTW).AND.
	1	(PRSA.NE.TRNTOW)) GO TO 10
	IF(PRSI.NE.0) GO TO 18200		! turn dial to x?
	CALL RSPEAK(806)			! must specify.
	RETURN
C
18200	IF((PRSI.GE.NUM1).AND.(PRSI.LE.NUM8)) GO TO 18300
	CALL RSPEAK(807)			! must be digit.
	RETURN
C
18300	PNUMB=PRSI-NUM1+1			! set up new.
	CALL RSPSUB(808,712+PNUMB)
	RETURN
C
C O50--	Global mirror
C
19000	NOBJS=MIRPAN(832,.FALSE.)
	RETURN
C
C O51--	Global panel
C
20000	IF(HERE.NE.FDOOR) GO TO 20100		! at front door?
	IF((PRSA.NE.OPENW).AND.(PRSA.NE.CLOSEW)) GO TO 10
	CALL RSPEAK(843)			! panel in door, nogo.
	RETURN
C
20100	NOBJS=MIRPAN(838,.TRUE.)
	RETURN
C
C O52--	Puzzle Room slit
C
21000	IF((PRSA.NE.PUTW).OR.(PRSI.NE.CSLIT)) GO TO 10
	IF(PRSO.NE.GCARD) GO TO 21100		! put card in slit?
	CALL NEWSTA(PRSO,863,0,0,0)		! kill card.
	CPOUTF=.TRUE.				! open door.
	RETURN
C
21100	IF(((OFLAG1(PRSO).AND.VICTBT).EQ.0).AND.
	1  ((OFLAG2(PRSO).AND.VILLBT).EQ.0)) GO TO 21200
	CALL RSPEAK(552+RND(6))			! joke for vill, vict.
	RETURN
C
21200	CALL NEWSTA(PRSO,0,0,0,0)		! kill object.
	CALL RSPSUB(864,ODO2)			! describe.
	RETURN

C NOBJS, PAGE 11
C
C O53--	Global brochure or stamp
C
22000	IF(PRSO.NE.STAMP) GO TO 22100		! stamp?
	IF(PRSA.EQ.TAKEW) OFLAG1(BROCH)=OFLAG1(BROCH).AND..NOT.CONTBT
	GO TO 10				! do normal take.
C
22100	IF((PRSO.NE.BROCH).OR.
	1  ((PRSA.NE.EXAMIW).AND.(PRSA.NE.READW)))
	2  GO TO 22200				! examine brochure?
	CALL RSPEAK(942)			! describe.
	IF(OCAN(STAMP).EQ.BROCH) CALL RSPEAK(943)
	RETURN
C
22200	IF((PRSA.NE.FINDW).OR..NOT.BROC1F) GO TO 22300
	CALL RSPEAK(944)			! on the way.
	RETURN
C
22300	IF(PRSA.NE.SENDW) GO TO 22400		! send?
	IF(BROC2F) CALL RSPEAK(945)		! already got it.
	IF(BROC1F.AND..NOT.BROC2F) CALL RSPEAK(944)	! on the way.
	IF(.NOT.BROC1F) CALL RSPEAK(947)	! postal service.
	BROC1F=.TRUE.				! send for brochure.
	RETURN
C
22400	IF(PRSO.NE.GBROCH) GO TO 10		! global brochure?
	CALL RSPEAK(1071)			! pretend it's not there.
	RETURN
C
C O54--	Global ground
C
23000	IF(HERE.EQ.SBEACH) GO TO 36000		! at sandy beach? then sand.
	IF(PRSA.NE.DIGW) GO TO 10		! dig?
	CALL RSPEAK(924)			! can't do it.
	RETURN
C
C O55--	Global granite wall
C
24000	I=916					! joke for take.
	IF(PRSA.EQ.TAKEW) GO TO 24100		! take?
	I=918					! temple, treasure find.
	IF(HERE.EQ.SLIDE) I=917			! slide find.
	IF(PRSA.NE.FINDW) GO TO 10		! find?
24100	CALL RSPEAK(I)				! tell all.
	RETURN

C NOBJS, PAGE 12
C
C O56--	Global house
C
25000	IF((HERE.GE.WHOUS).AND.(HERE.LE.EHOUS)) GO TO 25200
	IF(PRSA.NE.FINDW) GO TO 25100		! find, not at house?
	I=892					! joke 1.
	IF(HERE.EQ.CLEAR) I=893			! joke 2 for clearing.
	CALL RSPEAK(I)
	RETURN
C
25100	CALL RSPEAK(894)			! not there.
	RETURN
C
25200	IF(PRSA.NE.FINDW) GO TO 25300		! find, at house?
	CALL RSPEAK(895)			! right there.
	RETURN
C
25300	IF(PRSA.NE.EXAMIW) GO TO 25400		! look at?
	CALL RSPEAK(896)			! a nice house.
	RETURN
C
25400	IF(PRSA.NE.BURNW) GO TO 25500		! burn?
	CALL RSPEAK(897)			! bad boy.
	RETURN
C
25500	IF(PRSA.NE.GTHROW) GO TO 10		! go through?
	IF(HERE.EQ.EHOUS) GO TO 25600		! at east side?
	CALL RSPEAK(898)			! can't do it
	RETURN
C
25600	IF(QOPEN(WINDO)) GO TO 25700		! window open?
	CALL RSPEAK(899)			! no, shut
	RETURN
C
25700	F=MOVETO(KITCH,WINNER)			! move into kitchen.
	F=RMDESC(0)				! describe room
	RETURN
C
C O57--	Barred window in white house
C
26000	IF((PRSA.NE.OPENW).AND.(PRSA.NE.LOOKIW).AND.
	1  (PRSA.NE.GTHROW)) GO TO 26100	! open, look in, enter?
	CALL RSPEAK(1039)			! window barred.
	RETURN
C
26100	IF(PRSA.NE.CLOSEW) GO TO 10		! close?
	CALL RSPEAK(1040)			! already closed and locked.
	RETURN

C NOBJS, PAGE 13
C
C O58--	Global well
C
27000	IF(((OFLAG1(PRSO).AND.TAKEBT).EQ.0).OR.(PRSO.EQ.WATER).OR.
	1  ((PRSA.NE.THROWW).AND.(PRSA.NE.PUTW).AND.(PRSA.NE.DROPW)))
	2  GO TO 10				! throw, put, drop x in well?
	CALL RSPSUB(939,ODO2)
	CALL NEWSTA(PRSO,0,BWELL,0,0)		! put in well bottom.
	GO TO 50				! go see if now dark.
C
C O59--	Global rope
C
28000	IF(PRSA.NE.TAKEW) GO TO 28100		! take?
	CALL RSPEAK(1006)			! not a good idea.
	RETURN
C
28100	IF(PRSA.NE.DROPW) GO TO 28300		! drop?
	CALL RSPEAK(1007)			! you lose.
28200	F=MOVETO(CELLA,WINNER)			! drop into cellar.
	F=RMDESC(3)				! describe.
	RETURN
C
28300	IF((PRSA.EQ.CLMBW).OR.(PRSA.EQ.CLMBUW).OR.
	1  (PRSA.EQ.CLMBDW)) GO TO 10		! normal climb.
	CALL RSPEAK(1008)			! not a good idea.
	RETURN
C
C O60--	Global slide
C
29000	IF((PRSA.NE.GTHROW).AND.
	1 ((PRSA.NE.PUTW).OR.(PRSO.NE.AOBJ(PLAYER)))) GO TO 29100
	CALL RSPEAK(1010)			! down the slide.
	GO TO 28200
C
29100	IF(PRSA.NE.PUTW) GO TO 10		! put in slide?
	IF((OFLAG1(PRSO).AND.TAKEBT).NE.0) GO TO 29200
	CALL RSPEAK(552+RND(6))			! can't take it.
	RETURN
C
29200	IF(PRSO.NE.TTIE) GO TO 29300		! tied object?
	OFLAG1(TTIE)=OFLAG1(TTIE).AND..NOT.NDSCBT
	OFLAG1(ROPE)=OFLAG1(ROPE).AND..NOT.NDSCBT
	OFLAG2(ROPE)=OFLAG2(ROPE).AND..NOT.CLMBBT
	ODESC1(TIMBE)=1032			! restored untied desc.
	ODESC1(COFFI)=1033
	TTIE=0					! not tied any more.
	CALL NEWSTA(ROPE,0,CELLA,0,0)		! rope is now here.
29300	CALL RSPSUB(1011,ODO2)
	CALL NEWSTA(PRSO,0,CELLA,0,0)		! put in cellar,
	IF(PRSO.EQ.WATER) CALL NEWSTA(PRSO,0,0,0,0)	! unless water.
	GO TO 50				! go see if now dark.

C NOBJS, PAGE 14
C
C O61--	Barrel
C
30000	IF(ARG.NE.1) GO TO 10			! read in?
	I=0					! default.
	IF(PRSA.EQ.WALKW) I=920			! walk?
	IF(PRSA.EQ.LOOKW) I=921			! look?
	IF(PRSA.EQ.BURNW) I=922			! burn?
	IF(PRSA.EQ.TAKEW) I=552+RND(6)		! take?
	CALL RSPEAK(I)
	NOBJS=I.NE.0				! handled?
	RETURN
C
C O62--	Hot bell
C
31000	IF(PRSA.NE.TAKEW) GO TO 31100		! take?
	CALL RSPEAK(972)			! too hot.
	RETURN
C
31100	IF(PRSA.NE.RINGW) GO TO 31400		! ring?
	IF(PRSI.NE.0) GO TO 31200		! with something?
	CALL RSPEAK(973)			! too hot.
	RETURN
C
31200	IF((OFLAG1(PRSI).AND.BURNBT).EQ.0) GO TO 31300
	CALL RSPSUB(974,ODI2)			! burnable is consumed.
	CALL NEWSTA(PRSI,0,0,0,0)		! vanish it.
	RETURN
C
31300	I=975					! joke 1.
	IF(PRSI.EQ.HANDS) I=973			! joke for hands.
	CALL RSPEAK(I)
	RETURN
C
31400	IF(PRSA.NE.PORONW) GO TO 10		! pour on?
	CALL NEWSTA(HBELL,0,0,0,0)		! vanish bell.
	CALL NEWSTA(BELL,976,LLD1,0,0)		! insert real bell.
	IF(LASTIT.EQ.HBELL) LASTIT=BELL		! fix last it reference.
	CALL NEWSTA(PRSO,0,0,0,0)		! vanish water.
	CTICK(CEVXBH)=0				! cancel cooling.
	RETURN
C
C O63--	Axe
C
32000	IF(PRSA.NE.TAKEW) GO TO 10		! take?
	CALL RSPEAK(891)			! too hot.
	RETURN

C NOBJS, PAGE 15
C
C O64--	Timber (also coffin)
C
33000	IF((PRSA.NE.TAKEW).OR.(PRSO.NE.TTIE)) GO TO 10
	CALL RSPSUB(1009,ODO2)			! rope becomes untied.
	OFLAG1(TTIE)=OFLAG1(TTIE).AND..NOT.NDSCBT
	OFLAG1(ROPE)=OFLAG1(ROPE).AND..NOT.NDSCBT
	OFLAG2(ROPE)=OFLAG2(ROPE).AND..NOT.CLMBBT
	ODESC1(TIMBE)=1032			! restored untied desc.
	ODESC1(COFFI)=1033
	TTIE=0					! not tied any more.
	CALL NEWSTA(ROPE,0,HERE,0,0)		! rope is now here.
	GO TO 10				! don't handle.
C
C O65--	Guano
C
34000	IF(PRSA.NE.DIGW) GO TO 10		! dig?
	RVGUA=MIN0(4,RVGUA+1)			! go to next state.	
	CALL RSPEAK(91+RVGUA)			! describe.
	RETURN
C
C O66--	Alice room leak
C
35000	IF(PRSA.NE.TAKEW) GO TO 35100		! take?
	CALL RSPEAK(552+RND(6))			! joke.
	RETURN
C
35100	IF((PRSA.NE.PLUGW).OR.(PRSO.NE.PLEAK)) GO TO 10	! plug leak?
	CALL RSPEAK(929)			! can't reach.
	RETURN

C NOBJS, PAGE 16
C
C O67--	Sand
C
36000	IF(PRSA.NE.DIGW) GO TO 10		! dig?
	RVSND=RVSND+1				! go to next state.
	GO TO (36100,36100,36100,36400,36500),RVSND	! process
	CALL BUG(2,RVSND)
C
36100	CALL RSPEAK(85+RVSND)			! 1..3, describe.
	RETURN
C
36400	IF((OFLAG1(STATU).AND.VISIBT).EQ.0) CALL RSPEAK(89)
	OFLAG1(STATU)=OFLAG1(STATU).OR.VISIBT	! 4, statue appears.
	RETURN
C
36500	RVSND=0					! 5, sand collapses.
	IF(OROOM(STATU).EQ.HERE)
	1	OFLAG1(STATU)=OFLAG1(STATU).AND..NOT.VISIBT
	CALL JIGSUP(90)				! gonzo.
	RETURN
C
C O68--	Torch
C
37000	IF(PRSA.NE.TRNOFW) GO TO 10		! extinguish?
	CALL RSPEAK(900)			! can't do it.
	RETURN
C
C O69--	Tool chests
C
38000	IF(PRSA.NE.EXAMIW) GO TO 38100		! examine?
	CALL RSPEAK(907)			! describe.
	RETURN
C
38100	IF(PRSA.NE.TAKEW) GO TO 10		! take?
	CALL RSPEAK(908)			! can't do it.
	RETURN

C NOBJS, PAGE 17
C
C O70--	Palantir door
C
39000	IF((PRSA.NE.LOOKUW).OR..NOT.MATF) GO TO 39100
	CALL RSPEAK(995)			! mat under door.
	RETURN
C
39100	IF(PRSA.NE.UNLOKW) GO TO 39500		! unlock?
	IF(PRSI.NE.PKEY) GO TO 39400		! with rusty key?
39200	IF((OCAN(PKEY).EQ.(HERE-PRM+PKH1)).OR.
	1	QEMPTY(HERE-PRM+PKH1)) GO TO 39300	! keyhole empty?
39250	CALL RSPEAK(991)			! no
	RETURN
C
39300	CALL RSPEAK(996)			! now unlocked.
	PUNLKF=.TRUE.
	RETURN
C
39400	I=997					! joke 1.
	IF(PRSI.EQ.KEYS) I=998			! joke 2 for keys.
	CALL RSPEAK(I)
	RETURN
C
39500	IF(PRSA.NE.LOCKW) GO TO 39700		! lock?
	IF(PRSI.EQ.PKEY) GO TO 39600		! with rusty key?
	CALL RSPEAK(999)			! no
	RETURN
C
39600	IF((OCAN(PKEY).NE.(HERE-PRM+PKH1)).AND.
	1	.NOT.QEMPTY(HERE-PRM+PKH1)) GO TO 39250
	CALL RSPEAK(1000)			! now locked.
	PUNLKF=.FALSE.
	RETURN
C
39700	IF((PRSA.NE.PUTUNW).OR.((PRSO.NE.BLABE).AND.
	1  (PRSO.NE.LABEL).AND.(PRSO.NE.CARD).AND.
	2  (PRSO.NE.WARNI).AND.(PRSO.NE.RBTLB).AND.
	3  (PRSO.NE.GUIDE))) GO TO 39800	! put small paper?
	CALL NEWSTA(PRSO,1001,HERE.XOR.1,0,0)	! put in other room.
	RETURN
C
39800	IF((PRSA.NE.OPENW).AND.(PRSA.NE.CLOSEW)) GO TO 10
	IF(.NOT.PUNLKF) GO TO 39900		! open or close, unlocked?
	NOBJS=OPNCLS(PRSO,1002,1003)		! open or close.
	RETURN
C
39900	CALL RSPEAK(1000)			! door locked.
	RETURN
C
C O71--	Palantir window
C
40000	IF(PRSA.NE.GTHROW) GO TO 40100		! go through?
	CALL RSPEAK(1004)			! can't do it.
	RETURN
C
40100	IF(PRSA.NE.LOOKIW) GO TO 10		! look in?
	IF(QOPEN(PDOOR)) GO TO 40200		! door open?
	PLOOKF=.TRUE.				! set window flag.
	SVFLAG=RFLAG(HERE.XOR.1)		! save room flags from other.
	F=MOVETO(HERE.XOR.1,WINNER)		! go to other room.
	F=RMDESC(3)				! describe it.
	F=MOVETO(HERE.XOR.1,WINNER)		! come back.
	RFLAG(HERE.XOR.1)=SVFLAG		! restore flags.
	RETURN
C
40200	CALL RSPEAK(1005)			! door open, dummy.
	RETURN

C NOBJS, PAGE 18
C
C O72--	Keyhole lids
C
41000	IF((PRSA.NE.OPENW).AND.(PRSA.NE.RAISEW)) GO TO 41100
	CALL RSPEAK(985)			! open lid.
	OFLAG2(PRSO)=OFLAG2(PRSO).OR.OPENBT
	RETURN
C
41100	IF((PRSA.NE.CLOSEW).AND.(PRSA.NE.LOWERW)) GO TO 10
	IF(QEMPTY(PRSO-PLID1+PKH1)) GO TO 41200		! keyhole empty?
	CALL RSPEAK(986)			! can't do it.
	RETURN
C
41200	CALL RSPEAK(987)			! close lid.
	OFLAG2(PRSO)=OFLAG2(PRSO).AND..NOT.OPENBT
	RETURN
C
C O73--	Keyholes
C
42000	IF(PRSA.NE.LOOKIW) GO TO 42200		! look in?
	I=988					! doesn't work.
	IF(QOPEN(PLID1).AND.QOPEN(PLID2).AND.
	1	QEMPTY(PKH1).AND.QEMPTY(PKH2).AND.
	2	LIT(HERE.XOR.1)) I=989		! does work
	CALL RSPEAK(I)
	RETURN
C
42200	IF(PRSA.NE.PUTW) GO TO 10		! put?
	IF(QOPEN(PRSI-PKH1+PLID1)) GO TO 42300	! lid open?
	CALL RSPEAK(990)			! no.
	RETURN
C
42300	IF(QEMPTY(PRSI)) GO TO 42400		! keyhole empty?
	CALL RSPEAK(991)			! no.
	RETURN
C
42400	IF((PRSO.NE.SCREW).AND.(PRSO.NE.KEYS).AND.
	1  (PRSO.NE.STICK).AND.(PRSO.NE.PKEY)) GO TO 42700
	IF(QEMPTY(PRSI.XOR.1)) GO TO 10		! nothing to shove.
	DO 42500 I=1,OLNT
	  IF(OCAN(I).EQ.(PRSI.XOR.1)) GO TO 42600	! find obj in keyhole.
42500	CONTINUE
	CALL BUG(67,PRSI.XOR.1)
C
42600	CALL NEWSTA(I,992,HERE.XOR.1,0,0)	! obj falls to floor.
	IF(MATF) MATOBJ=I			! if mat, falls on that.
	GO TO 10				! finish put.
C
42700	CALL RSPSUB(993,ODO2)			! doesn't fit.
	RETURN

C NOBJS, PAGE 19
C
C O74--	Rusty key
C
43000	IF(PRSA.NE.TURNW) GO TO 10		! turn?
	IF(PUNLKF) GO TO 39600			! unlock?
	GO TO 39200				! otherwise lock.
C
C O75--	Palantirs
C
44000	IF(PRSA.NE.LOOKIW) GO TO 10		! look in?
	OBJ=PALAN				! assume dest = palan.
	IF(PRSO.EQ.PALAN) OBJ=PAL3		! if palan, then pal3.
	IF(PRSO.EQ.PAL3) OBJ=SPHER		! if pal3, then sphere.
	J=HERE					! save here.
	K=OCAN(OBJ)				! get dest container.
	IF(OROOM(OBJ).NE.0) J=OROOM(OBJ)	! if dest in room, use.
	IF(K.NE.0) J=OROOM(K)			! if cont, use cont room.
	IF((J.EQ.0).OR.(OADV(OBJ).EQ.-THIEF)) GO TO 44200
	IF(.NOT.LIT(J)) GO TO 44200		! see destination?
	IF(K.EQ.0) GO TO 44100			! contained?
	IF(((OFLAG1(K).AND.TRANBT).EQ.0).AND..NOT.QOPEN(K))
	1	GO TO 44200			! see out of it?
44100	CALL RSPEAK(1024)			! start vision.
	OFLAG1(OBJ)=OFLAG1(OBJ).AND..NOT.VISIBT	! object not visible.
	SVHERE=HERE				! save state.
	SVFLAG=RFLAG(J)
	F=MOVETO(J,WINNER)			! move to new room.
	F=RMDESC(3)				! describe.
	IF(J.EQ.SVHERE) CALL RSPEAK(1026)	! was it here?
	F=MOVETO(SVHERE,WINNER)			! come back.
	RFLAG(J)=SVFLAG				! restore flags.
	OFLAG1(OBJ)=OFLAG1(OBJ).OR.VISIBT	! restore object.
	RETURN
C
44200	CALL RSPEAK(1023)			! nothing to see.
	RETURN
C
C O76--	Mat
C
45000	IF((PRSA.NE.PUTUNW).OR.(PRSI.NE.PDOOR)) GO TO 45100
	MATF=.TRUE.				! put under right door.
	CALL NEWSTA(PRSO,983,HERE,0,0)
	RETURN
C
45100	IF(((PRSA.NE.TAKEW).AND.(PRSA.NE.MOVEW)).OR.(MATOBJ.EQ.0)) 
	1	GO TO 10			! take or move?
	CALL NEWSTA(MATOBJ,0,HERE,0,0)		! materialize mat object.
	CALL RSPSUB(984,ODESC2(MATOBJ))
	MATOBJ=0
	MATF=.FALSE.
	IF(PRSA.EQ.TAKEW) GO TO 10		! do normal take.
	RETURN					! move is done.
C
C O77--	Stove
C
46000	IF((PRSA.NE.TAKEW).AND.(PRSA.NE.RUBW).AND.
	1  (PRSA.NE.ATTACW).AND.(PRSA.NE.MUNGW))
	2  GO TO 46100				! take, feel, attack, mung?
	CALL RSPEAK(994)			! too hot.
	RETURN
C
46100	IF(PRSA.NE.THROWW) GO TO 10		! throw at stove?
	IF(PRSO.NE.WATER) GO TO 46200		! water?
	CALL NEWSTA(WATER,978,0,0,0)		! evaporates.
	RETURN
C
46200	IF((OFLAG1(PRSO).AND.BURNBT).EQ.0) GO TO 10	! burnable?
	CALL RSPSUB(974,ODO2)			! burns up.
	CALL NEWSTA(PRSO,0,0,0,0)		! vanishes.
	RETURN
C
	END

C MIRPAN--	Processor for global mirror/panel
C
C Declarations
C
	LOGICAL FUNCTION MIRPAN(ST,PNF)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL PNF
C
	MIRPAN=.TRUE.
	NUM=MRHERE(HERE)			! get mirror num.
	IF(NUM.NE.0) GO TO 100			! any here?
	CALL RSPEAK(ST)				! no, lose.
	RETURN
C
100	IF((PRSA.NE.MOVEW).AND.(PRSA.NE.OPENW)) GO TO 200
	CALL RSPEAK(ST+1)			! cant open or move.
	RETURN
C
200	MRBF=0					! assume mirror ok.
	IF(((NUM.EQ.1).AND..NOT.MR1F).OR.
	1  ((NUM.EQ.2).AND..NOT.MR2F)) MRBF=1
	IF(PNF.OR.((PRSA.NE.LOOKIW).AND.(PRSA.NE.EXAMIW).AND.
	1	(PRSA.NE.LOOKW))) GO TO 300
	CALL RSPEAK(844+MRBF)			! look in mirror.
	RETURN
C
300	IF(PRSA.NE.MUNGW) GO TO 400		! break?
	CALL RSPEAK(ST+2+MRBF)			! do it.
	IF((NUM.EQ.1).AND..NOT.PNF) MR1F=.FALSE.
	IF((NUM.EQ.2).AND..NOT.PNF) MR2F=.FALSE.
	RETURN
C
400	IF(PNF.OR.(MRBF.EQ.0)) GO TO 500	! broken mirror?
	CALL RSPEAK(846)
	RETURN
C
500	IF(PRSA.NE.PUSHW) GO TO 600		! push?
	CALL RSPEAK(ST+3+NUM)
	RETURN
C
600	MIRPAN=.FALSE.				! cant handle it.
	RETURN
C
	END

C BALLOP-	Balloon function
C
C Declarations
C
	LOGICAL FUNCTION BALLOP(ARG)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL FINDXT,QEMPTY
C
	BALLOP=.TRUE.				! assume wins.
	IF(ARG.NE.2) GO TO 200			! readout?
	IF(PRSA.NE.LOOKW) GO TO 10		! only process look.
	IF(BINFF.NE.0) GO TO 50			! inflated?
	CALL RSPEAK(543)			! no.
	GO TO 100
50	CALL RSPSUB(544,ODESC2(BINFF))		! yes.
100	IF(BTIEF.NE.0) CALL RSPEAK(545)		! hooked?
	GO TO 10
C
200	IF(ARG.NE.1) GO TO 500			! readin?
	IF(PRSA.NE.WALKW) GO TO 300		! walk?
	IF(FINDXT(PRSO,HERE)) GO TO 250		! valid exit?
	CALL RSPEAK(546)			! no, joke.
	RETURN
C
250	IF(BTIEF.EQ.0) GO TO 275		! tied up?
	CALL RSPEAK(547)			! yes, joke.
	RETURN
C
275	IF(XTYPE.NE.XNORM) GO TO 10		! normal exit?
	IF((RFLAG(XROOM1).AND.RMUNG).NE.0) GO TO 10
	BLOC=XROOM1
	CFLAG(CEVBAL)=.TRUE.
	CTICK(CEVBAL)=3
10	BALLOP=.FALSE.
	RETURN
C
300	IF((PRSA.NE.TAKEW).OR.(PRSO.NE.BINFF)) GO TO 350
	CALL RSPSUB(548,ODESC2(BINFF))		! recep cont too hot.
	RETURN
C
350	IF((PRSA.NE.PUTW).OR.(PRSI.NE.RECEP).OR.QEMPTY(RECEP))
	1	GO TO 10			! recep already full.
	CALL RSPEAK(549)
	RETURN
C
500	IF((PRSA.NE.BURNW).OR.(OCAN(PRSO).NE.RECEP)) GO TO 10
	CALL RSPSUB(550,ODESC2(PRSO))		! light fire in recep.
	CFLAG(CEVBRN)=.TRUE.
	CTICK(CEVBRN)=OSIZE(PRSO)*20
	OFLAG1(PRSO)=(OFLAG1(PRSO).OR.(ONBT+FLAMBT+LITEBT)).AND.
	1	.NOT.(TAKEBT+READBT)		! burn it.
	IF(BINFF.NE.0) RETURN			! already inflated?
	IF(.NOT.BLABF) CALL NEWSTA(BLABE,0,0,BALLO,0)	! insert label.
	BLABF=.TRUE.				! only once.
	BINFF=PRSO
	CFLAG(CEVBAL)=.TRUE.
	CTICK(CEVBAL)=3				! start countdown.
	CALL RSPEAK(551)
	RETURN
C
	END

C TROLLP-	Troll function
C
C Declarations
C
	LOGICAL FUNCTION TROLLP(ARG)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL QHERE,PROB
C
	TROLLP=.TRUE.				! assume wins.
	IF(PRSA.NE.FIGHTW) GO TO 1100		! fight?
	IF(OCAN(AXE).EQ.TROLL) GO TO 10		! got axe?  nothing.
	I=433					! assume cant get.
	IF(.NOT.QHERE(AXE,HERE).OR.PROB(25,10)) GO TO 1050	! here?
	I=434					! yes, recover.
	CALL NEWSTA(AXE,0,0,TROLL,0)
1050	IF(QHERE(TROLL,HERE)) CALL RSPEAK(I)	! if player here.
	RETURN
C
1100	IF(PRSA.NE.DEADXW) GO TO 1200		! dead?
	TROLLF=.TRUE.				! permit exits.
	RETURN
C
1200	IF(PRSA.NE.OUTXW) GO TO 1300		! out?
	TROLLF=.TRUE.				! permit exits.
	OFLAG1(AXE)=OFLAG1(AXE).AND. .NOT.VISIBT
	ODESC1(TROLL)=435			! troll out.
	RETURN
C
1300	IF((PRSA.NE.INXW).AND.			! wake from fight demon?
	1  (((PRSA.NE.ALARMW).AND.(PRSA.NE.KICKW)).OR.
	2    (OCAPAC(TROLL).GE.0))) GO TO 1400	! wake, kick while out?
	OCAPAC(TROLL)=IABS(OCAPAC(TROLL))	! yes, wake him.
	OFLAG1(AXE)=OFLAG1(AXE).OR.VISIBT
	TROLLF=.FALSE.				! forbid exits.
	ODESC1(TROLL)=436			! troll in.
	IF(QHERE(TROLL,HERE)) CALL RSPEAK(437)
	RETURN
C
1400	IF(PRSA.NE.FRSTQW) GO TO 1500		! first encounter?
	TROLLP=PROB(33,66)			! 33% true unless badlk.
	RETURN
C
1500	IF((PRSA.NE.MOVEW).AND.(PRSA.NE.TAKEW).AND.(PRSA.NE.MUNGW)
	1	.AND.(PRSA.NE.THROWW).AND.(PRSA.NE.GIVEW)) GO TO 2000
	IF(OCAPAC(TROLL).GE.0) GO TO 1550	! troll out?
	OCAPAC(TROLL)=IABS(OCAPAC(TROLL))	! yes, wake him.
	OFLAG1(AXE)=OFLAG1(AXE).OR.VISIBT
	TROLLF=.FALSE.				! forbid exits.
	ODESC1(TROLL)=436			! troll in.
	CALL RSPEAK(437)
C
1550	IF((PRSA.NE.TAKEW).AND.(PRSA.NE.MOVEW)) GO TO 1600
	CALL RSPEAK(438)			! joke.
	RETURN
C
1600	IF(PRSA.NE.MUNGW) GO TO 1700		! mung?
	CALL RSPEAK(439)			! joke.
	RETURN
C
1700	IF(PRSO.EQ.0) GO TO 10			! no object?
	I=440					! assume throw.
	IF(PRSA.EQ.GIVEW) I=441			! give?
	CALL RSPSUB(I,ODESC2(PRSO))		! troll takes.
	IF(PRSO.EQ.KNIFE) GO TO 1900		! obj knife?
	CALL NEWSTA(PRSO,442,0,0,0)		! no, eats it.
	RETURN
C
1900	CALL RSPEAK(443)			! knife, throws it back
	OFLAG2(TROLL)=OFLAG2(TROLL).OR.FITEBT	! and gets mad.
	RETURN
C
2000	IF(.NOT.TROLLF.OR.(PRSA.NE.HELLOW)) GO TO 10
	CALL RSPEAK(366)			! troll out.
	RETURN
C
10	TROLLP=.FALSE.				! couldnt handle it.
	RETURN
C
	END

C CYCLOP-	Cyclops function
C
C Declarations
C
	LOGICAL FUNCTION CYCLOP(ARG)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
C
	CYCLOP=.TRUE.				! assume wins.
	IF(.NOT.CYCLOF) GO TO 100		! asleep?
	IF((PRSA.NE.ALARMW).AND.(PRSA.NE.MUNGW).AND.(PRSA.NE.KICKW).AND.
	1	(PRSA.NE.BURNW).AND.(PRSA.NE.KILLW).AND.(PRSA.NE.ATTACW))
	2	 GO TO 10
	CYCLOF=.FALSE.				! wake cyclops.
	CALL RSPEAK(187)			! describe.
	RVCYC=IABS(RVCYC)
	OFLAG2(CYCLO)=OFLAG2(CYCLO).OR.FITEBT
	RETURN
C
100	IF(PRSA.NE.GIVEW) GO TO 500		! give?
	IF(PRSO.NE.FOOD) GO TO 300 		! food?
	IF(RVCYC.LT.0) GO TO 200		! already eaten?
	CALL NEWSTA(FOOD,189,0,0,0)		! eats peppers.
	RVCYC=MIN0(-1,-RVCYC)			! gets thirsty.
200	CFLAG(CEVCYC)=.TRUE.			! turn on cyclops timer.
	CTICK(CEVCYC)=-1
	RETURN
C
300	IF(PRSO.NE.WATER) GO TO 400		! drink when thirsty?
	IF(RVCYC.GE.0) GO TO 350
	CALL NEWSTA(PRSO,190,0,0,0)		! drinks and
	CYCLOF=.TRUE.				! falls asleep.
	OFLAG2(CYCLO)=OFLAG2(CYCLO).AND.(.NOT.FITEBT)
	CFLAG(CEVCYC)=.FALSE.			! turn off cyclops timer.
	RETURN
C
350	CALL RSPEAK(191)			! not thirsty.
10	CYCLOP=.FALSE.				! fails.
	RETURN
C
400	I=192					! assume inedible.
	IF(PRSO.EQ.GARLI) I=193			! garlic is joke.
450	CALL RSPEAK(I)				! disdain it.
	RETURN
C
500	IF((PRSA.NE.KILLW).AND.(PRSA.NE.ATTACW).AND.
	1  (PRSA.NE.MUNGW).AND.(PRSA.NE.THROWW)) GO TO 600
	CFLAG(CEVCYC)=.TRUE.			! turn on cyclops timer.
	CTICK(CEVCYC)=-1
	I=201					! assume not poke.
	IF(PRSA.EQ.MUNGW) I=912			! poke joke.
	GO TO 450				! go remark and return.
C
600	I=202					! assume take.
	IF(PRSA.EQ.TAKEW) GO TO 450
	I=203					! assume tie.
	IF(PRSA.EQ.TIEW) GO TO 450
	CYCLOP=.FALSE.
	RETURN
C
	END

C THIEFP-	Thief function
C
C Declarations
C
	LOGICAL FUNCTION THIEFP(ARG)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL QHERE,PROB,QEMPTY
C
	THIEFP=.TRUE.				! assume wins.
	IF(PRSA.NE.FIGHTW) GO TO 100		! fight?
	IF(OCAN(STILL).EQ.THIEF) GO TO 10	! got stilletto?  f.
	IF(QHERE(STILL,THFPOS)) GO TO 50	! can he recover it?
	CALL NEWSTA(THIEF,0,0,0,0)		! no, vanish.
	IF(QHERE(THIEF,HERE)) CALL RSPEAK(498)	! if hero, tell.
	RETURN
C
50	CALL NEWSTA(STILL,0,0,THIEF,0)		! yes, recover.
	IF(QHERE(THIEF,HERE)) CALL RSPEAK(499)	! if hero, tell.
	RETURN
C
100	IF(PRSA.NE.DEADXW) GO TO 200		! dead?
	THFACT=.FALSE.				! disable demon.
	IF(HERE.NE.TREAS) GO TO 150		! in treasure room?
	J=501
	DO 125 I=1,OLNT				! loop.
	  IF((I.EQ.CHALI).OR.(I.EQ.THIEF).OR..NOT.QHERE(I,HERE))
	1	GO TO 125			! is it here?
	  OFLAG1(I)=OFLAG1(I).OR.VISIBT		! rematerialize objects.
	  CALL RSPSUB(J,ODESC2(I))		! describe.
	  J=502
	  IF(.NOT.QEMPTY(I).AND.
	1   (((OFLAG1(I).AND.TRANBT).NE.0).OR.
	2    ((OFLAG2(I).AND.OPENBT).NE.0))) CALL PRINCO(I,573,.TRUE.)
125	CONTINUE
C
150	J=500
	DO 175 I=1,OLNT				! carrying anything?
	  IF(OADV(I).NE.-THIEF) GO TO 175
	  CALL NEWSTA(I,0,HERE,0,0)		! drop in room.
	  CALL RSPSUB(J,ODESC2(I))		! describe.
	  J=502
	  IF(.NOT.QEMPTY(I).AND.
	1   (((OFLAG1(I).AND.TRANBT).NE.0).OR.
	2    ((OFLAG2(I).AND.OPENBT).NE.0))) CALL PRINCO(I,573,.TRUE.)
175	CONTINUE
	RETURN
C
200	IF(PRSA.NE.FRSTQW) GO TO 250		! first encounter?
	THIEFP=PROB(20,75)
	RETURN
C
250	IF((PRSA.NE.HELLOW).OR.(OCAPAC(THIEF).GE.0)) GO TO 300
	CALL RSPEAK(626)			! hello to out thief?
	RETURN
C
300	IF(PRSA.NE.OUTXW) GO TO 400		! out?
	THFACT=.FALSE.				! disable demon.
	ODESC1(THIEF)=504			! change description.
	OFLAG1(STILL)=OFLAG1(STILL).AND..NOT.VISIBT
	RETURN
C
400	IF((PRSA.NE.INXW).AND.			! wake from fight demon?
	1  (((PRSA.NE.ALARMW).AND.(PRSA.NE.KICKW)).OR.
	2    (OCAPAC(THIEF).GE.0))) GO TO 500	! wake, kick while out?
	OCAPAC(THIEF)=IABS(OCAPAC(THIEF))	! wake him up.
	IF(QHERE(THIEF,HERE)) CALL RSPEAK(505)	! can hero see?
	THFACT=.TRUE.				! enable demon.
	ODESC1(THIEF)=503			! change description.
	OFLAG1(STILL)=OFLAG1(STILL).OR.VISIBT
	RETURN
C
500	IF(PRSA.NE.TAKEW) GO TO 600		! take?
	CALL RSPEAK(506)			! joke.
	RETURN
C
600	IF((PRSA.NE.THROWW).OR.(PRSO.NE.KNIFE).OR.
	1	((OFLAG2(THIEF).AND.FITEBT).NE.0)) GO TO 700
	IF(PROB(10,10)) GO TO 650		! threw knife, 10%?
	CALL RSPEAK(507)			! no, just makes
	OFLAG2(THIEF)=OFLAG2(THIEF).OR.FITEBT	! thief mad.
	RETURN
C
650	J=508					! thief drops stuff.
	DO 675 I=1,OLNT
	  IF(OADV(I).NE.-THIEF) GO TO 675	! thief carrying?
	  J=509
	  CALL NEWSTA(I,0,HERE,0,0)
675	CONTINUE
	CALL NEWSTA(THIEF,J,0,0,0)		! thief vanishes.
	RETURN
C
700	IF(((PRSA.NE.THROWW).AND.(PRSA.NE.GIVEW)).OR.(PRSO.EQ.0).OR.
	1	(PRSO.EQ.THIEF)) GO TO 10	! throw/give to thief?
	IF(OCAPAC(THIEF).GE.0) GO TO 750
	OCAPAC(THIEF)=IABS(OCAPAC(THIEF))	! wake him up.
	THFACT=.TRUE.				! enable demon.
	ODESC1(THIEF)=503			! change description.
	OFLAG1(STILL)=OFLAG1(STILL).OR.VISIBT
	CALL RSPEAK(510)
C
750	IF((PRSO.NE.BRICK).OR.(OCAN(FUSE).NE.BRICK).OR.
	1	(CTICK(CEVFUS).EQ.0)) GO TO 800
	CALL RSPEAK(511)			! thief refuses bomb.
	RETURN
C
800	IF(PRSO.NE.WATER) GO TO 850		! water?
	CALL NEWSTA(WATER,1081,0,0,0)		! slips through fingers.
	RETURN
C
850	CALL NEWSTA(PRSO,0,0,0,-THIEF)		! thief takes gift.
	IF(OTVAL(PRSO).GT.0) GO TO 900		! a treasure?
	CALL RSPSUB(512,ODESC2(PRSO))
	RETURN
C
900	CALL RSPSUB(627,ODESC2(PRSO))		! thief engrossed.
	THFENF=.TRUE.
	RETURN
C
10	THIEFP=.FALSE.
	RETURN
	END
