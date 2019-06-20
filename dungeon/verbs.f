C Verb processors for DUNGEON
C
C COPYRIGHT 1980, 1990, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA.
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK
C
C 20-Oct-94	RMS	Fixed bugs in INFLATE, FILL, collectives.
C 1-Oct-94	RMS	Fixed bugs in ANSWER, INCANT, UNTIE FROM, FILL,
C			GIVE, SHAKE, PLAY, POUR, POUR ON, TAKE, SPIN, BURN,
C			ALARM, collectives, subcripting.
C			Added COUNT, Puzzle Room entrance test.
C 11-Feb-94	RMS	Fixed bug in chimney load calculation.
C 30-Jan-94	RMS	Fixed bugs from MS DOS port.
C 27-Jan-94	RMS	Fixed portability problem in answer array.
C 25-Jan-94	RMS	Fixed bug in DIG not with shovel.
C 09-Jul-92	RMS	Fixed subscript range error.
C 01-Jul-92	RMS	Removed extraneous function from TAKE.
C 30-Jun-92	RMS	Changed file names to lower case.
C
C VAPPLI- Main verb processing routine
C
C Declarations
C
	LOGICAL FUNCTION VAPPLI(RI)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	PARAMETER (MXNOP=39,MXJOKE=64,MXSMP=99)
	PARAMETER (NUMANS=16)
	LOGICAL LIT,OBJACT,WASLIT
	LOGICAL QEMPTY,RMDESC,CLOCKD
	LOGICAL MOVETO,YESNO
	LOGICAL QOPEN,EDIBLE,DRKBLE
	LOGICAL TAKE,PUT,DROP,WALK
	LOGICAL QHERE,FINDXT,OAPPLI,F
	INTEGER JOKES(25),ANSWER(NUMANS)
	CHARACTER*12 ANSSTR(NUMANS)
	CHARACTER*(WRDLNT) PW(2),CH,CH2
C
C Functions and data
C
	QOPEN(R)=(OFLAG2(R).AND.OPENBT).NE.0
	EDIBLE(R)=(OFLAG1(R).AND.FOODBT).NE.0
	DRKBLE(R)=(OFLAG1(R).AND.DRNKBT).NE.0
	DATA JOKES/4,5,3,304,305,306,307,308,309,310,311,312,
	1	313,5314,5319,324,325,883,884,120,120,0,0,0,0/
	DATA ANSWER/0,1,2,2,3,4,4,4,4,4,5,5,5,6,7,7/
	DATA ANSSTR/'TEMPLE','FOREST','30003','30,003','FLASK','RUB',
	1	'FONDLE','CARESS','FEEL','TOUCH','BONES','BODY',
	2	'SKELETON','RUSTY KNIFE','NONE','NOWHERE'/

C VAPPLI, PAGE 2
C
	VAPPLI=.TRUE.				! assume wins.
	ODO2=0
	ODI2=0
	IF((PRSO.NE.0).AND.(PRSO.LE.OMAX)) ODO2=ODESC2(PRSO)
	IF(PRSI.NE.0) ODI2=ODESC2(PRSI)		! set up descriptors.
	AV=AVEHIC(WINNER)			! (PRSO can be direction.)
	RMK=372+RND(6)				! remark for hack-hacks.
	IF(RI.EQ.0) GO TO 10			! zero is false.
	IF(RI.LE.MXNOP) RETURN			! nop?
	IF(RI.LE.MXJOKE) GO TO 20		! joke?
	IF(RI.LE.MXSMP) GO TO 30		! simple verb?
	WASLIT=LIT(HERE)
	GO TO (
	1   500, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000,
	2 10000,11000,12000,13000,14000,15000,16000,17000,18000,19000,
	3 20000,21000,22000,23000,24000,25000,26000,27000,28000,29000,
	4 30000,31000,32000,33000,34000,35000,36000,37000,38000,39000,
	5 40000,41000,42000,43000,44000,45000,46000,47000,48000,49000,
	6 50000,51000,52000,53000,54000,55000,56000,57000,58000,59000,
	7 60000,61000,62000,63000,64000,65000,66000,67000,68000,69000),
	9	(RI-MXSMP)
	CALL BUG(7,RI)
C
C All verb processors return here to declare failure.
C
10	VAPPLI=.FALSE.				! lose.
	RETURN
C
C Joke processor.
C Find proper entry in jokes, use it to select string to print.
C
20	I=JOKES(RI-MXNOP)			! get table entry.
	J=I/1000				! isolate # strings.
	IF(J.NE.0) I=MOD(I,1000)+RND(J)		! if random, choose.
	CALL RSPEAK(I)				! print joke.
	RETURN
C
C 'Simple' verbs.
C
30	GO TO (65001,66001,67001,68001,69001,
	1 70000,71000,72000,73000,74000,75000,76000,77000,78000,79000,
	2 80000,81000,82000,83000,84000,85000,86000,87000,88000,89000,
	3 90000,91000,92000,93000,94000,95000,96000,97000),
	9	(RI-MXJOKE)
	CALL BUG(7,RI)
C
C Here to test for change in light.
C
50	IF(WASLIT.AND..NOT.LIT(HERE)) CALL RSPEAK(406)
	RETURN

C VAPPLI, PAGE 3
C
C V100--	Read
C
500	IF(LIT(HERE)) GO TO 600			! room lit?
	CALL RSPEAK(356)			! no, cant read.
	RETURN
C
600	IF(PRSI.EQ.0) GO TO 700			! read through obj?
	IF((OFLAG1(PRSI).AND.TRANBT).NE.0) GO TO 700
	CALL RSPSUB(357,ODI2)			! not transparent.
	RETURN
C
700	IF(OBJACT(X)) RETURN			! object handle?
	IF((OFLAG1(PRSO).AND.READBT).NE.0) GO TO 800
	CALL RSPSUB(358,ODO2)			! not readable.
	RETURN
C
800	CALL RSPEAK(OREAD(PRSO))		! print reading material.
	RETURN
C
C V101--	Melt
C
1000	IF(.NOT.OBJACT(X)) CALL RSPSUB(361,ODO2)
	RETURN
C
C V102--	Inflate
C
2000	IF((OFLAG1(PRSI).AND.TOOLBT).NE.0) GO TO 2100	! tool?
	CALL RSPSUB(303,ODI2)			! no, joke.
	RETURN
C
2100	IF(.NOT.OBJACT(X)) CALL RSPEAK(368)	! obj handle?
	RETURN
C
C V103--	Deflate.
C
3000	IF(.NOT.OBJACT(X)) CALL RSPEAK(369)	! obj handle?
	RETURN

C VAPPLI, PAGE 4
C
C V104--	Alarm
C
4000	IF((OFLAG2(PRSO).AND.VILLBT).NE.0) GO TO 4100	! villain?
	CALL RSPEAK(552+RND(6))			! no, joke.
	RETURN
C
4100	IF(.NOT.OBJACT(X)) CALL RSPSUB(370,ODO2)
	RETURN
C
C V105--	Exorcise
C
5000	F=OBJACT(X)				! objects handle.
	RETURN
C
C V106--	Plug
C
6000	IF(.NOT.OBJACT(X)) CALL RSPEAK(371)
	RETURN
C
C V107--	Kick
C
7000	IF(.NOT.OBJACT(X)) CALL RSPSB2(378,ODO2,RMK)
	RETURN
C
C V108--	Wave
C
8000	IF(.NOT.OBJACT(X)) CALL RSPSB2(379,ODO2,RMK)
	RETURN
C
C V109,V110--	Raise, lower
C
9000	CONTINUE
10000	IF(.NOT.OBJACT(X)) CALL RSPSB2(380,ODO2,RMK)
	RETURN
C
C V111--	Rub
C
11000	IF(.NOT.OBJACT(X)) CALL RSPSB2(381,ODO2,RMK)
	RETURN
C
C V112--	Push
C
12000	IF(.NOT.OBJACT(X)) CALL RSPSB2(382,ODO2,RMK)
	RETURN

C VAPPLI, PAGE 5
C
C V113--	Untie
C
13000	IF(OBJACT(X)) RETURN			! object handle?
	I=383					! no, not tied.
	IF((OFLAG2(PRSO).AND.TIEBT).EQ.0) I=384	! not tieable.
	CALL RSPEAK(I)
	RETURN
C
C V114--	Tie
C
14000	IF((OFLAG2(PRSO).AND.TIEBT).NE.0) GO TO 14100
	CALL RSPEAK(385)			! not tieable.
	RETURN
C
14100	IF(OBJACT(X)) RETURN			! object handle?
	I=386					! joke 1.
	IF(PRSI.EQ.OPLAY) I=919			! if player, joke 2.
	CALL RSPSUB(386,ODO2)			! print remark.
	RETURN
C
C V115--	Tie up
C
15000	IF((OFLAG2(PRSI).AND.TIEBT).NE.0) GO TO 15100
	CALL RSPSUB(387,ODO2)			! not tieable.
	RETURN
C
15100	I=388					! assume villain.
	IF((OFLAG2(PRSO).AND.VILLBT).EQ.0) I=389
	CALL RSPSUB(I,ODO2)			! joke.
	RETURN
C
C V116--	Turn
C
16000	IF((OFLAG1(PRSO).AND.TURNBT).NE.0) GO TO 16100
	CALL RSPEAK(390)			! not turnable.
	RETURN
C
16100	IF((OFLAG1(PRSI).AND.TOOLBT).NE.0) GO TO 16200
	CALL RSPSUB(391,ODI2)			! not a tool.
	RETURN
C
16200	VAPPLI=OBJACT(X)			! let object handle.
	RETURN
C
C V117--	Breathe
C
17000	PRSA=INFLAW
	PRSI=LUNGS
	GO TO 2000				! handle like inflate.

C VAPPLI, PAGE 5A.
C
C V118--	Knock
C
18000	IF(OBJACT(X)) RETURN			! obj handle?
	I=394					! joke for door.
	IF((OFLAG1(PRSO).AND.DOORBT).EQ.0) I=395
	CALL RSPSUB(I,ODO2)			! joke for nondoors too.
	RETURN
C
C V119--	Look
C
19000	IF(PRSO.NE.0) GO TO 20500		! something to look at?
	VAPPLI=RMDESC(3)			! handled by rmdesc.
	RETURN
C
C V120--	Examine
C
20000	IF(PRSO.NE.0) GO TO 20500		! something to examine?
	VAPPLI=RMDESC(0)			! handled by rmdesc.
	RETURN
C
20500	IF(OBJACT(X)) RETURN			! obj handle?
	I=OREAD(PRSO)				! get reading material.
	IF(I.NE.0) CALL RSPEAK(I)		! output if there,
	IF(I.EQ.0) CALL RSPSUB(429,ODO2)	! otherwise default.
	PRSA=FOOW				! defuse room processors.
	RETURN
C
C V121--	Shake
C
21000	IF(OBJACT(X)) RETURN			! object handle?
	IF(PRSO.EQ.GWATE) GO TO 10		! global water? fails.
	IF((OFLAG2(PRSO).AND.VILLBT).EQ.0) GO TO 21100
	CALL RSPEAK(371)			! joke for villains.
	RETURN
C
21100	IF(PRSO.NE.AV) GO TO 21150		! vehicle?
	CALL RSPEAK(672)			! yes, joke.
	RETURN
C
21150	IF((OFLAG1(PRSO).AND.TAKEBT).NE.0) GO TO 21200	! takeable?
	CALL RSPEAK(923)			! no, joke.
	RETURN
C
21200	IF(OADV(PRSO).EQ.WINNER) GO TO 21250	! carrying?
	CALL RSPEAK(527)			! no, joke.
	RETURN
C
21250	IF(QEMPTY(PRSO)) GO TO 10		! empty?  fails.
	IF(QOPEN(PRSO)) GO TO 21300		! open?  spill.
	CALL RSPSUB(396,ODO2)			! no, describe noise.
	RETURN
C
21300	CALL RSPSUB(397,ODO2)			! spill the works.
	DO 21500 I=1,OLNT			! spill contents.
	  IF(OCAN(I).NE.PRSO) GO TO 21500	! inside?
	  OFLAG2(I)=OFLAG2(I).OR.TCHBT		! touch object.
	  IF(AV.EQ.0) GO TO 21400		! in vehicle?
	  CALL NEWSTA(I,0,0,AV,0)		! yes, spill in there.
	  GO TO 21500
C
21400	  CALL NEWSTA(I,0,HERE,0,0)		! no, spill on floor,
	  IF(I.EQ.WATER) CALL NEWSTA(I,133,0,0,0)	! but water disappears.
21500	CONTINUE
	RETURN

C VAPPLI, PAGE 6
C
C V122--	Move
C
22000	IF(.NOT.QHERE(PRSO,HERE)) GO TO 22100	! is it here?
	IF(OBJACT(X)) RETURN			! obj handle?
	I=399					! assume not takeable.
	IF((OFLAG1(PRSO).AND.TAKEBT).NE.0) I=887
	CALL RSPSUB(I,ODO2)			! joke.
	RETURN
C
22100	CALL RSPEAK(398)			! not here.
	RETURN
C
C V123--	Turn on
C
23000	IF(OBJACT(X)) RETURN			! object handle?
	IF(((OFLAG1(PRSO).AND.LITEBT).NE.0).AND.
	1	(OADV(PRSO).EQ.WINNER)) GO TO 23100
	CALL RSPEAK(400)			! cant do it.
	RETURN
C
23100	IF((OFLAG1(PRSO).AND.ONBT).EQ.0) GO TO 23200
	CALL RSPEAK(401)			! already on.
	RETURN
C
23200	OFLAG1(PRSO)=OFLAG1(PRSO).OR.ONBT	! turn it on.
	CALL RSPSUB(404,ODO2)
	RETURN
C
C V124--	Turn off
C
24000	IF(OBJACT(X)) RETURN			! obj handle?
	IF(((OFLAG1(PRSO).AND.LITEBT).NE.0).AND.
	1	(OADV(PRSO).EQ.WINNER)) GO TO 24100
	CALL RSPEAK(402)			! cant do it.
	RETURN
C
24100	IF((OFLAG1(PRSO).AND.ONBT).NE.0) GO TO 24200
	CALL RSPEAK(403)			! already off.
	RETURN
C
24200	OFLAG1(PRSO)=OFLAG1(PRSO).AND. .NOT.ONBT
	CALL RSPSUB(405,ODO2)
	GO TO 50				! go see if now dark.
C
C V125--	Open
C
25000	IF(OBJACT(X)) RETURN			! obj handle?
	IF((OFLAG1(PRSO).AND.DOORBT).NE.0) GO TO 25150	! door?
	IF((OFLAG1(PRSO).AND.CONTBT).NE.0) GO TO 25100	! container?
25050	CALL RSPSUB(407,ODO2)			! not door or container.
	RETURN
C
25100	IF(OCAPAC(PRSO).NE.0) GO TO 25200	! openable container?
25150	CALL RSPSUB(408,ODO2)			! door or not openable.
	RETURN
C
25200	IF(.NOT.QOPEN(PRSO)) GO TO 25225
	CALL RSPEAK(412)			! already open.
	RETURN
C
25225	OFLAG2(PRSO)=OFLAG2(PRSO).OR.OPENBT	! open it.
	IF(((OFLAG1(PRSO).AND.TRANBT).NE.0).OR.QEMPTY(PRSO))
	1	GO TO 25300
	CALL PRINCO(PRSO,410,.FALSE.)		! print contents.
	RETURN
C
25300	CALL RSPEAK(409)			! done
	RETURN
C
C V126--	Close
C
26000	IF(OBJACT(X)) RETURN			! obj handle?
	IF((OFLAG1(PRSO).AND.DOORBT).NE.0) GO TO 26050	! door?
	IF((OFLAG1(PRSO).AND.CONTBT).EQ.0) GO TO 25050	! container?
	IF(OCAPAC(PRSO).NE.0) GO TO 26100	! closable container?
26050	CALL RSPSUB(411,ODO2)			! door or not closable.
	RETURN
C
26100	IF(QOPEN(PRSO)) GO TO 26200		! open?
	CALL RSPEAK(413)			! no, joke.
	RETURN
C
26200	OFLAG2(PRSO)=OFLAG2(PRSO).AND. .NOT.OPENBT
	CALL RSPEAK(414)			! done.
	GO TO 50				! go see if now dark.

C VAPPLI, PAGE 7
C
C V127--	Find
C
27000	IF(OBJACT(X)) RETURN			! obj handle?
	I=415					! room string.
	IF(QHERE(PRSO,HERE)) GO TO 27200	! in room?
	I=416					! winner string.
	IF(OADV(PRSO).EQ.WINNER) GO TO 27200	! on winner?
	J=OCAN(PRSO)				! down one level.
	IF(J.EQ.0) GO TO 10
	IF((((OFLAG1(J).AND.TRANBT).EQ.0).AND.
	2 (.NOT.QOPEN(J).OR.((OFLAG1(J).AND.(DOORBT+CONTBT)).EQ.0))))
	3	GO TO 10			! if not visible, punt.
	I=417					! assume in room.
	IF(QHERE(J,HERE)) GO TO 27100
	I=418					! assume on winner.
	IF(OADV(J).NE.WINNER) GO TO 10		! not here or on person.
27100	CALL RSPSUB(I,ODESC2(J))		! describe findings.
	RETURN
C
27200	CALL RSPSUB(I,ODO2)			! describe findings.
	RETURN
C
C V128--	Wait
C
28000	CALL RSPEAK(419)			! time passes.
	DO 28100 I=1,3
	  IF(CLOCKD(X)) RETURN
	  IF(.NOT.DEADF) CALL FIGHTD
	  IF(PRSCON.EQ.0) RETURN		! fighting happened.
28100	CONTINUE
	RETURN
C
C V129--	Spin
C V159--	Turn to
C
29000	CONTINUE
59000	IF(OBJACT(X)) RETURN			! object handle?
	IF((OFLAG1(PRSO).AND.TURNBT).NE.0) GO TO 59100
	CALL RSPEAK(390)			! can't turn?
	RETURN
C
59100	CALL RSPSUB(663,ODO2)			! won't budge.
	RETURN
C
C V130--	Board
C
30000	IF((OFLAG2(PRSO).AND.VEHBT).NE.0) GO TO 30100
	CALL RSPSUB(421,ODO2)			! not vehicle, joke.
	PRSCON=0				! kill cmd stream.
	RETURN
C
30100	IF(QHERE(PRSO,HERE)) GO TO 30200	! here?
	CALL RSPSUB(420,ODO2)			! no, joke.
	PRSCON=0				! kill cmd stream.
	RETURN
C
30200	IF(AV.EQ.0) GO TO 30300			! already got one?
	CALL RSPSUB(422,ODO2)			! yes, joke.
	PRSCON=0				! kill cmd stream.
	RETURN
C
30300	IF(OBJACT(X)) RETURN			! obj handle?
	CALL RSPSUB(423,ODO2)			! describe.
	AVEHIC(WINNER)=PRSO
	IF(WINNER.NE.PLAYER) CALL NEWSTA(AOBJ(WINNER),0,0,PRSO,0)
	RETURN
C
C V131--	Disembark
C
31000	IF(AV.EQ.PRSO) GO TO 31100		! from vehicle?
	CALL RSPEAK(424)			! no, joke.
	PRSCON=0				! kill cmd stream.
	RETURN
C
31100	IF(OBJACT(X)) RETURN			! obj handle?
	IF((RFLAG(HERE).AND.RLAND).NE.0) GO TO 31200
	CALL RSPEAK(425)			! not on land.
	PRSCON=0				! kill cmd stream.
	RETURN
C
31200	AVEHIC(WINNER)=0
	CALL RSPEAK(426)
	IF(WINNER.NE.PLAYER) CALL NEWSTA(AOBJ(WINNER),0,HERE,0,0)
	RETURN
C
C V132--	Take
C
32000	VAPPLI=TAKE(.TRUE.)
	RETURN
C
C V133--	Inventory
C
33000	CALL INVENT(WINNER)
	RETURN

C VAPPLI, PAGE 8
C
C V134--	Fill
C
34000	IF(PRSI.NE.0) GO TO 34050		! any obj specified?
	IF((RFLAG(HERE).AND.(RWATER+RFILL)).NE.0) GO TO 34025
	CALL RSPEAK(516)			! nothing to fill with.
	PRSWON=.FALSE.				! you lose.
	CALL ORPHAN(-1,ACT,PREP1,PRSO,2,' ',0,0)! orphan "fill obj with"
	GO TO 10				! return false.
C
34025	PRSI=GWATE				! use global water.
34050	PRSA=PUTW
	I=PRSO					! fill x with y becomes
	PRSO=PRSI				! put y in x
	PRSI=I
	VAPPLI=PUT(.TRUE.)
	RETURN
C
C V135,V136--	Eat/Drink
C
35000	CONTINUE
36000	IF(OBJACT(X)) RETURN			! obj handle?
	IF(PRSO.EQ.GWATE) GO TO 36500		! drink global water?
	IF(.NOT.EDIBLE(PRSO)) GO TO 36400	! edible?
	IF(OADV(PRSO).EQ.WINNER) GO TO 36200	! yes, on winner?
36100	CALL RSPSUB(454,ODO2)			! not accessible.
	RETURN
C
36200	IF(PRSA.EQ.DRINKW) GO TO 36300		! drink food?
	CALL NEWSTA(PRSO,455,0,0,0)		! no, it disappears.
	RETURN
C
36300	CALL RSPEAK(456)			! yes, joke.
	RETURN
C
36400	IF(.NOT.DRKBLE(PRSO)) GO TO 36600	! drinkable?
	IF(OCAN(PRSO).EQ.0) GO TO 36100		! yes, in something?
	IF(OADV(OCAN(PRSO)).NE.WINNER) GO TO 36100
	IF(QOPEN(OCAN(PRSO))) GO TO 36500	! cont open?
	CALL RSPEAK(457)			! no, joke.
	RETURN
C
36500	CALL NEWSTA(PRSO,458,0,0,0)		! gone.
	RETURN
C
36600	CALL RSPSUB(453,ODO2)			! not food or drink.
	RETURN
C
C V137--	Burn
C
37000	IF((OFLAG1(PRSI).AND.(FLAMBT+LITEBT+ONBT)).NE.
	1	(FLAMBT+LITEBT+ONBT)) GO TO 37600	! with flame?
	IF(OBJACT(X)) RETURN			! obj handle?
	IF((OFLAG1(PRSO).AND.BURNBT).EQ.0) GO TO 37500	! burnable?
	IF(OADV(PRSO).EQ.WINNER) GO TO 37400	! carrying it?
	IF(QHERE(PRSO,HERE)) GO TO 37200	! here?
	J=OCAN(PRSO)				! get container.
	IF(J.EQ.0) GO TO 37100			! is there one?
	IF(.NOT.QOPEN(J)) GO TO 37100		! open?
	IF(J.EQ.RECEP) GO TO 37300		! in balloon receptacle?
	IF(OADV(J).EQ.WINNER) GO TO 37400	! carrying?
	IF(QHERE(J,HERE)) GO TO 37200		! here?
37100	CALL RSPEAK(461)			! cant reach it.
	RETURN
C
37200	CALL RSPSUB(462,ODO2)			! burn it.
	CALL NEWSTA(PRSO,0,0,0,0)
	RETURN
C
37300	VAPPLI=OAPPLI(OACTIO(BALLO),0)		! fire in receptacle.
	RETURN
C
37400	CALL RSPSUB(459,ODO2)			! burnable on player.
	CALL JIGSUP(460)
	RETURN
C
37500	CALL RSPSUB(463,ODO2)			! cant burn it.
	RETURN
C
37600	CALL RSPSUB(301,ODI2)			! cant burn it with that.
	RETURN

C VAPPLI, PAGE 9
C
C V138--	Mung
C
38000	I=466					! choose phrase.
	IF((OFLAG2(PRSO).AND.VILLBT).NE.0) GO TO 41100
	IF(.NOT.OBJACT(X)) CALL RSPSB2(466,ODO2,RMK)
	RETURN
C
C V139--	Kill
C
39000	I=467					! choose phrase.
	GO TO 41100
C
C V140--	Swing
C
40000	J=PRSO					! invert.
	PRSO=PRSI
	PRSI=J
	J=ODO2
	ODO2=ODI2
	ODI2=J
	PRSA=ATTACW				! for objact.
C
C V141--	Attack
C
41000	I=468
C
C Common mung/attack/swing/kill code.
C
41100	IF(PRSO.NE.0) GO TO 41200		! anything?
	CALL RSPEAK(469)			! no, joke.
	RETURN
C
41200	IF(OBJACT(X)) RETURN			! obj handle?
	IF((OFLAG2(PRSO).AND.VILLBT).NE.0) GO TO 41300
	IF((OFLAG1(PRSO).AND.VICTBT).EQ.0)
	1	CALL RSPSUB(470,ODO2)		! not a villain.
	RETURN
C
41300	J=471					! assume no weapon.
	IF(PRSI.EQ.0) GO TO 41500
	IF((OFLAG2(PRSI).AND.WEAPBT).EQ.0) GO TO 41400
	MELEE=1					! assume sword.
	IF(PRSI.NE.SWORD) MELEE=2		! must be knife.
	I=BLOW(PLAYER,PRSO,MELEE,.TRUE.,0)	! strike blow.
	RETURN
C
41400	J=472					! not a weapon.
41500	CALL RSPSB2(I,ODO2,J)			! joke.
	RETURN

C VAPPLI, PAGE 10
C
C V142--	Walk
C
42000	VAPPLI=WALK(X)
	RETURN
C
C V143--	Tell
C
43000	CALL RSPEAK(603)
	PRSCON=0				! kill cmd stream.
	RETURN
C
C V144--	Put
C
44000	VAPPLI=PUT(.TRUE.)
	RETURN
C
C V145,V148--	Drop/Throw
C
45000	CONTINUE				! throw.
48000	VAPPLI=DROP(.TRUE.)			! drop.
	RETURN
C
C V146--	Give
C
46000	IF(OADV(PRSO).EQ.WINNER) GO TO 46100	! is he carrying obj?
	X=OCAN(PRSO)				! get container.
	IF(X.EQ.0) GO TO 46200			! container?
	IF(.NOT.QOPEN(X)) GO TO 46200		! open?
	IF(OADV(X).NE.WINNER) GO TO 46200	! carrying?
46100	VAPPLI=OBJACT(X)			! iobject must handle.
	RETURN
C
46200	CALL RSPEAK(527)			! don't have it.
	RETURN
C
C V147--	Pour
C
47000	IF(PRSO.EQ.WATER) GO TO 47100		! pour? must be water.
	CALL RSPEAK(1075)			! otherwise, joke.
	RETURN
C
47100	IF(PRSI.EQ.0) GO TO 48000		! pour x, treat like drop.
	PRSA=PUTW				! else, treat like put.
	VAPPLI=PUT(.TRUE.)
	RETURN
C
C V149--	Save
C
49000	IF((RFLAG(TSTRS).AND.RSEEN).EQ.0) GO TO 49100
	CALL RSPEAK(828)			! no saves in endgame.
	RETURN
C
49100	CALL SAVEGM				! save game.
	PRSWON=.FALSE.				! disable rest of move.
	PRSCON=0
	RETURN
C
C V150--	Restore
C
50000	IF((RFLAG(TSTRS).AND.RSEEN).EQ.0) GO TO 50100
	CALL RSPEAK(829)			! no restores in endgame.
	RETURN
C
50100	CALL RSTRGM				! restore game.
	PRSWON=.FALSE.				! disable rest of move.
	PRSCON=0
	RETURN

C VAPPLI, PAGE 11
C
C V151--	Hello
C
51000	IF(PRSO.NE.0) GO TO 51100		! any obj?
	CALL RSPEAK(346+RND(4))			! no, vanilla hello.
	RETURN
C
51100	IF(PRSO.NE.AVIAT) GO TO 51200		! hello aviator?
	CALL RSPEAK(350)			! nothing happens.
	RETURN
C
51200	IF(PRSO.NE.SAILO) GO TO 51300		! hello sailor?
	HS=HS+1					! count.
	I=351					! give normal or
	IF(MOD(HS,10).EQ.0) I=352		! random message.
	IF(MOD(HS,20).EQ.0) I=353
	CALL RSPEAK(I)				! speak up.
	RETURN
C
51300	IF(OBJACT(X)) RETURN			! obj handle?
	I=354					! assume villain.
	IF((OFLAG2(PRSO).AND.(VILLBT+ACTRBT)).EQ.0) I=355
	CALL RSPSUB(I,ODO2)			! hello there!
	RETURN
C
C V152--	Look into
C
52000	IF(OBJACT(X)) RETURN			! obj handle?
	IF((OFLAG1(PRSO).AND.DOORBT).EQ.0) GO TO 52300	! door?
	IF(.NOT.QOPEN(PRSO)) GO TO 52200	! open?
	CALL RSPSUB(628,ODO2)			! open door- uninteresting.
	RETURN
C
52200	CALL RSPSUB(525,ODO2)			! closed door- cant see.
	RETURN
C
52300	IF((OFLAG1(PRSO).AND.CONTBT).NE.0) GO TO 52500	! container?
	IF(QOPEN(PRSO)) GO TO 52400		! open anyway?
	CALL RSPSUB(630,ODO2)			! cant look inside.
	RETURN
C
52400	IF(.NOT.QEMPTY(PRSO)) GO TO 52700	! not empty?
	CALL RSPSUB(1054,ODO2)			! doesn't contain anything.
	RETURN
C
52500	IF(QOPEN(PRSO).OR.((OFLAG1(PRSO).AND.TRANBT).NE.0))
	1	GO TO 52600			! open or see inside?
	CALL RSPSUB(525,ODO2)			! closed.
	RETURN
C
52600	IF(QEMPTY(PRSO)) GO TO 52800		! see in.  empty?
52700	CALL PRINCO(PRSO,573,.TRUE.)		! no, list contents.
	RETURN
C
52800	CALL RSPSUB(629,ODO2)			! empty.
	RETURN

C VAPPLI, PAGE 12
C
C V153--	Look under
C
53000	IF(.NOT.OBJACT(X)) CALL RSPEAK(631)	! object handle?
	RETURN
C
C V154--	Pump
C
54000	IF((OROOM(PUMP).EQ.HERE).OR.(OADV(PUMP).EQ.WINNER))
	1	GO TO 54100			! pump here?
	CALL RSPEAK(632)			! no.
	RETURN
C
54100	PRSI=PUMP				! becomes inflate
	PRSA=INFLAW				! x with pump.
	GO TO 2000				! done.
C
C V155--	Wind
C
55000	IF(.NOT.OBJACT(X)) CALL RSPSUB(634,ODO2)	! obj handle?
	RETURN
C
C V156--	Climb
C V157--	Climb up
C V158--	Climb down
C
56000	CONTINUE
57000	CONTINUE
58000	IF(OBJACT(X)) RETURN			! object handle?
	I=XUP					! assume up.
	IF(PRSA.EQ.CLMBDW) I=XDOWN		! unless climb dn.
	F=(OFLAG2(PRSO).AND.CLMBBT).NE.0
	IF(F.AND.FINDXT(I,HERE)) GO TO 58500	! anything to climb?
	I=657
	IF(F) I=524				! variety of jokes.
	IF(.NOT.F .AND.((PRSO.EQ.WALL).OR.(PRSO.EQ.GRWAL).OR.
	1	((PRSO.GE.WNORT).AND.(PRSO.LE.WNORT+3))))
	2	I=656				! if walls.
	CALL RSPEAK(I)				! joke.
	RETURN
C
58500	PRSA=WALKW				! walk
	PRSO=I					! in specified dir.
	GO TO 42000				! treat as walk.

C VAPPLI, PAGE 13
C
C V160--	Pour on
C
60000	IF(PRSO.NE.WATER) GO TO 60500		! pour water?
	IF(OBJACT(X)) RETURN			! object handle?
	CALL NEWSTA(WATER,0,0,0,0)		! vanish water.
	IF(OCAN(PRSI).NE.RECEP) GO TO 60100	! onto obj in receptacle?
	CALL RSPSUB(977,ODI2)			! doesn't work.
	RETURN
C
60100	IF((OFLAG1(PRSI).AND.(LITEBT+FLAMBT+ONBT)).NE.
	1	(LITEBT+FLAMBT+ONBT)) GO TO 60300	! on flame?
	IF(PRSI.NE.TORCH) GO TO 60200		! on torch?
	CALL RSPEAK(978)			! doesn't work.
	RETURN
C
60200	OFLAG1(PRSI)=OFLAG1(PRSI).AND..NOT.ONBT	! extinguish.
	CALL RSPSUB(979,ODI2)			! describe.
	IF(PRSI.EQ.CANDL) CFLAG(CEVCND)=.FALSE.	! if candle, disable timer.
	IF(PRSI.EQ.MATCH) CTICK(CEVMAT)=0	! if match, gone.
	GO TO 50				! go see if now dark.
C
60300	CALL RSPSUB(980,ODI2)			! doesn't work.
	RETURN
C
60500	CALL RSPEAK(981)			! not water, joke.
	RETURN
C
C V161--	Put under
C
61000	IF(OBJACT(X)) RETURN			! object handle.
	I=1037					! can't do.
	IF((OFLAG1(PRSO).AND.DOORBT).NE.0) I=982	! if door, won't fit.
	CALL RSPEAK(I)
	RETURN
C
C V162--	Untie from
C
62000	IF(((PRSO.EQ.BROPE).AND.(BTIEF.NE.0)).OR.
	1  ((PRSO.EQ.ROPE).AND.(TTIE.EQ.PRSI)).OR.
	2  ((PRSO.EQ.ROPE).AND.(PRSI.EQ.RAILI).AND.DOMEF))
	3  GO TO 62100				! untie rope?
	CALL RSPEAK(1070)			! not attached to that.
	RETURN
C
62100	PRSA=UNTIEW				! treat as normal untie.
	GO TO 13000

C VAPPLI, PAGE 14
C
C V163--	Make
C V169--	Wish
C
63000	IF(PRSO.NE.GWISH) GO TO 10		! make a wish?
69000	IF(HERE.NE.BWELL) GO TO 63100		! at well?
	IF(OROOM(BAGCO).EQ.HERE) GO TO 63200	! coins here?
	IF(OROOM(ZORKM).EQ.HERE) GO TO 63300	! zorkmid here?
63100	CALL RSPEAK(937)			! doesn't work.
	RETURN
C
63200	CALL NEWSTA(BAGCO,938,0,0,0)		! vanish coins
	RETURN
C
63300	CALL NEWSTA(ZORKM,938,0,0,0)		! vanish zorkmid.
	RETURN
C
C V164--	Oil
C
64000	IF(PRSI.NE.PUTTY) GO TO 64100		! with putty?
	IF(OBJACT(X)) RETURN			! object handle?
	CALL RSPEAK(904)			! doesn't work.
	RETURN
C
64100	CALL RSPEAK(905)			! joke.
	RETURN
C
C V165--	Play
C
65000	IF(PRSO.NE.STRAD) GO TO 65200		! play violin?
	IF(PRSI.EQ.0) GO TO 65100		! with anything?
	IF((OFLAG2(PRSI).AND.WEAPBT).EQ.0) GO TO 65100
	OTVAL(STRAD)=0				! with weapon, ruined.
	CALL RSPEAK(933)
	RETURN
C
65100	CALL RSPEAK(934)			! offensive noise.
	RETURN
C
65200	IF((OFLAG2(PRSO).AND.VILLBT).EQ.0) GO TO 10	! play villain?
	CALL RSPSUB(935,ODO2)			! you're dead.
	CALL JIGSUP(0)
	RETURN
C
C V166--	Send
C
66000	IF(OBJACT(X)) RETURN			! object handle?
	I=940					! can't do it.
	IF((OFLAG2(PRSO).AND.VILLBT).NE.0) I=941 ! why do it?
	CALL RSPSUB(I,ODO2)
	RETURN

C VAPPLI, PAGE 15
C
C V167--	Enter
C
67000	PRSA=WALKW				! treat as walk.
	PRSO=XENTER
	GO TO 42000
C
C V168--	Leave
C
68000	PRSA=WALKW				! treat as walk.
	PRSO=XEXIT
	GO TO 42000
C
C V65--	Room
C
65001	VAPPLI=RMDESC(2)			! describe room only.
	RETURN
C
C V66--	Objects
C
66001	VAPPLI=RMDESC(1)			! describe obj only.
	IF(.NOT.TELFLG) CALL RSPEAK(138)	! no objects.
	RETURN
C
C V67--	Rname
C
67001	CALL RSPEAK(RDESC2-HERE)		! short room name.
	RETURN
C
C V68--	Squeeze
C
68001	IF(OBJACT(X)) RETURN			! object handle?
	I=901					! can't.
	IF((OFLAG2(PRSO).AND.VILLBT).NE.0) I=902 ! don't understand.
	CALL RSPSUB(I,ODO2)
	RETURN
C
C V69--	Smell
C
69001	CALL RSPSUB(903,ODO2)			! joke.
	RETURN

C VAPPLI, PAGE 16
C
C V70--	Brief
C
70000	BRIEFF=.TRUE.				! brief descriptions.
	SUPERF=.FALSE.
	CALL RSPEAK(326)
	RETURN
C
C V71--	Verbose
C
71000	BRIEFF=.FALSE.				! long descriptions.
	SUPERF=.FALSE.
	CALL RSPEAK(327)
	RETURN
C
C V72--	Superbrief
C
72000	SUPERF=.TRUE.
	CALL RSPEAK(328)
	RETURN
C
C V73-- Stay (used in endgame)
C
73000	IF(WINNER.NE.AMASTR) GO TO 73100	! tell master, stay.
	CALL RSPEAK(781)			! he does.
	CTICK(CEVFOL)=0				! not following.
	RETURN
C
73100	IF(WINNER.EQ.PLAYER) CALL RSPEAK(664)	! joke.
	RETURN
C
C V74--	Version
C
74000	WRITE(OUTCH,74010) VMAJ,VMIN,VEDIT
74010	FORMAT(' V',I1,'.',I1,A)
	TELFLG=.TRUE.
	RETURN
C
C V75--	Swim
C
75000	I=330					! assume water.
	IF((RFLAG(HERE).AND.(RWATER+RFILL)).EQ.0)
	1	I=331+RND(3)			! if no water, joke.
	CALL RSPEAK(I)
	RETURN
C
C V76--	Geronimo
C
76000	IF(AVEHIC(WINNER).EQ.BARRE) GO TO 76100	! in barrel?
	CALL RSPEAK(334)			! no, joke.
	RETURN
C
76100	CALL JIGSUP(335)			! over falls.
	RETURN
C
C V77--	Sinbad et al
C
77000	IF((HERE.EQ.MCYCL).AND.(OROOM(CYCLO).EQ.HERE)) GO TO 77100
	CALL RSPEAK(336)			! not here, joke.
	RETURN
C
77100	CALL NEWSTA(CYCLO,337,0,0,0)		! cyclops flees.
	CYCLOF=.TRUE.				! set all flags.
	MAGICF=.TRUE.
	OFLAG2(CYCLO)=OFLAG2(CYCLO).AND. .NOT.FITEBT
	RETURN
C
C V78--	unused
C
78000	GO TO 10
C
C V79--	Pray
c
79000	IF(HERE.NE.TEMP2) GO TO 79050		! in temple?
	IF(MOVETO(FORE1,WINNER)) GO TO 79100	! fore1 still there?
79050	CALL RSPEAK(340)			! joke.
	RETURN
C
79100	F=RMDESC(3)				! moved, describe.
	RETURN
C
C V80--	Treasure
c
80000	IF(HERE.NE.TEMP1) GO TO 80050		! in temple?
	IF(MOVETO(TREAS,WINNER)) GO TO 79100	! treasure room there?
80050	CALL RSPEAK(341)			! nothing happens.
	RETURN
C
C V81--	Temple
c
81000	IF(HERE.NE.TREAS) GO TO 81050		! in treasure?
	IF(MOVETO(TEMP1,WINNER)) GO TO 79100	! temp1 still there?
81050	CALL RSPEAK(341)			! nothing happens.
	RETURN
C
C V82--	Blast
C
82000	I=342					! dont understand.
	IF(PRSO.EQ.SAFE) I=252			! joke for safe.
	CALL RSPEAK(I)
	RETURN
C
C V83--	Score
C
83000	CALL SCORE(.FALSE.)
	RETURN
C
C V84--	Quit
C
84000	CALL SCORE(.TRUE.)			! tell score.
	IF(.NOT.YESNO(343,10,0)) RETURN		! ask for y/n decision.
	CALL EXIT				! bye.

C VAPPLI, PAGE 17
C
C V85--	Follow (used in endgame)
C
85000	IF(WINNER.NE.AMASTR) GO TO 85100	! tell master, follow.
	CALL RSPEAK(782)
	CFLAG(CEVFOL)=.TRUE.
	CTICK(CEVFOL)=-1			! starts following.
	RETURN
C
85100	I=10					! assume ok.
	IF(PRSO.EQ.0) GO TO 85200		! any object?
	I=964					! joke 1
	IF((OFLAG2(PRSO).AND.VILLBT).NE.0) I=965 ! joke 2.
85200	CALL RSPEAK(I)
	RETURN
C
C V86--	Walk through
C
86000	IF(OBJACT(X)) RETURN			! object handle?
	IF((SCOLRM.EQ.0).OR.((PRSO.NE.SCOL).AND.
	1	((PRSO.NE.WNORT).OR.(HERE.NE.BKBOX)))) GO TO 86100
	SCOLAC=SCOLRM				! walked thru scol.
	PRSO=0					! fake out fromdr.
	CFLAG(CEVSCL)=.TRUE.
	CTICK(CEVSCL)=6				! start alarm.
	CALL RSPEAK(668)			! disorient him.
	F=MOVETO(SCOLRM,WINNER)			! into room.
	F=RMDESC(0)				! describe.
	RETURN
C
86100	IF(HERE.NE.SCOLAC) GO TO 86300		! on other side of scol?
	DO 86200 I=1,12,3			! walk thru proper wall?
	  IF((SCOLWL(I).EQ.HERE).AND.(SCOLWL(I+1).EQ.PRSO))
	1	GO TO 86500			! in specified room?
86200	CONTINUE
C
86300	IF((OFLAG1(PRSO).AND.TAKEBT).NE.0) GO TO 86400	! tkble?
	I=669					! no, joke.
	IF(PRSO.EQ.SCOL) I=670			! special joke for scol.
	CALL RSPSUB(I,ODO2)
	RETURN
C
86400	I=671					! joke.
	IF(OROOM(PRSO).NE.0) I=552+RND(6)	! special jokes if carry.
	CALL RSPEAK(I)
	RETURN
C
86500	PRSO=SCOLWL(I+2)			! thru scol wall...
	DO 86600 I=1,8,2			! find matching room.
	  IF(PRSO.EQ.SCOLDR(I)) SCOLRM=SCOLDR(I+1)
86600	CONTINUE				! declare new scolrm.
	CTICK(CEVSCL)=0				! cancel alarm.
	CALL RSPEAK(668)			! disorient him.
	F=MOVETO(BKBOX,WINNER)			! back in box room.
	F=RMDESC(0)
	RETURN
C
C V87--	Ring
C
87000	IF(OBJACT(X)) RETURN			! object handle?
	I=359					! cant ring.
	IF(PRSO.EQ.BELL) I=360			! ding, dong.
	CALL RSPEAK(I)				! joke.
	RETURN
C
C V88--	Brush
C
88000	IF(PRSO.EQ.TEETH) GO TO 88100		! brush teeth?
	CALL RSPEAK(362)			! no, joke.
	RETURN
C
88100	IF(PRSI.NE.0) GO TO 88200		! with something?
	CALL RSPEAK(363)			! no, joke.
	RETURN
C
88200	IF((PRSI.EQ.PUTTY).AND.(OADV(PUTTY).EQ.WINNER))
	1	GO TO 88300			! with putty?
	CALL RSPSUB(364,ODI2)			! no, joke.
	RETURN
C
88300	CALL JIGSUP(365)			! yes, dead!!!!!
	RETURN

C VAPPLI, PAGE 18
C
C V89--	Dig
C
89000	IF(PRSI.NE.SHOVE) GO TO 89100		! shovel?
	VAPPLI=OBJACT(X)			! must handle.
	RETURN
C
89100	I=392					! assume tool.
	IF((OFLAG1(PRSI).AND.TOOLBT).EQ.0) I=393
	CALL RSPSUB(I,ODI2)
	RETURN
C
C V90--	Time
C
90000	CALL GTTIME(K)				! get play time.
	I=K/60
	J=MOD(K,60)
	WRITE(OUTCH,90010)
	IF(I.NE.0) WRITE(OUTCH,90011) I
	IF(I.GE.2) WRITE(OUTCH,90012)
	IF(I.EQ.1) WRITE(OUTCH,90013)
	IF(J.EQ.1) WRITE(OUTCH,90014) J
	IF(J.NE.1) WRITE(OUTCH,90015) J
	TELFLG=.TRUE.
	RETURN
C
90010	FORMAT(' You have been playing Dungeon for ',$)
90011	FORMAT('+',I3,' hour',$)
90012	FORMAT('+s and ',$)
90013	FORMAT('+ and ',$)
90014	FORMAT('+',I2,' minute.')
90015	FORMAT('+',I2,' minutes.')
C
C V91--	Leap
C
91000	IF(PRSO.EQ.0) GO TO 91200		! over something?
	IF(QHERE(PRSO,HERE)) GO TO 91100	! here?
	CALL RSPEAK(447)			! no, joke.
	RETURN
C
91100	IF((OFLAG2(PRSO).AND.VILLBT).EQ.0) GO TO 91300
	CALL RSPSUB(448,ODO2)			! cant jump villain.
	RETURN
C
91200	IF(.NOT.FINDXT(XDOWN,HERE)) GO TO 91300	! down exit?
	IF(XTYPE.EQ.XNO) GO TO 91400		! invalid?
	IF(XTYPE.NE.XCOND) GO TO 91300		! conditional?
	IF(.NOT.FLAGS(XFLAG)) GO TO 91400	! blocked off?
91300	CALL RSPEAK(314+RND(5))			! wheeee!
	RETURN
C
91400	IF(WINNER.EQ.PLAYER) GO TO 91500	! tell x, jump?
	CALL JIGSUP(452)			! Geronimo!
	RETURN
C
91500	CALL JIGSUP(449+RND(4))			! fatal leap.
	RETURN

C VAPPLI, PAGE 19
C
C V92--	Lock
C
92000	IF(OBJACT(X)) RETURN			! object handle?
	IF((PRSO.EQ.GRATE).AND..NOT.QOPEN(GRATE).AND.(HERE.EQ.MGRAT))
	1	GO TO 92200			! lock closed grate?
92100	CALL RSPEAK(464)			! not lock grate.
	RETURN
C
92200	GRUNLF=.FALSE.				! grate now locked.
	CALL RSPEAK(214)
	TRAVEL(REXIT(HERE)+1)=214		! change exit status.
	RETURN
C
C V93--	Unlock
C
93000	IF(OBJACT(X)) RETURN			! object handle?
	IF((PRSO.NE.GRATE).OR.(HERE.NE.MGRAT))
	1	GO TO 92100			! not unlock grate.
	IF(PRSI.EQ.KEYS) GO TO 93200		! got keys?
	CALL RSPSUB(465,ODI2)			! no, joke.
	RETURN
C
93200	GRUNLF=.TRUE.				! unlock grate.
	CALL RSPEAK(217)
	TRAVEL(REXIT(HERE)+1)=1041		! change exit status.
	RETURN
C
C V94--	Diagnose
C
94000	I=FIGHTS(WINNER,.FALSE.)		! get fights strength.
	J=ASTREN(WINNER)			! get health.
	K=MIN0(I+J,4)				! get state.
	IF(.NOT.CFLAG(CEVCUR)) J=0		! if no wounds.
	L=MIN0(4,IABS(J))			! scale.
	CALL RSPEAK(473+L)			! describe health.
	I=(30*(-J-1))+CTICK(CEVCUR)		! compute wait.
	IF(J.NE.0) WRITE(OUTCH,94100) I
94100	FORMAT(' You will be cured after ',I3,' moves.')
	CALL RSPEAK(478+K)			! how much more?
	IF(DEATHS.NE.0) CALL RSPEAK(482+DEATHS)	! how many deaths?
	RETURN

C VAPPLI, PAGE 20
C
C V95--	Incant
C
95000	IF(WINNER.NE.PLAYER) GO TO 10		! must do yourself.
	IF((RFLAG(MREYE).AND.RSEEN).NE.0) GO TO 95800	! too late?
	IF(SUBLNT.EQ.0) GO TO 95350		! any input?
	PW(1)=' '				! set up parse.
	PW(2)=' '
	WP=1
	CP=1
	DO 95200 I=1,SUBLNT			! scan substring
	  IF(SUBBUF(I:I).NE.' ') GO TO 95150	! blank?
	  IF(CP.EQ.1) GO TO 95200		! anything in word yet?
	  WP=MIN0(2,WP+1)			! advance word pointer.
	  CP=1					! reset char pointer.
	  GO TO 95200
95150	  IF(CP.LE.WRDLNT) PW(WP)(CP:CP)=SUBBUF(I:I)	! add char to word.
	  CP=CP+1
95200	CONTINUE
C
	IF(PW(1).NE.' ') GO TO 95400		! any input?
95350	CALL RSPEAK(856)			! no, ho hum.
95375	PRSCON=0				! kill cmd stream.
	RETURN
C
95400	CALL ENCRYP(PW(1),CH)			! compute response.
	IF(PW(2).NE.' ') GO TO 95600		! two phrases?
C
	IF(SPELLF) GO TO 95550			! he's trying to learn.
	IF((RFLAG(TSTRS).AND.RSEEN).EQ.0) GO TO 95575	! really in end game?
	SPELLF=.TRUE.				! tell him.
	TELFLG=.TRUE.
	WRITE(OUTCH,95510) PW(1)(1:NBLEN(PW(1))),CH
95510	FORMAT(' A hollow voice replies: "',A,1X,A,'".')
	RETURN
C
95550	CALL RSPEAK(857)			! he's got one already.
	GO TO 95375
C
95575	CALL RSPEAK(858)			! he's not in endgame.
	GO TO 95375
C
95600	CALL ENCRYP(PW(2),CH2)			! try both ways.
	IF(SPELLF.OR.((PW(1).NE.CH2).AND.(PW(2).NE.CH)))
	1  GO TO 95900				! wrong or second use.
	SPELLF=.TRUE.				! it works.
	CALL RSPEAK(859)
	CFLAG(CEVSTE)=.TRUE.
	CTICK(CEVSTE)=1				! force start.
	RETURN
C
95800	CALL RSPEAK(855)			! too late.
	GO TO 95375
C
95900	CALL RSPEAK(1052)			! got it wrong.
	GO TO 95375

C VAPPLI, PAGE 21
C
C V96--	Answer
C
96000	IF(WINNER.NE.PLAYER) GO TO 10		! must answer for himself.
	IF(SUBLNT.EQ.0) GO TO 96050		! any substring?
	IF((HERE.EQ.RIDDL).AND..NOT.RIDDLF) GO TO 96700	! riddle room?
	IF(HERE.EQ.FDOOR) GO TO 96100		! end game front door?
96050	CALL RSPEAK(799)			! no one listens.
	PRSCON=0				! kill cmd stream.
	RETURN
C
96100	IF(INQSTF.AND.(NQATT.LT.5).AND.(CORRCT.LT.3))
	1	GO TO 96200			! knocked, didn't lose or win?
	CALL RSPEAK(783)			! no reply.
	PRSCON=0				! kill cmd stream.
	RETURN
C
96200	DO 96300 J=1,NUMANS			! check answers.
	  IF(QUESNO.NE.ANSWER(J)) GO TO 96300	! only check proper ans.
	  IF(SUBBUF.EQ.ANSSTR(J)(1:NBLEN(ANSSTR(J)))) GO TO 96500
96300	CONTINUE
C
	PRSCON=0				! kill cmd stream.
	NQATT=NQATT+1				! wrong, cretin.
	IF(NQATT.GE.5) GO TO 96400		! too many wrong?
	CALL RSPEAK(800+NQATT)			! no, try again.
	RETURN
C
96400	CALL RSPEAK(826)			! all over.
	CFLAG(CEVINQ)=.FALSE.			! lose.
	RETURN
C
96500	CORRCT=CORRCT+1				! got it right.
	CALL RSPEAK(800)			! hooray.
	IF(CORRCT.GE.3) GO TO 96600		! won totally?
	CFLAG(CEVINQ)=.TRUE.
	CTICK(CEVINQ)=2				! no, start again.
	QUESNO=MOD(QUESNO+3,8)
	NQATT=0
	CALL RSPEAK(769)			! ask next question.
	CALL RSPEAK(770+QUESNO)
	RETURN
C
96600	CALL RSPEAK(827)			! quiz over,
	CFLAG(CEVINQ)=.FALSE.
	OFLAG2(QDOOR)=OFLAG2(QDOOR).OR.OPENBT	! open door.
	RETURN
C
96700	IF(SUBBUF.NE.'WELL') GO TO 96050	! right answer?
	RIDDLF=.TRUE.				! solved riddle.
	CALL RSPEAK(338)
	RETURN

C V97-- Count (valuables, possessions in VALUAC)
C
97000	IF(PRSO.NE.MATCH) GO TO 97100		! matches?
	IF(ORMTCH.EQ.1) WRITE(OUTCH,97010) ORMTCH	! print number.
	IF(ORMTCH.NE.1) WRITE(OUTCH,97020) ORMTCH
97010	FORMAT(' You have ',I1,' match.')
97020	FORMAT(' You have ',I1,' matches.')
	TELFLG=.TRUE.
	RETURN
C
97100	I=1062					! default.
	IF(PRSO.EQ.BAGCO) I=561			! bag of coins.
	IF(PRSO.EQ.CANDL) I=1058		! candles.
	IF(PRSO.EQ.BILLS) I=1059		! stack of bills.
	IF(PRSO.EQ.LEAVE) I=1060		! pile of leaves.
	IF(PRSO.EQ.GWISH) I=1061		! blessings.
	IF(PRSO.EQ.HEADS) I=1084		! heads.
	CALL RSPEAK(I)				! print response.
	RETURN
C
	END

C TAKE-- Basic take sequence
C
C Take an object (for verbs take, put, drop, read, etc.)
C
	LOGICAL FUNCTION TAKE(FLG)
C
C Declarations
C
	IMPLICIT INTEGER (A-Z)
	LOGICAL FLG,OBJACT,OAPPLI,QHERE
	INCLUDE 'dparam.for'

C TAKE, PAGE 2
C
	IF((PRSO.LE.STRBIT).AND.
	1 ((OFLAG2(PRSO).AND.NOCHBT).EQ.0)) GO TO 100	! star or nocheck?
	TAKE=OBJACT(X)				! yes, let it handle.
	RETURN
C
100	TAKE=.FALSE.				! assume loses.
	X=OCAN(PRSO)				! inside?
	IF((PRSI.EQ.0).OR.(PRSI.EQ.X)) GO TO 200 ! take x from ocan(x)?
	CALL RSPEAK(1038)			! not in that.
	RETURN
C
200	IF(PRSO.NE.AVEHIC(WINNER)) GO TO 400	! his vehicle?
	CALL RSPEAK(672)			! dummy.
	RETURN
C
400	IF((OFLAG1(PRSO).AND.TAKEBT).NE.0) GO TO 500 ! takeable?
	IF(.NOT.OAPPLI(OACTIO(PRSO),0)) CALL RSPEAK(552+RND(6))
	RETURN
C
C Object is takeable and in position to be taken.
C
500	IF((X.NE.0).OR. QHERE(PRSO,HERE)) GO TO 600
	I=103					! assume player.
	IF(WINNER.NE.PLAYER) I=1080
	IF(OADV(PRSO).EQ.WINNER) CALL RSPEAK(I)	! already got it?
	RETURN
C
600	IF(X.EQ.0) GO TO 650			! contained?
	IF(OADV(X).EQ.WINNER) GO TO 700		! already carrying cont?
650	IF((WEIGHR(PRSO,WINNER)+OSIZE(PRSO)).LE.MXLOAD)
	2	GO TO 700			! can he carry?
	I=558					! assume player.
	IF(WINNER.NE.PLAYER) I=1079
	CALL RSPEAK(I)				! too much weight.
	PRSCON=0				! kill cmd stream.
	RETURN
C
700	TAKE=.TRUE.				! at last.
	IF(OAPPLI(OACTIO(PRSO),0)) RETURN	! did it handle?
	CALL NEWSTA(PRSO,0,0,0,WINNER)		! take object for winner.
	OFLAG2(PRSO)=OFLAG2(PRSO).OR.TCHBT	! has been touched.
	CALL SCRUPD(OFVAL(PRSO))		! update score.
	OFVAL(PRSO)=0				! cant be scored again.
	IF(FLG) CALL RSPEAK(559)		! tell taken.
	RETURN
C
	END

C DROP- Drop verb processor (also throw, pour water)
C
C Declarations
C
	LOGICAL FUNCTION DROP(FLG)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL F,PUT,OBJACT,FLG
C
	IF((OFLAG2(PRSO).AND.NOCHBT).EQ.0) GO TO 100
	DROP=OBJACT(X)				! no check, let obj handle.
	RETURN
C
100	DROP=.TRUE.				! assume wins.
	X=OCAN(PRSO)				! get container.
	IF(X.EQ.0) GO TO 200			! is it inside?
	IF(OADV(X).NE.WINNER) GO TO 1000	! is he carrying con?
	IF((OFLAG2(X).AND.OPENBT).NE.0) GO TO 300 ! is it open?
	CALL RSPSUB(525,ODESC2(X))		! cant reach.
	RETURN
C
200	IF(OADV(PRSO).NE.WINNER) GO TO 1000	! is he carrying obj?
300	IF(AVEHIC(WINNER).EQ.0) GO TO 400	! is he in vehicle?
	PRSI=AVEHIC(WINNER)			! yes,
	F=PUT(.TRUE.)				! drop into vehicle.
	PRSI=0					! disarm parser.
	RETURN					! done.
C
400	CALL NEWSTA(PRSO,0,HERE,0,0)		! drop into room.
	CALL SCRUPD(OFVAL(PRSO))		! score object.
	OFVAL(PRSO)=0				! cant be scored again.
	OFLAG2(PRSO)=OFLAG2(PRSO).OR.TCHBT	! has been touched.
C
	IF(OBJACT(X)) RETURN			! did it handle?
	IF(PRSA.EQ.DROPW) CALL RSPEAK(528)
	IF(PRSA.EQ.THROWW) CALL RSPEAK(529)
	RETURN
C
1000	I=527					! assume player.
	IF(WINNER.NE.PLAYER) I=1078
	CALL RSPEAK(I)				! dont have it.
	RETURN
C
	END

C PUT- Put verb processor
C
C Declarations
C
	LOGICAL FUNCTION PUT(FLG)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL QOPEN,QHERE,OBJACT,FLG,TAKE
C
C Functions and data
C
	QOPEN(R)=(OFLAG2(R).AND.OPENBT).NE.0

C PUT, PAGE 2
C
	IF((OFLAG2(PRSO).AND.NOCHBT).EQ.0) GO TO 100
	PUT=OBJACT(X)				! no check, let obj handle.
	RETURN
C
100	PUT=.FALSE.
	IF((PRSO.LE.STRBIT).AND.(PRSI.LE.STRBIT)) GO TO 200
	IF(.NOT.OBJACT(X)) CALL RSPEAK(560)	! star
	PUT=.TRUE.
	RETURN
C
200	IF(QOPEN(PRSI).OR.((OFLAG1(PRSI).AND.(DOORBT+CONTBT)).NE.0)
	1	.OR.((OFLAG2(PRSI).AND.VEHBT).NE.0)) GO TO 300
	CALL RSPEAK(561)			! cant put in that.
	RETURN
C
300	IF(QOPEN(PRSI)) GO TO 400		! is it open?
	CALL RSPEAK(562)			! no, joke
	RETURN
C
400	IF(PRSO.NE.PRSI) GO TO 500		! into itself?
	CALL RSPEAK(563)			! yes, joke.
	RETURN
C
500	IF(OCAN(PRSO).NE.PRSI) GO TO 600	! already inside.
	CALL RSPSB2(564,ODESC2(PRSO),ODESC2(PRSI))
	PUT=.TRUE.
	RETURN
C
600	IF((WEIGHR(PRSO,0)+WEIGHR(PRSI,0)+OSIZE(PRSO))
	1	.LE.OCAPAC(PRSI)) GO TO 700	! not too full?
	IF(PRSI.NE.AVEHIC(WINNER)) GO TO 650	! into vehicle?
	CALL RSPSUB(889,ODESC2(PRSI))		! too full.
	RETURN
C
650	CALL RSPEAK(565)			! then cant do it.
	RETURN
C
C Now see if object (or its container) is in room
C
700	J=PRSO					! start search.
725	IF(QHERE(J,HERE)) GO TO 750		! is it here?
	J=OCAN(J)
	IF(J.NE.0) GO TO 725			! more to do?
	GO TO 800				! no, sch fails.
C
750	IF((PRSO.EQ.WATER).OR.(PRSO.EQ.GWATE)) GO TO 800
	SVO=PRSO				! save parser.
	SVI=PRSI
	PRSA=TAKEW
	PRSI=0
	IF(.NOT.TAKE(.FALSE.)) RETURN		! take object.
	PRSA=PUTW
	PRSO=SVO
	PRSI=SVI
	GO TO 1000
C
C Now see if object is on person.
C
800	IF(OCAN(PRSO).EQ.0) GO TO 1000		! inside?
	IF(QOPEN(OCAN(PRSO))) GO TO 900		! open?
	CALL RSPSUB(566,ODESC2(PRSO))		! lose.
	RETURN
C
900	CALL SCRUPD(OFVAL(PRSO))		! score object.
	OFVAL(PRSO)=0
	OFLAG2(PRSO)=OFLAG2(PRSO).OR.TCHBT	! has been touched.
	CALL NEWSTA(PRSO,0,0,0,WINNER)		! temporarily on winner.
C
1000	IF(OBJACT(X)) RETURN			! no, give object a shot.
	CALL NEWSTA(PRSO,2,0,PRSI,0)		! contained inside.
	PUT=.TRUE.
	RETURN
C
	END

C VALUAC- Handles valuables/everything/possessions/bunch object
C	  for take, put, drop, count
C
C Declarations
C
	SUBROUTINE VALUAC(V)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL LIT,F,F1,TAKE,PUT,DROP,NOTHIS,NOHERE,QHERE,QBUNCH
C
C Functions and data
C
	NOTHIS(R)=((SAVEP.EQ.BUNOBJ).AND.QBUNCH(R)) .OR.
	1	  ((OTVAL(R).LE.0).AND.((SAVEP.EQ.VALUA).OR.
	2	   ((SAVEP.EQ.BUNOBJ).AND.(BUNSUB.EQ.VALUA)))) .OR.
	3	  ((OADV(R).NE.WINNER).AND.((SAVEP.EQ.POSSE).OR.
	4	   ((SAVEP.EQ.BUNOBJ).AND.(BUNSUB.EQ.POSSE))))

	NOHERE(R)=((AV.EQ.0).AND..NOT.QHERE(R,HERE)) .OR.
	1	  ((AV.NE.0).AND.(OCAN(R).NE.AV))

C VALUAC, PAGE 2
C Count
C
	IF((PRSA.NE.COUNTW).OR.(PRSO.NE.POSSE)) GO TO 100
	K=0
	DO 50 J=1,OLNT				! count possessions.
	  IF(OADV(J).EQ.WINNER) K=K+1
50	CONTINUE
	IF(K.EQ.1) WRITE(OUTCH,60) K
	IF(K.NE.1) WRITE(OUTCH,70) K
60	FORMAT(' You have ',I1,' possession.')
70	FORMAT(' You have ',I2,' possessions.')
	TELFLG=.TRUE.
	RETURN
C
100	IF((PRSA.NE.COUNTW).OR.(PRSO.NE.VALUA)) GO TO 200
	K=0
	L=0
	DO 150 J=1,OLNT				! count treasures.
	  IF((OADV(J).EQ.WINNER).AND.(OTVAL(J).GT.0)) K=K+1
	  IF((OCAN(J).EQ.TCASE).AND.(OTVAL(J).GT.0)) L=L+1
150	CONTINUE
	IF(K.EQ.1) WRITE(OUTCH,160) K
	IF(K.NE.1) WRITE(OUTCH,170) K
160	FORMAT(' You have ',I1,' valuable.')
170	FORMAT(' You have ',I2,' valuables.')
	TELFLG=.TRUE.
	IF(HERE.NE.LROOM) RETURN
	IF(L.EQ.1) WRITE(OUTCH,180) L
	IF(L.NE.1) WRITE(OUTCH,190) L
180	FORMAT(' Your adventure has netted ',I1,' treasure.')
190	FORMAT(' Your adventure has netted ',I2,' treasures.')
	RETURN

C VALUAC, PAGE 3
C Take
C
200	SAVEP=PRSO				! save prso.
	SAVEH=HERE				! save here.
	F=.TRUE.				! assume no actions.
	I=579					! assume not lit.
	AV=AVEHIC(WINNER)			! get vehicle.
C
	IF(PRSA.NE.TAKEW) GO TO 1000		! take?
	IF(.NOT.LIT(HERE)) GO TO 4500		! if not lit, punt.
	IF((PRSO.NE.BUNOBJ).OR.(BUNSUB.NE.0)) GO TO 400	! bunch, no except?
	DO 300 I=1,BUNLNT			! loop through bunch.
	  PRSO=BUNVEC(I)			! get next item.
	  F=.FALSE.
	  CALL RSPSUB(580,ODESC2(PRSO))
	  F1=TAKE(.TRUE.)
	  IF(SAVEH.NE.HERE) GO TO 4500
300	CONTINUE
	GO TO 4000				! go clean up.
C
400	DO 500 PRSO=1,OLNT			! loop thru objects.
	  IF((((OFLAG1(PRSO).AND.TAKEBT).EQ.0).AND.
	1	((OFLAG2(PRSO).AND.TRYBT).EQ.0)).OR.
	2	((OFLAG1(PRSO).AND.VISIBT).EQ.0).OR.
	3	((OFLAG2(PRSO).AND.ACTRBT).NE.0).OR.
	4	NOTHIS(PRSO)) GO TO 500
	  IF(.NOT.NOHERE(PRSO)) GO TO 450	! is it here?
	  J=OCAN(PRSO)				! get container.
	  IF((J.EQ.0).OR.(PRSO.EQ.WATER)) GO TO 500	! in a cont?
	  IF(((OFLAG2(J).AND.OPENBT).EQ.0).OR.
	1	(NOHERE(J).AND.(OADV(J).NE.WINNER)))
	2	GO TO 500			! in open cont here?
C
450	  F=.FALSE.
	  CALL RSPSUB(580,ODESC2(PRSO))
	  F1=TAKE(.TRUE.)
	  IF(SAVEH.NE.HERE) GO TO 4500
500	CONTINUE
	GO TO 4000				! go clean up.

C VALUAC, PAGE 4
C Drop
C
1000	IF(PRSA.NE.DROPW) GO TO 2000		! drop?
	IF((PRSO.NE.BUNOBJ).OR.(BUNSUB.NE.0)) GO TO 1400 ! bunch, no except?
	DO 1300 I=1,BUNLNT			! loop through bunch.
	  PRSO=BUNVEC(I)			! get next item.
	  F=.FALSE.
	  CALL RSPSUB(580,ODESC2(PRSO))
	  F1=DROP(.TRUE.)
	  IF(SAVEH.NE.HERE) GO TO 4500
1300	CONTINUE
	GO TO 4000				! go clean up.
C
1400	DO 1500 PRSO=1,OLNT			! loop through inventory.
	  IF((OADV(PRSO).NE.WINNER).OR.NOTHIS(PRSO))
	1	GO TO 1500
	  F=.FALSE.
	  CALL RSPSUB(580,ODESC2(PRSO))
	  F1=DROP(.TRUE.)
	  IF(SAVEH.NE.HERE) GO TO 4500
1500	CONTINUE
	GO TO 4000				! go clean up.

C VALUAC, PAGE 5
C Put
C
2000	IF(PRSA.NE.PUTW) GO TO 3000		! put?
	IF(.NOT.LIT(HERE)) GO TO 4500		! if not lit, punt.
	IF((PRSO.NE.BUNOBJ).OR.(BUNSUB.NE.0)) GO TO 2400 ! bunch, no except?
	DO 2300 I=1,BUNLNT			! loop through bunch.
	  PRSO=BUNVEC(I)			! get next item.
	  F=.FALSE.
	  CALL RSPSUB(580,ODESC2(PRSO))
	  F1=PUT(.TRUE.)
	  IF(SAVEH.NE.HERE) GO TO 4500
2300	CONTINUE
	GO TO 4000				! go clean up.
C
2400	DO 2500 PRSO=1,OLNT			! loop thru objects.
	  IF(((OADV(PRSO).NE.WINNER).AND.
	1	(NOHERE(PRSO).OR.
	2	(((OFLAG1(PRSO).AND.TAKEBT).EQ.0).AND.
	3	 ((OFLAG2(PRSO).AND.TRYBT).EQ.0)))) .OR.
	4	(PRSO.EQ.PRSI).OR.NOTHIS(PRSO).OR.
	5	((OFLAG1(PRSO).AND.VISIBT).EQ.0)) GO TO 2500
	  F=.FALSE.
	  CALL RSPSUB(580,ODESC2(PRSO))
	  F1=PUT(.TRUE.)
	  IF(SAVEH.NE.HERE) GO TO 4500
2500	CONTINUE
	GO TO 4000				! go clean up.
C
C Wrong verb.
C
3000	I=677					! wrong verb.
	GO TO 4500
C
C Clean up.
C
4000	I=581					! right verb, choose
	IF(SAVEP.EQ.VALUA) I=582		! nothing happened message.
4500	IF(F) CALL RSPEAK(I)			! not lit, nothing, wrong verb?
	PRSO=SAVEP				! restore PRSO.
	BUNSUB=0				! cancel EXCEPT/BUT.
	RETURN
	END

C QBUNCH-	Is object in bunch vector?
C
C Declarations
C
	LOGICAL FUNCTION QBUNCH(OBJ)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
C
	IF(BUNLNT.EQ.0) GO TO 200		! bunch vector empty?
	QBUNCH=.TRUE.				! assume found.
	DO 100 I=1,BUNLNT			! search bunch vector.
	  IF(OBJ.EQ.BUNVEC(I)) RETURN		! got one.
100	CONTINUE
200	QBUNCH=.FALSE.				! not found.
	RETURN
C
	END

C SAVE- Save game state
C
C Declarations
C
	SUBROUTINE SAVEGM
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
C
	IF(SUBLNT.EQ.0) SUBBUF='DSAVE.DAT'
	OPEN (UNIT=1,NAME=SUBBUF,ACCESS='SEQUENTIAL',
	1	STATUS='UNKNOWN',FORM='UNFORMATTED',ERR=100)
C
	CALL GTTIME(I)				! get time.
	WRITE(1) VMAJ,VMIN
	WRITE(1) WINNER,HERE,THFPOS,TELFLG,THFFLG,THFACT,
	1	SWDACT,SWDSTA,CPVEC
	WRITE(1) I,MOVES,DEATHS,RWSCOR,EGSCOR,MXLOAD,
	1	LTSHFT,BLOC,MUNGRM,HS,FROMDR,SCOLRM,SCOLAC
	WRITE(1) ODESC1,ODESC2,OFLAG1,OFLAG2,OFVAL,OTVAL,
	1	OSIZE,OCAPAC,OROOM,OADV,OCAN
	WRITE(1) RDESC1,RVAL,RFLAG,TRAVEL
	WRITE(1) AROOM,ASCORE,AVEHIC,ASTREN,AFLAG
	WRITE(1) FLAGS,SWITCH,VPROB,CFLAG,CTICK,CCNCEL
C
	CALL RSPEAK(597)
	CLOSE (UNIT=1)
	RETURN
C
100	CALL RSPEAK(598)			! cant do it.
	RETURN
	END

C RESTORE- Restore game state
C
C Declarations
C
	SUBROUTINE RSTRGM
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
C
	IF(SUBLNT.EQ.0) SUBBUF='DSAVE.DAT'
	OPEN (UNIT=1,NAME=SUBBUF,ACCESS='SEQUENTIAL',
	1	STATUS='OLD',READONLY,FORM='UNFORMATTED',ERR=100)
C
	READ(1) I,J
	IF((I.NE.VMAJ).OR.(J.NE.VMIN)) GO TO 200
C
	READ(1) WINNER,HERE,THFPOS,TELFLG,THFFLG,THFACT,
	1	SWDACT,SWDSTA,CPVEC
	READ(1) PLTIME,MOVES,DEATHS,RWSCOR,EGSCOR,MXLOAD,
	1	LTSHFT,BLOC,MUNGRM,HS,FROMDR,SCOLRM,SCOLAC
	READ(1) ODESC1,ODESC2,OFLAG1,OFLAG2,OFVAL,OTVAL,
	1	OSIZE,OCAPAC,OROOM,OADV,OCAN
	READ(1) RDESC1,RVAL,RFLAG,TRAVEL
	READ(1) AROOM,ASCORE,AVEHIC,ASTREN,AFLAG
	READ(1) FLAGS,SWITCH,VPROB,CFLAG,CTICK,CCNCEL
C
	CALL RSPEAK(599)
	CLOSE (UNIT=1)
	RETURN
C
100	CALL RSPEAK(598)			! cant do it.
	RETURN
C
200	CALL RSPEAK(600)			! obsolete version
	CLOSE (UNIT=1)
	RETURN
	END

C WALK- Move in specified direction
C
C Declarations
C
	LOGICAL FUNCTION WALK(X)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL FINDXT,QOPEN,LIT,PROB,MOVETO,RMDESC
C
C Functions and data
C
	QOPEN(O)=(OFLAG2(O).AND.OPENBT).NE.0

C WALK, PAGE 2
C
	WALK=.TRUE.				! assume wins.
	IF((WINNER.NE.PLAYER).OR.LIT(HERE).OR.PROB(25,25))
	1	GO TO 500
	IF(.NOT.FINDXT(PRSO,HERE)) GO TO 450	! invalid exit? grue!
	GO TO (400,200,100,300),XTYPE		! decode exit type.
	CALL BUG(9,XTYPE)
C
100	IF(CXAPPL(XACTIO).NE.0) GO TO 400	! cexit... returned room?
	IF(FLAGS(XFLAG)) GO TO 400		! no, flag on?
200	CALL JIGSUP(523)			! bad exit, grue!
	RETURN
C
300	IF(CXAPPL(XACTIO).NE.0) GO TO 400	! door... returned room?
	IF(QOPEN(XOBJ)) GO TO 400		! no, door open?
	CALL JIGSUP(523)			! bad exit, grue!
	RETURN
C
400	IF(LIT(XROOM1)) GO TO 900		! valid room, is it lit?
450	CALL JIGSUP(522)			! no, grue!
	RETURN
C
C Room is lit, or winner is not player (no grue).
C
500	IF(FINDXT(PRSO,HERE)) GO TO 550		! exit exist?
525	XSTRNG=678				! assume wall.
	IF(PRSO.EQ.XUP) XSTRNG=679		! if up, cant.
	IF(PRSO.EQ.XDOWN) XSTRNG=680		! if down, cant.
	IF(((RFLAG(HERE).AND.RNWALL).NE.0).AND.(WINNER.EQ.PLAYER))
	1	XSTRNG=524			! no wall for player.
	CALL RSPEAK(XSTRNG)
	PRSCON=0				! stop cmd stream.
	RETURN
C
550	GO TO (900,600,700,800),XTYPE		! branch on exit type.
	CALL BUG(9,XTYPE)
C
700	IF(CXAPPL(XACTIO).NE.0) GO TO 900	! cexit... returned room?
	IF(FLAGS(XFLAG)) GO TO 900		! no, flag on?
600	IF(XSTRNG.EQ.0) GO TO 525		! if no reason, use std.
	CALL RSPEAK(XSTRNG)			! deny exit.
	PRSCON=0				! stop cmd stream.
	RETURN
C
800	IF(CXAPPL(XACTIO).NE.0) GO TO 900	! door... returned room?
	IF(QOPEN(XOBJ)) GO TO 900		! no, door open?
	IF(XSTRNG.EQ.0) XSTRNG=525		! if no reason, use std.
	CALL RSPSUB(XSTRNG,ODESC2(XOBJ))
	PRSCON=0				! stop cmd stream.
	RETURN
C
900	WALK=MOVETO(XROOM1,WINNER)		! move to room.
	IF(WALK) WALK=RMDESC(0)			! describe room.
	RETURN
	END

C CXAPPL- Conditional exit processors
C
C Declarations
C
	INTEGER FUNCTION CXAPPL(RI)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
C
	CXAPPL=0				! no return.
	IF(RI.EQ.0) RETURN			! if no action, done.
	GO TO (1000,2000,3000,4000,5000,6000,7000,8000,
	1	9000,10000,11000,12000,13000,14000,15000),RI
	CALL BUG(5,RI)

C CXAPPL, PAGE 2
C
C C1-	Coffin-cure
C
1000	EGYPTF=OADV(COFFI).NE.WINNER		! t if no coffin.
	RETURN
C
C C2-	Carousel exit
C C5-	Carousel out
C
2000	IF(CAROFF) RETURN			! if flipped, nothing.
2500	CALL RSPEAK(121)			! can't tell directions.
5000	I=XELNT(XCOND)*RND(8)			! choose random exit.
	XROOM1=(TRAVEL(REXIT(HERE)+I)).AND.XRMASK
	CXAPPL=XROOM1				! return exit.
	RETURN
C
C C3-	Chimney function
C
3000	LITLDF=.FALSE.				! assume heavy load.
	IF(DEADF) GO TO 3300			! if dead, always ok.
	J=0
	DO 3100 I=1,OLNT			! count objects.
	  IF(OADV(I).EQ.WINNER) J=J+1
3100	CONTINUE
C
	IF(J.GT.2) RETURN			! carrying too much?
	IF(J.NE.0) GO TO 3200			! carrying too little?
	XSTRNG=890				! bad idea.
	RETURN
C
3200	IF(OADV(LAMP).EQ.WINNER) GO TO 3300	! no lamp?
	XSTRNG=446				! bad idea.
	RETURN
C
3300	LITLDF=.TRUE.				! he can do it.
	IF((OFLAG2(DOOR).AND.OPENBT).EQ.0)
	1	OFLAG2(DOOR)=OFLAG2(DOOR).AND. .NOT.TCHBT
	RETURN
C
C C4-	Frobozz flag (Magnet Room, fake exit)
C C6-	Frobozz flag (Magnet Room, real exit)
C
4000	IF(CAROFF) GO TO 2500			! if flipped, go spin.
	FROBZF=.FALSE.				! otherwise, not an exit.
	RETURN
C
6000	IF(CAROFF) GO TO 2500			! if flipped, go spin.
	FROBZF=.TRUE.				! otherwise, an exit.
	RETURN

C CXAPPL, PAGE 3
C
C C7-	Frobozz flag (bank alarm)
C
7000	FROBZF=.FALSE.				! assume fails.
	J=BILLS					! check for bills.
7100	IF(OADV(J).EQ.WINNER) RETURN		! winner's got it, fail.
	J=OCAN(J)				! get container.
	IF(J.NE.0) GO TO 7100			! if inside, loop
	J=PORTR					! check for portrait.
7200	IF(OADV(J).EQ.WINNER) RETURN		! winner's got it, fail.
	J=OCAN(J)				! get container.
	IF(J.NE.0) GO TO 7200			! if inside, loop
	FROBZF=.TRUE.				! wins.
	RETURN
C
C C8-	Frobozz flag (MRGO)
C
8000	FROBZF=.FALSE.				! assume cant move.
	IF(MLOC.NE.XROOM1) GO TO 8100		! mirror in way?
	IF((PRSO.EQ.XNORTH).OR.(PRSO.EQ.XSOUTH)) GO TO 8200
	IF(MOD(MDIR,180).NE.0) GO TO 8300	! mirror must be n-s.
	XROOM1=((XROOM1-MRA)*2)+MRAE		! calc east room.
	IF(PRSO.GT.XSOUTH) XROOM1=XROOM1+1	! if sw/nw, calc west.
8100	CXAPPL=XROOM1
	RETURN
C
8200	XSTRNG=814				! assume struc blocks.
	IF(MOD(MDIR,180).EQ.0) RETURN		! if mirror n-s, done.
8300	LDIR=MDIR				! see which mirror.
	IF(PRSO.EQ.XSOUTH) LDIR=180
	XSTRNG=815				! mirror blocks.
	IF(((LDIR.GT.180).AND..NOT.MR1F).OR.
	1  ((LDIR.LT.180).AND..NOT.MR2F)) XSTRNG=816 ! mirror broken.
	RETURN
C
C C9-	Frobozz flag (MIRIN)
C
9000	IF(MRHERE(HERE).NE.1) GO TO 9100	! mirror 1 here?
	IF(MR1F) XSTRNG=805			! see if broken.
	FROBZF=MROPNF				! enter if open.
	RETURN
C
9100	FROBZF=.FALSE.				! not here,
	XSTRNG=817				! lose.
	RETURN

C CXAPPL, PAGE 4
C
C C10-	Frobozz flag (mirror exit)
C
10000	FROBZF=.FALSE.				! assume cant.
	LDIR=((PRSO-XNORTH)/XNORTH)*45		! xlate dir to degrees.
	IF(.NOT.MROPNF .OR.
	1	((MOD(MDIR+270,360).NE.LDIR).AND.(PRSO.NE.XEXIT)))
	2	GO TO 10200			! exit via mirror?
	XROOM1=((MLOC-MRA)*2)+MRAE+1-(MDIR/180)	! assume e-w exit.
	IF(MOD(MDIR,180).EQ.0) GO TO 10100	! if n-s, ok.
	XROOM1=MLOC+1				! assume n exit.
	IF(MDIR.GT.180) XROOM1=MLOC-1		! if south.
10100	CXAPPL=XROOM1
	RETURN
C
10200	IF(.NOT.WDOPNF .OR.
	1	((MOD(MDIR+180,360).NE.LDIR).AND.(PRSO.NE.XEXIT)))
	2	RETURN				! exit via open door?
	XROOM1=MLOC+1				! assume n.
	IF(MDIR.EQ.0) XROOM1=MLOC-1		! if s.
	CALL RSPEAK(818)			! close door.
	WDOPNF=.FALSE.
	CXAPPL=XROOM1
	RETURN
C
C C11-	Maybe door.  Normal message is that door is closed.
C	But if LCELL.NE.4, door isn't there.
C
11000	IF(LCELL.NE.4) XSTRNG=678		! set up msg.
	RETURN
C
C C12-	Frobozz flag (Puzzle Room main entrance)
C
12000	CPHERE=10				! set substate.
	FROBZF=CPVEC(CPHERE).EQ.0		! enter if not blocked.
	RETURN
C
C C13-	CPOUTF (Puzzle Room size entrance)
C
13000	CPHERE=52				! set substate.
	RETURN

C CXAPPL, PAGE 5
C
C C14-	Frobozz flag (Puzzle Room transitions)
C
14000	FROBZF=.FALSE.				! asssume lose.
	IF(PRSO.NE.XUP) GO TO 14100		! up?
	IF(CPHERE.NE.10) RETURN			! at exit?
	XSTRNG=881				! assume no ladder.
	IF(CPVEC(CPHERE+1).NE.-2) RETURN	! ladder here?
	CALL RSPEAK(882)			! you win.
	RFLAG(CPUZZ)=RFLAG(CPUZZ).AND..NOT.RSEEN	! reset seen.
	FROBZF=.TRUE.				! let him out.
	RETURN
C
14100	IF((CPHERE.NE.52).OR.(PRSO.NE.XWEST).OR..NOT.CPOUTF)
	1	GO TO 14200			! w exit at door?
	RFLAG(CPUZZ)=RFLAG(CPUZZ).AND..NOT.RSEEN	! reset seen.
	FROBZF=.TRUE.				! yes, let him out.
	RETURN
C
14200	IF((CPHERE.NE.52).OR.(PRSO.NE.XWEST)) GO TO 14250
	XSTRNG=932				! door in way.
	RETURN
C
14250	DO 14300 I=1,16,2			! locate exit.
	  IF(PRSO.EQ.CPDR(I)) GO TO 14400
14300	CONTINUE
	RETURN					! no such exit.
C
14400	J=CPDR(I+1)				! get directional offset.
	NXT=CPHERE+J				! get next state.
	K=8					! get orthogonal dir.
	IF(J.LT.0) K=-8
	IF((((IABS(J).EQ.1).OR.(IABS(J).EQ.8)).OR.
	1   ((CPVEC(CPHERE+K).EQ.0).OR.(CPVEC(NXT-K).EQ.0))).AND.
	2    (CPVEC(NXT).EQ.0)) GO TO 14500	! cant do it?
	RETURN
C
14500	CALL CPGOTO(NXT)			! move to state.
	XROOM1=CPUZZ				! stay in room.
	CXAPPL=XROOM1
	RETURN
C
C C15-	Frobozz flag (slide exit)
C
15000	FROBZF=.TRUE.				! works.
	IF((TTIE.EQ.0).OR.DEADF) RETURN		! if no rope or dead, cellar.
	IF(OROOM(TTIE).NE.HERE) RETURN		! if rope elsewhere, cellar.
	CALL RSPEAK(1014)			! slippery.
	CFLAG(CEVSLI)=.TRUE.			! turn on slide clock.
	CTICK(CEVSLI)=MAX0(2,100/WEIGHR(0,WINNER))
	XROOM1=SLID1				! on the ropes.
	CXAPPL=XROOM1
	RETURN
C
	END
