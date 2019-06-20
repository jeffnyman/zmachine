C Command loop, initialization for DUNGEON
C
C COPYRIGHT 1980, 1990, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA.
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK
C
C 17-Sep-94	RMS	Fixed TELL/parse fail bug.  Fixed VMS/UNIX
C			compatibility problem.
C 30-Jan-94	RMS	Fixed bugs from MS DOS port.
C 30-Jun-92	RMS	Changed file names to lower case.
C
C GAME- Main command loop
C
C Declarations
C
	SUBROUTINE GAME
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL RMDESC,VAPPLI,AAPPLI,OBJACT
	LOGICAL F,PARSE,FINDXT,XVEHIC,LIT,PRVLIT

C GAME, PAGE 2
C
C Start up, describe current location.
C
	CALL RSPEAK(1)				! welcome aboard.
	F=RMDESC(3)				! start game.
C
C Now loop, reading and executing commands.
C
100	WINNER=PLAYER				! player moving.
	TELFLG=.FALSE.				! assume nothing told.
	IF(PRSCON.LE.1) CALL RDLINE(INBUF,INLNT,1) ! read command.
C
	IF(INBUF(PRSCON:INLNT).NE.'GDT') GO TO 200	! call on gdt?
	CALL GDT				! yes, invoke.
	PRSCON=1				! force restart.
	GO TO 100				! onward.
C
200	MOVES=MOVES+1
	SUBLNT=0				! no substrings.
	PRVHER=HERE				! save current location.
	PRVLIT=LIT(HERE)			! save current lighting.
	PRSWON=PARSE(INBUF,INLNT,.TRUE.)	! parse input, normal mode.
	IF(.NOT.PRSWON) GO TO 400		! parse lose?
	IF(AAPPLI(AACTIO(WINNER))) GO TO 400	! player handle?
	IF(XVEHIC(1)) GO TO 400			! vehicle handle?
C
	IF(PRSA.EQ.TELLW) GO TO 2000		! tell?
300	IF((PRSO.EQ.VALUA).OR.(PRSO.EQ.EVERY).OR.(PRSO.EQ.POSSE)
	1	.OR.(PRSO.EQ.BUNOBJ)) GO TO 900	! collective object?
	IF(.NOT.VAPPLI(PRSA)) GO TO 400		! verb ok?
	IF(.NOT.PRVLIT.AND.(HERE.EQ.PRVHER)
	1	.AND.LIT(HERE)) F=RMDESC(0)	! now lit
350	IF(.NOT.(ECHOF.OR.DEADF).AND.(HERE.EQ.ECHOR)) GO TO 1000
	CALL RAPPLI(RACTIO(HERE))		! room action?
C
400	CALL XENDMV(TELFLG)			! do end of move.
	IF(.NOT.LIT(HERE)) PRSCON=1		! if not lit, restart.
	GO TO 100
C
900	CALL VALUAC(PRSO)			! collective object.
	GO TO 350

C GAME, PAGE 3
C
C Special case-- Echo Room.
C If input is not 'ECHO' or a direction, just echo.
C
1000	CALL RDLINE(INBUF,INLNT,0)		! read line.
	MOVES=MOVES+1				! charge for moves.
	IF(INBUF.NE.'ECHO') GO TO 1100		! input = echo?
C
	CALL RSPEAK(571)			! kill the echo.
	ECHOF=.TRUE.
	OFLAG2(BAR)=OFLAG2(BAR).AND. .NOT.SCRDBT ! let thief steal bar.
	PRSWON=.TRUE.				! fake out parser.
	PRSCON=1				! force new input.
	GO TO 400
C
1100	IF(INBUF.NE.'BUG') GO TO 1200		! bug request?
	CALL RSPEAK(913)			! wrong, jack.
	GO TO 1000				! try again.
C
1200	IF(INBUF.NE.'FEATURE') GO TO 1300	! feature request?
	CALL RSPEAK(914)			! right, jack.
	GO TO 1000				! try again
C
1300	PRSWON=PARSE(INBUF,INLNT,.FALSE.)	! parse input, echo mode.
	IF(.NOT.PRSWON .OR. (PRSA.NE.WALKW))
	1	GO TO 1400			! walk?
	IF(FINDXT(PRSO,HERE)) GO TO 300		! valid exit?
C
1400	WRITE(OUTCH,1410) INBUF(1:INLNT)	! echo input.
1410	FORMAT(1X,A)
	TELFLG=.TRUE.				! indicate output.
	GO TO 1000				! more echo room.

C GAME, PAGE 4
C
C Special case-- TELL <ACTOR> "NEW COMMAND".
C Note that we cannot be in the Echo Room.
C
2000	IF(SUBLNT.NE.0) GO TO 2050		! any substring?
	CALL RSPSUB(946,ODESC2(PRSO))		! no, joke.
	GO TO 2150				! done.
C
2050	IF(PRSO.NE.OPLAY) GO TO 2100		! to self?
	WRITE(OUTCH,2060) SUBBUF(1:SUBLNT)	! ok, tell it.
2060	FORMAT(' Ok: "',A,'".')
	TELFLG=.TRUE.
	GO TO 2150
C
2100	IF(OBJACT(X)) GO TO 350			! object handle?
	IF((OFLAG2(PRSO).AND.ACTRBT).NE.0) GO TO 2200 ! actor?
	I=602
	IF((OFLAG1(PRSO).AND.VICTBT).NE.0) I=888
	CALL RSPSUB(I,ODESC2(PRSO))		! no, joke.
2150	PRSCON=0				! disable cmd stream.
	GO TO 350
C
2200	SVPRSC=PRSCON				! save prscon.
	SVPRSO=PRSO				! save prso.
	PRSCON=1				! start of substring.
2300	WINNER=OACTOR(SVPRSO)			! new player.
	HERE=AROOM(WINNER)			! new location.
	PRSWON=PARSE(SUBBUF,SUBLNT,.TRUE.)	! parse command.
	IF(.NOT.PRSWON) GO TO 2600		! parse succeed?
C
	IF(AAPPLI(AACTIO(WINNER))) GO TO 2400	! actor handle?
	IF(XVEHIC(1)) GO TO 2400		! vehicle handle?
	IF((PRSO.EQ.VALUA).OR.(PRSO.EQ.EVERY).OR.(PRSO.EQ.POSSE)
	1	.OR.(PRSO.EQ.BUNOBJ)) GO TO 2900 ! collective object?
	IF(.NOT.VAPPLI(PRSA)) GO TO 2400	! verb handle?
2350	CALL RAPPLI(RACTIO(HERE))		! room action?
2400	IF(PRSCON-1) 2700,2550,2500		! parser reset?
2500	CALL XENDMV(TELFLG)			! more to do, end of move.
	GO TO 2300				! do next command.
C
2550	PRSCON=SVPRSC				! substring exhausted.
	GO TO 2700				! restore state.
C
2600	IF(OFLAG.NE.0) CALL RSPEAK(604)		! parse fails, orphans?
	OFLAG=0					! invalidate orphans.
2700	WINNER=PLAYER				! restore state.
	HERE=AROOM(WINNER)
	GO TO 400				! rejoin main loop.
C
2900	CALL VALUAC(PRSO)			! collective object.
	GO TO 2350
C
	END

C XENDMV-	Execute end of move functions.
C
C Declarations
C
	SUBROUTINE XENDMV(FLAG)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL F,CLOCKD,FLAG,XVEHIC
C
	IF(.NOT.FLAG) CALL RSPEAK(341)		! default remark.
	IF(THFACT) CALL THIEFD			! thief demon.
	IF(PRSWON.AND..NOT.DEADF) CALL FIGHTD	! fight demon.
	IF(SWDACT) CALL SWORDD			! sword demon.
	IF(PRSWON) F=CLOCKD(X)			! clock demon.
	IF(PRSWON) F=XVEHIC(2)			! vehicle readout.
	RETURN
	END

C XVEHIC- Execute vehicle function
C
C Declarations
C
	LOGICAL FUNCTION XVEHIC(N)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL OAPPLI
C
	XVEHIC=.FALSE.				! assume loses.
	AV=AVEHIC(WINNER)			! get vehicle.
	IF(AV.NE.0) XVEHIC=OAPPLI(OACTIO(AV),N)
	RETURN
	END

C INITFL-- DUNGEON file initialization subroutine
C
C Declarations
C
	LOGICAL FUNCTION INITFL(X)
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'dparam.for'
	LOGICAL PROTCT
	CHARACTER*1 KEDIT

C INITFL, PAGE 2
C
C First check for protection violation.
C
	INITFL=.FALSE.				! assume init fails.
	IF(PROTCT(X)) GO TO 10000		! protection violation?
	WRITE(OUTCH,10100)			! yes, throw him off.
10100	FORMAT(
	1 ' There appears before you a threatening figure clad all'/
	1 ' over in heavy black armor.  His legs seem like the massive'/
	2 ' trunk of the oak tree.  His broad shoulders and helmeted'/
	3 ' head loom high over your own puny frame, and you realize'/
	4 ' that his powerful arms could easily crush the very life'/
	5 ' from your body.  There hangs from his belt a veritable'/
	6 ' arsenal of deadly weapons: sword, mace, ball and chain'/
	7 ' dagger, lance, and trident.  He speaks with a commanding'/
	8 ' voice:'//20X,'"You shall not pass."'//
	9 ' As he grabs you by the neck all grows dim about you.')
	RETURN
C
C Now restore from existing index file.
C
10000	OPEN (UNIT=1,NAME='dindx',STATUS='OLD',READONLY,
	1	FORM='FORMATTED',ACCESS='SEQUENTIAL',ERR=1900)
	READ(1,130) I,J				! get version.
	READ(1,125) KEDIT			! get minor edit.
	IF((I.NE.VMAJ).OR.(J.NE.VMIN))
	1	GO TO 1925			! match to ours?
C
	OPEN (UNIT=DBCH,NAME='dtext',STATUS='OLD',READONLY,
	1	FORM='UNFORMATTED',ACCESS='DIRECT',
	2	RECL=RECLNT,ERR=1950)
C
	READ(1,130) MXSCOR,STRBIT,EGMXSC
	READ(1,130) RLNT,RDESC2,RDESC1,REXIT,RACTIO,RVAL,RFLAG
	READ(1,130) XLNT,TRAVEL
	READ(1,130) OLNT,ODESC1,ODESC2,ODESCO,OACTIO,OFLAG1,OFLAG2,
	1	OFVAL,OTVAL,OSIZE,OCAPAC,OROOM,OADV,OCAN,OREAD
	READ(1,130) R2LNT,O2,R2
	READ(1,130) CLNT,CTICK,CACTIO
	READ(1,135) CFLAG,CCNCEL
	READ(1,130) VLNT,VILLNS,VPROB,VOPPS,VBEST,VMELEE
	READ(1,130) ALNT,AROOM,ASCORE,AVEHIC,AOBJ,AACTIO,ASTREN,AFLAG
	READ(1,130) MBASE,MLNT,RTEXT
C
	CLOSE (UNIT=1)
	GO TO 1025				! init done.
C
125	FORMAT(A)
130	FORMAT(I8)
135	FORMAT(L4)

C INITFL, PAGE 3
C
C The internal data base is now established.
C Set up to play the game-- INITFL succeeds.
C
1025	CALL IDATE(SHOUR,SMIN,SSEC)		! get date (and toss).
	I=(SHOUR*64)+(SMIN*8)+SSEC		! first seed
	CALL ITIME(SHOUR,SMIN,SSEC)		! get time.
	J=(SHOUR*64)+(SMIN*8)+SSEC		! second seed
	CALL INIRND(I,J)			! init random number gen.
C
	WINNER=PLAYER
	THFPOS=OROOM(THIEF)
	BLOC=OROOM(BALLO)
	HERE=AROOM(WINNER)
	LASTIT=AOBJ(PLAYER)
C
	INITFL=.TRUE.
	RETURN
C
C Errors-- INITFL fails.
C
1900	WRITE(OUTCH,910)			! dindx.dat open err
	WRITE(OUTCH,980)
	RETURN
1925	WRITE(OUTCH,920) I,J,KEDIT,VMAJ,VMIN,VEDIT	! wrong dindx.dat ver
	WRITE(OUTCH,980)
	RETURN
1950	WRITE(OUTCH,960)			! dtext.dat open err
	WRITE(OUTCH,980)
	RETURN
910	FORMAT(' I can''t open "DINDX.DAT".')
920	FORMAT(' "DINDX.DAT" is version ',I1,'.',I1,A,'.'/
	1	' I require version ',I1,'.',I1,A,'.')
960	FORMAT(' I can''t open "DTEXT.DAT".')
980	FORMAT(
	1 ' Suddenly a sinister, wraithlike figure appears before you'/
	1 ' seeming to float in the air.  In a low, sorrowful voice he'/
	2 ' says, "Alas, the very nature of the world has changed, and'/
	3 ' the dungeon cannot be found.  All must now pass away."'/
	4 ' Raising his oaken staff in farewell, he fades into the'/
	5 ' spreading darkness.  In his place appears a tastefully'/
	6 ' lettered sign reading:'//20X,'INITIALIZATION FAILURE'//
	7 ' The darkness becomes all encompassing, and your vision fails.')
C
	END

C PROTCT-- Check for user violation
C
C This routine should be modified if you wish to add system
c dependant protection against abuse.
C
C At the moment, play is permitted under all circumstances.
C
	LOGICAL FUNCTION PROTCT(X)
	IMPLICIT INTEGER (A-Z)
C
	PROTCT=.TRUE.
	RETURN
	END
