C Parser for DUNGEON
C
C COPYRIGHT 1980, 1990, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA.
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK
C
C 20-Oct-94	RMS	Fixed bug in PUMP.  Added "am", "are" to buzzwords.
C 29-Sep-94	RMS	Fixed bugs in PLAY WITH, ALL BUT, GWIM, THISIT, IT.
C			Fixed vocabularly for ROCK, LIGHT, GATES, STACK,
C			BLIND.  Added COUNT, PERUSE, BLESSING, GHOSTS,
C			SPIRITS, CLIFFS, CORPSES, OUTPUT, CHIMNEY,
C			ZORKMID adjective, DIGBT flag.
C 30-Jan-94	RMS	Fixed bug in error message.
C 30-Jun-92	RMS	Changed file names to lower case.
C 29-Jun-92	RMS	Removed extraneous declaration from SPARSE.
C			Added dummy argument to SYNMCH.
C
C RDLINE-	Read input line
C
C Declarations
C
	SUBROUTINE RDLINE(INLINE,INLEN,WHO)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
	CHARACTER*(TEXLNT) INLINE
C
	LUCVT=ICHAR('A')-ICHAR('a')		! case conversion factor.
5	GO TO (90,10),WHO+1			! see who to prompt for.
10	WRITE(OUTCH,50)				! prompt for game.
50	FORMAT(' >',$)
C
90	READ(INPCH,100,END=5) INLINE		! get input.
100	FORMAT(A)
C
	INLEN=NBLEN(INLINE)			! len w/o trailing blanks.
	IF(INLEN.LE.0) GO TO 5			! anything left?
	DO 400 I=1,INLEN			! convert to upper case.
	  IF((INLINE(I:I).GE.'a').AND.(INLINE(I:I).LE.'z'))
	1	INLINE(I:I)=CHAR(ICHAR(INLINE(I:I))+LUCVT)
400	CONTINUE
	PRSCON=1				! restart lex scan.
	RETURN
	END

C PARSE-	Top level parse routine
C
C Declarations
C
C This routine details on bit 0 of PRSFLG
C
	LOGICAL FUNCTION PARSE(INLINE,INLEN,VBFLAG)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
	CHARACTER*(TEXLNT) INLINE
	CHARACTER*(WRDLNT) OUTBUF(LEXMAX),BAKBUF(LEXMAX)
	LOGICAL LEX,SYNMCH,DFLAG,VBFLAG
	SAVE BAKBUF,BAKLEN
	DATA BAKBUF(1)/'L'/,BAKLEN/1/
C
	DFLAG=(PRSFLG.AND.1).NE.0
	PARSE=.FALSE.				! assume fails.
	PRSA=0					! zero outputs.
	PRSI=0
	PRSO=0
C
	IF(.NOT.LEX(INLINE,INLEN,OUTBUF,OUTLEN,VBFLAG)) GO TO 1000
	IF((OUTLEN.NE.1).OR.(OUTBUF(1).NE.'AGAIN')) GO TO 100
	DO 50 I=1,LEXMAX			! use previous
	  OUTBUF(I)=BAKBUF(I)
50	CONTINUE
	OUTLEN=BAKLEN				! buffer and length.
100	IF(SPARSE(OUTBUF,OUTLEN,VBFLAG)) 1000,200,300	! do syn scan.
C
C Parse requires validation
C
200	IF(.NOT.VBFLAG) GO TO 350		! echo mode, force fail.
	IF(.NOT.SYNMCH(X)) GO TO 1000		! do syn match.
	IF(PRSO.EQ.BUNOBJ) LASTIT=BUNVEC(1)	! record for "it".
	IF((PRSO.GT.0).AND.(PRSO.LT.BUNOBJ)) LASTIT=PRSO
C
C Successful parse or successful validation
C
300	PARSE=.TRUE.
350	CALL ORPHAN(0,0,0,0,0,' ',0,0)		! clear orphans.
	DO 400 I=1,LEXMAX			! save command
	  BAKBUF(I)=OUTBUF(I)
400	CONTINUE
	BAKLEN=OUTLEN				! save length
	IF(DFLAG) WRITE(OUTCH,500) PARSE,PRSA,PRSO,PRSI
500	FORMAT(' PARSE RESULTS- ',L7,3I7)
	RETURN
C
C Parse fails, disallow continuation
C
1000	PRSCON=1
	IF(DFLAG) WRITE(OUTCH,500) PARSE,PRSA,PRSO,PRSI
	RETURN
C
	END

C LEX-	Lexical analyzer
C
C Declarations
C
C This routine details on bit 1 of PRSFLG
C
	LOGICAL FUNCTION LEX(INLINE,INLEN,OUTBUF,OP,VBFLAG)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
	CHARACTER*(TEXLNT) INLINE
	CHARACTER*(WRDLNT) OUTBUF(LEXMAX)
	CHARACTER*1 J
	LOGICAL DFLAG,VBFLAG
C
	DFLAG=(PRSFLG.AND.2).NE.0
	LEX=.FALSE.				! assume lex fails.
	OP=0					! output ptr.
	DO 10 I=1,LEXMAX			! clear output buf.
	  OUTBUF(I)=' '
10	CONTINUE
C
50	OP=OP+1					! adv output ptr.
	CP=0					! char ptr=0.
C
200	IF(PRSCON.GT.INLEN) GO TO 2000		! end of input?
	J=INLINE(PRSCON:PRSCON)			! no, get character,
	IF((J.EQ.'"').OR.(J.EQ.'''')) GO TO 3000! substring?
	PRSCON=PRSCON+1				! advance ptr.
	IF(J.EQ.' ') GO TO 1000			! space?
	IF((J.EQ.'.').OR.(J.EQ.';').OR.
	1  (J.EQ.'!').or.(J.EQ.'?')) GO TO 2000	! end of command?
	IF(J.EQ.',') GO TO 4000			! comma?
	IF(OP.GT.LEXMAX) GO TO 5000		! too many tokens?
	CP=CP+1					! adv char ptr.
	IF(CP.LE.WRDLNT) OUTBUF(OP)(CP:CP)=J	! insert char in word.
	GO TO 200
C
C Space.
C
1000	IF(CP.EQ.0) GO TO 200			! any word yet?
	GO TO 50				! yes, adv op.
C
C End of input, see if partial word available.
C
2000	IF(PRSCON.GT.INLEN) PRSCON=1		! force parse restart.
	IF((CP.EQ.0).AND.(OP.EQ.1)) RETURN	! any results?
	IF(CP.EQ.0) OP=OP-1			! any last word?
	LEX=.TRUE.
	IF(DFLAG) WRITE(OUTCH,2020) CP,OP,PRSCON,(OUTBUF(I),I=1,OP)
2020	FORMAT(' LEX RESULTS- ',3I7/1X,8(A,1X))
	RETURN
C
C Substring, J is delimiter.
C
3000	IF(SUBLNT.NE.0) GO TO 3400		! already got one?
3100	PRSCON=PRSCON+1				! skip initial quote.
	IF(PRSCON.GT.INLEN) GO TO 3500		! any more characters?
	IF(INLINE(PRSCON:PRSCON).EQ.' ') GO TO 3100	! skip blanks.
	K=INDEX(INLINE(PRSCON:INLEN),J)		! find closing quote.
	IF(K.LE.1) GO TO 3500			! none or empty?
	SUBBUF=INLINE(PRSCON:PRSCON+K-2)	! set up substring buffer,
	SUBLNT=K-1				! length.
	PRSCON=PRSCON+K				! skip over string.
	IF(DFLAG) WRITE(OUTCH,3030) SUBLNT,SUBBUF(1:SUBLNT)
3030	FORMAT(' SUBSTRING- ',I7,' "',A,'"')
	GO TO 1000				! treat as end of word.
C
3400	IF(VBFLAG) CALL RSPEAK(1046)		! multiple substrings.
	RETURN
C
3500	IF(VBFLAG) CALL RSPEAK(616)		! bad substring.
	RETURN					! fails.
C
C Comma.
C
4000	IF(CP.NE.0) OP=OP+1			! if partial word, go to next.
	IF(OP.EQ.1) GO TO 4500			! no first word? die.
	IF(OP.GT.LEXMAX) GO TO 5000		! too many tokens?
	OUTBUF(OP)='AND'			! insert 'AND'.
	GO TO 50				! start new word
C
4500	IF(VBFLAG) CALL RSPEAK(1047)		! misplaced comma.
	RETURN
C
C Too many tokens.
C
5000	IF(VBFLAG) CALL RSPEAK(1048)		! too many tokens.
	RETURN
C
	END

C SPARSE-	Start of parse
C
C Declarations
C
C This routine details on bit 2 of PRSFLG
C
	INTEGER FUNCTION SPARSE(LBUF,LLNT,VBFLAG)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
	CHARACTER*(WRDLNT) LBUF(LEXMAX),WORD,LCWORD,LCIFY
	CHARACTER*(WRDLNT+2) LCWRD1
	LOGICAL LIT,DFLAG,VBFLAG,ANDFLG,BUNFLG
	INTEGER OBJVEC(2),PRPVEC(2)
	EQUIVALENCE (OBJVEC(1),OBJ1),(PRPVEC(1),PREP1)

C SPARSE, PAGE 2
C
C Vocabularies
C
C Buzz words--	ignored in syntactic processing
C
	DATA BWORD/'BY','IS','A','AN','THE','AM','ARE',
	1	'TODAY','MY','YOUR','OUR','HIS'/
C
C Prepositions--	maps prepositions to indices
C
	DATA PWORD/'OVER','WITH','USING','AT','TO',
	1	'IN','INSIDE','INTO','DOWN','UP',
	2	'UNDER','OF','ON','OFF','FOR',
	3	'FROM','OUT','THROUGH',' ',' '/
C
	DATA PVOC/1,2,2,3,4,
	1	5,5,5,6,7,
	2	8,9,10,11,12,
	3	13,13,14,0,0/
C
C Directions--	maps directions to indices
C
	DATA DWORD/'N','NORTH','S','SOUTH',
	1 'E','EAST','W','WEST',
	2 'SE','SW','NE','NW',
	4 'U','UP','D','DOWN',
	5 'LAUNCH','LAND','EXIT','OUT',
	6 'TRAVEL','IN','CROSS',' ',' '/
C
	DATA DVOC/XNORTH,XNORTH,XSOUTH,XSOUTH,
	1 XEAST,XEAST,XWEST,XWEST,
	2 XSE,XSW,XNE,XNW,
	4 XUP,XUP,XDOWN,XDOWN,
	5 XLAUN,XLAND,XEXIT,XEXIT,
	6 XCROSS,XENTER,XCROSS,0,0/

C SPARSE, PAGE 3
C
C Adjectives--	maps adjectives to object numbers
C
C Each string entry in aword corresponds to a list of one or more
C object numbers in AVOC.  Object entries are delimited by the first
C object being positive, and all subsequent objects in the same entry
C being negative.
C
	DATA (AWORD(I),I=1,40) /
	1 'BROWN','ELONGATE','HOT','PEPPER',
	1 'VITREOUS','JADE','HUGE','ENORMOUS',
	2 'TROPHY','CLEAR','LARGE','NASTY',
	3 'ELVISH','BRASS','BROKEN','ORIENTAL',
	4 'BLOODY','RUSTY','BURNED-O','DEAD',
	5 'OLD','LEATHER','PLATINUM','PEARL',
	6 'MOBY','CRYSTAL','GOLD','IVORY',
	7 'SAPPHIRE','WOODEN','WOOD','STEEL',
	8 'DENTED','FANCY','ANCIENT','SMALL',
	9 'BLACK','TOUR','VISCOUS','VICIOUS'/
C
	DATA (AVOC(I),I=1,112) /
	1 1,-81,-133,1,3,-190,3,
	1 4,6,8,8,-122,
	2 9,10,12,-26,-47,-95,-96,-123,-133,-135,-144,-145,
	2	-150,-176,-191,13,-19,
	3 14,15,-16,-46,-156,-190,16,-22,-38,-92,-113,-155,-158,17,
	4 20,24,-205,22,22,
	5 25,-41,-44,-45,-208,25,26,27,
	6 31,32,-126,-206,-209,33,-85,-104,-157,-158,-188,34,
	7 37,38,-67,-75,-93,-136,-137,-165,-173,-174,-175,-197,-204,
	7	38,-67,-136,-137,-165,-173,-174,-175,
	7	39,-105,-124,-125,-189,
	8 39,40,41,-44,5,-46,-52,-53,-89,-102,-103,-153,-187,
	9 47,-162,49,55,62/
C
	DATA (AWORD(I),I=41,80) /
	1 'GLASS','TRAP','FRONT','STONE',
	1 'MANGLED','RED','YELLOW','BLUE',
	2 'VAMPIRE','MAGIC','SEAWORTH','TAN',
	3 'SHARP','WICKER','CLOTH','BRAIDED',
	4 'GAUDY','SQUARE','CLAY','SHINY',
	5 'THIN','GREEN','PURPLE','WHITE',
	6 'MARBLE','COKE','EMPTY','ROUND',
	7 'TRIANGUL','RARE','OBLONG','EAT-ME',
	8 'EATME','ORANGE','ECCH','ROCKY',
	9 'SHEER','200','NEAT','SHIMMERI'/
C
	DATA (AVOC(I),I=113,179) /
	1 10,-126,-132,-206,-209,66,68,69,-150,-278,
	1 	72,-124,79,-94,-140,-161,-170,-171,-190,-209,
	1	80,-159,82,-112,-114,-141,-206,
	2 83,90,-281,90,91,
	3 92,98,100,101,
	4 108,109,-127,109,110,
	5 110,77,-115,-143,116,117,-126,-147,-160,-266,
	6 119,121,121,128,
	7 129,134,135,138,
	8 138,139,141,146,
	9 146,148,148,151/
C
	DATA (AWORD(I),I=81,120) /
	1 'ZURICH','BIRDS','ENCRUSTE','BEAUTIFU',
	1 'CLOCKWOR','MECHANIC','MAHOGANY','PINE',
	2 'LONG','CENTER','SHORT','T',
	3 'COMPASS','BRONZE','CELL','LOCKED',
	4 'SUN','BARE','SONG','NORTH',
	5 'NORTHERN','SOUTH','SOUTHERN','EAST',
	6 'EASTERN','WEST','WESTERN','DUNGEON',
	7 'FREE','GRANITE','LOWERED','VOLCANO',
	8 'MAN-SIZE','METAL','PLASTIC','SILVER',
	9 'USED','USELESS','SEEING','ONE-EYED'/
C
	DATA (AVOC(I),I=180,238) /
	1 152,153,-154,-155,154,-155,86,-156,
	1 157,-158,157,-158,163,164,
	2 166,166,167,168,
	3 169,-275,172,174,-175,174,
	4 177,259,267,269,
	5 269,270,270,271,
	6 271,67,-272,67,-272,279,
	7 195,-262,265,36,111,
	8 93,64,-99,-200,-201,77,-87,-88,-90,59,
	9 22,22,126,-206,-209,58/
C
	DATA (AWORD(I),I=121,160) /
	1 'HOLY','HAND-HEL','UNRUSTY','PLAIN',
	1 'PRICELES','SANDY','GIGANTIC','LINE-PRI',
	2 'FLATHEAD','FINE','SHADY','SUSPICIO',
	3 'CROSS','TOOL','CONTROL','DON',
	4 'WOODS','GOLDEN','OAK','BARRED',
	5 'DUSTY','NARROW','IRON','WELCOME',
	6 'RUBBER','SKELETON','ALL','ZORKMID',
	7 12*' '/
C
	DATA (AVOC(I),I=239,282) /
	1 43,89,13,13,
	1 104,192,122,122,
	2 118,91,61,61,
	3 165,193,194,196,
	4 196,157,-158,197,198,-210,
	5 204,199,205,207,
	6 207,23,253,-254,104,-148,
	7 12*0/

C SPARSE, PAGE 4
C
C OBJECTS--	Maps objects to object indices,
C 		same format as AVOC.
C
	DATA (OWORD(I),I=1,40) /
	1 'BAG','SACK','GARLIC','CLOVE',
	1 'FOOD','SANDWICH','LUNCH','DINNER',
	2 'GUNK','PIECE','SLAG','COAL',
	3 'PILE','HEAP','FIGURINE','MACHINE',
	4 'PDP10','VAX','DRYER','LID',
	5 'DIAMOND','CASE','BOTTLE','CONTAINE',
	6 'WATER','QUANTITY','LIQUID','H2O',
	7 'ROPE','HEMP','COIL','KNIFE',
	8 'BLADE','SWORD','ORCHRIST','GLAMDRIN',
	9 'LAMP','LANTERN','RUG','CARPET'/
C
	DATA (OVOC(I),I=1,71) /
	1 1,-25,-100,1,2,2,
	1 3,3,3,3,
	2 4,-55,4,-143,-186,-282,4,5,
	3 5,-18,-38,-72,-73,-87,-88,-122,-148,5,6,7,
	4 7,7,7,7,-200,-201,
	5 8,9,-123,10,-121,10,
	6 11,-273,11,-273,11,-273,11,-273,
	7 12,-101,-282,12,12,-110,13,-24,
	8 13,-14,14,14,14,
	9 15,-16,-22,15,-16,-22,17,17/
C
	DATA (OWORD(I),I=41,80) /
	1 'LEAVES','LEAF','TROLL','AXE',
	1 'PRAYER','KEYS','KEY','SET',
	2 'BONES','SKELETON','BODY','COINS',
	3 'BAR','NECKLACE','PEARLS','MIRROR',
	4 'ICE','MASS','GLACIER','RUBY',
	5 'TRIDENT','FORK','COFFIN','CASKET',
	6 'TORCH','CAGE','DUMBWAIT','BASKET',
	7 'BRACELET','JEWEL','TIMBER','BOX',
	8 'STRADIVA','VIOLIN','ENGRAVIN','INSCRIPT',
	9 'GHOST','SPIRIT','FIEND','GRAIL'/
C
	DATA (OVOC(I),I=72,130) /
	1 18,18,19,-111,20,
	1 44,-47,23,23,-205,23,
	2 21,21,21,-72,-73,25,
	3 26,-165,-168,27,27,28,-29,-276,
	4 30,30,30,31,
	5 32,32,33,33,
	6 34,35,-36,-124,-125,35,-36,35,-36,-98,-113,
	7 37,37,38,39,-53,-105,
	8 40,40,41,41,-44,
	9 42,42,42,43/
C
	DATA (OWORD(I),I=81,120) /
	1 'TRUNK','CHEST','BELL','BOOK',
	1 'BIBLE','GOODBOOK','CANDLES','PAIR',
	2 'GUIDEBOO','GUIDE','PAPER','NEWSPAPE',
	3 'ISSUE','REPORT','MAGAZINE','NEWS',
	4 'MATCHBOO','MATCH','MATCHES','ADVERTIS',
	5 'PAMPHLET','LEAFLET','BOOKLET','MAILBOX',
	6 'TUBE','TOOTHPAS','PUTTY','MATERIAL',
	7 'GLUE','WRENCH','SCREWDRI','CYCLOPS',
	8 'MONSTER','CHALICE','CUP','GOBLET',
	9 'PAINTING','ART','CANVAS','PICTURE'/
C
	DATA (OVOC(I),I=131,182) /
	1 45,45,-193,46,-190,47,-49,-114,-115,-116,-117,
	1 47,47,48,48,
	2 49,49,50,-122,-143,-186,50,
	3 50,50,50,50,
	4 51,51,51,52,
	5 52,52,52,53,
	6 54,54,55,55,
	7 55,56,57,58,
	8 58,59,59,59,
	9 60,-149,60,-149,60,60/
C
	DATA (OWORD(I),I=121,160) /
	1 'WORK','MASTERPI','THIEF','ROBBER',
	1 'CRIMINAL','BANDIT','CROOK','GENT',
	2 'GENTLEMA','MAN','INDIVIDU','BAGMAN',
	3 'STILETTO','WINDOW','BOLT','NUT',
	4 'GRATE','GRATING','DOOR','TRAP-DOO',
	5 'SWITCH','HEAD','CORPSE','BODIES',
	6 'DAM','GATES','GATE','FCD',
	7 'RAIL','RAILING','BUTTON','BUBBLE',
	8 'LEAK','DRIP','HOLE','BAT',
	9 'RAINBOW','POT','STATUE','SCULPTUR'/
C
	DATA (OVOC(I),I=183,258) /
	1 60,60,61,61,
	1 61,61,61,61,
	2 61,61,61,61,
	3 62,63,-198,-210,64,64,
	4 65,65,66,-67,-68,-69,-119,-164,
	4	-172,-173,-174,-175,-189,-197,66,
	5 70,-79,-80,-81,-82,-170,71,-120,72,-73,72,-73,
	6 74,74,-76,74,-76,74,
	7 75,75,76,-79,-80,-81,-82,-127,-128,-129,-170,-176,77,
	8 78,-191,78,78,-107,-202,-203,83,
	9 84,85,86,86/
C
	DATA (OWORD(I),I=161,200) /
	1 'ROCK','BOAT','PLASTIC','PUMP',
	1 'AIRPUMP','AIR-PUMP','LABEL','FINEPRIN',
	2 'STICK','BARREL','BUOY','EMERALD',
	3 'SHOVEL','GUANO','CRAP','SHIT',
	4 'HUNK','BALLOON','RECEPTAC','WIRE',
	5 'HOOK','ZORKMID','COIN','SAFE',
	6 'CARD','NOTE','SLOT','CROWN',
	7 'BRICK','FUSE','GNOME','STAMP',
	8 'TOMB','CRYPT','GRAVE','HEADS',
	9 'POLES','IMPLEMEN','LOSERS','COKES'/
C
	DATA (OVOC(I),I=259,312) /
	1 86,87,-88,-90,87,-88,-90,89,
	1 89,89,91,-112,91,
	2 92,93,94,95,
	3 96,97,97,97,
	4 97,98,-113,99,101,-110,
	5 102,-103,104,-148,104,105,
	6 106,-188,106,-186,107,-187,108,
	7 109,110,111,-152,118,-196,
	8 119,119,119,120,
	9 120,120,120,121/
C
	DATA (OWORD(I),I=201,240) /
	1 'LISTINGS','OUTPUT','PRINTOUT','SPHERE',
	1 'BALL','ETCHING','WALLS','WALL',
	2 'FLASK','POOL','SEWAGE','TIN',
	3 'SAFFRON','SPICES','TABLE','POST',
	4 'POSTS','BUCKET','CAKE','ICING',
	5 'ROBOT','ROBBY','C3PO','R2D2',
	6 'PANEL','POLE','TBAR','T-BAR',
	7 'ARROW','POINT','BEAM','DIAL',
	8 'SUNDIAL','1','ONE','2',
	9 'TWO','3','THREE','4'/
C
	DATA (OVOC(I),I=313,387) /
	1 122,122,122,126,-206,-209,
	1 126,130,-131,130,-131,-257,130,-131,-159,
	1	-160,-161,-162,-163,-164,-257,-265,-269,-270,-271,-272,
	2 132,133,133,134,
	3 134,134,135,-204,136,-166,-167,
	4 136,137,138,-139,-140,-141,139,-140,-141,
	5 142,142,142,142,
	6 159,-160,-161,-162,-163,-164,-194,-277,120,-166,-167,168,168,
	7 169,169,171,177,
	8 177,178,178,179,
	9 179,180,180,181/
C
	DATA (OWORD(I),I=241,280) /
	1 'FOUR','5','FIVE','6',
	1 'SIX','7','SEVEN','8',
	2 'EIGHT','WARNING','SLIT','IT',
	3 'THAT','THIS','ME','MYSELF',
	4 'CRETIN','ALL','EVERYTHI','TREASURE',
	5 'VALUABLE','SAILOR','TEETH','GRUE',
	6 'HAND','HANDS','LUNGS','AIR',
	7 'AVIATOR','FLYER','TREE','CLIFF',
	8 'LEDGE','PORTRAIT','STACK','BILLS',
	9 'VAULT','CUBE','LETTERIN','CURTAIN'/
C
	DATA (OVOC(I),I=388,432) /
	1 181,182,182,183,
	1 183,184,184,185,
	2 185,186,187,250,
	3 250,250,251,251,
	4 251,252,252,253,
	5 253,255,256,258,
	6 259,259,260,260,
	7 261,261,144,-145,-268,146,-147,
	8 146,149,122,-148,148,
	9 150,150,67,-150,151/
C
	DATA (OWORD(I),I=281,320) /
	1 'LIGHT','NEST','EGG','BAUBLE',
	1 'CANARY','BIRD','SONGBIRD','GUARD',
	2 'GUARDIAN','ROSE','STRUCTUR','CHANNEL',
	3 'KEEPER','LADDER','BROCHURE','WISH',
	4 'GROUND','EARTH','SAND','WELL',
	5 'SLIDE','CHUTE','HOUSE','BOTTLES',
	6 'BUNCH','PALANTIR','STONE','FLINT',
	7 'POSSESSI','GOOP','BEACH','GRIP',
	8 'HANDGRIP','PRINT','ETCHINGS','CRACK',
	9 'KEYHOLE','MAT','STOVE','PLATINUM'/
C
	DATA (OVOC(I),I=433,485) /
	1 15,-151,-171,153,154,-155,156,
	1 157,-158,267,267,274,
	2 274,275,276,278,
	3 279,280,195,-262,263,
	4 264,264,192,-264,281,
	5 283,283,266,121,
	6 121,126,-206,-209,126,-206,-209,51,
	7 254,133,192,167,
	8 167,91,-122,130,-131,199,
	9 202,-203,207,208,26/
C
	DATA (OWORD(I),I=321,360) /
	1 'HIM','SELF','GOLD','SAPPHIRE',
	1 'IVORY','MASTER','CANDLE','JADE',
	2 'SCREEN','BLESSING','GHOSTS','SPIRITS',
	3 'CORPSES','JEWELS','CLIFFS','CHIMNEY',
	4 24*' '/
C
	DATA (OVOC(I),I=486,529) /
	1 250,251,85,-104,37,
	1 34,279,48,6,
	2 151,263,42,42,
	3 72,-73,37,-45,146,-147,211,
	4 24*0/

C SPARSE, PAGE 5
C
C VERBS--	Maps verbs to syntax slots
C
C Vocabulary entries are variable length and consist of one
C or more words.  If an entry contains more than one word,
C all but the last are prefaced with an '*'.  The preferred
C string for error messages should be first.
C
C Syntax entries consist of a flag word followed by 0, 1, or 2
C Object descriptions.  The flag word has the following format--
C
C bit <14>	if 1, syntax includes direct object
C bit <13>	if 1, syntax includes indirect object
C bit <12>	if 1, direct object is implicit (standard form)
C bit <11>	if 1, direct and indirect object must be swapped
C			after syntax processing
C bit <10>	if 1, this is default syntax for orphanery
C bits <8:0>	verb number for VAPPLI
C
C Object descriptions consist of a flag word and two FWIM words.
C The flag word has the following format--
C
C bit <14>	if 1, search adventurer for object
C bit <13>	if 1, search room for object
C bit <12>	if 1, parser will try to take object
C bit <11>	if 1, adventurer must have object
C bit <10>	if 1, qualifying bits (normally -1,-1) are same
C			as FWIM bits
C bit <9>	if 1, object must be reachable
C bits <8:0>	preposition number for SYNMCH
C
C The FWIM words have the same format as the two object flag words.
C
C Note that bits 12 and 11 of object descriptions actually have
C four distinct states--
C
C	bit 12	bit 11	mdldesc		interpretation
C	------	------	-------		---------------
C
C	  0	  0	 --		no parser action
C	  0	  1	 HAVE		adventurer must have object
C	  1	  0	 TRY		try to take, dont care if fail
C	  1	  1	 TAKE		try to take, care if fail
C

C SPARSE, PAGE 6
C
	DATA (VWORD(I),I=1,43) /
	1 'BRIEF','VERBOSE','SUPERBRI','STAY',
	1 'VERSION','*SWIM','*BATHE','WADE',
	2 'GERONIMO','*ULYSSES','ODYSSEUS','*PLUGH','XYZZY',
	3 'PRAY','TREASURE','TEMPLE','BLAST',
	4 'SCORE','*QUIT','*GOODBYE','*Q','BYE','HELP',
	5 'INFO','*HISTORY','UPDATE','BACK',
	6 '*MUMBLE','SIGH','*CHOMP','*LOSE',
	7 'BARF','DUNGEON','FROBOZZ','*FOO',
	8 '*BLETCH','BAR','REPENT','*HOURS',
	9 'SCHEDULE','WIN','*YELL','*SCREAM'/
C
	DATA (VVOC(I),I=1,54) /
	1 1,70,1,71,1,72,1,73,
	1 1,74,1,75,
	2 1,76,1,77,1,56,
	3 1,79,1,80,1,81,1,82,
	4 1,83,1,84,1,40,
	5 1,41,1,42,1,43,
	6 1,44,
	7 1,45,1,46,1,47,
	8 1,48,1,49,
	9 1,50,1,51/
C
	DATA (VWORD(I),I=44,86) /
	1 'SHOUT','*HOP','SKIP','*CURSE',
	1 '*SHIT','*DAMN','FUCK','ZORK',
	2 'WISH','SAVE','RESTORE','TIME',
	3 'DIAGNOSE','EXORCISE','*LIST','*I','INVENTOR',
	4 'WAIT','INCANT','*ANSWER','RESPOND','AGAIN',
	5 'NOOBJ','*BUG','*GRIPE','COMPLAIN',
	6 '*FEATURE','*COMMENT','*IDEA','SUGGESTI',
	7 'ROOM','*OBJECTS','OBJ','RNAME','DEFLATE',
	8 '*EXAMINE','*WHAT','DESCRIBE','FILL',
	9 '*FIND','*SEEK','*WHERE','SEE'/
C
	DATA (VVOC(I),I=55,120) /
	1 1,52,1,53,
	1 1,54,1,55,
	2 1,169,1,149,1,150,1,90,
	3 1,94,1,105,1,133,
	4 1,128,1,95,1,96,1,57,
	5 1,58,1,59,
	6 1,60,
	7 1,65,1,66,1,67,1,'50147'O,
	8 4,'40170'O,'60000'O,-1,-1,
	8 11,'60206'O,'61000'O,'200'O,0,'61002'O,-1,-1,
	8	'40206'O,'61000'O,'200'O,0,
	9 4,'40177'O,'60000'O,-1,-1/
C
	DATA (VWORD(I),I=87,131) /
	1 'FOLLOW','*KICK','*BITE','TAUNT',
	1 'LOWER','*PUSH','PRESS','*RING',
	2 'PEAL','*RUB','*FEEL','*CARESS','*TOUCH',
	3 'FONDLE','SHAKE','SPIN','*UNTIE',
	4 'FREE','*WALK','*RUN','*PROCEED','GO','*ATTACK','*FIGHT',
	5 '*INJURE','*HIT','HURT','BOARD',
	6 '*BRUSH','CLEAN','*BURN','*IGNITE',
	7 'INCINERA','CLIMB','CLOSE','DIG',
	8 'DISEMBAR','*DRINK','*IMBIBE','SWALLOW',
	9 '*DROP','RELEASE','*EAT','*GOBBLE','*CONSUME'/
C
	DATA (VVOC(I),I=121,278) /
	1 2,'125'O,'50125'O,1,'50153'O,
	1 1,'50156'O,9,'50160'O,'40160'O,'61012'O,-1,-1,
	1	'40241'O,'61010'O,-1,-1,
	2 5,'52127'O,'70127'O,'61002'O,-1,-1,
	3 1,'50157'O,1,'50171'O,1,'50201'O,
	4 11,'42161'O,'61000'O,0,'10000'O,
	4	'60242'O,'61000'O,0,'10000'O,'61015'O,-1,-1,
	4 9,'50216'O,'40126'O,'61016'O,-1,-1,'40126'O,'61005'O,-1,-1,
	5 7,'60215'O,'21000'O,0,'200'O,'44002'O,0,'1000'O,
	5 4,'40202'O,'21000'O,0,2,
	6 5,'52130'O,'70130'O,'61002'O,-1,-1,
	7 7,'60211'O,'61000'O,'20'O,0,'64002'O,'10'O,0,
	7 12,'40235'O,'20007'O,0,'4000'O,'40236'O,'20006'O,0,'4000'O,
	7	'40234'O,'20000'O,0,'4000'O,
	7 4,'40176'O,'61000'O,'10200'O,0,
	7 21,'60131'O,'20005'O,0,'40000'O,'44002'O,4,0,
	7 	'60131'O,'20016'O,0,'40000'O,'44002'O,4,0,
	7 	'60131'O,'20000'O,0,'40000'O,'44002'O,4,0,
	8 8,'40203'O,'20000'O,0,2,'40203'O,'20015'O,0,2,
	8 4,'40210'O,'61000'O,'400'O,0,
	9 25,'42221'O,'41000'O,-1,-1,
	9	'60220'O,'41000'O,-1,-1,'61005'O,-1,-1,
	9	'60220'O,'41000'O,-1,-1,'61006'O,-1,-1,
	9	'60220'O,'41000'O,-1,-1,'61016'O,-1,-1/
C
	DATA (VWORD(I),I=132,172) /
	1 '*MUNCH','TASTE','*DOUSE','EXTINGUI',
	1 '*GIVE','*HAND','DONATE','*HELLO',
	2 'HI','BLOW','INFLATE','*JUMP',
	3 'LEAP','*KILL','*MURDER','*SLAY',
	4 '*STAB','DISPATCH','*KNOCK','RAP',
	5 'LIGHT','LOCK','*LOOK','*L','*STARE',
	6 'GAZE','*MELT','LIQUIFY','MOVE',
	7 '*PULL','TUG','*DESTROY','*MUNG',
	8 '*BREAK','DAMAGE','OPEN','PICK',
	9 '*PLUG','*GLUE','PATCH','*POKE'/
C
	DATA (VVOC(I),I=279,450) /
	1 4,'40207'O,'75000'O,'2000'O,0,
	1 4,'40174'O,'75000'O,'100'O,0,
	1 11,'72222'O,'21004'O,'40'O,0,'64222'O,'21000'O,'40'O,0,
	1	'61000'O,-1,-1,
	2 2,'2227'O,'50227'O,
	2 15,'62146'O,'61007'O,-1,-1,'61002'O,4,0,
	2	'40122'O,'61007'O,-1,-1,'40165'O,'61005'O,-1,-1,
	2 4,'70146'O,'61002'O,4,0,
	3 5,'133'O,'40133'O,'61001'O,-1,-1,
	4 7,'60213'O,'21000'O,0,'200'O,'44002'O,0,'1000'O,
	4 12,'42166'O,'61003'O,-1,-1,'40166'O,'61012'O,-1,-1,
	4	'40215'O,'23006'O,'40'O,0,
	5 11,'42173'O,'75000'O,'100'O,0,'60211'O,'61000'O,'100'O,0,
	5	'54002'O,'10'O,0,
	5 7,'60134'O,'20000'O,-1,-1,'74002'O,4,0,
	6 31,'167'O,'40170'O,'60003'O,-1,-1,'40231'O,'61010'O,-1,-1,
	6	'40230'O,'60005'O,-1,-1,'40230'O,'60016'O,-1,-1,
	6	'60144'O,'60003'O,-1,-1,'61002'O,-1,-1,
	6	'60144'O,'60003'O,-1,-1,'61016'O,-1,-1,
	6 4,'70145'O,'61002'O,'10'O,0,
	6 4,'40172'O,'20000'O,-1,-1,
	7 8,'42172'O,'21000'O,-1,-1,'40172'O,'21012'O,-1,-1,
	8 5,'52212'O,'70212'O,'44002'O,-1,-1,
	8 11,'42175'O,'61000'O,'10200'O,0,'60175'O,'61000'O,'10200'O,0,
	8	'54002'O,4,'1000'O,
	8 4,'40204'O,'61007'O,'20000'O,'40'O,
	9 4,'70152'O,'61002'O,-1,-1/
C
	DATA (VWORD(I),I=173,212) /
	1 '*BLIND','JAB','*POUR','SPILL',
	1 'PUMP','*PUT','*INSERT','*STUFF',
	2 'PLACE','*RAISE','LIFT','*READ',
	3 '*PERUSE','SKIM','STRIKE','*SWING',
	4 'THRUST','*TAKE','*HOLD','*CARRY',
	5 'REMOVE','*TELL','*COMMAND','REQUEST',
	6 '*THROW','*HURL','CHUCK','*TIE',
	7 'FASTEN','*TURN','SET','UNLOCK',
	8 '*WAKE','*ALARM','*STARTLE','SURPRISE',
	9 '*WAVE','*FLAUNT','BRANDISH','WIND'/
C
	DATA (VVOC(I),I=451,654) /
	1 7,'60212'O,'21000'O,0,'200'O,'44002'O,0,'1000'O,
	1 25,'42223'O,'41000'O,'400'O,0,
	1	'60223'O,'41000'O,'400'O,0,'61005'O,-1,-1,
	1	'60223'O,'41000'O,'400'O,0,'61016'O,-1,-1,
	1	'60240'O,'41000'O,'400'O,0,'61012'O,-1,-1,
	1 4,'40232'O,'60007'O,-1,-1,
	2 16,'72220'O,'61005'O,-1,-1,'70220'O,'61016'O,-1,-1,
	2	'40221'O,'61006'O,-1,-1,'70241'O,'61010'O,-1,-1,
	2 5,'52155'O,'40155'O,'61007'O,-1,-1,
	3 18,'42144'O,'71000'O,'40000'O,0,
	3	'60144'O,'71000'O,'40000'O,0,'61002'O,-1,-1,
	3	'60144'O,'71000'O,'40000'O,0,'61016'O,-1,-1,
	3 12,'60215'O,'23000'O,'40'O,0,'44002'O,0,'1000'O,
	3	'42215'O,'23000'O,'40'O,0,'50173'O,
	4 7,'60214'O,'44000'O,0,'1000'O,'21003'O,0,'200'O,
	5 11,'42204'O,'61000'O,'20000'O,'40'O,
	5	'60204'O,'61000'O,'20000'O,0,'61015'O,-1,-1,
	5 4,'40217'O,'20000'O,0,'2000'O,
	6 21,'62224'O,'44000'O,-1,-1,'21003'O,'40'O,0,
	6	'60224'O,'44000'O,-1,-1,'21016'O,'40'O,0,
	6	'60220'O,'44000'O,-1,-1,'61005'O,-1,-1,
	7 11,'70162'O,'61004'O,-1,-1,'60163'O,'21007'O,'40'O,0,
	7	'65002'O,4,0,
	7 22,'62164'O,'61000'O,2,0,'64002'O,4,0,
	7	'40173'O,'75012'O,'100'O,0,'40174'O,'75013'O,'100'O,0,
	7	'60237'O,'61000'O,2,0,'20004'O,-1,-1,
	7 7,'60135'O,'21000'O,-1,-1,'74002'O,4,0,
	8 8,'42150'O,'20000'O,'40'O,0,'40150'O,'20007'O,'40'O,0,
	9 4,'40154'O,'40000'O,-1,-1,
	9 5,'50233'O,'40233'O,'61007'O,-1,-1/
C
	DATA (VWORD(I),I=213,240)/
	1 'ENTER','LEAVE','*MAKE','BUILD',
	1 '*OIL','*GREASE','LUBRICAT','PLAY',
	2 'SEND','SLIDE','*SMELL','SNIFF',
	3 'SQUEEZE','GET','COUNT',13*' '/
C
	DATA (VVOC(I),I=655,722) /
	1 2,167,'50126'O,2,168,'50220'O,1,'50243'O,
	1 4,'70244'O,'41002'O,-1,-1,
	1 5,'50245'O,'70245'O,'75002'O,4,0,
	2 4,'40246'O,'61014'O,-1,-1,
	2 4,'70241'O,'61010'O,-1,-1,1,'50105'O,
	3 1,'50104'O,19,'42204'O,'61000'O,'20000'O,'40'O,
	3	'40202'O,'21005'O,0,2,'40203'O,'21015'O,0,2,
	3	'60204'O,'61000'O,'20000'O,'40'O,'61015'O,-1,-1,
	3 1,'50141'O,13*0/

C SPARSE, PAGE 7
C
C Set up for parsing
C
	SPARSE=-1				! assume parse fails.
	ADJ=0					! clear parts holders.
	ACT=0
	PREP=0
	PPTR=0
	OBJ1=0
	OBJ2=0
	PREP1=0
	PREP2=0
	LOBJ=0
	ANDFLG=.FALSE.
	BUNFLG=.FALSE.
	DFLAG=(PRSFLG.AND.4).NE.0

C SPARSE, PAGE 8
C
C Now loop over input buffer of lexical tokens.
C
	I=0
10	I=I+1					! do 1000 i=1,llnt
	  WORD=LBUF(I)				! get current token.
	  ERRVOC=0				! assume won't find
	  IF(WORD.EQ.' ') GO TO 1000		! blank? ignore.
	  IF(WORD.EQ.'AND') GO TO 1500		! 'AND'?
	  IF((WORD.EQ.'EXCEPT').OR.(WORD.EQ.'BUT')) GO TO 2500
C
C Check for buzz word
C
	  DO 50 J=1,BWMAX
	    IF(WORD.EQ.BWORD(J)) GO TO 1000	! if match, ignore.
50	  CONTINUE
C
C Check for action or direction
C
	  J=1					! check for action.
	  DO 70 K=1,VWMAX
	    IF(VWORD(K)(1:1).EQ.'*') GO TO 65	! synonym?
	    IF(WORD.EQ.VWORD(K)) GO TO 2000	! match to base word?
	    J=J+VVOC(J)+1			! skip over syntax.
	    GO TO 70
65	    IF(WORD.EQ.VWORD(K)(2:WRDLNT)) GO TO 2000 ! synonym match?
70	  CONTINUE
C
75	  IF((ADJ.NE.0).OR.(PREP.NE.0).OR.(OBJ1.NE.0)) GO TO 200
	  IF(ACT.EQ.0) GO TO 80			! any verb yet?
	  IF((VVOC(ACT+1).AND.SVMASK).NE.WALKW) GO TO 200	! walk?
80	  DO 100 J=1,DWMAX			! then chk for dir.
	    IF(WORD.EQ.DWORD(J)) GO TO 3000	! match to direction?
100	  CONTINUE
C
C Not an action, check for preposition, adjective, or object.
C
200	  DO 250 J=1,PWMAX			! look for preposition.
	    IF(WORD.EQ.PWORD(J)) GO TO 4000	! match to preposition?
250	  CONTINUE
C
	  J=1					! look for adjective.
	  DO 350 K=1,AWMAX
	    IF(WORD.EQ.AWORD(K)) GO TO 5000	! match to adjective?
300	    J=J+1				! advance to next entry.
	    IF(AVOC(J).LT.0) GO TO 300		! found next entry yet?
350	  CONTINUE
C
400	  J=1					! look for object.
	  DO 550 K=1,OWMAX
	    IF(WORD.EQ.OWORD(K)) GO TO 6000	! match to object?
500	    J=J+1				! advance to next entry.
	    IF(OVOC(J).LT.0) GO TO 500		! found next entry yet?
550	  CONTINUE
C
C Not recognizable
C
	  IF(.NOT.VBFLAG) RETURN		! if mute, return
	  LCWORD=LCIFY(WORD,1)			! convert to lower case
	  WRITE(OUTCH,600) LCWORD(1:NBLEN(LCWORD)) ! don't recognize
600	  FORMAT(' I don''t understand "',A,'".')
	  CALL RSPEAK(ERRVOC)			! if extra verb, say so
800	  TELFLG=.TRUE.				! something said.
	  BUNSUB=0				! no valid EXCEPT clause.
	  RETURN

C SPARSE, PAGE 9
C
1000	IF(I.LT.LLNT) GO TO 10			! end of do loop
C
C At end of parse, check for:
C	1. dangling adjective
C	2. bunched object
C	3. simple directions
C	4. orphan preposition
C	5. dangling preposition
C
	IF(ADJ.NE.0) GO TO 4500			! dangling adjective?
	IF(BUNFLG) OBJ1=BUNOBJ			! bunched object?
	IF(BUNFLG.AND.(BUNSUB.NE.0).AND.(BUNLNT.EQ.0))
	1	GO TO 13200			! except for nothing?
	IF(ACT.EQ.0) ACT=OFLAG.AND.OACT		! if no action, take orphan.
	IF(ACT.EQ.0) GO TO 10000		! no action, punt.
	IF(((VVOC(ACT+1).AND.SVMASK).NE.WALKW).OR.(OBJ1.LT.XMIN))
	1	GO TO 1100			! simple direction?
	IF ((OBJ2.NE.0).OR.(PREP1.NE.0).OR.(PREP2.NE.0))
	1	GO TO 1050			! no extra junk?
	PRSA=WALKW				! yes, win totally.
	PRSO=OBJ1
	SPARSE=1				! special return value.
	RETURN
C
1050	IF(VBFLAG) CALL RSPEAK(618)		! direction+junk, fail.
	GO TO 800				! clean up state.
C
1100	IF((OFLAG.NE.0).AND.(OPREP.NE.0).AND.(PREP.EQ.0).AND.
	1	(OBJ1.NE.0).AND.(OBJ2.EQ.0).AND.(ACT.EQ.OACT))
	2	GO TO 11000
C
	IF(PREP.EQ.0) GO TO 1200		! if dangling prep,
	IF(PPTR.EQ.0) GO TO 12000		! and no object, die;
	IF(PRPVEC(PPTR).NE.0) GO TO 12000	! and prep already, die;
	PRPVEC(PPTR)=PREP			! cvt to 'pick up frob'.
1200	SPARSE=0				! parse succeeds.
	IF(DFLAG) WRITE(OUTCH,1310) ACT,OBJ1,OBJ2,PREP1,PREP2
1310	FORMAT(' SPARSE RESULTS- ',5I7)
	RETURN

C SPARSE, PAGE 10
C
C 1500--	AND
C
1500	IF(ADJ.NE.0) GO TO 4100			! dangling adj? treat as obj.
	IF((PREP.NE.0).OR.(PPTR.NE.1)) GO TO 8000	! prep or not dir obj?
	ANDFLG=.TRUE.				! flag 'AND'.
	GO TO 1000				! done.
C
C 2000--	Action
C
2000	IF(ACT.EQ.0) GO TO 2100			! got one already?
	ERRVOC=624				! flag for error report.
	GO TO 75				! try to construe differently.
C
2100	ACT=J					! save index to verb.
	OACT=0					! no orphan.
	ANDFLG=.FALSE.				! clear 'AND' flag.
	IF(DFLAG) WRITE(OUTCH,2020) J
2020	FORMAT(' SPARSE- ACT AT ',I6)
	GO TO 1000				! done.
C
C 2500--	EXCEPT/BUT
C
2500	IF(ADJ.NE.0) GO TO 4100			! dangling adjective?
	IF(ANDFLG.OR.BUNFLG.OR.(PPTR.NE.1).OR.
	1	(I.GE.LLNT)) GO TO 13000	! not in right place?
	IF(LBUF(I+1).NE.'FOR') GO TO 2600	! except for?
	I=I+1					! skip over.
	IF(I.GE.LLNT) GO TO 13000		! out of text?
2600	IF((OBJ1.NE.EVERY).AND.(OBJ1.NE.VALUA).AND.
	1  (OBJ1.NE.POSSE)) GO TO 13100		! "collective" EXCEPT?
	ANDFLG=.TRUE.				! force next object
	BUNFLG=.TRUE.				! into bunch vector.
	BUNLNT=0				! start at top.
	BUNSUB=OBJ1				! remember collective.
	GO TO 1000				! on to next word.
C
C 3000--	Direction
C 		Don't need to check for ambiguous use as adjective;
C		only possible overlap is north/south/east/west wall;
C		and global wall takes is found if no adjective given.
C
3000	OBJ=DVOC(J)				! save direction.
	ACT=1					! find value for action.
3600	IF(VVOC(ACT).EQ.0) CALL BUG(310,ACT)	! can't find walk.
	IF((VVOC(ACT+1).AND.SVMASK).EQ.WALKW) GO TO 6300 ! treat as obj.
	ACT=ACT+VVOC(ACT)+1			! to next syntax entry.
	GO TO 3600
C
C 4000--	Preposition (or dangling adjective at end of parse)
C
4000	IF(ADJ.EQ.0) GO TO 4600			! dangling adjective?
4100	I=I-1					! back up parse stream.
4500	WORD=AWORD(ADJPTR)			! get adjective string.
	ADJ=0					! now an object.
	GO TO 400				! go search object words.
C
4600	IF(ANDFLG) GO TO 8000			! 'AND' pending?
	IF(PREP.NE.0) GO TO 1000		! already have one? ignore.
	PREP=PVOC(J)				! no, get index.
	IF(DFLAG) WRITE(OUTCH,4030) J
4030	FORMAT(' SPARSE- PREP AT ',I6)
	GO TO 1000
C
C 5000--	Adjective
C
5000	ADJ=J					! save adjective.
	ADJPTR=K				! save string pointer.
	IF((I.LT.LLNT).OR.(OFLAG.EQ.0).OR.(ONAME.EQ.' '))
	1	GO TO 1000			! last word + orphan string?
	IF(DFLAG) WRITE(OUTCH,5040) ADJ,ONAME	! have orphan.
5040	FORMAT(' SPARSE- ADJ AT ',I6,' ORPHAN= ',A)
	WORD=ONAME				! get object string.
	GO TO 400				! go search object names.
C
C 6000--	Object
C
6000	OBJ=GETOBJ(J,ADJ,0)			! identify object.
	IF(DFLAG) WRITE(OUTCH,6010) J,OBJ
6010	FORMAT(' SPARSE- OBJ AT ',I6,'  OBJ= ',I6)
	IF(OBJ.LE.0) GO TO 7000			! if le, couldnt.
	IF(OBJ.NE.ITOBJ) GO TO 6100		! "it"?
	IF((OFLAG.AND.OOBJ1).NE.0) LASTIT=OFLAG.AND.OOBJ1	! orphan?
	OBJ=GETOBJ(0,0,LASTIT)			! find it.
	IF(OBJ.LE.0) GO TO 7500			! if le, couldnt.
C
6100	IF(PREP.NE.9) GO TO 6200		! "of" obj?
	IF((LOBJ.EQ.OBJ).OR.(LOBJ.EQ.OCAN(OBJ))) GO TO 6500	! same as prev?
	IF((LOBJ.EQ.EVERY).AND.((OBJ.EQ.VALUA).OR.(OBJ.EQ.POSSE)))
	1	GO TO 6350			! all of "collective"?
6150	IF(VBFLAG) CALL RSPEAK(601)		! doesn't work
	GO TO 800				! clean up state.
C
6200	IF(.NOT.ANDFLG) GO TO 6300		! 'AND' pending?
	IF(BUNFLG) GO TO 6250			! first object?
	BUNVEC(1)=OBJVEC(PPTR)			! put preceding obj in vector.
	BUNLNT=1
	BUNFLG=.TRUE.				! flag bunch of objects.
	BUNSUB=0				! no EXCEPT/BUT clause.
6250	BUNLNT=BUNLNT+1				! advance bunch pointer.
	IF(BUNLNT.GT.BUNMAX) GO TO 9000		! too many objects?
	BUNVEC(BUNLNT)=OBJ			! add to bunch vector.
	GO TO 6500
C
6300	IF(PPTR.EQ.2) GO TO 9000		! too many objs?
	PPTR=PPTR+1
	PRPVEC(PPTR)=PREP
6350	OBJVEC(PPTR)=OBJ			! stuff into vector.
6500	PREP=0
	ADJ=0
	ANDFLG=.FALSE.				! no pending 'AND'.
	LOBJ=OBJ				! record last object.
	GO TO 1000

C SPARSE, PAGE 11
C
C 7000--	Unidentifiable object (index into OVOC is J)
C
7000	LCWORD=LCIFY(WORD,1)			! convert obj to lower case.
	LCWRD1=' '				! assume no adjective
	IF(ADJ.NE.0) LCWRD1=' '//LCIFY(AWORD(ADJPTR),1)//' '
	IF(OBJ.LT.0) GO TO 7200			! ambiguous or unreachable?
	IF(LIT(HERE)) GO TO 7100		! lit?
	IF(VBFLAG) CALL RSPEAK(579)		! not lit, report.
	GO TO 800				! go clean up state.
C
7100	IF(VBFLAG) WRITE(OUTCH,7110)
	1	LCWRD1(1:NBLEN(LCWRD1)+1),LCWORD(1:NBLEN(LCWORD))
7110	FORMAT(' I can''t see any',A,A,' here.')
	GO TO 800				! go clean up state.
C
7200	IF(OBJ.NE.-10000) GO TO 7300		! inside vehicle?
	IF(VBFLAG) CALL RSPSUB(620,ODESC2(AVEHIC(WINNER)))
	GO TO 800				! go clean up state.
C
7300	IF(ACT.EQ.0) ACT=OFLAG.AND.OACT		! if no act, get orphan.
	CALL ORPHAN(-1,ACT,PREP1,OBJ1,PREP,WORD,0,0)	! orphan the world.
	IF(VBFLAG) WRITE(OUTCH,7310)
	1	LCWRD1(1:NBLEN(LCWRD1)+1),LCWORD(1:NBLEN(LCWORD))
7310	FORMAT(' Which',A,A,' do you mean?')
	GO TO 800				! go clean up state.
C
C 7500--	Unidentifiable 'IT' (last direct object is LASTIT).
C
7500	IF(OBJ.LT.0) GO TO 7200			! if lt, must be unreachable.
	IF(LIT(HERE)) GO TO 7600		! lit?
	IF(VBFLAG) CALL RSPEAK(1076)		! lose.
	GO TO 800				! go clean up state.
C
7600	IF(VBFLAG) CALL RSPSUB(1077,ODESC2(LASTIT))	! don't see it.
	GO TO 800				! go clean up state.
C
C 8000--	Misplaced 'AND'.
C
8000	IF(VBFLAG) CALL RSPEAK(1049)
	GO TO 800				! go clean up state.
C
C 9000--	Too many objects.
C
9000	IF(VBFLAG) CALL RSPEAK(617)
	GO TO 800				! go clean up state.
C
C 10000--	No action, punt.
C
10000	IF(OBJ1.EQ.0) GO TO 10100		! any direct object?
	IF(VBFLAG) CALL RSPSUB(621,ODESC2(OBJ1))	! what to do?
	CALL ORPHAN(-1,0,PREP1,OBJ1,0,' ',0,0)
	RETURN
C
10100	IF(VBFLAG) CALL RSPEAK(622)		! huh?
	GO TO 800				! go clean up state.
C
C 11000--	Orphan preposition.  Conditions are
C		OBJ1.NE.0, OBJ2=0, PREP=0, ACT=OACT
C
11000	IF(OOBJ1.NE.0) GO TO 11500		! orphan object?
	PREP1=OPREP				! no, just use prep.
	GO TO 1200
C
11500	OBJ2=OBJ1				! yes, use as direct obj.
	PREP2=OPREP
	OBJ1=OOBJ1
	PREP1=OPREP1
	GO TO 1200
C
C 12000--	True hanging preposition, no objects yet.
C
12000	CALL ORPHAN(-1,ACT,0,0,PREP,' ',0,0)	! orphan prep.
	GO TO 1200
C
C 13000--	EXCEPT/BUT errors.
C
13000	LCWORD=LCIFY(WORD,1)
	IF(VBFLAG) WRITE(OUTCH,13010) LCWORD(1:NBLEN(LCWORD))	! wrong place.
13010	FORMAT(' Misplaced "',A,'".')
	GO TO 800				! go clean up state.
C
13100	LCWORD=LCIFY(WORD,2)				! wrong case.
	IF(VBFLAG) WRITE(OUTCH,13110) LCWORD(1:NBLEN(LCWORD))	! not coll.
13110	FORMAT(' "',A,'" can only be used with "everything",',
	1 ' "valuables", or "possessions".')
	GO TO 800				! go clean up state.
C
13200	IF(VBFLAG) CALL RSPEAK(619)		! no objects.
	GO TO 800				! go clean up state.
C
	END

C GETOBJ--	Find obj described by adj, name pair
C
C Declarations
C
C This routine details on bit 3 of PRSFLG
C
	INTEGER FUNCTION GETOBJ(OIDX,AIDX,SPCOBJ)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
	LOGICAL THISIT,GHERE,LIT,CHOMP,DFLAG,NOADJS
C
	DFLAG=(PRSFLG.AND.8).NE.0
	CHOMP=.FALSE.
	AV=AVEHIC(WINNER)
	OBJ=0					! assume dark.
	IF(.NOT.LIT(HERE)) GO TO 200		! lit?
C
	OBJ=SCHLST(OIDX,AIDX,HERE,0,0,SPCOBJ)	! search room.
	IF(DFLAG) WRITE(OUTCH,10) OBJ
10	FORMAT(' SCHLST- ROOM SCH ',I6)
	IF(OBJ) 1000,200,100			! test result.
100	IF((AV.EQ.0).OR.(AV.EQ.OBJ).OR.(OCAN(OBJ).EQ.AV).OR.
	1	((OFLAG2(OBJ).AND.FINDBT).NE.0)) GO TO 200
	CHOMP=.TRUE.				! not reachable.
C
200	IF(AV.EQ.0) GO TO 400			! in vehicle?
	NOBJ=SCHLST(OIDX,AIDX,0,AV,0,SPCOBJ)	! search vehicle.
	IF(DFLAG) WRITE(OUTCH,220) NOBJ
220	FORMAT(' SCHLST- VEH SCH  ',I6)
	IF(NOBJ) 800,400,300			! test result.
300	CHOMP=.FALSE.				! reachable.
	IF(OBJ.EQ.NOBJ) GO TO 400		! same as before?
	IF(OBJ.NE.0) NOBJ=-NOBJ			! amb result?
	OBJ=NOBJ
C
400	NOBJ=SCHLST(OIDX,AIDX,0,0,WINNER,SPCOBJ)	! search adventurer.
	IF(DFLAG) WRITE(OUTCH,430) NOBJ
430	FORMAT(' SCHLST- ADV SCH  ',I6)
	IF(NOBJ) 800,900,500			! test result
500	IF(OBJ.EQ.0) GO TO 800			! any previous? no, use nobj.
	IF(AIDX.NE.0) GO TO 600			! yes, amb, any adj?
	IF(NOADJS(OBJ).NEQV.NOADJS(NOBJ)) GO TO 700 ! both adj or no adj?
600	OBJ=-NOBJ				! ambiguous result.
	GO TO 900
700	IF(NOADJS(OBJ)) GO TO 900		! if old no adj, use old.
800	OBJ=NOBJ				! return new object.
900	IF(CHOMP) OBJ=-10000			! unreachable.
1000	GETOBJ=OBJ
C
	IF(GETOBJ.NE.0) GO TO 1500		! got something?
	DO 1200 I=STRBIT+1,OLNT			! no, search globals.
	  IF(.NOT.THISIT(OIDX,AIDX,I,SPCOBJ)) GO TO 1200
	  IF(.NOT.GHERE(I,HERE)) GO TO 1200	! can it be here?
	  IF(GETOBJ.EQ.0) GO TO 1150		! got one yet?
	  IF(AIDX.NE.0) GO TO 1050		! yes, no adj?
	  IF(NOADJS(GETOBJ).NEQV.NOADJS(I)) GO TO 1100	! only one with no adj?
1050	  GETOBJ=-I				! ambiguous
	  GO TO 1200
1100	  IF(NOADJS(GETOBJ)) GO TO 1200		! if old no adj, retain.
1150	  GETOBJ=I				! new is target.
1200	CONTINUE
C
1500	CONTINUE				! end of search.
	IF(DFLAG) WRITE(OUTCH,1540) GETOBJ
1540	FORMAT(' SCHLST- RESULT   ',I6)
	RETURN
	END

C SCHLST--	Search for object
C
C Declarations
C
	INTEGER FUNCTION SCHLST(OIDX,AIDX,RM,CN,AD,SPCOBJ)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
	LOGICAL THISIT,QHERE,NOTRAN,NOVIS,AEMPTY,NOADJS
C
C Functions and data
C
	NOTRAN(O)=((OFLAG1(O).AND.TRANBT).EQ.0).AND.
	1	((OFLAG2(O).AND.OPENBT).EQ.0)
	NOVIS(O)=((OFLAG1(O).AND.VISIBT).EQ.0)
C
	SCHLST=0				! no result.
	AEMPTY=.FALSE.				! no ambiguous empty.
	DO 1000 I=1,OLNT			! search objects.
	  IF(NOVIS(I).OR.
	1	(((RM.EQ.0).OR.(.NOT.QHERE(I,RM))).AND.
	2	 ((CN.EQ.0).OR.(OCAN(I).NE.CN)).AND.
	3	 ((AD.EQ.0).OR.(OADV(I).NE.AD)))) GO TO 1000
	  IF(.NOT.THISIT(OIDX,AIDX,I,SPCOBJ)) GO TO 200
	  IF(SCHLST.EQ.0) GO TO 150		! got one already?
	  IF(AIDX.NE.0) GO TO 2000		! adj? then ambiguous
	  IF(NOADJS(I)) GO TO 100		! new have no adj?
	  AEMPTY=.TRUE.				! no, old might, flag.
	  GO TO 200
100	  IF(NOADJS(SCHLST)) GO TO 2000		! old have no adj?
150	  SCHLST=I				! new is unique, or
						! new has no adj, old does.
C
C If open or transparent, search the object itself.
C
200	  IF(NOTRAN(I)) GO TO 1000
C
C Search is conducted in reverse.  All objects are checked to
C See if they are at some level of containment inside object 'I'.
C If they are at level 1, or if all links in the containment
C chain are open, visible, and have SEARCHME set, they can qualify
C as a potential match.
C
	  DO 500 J=1,OLNT			! search objects.
	    IF(NOVIS(J).OR. (.NOT.THISIT(OIDX,AIDX,J,SPCOBJ)))
	1	GO TO 500			! visible & match?
	    X=OCAN(J)				! get container.
300	    IF(X.EQ.I) GO TO 400		! inside target?
	    IF(X.EQ.0) GO TO 500		! inside anything?
	    IF(NOVIS(X).OR.NOTRAN(X).OR.
	1	((OFLAG2(X).AND.SCHBT).EQ.0)) GO TO 500
	    X=OCAN(X)				! go another level.
	    GO TO 300
C
400	    IF(SCHLST.EQ.0) GO TO 450		! already got one?
	    IF(AIDX.NE.0) GO TO 2000		! adj? then ambiguous.
	    IF(NOADJS(J)) GO TO 425		! new have no adj?
	    AEMPTY=.TRUE.			! no, ambiguous empty.
	    GO TO 500
425	    IF(NOADJS(SCHLST)) GO TO 2000	! old have no adj? then amb.
450	    SCHLST=J				! new is unique, or
						! new has no adj, and old does.
500	  CONTINUE
C
1000	CONTINUE
	IF(.NOT.AEMPTY.OR.(SCHLST.EQ.0)) RETURN	! if none or not amb, done.
	IF(NOADJS(SCHLST)) RETURN		! if amb, and no adj, done.
2000	SCHLST=-SCHLST				! amb return.
	RETURN
C
	END

C THISIT--	Validate object vs description
C
C Declarations
C
	LOGICAL FUNCTION THISIT(OIDX,AIDX,OBJ,SPCOBJ)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
C
	THISIT=.FALSE.				! assume no match.
	IF((SPCOBJ.NE.0).AND.(OBJ.EQ.SPCOBJ)) GO TO 500
C
C Check for object names
C
	IF(OIDX.EQ.0) RETURN			! no obj? lose.
	I=OIDX
100	IF(IABS(OVOC(I)).EQ.OBJ) GO TO 200	! found it?
	I=I+1					! adv to next.
	IF(OVOC(I).LT.0) GO TO 100		! still part of list?
	RETURN					! if done, lose.
C
200	IF(AIDX.EQ.0) GO TO 500			! no adj? done.
	I=AIDX
300	IF(IABS(AVOC(I)).EQ.OBJ) GO TO 500	! found it?
	I=I+1					! adv to next.
	IF(AVOC(I).LT.0) GO TO 300		! still part of list?
	RETURN					! if done, lose.
C
500	THISIT=.TRUE.
	RETURN
	END

C SYNMCH--	Syntax matcher
C
C Declarations
C
C This routine details on bit 4 of PRSFLG
C
	LOGICAL FUNCTION SYNMCH(X)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
	LOGICAL SYNEQL,TAKEIT,DFLAG
	CHARACTER*(TEXLNT) STR
	CHARACTER*(WRDLNT) FINDVB,FINDPR,LCIFY,LCWORD
	CHARACTER*(WRDLNT+2) LCPRP1,LCPRP2

C SYNMCH, PAGE 2
C
	SYNMCH=.FALSE.
	DFLAG=(PRSFLG.AND.16).NE.0
	J=ACT					! set up ptr to syntax.
	DRIVE=0					! no default.
	DFORCE=0				! no forced default.
	QPREP=OFLAG.AND.OPREP			! valid orphan prep flag.
	LIMIT=J+VVOC(J)+1			! compute limit.
	J=J+1					! advance to next.
C
200	CALL UNPACK(J,NEWJ)			! unpack syntax.
	IF(DFLAG) WRITE(OUTCH,210) J,OBJ1,PREP1,DOBJ,DFL1,DFL2
210	FORMAT(' SYNMCH DOBJ INPUTS TO SYNEQL- ',6I7)
	SPREP=DOBJ.AND.VPMASK			! save expected prep.
	IF(SYNEQL(PREP1,OBJ1,DOBJ,DFL1,DFL2)) GO TO 1000
C
C Direct syntax match fails, try direct as indirect.
C
	IF((OBJ2.NE.0).OR.(OBJ1.EQ.0).OR.
	1	(.NOT.SYNEQL(PREP1,OBJ1,IOBJ,IFL1,IFL2)))
	2	GO TO 500			! try direct as indirect.
	OBJ2=OBJ1				! move direct to indirect.
	PREP2=PREP1
	OBJ1=0					! no direct.
	PREP1=0
	DRIVE=J					! save as driver.
	GO TO 3100				! go try to get direct obj
C
C Direct syntax match and direct-as-indirect fail.
C
500	IF(OBJ1.NE.0) GO TO 3000		! if direct obj, on to next.
	GO TO 2500				! go do defaults.
C
C Direct syntax match succeeded, try indirect.
C
1000	IF(DFLAG) WRITE(OUTCH,1010) J,OBJ2,PREP2,IOBJ,IFL1,IFL2
1010	FORMAT(' SYNMCH IOBJ INPUTS TO SYNEQL- ',6I7)
	SPREP=IOBJ.AND.VPMASK			! save expected prep.
	IF(SYNEQL(PREP2,OBJ2,IOBJ,IFL1,IFL2)) GO TO 6000
C
C Indirect syntax match fails.
C
	IF(OBJ2.NE.0) GO TO 3000		! if ind object, on to next.
2500	IF((QPREP.EQ.0).OR.(QPREP.EQ.SPREP)) DFORCE=J	 ! if prep mch.
	IF((VFLAG.AND.SDRIV).NE.0) DRIVE=J	! if driver, record.
	IF(DFLAG) WRITE(OUTCH,2510) J,QPREP,SPREP,DFORCE,DRIVE
2510	FORMAT(' SYNMCH DEFAULT SYNTAXES- ',5I7)
3000	J=NEWJ
	IF(J.LT.LIMIT) GO TO 200		! more to do?

C SYNMCH, PAGE 3
C
C Match has failed.  If default syntax exists, try to snarf
C orphans or GWIMs, or make new orphans.
C
3100	IF(DFLAG) WRITE(OUTCH,3110) DRIVE,DFORCE,OBJ1,OBJ2
3110	FORMAT(' SYNMCH, DRIVE=',2I6,'  OBJECTS =',2I6)
	IF(DRIVE.EQ.0) DRIVE=DFORCE		! no driver? use force.
	IF(DRIVE.EQ.0) GO TO 10000		! any driver?
	CALL UNPACK(DRIVE,DFORCE)		! unpack dflt syntax.
	LCWORD=LCIFY(FINDVB(DRIVE),2)		! get verb string.
	LCPRP1=' '//LCIFY(FINDPR(DOBJ.AND.VPMASK),1)//' '
	LCPRP2=' '//LCIFY(FINDPR(IOBJ.AND.VPMASK),1)//' '
C
C Try to fill direct object slot if that was the problem.
C
	IF(((VFLAG.AND.SDIR).EQ.0).OR.(OBJ1.NE.0)) GO TO 4000
	OBJ1=OFLAG.AND.OOBJ1
	IF(OBJ1.EQ.0) GO TO 3500		! any orphan?
	IF(SYNEQL(OPREP1,OBJ1,DOBJ,DFL1,DFL2)) GO TO 4000
C
C Orphan fails, try GWIM.
C
3500	OBJ1=GWIM(DOBJ,DFW1,DFW2)		! get gwim.
	IF(DFLAG) WRITE(OUTCH,3530) OBJ1
3530	FORMAT(' SYNMCH- DO GWIM= ',I6)
	IF(OBJ1.GT.0) GO TO 4000		! test result.
	CALL ORPHAN(-1,ACT,0,0,DOBJ.AND.VPMASK,' ',PREP2,OBJ2)	! fails, orphan.
	BUNSUB=0				! no EXCEPT clause.
	IF(OBJ2.GT.0) GO TO 3800		! if iobj, go print.
3700	WRITE(OUTCH,3750)
	1	LCWORD(1:NBLEN(LCWORD)),LCPRP1(1:NBLEN(LCPRP1)+1)
3750	FORMAT(1X,A,A,'what?')
	TELFLG=.TRUE.
	RETURN
C
3800	X=IABS(ODESC2(OBJ2))			! get iobj description.
	READ(DBCH,REC=X) J,STR			! read data base.
	CALL TXCRYP(X,STR)			! decrypt the line.
	WRITE(OUTCH,3880) LCWORD(1:NBLEN(LCWORD)),
	1	LCPRP1(1:NBLEN(LCPRP1)+1),
	2	LCPRP2(1:NBLEN(LCPRP2)+1),STR(1:NBLEN(STR))
3880	FORMAT(1X,A,A,'what',A,'the ',A,'?')
	TELFLG=.TRUE.
	RETURN

C SYNMCH, PAGE 4
C
C Try to fill indirect object slot if that was the problem.
C
4000	IF(((VFLAG.AND.SIND).EQ.0).OR.(OBJ2.NE.0)) GO TO 6000
	OBJ2=OFLAG.AND.OOBJ2
	IF(OBJ2.EQ.0) GO TO 4500		! any orphan?
	IF(SYNEQL(OPREP2,OBJ2,IOBJ,IFL1,IFL2)) GO TO 6000
C
C Orphan fails, try GWIM.
C
4500	OBJ2=GWIM(IOBJ,IFW1,IFW2)		! gwim.
	IF(DFLAG) WRITE(OUTCH,4550) OBJ2
4550	FORMAT(' SYNMCH- IO GWIM= ',I6)
	IF(OBJ2.GT.0) GO TO 6000
	IF(OBJ1.GT.0) GO TO 4600		! if dobj, go print.
	CALL ORPHAN(-1,ACT,OFLAG.AND.OPREP1,
	1	OFLAG.AND.OOBJ1,IOBJ.AND.VPMASK,' ',0,0)
	GO TO 3700
C
C Error with direct object available.
C
4600	CALL ORPHAN(-1,ACT,PREP1,OBJ1,IOBJ.AND.VPMASK,' ',0,0)
	X=IABS(ODESC2(OBJ1))			! get dobj description.
	READ(DBCH,REC=X) J,STR			! read data base.
	CALL TXCRYP(X,STR)			! decrypt the line.
	WRITE(OUTCH,4660) LCWORD(1:NBLEN(LCWORD)),
	1	LCPRP1(1:NBLEN(LCPRP1)+1),
	2	STR(1:NBLEN(STR)),LCPRP2(1:NBLEN(LCPRP2)+1)
4660	FORMAT(1X,A,A,'the ',A,A,'what?')
	TELFLG=.TRUE.
	RETURN
C
C Total chomp.
C
10000	CALL RSPEAK(601)			! cant do anything.
	BUNSUB=0
	RETURN

C SYNMCH, PAGE 5
C
C Now try to take individual objects and
C in general clean up the parse vector.
C
6000	IF((VFLAG.AND.SFLIP).EQ.0) GO TO 7000	! flip?
	J=OBJ1					! yes.
	OBJ1=OBJ2
	OBJ2=J
C
7000	PRSA=VFLAG.AND.SVMASK			! get verb.
	PRSO=OBJ1				! get dir obj.
	PRSI=OBJ2				! get ind obj.
	IF(.NOT.TAKEIT(PRSO,DOBJ)) RETURN	! try take.
	IF(.NOT.TAKEIT(PRSI,IOBJ)) RETURN	! try take.
	SYNMCH=.TRUE.
	IF(DFLAG) WRITE(OUTCH,7050) SYNMCH,PRSA,PRSO,PRSI,ACT,OBJ1,OBJ2
7050	FORMAT(' SYNMCH- RESULTS ',L1,6I7)
	RETURN
C
	END

C UNPACK-	Unpack syntax specification, adv pointer
C
C Declarations
C
	SUBROUTINE UNPACK(OLDJ,J)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
C
	DO 10 I=1,11				! clear syntax.
	  SYN(I)=0
10	CONTINUE
C
	VFLAG=VVOC(OLDJ)
	J=OLDJ+1
	IF((VFLAG.AND.SDIR).EQ.0) RETURN	! dir object?
	DFL1=-1					! assume std.
	DFL2=-1
	IF((VFLAG.AND.SSTD).EQ.0) GO TO 100	! std object?
	DFW1=-1					! yes.
	DFW2=-1
	DOBJ=VABIT+VRBIT+VFBIT
	GO TO 200
C
100	DOBJ=VVOC(J)				! not std.
	DFW1=VVOC(J+1)
	DFW2=VVOC(J+2)
	J=J+3
	IF((DOBJ.AND.VEBIT).EQ.0) GO TO 200	! vbit = vfwim?
	DFL1=DFW1				! yes.
	DFL2=DFW2
C
200	IF((VFLAG.AND.SIND).EQ.0) RETURN	! ind object?
	IFL1=-1					! assume std.
	IFL2=-1
	IOBJ=VVOC(J)
	IFW1=VVOC(J+1)
	IFW2=VVOC(J+2)
	J=J+3
	IF((IOBJ.AND.VEBIT).EQ.0) RETURN	! vbit = vfwim?
	IFL1=IFW1				! yes.
	IFL2=IFW2
	RETURN
C
	END

C SYNEQL-	Test for syntax equality
C
C Declarations
C
	LOGICAL FUNCTION SYNEQL(PREP,OBJ,SPREP,SFL1,SFL2)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
C
	IF(OBJ.EQ.0) GO TO 100			! any object?
	SYNEQL=(PREP.EQ.(SPREP.AND.VPMASK)).AND.
	1	(((SFL1.AND.OFLAG1(OBJ)).OR.
	2	  (SFL2.AND.OFLAG2(OBJ))).NE.0)
	RETURN
C
100	SYNEQL=(PREP.EQ.0).AND.(SFL1.EQ.0).AND.(SFL2.EQ.0)
	RETURN
C
	END

C TAKEIT-	Parser based take of object
C
C Declarations
C
	LOGICAL FUNCTION TAKEIT(OBJ,SFLAG)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
	LOGICAL TAKE,LIT
C
	TAKEIT=.FALSE.				! assume loses.
	IF((OBJ.EQ.0).OR.(OBJ.GT.STRBIT).OR.DEADF)
	1	GO TO 4000			! null/stars/dead win.
	ODO2=ODESC2(OBJ)			! get desc.
	X=OCAN(OBJ)				! get container.
	IF((X.EQ.0).OR.((SFLAG.AND.VFBIT).EQ.0)) GO TO 500
	IF((OFLAG2(X).AND.OPENBT).NE.0) GO TO 500
	CALL RSPSUB(566,ODO2)			! cant reach.
	RETURN
C
500	IF((SFLAG.AND.VRBIT).EQ.0) GO TO 1000	! shld be in room?
	IF((SFLAG.AND.VTBIT).EQ.0) GO TO 2000	! can be taken?
C
C Should be in room (VRBIT NE 0) and can be taken (VTBIT NE 0)
C
	IF(SCHLST(0,0,HERE,0,0,OBJ).LE.0) GO TO 4000 ! if not, ok.
C
C Its in the room and can be taken.
C
	IF((OFLAG1(OBJ).AND.TAKEBT).NE.0) GO TO 3000
C
C Not takeable.  If we care, fail.
C
	IF((SFLAG.AND.VCBIT).EQ.0) GO TO 4000	! if no care, return.
	CALL RSPSUB(445,ODO2)
	RETURN
C
C 1000--	It should not be in the room.
C 2000--	It cant be taken.
C
2000	IF((SFLAG.AND.VCBIT).EQ.0) GO TO 4000	! if no care, return
1000	IF(SCHLST(0,0,HERE,0,0,OBJ).LE.0) GO TO 4000
	I=665					! assume player.
	IF(WINNER.NE.PLAYER) I=1082
	CALL RSPSUB(I,ODO2)			! doesn't have it.
	RETURN
C
C 3000--	Take object.
C
3000	IF(LIT(HERE)) GO TO 3500		! lit?
	CALL RSPEAK(579)			! can't do it.
	RETURN
C
3500	SVA=PRSA				! save parse vector
	SVO=PRSO
	SVI=PRSI
	PRSA=TAKEW				! make 'take obj'
	PRSO=OBJ
	PRSI=0					! no indirect object
	TAKEIT=TAKE(.TRUE.)			! try to take object
	PRSA=SVA				! restore parse vector.
	PRSO=SVO
	PRSI=SVI
	RETURN
C
C 4000--	Win on general principles.
C
4000	TAKEIT=.TRUE.
	RETURN
C
	END

C GWIM- Get what I mean in ambiguous situations
C
C Declarations
C
	INTEGER FUNCTION GWIM(SFLAG,SFW1,SFW2)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
	LOGICAL TAKEIT,NOCARE,LIT
C
	GWIM=0					! no result.
	IF(DEADF) RETURN			! dead? gwim disabled.
	AV=AVEHIC(WINNER)
	NOCARE=(SFLAG.AND.VCBIT).EQ.0
C
C First search adventurer
C
	IF((SFLAG.AND.VABIT).NE.0)
	1	GWIM=FWIM(SFW1,SFW2,0,0,WINNER,NOCARE)
	IF((GWIM.LT.0).OR..NOT.LIT(HERE).OR.
	1  ((SFLAG.AND.VRBIT).EQ.0)) RETURN
C
C Also search room
C
100	ROBJ=FWIM(SFW1,SFW2,HERE,0,0,NOCARE)
	IF(ROBJ) 500,600,200			! test result.
C
C ROBJ > 0: if prev object, fail
C
200	IF((AV.EQ.0).OR.(ROBJ.EQ.AV).OR.
	1	((OFLAG2(ROBJ).AND.FINDBT).NE.0)) GO TO 300
	IF(OCAN(ROBJ).NE.AV) RETURN		! unreachable? use prev obj.
C
300	IF(GWIM.EQ.0) GO TO 400			! prev obj?
	GWIM=-GWIM				! yes, ambiguous.
	RETURN
C
400	IF(.NOT.TAKEIT(ROBJ,SFLAG)) RETURN	! if untakeable, return prev.
500	GWIM=ROBJ				! return room seach.
600	RETURN
C
	END

C NOADJS-	See if any adjectives for object
C
C Declarations
C
	LOGICAL FUNCTION NOADJS(OBJ)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
C
	NOADJS=.FALSE.				! assume false.
	DO 100 I=1,AVMAX			! search adj.
	  IF(IABS(AVOC(I)).EQ.OBJ) RETURN	! found adjective?
	  IF(AVOC(I).EQ.0) GO TO 200		! end of list?
100	CONTINUE
200	NOADJS=.TRUE.				! true.
	RETURN
C
	END

C LCIFY-	"Lower case"-ify a string for printing
C
C Declarations
C
	CHARACTER*(*) FUNCTION LCIFY(STRING,START)
	IMPLICIT INTEGER(A-Z)
	CHARACTER*(*) STRING
C
	LCIFY=STRING				! assume input = output.
	K=LEN(STRING)				! get input length.
	IF(START.GT.K) RETURN			! anything to convert?
C
	ULCVT=ICHAR('a')-ICHAR('A')		! conversion factor
	DO 100 I=START,K			! loop on characters
	  IF((STRING(I:I).GE.'A').AND.(STRING(I:I).LE.'Z'))
	1	LCIFY(I:I)=CHAR(ICHAR(STRING(I:I))+ULCVT)
100	CONTINUE
	RETURN
C
	END

C FINDVB-	Find verb string corresponding to syntax.
C
C Declarations
C
	CHARACTER*(*) FUNCTION FINDVB(SYNTAX)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
C
	J=1
	DO 100 K=1,VWMAX			! loop through verbs
	  NEWJ=J+VVOC(J)+1			! start of next syntax
	  IF((J.LE.SYNTAX).AND.(SYNTAX.LT.NEWJ)) GO TO 200
	  IF(VWORD(K)(1:1).NE.'*') J=NEWJ	! if last synonym, advance.
100	CONTINUE
	FINDVB=' '				! disaster
	RETURN
C
200	FINDVB=VWORD(K)				! return string
	IF(VWORD(K)(1:1).EQ.'*') FINDVB=VWORD(K)(2:WRDLNT)
	RETURN
C
	END

C FINDPR-	Find preposition string corresponding to index.
C
C Declarations
C
	CHARACTER*(*) FUNCTION FINDPR(PREPNO)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
C
	DO 100 I=1,PWMAX			! loop through prepositions.
	  IF(PVOC(I).EQ.PREPNO) GO TO 200
100	CONTINUE
	FINDPR=' '
	RETURN
C
200	FINDPR=PWORD(I)
	RETURN
C
	END
