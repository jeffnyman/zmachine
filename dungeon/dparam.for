C Declarations for DUNGEON
C
C COPYRIGHT 1980, 1990, INFOCOM COMPUTERS AND COMMUNICATIONS, CAMBRIDGE MA.
C ALL RIGHTS RESERVED, COMMERCIAL USAGE STRICTLY PROHIBITED
C WRITTEN BY R. M. SUPNIK
C
C This file should be included in *all* modules to supply consistent
C data structure definitions.
C
C 20-Oct-94	RMS	Increased BWMAX, added PLEAK, deleted FILLW.
C 29-Sep-94	RMS	Added COUNT, LDOOR, HEADS, COKES, GBROCH, SHOUS,
C			FORE2, DIGBT.  Deleted SLEPBT.  Modified GLOBAL.
C 01-Feb-94	RMS	Aligned vocabulary COMMON.
C 25-Jan-94	RMS	Added sandy beach room.

C
C Array size parameters
C
	PARAMETER (MMAX=1500)			! messages
	PARAMETER (RMAX=200)			! rooms
	PARAMETER (XXMAX=1000)			! exits
	PARAMETER (OMAX=300)			! objects
	PARAMETER (R2MAX=20)			! multiroom objects
	PARAMETER (CMAX=30)			! clock events
	PARAMETER (VMAX=4)			! villains
	PARAMETER (AMAX=4)			! actors
	PARAMETER (FMAX=56)			! flags
	PARAMETER (SMAX=24)			! switches
	PARAMETER (BWMAX=12)			! buzzword vocabulary
	PARAMETER (DWMAX=25)			! direction vocabulary
	PARAMETER (PWMAX=20)			! preposition vocabulary
	PARAMETER (AWMAX=160)			! adjective vocabularly
	PARAMETER (AVMAX=300)			! string to obj index
	PARAMETER (OWMAX=360)			! object vocabularly
	PARAMETER (OVMAX=550)			! string to obj index
	PARAMETER (VWMAX=240)			! verb vocabularly
	PARAMETER (VVMAX=750)			! verb syntaxes
C
C Other parameters
C
	PARAMETER (RECLNT=20)			! DTEXT.DAT record size, lw
	PARAMETER (TEXLNT=76)			! text buffer size, char
	PARAMETER (WRDLNT=8)			! word length size, char
	PARAMETER (BUNMAX=10)			! bunched objects
	PARAMETER (LEXMAX=20)			! lexical tokens
C
C Syntax definitions
C 
	PARAMETER (SDIR=16384)			! direct object present
	PARAMETER (SIND=8192)			! indirect object present
	PARAMETER (SSTD=4096)			! direct object std format
	PARAMETER (SFLIP=2048)			! flip direct, indirect obj
	PARAMETER (SDRIV=1024)			! default (driver) syntax
	PARAMETER (SVMASK=511)			! mask for verb number
	PARAMETER (VABIT=16384)			! search adv for object
	PARAMETER (VRBIT=8192)			! search room for object
	PARAMETER (VTBIT=4096)			! take object if possible
	PARAMETER (VCBIT=2048)			! must have object
	PARAMETER (VEBIT=1024)			! qualifier flags = fwim flags
	PARAMETER (VFBIT=512)			! must reach object
	PARAMETER (VPMASK=511)			! mask for prep number
C
C Clock event indices
C
	PARAMETER (CEVCUR=1)			! wound cure timer
	PARAMETER (CEVMNT=2)			! maintenance room timer
	PARAMETER (CEVLNT=3)			! lantern timer
	PARAMETER (CEVMAT=4)			! match timer
	PARAMETER (CEVCND=5)			! candle timer
	PARAMETER (CEVBAL=6)			! balloon inflation
	PARAMETER (CEVBRN=7)			! balloon burnup
	PARAMETER (CEVFUS=8)			! fuse burn
	PARAMETER (CEVLED=9)			! ledge collapse timer
	PARAMETER (CEVSAF=10)			! safe timer
	PARAMETER (CEVVLG=11)			! volcano gnome appearance
	PARAMETER (CEVGNO=12)			! volcano gnome exit
	PARAMETER (CEVBUC=13)			! bucket timer
	PARAMETER (CEVSPH=14)			! sphere timer
	PARAMETER (CEVEGH=15)			! end game herald
	PARAMETER (CEVFOR=16)			! forest noises
	PARAMETER (CEVSCL=17)			! screen of light
	PARAMETER (CEVZGI=18)			! Zurich gnome appearance
	PARAMETER (CEVZGO=19)			! Zurich gnome exit
	PARAMETER (CEVSTE=20)			! end game start
	PARAMETER (CEVMRS=21)			! mirror timer
	PARAMETER (CEVPIN=22)			! panel timer
	PARAMETER (CEVINQ=23)			! inquisition timer
	PARAMETER (CEVFOL=24)			! master follows
	PARAMETER (CEVBRO=25)			! brochure timer
	PARAMETER (CEVCYC=26)			! cyclops demon
	PARAMETER (CEVSLI=27)			! slide timer
	PARAMETER (CEVXB=28)			! exorcism bell timer
	PARAMETER (CEVXC=29)			! exorcism candle timer
	PARAMETER (CEVXBH=30)			! exorcism bell cooling
C
C Exit definitions
C
	PARAMETER (XLFLAG=32768)		! last entry flag
	PARAMETER (XDMASK=31744)		! direction mask
	PARAMETER (XRMASK=255)			! room mask
	PARAMETER (XFMASK=3)			! type mask
	PARAMETER (XFSHFT=256)
	PARAMETER (XASHFT=256)
	PARAMETER (XNORM=1)			! normal exit
	PARAMETER (XNO=2)			! no exit
	PARAMETER (XCOND=3)			! conditional exit
	PARAMETER (XDOOR=4)			! door
	PARAMETER (XMIN=1024)			! minimum direction
	PARAMETER (XMAX=16384)			! maximum direction
	PARAMETER (XNORTH=1024)
	PARAMETER (XNE=2048)
	PARAMETER (XEAST=3072)
	PARAMETER (XSE=4096)
	PARAMETER (XSOUTH=5120)
	PARAMETER (XSW=6144)
	PARAMETER (XWEST=7168)
	PARAMETER (XNW=8192)
	PARAMETER (XUP=9216)
	PARAMETER (XDOWN=10240)
	PARAMETER (XLAUN=11264)
	PARAMETER (XLAND=12288)
	PARAMETER (XENTER=13312)
	PARAMETER (XEXIT=14336)
	PARAMETER (XCROSS=15360)
C
C Actor indices
C
	PARAMETER (PLAYER=1)			! player
	PARAMETER (AROBOT=2)			! robot
	PARAMETER (AMASTR=3)			! dungeon master
C
C Actor flags
C
	PARAMETER (ASTAG=32768)			! staggered
C
C Room flags
C
	PARAMETER (RSEEN=32768)			! seen
	PARAMETER (RLIGHT=16384)		! lighted
	PARAMETER (RLAND=8192)			! land
	PARAMETER (RWATER=4096)			! water
	PARAMETER (RAIR=2048)			! air
	PARAMETER (RSACRD=1024)			! sacred (thief cant visit)
	PARAMETER (RFILL=512)			! contains water
	PARAMETER (RMUNG=256)			! destroyed
	PARAMETER (RBUCK=128)			! part of bucket
	PARAMETER (RHOUSE=64)			! part of house
	PARAMETER (RNWALL=32)			! no walls
	PARAMETER (REND=16)			! part of endgame
C
C Room indices
C
	PARAMETER (WHOUS=2)			! west of house
	PARAMETER (SHOUS=4)			! south of house
	PARAMETER (EHOUS=5)			! east of house
	PARAMETER (KITCH=6)			! kitchen
	PARAMETER (LROOM=8)			! living room
	PARAMETER (CELLA=9)			! cellar
	PARAMETER (MTROL=10)			! troll room
	PARAMETER (MAZE1=11)			! start of maze
	PARAMETER (MGRAT=25)			! grating room
	PARAMETER (MAZ15=30)			! end of maze
	PARAMETER (FORE1=31)			! forest 1
	PARAMETER (FORE2=32)			! forest 2
	PARAMETER (FORE3=33)			! forest 3
	PARAMETER (CLEAR=36)			! clearing
	PARAMETER (RESER=40)			! reservoir
	PARAMETER (STREA=42)			! stream
	PARAMETER (EGYPT=44)			! egypt room
	PARAMETER (ECHOR=49)			! echo room
	PARAMETER (SLIDE=58)			! slide room
	PARAMETER (TSHAF=61)			! top of shaft
	PARAMETER (BSHAF=76)			! bottom of shaft
	PARAMETER (MMACH=77)			! machine room
	PARAMETER (DOME=79)			! dome room
	PARAMETER (MTORC=80)			! torch room
	PARAMETER (CAROU=83)			! carousel room
	PARAMETER (RIDDL=91)			! riddle room
	PARAMETER (LLD1=93)			! entrance to hades
	PARAMETER (LLD2=94)			! land of the living dead
	PARAMETER (TEMP1=96)			! temple
	PARAMETER (TEMP2=97)			! altar
	PARAMETER (MAINT=100)			! maintenance room
	PARAMETER (MCYCL=101)			! cyclops room
	PARAMETER (BLROO=102)			! back of living room
	PARAMETER (TREAS=103)			! thief's treasury
	PARAMETER (RIVR1=107)			! river 1
	PARAMETER (RIVR2=108)			! river 2
	PARAMETER (RIVR3=109)			! river 3
	PARAMETER (RIVR4=112)			! river 4
	PARAMETER (RIVR5=113)			! river 5
	PARAMETER (FCHMP=114)			! over falls
	PARAMETER (SBEACH=116)			! sandy beach
	PARAMETER (FALLS=120)			! top of falls
	PARAMETER (MRAIN=121)			! rainbow room
	PARAMETER (POG=122)			! pot of gold room
	PARAMETER (VLBOT=126)			! volcano bottom
	PARAMETER (VAIR1=127)			! mid air 1
	PARAMETER (VAIR2=128)			! mid air 2
	PARAMETER (VAIR3=129)			! mid air 3
	PARAMETER (VAIR4=130)			! mid air 4
	PARAMETER (LEDG2=131)			! ledge 2
	PARAMETER (LEDG3=132)			! ledge 3
	PARAMETER (LEDG4=133)			! ledge 4
	PARAMETER (MSAFE=135)			! safe room
	PARAMETER (CAGER=140)			! cage room
	PARAMETER (CAGED=141)			! in cage
	PARAMETER (TWELL=142)			! top of well
	PARAMETER (BWELL=143)			! bottom of well
	PARAMETER (ALICE=144)			! alice room
	PARAMETER (ALISM=145)			! alice small room
	PARAMETER (ALITR=146)			! alice trap room
	PARAMETER (MTREE=147)			! up a tree
	PARAMETER (BKENT=148)			! bank entrance
	PARAMETER (BKVW=151)			! bank view west
	PARAMETER (BKVE=152)			! bank view east
	PARAMETER (BKTWI=153)			! bank
	PARAMETER (BKVAU=154)			! bank vault
	PARAMETER (BKBOX=155)			! bank box
	PARAMETER (CRYPT=157)			! crypt
	PARAMETER (TSTRS=158)			! top of stairs
	PARAMETER (MRANT=159)			! mirror anteroom
	PARAMETER (MREYE=160)			! mirror eye
	PARAMETER (MRA=161)			! mirror A
	PARAMETER (MRB=162)			! mirror B
	PARAMETER (MRC=163)			! mirror C
	PARAMETER (MRG=164)			! mirror G
	PARAMETER (MRD=165)			! mirror D
	PARAMETER (FDOOR=166)			! front door
	PARAMETER (MRAE=167)			! mirror A east
	PARAMETER (MRCE=171)			! mirror C east
	PARAMETER (MRCW=172)			! mirror C west
	PARAMETER (MRGE=173)			! mirror G east
	PARAMETER (MRGW=174)			! mirror G west
	PARAMETER (MRDW=176)			! mirror D west
	PARAMETER (INMIR=177)			! in mirror
	PARAMETER (SCORR=179)			! south corridor
	PARAMETER (NCORR=182)			! north corridor
	PARAMETER (PARAP=183)			! parapet
	PARAMETER (CELL=184)			! cell
	PARAMETER (PCELL=185)			! prison cell
	PARAMETER (NCELL=186)			! nirvana cell
	PARAMETER (CPANT=188)			! puzzle anteroom
	PARAMETER (CPOUT=189)			! puzzle exit
	PARAMETER (CPUZZ=190)			! puzzle room
	PARAMETER (PRM=192)			! palantir anteroom
	PARAMETER (PALRM=193)			! palantir room
	PARAMETER (SLID1=194)			! slide 1
	PARAMETER (SLEDG=197)			! slide ledge
C
C Verb indices
C
	PARAMETER (CINTW=1)			! clock interrupt
	PARAMETER (DEADXW=2)			! villain dead
	PARAMETER (FRSTQW=3)			! villain first encounter
	PARAMETER (INXW=4)			! villain wakes up
	PARAMETER (OUTXW=5)			! villain out cold
	PARAMETER (WALKIW=6)			! player walks in
	PARAMETER (FIGHTW=7)			! fighting starts
	PARAMETER (FOOW=8)			! nop
	PARAMETER (SQUEEW=68)			! squeeze
	PARAMETER (STAYW=73)			! stay
	PARAMETER (PRAYW=79)			! pray
	PARAMETER (BLASTW=82)			! blast
	PARAMETER (SCOREW=83)			! score
	PARAMETER (QUITW=84)			! quit
	PARAMETER (FOLLOW=85)			! follow
	PARAMETER (GTHROW=86)			! go through
	PARAMETER (RINGW=87)			! ring
	PARAMETER (DIGW=89)			! dig
	PARAMETER (LEAPW=91)			! leap
	PARAMETER (LOCKW=92)			! lock
	PARAMETER (UNLOKW=93)			! unlock
	PARAMETER (DIAGNW=94)			! diagnose
	PARAMETER (COUNTW=97)			! count
	PARAMETER (READW=100)			! read
	PARAMETER (MELTW=101)			! melt
	PARAMETER (INFLAW=102)			! inflate
	PARAMETER (DEFLAW=103)			! deflate
	PARAMETER (ALARMW=104)			! alarm
	PARAMETER (EXORCW=105)			! exorcise
	PARAMETER (PLUGW=106)			! plug
	PARAMETER (KICKW=107)			! kick
	PARAMETER (WAVEW=108)			! wave
	PARAMETER (RAISEW=109)			! raise
	PARAMETER (LOWERW=110)			! lower
	PARAMETER (RUBW=111)			! rub
	PARAMETER (PUSHW=112)			! push
	PARAMETER (UNTIEW=113)			! untie
	PARAMETER (TIEW=114)			! tie
	PARAMETER (TIEUPW=115)			! tie up
	PARAMETER (TURNW=116)			! turn
	PARAMETER (BREATW=117)			! breathe
	PARAMETER (KNOCKW=118)			! knock
	PARAMETER (LOOKW=119)			! look
	PARAMETER (EXAMIW=120)			! examine
	PARAMETER (SHAKEW=121)			! shake
	PARAMETER (MOVEW=122)			! move
	PARAMETER (TRNONW=123)			! turn on
	PARAMETER (TRNOFW=124)			! turn off
	PARAMETER (OPENW=125)			! open
	PARAMETER (CLOSEW=126)			! close
	PARAMETER (FINDW=127)			! find
	PARAMETER (WAITW=128)			! wait
	PARAMETER (SPINW=129)			! spin
	PARAMETER (BOARDW=130)			! board
	PARAMETER (UNBOAW=131)			! disembark
	PARAMETER (TAKEW=132)			! take
	PARAMETER (INVENW=133)			! inventory
	PARAMETER (EATW=135)			! eat
	PARAMETER (DRINKW=136)			! drink
	PARAMETER (BURNW=137)			! burn
	PARAMETER (MUNGW=138)			! destroy
	PARAMETER (KILLW=139)			! kill
	PARAMETER (SWINGW=140)			! swing
	PARAMETER (ATTACW=141)			! attack
	PARAMETER (WALKW=142)			! walk
	PARAMETER (TELLW=143)			! tell
	PARAMETER (PUTW=144)			! put
	PARAMETER (DROPW=145)			! drop
	PARAMETER (GIVEW=146)			! give
	PARAMETER (POURW=147)			! pour
	PARAMETER (THROWW=148)			! throw
	PARAMETER (HELLOW=151)			! hello
	PARAMETER (LOOKIW=152)			! look in
	PARAMETER (LOOKUW=153)			! look under
	PARAMETER (PUMPW=154)			! pump
	PARAMETER (WINDW=155)			! wind
	PARAMETER (CLMBW=156)			! climb
	PARAMETER (CLMBUW=157)			! climb up
	PARAMETER (CLMBDW=158)			! climb down
	PARAMETER (TRNTOW=159)			! turn to
	PARAMETER (PORONW=160)			! pour on
	PARAMETER (PUTUNW=161)			! put under
	PARAMETER (UTFRMW=162)			! untie from
	PARAMETER (MAKEW=163)			! make
	PARAMETER (OILW=164)			! oil
	PARAMETER (PLAYW=165)			! play
	PARAMETER (SENDW=166)			! send
C
C Object flags
C
	PARAMETER (VISIBT=32768)		! visible
	PARAMETER (READBT=16384)		! readable
	PARAMETER (TAKEBT=8192)			! takeable
	PARAMETER (DOORBT=4096)			! door
	PARAMETER (TRANBT=2048)			! transparent
	PARAMETER (FOODBT=1024)			! edible
	PARAMETER (NDSCBT=512)			! don't describe
	PARAMETER (DRNKBT=256)			! drinkable
	PARAMETER (CONTBT=128)			! container
	PARAMETER (LITEBT=64)			! provides light
	PARAMETER (VICTBT=32)			! victim
	PARAMETER (BURNBT=16)			! burnable
	PARAMETER (FLAMBT=8)			! flaming
	PARAMETER (TOOLBT=4)			! tool
	PARAMETER (TURNBT=2)			! turnable
	PARAMETER (ONBT=1)			! turned on
	PARAMETER (FINDBT=32768)		! find
	PARAMETER (DIGBT=16384)			! digable
	PARAMETER (SCRDBT=8192)			! sacred (thief wont steal)
	PARAMETER (TIEBT=4096)			! tieable
	PARAMETER (CLMBBT=2048)			! climbable
	PARAMETER (ACTRBT=1024)			! actor
	PARAMETER (WEAPBT=512)			! weapon
	PARAMETER (FITEBT=256)			! fighting
	PARAMETER (VILLBT=128)			! villain
	PARAMETER (STAGBT=64)			! staggered
	PARAMETER (TRYBT=32)			! try to take
	PARAMETER (NOCHBT=16)			! don't check
	PARAMETER (OPENBT=8)			! open
	PARAMETER (TCHBT=4)			! touched
	PARAMETER (VEHBT=2)			! vehicle
	PARAMETER (SCHBT=1)			! searchable
C
C Object indices
C
	PARAMETER (GARLI=2)			! garlic
	PARAMETER (FOOD=3)			! hot peppers
	PARAMETER (GUNK=4)			! gunk
	PARAMETER (COAL=5)			! piece of coal
	PARAMETER (MACHI=7)			! machine
	PARAMETER (DIAMO=8)			! diamond
	PARAMETER (TCASE=9)			! trophy case
	PARAMETER (BOTTL=10)			! bottle
	PARAMETER (WATER=11)			! water
	PARAMETER (ROPE=12)			! rope
	PARAMETER (KNIFE=13)			! knife
	PARAMETER (SWORD=14)			! sword
	PARAMETER (LAMP=15)			! lamp
	PARAMETER (BLAMP=16)			! broken lamp
	PARAMETER (RUG=17)			! rug
	PARAMETER (LEAVE=18)			! pile of leaves
	PARAMETER (TROLL=19)			! troll
	PARAMETER (AXE=20)			! axe
	PARAMETER (KEYS=23)			! keys
	PARAMETER (RKNIF=24)			! rusty knife
	PARAMETER (BAGCO=25)			! bag of coins
	PARAMETER (BAR=26)			! platinum bar
	PARAMETER (ICE=30)			! glacier
	PARAMETER (COFFI=33)			! coffin
	PARAMETER (TORCH=34)			! torch
	PARAMETER (TBASK=35)			! true basket
	PARAMETER (FBASK=36)			! false basket
	PARAMETER (TIMBE=38)			! timber
	PARAMETER (IRBOX=39)			! iron box
	PARAMETER (STRAD=40)			! violin
	PARAMETER (GHOST=42)			! spirits
	PARAMETER (TRUNK=45)			! trunk
	PARAMETER (BELL=46)			! bell
	PARAMETER (BOOK=47)			! book
	PARAMETER (CANDL=48)			! candles
	PARAMETER (GUIDE=49)			! guidebook
	PARAMETER (MATCH=51)			! matches
	PARAMETER (MAILB=53)			! mailbox
	PARAMETER (TUBE=54)			! tube of putty
	PARAMETER (PUTTY=55)			! putty
	PARAMETER (WRENC=56)			! wrench
	PARAMETER (SCREW=57)			! screwdriver
	PARAMETER (CYCLO=58)			! cyclops
	PARAMETER (CHALI=59)			! chalice
	PARAMETER (THIEF=61)			! thief
	PARAMETER (STILL=62)			! stiletto
	PARAMETER (WINDO=63)			! window
	PARAMETER (GRATE=65)			! grating
	PARAMETER (DOOR=66)			! door
	PARAMETER (HPOLE=71)			! head on pole
	PARAMETER (RBUTT=79)			! red button
	PARAMETER (LEAK=78)			! leak
	PARAMETER (RAILI=75)			! railing
	PARAMETER (POT=85)			! pot of gold
	PARAMETER (STATU=86)			! statue
	PARAMETER (IBOAT=87)			! inflatable boat
	PARAMETER (DBOAT=88)			! dead boat
	PARAMETER (PUMP=89)			! pump
	PARAMETER (RBOAT=90)			! inflated boat
	PARAMETER (LABEL=91)			! boat label
	PARAMETER (STICK=92)			! stick
	PARAMETER (BARRE=93)			! barrel
	PARAMETER (BUOY=94)			! buoy
	PARAMETER (SHOVE=96)			! shovel
	PARAMETER (GUANO=97)			! pile of guano
	PARAMETER (BALLO=98)			! balloon
	PARAMETER (RECEP=99)			! receptacle
	PARAMETER (BROPE=101)			! braided rope
	PARAMETER (HOOK1=102)			! hook 1
	PARAMETER (HOOK2=103)			! hook 2
	PARAMETER (ZORKM=104)			! zorkmid coin
	PARAMETER (SAFE=105)			! safe
	PARAMETER (CARD=106)			! card
	PARAMETER (SSLOT=107)			! safe slot
	PARAMETER (BRICK=109)			! brick (bomb)
	PARAMETER (FUSE=110)			! fuse
	PARAMETER (GNOME=111)			! volcano gnome
	PARAMETER (BLABE=112)			! balloon label
	PARAMETER (DBALL=113)			! dead balloon
	PARAMETER (TOMB=119)			! tomb
	PARAMETER (HEADS=120)			! heads
	PARAMETER (COKES=121)			! coke bottles
	PARAMETER (LCASE=123)			! large case
	PARAMETER (CAGE=124)			! cage
	PARAMETER (RCAGE=125)			! real cage
	PARAMETER (SPHER=126)			! white crystal sphere
	PARAMETER (SQBUT=127)			! square button
	PARAMETER (FLASK=132)			! flask
	PARAMETER (POOL=133)			! pool of sewage
	PARAMETER (SAFFR=134)			! spices
	PARAMETER (BUCKE=137)			! bucket
	PARAMETER (ECAKE=138)			! eatme cake
	PARAMETER (ORICE=139)			! orange icing
	PARAMETER (RDICE=140)			! red icing
	PARAMETER (BLICE=141)			! blue icing
	PARAMETER (ROBOT=142)			! robot
	PARAMETER (RBTLB=143)			! robot label
	PARAMETER (TTREE=144)			! foot of tree
	PARAMETER (FTREE=145)			! foot of tree
	PARAMETER (BILLS=148)			! pile of bills
	PARAMETER (PORTR=149)			! portrait
	PARAMETER (SCOL=151)			! screen of light
	PARAMETER (ZGNOM=152)			! gnome of Zurich
	PARAMETER (NEST=153)			! nest
	PARAMETER (EGG=154)			! egg
	PARAMETER (BEGG=155)			! broken egg
	PARAMETER (BAUBL=156)			! bauble
	PARAMETER (CANAR=157)			! canary
	PARAMETER (BCANA=158)			! broken canary
	PARAMETER (YLWAL=159)			! yellow wall
	PARAMETER (RDWAL=161)			! red wall
	PARAMETER (PINDR=164)			! pine door
	PARAMETER (RBEAM=171)			! red beam
	PARAMETER (ODOOR=172)			! endgame door
	PARAMETER (QDOOR=173)			! endgame door
	PARAMETER (LDOOR=174)			! endgame door
	PARAMETER (CDOOR=175)			! endgame door
	PARAMETER (NUM1=178)			! numeral 1
	PARAMETER (NUM8=185)			! numeral 8
	PARAMETER (WARNI=186)			! warning
	PARAMETER (CSLIT=187)			! card slit
	PARAMETER (GCARD=188)			! gold card
	PARAMETER (STLDR=189)			! steel door
	PARAMETER (HBELL=190)			! hot bell
	PARAMETER (PLEAK=191)			! alice room leak
	PARAMETER (BROCH=195)			! brochure
	PARAMETER (STAMP=196)			! stamp on brochure
	PARAMETER (PDOOR=197)			! palantir door
	PARAMETER (PLID1=200)			! lid 1
	PARAMETER (PLID2=201)			! lid 2
	PARAMETER (PKH1=202)			! keyhole 1
	PARAMETER (PKH2=203)			! keyhole 2
	PARAMETER (PKEY=205)			! rusty key
	PARAMETER (PALAN=206)			! blue crystal sphere
	PARAMETER (MAT=207)			! welcome mat
	PARAMETER (PAL3=209)			! red crystal sphere
C
	PARAMETER (ITOBJ=250)			! global it
	PARAMETER (OPLAY=251)			! global me
	PARAMETER (EVERY=252)			! global everything
	PARAMETER (VALUA=253)			! global valuables
	PARAMETER (POSSE=254)			! global possessions
	PARAMETER (SAILO=255)			! global sailor
	PARAMETER (TEETH=256)			! global teeth
	PARAMETER (WALL=257)			! global wall
	PARAMETER (HANDS=259)			! global hands
	PARAMETER (LUNGS=260)			! global lungs
	PARAMETER (AVIAT=261)			! global flyer
	PARAMETER (GBROCH=262)			! global brochure
	PARAMETER (GWISH=263)			! global wish (blessing)
	PARAMETER (GLOBAL=264)			! end of universals
	PARAMETER (GRWAL=265)			! global granite wall
	PARAMETER (WNORT=269)			! global north wall
	PARAMETER (GWATE=273)			! global water
	PARAMETER (MASTER=279)			! global dungeon master
	PARAMETER (BUNOBJ=284)			! bunch pseudo object
C
C Misc definitions
C
	PARAMETER (HFACTR=500)
C
C Parser output
C
	LOGICAL PRSWON
	COMMON /PRSVEC/ PRSA,PRSI,PRSO,PRSWON,PRSCON
C
C Parser state
C
	CHARACTER*(WRDLNT) ONAME
	COMMON /PRSSTA/
	1	OFLAG,OACT,OPREP1,OOBJ1,OPREP,ONAME,OPREP2,OOBJ2,
	2	LASTIT,ACT,OBJ1,OBJ2,PREP1,PREP2,
	3	VFLAG,DOBJ,DFL1,DFL2,DFW1,DFW2,
	4	IOBJ,IFL1,IFL2,IFW1,IFW2,
	5	BUNLNT,BUNSUB,BUNVEC(BUNMAX)
	INTEGER SYN(11)
	EQUIVALENCE (SYN(1),VFLAG)
C
C Parser vocabularies
C
	CHARACTER*(WRDLNT) BWORD,PWORD,DWORD,AWORD,OWORD,VWORD
	COMMON /VOCAB/ BWORD(BWMAX),PWORD(PWMAX),DWORD(DWMAX),
	1	AWORD(AWMAX),OWORD(OWMAX),VWORD(VWMAX),PVOC(PWMAX),
	2	DVOC(DWMAX),AVOC(AVMAX),OVOC(OVMAX),VVOC(VVMAX)
C
C Game state
C
	LOGICAL TELFLG
	COMMON /PLAY/ WINNER,HERE,TELFLG,
	1	MOVES,DEATHS,RWSCOR,MXSCOR,MXLOAD,
	2	LTSHFT,BLOC,MUNGRM,HS,EGSCOR,EGMXSC
C
C Screen of light state
C
	COMMON /SCREEN/ FROMDR,SCOLRM,SCOLAC,
	1	SCOLDR(8),SCOLWL(12)
C
C Puzzle room state
C
	COMMON /PUZZLE/ CPDR(16),CPWL(8),CPVEC(64)
C
C Message index
C
	COMMON /RMSG/ MLNT,RTEXT(MMAX)
C
C Miscellaneous variables
C
	CHARACTER*(TEXLNT) INBUF,SUBBUF
	CHARACTER*1 VEDIT
	COMMON /INPUT/ INLNT,INBUF,SUBLNT,SUBBUF
	COMMON /MISC/ MBASE,STRBIT,
	1	PLTIME,SHOUR,SMIN,SSEC,
	2	BATDRP(9),
	3	INPCH,OUTCH,DBCH,
	4	DBGFLG,PRSFLG,GDTFLG,
	5	VMAJ,VMIN,VEDIT
C
C Rooms
C
	COMMON /ROOMS/ RLNT,RDESC2,RDESC1(RMAX),REXIT(RMAX),
	1	RACTIO(RMAX),RVAL(RMAX),RFLAG(RMAX)
C
C Exits
C
	COMMON /EXITS/ XLNT,TRAVEL(XXMAX)
C
	COMMON /CURXT/ XELNT(4),XTYPE,XROOM1,XSTRNG,XACTIO,XOBJ
	EQUIVALENCE (XFLAG,XOBJ)
C
C Objects
C
	COMMON /OBJCTS/ OLNT,ODESC1(OMAX),ODESC2(OMAX),ODESCO(OMAX),
	1	OACTIO(OMAX),OFLAG1(OMAX),OFLAG2(OMAX),OFVAL(OMAX),
	2	OTVAL(OMAX),OSIZE(OMAX),OCAPAC(OMAX),OROOM(OMAX),
	3	OADV(OMAX),OCAN(OMAX),OREAD(OMAX)
C
	COMMON /OROOM2/ R2LNT,O2(R2MAX),R2(R2MAX)
C
C Clock interrupts
C
	LOGICAL CFLAG,CCNCEL
	COMMON /CEVENT/ CLNT,CTICK(CMAX),CACTIO(CMAX),
	1	CFLAG(CMAX),CCNCEL(CMAX)
C
C Villains and demons
C
	LOGICAL THFFLG,SWDACT,THFACT
	COMMON /HACK/ THFPOS,THFFLG,THFACT,SWDACT,SWDSTA
	COMMON /VILL/ VLNT,VILLNS(VMAX),VPROB(VMAX),VOPPS(VMAX),
	1	VBEST(VMAX),VMELEE(VMAX)
C
C Adventurers
C
	COMMON /ADVS/ ALNT,AROOM(AMAX),ASCORE(AMAX),AVEHIC(AMAX),
	1	AOBJ(AMAX),AACTIO(AMAX),ASTREN(AMAX),AFLAG(AMAX)
C
C Flags
C
	LOGICAL FLAGS(FMAX)
	EQUIVALENCE (FLAGS(1),TROLLF)
	INTEGER SWITCH(SMAX)
	EQUIVALENCE (SWITCH(1),BTIEF)
	LOGICAL TROLLF,CAGESF,BUCKTF,CAROFF,CAROZF,LWTIDF
	LOGICAL DOMEF,GLACRF,ECHOF,RIDDLF,LLDF,CYCLOF
	LOGICAL MAGICF,LITLDF,SAFEF,GNOMEF,GNODRF,MIRRMF
	LOGICAL EGYPTF,ONPOLF,BLABF,BRIEFF,SUPERF,BUOYF
	LOGICAL GRUNLF,GATEF,RAINBF,CAGETF,EMPTHF,DEFLAF
	LOGICAL GLACMF,FROBZF,ENDGMF,BADLKF,THFENF,SINGSF
	LOGICAL MRPSHF,MROPNF,WDOPNF,MR1F,MR2F,INQSTF
	LOGICAL FOLLWF,SPELLF,CPOUTF,CPUSHF
	LOGICAL DEADF,ZGNOMF,MATF,PLOOKF,PTOUCF
	LOGICAL BROC1F,BROC2F,EXORBF,EXORCF,PUNLKF
	COMMON /FINDEX/ TROLLF,CAGESF,BUCKTF,CAROFF,CAROZF,LWTIDF,
	1	DOMEF,GLACRF,ECHOF,RIDDLF,LLDF,CYCLOF,
	2	MAGICF,LITLDF,SAFEF,GNOMEF,GNODRF,MIRRMF,
	3	EGYPTF,ONPOLF,BLABF,BRIEFF,SUPERF,BUOYF,
	4	GRUNLF,GATEF,RAINBF,CAGETF,EMPTHF,DEFLAF,
	5	GLACMF,FROBZF,ENDGMF,BADLKF,THFENF,SINGSF,
	6	MRPSHF,MROPNF,WDOPNF,MR1F,MR2F,INQSTF,
	7	FOLLWF,SPELLF,CPOUTF,CPUSHF,
	8	DEADF,ZGNOMF,MATF,PLOOKF,PTOUCF,
	9	BROC1F,BROC2F,EXORBF,EXORCF,PUNLKF
	COMMON /FINDEX/ BTIEF,BINFF
	COMMON /FINDEX/ RVMNT,RVCLR,RVCYC,RVSND,RVGUA
	COMMON /FINDEX/ ORRUG,ORCAND,ORMTCH,ORLAMP
	COMMON /FINDEX/ MDIR,MLOC,POLEUF
	COMMON /FINDEX/ QUESNO,NQATT,CORRCT
	COMMON /FINDEX/ LCELL,PNUMB,ACELL,DCELL,CPHERE
	COMMON /FINDEX/ TTIE,MATOBJ
