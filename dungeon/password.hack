	implicit integer(a-z)
	character*8 outw
	call encryp ('ABCDEFGH',outw)
	type 100,outw
100	format (' ABCDEFGH encrypts as ',a)
	call exit
	end

C ENCRYP--	Encrypt password
C
C Declarations
C
	SUBROUTINE ENCRYP(INW,OUTW)
	IMPLICIT INTEGER(A-Z)
	INCLUDE 'dparam.for'
	CHARACTER*8 INW,OUTW,KEYW
	INTEGER UINW(8),UKEYW(8)
	DATA KEYW/'ECOVXRMS'/
C
	ICHARA=ICHAR('A')-1			! character base.
	UINWS=0					! unbiased inw sum.
	UKEYWS=0				! unbiased keyw sum.
	J=1					! pointer in keyword.
	DO 100 I=1,WRDLNT			! unbias, compute sums.
	  UKEYW(I)=ICHAR(KEYW(I:I))-ICHARA	! strip ascii.
	  IF(ICHAR(INW(J:J)).LE.ICHARA) J=1	! recycle on bad.
	  UINW(I)=ICHAR(INW(J:J))-ICHARA
	  UKEYWS=UKEYWS+UKEYW(I)
	  UINWS=UINWS+UINW(I)
	  J=J+1
100	CONTINUE
C
	USUM=MOD(UINWS,8)+(8*MOD(UKEYWS,8))	! compute mask.
	DO 200 I=1,8
	  J=(UINW(I).XOR.UKEYW(I).XOR.USUM).AND.31
	  USUM=MOD(USUM+1,32)
	  IF(J.GT.26) J=MOD(J,26)
	  OUTW(I:I)=CHAR(MAX0(1,J)+ICHARA)
200	CONTINUE
	RETURN
C
	END
