/* File: functions.c */
/* (C)opyright 1987 InfoTaskforce. */

#include	"infocom.h"

plus ( a,b )
word	a,b ;
{
	store ( a + b ) ;
}

minus ( a,b )
word	a,b ;
{
	store ( a - b ) ;
}

times ( a,b )
word	a,b ;
{
	store ( a * b ) ;
}

divide ( a,b )
word	a,b ;
{
	store ( a / b ) ;
}

mod ( a,b )
word	a,b ;
{
	store ( a % b ) ;
}

random ( num )
word	num ;
{
	extern word		random1 ;
	extern word		random2 ;

	word			temp ;

	temp = random1 << 1 ;
	random1 = random2 ;
	if ( random2 & 0x8000 )
		++temp ;
	random2 ^= temp ;
	store ((( random2 & 0x7FFF ) % num ) + 1 ) ;
}

LTE ( a,b )
word	a,b ;
{
	if ( (signed_word)a < (signed_word)b )
		ret_value ( true ) ;
	else
		ret_value ( false ) ;
}

GTE ( a,b )
word	a,b ;
{
	if ( (signed_word)a > (signed_word)b )
		ret_value ( true ) ;
	else
		ret_value ( false ) ;
}

bit ( a,b )
word	a,b ;
{
	if (( b & ( ~a )) == 0 )
		ret_value ( true ) ;
	else
		ret_value ( false ) ;
}

or ( a,b )
word	a,b ;
{
	store ( a | b ) ;
}

not ( a )
word	a ;
{
	store ( ~a ) ;
}

and ( a,b )
word	a,b ;
{
	store ( a & b ) ;
}

compare ( a,b,c,d,num )
word	a,b,c,d,num ;
{
	Boolean		equal ;

	equal = false ;
	if ( num == 4 )
		equal |= ( a == d ) ;
	if ( num >= 3 )
		equal |= ( a == c ) ;
	equal |= ( a == b ) ;
	if ( equal )
		ret_value ( true ) ;
	else
		ret_value ( false ) ;
}

cp_zero ( a )
word	a ;
{
	if ( a == 0 )
		ret_value ( true ) ;
	else
		ret_value ( false ) ;
}
