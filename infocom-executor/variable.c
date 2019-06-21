/* File: variable.c */
/* (C)opyright 1987 InfoTaskforce. */

#include	"infocom.h"

get_var ( var )
word	var ;
{
	store ( load_var ( var ) ) ;
}

word
load_var ( var )
word	var ;
{
	extern byte		*global_ptr ;
	extern word		*stack_var_ptr ;
	extern word		*stack ;

	word			value ;
	word			*svp ;
	byte			*ptr ;

	if ( var == 0 )
		return ( *stack ) ;
	else
	{
		if ( var < local_vars )
		{
			svp = stack_var_ptr - ( var - 1 ) ;
			return ( *svp ) ;
		}
		else
		{
			ptr = global_ptr + (( var - local_vars ) << 1 ) ;
			value = *(ptr++) << 8 ;
			return ( (word)(value + *ptr) ) ;
		}
	}
}

put_var ( var,value )
word	var,value ;
{
	extern byte		*global_ptr ;
	extern word		*stack_var_ptr ;
	extern word		*stack ;

	word			*svp ;
	byte			*ptr ;

	if ( var == 0 )
		*stack = value ;
	else
	{
		if ( var < local_vars )
		{
			svp = stack_var_ptr - ( var - 1 ) ;
			*svp = value ;
		}
		else
		{
			ptr = global_ptr + (( var - local_vars ) << 1 ) ;
			*ptr++ = value >> 8 ;
			*ptr = value ;
		}
	}
}

push ( value )
word	value ;
{
	extern word		*stack ;

	*(--stack) = value ;
}

pop ( var )
word	var ;
{
	extern word		*stack ;

	put_var ( var,*stack++ ) ;
}

inc_var ( var )
word	var ;
{
	put_var ( var,load_var ( var ) + 1 ) ;
}

dec_var ( var )
word	var ;
{
	put_var ( var,load_var ( var ) - 1 ) ;
}

inc_chk ( var,threshold )
word	var,threshold ;
{
	word	value ;

	value = load_var ( var ) + 1 ;
	put_var ( var,value ) ;
	if ( (signed_word)value > (signed_word)threshold )
		ret_value ( true ) ;
	else
		ret_value ( false ) ;
}

dec_chk ( var,threshold )
word	var,threshold ;
{
	word	value ;

	value = load_var ( var ) - 1 ;
	put_var ( var,value ) ;
	if ( (signed_word)value < (signed_word)threshold )
		ret_value ( true ) ;
	else
		ret_value ( false ) ;
}

word
load ( mode )
int		mode ;
{
	/*
		Mode 0 = Immediate Word ;
		Mode 1 = Immediate Byte ;
		Mode 2 = Variable ;
	*/

	extern word		*stack ;

	word			var ;

	--mode ;
	if ( mode < 0 )
		return ( next_word () ) ;
	if ( mode == 0 )
		return ( (word)next_byte () ) ;
	var = next_byte () ;
	if ( var == 0 )
		return ( *stack++ ) ;
	else
		return ( load_var ( var ) ) ;
}
