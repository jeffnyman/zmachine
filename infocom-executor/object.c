/* File: object.c */
/* (C)opyright 1987 IntTaskforce. */

#include	"infocom.h"

transfer ( o1,o2 )
word	o1,o2 ;
{
	extern Boolean	xfer ;

	object			*obj1 ;
	object			*obj2 ;

	remove_obj ( o1 ) ;
	obj1 = obj_addr ( o1 ) ;
	obj2 = obj_addr ( o2 ) ;
	obj1 -> link = obj2 -> holds ;
	obj1 -> location = (byte) o2 ;
	obj2 -> holds = (byte) o1 ;
	if ( xfer )
	{
		print_char ( (word)'[' ) ;
		p_obj ( o1 ) ;
		print_char ( (word)' ' ) ;
		print_char ( (word)'-' ) ;
		print_char ( (word)'>' ) ;
		print_char ( (word)' ' ) ;
		p_obj ( o2 ) ;
		print_char ( (word)']' ) ;
		new_line () ;
	}
}

remove_obj ( obj_num )
word	obj_num ;
{
	object		*obj1 ;
	object		*obj2 ;

	obj1 = obj_addr ( obj_num ) ;
	if ( obj1 -> location != 0 )
	{
		obj2 = obj_addr ((word)(obj1 -> location)) ;
		if ( obj2 -> holds == (byte)obj_num )
			obj2 -> holds = obj1 -> link ;
		else
		{
			obj2 = obj_addr ((word)(obj2 -> holds)) ;
			while ( obj2 -> link != (byte)obj_num )
				obj2 = obj_addr ((word)(obj2 -> link)) ;
			obj2 -> link = obj1 -> link ;
		}
		obj1 -> location = 0 ;
		obj1 -> link = 0 ;
	}
}

test_attr ( obj_num,attr )
word	obj_num,attr ;
{
	extern Boolean	attribute ;

	object			*obj ;
	byte			bit ;
	int				i ;

	obj = obj_addr ( obj_num ) ;
	bit = 0x80 ;
	for ( i = 0 ; i < attr % 8 ; i++ )
		bit >>= 1 ;
	if ( attribute )
	{
		print_char ( (word)'[' ) ;
		p_obj ( obj_num ) ;
		print_char ( (word)'(' ) ;
		print_num ( attr ) ;
		print_char ( (word)')' ) ;
		print_char ( (word)' ' ) ;
		print_char ( (word)'=' ) ;
		print_char ( (word)'=' ) ;
		print_char ( (word)' ' ) ;
		if ( obj -> attributes[attr / 8] & bit )
			print_num ( 1 ) ;
		else
			print_num ( 0 ) ;
		print_char ( (word)']' ) ;
		new_line () ;
	}
	if ( obj -> attributes[attr / 8] & bit )
		ret_value ( true ) ;
	else
		ret_value ( false ) ;
}

set_attr ( obj_num,attr )
word	obj_num,attr ;
{
	extern Boolean	attribute ;

	object			*obj ;
	byte			bit ;
	int				i ;

	obj = obj_addr ( obj_num ) ;
	bit = 0x80 ;
	for ( i = 0 ; i < attr % 8 ; i++ )
		bit >>= 1 ;
	obj -> attributes[attr / 8] |= bit ;
	if ( attribute )
	{
		print_char ( (word)'[' ) ;
		p_obj ( obj_num ) ;
		print_char ( (word)'(' ) ;
		print_num ( attr ) ;
		print_char ( (word)')' ) ;
		print_char ( (word)' ' ) ;
		print_char ( (word)':' ) ;
		print_char ( (word)'=' ) ;
		print_char ( (word)' ' ) ;
		print_num ( 1 ) ;
		print_char ( (word)']' ) ;
		new_line () ;
	}
}

clr_attr ( obj_num,attr )
word	obj_num,attr ;
{
	extern Boolean	attribute ;

	object			*obj ;
	byte			bit ;
	int				i ;

	obj = obj_addr ( obj_num ) ;
	bit = 0x80 ;
	for ( i = 0 ; i < attr % 8 ; i++ )
		bit >>= 1 ;
	obj -> attributes[attr / 8] &= ( ~bit ) ;
	if ( attribute )
	{
		print_char ( (word)'[' ) ;
		p_obj ( obj_num ) ;
		print_char ( (word)'(' ) ;
		print_num ( attr ) ;
		print_char ( (word)')' ) ;
		print_char ( (word)' ' ) ;
		print_char ( (word)':' ) ;
		print_char ( (word)'=' ) ;
		print_char ( (word)' ' ) ;
		print_num ( 0 ) ;
		print_char ( (word)']' ) ;
		new_line () ;
	}
}

get_loc ( obj_num )
word	obj_num ;
{
	object			*obj ;

	obj = obj_addr ( obj_num ) ;
	store ((word)(obj -> location)) ;
}

get_holds ( obj_num )
word	obj_num ;
{
	object			*obj ;

	obj = obj_addr ( obj_num ) ;
	store ((word)(obj -> holds)) ;
	if ( obj -> holds )
		ret_value ( true ) ;
	else
		ret_value ( false ) ;
}

get_link ( obj_num )
word	obj_num ;
{
	object			*obj ;

	obj = obj_addr ( obj_num ) ;
	store ((word)(obj -> link)) ;
	if ( obj -> link )
		ret_value ( true ) ;
	else
		ret_value ( false ) ;
}

check_loc ( o1,o2 )
word	o1,o2 ;
{
	object			*obj ;

	obj = obj_addr ( o1 ) ;
	if ( obj -> location == (byte)o2 )
		ret_value ( true ) ;
	else
		ret_value ( false ) ;
}

object
*obj_addr ( obj_num )
word		obj_num ;
{
	extern object	*obj_list ;

	return ((object *)((byte *)obj_list + (obj_num * obj_size) +
														obj_offset)) ;
}
