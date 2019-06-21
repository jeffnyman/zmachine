/* File: property.c */
/* (C)opyright 1987 InfoTaskforce. */

#include	"infocom.h"

property
prop_addr ( obj )
object	*obj ;
{
	extern byte		*base_ptr ;

	property		p ;

	p = base_ptr + ((obj -> prop_ptr[0]) << 8) + (obj -> prop_ptr[1]) ;
	return ( p + (*p << 1) + 1 ) ;
}

property
next_addr ( p )
property	p ;
{
	return ( p + (*p >> 5) + 2 ) ;
}

getprop ( obj_num,prop_num )
word	obj_num,prop_num ;
{
	extern object	*obj_list ;

	property		p ;
	word			p_num ;
	word			prop ;

	p = prop_addr ( obj_addr ( obj_num )) ;
	p_num = *p & 0x1F ;

	/* Properties are kept in descending order */

	while ( p_num > prop_num )
	{
		p = next_addr ( p ) ;
		p_num = *p & 0x1F ;
	}
	if ( p_num < prop_num )
	{
		prop_num = (--prop_num) << 1 ;
		p = (property) obj_list + prop_num ;
		prop = (*p++) << 8 ;
		prop += *p ;
	}
	else
	{
		if ( (*p++) & 0x20 )
		{
			prop = (*p++) << 8 ;
			prop += *p ;
		}
		else
			prop = *p ;
	}
	store ( prop ) ;
}

put_prop ( obj_num,prop_num,value )
word	obj_num,prop_num,value ;
{
	property		p ;
	word			p_num ;

	p = prop_addr ( obj_addr ( obj_num )) ;
	p_num = *p & 0x1F ;

	/* Properties are kept in descending order */

	while ( p_num > prop_num )
	{
		p = next_addr ( p ) ;
		p_num = *p & 0x1F ;
	}
	if ( p_num < prop_num )
		error ( err_put_prop ) ;
	else
	{
		if ( (*p++) & 0x20 )
		{
			(*p++) = value >> 8 ;
			*p = value ;
		}
		else
			*p = value ;
	}
}

get_next_prop ( obj_num,prop_num )
word	obj_num,prop_num ;
{
	property	p ;
	word		p_num ;

	p = prop_addr ( obj_addr ( obj_num )) ;
	if ( prop_num != 0 )
	{
		p_num = *p & 0x1F ;

		/* Properties are kept in descending order */

		while ( p_num > prop_num )
		{
			p = next_addr ( p ) ;
			p_num = *p & 0x1F ;
		}
		if ( p_num < prop_num )
			error ( err_next_prop ) ;
		else
			p = next_addr ( p ) ;
	}
	store ( (word)(*p & 0x1F) ) ;
}

load_word_array ( base,offset )
word	base,offset ;
{
	word	page ;
	word	page_offset ;

	base += ( offset << 1 ) ;
	page = base / block_size ;
	page_offset = base % block_size ;
	store ( get_word ( &page,&page_offset ) ) ;
}

load_byte_array ( base,offset )
word	base,offset ;
{
	word	page ;
	word	page_offset ;

	base += offset ;
	page = base / block_size ;
	page_offset = base % block_size ;
	store ( (word)get_byte ( &page,&page_offset ) ) ;
}

save_word_array ( base,offset,value )
word	base,offset,value ;
{
	extern byte		*base_ptr ;

	byte			*ptr ;

	/* The quantity added to 'base_ptr' must be a word */

	base += ( offset << 1 ) ;
	ptr = base_ptr + base ;
	(*ptr++) = value >> 8 ;
	*ptr = value ;
}

save_byte_array ( base,offset,value )
word	base,offset,value ;
{
	extern byte		*base_ptr ;

	byte			*ptr ;

	/* The quantity added to 'base_ptr' must be a word */

	base += offset ;
	ptr = base_ptr + base ;
	*ptr = value ;
}

get_prop_addr ( obj_num,prop_num )
word	obj_num,prop_num ;
{
	extern byte		*base_ptr ;

	property		p ;
	word			p_num ;

	p = prop_addr ( obj_addr ( obj_num )) ;
	p_num = *p & 0x1F ;

	/* Properties are kept in descending order */

	while ( p_num > prop_num )
	{
		p = next_addr ( p ) ;
		p_num = *p & 0x1F ;
	}
	if ( p_num < prop_num )
		store ( 0 ) ;
	else
		store ( (word)(p + 1 - base_ptr) ) ;
}

get_p_len ( prop_num )
word	prop_num;
{
	extern byte		*base_ptr ;

	property		p ;

	p = base_ptr + prop_num - 1 ;
	store( (word)((*p >> 5) + 1) ) ;
}
