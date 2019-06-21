/* File: support.c */
/* (C)opyright 1987 InfoTaskforce. */

#include	"infocom.h"

null ()
{
	/* The NULL function */
}

restart ()
{
	extern word		status ;

	new_line () ;
	status = restart_game ;
}

quit ()
{
	extern word		status ;

	status = quit_game ;
	close_file () ;
	deallocate () ;
}

verify ()
{
	extern word		resident_blocks ;
	extern header	data_head ;

	word			addr_hi ;
	word			addr_lo ;
	word			page ;
	word			offset ;
	word			sum ;
	word			save ;

	echo ( "Interpreter: C Version 1.0\n" ) ;
	addr_lo = ( data_head.verify_length & 0xFF ) << 1 ;
	addr_hi = data_head.verify_length >> 8 ;
	save = resident_blocks ;

	resident_blocks = 0 ;
	page = 0 ;
	offset = sizeof ( header ) ;
	sum = 0 ;
	while (( page != addr_hi ) || ( offset != addr_lo ))
		sum += get_byte ( &page,&offset ) ;

	resident_blocks = save ;
	if ( sum == data_head.verify_checksum )
		ret_value ( true ) ;
	else
		ret_value ( false ) ;
}

store ( value )
word	value ;
{
	extern word		*stack ;

	word			var ;

	var = next_byte () ;
	if ( var == 0 )
		*(--stack) = value ;
	else
		put_var ( var,value ) ;
}

ret_value ( result )
word	result ;
{
	extern word		pc_offset ;

	word	branch ;

	branch = next_byte () ;

	/* Test bit 7 */
	if (( branch & 0x80 ) != 0 )
	{
		/* Clear bit 7 */
		branch &= 0x7F ;
		++result ;
	}

	/* Test bit 6 */
	if (( branch & 0x40 ) == 0 )
	{
		branch = ( branch << 8 ) + next_byte () ;
		/* Test bit D. If set, make branch negative. */
		if ( branch & 0x2000 )
			branch |= 0xC000 ;
	}
	else
		/* Clear bit 6 */
		branch &= 0xBF ;

	if (( --result ) != 0 )
	{
		switch ( branch )
		{
			case 0 :	ret_false () ;
						break ;
			case 1 :	ret_true () ;
						break ;
			default :	pc_offset += ( branch - 2 ) ;
						fix_pc () ;
		}
	}
}

byte
get_byte ( page,offset )
word	*page,*offset ;
{
	extern word		resident_blocks ;
	extern byte		*base_ptr ;

	byte			*ptr ;

	if ( *page < resident_blocks )
		ptr = base_ptr + ((long_word)*page * block_size) + *offset ;
	else
		ptr = fetch_page ( *page ) + *offset ;
	++(*offset) ;
	if ( *offset == block_size )
	{
		*offset = 0 ;
		++(*page) ;
	}
	return ( *ptr ) ;
}

word
get_word ( page,offset )
word	*page,*offset ;
{
	word	temp ;

	temp = get_byte ( page,offset ) << 8 ;
	return ( temp + get_byte ( page,offset ) ) ;
}

byte
next_byte ()
{
	extern word		pc_offset ;
	extern byte		*prog_block_ptr ;

	register byte		*ptr ;

	ptr = prog_block_ptr + pc_offset++ ;
	if ( pc_offset == block_size )
		fix_pc () ;
	return ( *ptr ) ;
}

word
next_word ()
{
	word	temp ;

	temp = next_byte () << 8 ;
	return ( temp + next_byte () ) ;
}

error ( err_num )
int		err_num ;
{
	char	ch[4] ;

	/*
		'err_num' must be between 00 and 99 inclusive.
		We can't use print_char or print_num because the print
		buffer may not yet have been created or initialised.
	*/

	echo ( "\nInternal Error " ) ;
	if ( err_num != err_memory )
	{
		ch[0] = '#' ;
		ch[1] = '0' + ( err_num / 10 ) ;
		ch[2] = '0' + ( err_num % 10 ) ;
		ch[3] = 0 ;
		echo ( ch ) ;
	}
	echo ( "\n" ) ;
	quit () ;
}
