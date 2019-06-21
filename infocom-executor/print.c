/* File: print.c */
/* (C)opyright 1987 InfoTaskforce. */

#include	"infocom.h"

int		print_mode ;	/* General Printing Mode           */
int		single_mode ;	/* Mode for printing the next char */
word	word_bank ;		/* There are 3 banks of common     */
						/* words, each 32 words long.      */

print_num ( number )
word	number ;
{
	extern ProcPtr	PrintChar ;

	int				num ;

	if ( number == 0 )
		(*PrintChar)( (word)'0' ) ;
	else
	{
		num = (signed_word)number ;
		if ( num < 0 )
		{
			num = -num ;
			(*PrintChar)( (word)'-' ) ;
		}
		PrintNumber ( num ) ;
	}
}

PrintNumber ( num )
int		num ;
{
	extern ProcPtr	PrintChar ;

	word			ch ;

	if ( num > 9 )
		PrintNumber ( num / 10 ) ;
	ch = '0' + ( num % 10 ) ;
	(*PrintChar)( ch ) ;
}

print2 ( address )
word	address ;
{
	word	page ;
	word	offset ;

	page = address >> 8 ;
	offset = ( address & 0xFF ) << 1 ;
	print_coded ( &page,&offset ) ;
}

print1 ( address )
word	address ;
{
	word	page ;
	word	offset ;

	page = address / block_size ;
	offset = address % block_size ;
	print_coded ( &page,&offset ) ;
}

p_obj ( obj_num )
word	obj_num ;
{
	object		*obj ;
	word		address ;

	obj = obj_addr ( obj_num ) ;
	address = ((obj -> prop_ptr[0]) << 8) + (obj -> prop_ptr[1]) + 1 ;
	print1 ( address ) ;
}

wrt ()
{
	extern word		pc_page ;
	extern word		pc_offset ;

	print_coded ( &pc_page,&pc_offset ) ;
	fix_pc () ;
}

writeln ()
{
	wrt () ;
	new_line () ;
	ret_true () ;
}

new_line ()
{
	extern byte		*p_buff_strt ;
	extern byte		*p_buff_ptr ;

	*p_buff_ptr++ = '\n' ;
	print_buffer ( p_buff_ptr ) ;
	p_buff_ptr = p_buff_strt ;
}

print_char ( ch )
word	ch ;
{
	extern byte		*p_buff_strt ;
	extern byte		*p_buff_ptr ;
	extern byte		*p_buff_end ;

	byte			*ptr ;

	if ( p_buff_ptr == p_buff_end )
	{
		if ( ch == (word)' ' )
		{
			cr_print_buffer ( p_buff_end ) ;
			p_buff_ptr = p_buff_strt ;
			return;
		}
		else
		{
			--p_buff_ptr ;
			while ((*p_buff_ptr != (byte)' ') &&
											(p_buff_ptr != p_buff_strt))
				--p_buff_ptr ;
			if ( p_buff_ptr == p_buff_strt )
				cr_print_buffer ( p_buff_end ) ;
			else
			{
				cr_print_buffer ( ++p_buff_ptr ) ;
				ptr = p_buff_strt ;
				while ( p_buff_ptr != p_buff_end )
					*ptr++ = *p_buff_ptr++ ;
				p_buff_ptr = ptr ;
			}
		}
	}
	*p_buff_ptr++ = (byte)ch ;
}

print_coded ( page,offset )
word	*page,*offset ;
{
	word	data ;

	/*
		Print mode 			(& 0x80) != 0 :	Common Word ;
						          = 0 :	Lower Case Letter ;
						          = 1 :	Upper Case Letter ;
						          = 2 :	Number or Symbol ;
						          = 3 :	ASCII Letter - first byte ;
						(& 0x40) != 0 :	ASCII Letter - second byte ;
	*/

	print_mode = 0 ;
	single_mode = 0 ;

	/* Last word has high bit set */

	do
	{
		data = get_word ( page,offset ) ;
		decode ( data ) ;
	}
	while (( data & 0x8000 ) == 0 ) ;
}

decode ( data ) 
word	data ;
{
	extern ProcPtr	PrintChar ;
	extern byte		*common_word_ptr ;

	word			page ;
	word			offset ;
	word			code ;
	int				i ;
	byte			*ptr ;
	char			ch[3] ;

	/* Reduce word to 3 characters of 5 bits */

	code = data ;
	for ( i = 0 ; i <= 2 ; i++ )
	{
		ch[i] = code & 0x1F ;
		code >>= 5 ;
	}

	/* Print each character */

	for ( i = 2 ; i >= 0 ; i-- )
	{
		if ( single_mode & 0x80 )
		{
			/* Print a Special Word */

			ptr = common_word_ptr + word_bank + (int)( ch[i] << 1 ) ;
			page = *ptr++ ;
			offset = *ptr << 1 ;
			print_coded ( &page,&offset ) ;
			single_mode = print_mode ;
			continue ;
		}
		if ( single_mode < 3 )
		{
			/* Print a single character */

			letter ( ch[i] ) ;
			continue ;
		}
		if ( single_mode == 3 )
		{
			/*
				Print ASCII character - store the high 3 bits of
				char in the low 3 bits of the current printing mode.
			*/

			single_mode = 0x40 + ch[i] ;
			continue ;
		}
		if ( single_mode & 0x40 )
		{
			/*
				Print an ASCII character - consists of the current
				character as the low 5 bits and the high 3 bits coming
				from the low 3 bits of the current printing mode.
			*/

			ch[i] += ( single_mode & 0x03 ) << 5 ;
			(*PrintChar)( (word)ch[i] ) ;
			single_mode = print_mode ;
		}
	}
}

letter ( ch )
char	ch ;
{
	extern char		table[] ;
	extern ProcPtr	PrintChar ;

	if ( ch == 0 )
	{
		(*PrintChar)( (word)' ' ) ;
		single_mode = print_mode ;
		return ;
	}

	if ( ch <= 3 )
	{
		/* Set single_mode to "Common Word" & set word_bank */

		single_mode |= 0x80 ;
		word_bank = ( ch - 1 ) << 6 ;
		return ;
	}

	if (( ch == 4 ) || ( ch == 5 ))
	{
		/* Switch printing modes */

		if ( single_mode == 0 )
			single_mode = ch - 3 ;
		else
		{
			if ( single_mode == ch - 3 )
				single_mode = 0 ;
			print_mode = single_mode ;
		}
		return ;
	}

	if (( ch == 6 ) && ( single_mode == 2 ))
	{
		/* Increment printing mode to 3 - ASCII Letter. */

		++single_mode ;
		return ;
	}

	if (( ch == 7 ) && ( single_mode == 2 ))
	{
		/* Print a Carriage Return */

		new_line () ;
		single_mode = print_mode ;
		return ;
	}

	/* None of the above, so this must be a single character */

	(*PrintChar)( (word)table[( single_mode * 26 ) + ch - 6] ) ;
	single_mode = print_mode ;
}

show_score ()
{
	extern word		score_time ;
	extern byte		*s_buff_strt ;
	extern byte		*s_buff_ptr ;
	extern ProcPtr	PrintChar ;

	word			hour ;
	word			minutes ;
	char			*score	= "Score: " ;
	char			*time	= "Time:  " ;
	char			ch ;
	ProcPtr			save ;

	s_buff_ptr = s_buff_strt ;
	*s_buff_ptr++ = status_buff_size ;
	*s_buff_ptr++ = ' ' ;
	save = PrintChar ;
	PrintChar = put_status ;

	p_obj ( load_var ( 0x10 ) ) ;

	if ( score_time == 0 )
	{
		while ( s_buff_ptr <= s_buff_strt + 0x26 )
			put_status ( (word)' ' ) ;
		copy_string ( score ) ;
		print_num ( load_var ( 0x11 ) ) ;
		put_status ( (word)'/' ) ;
		print_num ( load_var ( 0x12 ) ) ;
	}
	else
	{
		while ( s_buff_ptr <= s_buff_strt + 0x2A )
			put_status ( (word)' ' ) ;
		copy_string ( time ) ;
		hour = load_var ( 0x11 ) ;

		/* Convert 24 hour time to AM/PM */

		ch = 'A' ;
		if ( hour >= 12 )
		{
			hour -= 12 ;
			ch = 'P' ;
		}
		if ( hour == 0 )
			hour = 12 ;

		/* Print Time */

		print_num ( hour ) ;
		put_status ( (word)':' ) ;

		/* Can't use print_num for minutes since we want leading zeros */

		minutes = load_var ( 0x12 ) ;
		put_status ((word)((minutes / 10) + '0')) ;
		put_status ((word)((minutes % 10) + '0')) ;
		put_status ( (word)' ' ) ;
		put_status ( (word)ch ) ;
		put_status ( (word)'M' ) ;
	}
	while ( s_buff_ptr <= s_buff_strt + status_buff_size )
		put_status ( (word)' ' ) ;
	print_status () ;
	PrintChar = save ;
}

put_status ( ch )
word	ch ;
{
	extern byte		*s_buff_ptr ;

	*s_buff_ptr++ = (byte)ch ;
}

copy_string ( s )
char	*s ;
{
	char		*ptr ;

	ptr = s ;
	while ( *ptr != '\0' )
		put_status ( (word)*ptr++ ) ;
}
