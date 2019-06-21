/* File: input.c */
/* (C)opyright 1987 InfoTaskforce. */

#include	"infocom.h"

word	coded[2] ;

input ( char_offset,word_offset )
word	char_offset,word_offset ;
{
	extern byte		*base_ptr ;
	extern byte		*p_buff_strt ;
	extern byte		*p_buff_ptr ;
	extern int		linecount;

	byte			*in_buf_strt ;
	byte			*in_buf_end ;
	byte			*word_buff_strt ;

	/* Empty the Print Buffer */

	show_score () ;
	print_buffer ( p_buff_ptr ) ;
	p_buff_ptr = p_buff_strt ;
	linecount = 0;

	/* Get an Input Line and Parse it */

	in_buf_strt = base_ptr + char_offset ;
	word_buff_strt = base_ptr + word_offset ;
	in_buf_end = read_line ( in_buf_strt ) ;
	parse ( in_buf_strt,in_buf_end,word_buff_strt ) ;
}

byte
*read_line ( buffer )
byte	*buffer ;
{
	byte	*char_ptr ;
	byte	char_count ;
	char	ch ;

	/* First byte of buffer contains buffer length */

	char_ptr = buffer ;
	char_count = *char_ptr++ ;
	ch = read_char () ;
	while ( ch != '\n' )
	{
		if (( ch == back_space ) && (( char_ptr - 1 ) != buffer ))
		{
			--char_ptr ;
			++char_count ;
			ch = read_char () ;
		}
		else
		{
			/* Make it Lower Case */

			if (( ch >= 'A' ) && ( ch <= 'Z' ))
				ch += ( 'a' - 'A' ) ;
			*char_ptr++ = (byte)ch ;
			--char_count ;

			/* Check to see if Buffer is full */

			if ( char_count == 0 )
			{
				/* Flush remaining characters */

				new_line () ;
				echo ( "Input line too long. Flushing: " ) ;
				ch = read_char () ;
				while ( ch != '\n' )
				{
					print_char ( (word)ch ) ;
					ch = read_char () ;
				}
				new_line () ;
			}
			else
				ch = read_char () ;
		}
	}
	/* Return Pointer to the End of the Buffer */

	return ( char_ptr ) ;
}

parse ( in_buf_strt,in_buf_end,word_buff_strt )
byte	*in_buf_strt,*in_buf_end,*word_buff_strt ;
{
	extern byte		*ws_buff_strt ;
	extern byte		*end_of_sentence ;

	byte			*last_word ;
	byte			*word_ptr ;
	byte			*char_ptr ;
	byte			*ws ;
	byte			the_word[8] ;
	byte			word_count ;
	byte			ch ;
	int				i ;
	Boolean			white_space ;

	word_count = 0 ;
	char_ptr = in_buf_strt + 1 ;
	word_ptr = word_buff_strt + 2 ;

	i = 0 ;
	while (( char_ptr != in_buf_end ) || ( i != 0 ))
	{
		i = 0 ;
		last_word = char_ptr ;
		white_space = false ;
		while (( char_ptr != in_buf_end ) && ( !white_space ))
		{
			ch = *char_ptr++ ;
			ws = ws_buff_strt ;
			while (( *ws != ch ) && ( *ws != 0 ))
				++ws ;
			if ( *ws == ch )
			{
				white_space = true ;
				if ( i != 0 )
					--char_ptr ;
				if (( i == 0 ) && ( ws < end_of_sentence ))
					the_word[i++] = ch ;
			}
			else
			{
				if ( i < 6 )
					the_word[i++] = ch ;
			}
		}

		if ( i != 0 )
		{

			/* First byte of buffer contains the buffer length */

			if ( word_count == *word_buff_strt )
			{
				echo ( "Too many words typed. Flushing: " ) ;
				*in_buf_end = 0 ;
				echo ( (char *)last_word ) ;
				new_line () ;
				*(word_buff_strt + 1) = *word_buff_strt ;
				return ;
			}
			else
			{
				++word_count ;
				*( word_ptr + 2 ) = (byte)(char_ptr - last_word) ;
				*( word_ptr + 3 ) = (byte)(last_word - in_buf_strt) ;
				the_word[i] = 0 ;
				look_up ( the_word,word_ptr ) ;
				word_ptr += 4 ;
			}
		}
	}
	*(word_buff_strt + 1) = word_count ;
}

look_up ( the_word,word_ptr )
byte	*the_word,*word_ptr ;
{
	extern word		num_vocab_words ;
	extern word		vocab_entry_size ;
	extern byte		*base_ptr ;
	extern byte		*strt_vocab_table ;
	extern byte		*end_vocab_table ;

	byte			*vocab_strt ;
	byte			*v_ptr ;
	word			first ;
	word			second ;
	word			shift ;
	word			chop ;
	word			offset ;
	Boolean			found ;

	encode ( the_word ) ;
	shift = num_vocab_words ;
	chop = vocab_entry_size ;
	shift >>= 1 ;
	do
	{
		chop <<= 1 ;
		shift >>= 1 ;
	} while ( shift != 0 ) ;
	vocab_strt = strt_vocab_table + chop - vocab_entry_size ;
	found = false ;
	do
	{
		chop >>= 1 ;
		v_ptr = vocab_strt ;
		first = *(v_ptr++) << 8 ;
		first += *v_ptr++ ;
		if ( first == coded[0] )
		{
			second = *(v_ptr++) << 8 ;
			second += *v_ptr++ ;
			if ( second == coded[1] )
				found = true ;
			else
			{
				if ( coded[1] > second )
				{
					vocab_strt += chop ;
					if ( vocab_strt > end_vocab_table )
						vocab_strt = end_vocab_table ;
				}
				else
					vocab_strt -= chop ;
			}
		}
		else
		{
			if ( coded[0] > first )
			{
				vocab_strt += chop ;
				if ( vocab_strt > end_vocab_table )
					vocab_strt = end_vocab_table ;
			}
			else
				vocab_strt -= chop ;
		}
	} while (( chop >= vocab_entry_size ) && ( !found )) ;

	if ( !found )
		offset = 0 ;
	else
		offset = vocab_strt - base_ptr ;
	*(word_ptr + 1) = (byte)offset ;
	*word_ptr = (byte)(offset >> 8) ;
}

encode ( the_word )
byte	*the_word ;
{
	word	data[6] ;
	word	mode ;
	word	offset ;
	byte	*ptr ;
	byte	ch ;
	int		count ;

	count = 0 ;
	ptr = the_word ;
	while ( count < 6 )
	{
		ch = *ptr++ ;
		if ( ch == 0 )
		{
			/* Finished, so fill with blanks */

			while ( count < 6 )
				data[count++] = 5 ;
		}
		else
		{
			/* Get Character Print-Mode */

			mode = find_mode ( (char)ch ) ;
			if ( mode != 0 )
				data[count++] = mode + 3 ;

			/* Get offset of character in Table[] */

			if ( count < 6 )
			{
				offset = convert ( (char)ch ) ;
				if ( offset == 0 )
				{
					/* Character not in Table[], so use ASCII */

					data[count++] = 6 ;
					if ( count < 6 )
						data[count++] = ch >> 5 ;
					if ( count < 6 )
						data[count++] = ch & 0x1F ;
				}
				else
					data[count++] = offset ;
			}
		}
	}

	/* Encrypt */

	coded[0] = ( data[0] << 10 ) | ( data[1] << 5 ) | data[2] ;
	coded[1] = ( data[3] << 10 ) | ( data[4] << 5 ) | data[5] ;
	coded[1] |= 0x8000 ;
}

word
find_mode ( ch )
char	ch ;
{
	if ( ch == 0 )
		return ( 3 ) ;
	if (( ch >= 'a' ) && ( ch <= 'z' ))
		return ( 0 ) ;
	if (( ch >= 'A' ) && ( ch <= 'Z' ))
		return ( 1 ) ;
	return ( 2 ) ;
}

word
convert ( ch )
char	ch ;
{
	extern char		table[] ;

	char			*ptr ;
	word			code ;

	ptr = table ;
	while (( *ptr != ch ) && ( *ptr != 0 ))
		++ptr ;
	if ( *ptr == 0 )
		return ( 0 ) ;
	code = ( ptr - table ) + 6 ;
	while ( code >= 0x20 )
		code -= 0x1A ;
	return ( code ) ;
}
