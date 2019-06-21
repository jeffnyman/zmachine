/* File: io.c */
/* (C)opyright 1987 InfoTaskforce. */

#include	<stdio.h>
#include	"infocom.h"

extern int	mx_scrx;
extern int	mx_scry;
extern int	prt_buff_size;

init_io()
{
#ifdef	TERMCAP

	init_tcap();
	prt_buff_size = mx_scrx-1;
	cls();
	gotoxy(0, mx_scry-1);
#endif
}

print_buffer ( buff_end )
byte	*buff_end ;
{
	extern byte		*p_buff_strt ;

	byte			*ptr ;

	ptr = p_buff_strt ;
	while ( ptr != buff_end )
		out_char ( (char)*ptr++ ) ;
}

cr_print_buffer ( buff_end )
byte	*buff_end ;
{
	print_buffer ( buff_end ) ;
	out_char ( '\n' ) ;
}

print_status ()
{
	extern byte		*s_buff_strt ;

	byte			*ptr ;
	byte			i ;

	/* First byte of buffer is the buffer size */

#ifdef	TERMCAP
	gotoxy(0,0);
	standout();
#endif
	ptr = s_buff_strt ;
	for ( i = *ptr++ ; i ; i-- )
		out_char ( (char)*ptr++ ) ;
#ifdef	TERMCAP
	for (i = mx_scrx - *s_buff_strt; i--; )
		out_char(' ');
	standend();
	gotoxy(0, mx_scry);
#else
	out_char ( '\n' ) ;
#endif
}

echo ( s )
char	*s ;
{
	printf ( "%s",s ) ;
}

char
read_char ()
{
	extern word	status;
	extern Boolean	echo_in;

	char		ch;
	int		in;

	in = getchar ();
	if ( in == -1 )
	{
		/* If input is from file, handle EOF */
		ch = '\n' ;
		status = quit_game ;
		close_file() ;
		deallocate () ;
	}
	else
		ch = (char)in;
	if ( echo_in )
		putchar ( ch ) ;
	return ( ch ) ;
}

out_char(c)
char	c;
{
	extern int	linecount;
	extern Boolean	page_out;

	putchar(c);
	if (c == '\n')
	{
		if (linecount++ >= mx_scry-4)
		{
			if (!page_out)
			{
				echo("**MORE**");
				read_char();
			}
			linecount = 0;
		}
	}
}

allocate()
{
	/*
	 *	This routine tries to allocate a contiguous
	 *	block of memory that is at least 'max_mem'
	 *	bytes long. If one is not available, 400H is
	 *	continually subtracted from this amount, and
	 *	it tries again. Having found the size of the
	 *	largest contiguous block avaiable, it subtracts
	 *	6400H from this value. If the result is less
	 *	than 6000H, there is not enough memory
	 *	available, and the routine exits with a memory
	 *	error. Otherwise, it returns a pointer to the
	 *	allocated area of memory.
	 *
	 *	The strange behaviour of this routine is due to
	 *	the way the Apple Macintosh behaves ( since
	 *	there must always be enough memory available to
	 *	run things like Desk Accessories, load Fonts,
	 *	etc. ). It can be greatly simplified for other
	 *	systems ( just delete :
	 *		free ( (char *)base_ptr ) ;
	 *		size -= 0x6400 ;
	 *	and also the else part of the if :
	 *		else
	 *		{
	 *			base_ptr = (byte *)malloc ( size ) ;
	 *			base_end = base_ptr + size;
	 *		}
	 *	and that should do it).
	 */

	extern byte		*base_ptr ;
	extern byte		*base_end ;
	extern word		status ;

	unsigned		size ;

	size = max_mem ;
	while ((size != 0)&&((base_ptr =(byte *)malloc (size)) ==(byte *)0))
		size -= 0x0400 ;
#ifdef ALOCALL
#else
	free ( (char *)base_ptr ) ;
	size -= 0x6400 ;
#endif
	if ( size < 0x6000 )
	{
		error ( err_memory ) ;
		echo ( "Not enough memory." ) ;
		quit () ;
	}
#ifdef ALOCALL
#else
	else
	{
		base_ptr = (byte *)malloc ( size ) ;
		base_end = base_ptr + size ;
	}
#endif
}

deallocate ()
{
	extern byte		*base_ptr ;

	if ( base_ptr != (byte *)0 )
		free ( (char *)base_ptr ) ;
}

seed_random ()
{
	/*
	 *	This routine seeds the random number generator.
	 *	It currently seeds it with a fixed value -
	 *	however, if your system supports time() then
	 *	use this instead:
	 *	seed_random ()
	 *	{
	 *		extern word	random1 ;
	 *		extern word	random2 ;
	 *		extern long	time() ;
	 *
	 *		random1 = random2 = (word)time((long *)0) ;
	 *	}
	 *	If your system doesn't support time, but
	 *	does support some other routine that returns
	 *	an unpredictable number, then use that instead.
	 *	NOTE - support for the above technique has now
	 *	been added, just define TIMESEED - and seeding
	 *	using time() will be used.
	 */
	extern word		random1 ;
	extern word		random2 ;
#ifdef TIMESEED
	extern long		time() ;

	random1 = random2 = (word)time((long *)0) ;
#else

	random1 = random2 = 0xFFFF ;
#endif
}
