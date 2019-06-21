/* File: infocom.c */
/* (C)opyright 1987 InfoTaskforce. */

#include	"infocom.h"

word		random1 ;
word		random2 ;
word		pc_offset ;
word		pc_page ;
word		resident_blocks ;
word		save_blocks ;
word		status ;

header		data_head ;
object		*obj_list ;
byte		*base_ptr ;
byte		*base_end ;
byte		*vocab ;
byte		*global_ptr ;
byte		*end_resident_area ;
word		*stack_base ;
word		*stack_var_ptr ;
word		*stack ;

pg_table	*strt_page_table ;
byte		*page_strt ;
byte		*prog_block_ptr ;

/* Score Routine Variables */

word		score_time ;
byte		*s_buff_strt ;
byte		*s_buff_ptr ;

/* Input Routine Variables */

byte		*ws_buff_strt ;
byte		*end_of_sentence ;
word		num_vocab_words ;
word		vocab_entry_size ;
byte		*strt_vocab_table ;
byte		*end_vocab_table ;

/* Print Routine Variables */

ProcPtr		PrintChar ;
byte		*common_word_ptr ;
byte		*p_buff_strt ;
byte		*p_buff_ptr ;
byte		*p_buff_end ;
int		linecount;
int		mx_scrx;
int		mx_scry;
int		prt_buff_size;

/* Command Line Options */

Boolean		attribute	= false ;		/* Monitor Object Attributes  */
Boolean		echo_in		= false ;		/* Echo Input Characters      */
Boolean		head_info	= false ;		/* Print Header Information   */
Boolean		objects		= false ;		/* Print Object / Room List   */
Boolean		page_out	= false ;		/* Disable pager */
Boolean		tree		= false ;		/* Print Object / Room Tree */
Boolean		vocabulary	= false ;		/* Print Vocabulary Word List */
Boolean		xfer		= false ;		/* Monitor Object Transfers   */

Boolean		play		= true ;		/* Play the Game              */

/* Character Tables */

char	ws_table[] =	{ ' ','\t','\r','.',',','?','\0','\0' } ;

char	table[] =	{	'a','b','c','d','e','f','g','h','i','j','k','l',
						'm','n','o','p','q','r','s','t','u','v','w','x',
						'y','z','A','B','C','D','E','F','G','H','I','J',
						'K','L','M','N','O','P','Q','R','S','T','U','V',
						'W','X','Y','Z',' ',' ','0','1','2','3','4','5',
						'6','7','8','9','.',',','!','?','_','#','\'',
						'\"','/','\\','-',':','(',')','\0','\0'
					} ;

main ( argc,argv )
int		argc ;
char	*argv[] ;
{
	int			i ;

	--argc ;
	if (( argc == 1 ) || ( argc == 2 ))
	{
		if ( argc == 2 )
		{
			if ( argv[1][0] == '-' )
			{
				i = 0 ;
				while ( argv[1][++i] != '\0' )
				{
					switch ( argv[1][i] )
					{
						case 'a' :	attribute = true ;
									break ;
						case 'e' :	echo_in = true ;
									break ;
						case 'h' :	head_info = true ;
									play = false ;
									break ;
						case 'o' :	objects = true ;
									play = false ;
									break ;
						case 'p' :	page_out = true ;
									break ;
						case 'r' :	tree = true ;
									play = false ;
									break ;
						case 't' :	xfer = true ;
									break ;
						case 'v' :	vocabulary = true ;
									play = false ;
									break ;
						default  :	printf ( "Unrecognised Option: %c\n",argv[1][i] ) ;
					}
				}
			}
			else
				usage ( argv[0] ) ;
		}
		if ( open_file ( argv[argc] ) )
		{
			init () ;
			if ( play )
				interp () ;
			else
				options ( head_info,objects,vocabulary,tree ) ;
		}
		else
			printf ( "Failed to Open File %s\n",argv[argc] ) ;
	}
	else
		usage ( argv[0] ) ;
}

usage ( name )
char	*name ;
{
	printf ( "Usage: %s [-aehoprtv] <filename>\n",name ) ;
	exit ( 0 ) ;
}
