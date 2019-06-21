/* File: options.c */
/* (C)opyright 1987 InfoTaskforce. */

#include	"infocom.h"

options ( head,objs,vocab,tree )
Boolean		head,objs,vocab,tree ;
{
	if ( head )
		show_header () ;
	if ( vocab )
		show_vocab () ;
	if ( objs )
		show_objects () ;
	if ( tree )
		show_tree () ;
}

show_header ()
{
	extern header	data_head ;

	word			i ;

	printf ( "INFOCOM Data File Header." ) ;
	out_char('\n') ;
	out_char('\n') ;
	printf ( "Z-Code Version: $%x",(int)data_head.z_code_version )
;
	out_char('\n') ;
	if ( (data_head.score_or_time & 0x02) == 0 )
		printf ( "Score/Time    : Score" ) ;
	else
		printf ( "Score/Time    : Time" ) ;
	out_char('\n') ;
	printf ( "Release Number: $%x",data_head.release ) ;
	out_char('\n') ;
	printf ( "Resident Bytes: $%x",data_head.resident_bytes ) ;
	out_char('\n') ;
	printf ( "Start Address : $%x",data_head.start ) ;
	out_char('\n') ;
	printf ( "Vocab Address : $%x",data_head.vocab ) ;
	out_char('\n') ;
	printf ( "Object List   : $%x",data_head.object_list ) ;
	out_char('\n') ;
	printf ( "Global Vars.  : $%x",data_head.globals ) ;
	out_char('\n') ;
	printf ( "Save Bytes    : $%x",data_head.save_bytes ) ;
	out_char('\n') ;
	if ( data_head.script_status == 0 )
		printf ( "Script Status : Off" ) ;
	else
		printf ( "Script Status : On" ) ;
	out_char('\n') ;
	printf ( "Serial Number : " ) ;
	for ( i = 0 ; i < 6 ; i++ )
		printf ( "%c",data_head.serial_no[i] ) ;
	out_char ( '\n' ) ;
	printf ( "Common Words  : $%x",data_head.common_word ) ;
	out_char( '\n' ) ;
	printf ( "Verify Length : $%x",data_head.verify_length ) ;
	out_char('\n') ;
	printf ( "Verify Check  : $%x",data_head.verify_checksum ) ;
	out_char('\n') ;
	out_char('\n') ;
	out_char('\n') ;
}

show_vocab ()
{
	extern word		num_vocab_words ;
	extern word		vocab_entry_size ;
	extern byte		*strt_vocab_table ;
	extern byte		*base_ptr ;
	extern byte		*p_buff_strt ;
	extern byte		*p_buff_ptr ;
	extern ProcPtr	PrintChar ;
	extern int		mx_scrx ;

	byte			*ptr ;
	word			page ;
	word			offset ;
	word			count ;
	int			words_per_line ;

	printf ( "INFOCOM Adventure - Vocab List." ) ;
	out_char('\n') ;
	out_char('\n') ;
	printf ( "Number of Words: $%x",num_vocab_words ) ;
	out_char('\n') ;
	out_char('\n') ;
	p_buff_ptr = p_buff_strt ;
	words_per_line = mx_scrx / 8 ;
	count = 0 ;
	ptr = strt_vocab_table ;
	while ( count < num_vocab_words )
	{
		page = ( ptr - base_ptr ) / block_size ;
		offset = ( ptr - base_ptr ) % block_size ;
		print_coded ( &page,&offset ) ;
		ptr += vocab_entry_size ;
		++count ;
		if ( (count % words_per_line) == 0 )
			new_line () ;
		else (*PrintChar)((word)'\t');
	}
	new_line () ;
	new_line () ;
}

show_objects ()
{
	extern object	*obj_list ;
	extern byte		*p_buff_strt ;
	extern byte		*p_buff_ptr ;
	extern byte		*base_ptr ;

	object			*obj ;
	word			i,j ;
	int			n_objs ;

	printf ( "INFOCOM Adventure - Object List." ) ;
	out_char('\n') ;
	out_char('\n') ;
	p_buff_ptr = p_buff_strt ;
	obj = obj_addr(1);
	n_objs = ((obj->prop_ptr[0]<<8) + obj->prop_ptr[1] - ((byte *)obj - base_ptr)) / 9 ;
	printf( "Number of Objects: $%x",n_objs);
	out_char('\n') ;
	out_char('\n') ;
	for ( i = 1 ; i <= n_objs ; i++ )
	{
		obj = obj_addr ( i ) ;
		printf ( "Object $%x : ",i ) ;
		p_obj ( i ) ;
		new_line () ;
		printf ( "\t -> attributes : " ) ;
		for ( j = 0 ; j < 4 ; j++ )
			bit_byte ( obj->attributes[j] ) ;
		out_char('\n') ;
		printf ( "\t -> location   : $%x",obj->location ) ;
		out_char('\n')  ;
		printf ( "\t -> link       : $%x",obj->link ) ;
		out_char('\n') ;
		printf ( "\t -> holds      : $%x",obj->holds ) ;
		out_char('\n') ;
		out_char('\n');
	}
}

bit_byte ( a )
byte	a ;
{
	byte	b ;

	for ( b = 0x80 ; b ; b>>=1 )
		out_char( a & b ? '1' : '0' ) ;
	out_char(' ') ;
}

show_tree ()
{
	extern object	*obj_list ;
	extern byte		*p_buff_strt ;
	extern byte		*p_buff_ptr ;
	extern byte		*base_ptr ;

	object			*obj ;
	word			i ;
	int			n_objs ;

	printf ( "INFOCOM Adventure - Object Tree." ) ;
	out_char('\n') ;
	out_char('\n') ;
	p_buff_ptr = p_buff_strt ;
	obj = obj_addr(1);
	n_objs = ((obj->prop_ptr[0]<<8) + obj->prop_ptr[1] - ((byte *)obj - base_ptr)) / 9 ;
	printf( "Number of Objects: $%x",n_objs);
	out_char('\n') ;
	out_char('\n') ;
	for ( i = 1 ; i <= n_objs ; i++ )
	{
		obj = obj_addr ( i ) ;
		if ( !obj->location )
		{
			obtree ( i,0 ) ;
			out_char('\n') ;
		}
	}
}

obtree ( a , b )
word	a ;
int	b ;
{
	int	c ;
	object	*obj ;

	obj = obj_addr( a ) ;
	for ( c = b ; c ; c-- )
		out_char('\t') ;
	printf ( "$%x : ",a ) ;
	p_obj( a ) ;
	new_line () ;
	if ( obj->holds )
		obtree ( (word)obj->holds , b + 1 ) ;
	if ( obj->link )
		obtree ( (word)obj->link , b ) ;
}
