/* File: init.c */
/* (C)opyright 1987 InfoTaskforce. */

#include	"infocom.h"

init ()
{
	extern word		resident_blocks ;
	extern word		save_blocks ;
	extern word		status ;
	extern word		score_time ;
	extern header	data_head ;
	extern object	*obj_list ;
	extern byte		*base_ptr ;
	extern byte		*base_end ;
	extern byte		*vocab ;
	extern byte		*global_ptr ;
	extern byte		*common_word_ptr ;
	extern byte		*end_resident_area ;
	extern word		*stack_base ;

	extern byte		*p_buff_strt ;
	extern byte		*p_buff_end ;
	extern byte		*s_buff_strt ;
	extern int		prt_buff_size;
	extern int		mx_scrx;
	extern int		mx_scry;
	extern int		linecount ;
	extern byte		*ws_buff_strt ;
	extern char		ws_table[] ;
	extern byte		*end_of_sentence ;
	extern word		vocab_entry_size ;
	extern word		num_vocab_words ;
	extern byte		*strt_vocab_table ;
	extern byte		*end_vocab_table ;
	extern byte		*page_strt ;
	extern pg_table	*strt_page_table ;
	extern ProcPtr	PrintChar ;

	word			num ;
	byte			*p,*q ;
	pg_table		*page_ptr ;

	status = init_game  ;
	read_header ( &data_head ) ;
	allocate () ;
	if (  status != quit_game )
	{
		mx_scrx = dmx_scrx;
		mx_scry = dmx_scry;
		init_io ();
		prt_buff_size = mx_scrx-1;
		score_time = ( data_head.score_or_time & 0x02 ) >> 1 ;
		resident_blocks = data_head.resident_bytes / block_size ;
		if ( data_head.resident_bytes % block_size )
			++resident_blocks ;
		load_page ( 0,resident_blocks,base_ptr ) ;
		vocab = base_ptr + data_head.vocab ;
		obj_list = (object *)( base_ptr + data_head.object_list ) ;
		global_ptr = base_ptr + data_head.globals ;
		common_word_ptr = base_ptr + data_head.common_word ;
		save_blocks = data_head.save_bytes / block_size ;
		if ( data_head.save_bytes % block_size )
			++ save_blocks ;
		end_resident_area = base_ptr + ( resident_blocks * block_size ) ;
		stack_base = (word *)( end_resident_area + stack_size ) ;
		p_buff_strt = (byte *)stack_base ;
		p_buff_end = p_buff_strt + prt_buff_size ;
		s_buff_strt = p_buff_end + 2 ;
		linecount = 0 ;

		ws_buff_strt = s_buff_strt + status_buff_size + 1 ;
		p = vocab ;
		num = *p++ ;
		q = ws_buff_strt ;
		while ( num-- )
			*q++ = *p++ ;
		end_of_sentence = q ;
		vocab_entry_size = *p++ ;

		num_vocab_words = *(p++) << 8 ;
		num_vocab_words += *p++ ;

		strt_vocab_table = p ;
		end_vocab_table = strt_vocab_table +
					vocab_entry_size * ( num_vocab_words - 1 ) ;
		p = (byte *)ws_table ;
		while ( *q++ = *p++ ) ;
		if ( (int)q & 0x01 )
			++q ;
		strt_page_table = (pg_table *)q ;
		page_strt = q + (((word)(base_end - q) / block_size) *
													sizeof(pg_table));
		page_ptr = strt_page_table ;
		while ( page_ptr != (pg_table *)page_strt )
		{
			page_ptr -> page = 0xFE ;
			page_ptr -> count_hi = 0 ;
			page_ptr -> count_lo = 0 ;
			++page_ptr ;
		}

		/* Mark end of Page Table */

		*page_strt++ = 0xFF ;
		*page_strt++ = 0x00 ;

		seed_random () ;
		PrintChar = print_char ;
	}
}
