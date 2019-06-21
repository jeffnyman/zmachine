/* File: page.c */
/* (C)opyright 1987 InfoTaskforce. */

#include	"infocom.h"

/*
	Many of these variables must retain their values
	between invocations of the paging functions.
*/

word		tick_lo			= 0 ;
byte		tick_hi			= 0 ;
word		prog_page		= 0 ;
word		MRU_page		= 0 ;
byte		*MRU_block_ptr	= (byte *)0 ;
pg_table	*MRU			= (pg_table *)0 ;
pg_table	*page_entry		= (pg_table *)0 ;

byte
*fetch_page ( new_page )
word	new_page ;
{
	extern byte			*page_strt ;
	extern pg_table		*strt_page_table ;

	pg_table			*ptr ;
	Boolean				found ;

	if ( MRU_page != new_page )
	{
		found = false ;
		MRU_page = new_page ;
		++tick_lo ;
		if ( tick_lo == 0 )
			++tick_hi ;
		ptr = strt_page_table ;

		/* 0xFF marks end of page table */

		while (( ptr -> page != 0xFF ) && ( found == false ))
		{
			if ( ptr -> page == new_page )
			{
				if ( prog_page != new_page )
				{
					ptr -> count_lo = tick_lo ;
					ptr -> count_hi = tick_hi ;
				}
				MRU = ptr ;
				MRU_block_ptr = page_strt +
					((long_word)(MRU - strt_page_table) * block_size) ;
				found = true ;
			}
			++ptr ;
		}
		if ( found == false )
		{
			MRU = get_LRU_page () ;
			MRU -> page = MRU_page ;
			MRU -> count_hi = tick_hi ;
			MRU -> count_lo = tick_lo ;
			MRU_block_ptr = page_strt +
					((long_word)(MRU - strt_page_table) * block_size) ;
			load_page ( MRU_page,1,MRU_block_ptr ) ;
		}
	}
	return ( MRU_block_ptr ) ;
}

pg_table
*get_LRU_page ()
{
	extern pg_table		*strt_page_table ;

	byte				test_hi ;
	word				test_lo ;
	pg_table			*LRU ;
	pg_table			*ptr ;

	ptr = strt_page_table ;
	test_lo = 0xFFFF ;
	test_hi = 0xFF ;
	while ( ptr -> page != 0xFF )
	{
		if (( test_hi > ptr -> count_hi ) ||
			(( test_hi == ptr -> count_hi ) &&
			( test_lo >= ptr -> count_lo )))
		{
			test_hi = ptr -> count_hi ;
			test_lo = ptr -> count_lo ;
			LRU = ptr ;
		}
		++ptr ;
	}
	return ( LRU ) ;
}

fix_pc ()
{
	extern word		pc_page ;
	extern word		pc_offset ;
	extern word		resident_blocks ;
	extern byte		*prog_block_ptr ;
	extern byte		*base_ptr ;

	/*
		This must be 32 bits long since 'pc_page'
		can have any value from $0000 to $FFFF.
	*/

	long_word		pc ;

	/* The high bit of 'pc_offset' is actually a sign bit. */

	pc = ( (long_word)pc_page * block_size ) + (signed_word)pc_offset ;
	pc_offset = pc % block_size ;
	pc_page = pc / block_size ;
	if ( prog_page != pc_page )
	{
		prog_page = pc_page ;
		if ( page_entry != (pg_table *)0 )
		{
			page_entry -> count_hi = tick_hi ;
			page_entry -> count_lo = tick_lo ;
		}
		if ( pc_page < resident_blocks )
		{
			prog_block_ptr = base_ptr +((long_word)pc_page * block_size);
			page_entry = (pg_table *)0 ;
		}
		else
		{
			prog_block_ptr = fetch_page ( pc_page ) ;
			page_entry = MRU ;
			page_entry -> count_hi = 0xFF ;
		}
	}
}
