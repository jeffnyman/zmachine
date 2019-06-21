/* File: interp.c */
/* (C)opyright 1987 InfoTaskforce. */

#include	"infocom.h"

/* functions that execute opcodes */
int	ret_true();
int	ret_false();
int	wrt();
int	writeln();
int	null();
int	save();
int	restore();
int	restart();
int	rts();
int	pop_stack();
int	quit();
int	new_line();
int	show_score();
int	verify();

int	cp_zero();
int	get_link();
int	get_holds();
int	get_loc();
int	get_p_len();
int	inc_var();
int	dec_var();
int	print1();
int	error();
int	remove_obj();
int	p_obj();
int	rtn();
int	jump();
int	print2();
int	get_var();
int	not();

int	compare();
int	LTE();
int	GTE();
int	dec_chk();
int	inc_chk();
int	check_loc();
int	bit();
int	or();
int	and();
int	test_attr();
int	set_attr();
int	clr_attr();
int	put_var();
int	transfer();
int	load_word_array();
int	load_byte_array();
int	getprop();
int	get_prop_addr();
int	get_next_prop();
int	plus();
int	minus();
int	times();
int	divide();
int	mod();

int	gosub();
int	save_word_array();
int	save_byte_array();
int	put_prop();
int	input();
int	print_char();
int	print_num();
int	random();
int	push();
int	pop();

interp ()
{
	extern word		status ;
	extern word		save_blocks ;
	extern word		pc_page ;
	extern word		pc_offset ;
	extern word		*stack_base ;
	extern word		*stack_var_ptr ;
	extern word		*stack ;
	extern byte		*base_ptr ;
	extern byte		*p_buff_strt ;
	extern byte		*p_buff_ptr ;
	extern header	data_head ;

	word			opcode ;
#ifdef DEBUG
	long			pc ;
#endif
	static int		(*jmp_op0[])() =
	{
		ret_true,
		ret_false,
		wrt,
		writeln,
		null,
		save,
		restore,
		restart,
		rts,
		pop_stack,
		quit,
		new_line,
		show_score,
		verify
	};


	while ( status != quit_game )
	{
		if (( status == restart_game ) || ( status == init_game ))
		{
			if ( status == restart_game )
				load_page ( 0,save_blocks,base_ptr ) ;
			stack_var_ptr = stack_base ;
			stack = --stack_var_ptr ;
			p_buff_ptr = p_buff_strt ;
			pc_page = data_head.start / block_size ;
			pc_offset = data_head.start % block_size ;
			fix_pc () ;
			status = play_game ;
		}

#ifdef DEBUG
		pc = ( (long)pc_page * block_size ) + (long)pc_offset ;
#endif
		opcode = next_byte () ;
#ifdef DEBUG
		printf ( "PC: %lx - opcode: %x",pc,opcode ) ;
#endif
		if ( opcode < 0x80 )
			operand2 ( opcode ) ;
		else if ( opcode < 0xB0 )
			operand1 ( opcode ) ;
		else if ( opcode >= 0xC0 )
			operand3 ( opcode ) ;
		else if ((opcode & 0x0F) <= 0x0D)
			(*jmp_op0[opcode & 0x0F])();
		else
		{
#ifdef DEBUG
			out_char('\n') ;
#endif
			error ( err_opcode ) ;
		}
#ifdef DEBUG
		out_char('\n') ;
#endif
	}
}

operand1 ( opcode )
word	opcode ;
{
	word	param1 ;
	static int		(*jmp_op1[])() =
	{
		cp_zero,
		get_link,
		get_holds,
		get_loc,
		get_p_len,
		inc_var,
		dec_var,
		print1,
		error,
		remove_obj,
		p_obj,
		rtn,
		jump,
		print2,
		get_var,
		not
	};

	param1 = load ((int)(( opcode >> 4 ) & 0x03 )) ;
#ifdef DEBUG
	printf ( "\t1 Parameter: %x",param1 ) ;
#endif
	(*jmp_op1[opcode & 0x0f])(param1);
}

operand2 ( opcode )
word	opcode ;
{
	word	param1 ;
	word	param2 ;
	word	num_params ;
	int		mode ;

	mode = 1 ;
	if ( opcode & 0x40 )
		++mode ;
	param1 = load ( mode ) ;
	mode = 1 ;
	if ( opcode & 0x20 )
		++mode ;
	param2 = load ( mode ) ;
	num_params = 2 ;
#ifdef DEBUG
	printf( "\t2 Parameters: %x %x",param1,param2 ) ;
#endif
	execute ( (opcode & 0x1F),num_params,param1,param2,0,0 ) ;
}

operand3 ( opcode )
word	opcode ;
{
	word	param1		= 0 ;
	word	param2		= 0 ;
	word	param3		= 0 ;
	word	param4		= 0 ;
	word	num_params ;
	byte	modes ;
	int		addr_mode ;

	modes = next_byte () ;
	num_params = 0 ;
	addr_mode = ( modes >> 6 ) & 0x03 ;
	if ( addr_mode != 3 )
	{
		++num_params ;
		param1 = load ( addr_mode ) ;
		addr_mode = ( modes >> 4 ) & 0x03 ;
		if ( addr_mode != 3 )
		{
			++num_params ;
			param2 = load ( addr_mode ) ;
			addr_mode = ( modes >> 2 ) & 0x03 ;
			if ( addr_mode != 3 )
			{
				++num_params ;
				param3 = load ( addr_mode ) ;
				addr_mode = modes & 0x03 ;
				if ( addr_mode != 3 )
				{
					++num_params ;
					param4 = load ( addr_mode ) ;
				}
			}
		}
	}
#ifdef DEBUG
	printf( "\t%x Parameters: %x %x %x %x",num_params,param1,param2,param3,param4 ) ;
#endif
	execute ( (opcode & 0x3F),num_params,param1,param2,param3,param4 ) ;
}

execute ( opcode,num_params,param1,param2,param3,param4 )
register word	opcode;
word	num_params,param1,param2,param3,param4 ;
{
	static int		(*jmp_lowop[])() =
	{
		compare,
		LTE,
		GTE,
		dec_chk,
		inc_chk,
		check_loc,
		bit,
		or,
		and,
		test_attr,
		set_attr,
		clr_attr,
		put_var,
		transfer,
		load_word_array,
		load_byte_array,
		getprop,
		get_prop_addr,
		get_next_prop,
		plus,
		minus,
		times,
		divide,
		mod,
	};
	static int		(*jmp_hiop[])() =
	{
		gosub,
		save_word_array,
		save_byte_array,
		put_prop,
		input,
		print_char,
		print_num,
		random,
		push,
		pop
	};
	register int		(*func)();

	if (--opcode <= 0x17)
		func = jmp_lowop[opcode];
	else if ((opcode -= 0x20-1) <= 0x9)
		func = jmp_hiop[opcode];
	else
		error ( err_opcode ) ;
	
	if (func == compare || func == gosub)
		(*func) ( param1,param2,param3,param4,num_params ) ;
	else
	{
		switch(num_params)
		{
		case 0:
			(*func)();
			break;
		case 1:
			(*func)(param1);
			break;
		case 2:
			(*func)(param1, param2);
			break;
		case 3:
			(*func)(param1, param2, param3);
			break;
		case 4:
			(*func)(param1, param2, param3, param4);
			break;
		default:
			error ( err_opcode ) ;
		}
	}
}
