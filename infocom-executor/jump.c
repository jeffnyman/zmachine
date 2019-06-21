/* File: jump.c */
/* (C)opyright 1987 InfoTaskforce. */

#include	"infocom.h"

gosub ( address,var1,var2,var3,num_params )
word	address,var1,var2,var3,num_params ;
{
	extern word		pc_offset ;
	extern word		pc_page ;
	extern word		*stack_base ;
	extern word		*stack_var_ptr ;
	extern word		*stack ;

	byte			vars ;
	word			parameter ;

	if ( address == 0 )
		store ( address ) ;
	else
	{
		*(--stack) = pc_page ;
		*(--stack) = pc_offset ;

		/* Push offset of old stack_var_ptr from stack_base onto stack */

		*(--stack) = stack_base - stack_var_ptr ;

		pc_page = address >> 8 ;
		pc_offset = ( address & 0xFF ) << 1 ;
		fix_pc () ;

		/*
			The value of the current stack pointer is the
			new value of stack_var_ptr.
		*/

		stack_var_ptr = stack ;
		--stack_var_ptr ;

		/*
			Global variables 1 to 15 are Local variables, which
			reside on the stack (and so are local to each procedure).

			Get number of local variables to push onto the stack.
			var1, var2 and var3, if defined, are the first 3 local
			variables. There are also bytes reserved after the
			gosub opcode in the calling procedure to initialise
			all local variables - including the first 3 local
			variables ( and so are ignored ).

			The use of parameters also allows procedures to be
			passed variables by value.
		*/

		vars = next_byte () ;
		if ( vars == 0 )
			return ;

		parameter = next_word () ;
		if ( (--num_params) != 0 )
		{
			*(--stack) = var1 ;
			if ( (--vars) == 0 )
				return ;

			parameter = next_word () ;
			if ( (--num_params) != 0 )
			{
				*(--stack) = var2 ;
				if ( (--vars) == 0 )
					return ;

				parameter = next_word () ;
				if ( (--num_params) != 0 )
				{
					*(--stack) = var3 ;
					if ( (--vars) == 0 )
						return ;

					parameter = next_word () ;
				}
			}
		}
		*(--stack) = parameter ;
		while ( (--vars) != 0 )
			*(--stack) = next_word () ;
	}
}

rtn ( value )
word	value ;
{
	extern word		pc_offset ;
	extern word		pc_page ;
	extern word		*stack_base ;
	extern word		*stack_var_ptr ;
	extern word		*stack ;

	stack = stack_var_ptr ;
	++stack ;
	stack_var_ptr = stack_base - *stack++ ;
	pc_offset = *stack++ ;
	pc_page = *stack++ ;
	fix_pc () ;
	store ( value ) ;
}

ret_true ()
{
	rtn ( true ) ;
}

ret_false ()
{
	rtn ( false ) ;
}

jump ( offset )
word	offset ;
{
	extern word		pc_offset ;

	pc_offset += offset - 2 ;
	fix_pc () ;
}

rts ()
{
	extern word		*stack ;

	rtn ( *stack++ ) ;
}

pop_stack ()
{
	extern word		*stack ;

	++stack ;
}
