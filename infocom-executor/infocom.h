/* File: infocom.h */

/*
 *	STANDARD SERIES INFOCOM INTERPRETER.
 *	
 *	(C)opyright InfoTaskforce.
 *	23rd June, 1987.
 *
 *	"May the Grues be with you !"
 *
 */

/*
	Universal Type Definitions.

	'byte'			- 8 bits	; unsigned.
	'word'			- 16 bits	; unsigned.
	'signed_word'		- 16 bits	; signed.
	'long_word'		- 32 bits	; signed.
*/

#define		byte		unsigned char
#define		word		unsigned short
#define		signed_word	short
#define		long_word	long

/* Universal Constants */

#define		false				0
#define		true				1
#define		back_space			0x08
#define		local_vars			0x10
#define		dmx_scrx		80
#define		dmx_scry		24
#define		status_buff_size	0x3A
#define		stack_size			0x0200
#define		block_size			0x0200
#define		max_mem				0xFc00

/* Error Codes */

#define		err_memory		0x00
#define		err_opcode		0x15
#define		err_header		0x16
#define		err_put_prop	0x17
#define		err_next_prop	0x18

/* Status Codes */

#define		init_game		0x00
#define		play_game		0x01
#define		restart_game	0x02
#define		load_game		0x03
#define		quit_game		0x04

/* Type Definitions */

typedef		char	Boolean ;
typedef		int		(*ProcPtr)() ;
typedef		byte	*property ;

/*
	Object Structure Definition.

	Problem:
			We want the size of an object to be 9 bytes, but some
	compliers will only make it an integral number of WORDS. Thus
	we have to explicitly define the size of an object structure.
*/

#define		obj_size	0x09
#define		obj_offset	0x35

struct	object
{
	byte	attributes[4] ;
	byte	location ;
	byte	link ;
	byte	holds ;
	byte	prop_ptr[2] ;
} ;
typedef		struct object	object ;

struct	pg_table
{
	byte	page ;
	byte	count_hi ;
	word	count_lo ;
} ;
typedef		struct pg_table		pg_table ;

/* Infocom Game Header Structure */

struct	header
{
	byte	z_code_version ;	/* Game's Z-CODE Version Number    */
	byte	score_or_time ;		/* Status Bar display indicator    */
	word	release ;			/* Game Release Number             */
	word	resident_bytes ;	/* No. bytes in the Resident Atea  */
	word	start ;				/* Offset to Start of Game         */
	word	vocab ;				/* Offset to VocabtList            */
	word	object_list ;		/* Offset to Object/Room List      */
	word	globals ;			/* Offset to Global Variables      */
	word	save_bytes ;		/* No. bytes in the Save Game Area */
	word	script_status ;		/* Z-CODE printing modes           */
	char	serial_no[6] ;		/* Game's Serial Number            */
	word	common_word ;		/* Offset to Common Word List      */
	word	verify_length ;		/* No. words in the Game File      */
	word	verify_checksum ;	/* Game Checksum - used by Verify  */
	word	padding[17] ;		/* Blank                           */
} ;
typedef		struct header	header ;

/*
	Header Information.

	The 'z_code_version' byte has the following meaning:
		$00 : Game compiled for an early version of the interpreter
		$01 : Game compiled for an early version of the interpreter
		$02 : Game compiled for an early version of the interpreter
		$03 : Game compiled for the current 'Standard Series Interpreter'
		$04 : Game compiled for the current 'Plus Series Interpreter'

	The 'score_or_time' byte performs the following functions:
		Bit 0 :		Clear	- Game Header OK.
					Set		- Game Header Error.
		Bit 1 :		Clear	- Status Bar displays the SCORE.
					Set		- Status Bar displays the TIME.

	The 'script_status' word is used by Z-CODE to set printing modes
	for use by the interpreter:
		Bit 0 :		Clear	- Script mode off.
					Set		- Script mode on.
		Bit 1 :		Clear	- Use any type of Font.
					Set		- Use a Non-Proportional Font only.
*/

/* Function Definitions */

object		*obj_addr() ;
word		find_mode() ;
word		convert() ;
byte		get_byte() ;
word		get_word() ;
byte		next_byte() ;
word		next_word() ;
word		load_var() ;
word		load() ;
word		make_word() ;
byte		*read_line() ;
byte		*fetch_page() ;
pg_table	*get_LRU_page() ;

char		read_char() ;
int			print_char() ;
int			put_status() ;
int			open_file() ;

/* Standard C Function Definition */

char		*malloc() ;
