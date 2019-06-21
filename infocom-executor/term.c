/* File: term.c */
/* (C)opyright 1987 InfoTaskforce. */

/* only compiled in if TERMCAP is defined */

#include 	<stdio.h>
#include 	"infocom.h"

#ifdef	TERMCAP

char		*CM;
char		*SO;
char		*SE;
char		*CL;
char		cmdbuf[1024];
char		*cmd_p = cmdbuf;
char		termbuf[1024];

int		out_char();
char		*getenv();
char		*tgetstr();
char		*tgoto();

init_tcap()
{
	extern int	mx_scrx;
	extern int	mx_scry;
	char	*term;

	cmd_p = cmdbuf;
	if (((term = getenv("TERM")) == NULL) || tgetent(termbuf, term) <= 0)
	{
		CM = CL = SE = SO = "";
		return;
	}

	if ((CM = tgetstr("cm", &cmd_p)) == NULL)
		CM = "";
	if ((CL = tgetstr("cl", &cmd_p)) == NULL)
		CL = "";
	if 
	(
		((SO = tgetstr("so", &cmd_p)) == NULL)
		||
		((SE = tgetstr("se", &cmd_p)) == NULL)
	)
		SE = SO = "";

	if ((mx_scrx = tgetnum("co")) == -1)
		mx_scrx = dmx_scrx;
	if ((mx_scry = tgetnum("li")) == -1)
		mx_scry = dmx_scry;
}

gotoxy(x, y)
int	x, y;
{
	tputs(tgoto(CM, x, y), 0, out_char);
}

cls()
{
	tputs(CL, 0, out_char);
}

standout()
{
	tputs(SO, 0, out_char);
}

standend()
{
	tputs(SE, 0, out_char);
}
#endif
