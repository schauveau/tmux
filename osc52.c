
#include <sys/types.h>

#include <stdlib.h>
#include <string.h>
#include <time.h>

/* <resolv.h> for b64_ntop */
#include <resolv.h>

#include "tmux.h"

/*
 * Based on the description by Paul Williams at:
 *
 * http://vt100.net/emu/dec_ansi_parser
 *
 * With the following changes:
 *
 * - 7-bit only.
 *
 * - Support for UTF-8.
 *
 * - OSC (but not APC) may be terminated by \007 as well as ST.
 *
 * - A state for APC similar to OSC. Some terminals appear to use this to set
 *   the title.
 *
 * - A state for the screen \033k...\033\\ sequence to rename a window. This is
 *   pretty stupid but not supporting it is more trouble than it is worth.
 *
 * - Special handling for ESC inside a DCS to allow arbitrary byte sequences to
 *   be passed to the underlying terminals.
 */

/* Input parser cell. */
struct input_cell {
	struct grid_cell	cell;
	int			set;
	int			g0set;	/* 1 if ACS */
	int			g1set;	/* 1 if ACS */
};

/* Input parser context. */
struct input_ctx {
	struct window_pane     *wp;
	struct screen_write_ctx ctx;

	struct input_cell	cell;

	struct input_cell	old_cell;
	u_int 			old_cx;
	u_int			old_cy;

	u_char			interm_buf[4];
	size_t			interm_len;

	u_char			param_buf[64];
	size_t			param_len;

#define INPUT_BUF_START 32
#define INPUT_BUF_LIMIT 1048576
	u_char		       *input_buf;
	size_t			input_len;
	size_t			input_space;

	int			param_list[24];	/* -1 not present */
	u_int			param_list_len;

	struct utf8_data	utf8data;

	int			ch;
	int			flags;
#define INPUT_DISCARD 0x1

	const struct input_state *state;

	/*
	 * All input received since we were last in the ground state. Sent to
	 * control clients on connection.
	 */
	struct evbuffer	 	*since_ground;
};

/* Helper functions. */
struct input_transition;
int	input_split(struct input_ctx *);
int	input_get(struct input_ctx *, u_int, int, int);
void printflike(2, 3) input_reply(struct input_ctx *, const char *, ...);
void	input_set_state(struct window_pane *, const struct input_transition *);
void	input_reset_cell(struct input_ctx *);

/* Transition entry/exit handlers. */
void	input_clear(struct input_ctx *);
void	input_ground(struct input_ctx *);
void	input_enter_osc(struct input_ctx *);
void	input_exit_osc(struct input_ctx *);
void	input_enter_apc(struct input_ctx *);
void	input_exit_apc(struct input_ctx *);
void	input_enter_rename(struct input_ctx *);
void	input_exit_rename(struct input_ctx *);

/* Input state handlers. */
int	input_print(struct input_ctx *);
int	input_intermediate(struct input_ctx *);
int	input_parameter(struct input_ctx *);
int	input_input(struct input_ctx *);
int	input_c0_dispatch(struct input_ctx *);
int	input_esc_dispatch(struct input_ctx *);
int	input_csi_dispatch(struct input_ctx *);
void	input_csi_dispatch_rm(struct input_ctx *);
void	input_csi_dispatch_rm_private(struct input_ctx *);
void	input_csi_dispatch_sm(struct input_ctx *);
void	input_csi_dispatch_sm_private(struct input_ctx *);
void	input_csi_dispatch_winops(struct input_ctx *);
void	input_csi_dispatch_sgr_256(struct input_ctx *, int, u_int *);
void	input_csi_dispatch_sgr_rgb(struct input_ctx *, int, u_int *);
void	input_csi_dispatch_sgr(struct input_ctx *);
int	input_dcs_dispatch(struct input_ctx *);
int	input_utf8_open(struct input_ctx *);
int	input_utf8_add(struct input_ctx *);
int	input_utf8_close(struct input_ctx *);

/* Command table comparison function. */
int	input_table_compare(const void *, const void *);

/* Command table entry. */
struct input_table_entry {
	int		ch;
	const char     *interm;
	int		type;
};

/* Escape commands. */
enum input_esc_type {
	INPUT_ESC_DECALN,
	INPUT_ESC_DECKPAM,
	INPUT_ESC_DECKPNM,
	INPUT_ESC_DECRC,
	INPUT_ESC_DECSC,
	INPUT_ESC_HTS,
	INPUT_ESC_IND,
	INPUT_ESC_NEL,
	INPUT_ESC_RI,
	INPUT_ESC_RIS,
	INPUT_ESC_SCSG0_OFF,
	INPUT_ESC_SCSG0_ON,
	INPUT_ESC_SCSG1_OFF,
	INPUT_ESC_SCSG1_ON,
};

/* Escape command table. */
const struct input_table_entry input_esc_table[] = {
	{ '0', "(", INPUT_ESC_SCSG0_ON },
	{ '0', ")", INPUT_ESC_SCSG1_ON },
	{ '7', "",  INPUT_ESC_DECSC },
	{ '8', "",  INPUT_ESC_DECRC },
	{ '8', "#", INPUT_ESC_DECALN },
	{ '=', "",  INPUT_ESC_DECKPAM },
	{ '>', "",  INPUT_ESC_DECKPNM },
	{ 'B', "(", INPUT_ESC_SCSG0_OFF },
	{ 'B', ")", INPUT_ESC_SCSG1_OFF },
	{ 'D', "",  INPUT_ESC_IND },
	{ 'E', "",  INPUT_ESC_NEL },
	{ 'H', "",  INPUT_ESC_HTS },
	{ 'M', "",  INPUT_ESC_RI },
	{ 'c', "",  INPUT_ESC_RIS },
};

/* Control (CSI) commands. */
enum input_csi_type {
	INPUT_CSI_CBT,
	INPUT_CSI_CNL,
	INPUT_CSI_CPL,
	INPUT_CSI_CUB,
	INPUT_CSI_CUD,
	INPUT_CSI_CUF,
	INPUT_CSI_CUP,
	INPUT_CSI_CUU,
	INPUT_CSI_DA,
	INPUT_CSI_DA_TWO,
	INPUT_CSI_DCH,
	INPUT_CSI_DECSCUSR,
	INPUT_CSI_DECSTBM,
	INPUT_CSI_DL,
	INPUT_CSI_DSR,
	INPUT_CSI_ECH,
	INPUT_CSI_ED,
	INPUT_CSI_EL,
	INPUT_CSI_HPA,
	INPUT_CSI_ICH,
	INPUT_CSI_IL,
	INPUT_CSI_RCP,
	INPUT_CSI_RM,
	INPUT_CSI_RM_PRIVATE,
	INPUT_CSI_SCP,
	INPUT_CSI_SGR,
	INPUT_CSI_SM,
	INPUT_CSI_SM_PRIVATE,
	INPUT_CSI_TBC,
	INPUT_CSI_VPA,
	INPUT_CSI_WINOPS,
};

/* Control (CSI) command table. */
const struct input_table_entry input_csi_table[] = {
	{ '@', "",  INPUT_CSI_ICH },
	{ 'A', "",  INPUT_CSI_CUU },
	{ 'B', "",  INPUT_CSI_CUD },
	{ 'C', "",  INPUT_CSI_CUF },
	{ 'D', "",  INPUT_CSI_CUB },
	{ 'E', "",  INPUT_CSI_CNL },
	{ 'F', "",  INPUT_CSI_CPL },
	{ 'G', "",  INPUT_CSI_HPA },
	{ 'H', "",  INPUT_CSI_CUP },
	{ 'J', "",  INPUT_CSI_ED },
	{ 'K', "",  INPUT_CSI_EL },
	{ 'L', "",  INPUT_CSI_IL },
	{ 'M', "",  INPUT_CSI_DL },
	{ 'P', "",  INPUT_CSI_DCH },
	{ 'X', "",  INPUT_CSI_ECH },
	{ 'Z', "",  INPUT_CSI_CBT },
	{ 'c', "",  INPUT_CSI_DA },
	{ 'c', ">", INPUT_CSI_DA_TWO },
	{ 'd', "",  INPUT_CSI_VPA },
	{ 'f', "",  INPUT_CSI_CUP },
	{ 'g', "",  INPUT_CSI_TBC },
	{ 'h', "",  INPUT_CSI_SM },
	{ 'h', "?", INPUT_CSI_SM_PRIVATE },
	{ 'l', "",  INPUT_CSI_RM },
	{ 'l', "?", INPUT_CSI_RM_PRIVATE },
	{ 'm', "",  INPUT_CSI_SGR },
	{ 'n', "",  INPUT_CSI_DSR },
	{ 'q', " ", INPUT_CSI_DECSCUSR },
	{ 'r', "",  INPUT_CSI_DECSTBM },
	{ 's', "",  INPUT_CSI_SCP },
	{ 't', "",  INPUT_CSI_WINOPS },
	{ 'u', "",  INPUT_CSI_RCP },
};

/* Input transition. */
struct input_transition {
	int				first;
	int				last;

	int				(*handler)(struct input_ctx *);
	const struct input_state       *state;
};

/* Input state. */
struct input_state {
	const char			*name;
	void				(*enter)(struct input_ctx *);
	void				(*exit)(struct input_ctx *);
	const struct input_transition	*transitions;
};

/* State transitions available from all states. */
#define INPUT_STATE_ANYWHERE \
	{ 0x18, 0x18, input_c0_dispatch, &input_state_ground }, \
	{ 0x1a, 0x1a, input_c0_dispatch, &input_state_ground }, \
	{ 0x1b, 0x1b, NULL,		 &input_state_esc_enter }

/* Forward declarations of state tables. */
const struct input_transition input_state_ground_table[];
const struct input_transition input_state_esc_enter_table[];
const struct input_transition input_state_esc_intermediate_table[];
const struct input_transition input_state_csi_enter_table[];
const struct input_transition input_state_csi_parameter_table[];
const struct input_transition input_state_csi_intermediate_table[];
const struct input_transition input_state_csi_ignore_table[];
const struct input_transition input_state_dcs_enter_table[];
const struct input_transition input_state_dcs_parameter_table[];
const struct input_transition input_state_dcs_intermediate_table[];
const struct input_transition input_state_dcs_handler_table[];
const struct input_transition input_state_dcs_escape_table[];
const struct input_transition input_state_dcs_ignore_table[];
const struct input_transition input_state_osc_string_table[];
const struct input_transition input_state_apc_string_table[];
const struct input_transition input_state_rename_string_table[];
const struct input_transition input_state_consume_st_table[];
const struct input_transition input_state_utf8_three_table[];
const struct input_transition input_state_utf8_two_table[];
const struct input_transition input_state_utf8_one_table[];

/* ground state definition. */
const struct input_state input_state_ground = {
	"ground",
	input_ground, NULL,
	input_state_ground_table
};

/* esc_enter state definition. */
const struct input_state input_state_esc_enter = {
	"esc_enter",
	input_clear, NULL,
	input_state_esc_enter_table
};

/* esc_intermediate state definition. */
const struct input_state input_state_esc_intermediate = {
	"esc_intermediate",
	NULL, NULL,
	input_state_esc_intermediate_table
};

/* csi_enter state definition. */
const struct input_state input_state_csi_enter = {
	"csi_enter",
	input_clear, NULL,
	input_state_csi_enter_table
};

/* csi_parameter state definition. */
const struct input_state input_state_csi_parameter = {
	"csi_parameter",
	NULL, NULL,
	input_state_csi_parameter_table
};

/* csi_intermediate state definition. */
const struct input_state input_state_csi_intermediate = {
	"csi_intermediate",
	NULL, NULL,
	input_state_csi_intermediate_table
};

/* csi_ignore state definition. */
const struct input_state input_state_csi_ignore = {
	"csi_ignore",
	NULL, NULL,
	input_state_csi_ignore_table
};

/* dcs_enter state definition. */
const struct input_state input_state_dcs_enter = {
	"dcs_enter",
	input_clear, NULL,
	input_state_dcs_enter_table
};

/* dcs_parameter state definition. */
const struct input_state input_state_dcs_parameter = {
	"dcs_parameter",
	NULL, NULL,
	input_state_dcs_parameter_table
};

/* dcs_intermediate state definition. */
const struct input_state input_state_dcs_intermediate = {
	"dcs_intermediate",
	NULL, NULL,
	input_state_dcs_intermediate_table
};

/* dcs_handler state definition. */
const struct input_state input_state_dcs_handler = {
	"dcs_handler",
	NULL, NULL,
	input_state_dcs_handler_table
};

/* dcs_escape state definition. */
const struct input_state input_state_dcs_escape = {
	"dcs_escape",
	NULL, NULL,
	input_state_dcs_escape_table
};

/* dcs_ignore state definition. */
const struct input_state input_state_dcs_ignore = {
	"dcs_ignore",
	NULL, NULL,
	input_state_dcs_ignore_table
};

/* osc_string state definition. */
const struct input_state input_state_osc_string = {
	"osc_string",
	input_enter_osc, input_exit_osc,
	input_state_osc_string_table
};

/* apc_string state definition. */
const struct input_state input_state_apc_string = {
	"apc_string",
	input_enter_apc, input_exit_apc,
	input_state_apc_string_table
};

/* rename_string state definition. */
const struct input_state input_state_rename_string = {
	"rename_string",
	input_enter_rename, input_exit_rename,
	input_state_rename_string_table
};

/* consume_st state definition. */
const struct input_state input_state_consume_st = {
	"consume_st",
	NULL, NULL,
	input_state_consume_st_table
};

/* utf8_three state definition. */
const struct input_state input_state_utf8_three = {
	"utf8_three",
	NULL, NULL,
	input_state_utf8_three_table
};

/* utf8_two state definition. */
const struct input_state input_state_utf8_two = {
	"utf8_two",
	NULL, NULL,
	input_state_utf8_two_table
};

/* utf8_one state definition. */
const struct input_state input_state_utf8_one = {
	"utf8_one",
	NULL, NULL,
	input_state_utf8_one_table
};

/* ground state table. */
const struct input_transition input_state_ground_table[] = {
	INPUT_STATE_ANYWHERE,

	{ 0x00, 0x17, input_c0_dispatch, NULL },
	{ 0x19, 0x19, input_c0_dispatch, NULL },
	{ 0x1c, 0x1f, input_c0_dispatch, NULL },
	{ 0x20, 0x7e, input_print,	 NULL },
	{ 0x7f, 0x7f, NULL,		 NULL },
	{ 0x80, 0xc1, NULL,		 NULL },
	{ 0xc2, 0xdf, input_utf8_open,	 &input_state_utf8_one },
	{ 0xe0, 0xef, input_utf8_open,	 &input_state_utf8_two },
	{ 0xf0, 0xf4, input_utf8_open,	 &input_state_utf8_three },
	{ 0xf5, 0xff, NULL,		 NULL },

	{ -1, -1, NULL, NULL }
};

/* esc_enter state table. */
const struct input_transition input_state_esc_enter_table[] = {
	INPUT_STATE_ANYWHERE,

	{ 0x00, 0x17, input_c0_dispatch,  NULL },
	{ 0x19, 0x19, input_c0_dispatch,  NULL },
	{ 0x1c, 0x1f, input_c0_dispatch,  NULL },
	{ 0x20, 0x2f, input_intermediate, &input_state_esc_intermediate },
	{ 0x30, 0x4f, input_esc_dispatch, &input_state_ground },
	{ 0x50, 0x50, NULL,		  &input_state_dcs_enter },
	{ 0x51, 0x57, input_esc_dispatch, &input_state_ground },
	{ 0x58, 0x58, NULL,		  &input_state_consume_st },
	{ 0x59, 0x59, input_esc_dispatch, &input_state_ground },
	{ 0x5a, 0x5a, input_esc_dispatch, &input_state_ground },
	{ 0x5b, 0x5b, NULL,		  &input_state_csi_enter },
	{ 0x5c, 0x5c, input_esc_dispatch, &input_state_ground },
	{ 0x5d, 0x5d, NULL,		  &input_state_osc_string },
	{ 0x5e, 0x5e, NULL,		  &input_state_consume_st },
	{ 0x5f, 0x5f, NULL,		  &input_state_apc_string },
	{ 0x60, 0x6a, input_esc_dispatch, &input_state_ground },
	{ 0x6b, 0x6b, NULL,		  &input_state_rename_string },
	{ 0x6c, 0x7e, input_esc_dispatch, &input_state_ground },
	{ 0x7f, 0xff, NULL,		  NULL },

	{ -1, -1, NULL, NULL }
};

/* esc_interm state table. */
const struct input_transition input_state_esc_intermediate_table[] = {
	INPUT_STATE_ANYWHERE,

	{ 0x00, 0x17, input_c0_dispatch,  NULL },
	{ 0x19, 0x19, input_c0_dispatch,  NULL },
	{ 0x1c, 0x1f, input_c0_dispatch,  NULL },
	{ 0x20, 0x2f, input_intermediate, NULL },
	{ 0x30, 0x7e, input_esc_dispatch, &input_state_ground },
	{ 0x7f, 0xff, NULL,		  NULL },

	{ -1, -1, NULL, NULL }
};

/* csi_enter state table. */
const struct input_transition input_state_csi_enter_table[] = {
	INPUT_STATE_ANYWHERE,

	{ 0x00, 0x17, input_c0_dispatch,  NULL },
	{ 0x19, 0x19, input_c0_dispatch,  NULL },
	{ 0x1c, 0x1f, input_c0_dispatch,  NULL },
	{ 0x20, 0x2f, input_intermediate, &input_state_csi_intermediate },
	{ 0x30, 0x39, input_parameter,	  &input_state_csi_parameter },
	{ 0x3a, 0x3a, NULL,		  &input_state_csi_ignore },
	{ 0x3b, 0x3b, input_parameter,	  &input_state_csi_parameter },
	{ 0x3c, 0x3f, input_intermediate, &input_state_csi_parameter },
	{ 0x40, 0x7e, input_csi_dispatch, &input_state_ground },
	{ 0x7f, 0xff, NULL,		  NULL },

	{ -1, -1, NULL, NULL }
};

/* csi_parameter state table. */
const struct input_transition input_state_csi_parameter_table[] = {
	INPUT_STATE_ANYWHERE,

	{ 0x00, 0x17, input_c0_dispatch,  NULL },
	{ 0x19, 0x19, input_c0_dispatch,  NULL },
	{ 0x1c, 0x1f, input_c0_dispatch,  NULL },
	{ 0x20, 0x2f, input_intermediate, &input_state_csi_intermediate },
	{ 0x30, 0x39, input_parameter,	  NULL },
	{ 0x3a, 0x3a, NULL,		  &input_state_csi_ignore },
	{ 0x3b, 0x3b, input_parameter,	  NULL },
	{ 0x3c, 0x3f, NULL,		  &input_state_csi_ignore },
	{ 0x40, 0x7e, input_csi_dispatch, &input_state_ground },
	{ 0x7f, 0xff, NULL,		  NULL },

	{ -1, -1, NULL, NULL }
};

/* csi_intermediate state table. */
const struct input_transition input_state_csi_intermediate_table[] = {
	INPUT_STATE_ANYWHERE,

	{ 0x00, 0x17, input_c0_dispatch,  NULL },
	{ 0x19, 0x19, input_c0_dispatch,  NULL },
	{ 0x1c, 0x1f, input_c0_dispatch,  NULL },
	{ 0x20, 0x2f, input_intermediate, NULL },
	{ 0x30, 0x3f, NULL,		  &input_state_csi_ignore },
	{ 0x40, 0x7e, input_csi_dispatch, &input_state_ground },
	{ 0x7f, 0xff, NULL,		  NULL },

	{ -1, -1, NULL, NULL }
};

/* csi_ignore state table. */
const struct input_transition input_state_csi_ignore_table[] = {
	INPUT_STATE_ANYWHERE,

	{ 0x00, 0x17, input_c0_dispatch, NULL },
	{ 0x19, 0x19, input_c0_dispatch, NULL },
	{ 0x1c, 0x1f, input_c0_dispatch, NULL },
	{ 0x20, 0x3f, NULL,		 NULL },
	{ 0x40, 0x7e, NULL,		 &input_state_ground },
	{ 0x7f, 0xff, NULL,		 NULL },

	{ -1, -1, NULL, NULL }
};

/* dcs_enter state table. */
const struct input_transition input_state_dcs_enter_table[] = {
	INPUT_STATE_ANYWHERE,

	{ 0x00, 0x17, NULL,		  NULL },
	{ 0x19, 0x19, NULL,		  NULL },
	{ 0x1c, 0x1f, NULL,		  NULL },
	{ 0x20, 0x2f, input_intermediate, &input_state_dcs_intermediate },
	{ 0x30, 0x39, input_parameter,	  &input_state_dcs_parameter },
	{ 0x3a, 0x3a, NULL,		  &input_state_dcs_ignore },
	{ 0x3b, 0x3b, input_parameter,	  &input_state_dcs_parameter },
	{ 0x3c, 0x3f, input_intermediate, &input_state_dcs_parameter },
	{ 0x40, 0x7e, input_input,	  &input_state_dcs_handler },
	{ 0x7f, 0xff, NULL,		  NULL },

	{ -1, -1, NULL, NULL }
};

/* dcs_parameter state table. */
const struct input_transition input_state_dcs_parameter_table[] = {
	INPUT_STATE_ANYWHERE,

	{ 0x00, 0x17, NULL,		  NULL },
	{ 0x19, 0x19, NULL,		  NULL },
	{ 0x1c, 0x1f, NULL,		  NULL },
	{ 0x20, 0x2f, input_intermediate, &input_state_dcs_intermediate },
	{ 0x30, 0x39, input_parameter,	  NULL },
	{ 0x3a, 0x3a, NULL,		  &input_state_dcs_ignore },
	{ 0x3b, 0x3b, input_parameter,	  NULL },
	{ 0x3c, 0x3f, NULL,		  &input_state_dcs_ignore },
	{ 0x40, 0x7e, input_input,	  &input_state_dcs_handler },
	{ 0x7f, 0xff, NULL,		  NULL },

	{ -1, -1, NULL, NULL }
};

/* dcs_interm state table. */
const struct input_transition input_state_dcs_intermediate_table[] = {
	INPUT_STATE_ANYWHERE,

	{ 0x00, 0x17, NULL,		  NULL },
	{ 0x19, 0x19, NULL,		  NULL },
	{ 0x1c, 0x1f, NULL,		  NULL },
	{ 0x20, 0x2f, input_intermediate, NULL },
	{ 0x30, 0x3f, NULL,		  &input_state_dcs_ignore },
	{ 0x40, 0x7e, input_input,	  &input_state_dcs_handler },
	{ 0x7f, 0xff, NULL,		  NULL },

	{ -1, -1, NULL, NULL }
};

/* dcs_handler state table. */
const struct input_transition input_state_dcs_handler_table[] = {
	/* No INPUT_STATE_ANYWHERE */

	{ 0x00, 0x1a, input_input,  NULL },
	{ 0x1b, 0x1b, NULL,	    &input_state_dcs_escape },
	{ 0x1c, 0xff, input_input,  NULL },

	{ -1, -1, NULL, NULL }
};

/* dcs_escape state table. */
const struct input_transition input_state_dcs_escape_table[] = {
	/* No INPUT_STATE_ANYWHERE */

	{ 0x00, 0x5b, input_input,	  &input_state_dcs_handler },
	{ 0x5c, 0x5c, input_dcs_dispatch, &input_state_ground },
	{ 0x5d, 0xff, input_input,	  &input_state_dcs_handler },

	{ -1, -1, NULL, NULL }
};

/* dcs_ignore state table. */
const struct input_transition input_state_dcs_ignore_table[] = {
	INPUT_STATE_ANYWHERE,

	{ 0x00, 0x17, NULL,	    NULL },
	{ 0x19, 0x19, NULL,	    NULL },
	{ 0x1c, 0x1f, NULL,	    NULL },
	{ 0x20, 0xff, NULL,	    NULL },

	{ -1, -1, NULL, NULL }
};

/* osc_string state table. */
const struct input_transition input_state_osc_string_table[] = {
	INPUT_STATE_ANYWHERE,

	{ 0x00, 0x06, NULL,	    NULL },
	{ 0x07, 0x07, NULL,	    &input_state_ground },
	{ 0x08, 0x17, NULL,	    NULL },
	{ 0x19, 0x19, NULL,	    NULL },
	{ 0x1c, 0x1f, NULL,	    NULL },
	{ 0x20, 0xff, input_input,  NULL },

	{ -1, -1, NULL, NULL }
};

/* apc_string state table. */
const struct input_transition input_state_apc_string_table[] = {
	INPUT_STATE_ANYWHERE,

	{ 0x00, 0x17, NULL,	    NULL },
	{ 0x19, 0x19, NULL,	    NULL },
	{ 0x1c, 0x1f, NULL,	    NULL },
	{ 0x20, 0xff, input_input,  NULL },

	{ -1, -1, NULL, NULL }
};

/* rename_string state table. */
const struct input_transition input_state_rename_string_table[] = {
	INPUT_STATE_ANYWHERE,

	{ 0x00, 0x17, NULL,	    NULL },
	{ 0x19, 0x19, NULL,	    NULL },
	{ 0x1c, 0x1f, NULL,	    NULL },
	{ 0x20, 0xff, input_input,  NULL },

	{ -1, -1, NULL, NULL }
};

/* consume_st state table. */
const struct input_transition input_state_consume_st_table[] = {
	INPUT_STATE_ANYWHERE,

	{ 0x00, 0x17, NULL,	    NULL },
	{ 0x19, 0x19, NULL,	    NULL },
	{ 0x1c, 0x1f, NULL,	    NULL },
	{ 0x20, 0xff, NULL,	    NULL },

	{ -1, -1, NULL, NULL }
};

/* utf8_three state table. */
const struct input_transition input_state_utf8_three_table[] = {
	/* No INPUT_STATE_ANYWHERE */

	{ 0x00, 0x7f, NULL,		&input_state_ground },
	{ 0x80, 0xbf, input_utf8_add,	&input_state_utf8_two },
	{ 0xc0, 0xff, NULL,		&input_state_ground },

	{ -1, -1, NULL, NULL }
};

/* utf8_two state table. */
const struct input_transition input_state_utf8_two_table[] = {
	/* No INPUT_STATE_ANYWHERE */

	{ 0x00, 0x7f, NULL,	      &input_state_ground },
	{ 0x80, 0xbf, input_utf8_add, &input_state_utf8_one },
	{ 0xc0, 0xff, NULL,	      &input_state_ground },

	{ -1, -1, NULL, NULL }
};

/* utf8_one state table. */
const struct input_transition input_state_utf8_one_table[] = {
	/* No INPUT_STATE_ANYWHERE */

	{ 0x00, 0x7f, NULL,		&input_state_ground },
	{ 0x80, 0xbf, input_utf8_close, &input_state_ground },
	{ 0xc0, 0xff, NULL,		&input_state_ground },

	{ -1, -1, NULL, NULL }
};

/* Input table compare. */
int
input_table_compare(const void *key, const void *value)
{
	const struct input_ctx		*ictx = key;
	const struct input_table_entry	*entry = value;

	if (ictx->ch != entry->ch)
		return (ictx->ch - entry->ch);
	return (strcmp(ictx->interm_buf, entry->interm));
}

/* Reset cell state to default. */
void
input_reset_cell(struct input_ctx *ictx)
{
	memcpy(&ictx->cell.cell, &grid_default_cell, sizeof ictx->cell.cell);
	ictx->cell.set = 0;
	ictx->cell.g0set = ictx->cell.g1set = 0;

	memcpy(&ictx->old_cell, &ictx->cell, sizeof ictx->old_cell);
	ictx->old_cx = 0;
	ictx->old_cy = 0;
}

/* Initialise input parser. */
void
input_init(struct window_pane *wp)
{
	struct input_ctx	*ictx;

	ictx = wp->ictx = xcalloc(1, sizeof *ictx);

	ictx->input_space = INPUT_BUF_START;
	ictx->input_buf = xmalloc(INPUT_BUF_START);

	ictx->since_ground = evbuffer_new();

	input_reset(wp, 0);
}

/* Destroy input parser. */
void
input_free(struct window_pane *wp)
{
	struct input_ctx	*ictx = wp->ictx;

	free(ictx->input_buf);
	evbuffer_free(ictx->since_ground);

	free (ictx);
	wp->ictx = NULL;
}

/* Reset input state and clear screen. */
void
input_reset(struct window_pane *wp, int clear)
{
	struct input_ctx	*ictx = wp->ictx;

	input_reset_cell(ictx);

	if (clear) {
		if (wp->mode == NULL)
			screen_write_start(&ictx->ctx, wp, &wp->base);
		else
			screen_write_start(&ictx->ctx, NULL, &wp->base);
		screen_write_reset(&ictx->ctx);
		screen_write_stop(&ictx->ctx);
	}

	*ictx->interm_buf = '\0';
	ictx->interm_len = 0;

	*ictx->param_buf = '\0';
	ictx->param_len = 0;

	*ictx->input_buf = '\0';
	ictx->input_len = 0;

	ictx->state = &input_state_ground;
	ictx->flags = 0;
}

/* Return pending data. */
struct evbuffer *
input_pending(struct window_pane *wp)
{
	return (wp->ictx->since_ground);
}

/* Change input state. */
void
input_set_state(struct window_pane *wp, const struct input_transition *itr)
{
	struct input_ctx	*ictx = wp->ictx;

	if (ictx->state->exit != NULL)
		ictx->state->exit(ictx);
	ictx->state = itr->state;
	if (ictx->state->enter != NULL)
		ictx->state->enter(ictx);
}

/* Parse input. */
void
input_parse(struct window_pane *wp)
{
	struct input_ctx		*ictx = wp->ictx;
	const struct input_transition	*itr;
	struct evbuffer			*evb = wp->event->input;
	u_char				*buf;
	size_t				 len, off;

	if (EVBUFFER_LENGTH(evb) == 0)
		return;

	window_update_activity(wp->window);
	wp->flags |= PANE_CHANGED;

	/*
	 * Open the screen. Use NULL wp if there is a mode set as don't want to
	 * update the tty.
	 */
	if (wp->mode == NULL)
		screen_write_start(&ictx->ctx, wp, &wp->base);
	else
		screen_write_start(&ictx->ctx, NULL, &wp->base);
	ictx->wp = wp;

	buf = EVBUFFER_DATA(evb);
	len = EVBUFFER_LENGTH(evb);
	notify_input(wp, evb);
	off = 0;

	log_debug("%s: %%%u %s, %zu bytes: %.*s", __func__, wp->id,
	    ictx->state->name, len, (int)len, buf);

	/* Parse the input. */
	while (off < len) {
		ictx->ch = buf[off++];

		/* Find the transition. */
		itr = ictx->state->transitions;
		while (itr->first != -1 && itr->last != -1) {
			if (ictx->ch >= itr->first && ictx->ch <= itr->last)
				break;
			itr++;
		}
		if (itr->first == -1 || itr->last == -1) {
			/* No transition? Eh? */
			fatalx("no transition from state");
		}

		/*
		 * Execute the handler, if any. Don't switch state if it
		 * returns non-zero.
		 */
		if (itr->handler != NULL && itr->handler(ictx) != 0)
			continue;

		/* And switch state, if necessary. */
		if (itr->state != NULL)
			input_set_state(wp, itr);

		/* If not in ground state, save input. */
		if (ictx->state != &input_state_ground)
			evbuffer_add(ictx->since_ground, &ictx->ch, 1);
	}

	/* Close the screen. */
	screen_write_stop(&ictx->ctx);

	evbuffer_drain(evb, len);
}

/* Split the parameter list (if any). */
int
input_split(struct input_ctx *ictx)

{
	const char	*errstr;
	char		*ptr, *out;
	int		 n;

	ictx->param_list_len = 0;
	if (ictx->param_len == 0)
		return (0);

	ptr = ictx->param_buf;
	while ((out = strsep(&ptr, ";")) != NULL) {
		if (*out == '\0')
			n = -1;
		else {
			n = strtonum(out, 0, INT_MAX, &errstr);
			if (errstr != NULL)
				return (-1);
		}

		ictx->param_list[ictx->param_list_len++] = n;
		if (ictx->param_list_len == nitems(ictx->param_list))
			return (-1);
	}

	return (0);
}

/* Get an argument or return default value. */
int
input_get(struct input_ctx *ictx, u_int validx, int minval, int defval)
{
	int	retval;

	if (validx >= ictx->param_list_len)
	    return (defval);

	retval = ictx->param_list[validx];
	if (retval == -1)
		return (defval);
	if (retval < minval)
		return (minval);
	return (retval);
}

/* Reply to terminal query. */
void
input_reply(struct input_ctx *ictx, const char *fmt, ...)
{
	va_list	ap;
	char   *reply;

	va_start(ap, fmt);
	xvasprintf(&reply, fmt, ap);
	va_end(ap);

	bufferevent_write(ictx->wp->event, reply, strlen(reply));
	free(reply);
}

/* Clear saved state. */
void
input_clear(struct input_ctx *ictx)
{
	*ictx->interm_buf = '\0';
	ictx->interm_len = 0;

	*ictx->param_buf = '\0';
	ictx->param_len = 0;

	*ictx->input_buf = '\0';
	ictx->input_len = 0;

	ictx->flags &= ~INPUT_DISCARD;
}

/* Reset for ground state. */
void
input_ground(struct input_ctx *ictx)
{
	evbuffer_drain(ictx->since_ground, EVBUFFER_LENGTH(ictx->since_ground));

	if (ictx->input_space > INPUT_BUF_START) {
		ictx->input_space = INPUT_BUF_START;
		ictx->input_buf = xrealloc(ictx->input_buf, INPUT_BUF_START);
	}
}

/* Output this character to the screen. */
int
input_print(struct input_ctx *ictx)
{
	int	set;

	set = ictx->cell.set == 0 ? ictx->cell.g0set : ictx->cell.g1set;
	if (set == 1)
		ictx->cell.cell.attr |= GRID_ATTR_CHARSET;
	else
		ictx->cell.cell.attr &= ~GRID_ATTR_CHARSET;

	utf8_set(&ictx->cell.cell.data, ictx->ch);
	screen_write_cell(&ictx->ctx, &ictx->cell.cell);

	ictx->cell.cell.attr &= ~GRID_ATTR_CHARSET;

	return (0);
}

/* Collect intermediate string. */
int
input_intermediate(struct input_ctx *ictx)
{
	if (ictx->interm_len == (sizeof ictx->interm_buf) - 1)
		ictx->flags |= INPUT_DISCARD;
	else {
		ictx->interm_buf[ictx->interm_len++] = ictx->ch;
		ictx->interm_buf[ictx->interm_len] = '\0';
	}

	return (0);
}

/* Collect parameter string. */
int
input_parameter(struct input_ctx *ictx)
{
	if (ictx->param_len == (sizeof ictx->param_buf) - 1)
		ictx->flags |= INPUT_DISCARD;
	else {
		ictx->param_buf[ictx->param_len++] = ictx->ch;
		ictx->param_buf[ictx->param_len] = '\0';
	}

	return (0);
}

/* Collect input string. */
int
input_input(struct input_ctx *ictx)
{
	size_t available;

	available = ictx->input_space;
	while (ictx->input_len + 1 >= available) {
		available *= 2;
		if (available > INPUT_BUF_LIMIT) {
			ictx->flags |= INPUT_DISCARD;
			return (0);
		}
		ictx->input_buf = xrealloc(ictx->input_buf, available);
		ictx->input_space = available;
	}
	ictx->input_buf[ictx->input_len++] = ictx->ch;
	ictx->input_buf[ictx->input_len] = '\0';

	return (0);
}

/* Execute C0 control sequence. */
int
input_c0_dispatch(struct input_ctx *ictx)
{
	struct screen_write_ctx	*sctx = &ictx->ctx;
	struct window_pane	*wp = ictx->wp;
	struct screen		*s = sctx->s;

	log_debug("%s: '%c'", __func__, ictx->ch);

	switch (ictx->ch) {
	case '\000':	/* NUL */
		break;
	case '\007':	/* BEL */
		alerts_queue(wp->window, WINDOW_BELL);
		break;
	case '\010':	/* BS */
		screen_write_backspace(sctx);
		break;
	case '\011':	/* HT */
		/* Don't tab beyond the end of the line. */
		if (s->cx >= screen_size_x(s) - 1)
			break;

		/* Find the next tab point, or use the last column if none. */
		do {
			s->cx++;
			if (bit_test(s->tabs, s->cx))
				break;
		} while (s->cx < screen_size_x(s) - 1);
		break;
	case '\012':	/* LF */
	case '\013':	/* VT */
	case '\014':	/* FF */
		screen_write_linefeed(sctx, 0);
		break;
	case '\015':	/* CR */
		screen_write_carriagereturn(sctx);
		break;
	case '\016':	/* SO */
		ictx->cell.set = 1;
		break;
	case '\017':	/* SI */
		ictx->cell.set = 0;
		break;
	default:
		log_debug("%s: unknown '%c'", __func__, ictx->ch);
		break;
	}

	return (0);
}

/* Execute escape sequence. */
int
input_esc_dispatch(struct input_ctx *ictx)
{
	struct screen_write_ctx		*sctx = &ictx->ctx;
	struct screen			*s = sctx->s;
	struct input_table_entry	*entry;

	if (ictx->flags & INPUT_DISCARD)
		return (0);
	log_debug("%s: '%c', %s", __func__, ictx->ch, ictx->interm_buf);

	entry = bsearch(ictx, input_esc_table, nitems(input_esc_table),
	    sizeof input_esc_table[0], input_table_compare);
	if (entry == NULL) {
		log_debug("%s: unknown '%c'", __func__, ictx->ch);
		return (0);
	}

	switch (entry->type) {
	case INPUT_ESC_RIS:
		input_reset_cell(ictx);
		screen_write_reset(sctx);
		break;
	case INPUT_ESC_IND:
		screen_write_linefeed(sctx, 0);
		break;
	case INPUT_ESC_NEL:
		screen_write_carriagereturn(sctx);
		screen_write_linefeed(sctx, 0);
		break;
	case INPUT_ESC_HTS:
		if (s->cx < screen_size_x(s))
			bit_set(s->tabs, s->cx);
		break;
	case INPUT_ESC_RI:
		screen_write_reverseindex(sctx);
		break;
	case INPUT_ESC_DECKPAM:
		screen_write_mode_set(sctx, MODE_KKEYPAD);
		break;
	case INPUT_ESC_DECKPNM:
		screen_write_mode_clear(sctx, MODE_KKEYPAD);
		break;
	case INPUT_ESC_DECSC:
		memcpy(&ictx->old_cell, &ictx->cell, sizeof ictx->old_cell);
		ictx->old_cx = s->cx;
		ictx->old_cy = s->cy;
		break;
	case INPUT_ESC_DECRC:
		memcpy(&ictx->cell, &ictx->old_cell, sizeof ictx->cell);
		screen_write_cursormove(sctx, ictx->old_cx, ictx->old_cy);
		break;
	case INPUT_ESC_DECALN:
		screen_write_alignmenttest(sctx);
		break;
	case INPUT_ESC_SCSG0_ON:
		ictx->cell.g0set = 1;
		break;
	case INPUT_ESC_SCSG0_OFF:
		ictx->cell.g0set = 0;
		break;
	case INPUT_ESC_SCSG1_ON:
		ictx->cell.g1set = 1;
		break;
	case INPUT_ESC_SCSG1_OFF:
		ictx->cell.g1set = 0;
		break;
	}

	return (0);
}

/* Execute control sequence. */
int
input_csi_dispatch(struct input_ctx *ictx)
{
	struct screen_write_ctx	       *sctx = &ictx->ctx;
	struct screen		       *s = sctx->s;
	struct input_table_entry       *entry;
	int				n, m;
	u_int				cx;

	if (ictx->flags & INPUT_DISCARD)
		return (0);
	if (input_split(ictx) != 0)
		return (0);
	log_debug("%s: '%c' \"%s\" \"%s\"",
	    __func__, ictx->ch, ictx->interm_buf, ictx->param_buf);

	entry = bsearch(ictx, input_csi_table, nitems(input_csi_table),
	    sizeof input_csi_table[0], input_table_compare);
	if (entry == NULL) {
		log_debug("%s: unknown '%c'", __func__, ictx->ch);
		return (0);
	}

	switch (entry->type) {
	case INPUT_CSI_CBT:
		/* Find the previous tab point, n times. */
		cx = s->cx;
		if (cx > screen_size_x(s) - 1)
			cx = screen_size_x(s) - 1;
		n = input_get(ictx, 0, 1, 1);
		while (cx > 0 && n-- > 0) {
			do
				cx--;
			while (cx > 0 && !bit_test(s->tabs, cx));
		}
		s->cx = cx;
		break;
	case INPUT_CSI_CUB:
		screen_write_cursorleft(sctx, input_get(ictx, 0, 1, 1));
		break;
	case INPUT_CSI_CUD:
		screen_write_cursordown(sctx, input_get(ictx, 0, 1, 1));
		break;
	case INPUT_CSI_CUF:
		screen_write_cursorright(sctx, input_get(ictx, 0, 1, 1));
		break;
	case INPUT_CSI_CUP:
		n = input_get(ictx, 0, 1, 1);
		m = input_get(ictx, 1, 1, 1);
		screen_write_cursormove(sctx, m - 1, n - 1);
		break;
	case INPUT_CSI_WINOPS:
		input_csi_dispatch_winops(ictx);
		break;
	case INPUT_CSI_CUU:
		screen_write_cursorup(sctx, input_get(ictx, 0, 1, 1));
		break;
	case INPUT_CSI_CNL:
		screen_write_carriagereturn(sctx);
		screen_write_cursordown(sctx, input_get(ictx, 0, 1, 1));
		break;
	case INPUT_CSI_CPL:
		screen_write_carriagereturn(sctx);
		screen_write_cursorup(sctx, input_get(ictx, 0, 1, 1));
		break;
	case INPUT_CSI_DA:
		switch (input_get(ictx, 0, 0, 0)) {
		case 0:
			input_reply(ictx, "\033[?1;2c");
			break;
		default:
			log_debug("%s: unknown '%c'", __func__, ictx->ch);
			break;
		}
		break;
	case INPUT_CSI_DA_TWO:
		switch (input_get(ictx, 0, 0, 0)) {
		case 0:
			input_reply(ictx, "\033[>84;0;0c");
			break;
		default:
			log_debug("%s: unknown '%c'", __func__, ictx->ch);
			break;
		}
		break;
	case INPUT_CSI_ECH:
		screen_write_clearcharacter(sctx, input_get(ictx, 0, 1, 1));
		break;
	case INPUT_CSI_DCH:
		screen_write_deletecharacter(sctx, input_get(ictx, 0, 1, 1));
		break;
	case INPUT_CSI_DECSTBM:
		n = input_get(ictx, 0, 1, 1);
		m = input_get(ictx, 1, 1, screen_size_y(s));
		screen_write_scrollregion(sctx, n - 1, m - 1);
		break;
	case INPUT_CSI_DL:
		screen_write_deleteline(sctx, input_get(ictx, 0, 1, 1));
		break;
	case INPUT_CSI_DSR:
		switch (input_get(ictx, 0, 0, 0)) {
		case 5:
			input_reply(ictx, "\033[0n");
			break;
		case 6:
			input_reply(ictx, "\033[%u;%uR", s->cy + 1, s->cx + 1);
			break;
		default:
			log_debug("%s: unknown '%c'", __func__, ictx->ch);
			break;
		}
		break;
	case INPUT_CSI_ED:
		switch (input_get(ictx, 0, 0, 0)) {
		case 0:
			screen_write_clearendofscreen(sctx);
			break;
		case 1:
			screen_write_clearstartofscreen(sctx);
			break;
		case 2:
			screen_write_clearscreen(sctx);
			break;
		case 3:
			switch (input_get(ictx, 1, 0, 0)) {
			case 0:
				/*
				 * Linux console extension to clear history
				 * (for example before locking the screen).
				 */
				screen_write_clearhistory(sctx);
				break;
			}
			break;
		default:
			log_debug("%s: unknown '%c'", __func__, ictx->ch);
			break;
		}
		break;
	case INPUT_CSI_EL:
		switch (input_get(ictx, 0, 0, 0)) {
		case 0:
			screen_write_clearendofline(sctx);
			break;
		case 1:
			screen_write_clearstartofline(sctx);
			break;
		case 2:
			screen_write_clearline(sctx);
			break;
		default:
			log_debug("%s: unknown '%c'", __func__, ictx->ch);
			break;
		}
		break;
	case INPUT_CSI_HPA:
		n = input_get(ictx, 0, 1, 1);
		screen_write_cursormove(sctx, n - 1, s->cy);
		break;
	case INPUT_CSI_ICH:
		screen_write_insertcharacter(sctx, input_get(ictx, 0, 1, 1));
		break;
	case INPUT_CSI_IL:
		screen_write_insertline(sctx, input_get(ictx, 0, 1, 1));
		break;
	case INPUT_CSI_RCP:
		memcpy(&ictx->cell, &ictx->old_cell, sizeof ictx->cell);
		screen_write_cursormove(sctx, ictx->old_cx, ictx->old_cy);
		break;
	case INPUT_CSI_RM:
		input_csi_dispatch_rm(ictx);
		break;
	case INPUT_CSI_RM_PRIVATE:
		input_csi_dispatch_rm_private(ictx);
		break;
	case INPUT_CSI_SCP:
		memcpy(&ictx->old_cell, &ictx->cell, sizeof ictx->old_cell);
		ictx->old_cx = s->cx;
		ictx->old_cy = s->cy;
		break;
	case INPUT_CSI_SGR:
		input_csi_dispatch_sgr(ictx);
		break;
	case INPUT_CSI_SM:
		input_csi_dispatch_sm(ictx);
		break;
	case INPUT_CSI_SM_PRIVATE:
		input_csi_dispatch_sm_private(ictx);
		break;
	case INPUT_CSI_TBC:
		switch (input_get(ictx, 0, 0, 0)) {
		case 0:
			if (s->cx < screen_size_x(s))
				bit_clear(s->tabs, s->cx);
			break;
		case 3:
			bit_nclear(s->tabs, 0, screen_size_x(s) - 1);
			break;
		default:
			log_debug("%s: unknown '%c'", __func__, ictx->ch);
			break;
		}
		break;
	case INPUT_CSI_VPA:
		n = input_get(ictx, 0, 1, 1);
		screen_write_cursormove(sctx, s->cx, n - 1);
		break;
	case INPUT_CSI_DECSCUSR:
		n = input_get(ictx, 0, 0, 0);
		screen_set_cursor_style(s, n);
		break;
	}

	return (0);
}

/* Handle CSI RM. */
void
input_csi_dispatch_rm(struct input_ctx *ictx)
{
	u_int	i;

	for (i = 0; i < ictx->param_list_len; i++) {
		switch (input_get(ictx, i, 0, -1)) {
		case 4:		/* IRM */
			screen_write_mode_clear(&ictx->ctx, MODE_INSERT);
			break;
		case 34:
			screen_write_mode_set(&ictx->ctx, MODE_BLINKING);
			break;
		default:
			log_debug("%s: unknown '%c'", __func__, ictx->ch);
			break;
		}
	}
}

/* Handle CSI private RM. */
void
input_csi_dispatch_rm_private(struct input_ctx *ictx)
{
	struct window_pane	*wp = ictx->wp;
	u_int			 i;

	for (i = 0; i < ictx->param_list_len; i++) {
		switch (input_get(ictx, i, 0, -1)) {
		case 1:		/* DECCKM */
			screen_write_mode_clear(&ictx->ctx, MODE_KCURSOR);
			break;
		case 3:		/* DECCOLM */
			screen_write_cursormove(&ictx->ctx, 0, 0);
			screen_write_clearscreen(&ictx->ctx);
			break;
		case 7:		/* DECAWM */
			screen_write_mode_clear(&ictx->ctx, MODE_WRAP);
			break;
		case 12:
			screen_write_mode_clear(&ictx->ctx, MODE_BLINKING);
			break;
		case 25:	/* TCEM */
			screen_write_mode_clear(&ictx->ctx, MODE_CURSOR);
			break;
		case 1000:
		case 1001:
		case 1002:
			screen_write_mode_clear(&ictx->ctx, ALL_MOUSE_MODES);
			break;
		case 1004:
			screen_write_mode_clear(&ictx->ctx, MODE_FOCUSON);
			break;
		case 1005:
			screen_write_mode_clear(&ictx->ctx, MODE_MOUSE_UTF8);
			break;
		case 1006:
			screen_write_mode_clear(&ictx->ctx, MODE_MOUSE_SGR);
			break;
		case 47:
		case 1047:
			window_pane_alternate_off(wp, &ictx->cell.cell, 0);
			break;
		case 1049:
			window_pane_alternate_off(wp, &ictx->cell.cell, 1);
			break;
		case 2004:
			screen_write_mode_clear(&ictx->ctx, MODE_BRACKETPASTE);
			break;
		default:
			log_debug("%s: unknown '%c'", __func__, ictx->ch);
			break;
		}
	}
}

/* Handle CSI SM. */
void
input_csi_dispatch_sm(struct input_ctx *ictx)
{
	u_int	i;

	for (i = 0; i < ictx->param_list_len; i++) {
		switch (input_get(ictx, i, 0, -1)) {
		case 4:		/* IRM */
			screen_write_mode_set(&ictx->ctx, MODE_INSERT);
			break;
		case 34:
			screen_write_mode_clear(&ictx->ctx, MODE_BLINKING);
			break;
		default:
			log_debug("%s: unknown '%c'", __func__, ictx->ch);
			break;
		}
	}
}

/* Handle CSI private SM. */
void
input_csi_dispatch_sm_private(struct input_ctx *ictx)
{
	struct window_pane	*wp = ictx->wp;
	u_int			 i;

	for (i = 0; i < ictx->param_list_len; i++) {
		switch (input_get(ictx, i, 0, -1)) {
		case 1:		/* DECCKM */
			screen_write_mode_set(&ictx->ctx, MODE_KCURSOR);
			break;
		case 3:		/* DECCOLM */
			screen_write_cursormove(&ictx->ctx, 0, 0);
			screen_write_clearscreen(&ictx->ctx);
			break;
		case 7:		/* DECAWM */
			screen_write_mode_set(&ictx->ctx, MODE_WRAP);
			break;
		case 12:
			screen_write_mode_set(&ictx->ctx, MODE_BLINKING);
			break;
		case 25:	/* TCEM */
			screen_write_mode_set(&ictx->ctx, MODE_CURSOR);
			break;
		case 1000:
			screen_write_mode_clear(&ictx->ctx, ALL_MOUSE_MODES);
			screen_write_mode_set(&ictx->ctx, MODE_MOUSE_STANDARD);
			break;
		case 1002:
			screen_write_mode_clear(&ictx->ctx, ALL_MOUSE_MODES);
			screen_write_mode_set(&ictx->ctx, MODE_MOUSE_BUTTON);
			break;
		case 1004:
			if (ictx->ctx.s->mode & MODE_FOCUSON)
				break;
			screen_write_mode_set(&ictx->ctx, MODE_FOCUSON);
			wp->flags |= PANE_FOCUSPUSH; /* force update */
			break;
		case 1005:
			screen_write_mode_set(&ictx->ctx, MODE_MOUSE_UTF8);
			break;
		case 1006:
			screen_write_mode_set(&ictx->ctx, MODE_MOUSE_SGR);
			break;
		case 47:
		case 1047:
			window_pane_alternate_on(wp, &ictx->cell.cell, 0);
			break;
		case 1049:
			window_pane_alternate_on(wp, &ictx->cell.cell, 1);
			break;
		case 2004:
			screen_write_mode_set(&ictx->ctx, MODE_BRACKETPASTE);
			break;
		default:
			log_debug("%s: unknown '%c'", __func__, ictx->ch);
			break;
		}
	}
}

/* Handle CSI window operations. */
void
input_csi_dispatch_winops(struct input_ctx *ictx)
{
	struct window_pane	*wp = ictx->wp;
	int			 n, m;

	m = 0;
	while ((n = input_get(ictx, m, 0, -1)) != -1) {
		switch (n) {
		case 1:
		case 2:
		case 5:
		case 6:
		case 7:
		case 11:
		case 13:
		case 14:
		case 19:
		case 20:
		case 21:
		case 24:
			break;
		case 3:
		case 4:
		case 8:
			m++;
			if (input_get(ictx, m, 0, -1) == -1)
				return;
			/* FALLTHROUGH */
		case 9:
		case 10:
		case 22:
		case 23:
			m++;
			if (input_get(ictx, m, 0, -1) == -1)
				return;
			break;
		case 18:
			input_reply(ictx, "\033[8;%u;%ut", wp->sy, wp->sx);
			break;
		default:
			log_debug("%s: unknown '%c'", __func__, ictx->ch);
			break;
		}
		m++;
	}
}

/* Handle CSI SGR for 256 colours. */
void
input_csi_dispatch_sgr_256(struct input_ctx *ictx, int fgbg, u_int *i)
{
	struct grid_cell	*gc = &ictx->cell.cell;
	int			 c;

	(*i)++;
	c = input_get(ictx, *i, 0, -1);
	if (c == -1) {
		if (fgbg == 38)
			gc->fg = 8;
		else if (fgbg == 48)
			gc->bg = 8;
	} else {
		if (fgbg == 38)
			gc->fg = c | COLOUR_FLAG_256;
		else if (fgbg == 48)
			gc->bg = c | COLOUR_FLAG_256;
	}
}

/* Handle CSI SGR for RGB colours. */
void
input_csi_dispatch_sgr_rgb(struct input_ctx *ictx, int fgbg, u_int *i)
{
	struct grid_cell	*gc = &ictx->cell.cell;
	int			 r, g, b;

	(*i)++;
	r = input_get(ictx, *i, 0, -1);
	if (r == -1 || r > 255)
		return;
	(*i)++;
	g = input_get(ictx, *i, 0, -1);
	if (g == -1 || g > 255)
		return;
	(*i)++;
	b = input_get(ictx, *i, 0, -1);
	if (b == -1 || b > 255)
		return;

	if (fgbg == 38)
		gc->fg = colour_join_rgb(r, g, b);
	else if (fgbg == 48)
		gc->bg = colour_join_rgb(r, g, b);
}

/* Handle CSI SGR. */
void
input_csi_dispatch_sgr(struct input_ctx *ictx)
{
	struct grid_cell	*gc = &ictx->cell.cell;
	u_int			 i;
	int			 n;

	if (ictx->param_list_len == 0) {
		memcpy(gc, &grid_default_cell, sizeof *gc);
		return;
	}

	for (i = 0; i < ictx->param_list_len; i++) {
		n = input_get(ictx, i, 0, 0);

		if (n == 38 || n == 48) {
			i++;
			switch (input_get(ictx, i, 0, -1)) {
			case 2:
				input_csi_dispatch_sgr_rgb(ictx, n, &i);
				break;
			case 5:
				input_csi_dispatch_sgr_256(ictx, n, &i);
				break;
			}
			continue;
		}

		switch (n) {
		case 0:
		case 10:
			memcpy(gc, &grid_default_cell, sizeof *gc);
			break;
		case 1:
			gc->attr |= GRID_ATTR_BRIGHT;
			break;
		case 2:
			gc->attr |= GRID_ATTR_DIM;
			break;
		case 3:
			gc->attr |= GRID_ATTR_ITALICS;
			break;
		case 4:
			gc->attr |= GRID_ATTR_UNDERSCORE;
			break;
		case 5:
			gc->attr |= GRID_ATTR_BLINK;
			break;
		case 7:
			gc->attr |= GRID_ATTR_REVERSE;
			break;
		case 8:
			gc->attr |= GRID_ATTR_HIDDEN;
			break;
		case 22:
			gc->attr &= ~(GRID_ATTR_BRIGHT|GRID_ATTR_DIM);
			break;
		case 23:
			gc->attr &= ~GRID_ATTR_ITALICS;
			break;
		case 24:
			gc->attr &= ~GRID_ATTR_UNDERSCORE;
			break;
		case 25:
			gc->attr &= ~GRID_ATTR_BLINK;
			break;
		case 27:
			gc->attr &= ~GRID_ATTR_REVERSE;
			break;
		case 30:
		case 31:
		case 32:
		case 33:
		case 34:
		case 35:
		case 36:
		case 37:
			gc->fg = n - 30;
			break;
		case 39:
			gc->fg = 8;
			break;
		case 40:
		case 41:
		case 42:
		case 43:
		case 44:
		case 45:
		case 46:
		case 47:
			gc->bg = n - 40;
			break;
		case 49:
			gc->bg = 8;
			break;
		case 90:
		case 91:
		case 92:
		case 93:
		case 94:
		case 95:
		case 96:
		case 97:
			gc->fg = n;
			break;
		case 100:
		case 101:
		case 102:
		case 103:
		case 104:
		case 105:
		case 106:
		case 107:
			gc->bg = n - 10;
			break;
		}
	}
}

/* DCS terminator (ST) received. */
int
input_dcs_dispatch(struct input_ctx *ictx)
{
	const char	prefix[] = "tmux;";
	const u_int	prefix_len = (sizeof prefix) - 1;

	if (ictx->flags & INPUT_DISCARD)
		return (0);

	log_debug("%s: \"%s\"", __func__, ictx->input_buf);

	/* Check for tmux prefix. */
	if (ictx->input_len >= prefix_len &&
	    strncmp(ictx->input_buf, prefix, prefix_len) == 0) {
		screen_write_rawstring(&ictx->ctx,
		    ictx->input_buf + prefix_len, ictx->input_len - prefix_len);
	}

	return (0);
}

/* OSC string started. */
void
input_enter_osc(struct input_ctx *ictx)
{
	log_debug("%s", __func__);

	input_clear(ictx);
}



typedef enum { 
  OSC52_UNKNOWN = -1,
  OSC52_SELECT  =  0,
  OSC52_PRIMARY,
  OSC52_CLIPBOARD, 
  OSC52_CUT_BUFFER0,
  OSC52_CUT_BUFFER1,
  OSC52_CUT_BUFFER2,
  OSC52_CUT_BUFFER3,
  OSC52_CUT_BUFFER4,
  OSC52_CUT_BUFFER5,
  OSC52_CUT_BUFFER6,
  OSC52_CUT_BUFFER7,  
  OSC52_target_count 
} osc52_target_t ;

typedef struct {
  char c1 ;
  char c2 ;
  const char * name ;
} osc52_target_info_t ;


// Describe each OSC52 target. 
// The order should match the osc52_target_t enum.
const osc52_target_info_t osc52_target_info[OSC52_target_count] = 
{
  { 's', 'S', "SELECT"      },
  { 'p', 'P', "PRIMARY"     },
  { 'c', 'C', "CLIPBOARD"   },
  { '0', '0', "CUT_BUFFER0" },
  { '1', '1', "CUT_BUFFER1" },
  { '2', '2', "CUT_BUFFER2" },
  { '3', '3', "CUT_BUFFER3" },
  { '4', '4', "CUT_BUFFER4" },
  { '5', '5', "CUT_BUFFER5" },
  { '6', '6', "CUT_BUFFER6" },
  { '7', '7', "CUT_BUFFER7" }  
} ;

/* Convert a target character used in OSC52 into a osc52_target_t enum value.
 * Return OSC52_UNKNOWN if character is unknown.
 * Remark: character 's' should be handled separately.
 */
static osc52_target_t 
osc52_char_to_target( char c )
{
  int i ; 
  for (i=0;i<OSC52_target_count;i++) {
    if (osc52_target_info[i].c1 == c || osc52_target_info[i].c2 == c) 
      return (osc52_target_t) i ;
  }
  return OSC52_UNKNOWN ;
}


#define BASE64_BAD_CHAR   ((unsigned int)-1)

unsigned int base64_char2bits(char c)
{
  if (c>='A' && c<='Z') 
    return c-'A'+0 ;
  else if ( c>='a' && c<='z')  
    return c-'a'+26 ;
  else if ( c>='0' && c<='9')  
    return c-'0'+52 ;
  else if (c=='+')
    return 62 ;
  else if (c=='/')
    return 63 ;
  else 
    return BASE64_BAD_CHAR ;
}

#
/* Compute the size required to base64 decode the input_size characters at src[] 
 *
 */
size_t base64_decode_size(const char *src, size_t input_size ) {

  size_t i ;
  size_t count = 0  ;
  size_t size   ;

  /* Count the significant characters.
   * Ignore all other characters (as Xterm does)
   */
  for ( i=0; i<input_size ; i++) {
    char c = src[i] ;
    if (  (c>='A' && c<='Z') ||
          (c>='a' && c<='z') ||
          (c>='0' && c<='9') ||
          (c=='+') ||
          (c=='/')
          )
      count++ ;      
  }

  /* 3 output bytes per group of 4 input characters */
  size = ( count / 4 ) * 3 ; 

  /* And each of the remaining input characters provides 6 bytes 
   *   + 0 character  = 0  bits = +0 bytes 
   *   + 1 character  = 6  bits = +0 bytes 
   *   + 2 characters = 12 bits = +1 bytes 
   *   + 3 characters = 18 bits = +2 bytes 
   */  
  size += ( (count % 4) * 6) / 8  ; 

  return size ; 

}

/* Base64 decode srclength characters at src. 
 *
 * 
 */
size_t base64_decode(const char *src, size_t srclength, uint8_t *dest, size_t destlength ) {


  size_t off = 0 ;
  size_t i ;

  unsigned int state ;  
  int sig  ;  // number of significant bits in state

  state = 0 ;
  sig   = 0 ;
  for (i=0 ; i<srclength ;i++) {
    char c = *src++ ;
    unsigned int val;    
    
    if (c>='A' && c<='Z') 
      val =  c-'A'+0 ;
    else if ( c>='a' && c<='z')  
      val =  c-'a'+26 ;
    else if ( c>='0' && c<='9')  
      val =  c-'0'+52 ;
    else if (c=='+')
      val = 62 ;
    else if (c=='/')
      val = 63 ;
    else 
      continue ;
    
    state <<= 6 ;
    state |= val ;
    sig += 6 ;

    //fprintf(stderr, "%08x  (%04x)\n",state,val);

    if (sig>=8) {
      // Consume 8 bits from state
      if (off>=destlength) 
        break ;

      //fprintf(stderr, " ==> %x \n",(uint8_t) state );

      dest[off] = (uint8_t) ( state >> (sig-8) ) ;
      off++ ;
      sig -= 8 ;

    }        
  }
  
  return off ;
  
  
}


/* Process a client OSC52 sequence to manipulate the buffers  
 *  OSC 52 ; p1 ; p2  
 *    
 * Each character in p1 describes a target buffer (see osc52_target_info).
 * if p2 is empty then the specified buffer (or buffers) is cleared 
 * else if p2 is '?'   then reply with the content of the 1st of the specified buffer that currently exit 
 *       ==> Reminder: PRIMARY and CLIPBOARD may not exist but the CUT_BUFFER are always present.
 * else p2 shall contain the base64 encode of the text to be placed in those buffers
 *
 */
static void 
process_osc52(struct input_ctx *ictx, u_char *args)
{
  u_char *sep ;
  u_char *p1 ;
  u_char *p2 ;

  log_debug("%s", __func__);

  /* Find the second ';' in 'args' and split it to create two well-formed C-strings for target and data  */ 
  sep = (u_char*) strchr( (char*)args,';') ;
  if (!sep) {        
    return ;  
  }
  *sep = '\0' ;
  p1 = args  ;
  p2 = sep+1 ;
  
  /* TODO: parse target */

  if (strcmp(p2,"?")==0) {
    // get-selection 
    // TODO

    const char * bufname ;
    struct paste_buffer * pb ; 

    /* The answer must use the same OSC termination that 
     * the query. That is either \a or \e\\ 
     *
     */
    const char * tail = (ictx->ch=='\a') ? "\a" : "\e\\" ;

    pb = paste_get_top(&bufname) ;

    if (pb==NULL) 
      {        
        input_reply(ictx, "\e]52;%s;%s",p1,tail);
      }
    else
      {
        size_t off;
    
        size_t bufsize ;
        const char * bufdata = paste_buffer_data(pb, &bufsize);

          
        size_t b64_size = 4 * ((bufsize + 2) / 3) + 1; /* storage size for base64 */
        char * b64_data = xmalloc(b64_size);
        
        b64_ntop(bufdata, bufsize, b64_data, b64_size);
        
        input_reply(ictx, "\e]52;%s;%s%s",p1,b64_data,tail);
        
        free(b64_data);
      }



    
  } else {
    // set-selection 

    int            has_select = 0 ;  
    osc52_target_t targets[OSC52_target_count+1] ;     /* ordered list of targets without duplicates */
    int            present[OSC52_target_count] = {0} ; 
    int            nt = 0 ;
    int            i ;

    log_debug("%s: '%s' '%s'", __func__, p1, p2);

    if ( *p1=='\0' )
      p1 = "s0" ;  /* the default target specification */

    /* Build a list of targets */
    for ( ; *p1 != '\0' ; p1++ )  {     
      osc52_target_t tgt = osc52_char_to_target(*p1) ;
      /* Ignore unknown characters */
      if (tgt==OSC52_UNKNOWN)
        continue ;    
      /* Ignore duplicate targets */
      if (present[tgt]) 
        continue ; 
      /* add to list of targets */
      present[tgt]  = 1 ; 
      targets[nt++] = tgt ;
    }
    log_debug("%s: %d",__func__,__LINE__);

    size_t size = base64_decode_size(p2,strlen(p2)) ;

    log_debug("%s: %d  %ld",__func__,__LINE__,(long int)size);

    char * data = xmalloc(size+1) ;

    log_debug("%s: %d",__func__,__LINE__);

    log_debug("%s: Decoding base64 message of %d bytes",__func__, (int)size);

    size_t size2 = base64_decode(p2, strlen(p2), data, size ) ;    
    log_debug("%s: success",__func__ );

    if (size2!=size) {
      log_debug("%s: Unexpected errror during base64 decoding (%d != %d) \n", __func__ , (int)size, (int)size2);
      free(data) ;
    }

    data[size] = '\0' ;

    /* Broadcast the clipboad to the terminal if set-clipboard option is activated.
     * TODO: Do we want to do that systematically or only for some targets and if so, which ones?
     *       For now, let's do it if either S, P or C is requested.
     *       That could become configurable.
     */
    if ( options_get_number(global_options, "set-clipboard")) {
      if ( present[OSC52_SELECT] || present[OSC52_CLIPBOARD] || present[OSC52_PRIMARY] ) {
        struct screen_write_ctx ctx;
        screen_write_start(&ctx, ictx->wp, NULL);
        screen_write_setselection(&ctx, data, size);
        screen_write_stop(&ctx);
      }
    }


    /* Process the list of targets in reverse order. 
     * The reason is that the buffer for the first specified target should always 
     * end up on top of the buffer list.
     */
    
    for (i=nt-1 ; i>=0 ; i--) 
    {
      osc52_target_t  tgt= targets[i] ; 
      const osc52_target_info_t * tinfo = &osc52_target_info[tgt] ;

      /* SELECT will be handled last as an automatic buffer so that he always ends up on top */       
      if (tgt==OSC52_SELECT) 
        continue ; 

      if (size==0) {
        /* delete the buffer when given empty data */ 
        struct paste_buffer *pb = paste_get_name( tinfo-name ) ;
        if (pb) 
          paste_free(pb) ;  
        log_debug("FREE %s",tinfo->name);
          
      } else {        
        /* Each paste buffer must own it data so make a copy*/
        char * pb_data =  (char*) xmalloc(size) ; 
        memcpy(pb_data,data,size) ;        
        paste_set( pb_data, size, tinfo->name , NULL ) ;   
        log_debug("SET %s %d",tinfo->name,(int)size);   
      }     
    }

    log_debug("Hello");

#if 1
    if ( present[OSC52_SELECT] ) {  
      log_debug("ADD %d",(int)size);
      paste_add(data,size) ;        
    } else {
      free(data) ;
    }
#endif
  }

}

/* OSC terminator (ST) received. */
void
input_exit_osc(struct input_ctx *ictx)
{
	u_char	*p = ictx->input_buf;
	u_int	 option;

	if (ictx->flags & INPUT_DISCARD)
		return;
	if (ictx->input_len < 1 || *p < '0' || *p > '9')
		return;

	log_debug("%s: \"%s\"", __func__, p);

	option = 0;
	while (*p >= '0' && *p <= '9')
		option = option * 10 + *p++ - '0';
	if (*p == ';')
		p++;

	switch (option) {
	case 0:
	case 2:
		screen_set_title(ictx->ctx.s, p);
		server_status_window(ictx->wp->window);
		break;
	case 12:
		if (*p != '?') /* ? is colour request */
			screen_set_cursor_colour(ictx->ctx.s, p);
		break;
	case 112:
		if (*p == '\0') /* no arguments allowed */
			screen_set_cursor_colour(ictx->ctx.s, "");
		break;
        case 52:
                process_osc52(ictx,p) ;                
                break ;
	default:
		log_debug("%s: unknown '%u'", __func__, option);
		break;
	}
}

/* APC string started. */
void
input_enter_apc(struct input_ctx *ictx)
{
	log_debug("%s", __func__);

	input_clear(ictx);
}

/* APC terminator (ST) received. */
void
input_exit_apc(struct input_ctx *ictx)
{
	if (ictx->flags & INPUT_DISCARD)
		return;
	log_debug("%s: \"%s\"", __func__, ictx->input_buf);

	screen_set_title(ictx->ctx.s, ictx->input_buf);
	server_status_window(ictx->wp->window);
}

/* Rename string started. */
void
input_enter_rename(struct input_ctx *ictx)
{
	log_debug("%s", __func__);

	input_clear(ictx);
}

/* Rename terminator (ST) received. */
void
input_exit_rename(struct input_ctx *ictx)
{
	if (ictx->flags & INPUT_DISCARD)
		return;
	if (!options_get_number(ictx->wp->window->options, "allow-rename"))
		return;
	log_debug("%s: \"%s\"", __func__, ictx->input_buf);

	window_set_name(ictx->wp->window, ictx->input_buf);
	options_set_number(ictx->wp->window->options, "automatic-rename", 0);

	server_status_window(ictx->wp->window);
}

/* Open UTF-8 character. */
int
input_utf8_open(struct input_ctx *ictx)
{
	struct utf8_data	*ud = &ictx->utf8data;

	if (utf8_open(ud, ictx->ch) != UTF8_MORE)
		fatalx("UTF-8 open invalid %#x", ictx->ch);

	log_debug("%s %hhu", __func__, ud->size);

	return (0);
}

/* Append to UTF-8 character. */
int
input_utf8_add(struct input_ctx *ictx)
{
	struct utf8_data	*ud = &ictx->utf8data;

	if (utf8_append(ud, ictx->ch) != UTF8_MORE)
		fatalx("UTF-8 add invalid %#x", ictx->ch);

	log_debug("%s", __func__);

	return (0);
}

/* Close UTF-8 string. */
int
input_utf8_close(struct input_ctx *ictx)
{
	struct utf8_data	*ud = &ictx->utf8data;

	if (utf8_append(ud, ictx->ch) != UTF8_DONE) {
		/*
		 * An error here could be invalid UTF-8 or it could be a
		 * nonprintable character for which we can't get the
		 * width. Drop it.
		 */
		return (0);
	}

	log_debug("%s %hhu '%*s' (width %hhu)", __func__, ud->size,
	    (int)ud->size, ud->data, ud->width);

	utf8_copy(&ictx->cell.cell.data, ud);
	screen_write_cell(&ictx->ctx, &ictx->cell.cell);

	return (0);
}
