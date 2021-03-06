;exciting EQUS for this game
DISPLEN			EQU $21
BLANK_BITS		EQU	$22
FULL_BITS		EQU $A2
HIRES_IDX		EQU	18
MAX_X			EQU	238
MIN_X			EQU	0
BULL_INIT		EQU 126

;delay before demo starts
DEMO_WAIT		EQU	10000
DEMO_KEYCNT		EQU	28


;invaders bomb speed increase
INV_BOMB_TRIG	EQU	4
INV_BOMB_DEC	EQU 4

MOVE_RIGHT		EQU	0
MOVE_LEFT		EQU	1

INVCOL_WIDTH	EQU	6

INVROWS			EQU	5
INVCOLS			EQU 6
INVSPACING		EQU	22


MAXINVX			EQU $18
MININVX			EQU $0

COLOFFSET		EQU	5

MOVEUPBY		EQU	4
BULHEIGHT		EQU 4

BOMBMOVEBY		EQU	2

MOVEDOWNBY		EQU	4
INVHEIGHT		EQU	16
MAXBOMBY		EQU 128
IBULCNTM		EQU	32

INVASIONY		EQU	54

MSHIPCNT		EQU 246
MSHIENDX		EQU 228
MSHISTX			EQU 28
MSHISTXPOS		EQU 224
MSHIXPL			EQU 4

BASEEXPCNT		EQU 16
;exit 
ALL_OK			EQU	0
LOST_LIFE		EQU	1
INVADED			EQU	2
CLEARED_SHEET	EQU	3
QUIT_GAME		EQU 4
NXT_CLEARED		EQU 5
FIRST_LOOP		EQU	6
FIRST_PAUSE		EQU	20
;BCD - lazy score display
BCD_10			EQU	16
BCD_20			EQU 32
BCD_30			EQU 48
BCD_40			EQU 64
BCD_50			EQU 80
BCD_100			EQU 256
BCD_200			EQU 512
BCD_300			EQU 768