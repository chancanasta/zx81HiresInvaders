;our variables
hitcnt	DEFB	$00
maxicol	DEFB	$00
minicol	DEFB	$00
firedwn	DEFB	$00
lives	DEFB	$00
maxinvx DEFB	$00
mininvx	DEFB	$00
winvmax	DEFB	$00
winvmin	DEFB	$00
noacti	DEFB	$00
score	DEFW	$0000
hiscore DEFW	$0000
ihitrow DEFB	$00
lowrow	DEFB	$00

exitlp	DEFB	$00
	
ibulctm DEFB	$00	
invwait DEFB	$00
invxpos	DEFB	$00
invxchk DEFB	$00
invypos	DEFB	$00
invdir	DEFB	$00
invdown	DEFB	$00
invystr	DEFB	$00

tmpvar	DEFB	$00

shldpos DEFW	$0000
basex	DEFB	$00
baseg	DEFB	$00
baseexp	DEFB	$00
basepos	DEFW	$0000
baselin	DEFW	$0000
scrlin	DEFW	$0000
skip	DEFB	$00
alldone	DEFB	$fe
bullact	DEFB	$00
bullpos	DEFW	$0000
bully	DEFB	$00
bullx	DEFB	$00
bullgr	DEFW	$0000
bulltp	DEFB	$00

ivpos1	DEFW	$0000


ivgrp1	DEFW	$0000
ivgrp2	DEFW	$0000
ivgrp3	DEFW	$0000

ivcnt	DEFB	$00

instruct1
		DEFB	_Z,_X,_8,_1,_N,_V,_A,_D,_E,_R,_S,$ff
instruct2
		DEFB	_K,_E,_Y,_S,_CL,__,_5,__,_L,_E,_F,_T,_CM,_8,__,_R,_I,_G,_H,_T,_CM,_S,_P,_C,__,_F,_I,_R,_E,$ff
instruct3
		DEFB	_P,_R,_E,_S,_S,__,_1,__,__,_T,_O,__,_S,_T,_A,_R,_T,$ff
instruct4
		DEFB	_OP,_C,_CP,__,_B,_R,_I,_A,_N,__,_L,_E,_W,_I,_S,__,_2,_0,_1,_3,$ff

lives_txt
		DEFB	_L,_I,_V,_E,_S,$ff
score_txt
		DEFB 	_S,_C,_O,_R,_E,$ff
hiscore_txt
		DEFB	_H,_I,_S,_C,_O,_R,_E,$ff
		

vsynclp	DEFB	$00		
;state of invaders
invst	DEFB	1,1,1,1,1,1
		DEFB	1,1,1,1,1,1
		DEFB	1,1,1,1,1,1
		DEFB	1,1,1,1,1,1
		DEFB	1,1,1,1,1,1

invstp	DEFW	$0000

;mothership
imsact	DEFB	$00
imshipx	DEFB	$00
imspos	DEFW	$0000
imcnt	DEFB	$00
imbaseg	DEFB	$00
imdir	DEFB	$00
imxpl	DEFB	$00

;invaders bullets
ibulact	DEFB	$00
ibulx	DEFB	$00
ibuly	DEFB	$00
ibulpos	DEFW	$0000
ibulcnt	DEFB	$00
ibulcol DEFB	$00

ibulact1 DEFB	$00
ibulx1	 DEFB	$00
ibuly1	 DEFB	$00
ibulpos1 DEFW	$0000
ibulcnt1 DEFB	$00
ibulcol1 DEFB	$00

ibulact2 DEFB	$00
ibulx2	 DEFB	$00
ibuly2	 DEFB	$00
ibulpos2 DEFW	$0000
ibulcnt2 DEFB	$00
ibulcol2 DEFB	$00

ilowrow	DEFB	$04

;bullets
bull1
		DEFB $06		
bull2
		DEFB $2b
bull3
		DEFB $05		
bull4
		DEFB $bb
;invaders
inv11
            DEFB $13,$22
            DEFB $39,$22
            DEFB $80,$22
            DEFB $14,$2b
            DEFB $ab,$2b
            DEFB $02,$22
            DEFB $18,$22
            DEFB $02,$2b
inv12
            DEFB $22,$00
            DEFB $81,$00
            DEFB $03,$3a
            DEFB $06,$21
            DEFB $ba,$3a
            DEFB $06,$00
            DEFB $23,$bb
            DEFB $06,$02
inv21
            DEFB $05,$2b
            DEFB $0c,$22
            DEFB $80,$2b
            DEFB $9e,$00
            DEFB $a2,$00
            DEFB $a9,$05
            DEFB $05,$05
            DEFB $03,$22
						
inv22
            DEFB $06,$bb
            DEFB $11,$02
            DEFB $03,$01
            DEFB $13,$01
            DEFB $03,$01
            DEFB $03,$3a
            DEFB $06,$bb
            DEFB $23,$23

inv31
            DEFB $03,$22
            DEFB $ab,$00
            DEFB $a2,$3a
            DEFB $91,$10
            DEFB $a2,$3a
            DEFB $16,$00
            DEFB $1f,$05
            DEFB $05,$00
inv32
            DEFB $22,$3a
            DEFB $ba,$01
            DEFB $03,$a2
            DEFB $13,$92
            DEFB $03,$a2
            DEFB $81,$12
            DEFB $03,$84
            DEFB $13,$03

bomb1

            DEFB $22,$22
            DEFB $23,$22
            DEFB $bb,$22
            DEFB $23,$22
            DEFB $06,$22
            DEFB $23,$22
            DEFB $bb,$22
            DEFB $23,$22

bomb2
			DEFB $22,$22
            DEFB $22,$29
            DEFB $22,$2b
            DEFB $22,$29
            DEFB $22,$05
            DEFB $22,$29
            DEFB $22,$2b
            DEFB $22,$29

shield
            DEFB $03,$a2,$00
            DEFB $80,$a2,$00
            DEFB $80,$a2,$3a
            DEFB $ab,$a2,$3a
            DEFB $a2,$a2,$01
            DEFB $a2,$a2,$01
            DEFB $a2,$a2,$01
            DEFB $a2,$a2,$01
            DEFB $a2,$a2,$01
            DEFB $a2,$a2,$01
            DEFB $a2,$a2,$01
            DEFB $a2,$a2,$01
            DEFB $01,$03,$01
            DEFB $01,$81,$01
            DEFB $3a,$22,$01
            DEFB $3a,$22,$01
rubout
			DEFB $22,$22
			DEFB $22,$22
			DEFB $22,$22
			DEFB $22,$22
			DEFB $22,$22
			DEFB $22,$22
			DEFB $22,$22
			DEFB $22,$22
			
expl1
            DEFB $bb,$2b
            DEFB $17,$02
            DEFB $05,$05
            DEFB $02,$29
            DEFB $00,$12
            DEFB $02,$29
            DEFB $05,$05
            DEFB $21,$2b
		
;'us'
base1		
			DEFB $06,$22,$22
            DEFB $ba,$22,$22
            DEFB $ba,$22,$22
            DEFB $ab,$3a,$22
            DEFB $a2,$3a,$22
            DEFB $a2,$3a,$22
            DEFB $a2,$3a,$22
            DEFB $a2,$3a,$22
;same graphic, 2bits to the right			
base2
            DEFB $22,$2b,$22
            DEFB $81,$00,$22
            DEFB $81,$00,$22
            DEFB $80,$01,$22
            DEFB $80,$01,$22
            DEFB $80,$01,$22
            DEFB $80,$01,$22
            DEFB $80,$01,$22
;same graphic, 4bits to the right			
base3
            DEFB $22,$05,$22
            DEFB $22,$10,$22
            DEFB $22,$10,$22
            DEFB $ba,$a2,$22
            DEFB $03,$a2,$2b
            DEFB $03,$a2,$2b
            DEFB $03,$a2,$2b
            DEFB $03,$a2,$2b
;same graphic, 6bits to the right			
base4
            DEFB $22,$bb,$22
            DEFB $22,$88,$22
            DEFB $22,$88,$22
            DEFB $81,$a2,$00
            DEFB $03,$a2,$00
            DEFB $03,$a2,$00
            DEFB $03,$a2,$00
            DEFB $03,$a2,$00
;mother ship
mother1
            DEFB $ba,$00,$22
            DEFB $80,$3a,$22
            DEFB $80,$01,$22
            DEFB $14,$89,$22
            DEFB $a2,$a2,$22
            DEFB $16,$88,$22
            DEFB $02,$bb,$22
mother2
            DEFB $81,$3a,$22
            DEFB $ba,$01,$22
            DEFB $03,$a2,$22
            DEFB $03,$14,$2b
            DEFB $80,$a2,$00
            DEFB $13,$92,$22
            DEFB $23,$06,$22
mother3
            DEFB $22,$01,$22
            DEFB $81,$a2,$2b
            DEFB $03,$a2,$00
            DEFB $06,$0f,$05
            DEFB $03,$a2,$3a
            DEFB $03,$11,$00
            DEFB $81,$22,$2b

mother4
            DEFB $22,$80,$2b
            DEFB $22,$ab,$00
            DEFB $22,$a2,$3a
            DEFB $81,$89,$21
            DEFB $03,$a2,$01
            DEFB $22,$91,$10
            DEFB $22,$29,$05

