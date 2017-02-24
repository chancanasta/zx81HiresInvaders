;
; To assembly this, either use the zxasm.bat file:
;
; zxasm invaders
;
; or... assemble with the following options:
;
; tasm -80 -b -s invaders.asm invaders.p
;
;==============================================
;    'Hi Res' Space Invaders for the ZX81
;==============================================
;
;defs
#include "zx81defs.asm"
;EQUs for ROM routines
#include "zx81rom.asm"
;ZX81 char codes/how to survive without ASCII
#include "charcodes.asm"
;system variables
#include "zx81sys.asm"

;the standard REM statement that will contain our 'hex' code
#include "line1.asm"
;a bunch of useful EQUs
#include "equs.asm"

;------------------------------------------------------------
; code starts here and gets added to the end of the REM 
;------------------------------------------------------------
;restart the hi score here - then never mention it again
	ld hl,0
	ld (hiscore),hl
	
;instructions
restart
	call instruct	

;switch to hires	
	call hires


;set everything up
	call init1up
past1up	
	call initvars
	call hirescls
	
	call drawstatic
;draw live,score and hiscore static
	
;draw the 'statics' 	
shieldstart
	call drawshields
oneupstart
	call displives
	call disphiscore
	ld a,FIRST_LOOP
	ld (exitlp),a
;then - onto the main loop	
mainlp		
	call dispscore
;read the keys
	call readkeys
;draw the player	
	call drawp1
;draw the invaders	
	call drawinv
;draw the mothership	
	call mothership
;then the bombs	
	call invbombs
	
;pause the first time around
	ld a,(exitlp)
	cp FIRST_LOOP
	jr nz,notfirstpause
	ld a,FIRST_PAUSE
	jr infirstloop
notfirstpause:	
;vsync - well, realy a timed delay to help to avoid flicker
	ld a,(vsynclp)
infirstloop	
	ld b,a
pauselp	
	call vsync
	djnz pauselp
;check for exit condition, if none - continue with the main loop	
	ld a,(exitlp)
	cp ALL_OK
	jr z,mainlp
;first loop - pause slightly before the game starts	
	cp FIRST_LOOP
	jr nz,notfirstloop
	ld b,FIRST_PAUSE
floop
	call vsync
	djnz floop
	ld a,ALL_OK
	ld (exitlp),a
	jr mainlp
notfirstloop

	cp NXT_CLEARED
	jr nz,notnxtcleared
	ld a,CLEARED_SHEET
	ld (exitlp),a
	jr mainlp
notnxtcleared	
;exitlp should contain what happened
;this is a jump to the end of all this for debug purposes
;	jr endall
	
	cp LOST_LIFE
	jr nz,notlostlife
	call rubbase
	
;decrease lives	
	ld a,(lives)
	dec a
	ld (lives),a
	cp $0
	jr z,endgame
;some lives left, so jump back	
	ld a,ALL_OK
	ld (exitlp),a
	jr oneupstart
notlostlife	
	cp CLEARED_SHEET
	jr nz,notcleared
;cleared the sheet, wait a bit	
	ld b,20
clearwait
	call vsync
	djnz clearwait
;move invaders down
	call startlower
	jp past1up

notcleared
endgame
	ld b,20
endwait
	call vsync
	djnz endwait

endall
;exit the main loop - switch back to low res	
	call lores
	call cls
;see if we've got a new hiscore	
	ld hl,(hiscore)
	ld bc,(score)
;compare the MSB of the score
	ld a,h
	cp b
;if the score MSB is larger than the hiscore MSB - then it must be a new high score	
	jr c,newhs
;the score MSB is not larger than the hiscore, check if it's the same	
	jr nz,notnewhs
;score MSB is same as hiscore MSB, check LSB	
	ld a,l
	cp c
;if the LSB is not larger, no new hiscore, skip the hiscore copy	
	jr nc,notnewhs
newhs
	ld (hiscore),bc
notnewhs	
	
	
	
;comment out the line below for debug purposes	
	jp restart
;
;DEBUG CODE
; the following is never called outside of debug
;
	
	
;display a bunch of numbers that I was interested in at some point or other	
	ld	hl,(D_FILE)
	ld	bc,(DISPLEN*3)+1
	add hl,bc
	
	
	push hl
	ld a,(invypos)
	call dispno
	pop	hl

	ld	bc,DISPLEN
	add	hl,bc
	
	push hl
	ld a,(lowrow)
	call dispno
	pop hl

;	ld	bc,DISPLEN
;	add	hl,bc
	
;	push hl
;	ld a,(ibulx1)
;	call dispno
;	pop hl
	
;	ld	bc,DISPLEN
;	add	hl,bc
	
;	push hl
;	ld a,(ibulx2)
;	call dispno
;	pop hl
	
	
;back to BASIC	
	ret
;
; END OF DEBUG Code
;
	
;subroutines
;clear screen
#include "cls.asm"

	
drawstatic
	ld bc,1
	ld de,lives_txt
	call dispstring
	
	ld bc,10
	ld de,score_txt
	call dispstring

	ld bc,21
	ld de,hiscore_txt
	call dispstring
	ret
;scores
incscore
	ld a,(score)
	add a,c
	daa 
	ld (score),a
	ld a,(score+1)
	adc a,b
	daa
	ld (score+1),a
	ret
	

dispscore
	ld bc,16
	ld de,score
	jr dispBCD
	
disphiscore
	ld bc,29
	ld de,hiscore
	
	
dispBCD	
	ld	hl,(D_FILE)
	add hl,bc
;lazy msb/lsb swap
	inc de
	ld a,(de)
;display the 4 BCD digits		
	push af
	and $f0
	rra
	rra
	rra
	rra
	add	a,$1c
	ld (hl),a
	inc hl
	pop af
	and $0f
	add a,$1c
	ld (hl),a
	inc hl
	
	dec de
	ld a,(de)
	push af
	and $f0
	rra
	rra
	rra
	rra
	add	a,$1c
	ld (hl),a
	inc hl
	pop af
	and $0f
	add a,$1c
	ld (hl),a		
	ret
;lives
displives
	push hl
	push bc
	ld	hl,(D_FILE)
	ld	bc,7
	add hl,bc
		
		
	ld a,(lives)
	call dispno
	pop bc
	pop	hl
ret

;reset
rubbase
	ld a,BLANK_BITS
	ld hl,(basepos)
rubstart	
	ld b,8	
rublp1	
	push bc
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	ld bc,31
	add hl,bc
	pop bc
	djnz rublp1
	
;lost a life, reset stuff	
	xor a
	ld (basex),a
	ld (baseg),a
	ld (baseexp),a
	ld (bullact),a
	ld hl,HDISPLAY
	ld de,DISPLEN*(8+64+54)
	add hl,de
	ld (basepos),hl
	ret
	
#include "mothership.asm"
	
	
;invaders bombs
invbombs
;be lazy - it's quicker to copy 7 bytes in and out of
;a single memory location than to indirectly address everything
	call cpyinbul1
	call invbomb
;copy the bullet vars back 'out'
	call cpyoutbul1	

	call cpyinbul2
	call invbomb
;copy the bullet vars back 'out'
	call cpyoutbul2


	ret
	
invbomb
	call startibul
;check for inv bullets
	ld a,(ibulact)
	cp $1
	jr nz,ibullnotact
	call drawibull
ibullnotact	
	ld a,(bullact)
	cp $1
	jr nz,bullnotact
	call drawbull
bullnotact
	ret

;copy invaders bullets var 1 in
cpyinbul1
;from hl to de	
	ld hl,ibulcol1
	ld de,ibulcol
	jr sibullp

;copy invaders bullets var 2 in	
cpyinbul2
;from hl to de	
	ld hl,ibulcol2
	ld de,ibulcol
	jr sibullp

;copy invaders bullets var 1 out
cpyoutbul1
;from hl to de	
	ld hl,ibulcol
	ld de,ibulcol1
	jr sibullp

;copy invaders bullets var 2 out	
cpyoutbul2
;from hl to de	
	ld hl,ibulcol
	ld de,ibulcol2
	
sibullp		
	ld bc,7
	lddr

	ret

startibul
	ld a,(ibulact)
	cp $0
	jp nz,skipibul

	
	ld a,(ibulctm)
	ld c,a
	ld a,(ibulcnt)
	inc a
	ld (ibulcnt),a
	cp c
	jp c,skipibul
	xor a
	ld (ibulcnt),a

	ld a,(baseexp)
	cp 0
	jp nz,skipibul

	ld a,(invxpos)
	ld c,a
	
;see if base is to the left of the invaders block
;if it is, just pick the first row
	ld a,(basex)
	ld b,a
;before we do anything, check if the leftmost column is '0'	
	ld a,(minicol)
	cp $0
	jr z,minicolz
;it's non-zero - so do some maths	
;c still contains invxpos
	ld a,c
;need to shift to the right based on 'destroyed' columns
	push bc
	ld b,a
invxstart
	add a,INVSPACING
	djnz invxstart
	
	pop bc
	jr minicolz2
minicolz
	ld a,c
minicolz2
	cp b
	jr nc,basetoleft
;see if base is to the right of the invaders block
;if it is, just pick the last row
	push bc
;save the accumulator by loading to bc then switch the registers	
	ld bc,(maxicol)
	ld b,c	
	dec b	
;obviously, size of invaders block is dynamic - this is war after all	
;so loop through the number of 'still alive' columns to figure out the end point
invxend
	add a,INVSPACING
	djnz invxend
	
	pop bc
	cp b
	jr nc,pbase2
	
	ld a,(maxicol)
	cp INVCOLS
	jr nz,notFullCols
	dec a
notFullCols
	jr pbase

pbase2
;base is somewhere under the invaders, find closest column
	and a
;c still contains invxpos	
	ld a,c
	sbc a,COLOFFSET
	ld b,a
	ld a,(basex)
	sbc a,b
	ld d,a
	and a
	ld a,d
	ld d,0
bmbwrkx
	cp INVSPACING
	jr c,bmbix
	jr z,bmbix
	
	inc d
	sbc a,INVSPACING
	jr bmbwrkx	
bmbix	
;d contains invader column
	ld a,d
	jr pbase
	
basetoleft
	ld a,(minicol)
	
pbase	
	ld (ibulcol),a
;find the lowest 'active' invader in this column
;start of invaders on/off stuff
	ld hl,invst
;move down to the last invader in the fist column
;save c
	ld d,c
	ld bc,INVCOLS*(INVROWS-1)
	add hl,bc
;get c back	
	ld c,d
;move to the right column, a still contains column
	or a
	cp $0
	jr z,noicolmov	
	ld b,a
seticol
	inc hl
	djnz seticol
noicolmov	
;hl now points to the bottom invader in the matching column
;find an invader
	ld de,INVCOLS
	ld b,INVROWS
fndbmb
	ld a,(hl)
	cp $1
	jr z,fndlowi
	
	and a
	sbc hl,de
	
	djnz fndbmb
	ret
	
fndlowi	
	dec	b
	ld a,b
	ld (ilowrow),a
	
	
;start 8 lines below the invader	
	ld hl,(ivpos1)
	ld de,(DISPLEN*8)+1
	add hl,de
	
	ld a,(invypos)
	add a,8
	push af
;move over to the right column
	ld a,(ibulcol)
	cp $0
	jr z,nosetbcol
	ld b,a
	ld de,3
	ld a,c
setbcol
	add a,$20
	add hl,de
	djnz setbcol
	ld c,a
nosetbcol
	ld a,c
	ld (ibulx),a
	ld a,(ilowrow)
	ld b,a	
	cp $0
	jr z,ibulset
	
	

	pop af
	ld de,DISPLEN*INVHEIGHT	
setibul	
	add hl,de
	add a,INVHEIGHT
	djnz setibul
;make sure we don't 'pop' again
	jr ppopaf
ibulset	
;must be set to row 0, pop af
	pop af
ppopaf
	ld (ibulpos),hl
	ld (ibuly),a
	
	ld a,$1
	ld (ibulact),a
	
skipibul	
	ret
	
drawibull
	
	ld hl,(ibulpos)
	push hl	
;rub out old bomb
	ld c,BLANK_BITS
	ld b,BOMBMOVEBY
	ld de,DISPLEN-1
rubbomb
	ld (hl),c
	inc hl
	ld (hl),c
	add	hl,de
	djnz rubbomb

;move down
	pop	hl
	ld de,DISPLEN*BOMBMOVEBY
	add	hl,de
	ld (ibulpos),hl
	ld a,(ibuly)
	add a,BOMBMOVEBY
	
	ld (ibuly),a
;check if there's anything already there
	push hl
	ld de,DISPLEN*(4+BOMBMOVEBY)
	add hl,de
	ld a,(hl)
	inc hl
	ld b,(hl)
	pop hl
;if there, jump to the "hit the bottom" logic
	cp BLANK_BITS	
	jr nz,endibul
	ld a,b
	cp BLANK_BITS	
	jr nz,endibul
	

	
	ld de,bomb1
	ld b,8
;did we reach the bottom ?
	ld a,(ibuly)
	cp MAXBOMBY
	jr c,bmbnobot
	
endibul	
	ld a,(ibuly)
	cp MAXBOMBY
	jr nc,shortbomb

	cp 120
	jr c,shortbomb
	ld a,BASEEXPCNT
	ld (baseexp),a
	ret
shortbomb	
;we hit the bottom - clear the bomb	
	ld b,8
	ld a,BLANK_BITS
	ld de,DISPLEN-1
bombrblp
	ld (hl),a
	inc hl
	ld (hl),a
	add hl,de
	djnz bombrblp
	xor a
	ld (ibulact),a
	
	jr pdrawbmb
	
bmbnobot
;draw the damn bomb	
bomblp1
	push bc
	ld a,(de)
	ld (hl),a
	inc hl
	inc de
	ld a,(de)
	ld (hl),a
	inc de
	ld bc,DISPLEN-1
	add hl,bc
	
	pop bc
	djnz bomblp1
pdrawbmb	
	ret
	
	
invhit
	push af
	push bc
	push de
	push hl
;check for hitting the mother ship
	ld a,(bully)
	cp 9
	jr nc,nohitmoth
	call hitmship
	pop hl
	pop de
	pop bc
	pop af
	ret
nohitmoth	
;hit something - so do the "fire more often" thing

	xor a
;need to figure out which row we've hit - for scoring purposes	
	ld (ihitrow),a
;we didn't hit the mothership - check what we did hit
	ld hl,invst
	ld de,INVCOLS
;work out what (if anything) we hit
	and a
	ld a,(invypos)
	ld b,a
	ld a,(bully)
	sbc	a,b
	
	ld b,0
	
	ld c,a
	and a
	ld a,c
invwrky
	cp 16
	jr c,pasyiy
	inc b
	push af
;update which row we've hit	
	ld a,(ihitrow)
	inc a
	ld (ihitrow),a
	pop af
	add hl,de
	sbc a,16
	jr invwrky
pasyiy	
	ld a,b
	cp 5 		
	jp nc,noihit
;handle the left offset...	
	and a
	ld a,(invxpos)
	sbc a,COLOFFSET
	ld b,a
	ld a,(bullx)
	sbc a,b
	ld c,a
	and a
	ld a,c
	
invwrkx
	cp INVSPACING
	jr c,pasyix
	jr z,pasyix
	
	inc hl
	sbc a,INVSPACING
	jr invwrkx	
pasyix	

	ld a,(hl)
	cp $0
	jr z,noihit
	ld (hl),2
;score is based on which row we've hit
	ld a,(ihitrow)
	cp 0
	jr nz,nohitrow0
	ld bc,BCD_30
	jr goinscore
nohitrow0	
	cp 1
	jr nz,nohitrow1
	ld bc,BCD_20
	jr goinscore
nohitrow1	
	cp 2
	jr nz,nohitrow2
	ld bc,BCD_20
	jr goinscore
nohitrow2	
	cp 3
	jr nz,nohitrow3
	ld bc,BCD_10
	jr goinscore
nohitrow3		
	ld bc,BCD_10
goinscore	
	call incscore
;hit a invader - check min x/y boundaries
	call chkinvbd
;as the number of invaders decreases - increase the bombing rate
	ld a,(hitcnt)
	inc a
	ld (hitcnt),a
	cp INV_BOMB_TRIG
	jp nz,notmorefire
	xor	a
	ld (hitcnt),a
	ld a,(ibulctm)
	cp $0
	jr z,notmorefire
	and a
	sbc a,INV_BOMB_DEC
	ld (ibulctm),a
	
notmorefire
	
;keep count of number of invaders left	
	ld a,(noacti)
	dec a
	ld (noacti),a
	cp $0
	jr nz,noihit
;we have destroyed them all, so set the 'cleared' flag to trip after the loop	
	ld a,NXT_CLEARED
	ld (exitlp),a
;skip past speed checks	
	jr noihit5
;change the speed of the invaders based on the number left	
noihit
	cp	$15
	jr	nz,noihit2
	ld	a,4	
	jr noihit4a

noihit2	
	cp $10
	jr nz,noihit3
	ld a,2	
	jr noihit4
	
noihit3	
	cp	$3
	jr	nz,noihit5
	ld	a,1

noihit4
	ld (vsynclp),a		
noihit4a	
	ld (invwait),a
noihit5	
	xor a
	ld	(ivcnt),a

	pop hl
	pop de
	pop bc
	pop af
	
	ret
;work out of the min/max of inv pos needs to be changed
chkinvbd	
	push af
	push bc
	push de
	push hl
	
;work out the right pos
;start with max
	ld a,MAXINVX
	ld (winvmax),a
	ld de,INVCOLS-1
;start checking at the last cell on the top row...
	ld hl,invst
	add hl,de
;increase de, so we can use it move down a row
	inc de
;column loop
	ld b,INVCOLS
hinvchk
;save column counter
	push bc
;save array pos as we'll move back 1 from here
	push hl	
;rows loop - read through every row in this column
	ld b,INVROWS
vinvchk	
;load an 'invader'
	ld a,(hl)
;is it active ?
	cp $1
;if it is - we're done	
	jr z,chkdone
;move down 1 row
	add hl,de
	djnz vinvchk

	
;got to here,so no invaders alive in this column, increase our 'max x position' var
	ld a,(winvmax)
	add a,INVCOL_WIDTH
	ld (winvmax),a
	
;loop back to the next column
;retrieve the array address
	pop hl
;move back one
	dec hl
;get column counter back
	pop bc
	
	djnz hinvchk
;jump past the pops - as we just did them
	jr pastchkd

chkdone
	pop hl
	pop bc
	
pastchkd
;
	ld a,(winvmax)
	ld (maxinvx),a
	ld a,b
	dec a
	ld (maxicol),a
	
	
;;;;
;work out the left pos
;start with min
	ld a,MININVX
	ld (winvmin),a
;;;	ld de,INVCOLS
;start checking at the first cell on the top row...
	ld hl,invst
;column loop
	ld b,INVCOLS
hinvchk2
;save column counter
	push bc
;save array pos as we'll move forward 1 from here
	push hl	
;rows loop - read through every row in this column
	ld b,INVROWS
vinvchk2
;load an 'invader'
	ld a,(hl)
;is it active ?
	cp $1
;if it is - we're done	
	jr z,chkdone2
;move down 1 row
	add hl,de
	djnz vinvchk2
	
;got to here,so no invaders alive in this column, decrease our 'min x position' var
	and a	
	ld a,(winvmin)
	sbc a,INVCOL_WIDTH
	ld (winvmin),a
	
;loop back to the next column
;retrieve the array address
	pop hl
;move forward one
	inc hl
;get column counter back
	pop bc
	djnz hinvchk2
;jump past the pops - as we just did them
	jr pastchkd2

chkdone2
	pop hl
	pop bc
pastchkd2

	ld a,(winvmin)
	ld (mininvx),a
;'invert' the column variable	
	and a
	ld a,7
	sbc a,b
	dec a
	ld (minicol),a

	
;;;	
	
	pop hl
	pop de
	pop bc
	pop af
	ret

;rub out invaders (for move down)
rubinv

	ld bc,invst
	ld (invstp),bc
	ld b,INVROWS
rboutlp	
	push bc
	ld hl,(ivpos1)
	ld a,b
	ld bc,DISPLEN*16
	cp $5
	jr nz,rbnotpos1	
	jr rbpastpos
rbnotpos1
	add hl,bc
	cp $4
	jr nz,rbnotpos2
	jr rbpastpos
rbnotpos2
	add hl,bc
	cp $3
	jr nz,rbnotpos3
	jr rbpastpos
rbnotpos3
	add hl,bc
	cp $2
	jr nz,rbnotpos4	
	jr rbpastpos
rbnotpos4
	add hl,bc
rbpastpos	
	
	ld b,INVCOLS
rbinvrowlp
	push bc
	push de
	ld bc,(invstp)
	ld a,(bc)
	inc bc
	ld (invstp),bc
	
	cp $0
	jr z,rbnodrawi
	ld a,$22
	ld b,MOVEDOWNBY
	ld de,DISPLEN-2
rbinvlp1
	
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	
	
	add hl,de
	
;loop back for the 8 lines of the char
	
	djnz rbinvlp1
	ld de,DISPLEN*(8-MOVEDOWNBY)
	add hl,de
	
	jr rbpnodrawi

rbnodrawi
	ld bc,DISPLEN*8
	add hl,bc
rbpnodrawi
;loop back for the 10 invaders per row
	ld de,261
;there's no clear carry flag - so we and to clear it	
	and a
	sbc hl,de
	
	pop de
	pop bc
	
	djnz rbinvrowlp
	
;loop back for the 5 different rows of invaders	
	pop bc
	dec b
	jp nz,rboutlp	
	ret
	
;draw invaders
drawinv	
	ld a,(invdown)
	cp $1
	jr nz,notdown
	ld a,$0
	ld (invdown),a
	call rubinv
	ld hl,(ivpos1)
	ld de,DISPLEN*MOVEDOWNBY
	add hl,de
	ld (ivpos1),hl
	ld a,(invypos)
	add	a,MOVEDOWNBY
	ld (invypos),a
;check for an invasion	
;invasion y pos depends on lowest active invader
	ld hl,invst
;move to bottom right of invaders
	ld de,(INVCOLS*INVROWS)-1
	add hl,de	
;ok now move through the rows and cols to find the lowest active row
	ld b,INVROWS
;use d as a counter
	ld d,0
checklowrow	
	push bc
	ld b,INVCOLS
checklowcol
;get this 'invader'
	ld a,(hl)
;is there anything there
	cp $1
	jr nz,notsetinv
;found something, so this row is the lowest
	jr endicheck
notsetinv	
	dec hl
	djnz checklowcol
	
	pop bc
	inc d
	djnz checklowrow
	jr psafep
endicheck
	pop	bc
	
psafep
	ld a,d
	ld (lowrow),a
;check if all the rows are there - if so, skip all this stuff	
	ld a,d	
	cp $0
	jr z,lastrowchk
;not the last row, work out the offest	
	ld a,INVASIONY
	ld b,d
invloop
	add a,INVHEIGHT
	djnz invloop
	ld c,a
	jr plastrow
lastrowchk
	ld c,INVASIONY
plastrow
	ld a,(invypos)
	cp c
	jr c,notinvaded
	ld a,INVADED
	ld (exitlp),a
	
notinvaded

	
	
notdown
	ld bc,invst
	ld (invstp),bc
	ld b,INVROWS
outlp	
	push bc
	ld hl,(ivpos1)
	ld a,b
	ld bc,DISPLEN*16
	cp $5
	jr nz,notpos1	
	ld de,(ivgrp1)
	jr pastpos
notpos1
	add hl,bc
	cp $4
	jr nz,notpos2
	ld de,(ivgrp2)
	
	jr pastpos
notpos2
	add hl,bc
	cp $3
	jr nz,notpos3
	ld de,(ivgrp2)
	
	jr pastpos
notpos3
	add hl,bc
	cp $2
	jr nz,notpos4
	ld de,(ivgrp3)
	
	jr pastpos
notpos4
	add hl,bc
	ld de,(ivgrp3)
	
pastpos	
	
	ld b,INVCOLS
invrowlp
	push bc
	push de
	ld bc,(invstp)
	ld a,(bc)
	inc bc
	ld (invstp),bc
	
	cp $0
	jr z,nodrawi
	cp $2
	jr nz,noexp1
	ld de,expl1
	ld a,$3
	dec bc
	ld (bc),a
	inc bc
	jr noexp2	
noexp1	
	cp $3
	jr nz,noexp2
	ld de,rubout
	xor a
	dec	bc
	ld (bc),a
	inc bc
noexp2

	ld b,8	
	ld a,(invdir)
	cp	MOVE_LEFT
	jr	z,invlp1b
invlp1a
	push bc
;lazy - rub out the left
	ld a,$22
	ld (hl),a
	inc hl
	ld a,(de)
	ld (hl),a
	inc hl
	inc de
	ld a,(de)
	ld (hl),a
	inc de	
	ld bc,31
	add hl,bc
	pop bc
	djnz invlp1a
	jr pinvlp1b

invlp1b
	push bc
	inc hl
	ld a,(de)
	ld (hl),a
	inc hl
	inc de
	ld a,(de)
	ld (hl),a
	inc de
	inc hl
;lazy - rub out the right	
	ld (hl),$22
	ld bc,30
	add hl,bc
	pop bc
	djnz invlp1b
	
pinvlp1b
;loop back for the 8 lines of the char
	
	
	jr pnodrawi

nodrawi
	ld bc,DISPLEN*8
	add hl,bc
pnodrawi
;loop back for the 10 invaders per row
	ld de,261
;there's no clear carry flag - so we and to clear it	
	and a
	sbc hl,de
	
	pop de
	pop bc
	
	djnz invrowlp
	
;loop back for the 5 different rows of invaders	
	pop bc
	dec b
	jp nz,outlp	
;spin loop - to see if we move	
	ld a,(ivcnt)
	inc a
	ld b,a
	ld (ivcnt),a
	ld a,(invwait)
	cp b
	jp nz,noimove
	
;we're on the move	
;reset
	xor a	
	ld (ivcnt),a

	ld a,(baseexp)
	cp 0
	jp nz,noimove
	
;check xpos
	ld a,(invdir)
;save direction in c	
	ld c,a
;then figure out if it's left or right	
	cp MOVE_RIGHT
	jr z,goright
	and a
	ld a,(invxpos)	
	sbc a,4	
	ld b,a
	ld a,(invxchk)
	dec a
	jr pastgo
goright
	ld a,(invxpos)	
	add a,4
	ld b,a
	ld a,(invxchk)
	inc a
	
	
pastgo	

	ld hl,maxinvx
	cp (hl)
	jp z,hitright
	ld hl,mininvx
	cp (hl)
	jp z,hitleft	
	ld (invxchk),a
	ld a,b
	ld (invxpos),a
	
	
;get pos loop	
	ld a,(tmpvar)
	cp $1
	jr z,thing1
;0 - move to next graphic	
	ld a,c
	cp $0
	jr z,goright3
	ld hl,inv11
	ld de,inv21
	ld bc,inv31
	
	jr pastgo3
goright3	
	ld hl,inv12
	ld de,inv22
	ld bc,inv32
pastgo3
	
	ld a,$1
	jr thing2
thing1
;1 -  move pos and loop back to first graphic
	ld a,c
	cp $0
	jr z,goright2
	ld hl,(ivpos1)
	dec hl
	ld (ivpos1),hl

	ld hl,inv12
	ld de,inv22
	ld bc,inv32
	
	jr pastgo2
goright2	
	ld hl,(ivpos1)
	inc hl
	ld (ivpos1),hl

	ld hl,inv11
	ld de,inv21
	ld bc,inv31
	
pastgo2
	xor a
	
thing2	
	ld (ivgrp1),hl
	ld (ivgrp2),de
	ld (ivgrp3),bc
	ld (tmpvar),a
noimove	
	ret
hitright	

	ld a,1
	ld (invdown),a
	ld a,MOVE_LEFT
	ld (invdir),a
	ld a,(tmpvar)
	xor 1
	ld (tmpvar),a
	
	ret
hitleft
	ld a,1
	ld (invdown),a
	ld a,MOVE_RIGHT
	ld (invdir),a
	ld a,(tmpvar)
	xor 1
	ld (tmpvar),a
	
	ret

	
;draw bullet
drawbull
	ld hl,(bullpos)
	ld a,(bully)
	cp BULL_INIT
	jr z,norub
;rub out old bullet
	ld c,BLANK_BITS
	ld b,BULHEIGHT
	ld de,DISPLEN
rubbull
	ld (hl),c
	and a
	sbc hl,de
	djnz rubbull
;we've already moved up..
	jr pmoveup
norub
;move up	
	and a
	ld de,DISPLEN*MOVEUPBY
	sbc hl,de
pmoveup	
	ld (bullpos),hl
	
notopyxet	
	and a
	ld a,(bully)
	sbc a,MOVEUPBY
	ld (bully),a	
	cp 0
	jr nz,notopyet2
	xor a
	ld (bullact),a
	jr nobuldraw
notopyet2
	ld de,(bullgr)
	ld a,(de)
	ld c,a
	ld b,BULHEIGHT
bulllp1
;check if there's anything already there
	ld a,(hl)
;if it's the bullet graphic or blank - we've not hit anything	
	cp c	
	jr z,nobulhit
	cp BLANK_BITS
	jr z,nobulhit
;we've hit something...	
;make sure we we rub out the bullet
	ld hl,(bullpos)
	ld b,BULHEIGHT
	xor a	
	ld (bullact),a
	ld c,BLANK_BITS
	ld de,DISPLEN
;if it's an invader - DESTROY IT
	call invhit
	
bulhitrub	
	ld (hl),c
	and a
	sbc hl,de
	djnz bulhitrub
	jr nobuldraw
	
nobulhit	
	
	ld (hl),c
;there's no clear carry flag - so we and to clear it	
	and a
	ld de,DISPLEN
	sbc hl,de
	djnz bulllp1
	
nobuldraw
	
	ret
	
;draw player
drawp1
	ld hl,(basepos)

	ld a,(baseexp)
	cp $0
	jr z,notexp
	
	dec a
	ld (baseexp),a

	cp $0
	jr z,expalldone
	ld de,expl1
	inc hl
	
	ld b,8	
drwlp1x
	push bc
	ld b,2
drwlp2x
	ld a,(de)
	ld (hl),a
	inc hl
	inc de
	djnz drwlp2x
	ld bc,31
	add hl,bc
	pop bc
	djnz drwlp1x
	ret
	
expalldone
	ld a,LOST_LIFE
	ld (exitlp),a
	ret 
notexp	
	ld a,(baseg)
	cp $0
	jp nz,nobase1	
	ld de,base1
	jr drwstart
nobase1
	cp $1
	jp nz,nobase2
	ld de,base2
	jr drwstart
nobase2
	cp $2
	jp nz,nobase3
	ld de,base3
	jr drwstart
nobase3
	ld de,base4

drwstart	
	ld b,8	
drwlp1	
	push bc
	ld b,3
drwlp2	
	ld a,(de)
	ld (hl),a
	inc hl
	inc de
	djnz drwlp2
	ld bc,30
	add hl,bc
	pop bc
	djnz drwlp1	
	ret
	
	


;switch to high res
hires	
;wait for an interrupt
	halt 	
;wait for a vsync
	ld a,(FRAMES)
	ld c,a
sync1	
	ld a,(FRAMES)		
	cp c
	jr z,sync1
;replace the render routine		
	ld ix,hresgen
	ret
		
;switch back to low res
lores	
;wait for an interrupt
	halt 
;wait for a vsync
;	ld a,(FRAMES)
;	ld c,a
sync2	
;	ld a,(FRAMES)
;	cp c
;	jr z,sync2
;reset the I register to the ROM default		
	ld a,$1e
	ld i,a
;put back the normal display routine		
	ld ix,DISPROUT
	ret

;actual hires routine
hresgen
;slightly odd address - but it's basically 'back one line' from the start of the screen memory
;with bit 15 of the address set
	ld hl,(HDISPLAY - DISPLEN) + $8000
;set the line width	
	ld de,DISPLEN
;the ULA port address		
	ld c,$0FE
;offset within the ROM that gives us the best bit patterns for this game...
	ld a,HIRES_IDX
	ld i,a
;delay to sync with tv	
	ld b,5
syncx
	djnz syncx
;draw 136 lines of hires things
	ld b,$88
hreslp1	
;keep the ULA thinking it's on the first line of a character		
	in a,(c)
	out ($ff),a
	add hl,de
	call intoula	
	dec b
	jp nz,hreslp1
	ret nz
;back to lowres for the final few scanlines - we have to throw a few away to make everyhing sync, 
;but we can get 8 'good' lines in	
	ld hl,Display+$8000
	ld bc,$207
	ld a,$1e
	ld i,a
	ld a,$e4
;sneakily jump into a ROM routinte	
	call DISPLAY_5
	call exthlp

	ld a,$fd
	in a,($fe)
	ld e,a
	ld a,$7f
	in a,($fe)
	or e
	rra
	jr xthing1 
	ld iy,ERR_NR
	jp $229
	
intoula
	jp (hl)
xthing1
	ld b,$3c
xthing2
	djnz xthing2
	out ($ff),a
	ld ix,hresgen
	jr xthing3
exthlp
	pop ix
xthing3
	ld a,$b4	
;and off into the ROM again	
	jp $2a1



;that was all pretty exciting
;but here's far more girly routines...

;routine to display numeric value of accumulator on screen
;fairly simple - will ony handle 1 byte values
;a - value, hl - screen position (changes values of b,c,d,e)
dispno
	ld d,0
;hundreds	
	ld e,100
	cp e
	jr	c,nothuns
	call digitcalc	
nothuns
;tens
	ld e,10
	cp e
	jr	c,notens
	call digitcalc
	jr	pnotens
notens
;check if we need to show zeros
	ld c,a
	ld	a,d
	cp	1
	ld	a,c
	jr	nz,pnotens
	ld	b,0
	call dispdigit
pnotens
;last digit is straight forward - whatever is left
	ld	b,a
	call dispdigit
	ret

;subroutine to work out a given digit (100s or 10s)
digitcalc
;d register is 'somethign has been displayed, so we need to show 0s from now on' flag
	ld d,1
;save a in b, so we can clear the carry flag	
	ld b,a
;clear carry flag	
	and a
;retrieve a	
	ld a,b
;now - create the digit	
	ld b,0
digitloop
;each time the number is large than the current '10s' column - increase the digit
	inc	b
	sbc	a,e
	cp	e
	jr	nc,digitloop
	call dispdigit	
	ret

;subroutine to actually write a digit to the screen	
dispdigit
;save a	
	ld c,a
;move digit stored in b to a
	ld a,b
;change to disp digit	
	add	a,$1c
	ld (hl),a
	inc hl
;retrieve original value of a	
	ld a,c
	ret
	
;draw the shields
drawshields
	ld hl,(baselin)
	ld b,DISPLEN-1
	ld a,FULL_BITS
baselp
	ld (hl),a
	inc hl
	djnz baselp
	ld a,(invystr)
	cp 30
	jr c,notlow
	ret
notlow	
	ld hl,(shldpos)
	ld b,4
shldlp3
	push bc
	ld de,shield
	
	ld b,16
shldlp2
	push bc
	ld b,3
shldlp1
	ld a,(de)
	ld (hl),a
	inc hl
	inc de
	djnz shldlp1
	
	ld bc,30
	add hl,bc
	pop bc
	djnz shldlp2
	
	xor a
	
	ld bc,(DISPLEN*16)-8
	sbc hl,bc
	pop bc
	djnz shldlp3

	
	ret

	
	
;print things on screen
dispstring
;write directly to the screen
	ld hl,(D_FILE)
	add hl,bc	
loop2
	ld a,(de)
	cp $ff
	jp z,loop2End
	ld (hl),a
	inc hl
	inc de
	jp loop2
loop2End	
	ret


clearstring
	ld hl,(D_FILE)
	add hl,bc	
cloop2
	ld a,(de)
	cp $ff
	jp z,cloop2End
	ld (hl),__
	inc hl
	inc de
	jp cloop2
cloop2End	
	ret
	
	
;read keys
readkeys
;a bit lazy - call the ROM routine to get the zone values into HL
	call KSCAN
;lazy test for zones...	
;exit on q
 	bit 2,l
	jr nz,noexkey
	ld a,QUIT_GAME
	ld (exitlp),a
noexkey	
;check for explosion
	ld a,(baseexp)
	cp $0
	jr z,nobaseexp
	ret
nobaseexp	
;bit 3 will give us left half of the keys on the top row of the keyboard
;LEFT
	bit 3,l	
	
	jr nz,nokey1
	ld a,(basex)
	cp MIN_X
	jp z,nokey2

	push hl
;it's cheaper to do 2 decs rather than a sbc 2 - as you have to clear the carry flag, which means anding a
;which means you have to save and restore a, and since we not doing any comparisons, decs will do fine...
	dec a
	dec a
	
	ld (basex),a
	
	ld a,(baseg)
	dec a
	cp $ff
	jp nz,nobaseg1
	
	ld hl,(basepos)
	push hl
	inc hl
	inc hl
	
	ld b,8
	ld a,BLANK_BITS
	ld de,DISPLEN
clrlp1
	ld (hl),a
	add hl,de
	djnz clrlp1
	pop hl
	dec hl
	ld (basepos),hl
	ld a,$3
nobaseg1
	ld (baseg),a
;get key value back, but jump to the fire button check	
	pop hl
	jr nokey2
	
	
nokey1
;bit 4 will give us right half of the keys on the top row of the keyboard
;RIGHT
	bit 4,l
	jr nz,nokey2
	ld a,(basex)
	cp MAX_X
	jr z,nokey2
	push hl

	
	add a,2
	ld (basex),a
	ld a,(baseg)
	inc a
	cp $4
	jp nz,nobaseg2
	
	ld hl,(basepos)
	push hl
	ld b,8
	ld a,BLANK_BITS
	ld de,DISPLEN
clrlp2
	ld (hl),a
	add hl,de
	djnz clrlp2
	pop hl
	inc hl
	ld (basepos),hl
	xor a
nobaseg2
	ld (baseg),a
;get the key value back	
	pop hl
		
nokey2
;skip past this if there's already a bullet on screen
	ld a,(bullact)
	cp $1
	jr z,nokey3
	
;bit 7 will give us right half of the keys on the bottom row of the keyboard
;FIRE
	bit 7,l	
	jr nz,nokey4			

	
;make sure we can't just hold down the fire button	
	ld a,(firedwn)
	cp $0
	jr nz,nokey3
	
	ld a,1
	ld (firedwn),a
	
	ld de,DISPLEN*4
	ld hl,(basepos)
	add hl,de

	ld a,(baseg)
;when we fire  - store the offset within the byte	
;pos 0 (within byte)	
	cp $0
	jr nz,notfire1
	ld a,0
	ld de,bull1
	jr pastnotfire
notfire1
	inc hl
	cp $1
	jr nz,notfire2	
;pos 2 (witihn byte)
	ld a,1
	ld de,bull2
	jr pastnotfire
notfire2
	cp $2
	jr nz,notfire3
;pos 3 (within byte)
	ld a,2
	ld de,bull3
	jr pastnotfire
notfire3
;pos 4 (within byte)
	ld a,3
	ld de,bull4

	
pastnotfire	
	ld (bulltp),a
	ld (bullpos),hl
	ld a,(basex)
	ld (bullx),a
	ld a,1
	ld (bullact),a	
	ld a,BULL_INIT
	ld (bully),a
	ld (bullgr),de
	
nokey3		
	ret
nokey4
	ld a,0
	ld (firedwn),a
	ret

	
;check for video sync
vsync	
	push bc
	ld bc,$000a
fkvs1
	djnz fkvs1
	dec c
	jr nz,fkvs1
	
;	ld a,(FRAMES)
;	ld c,a
sync
;	ld a,(FRAMES)
;	cp c
;	jr z,sync

	pop bc
	ret
	

#include "initvars.asm"

;show some rubbish instructions	
instruct
	ld bc,(DISPLEN*7)+11
	ld de,instruct1
	call dispstring

	ld bc,(DISPLEN*10)+10
	ld de,hiscore_txt
	call dispstring

	ld bc,(DISPLEN*10)+19
	ld de,hiscore
	call dispBCD
	
	ld bc,(DISPLEN*13)+2
	ld de,instruct2
	call dispstring

	

	ld bc,(DISPLEN*16)+8
	ld de,instruct3
	call dispstring

	ld bc,(DISPLEN*21)+6	
	ld de,instruct4
	call dispstring
	

	
	
	
waitkpress
	call KSCAN
	
 	bit 3,l
	jr nz,waitkpress
	
;	ld a,l
;	cp $ff
;	jp z,waitkpress

	ld bc,(DISPLEN*7)+10
	ld de,instruct1
	call clearstring
	ld bc,(DISPLEN*10)+6
	ld de,instruct2
	call clearstring

	ret
;include our variables
#include "vars.asm"

; ===========================================================
; code ends
; ===========================================================
;end the REM line and put in the RAND USR line to call our 'hex code'
#include "line2.asm"

;display file defintions for lowres and hires screens
#include "screen.asm"               

						
;close out the basic program
#include "endbasic.asm"
						