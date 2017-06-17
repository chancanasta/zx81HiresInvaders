;initialise the variables
demoinit
;demo mode counter	
	ld hl,DEMO_WAIT
	ld (democnt),hl
	xor a
;demo state and idx for keys	
	ld (demo),a
	ld (democlk),a
	ld (firedwn),a
	ld hl,demokey
	ld (demopnt),hl
	ret
	
;reset scores and lives
init1up
;these are all reset once at the start of the game
;set score to zero
	ld hl,0
	ld (score),hl
;lives to 3 or 1
;number of lives
	ld a,(demo)
	cp 1
	jr z,demolive
	ld a,3
demolive	
	ld (lives),a

	;and the starting point of the invaders	
	ld a,8
	ld (invystr),a
	ret
;start lower
startlower
	ld a,(invystr)
	cp 48
	jr z,nolower
	add a,8
	ld (invystr),a
nolower		
	ret
;reset the rest	
initvars
;these are called everytime we have to re-enter the main loop
;i.e. when the player is hit or they clear a sheet
	ld a,32
	ld (ibulctm),a
	ld a,MAXINVX
	ld (maxinvx),a
	ld a,MININVX
	ld (mininvx),a
	
	ld a,INVCOLS
	ld (maxicol),a
	ld a,8
	ld (invwait),a
;set ypos var	
	ld a,(invystr)
	ld (invypos),a
	
;these 2 lines may look strange, invrows*invcols is used both in a djnz later on
;and also as an immediate to set vars - so we set b then copy it to a
	ld b,INVROWS*INVCOLS
	ld a,b
;next line is for debug - to clear sheets quickly when testing...	
;	ld a,4
;set the number of 'active' invaders
	ld (noacti),a
	ld hl,invst
	ld a,1
	ld (vsynclp),a
setupinv
	ld (hl),a
	inc hl
	djnz setupinv
;setup init pos	
	ld hl,inv11
	ld (ivgrp1),hl
	ld hl,inv21
	ld (ivgrp2),hl
	ld hl,inv31
	ld (ivgrp3),hl
	
	ld hl,HDISPLAY
	ld de,DISPLEN
;set actual screen starting pos for invaders block
	ld a,(invystr)
;store the start y pos in c, so we can use it in the sub coming up
;save b for loops and a for the 'maths'	
	ld c,a
	ld b,a
invyposlp	
	add hl,de
	djnz invyposlp
	

	ld (ivpos1),hl
	
	ld (ibulpos),hl
	ld a,72
	sub c
	ld b,a
	
scrlp1	
	add hl,de
	djnz scrlp1
;96	
	

	ld de,DISPLEN*(54)
	add hl,de
	
	ld (basepos),hl
	

	ld de,DISPLEN*9
	add hl,de
	
	ld (baselin),hl
	
	ld de,DISPLEN*2
	add hl,de
	
	ld (scrlin),hl
	
	
	xor a
	
	ld (hitcnt),a

	ld de,(DISPLEN*((32-8)+9+2))-2
	sbc hl,de
	ld (shldpos),hl
	
	ld (minicol),a
	
	ld (imsact),a
	ld (imshipx),a
	ld (imspos),a
	ld (imcnt),a
	ld (imbaseg),a
	ld (imdir),a
	ld (imxpl),a
	ld (exitlp),a
	ld (basex),a
	ld (baseg),a
	ld (baseexp),a
	ld (bullact),a
	ld (invxpos),a
	ld (invxchk),a
	ld (invdir),a
	ld (invdown),a
	
	ld (ibulact1),a
	ld (ibulx1),a
	ld (ibuly1),a
	ld (ibulcnt1),a
	
	ld (ibulact2),a
	ld (ibulx2),a
	ld (ibuly2),a
	ld a,6
	ld (ibulcnt2),a
	
	ld a,4	
	ld (ilowrow),a

	ret
