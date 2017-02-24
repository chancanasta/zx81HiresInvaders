;mothership routines
hitmship
	ld bc,BCD_100
	call incscore
	
	
	ld a,MSHIXPL
	ld (imxpl),a
	ret

mothership
;check if the mother ship is active
	ld a,(imsact)
	cp $1
	jr nz,nomship
;it is, so draw/move it	
	jr drawmship	
nomship
;no ship yet, so increase the counter
	ld a,(imcnt)
	inc a
	ld (imcnt),a
	cp MSHIPCNT
	jp nz,skipmship
;ok - start up the mother ship	
	ld a,1
	ld (imsact),a
;work out position	

;reset counter
	xor a	
	ld (imcnt),a
;setup the actual position
	ld hl,HDISPLAY
;see which direction we're going	
	ld a,(imdir)
	cp $0
	jr z,mshipgoright
;going left
;move screen pos to far right
	ld de,MSHISTX
	add hl,de
	ld a,MSHISTXPOS
	ld a,$3
	ld (imbaseg),a	
	jr pmshipgoright	
mshipgoright
;going right
;easiest - set position to far left (basically do nothing)
	xor a
	ld (imbaseg),a
pmshipgoright	
;store position , x and starting graphic
	ld (imspos),hl
	ld (imshipx),a

;jump to the drawing
	jr drawmship
	
	
skipmship
	
	ret
;draw/move the mother ship
drawmship
;check if it's just an explosion
	ld a,(imxpl)
	cp $0
	jr z,notmsexp
;explosion - so draw it
	dec a
	ld (imxpl),a
	cp $0
	jr z,finishedms


	ld de,expl1	
	ld hl,(imspos)	
	inc hl
	ld	b,8
msexpl1	
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
	djnz msexpl1
		
	
	jr pfinishedms
finishedms	
	jp resetmother

pfinishedms
	ret

notmsexp
	
	ld hl,(imspos)	
	
	ld a,(imbaseg)	
	cp $0
	jr nz,nomship1	
	ld de,mother1
	jr mdrwstart
nomship1
	cp $1
	jr nz,nomship2
	ld de,mother2
	jr mdrwstart
nomship2
	cp $2
	jr nz,nomship3
	ld de,mother3
	jr mdrwstart
nomship3
	ld de,mother4

mdrwstart	
;add blank to left or right of ship
	push de
	push hl
	ld a,(imdir)
	cp $0
	jr z,mgoright1
;mother ship going left
;blank the right of it
	inc	hl
	inc	hl
	inc hl
mgoright1
;mother ship going right	
;no need to do anything - blank will appear on left
pmgoright1
	ld a,BLANK_BITS
	ld b,7
	ld de,DISPLEN
mrublp:	
	ld (hl),a
	add hl,de	
	djnz mrublp
	
	pop hl
	pop de

	ld a,(imdir)
	cp $1
	jr z,mgoleft1
;if we're going left - don't move past the blank, as it's not there...	
	inc hl
mgoleft1:	
;draw the ship	
	ld b,7	
mdrwlp1	
	push bc
	ld b,3
mdrwlp2	
	ld a,(de)
	ld (hl),a
	inc hl
	inc de
	djnz mdrwlp2
	ld bc,30
	add hl,bc
	pop bc
	djnz mdrwlp1
	;move the ship to the right or left
;comment out line below to stop mother ship from moving	(for tests)
;	jr pshipmove
	
;x var
	ld a,(imshipx)
	add a,2
;check if we reached the end of the screen
	ld (imshipx),a
	cp MSHIENDX
	jr c,nomsend	
;we did - so reset	
resetmother
	xor a
	ld (imsact),a
	ld (imcnt),a
;switch direction
	ld a,(imdir)
	cp $1
	jr z,rmshipl
	ld a,$1
	jr prmshipl
rmshipl
	xor a	
prmshipl	
	ld (imdir),a

;blank out what's there
	ld hl,(imspos)
	ld a,BLANK_BITS
	ld b,8
mshipwp1
	push bc
	ld b,4
mshipwp2
	ld (hl),a
	inc hl
	djnz mshipwp2
	ld bc,29
	add hl,bc
	pop bc
	djnz mshipwp1
	
;;	
	
;quit
pshipmove
	ret
	
nomsend
;going left or right
	ld a,(imdir)
	cp $1
	jr z,mshipgoleft
;graphic counter	
	ld a,(imbaseg)
	inc a
	ld (imbaseg),a
	cp $4
	jr nz,nomovmship
;need to physically move the thing	
	xor a
	ld (imbaseg),a
		
	ld hl,(imspos)
;then move to the right
	inc hl
	ld (imspos),hl
	
nomovmship
		
	ret

mshipgoleft
;graphic counter	
	ld a,(imbaseg)
	dec a	
	ld (imbaseg),a
	cp $ff
	jr nz,nomovmshipl
;need to physically move the thing	
	ld a,$3
	ld (imbaseg),a
		
	ld hl,(imspos)
;then move to the left
	dec hl
	ld (imspos),hl
nomovmshipl
	ret
