;clear screen routines
cls
	push af
	push bc
	push hl
	ld a,0
	ld hl,Display
	inc hl
	ld c,24
clslp1	
	ld b,32
clslp2
	ld (hl),a
	inc hl
	djnz clslp2
	inc hl
	dec c
	jr nz,clslp1
	
	
	pop hl
	pop bc
	pop af
	ret
	
hirescls
	push af
	push bc
	push hl
	ld a,BLANK_BITS
	ld hl,HDISPLAY	
	ld c,136
hrclslp1	
	ld b,32
hrclslp2
	ld (hl),a
	inc hl
	djnz hrclslp2
	inc hl
	dec c
	jr nz,hrclslp1
	
	
	pop hl
	pop bc
	pop af
	ret
