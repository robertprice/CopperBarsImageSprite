;------------------------------
; Copper bars that show and fade in time to a soundtracker mod.
; Logo colour cycles over the copper bars
; Two sprites orbit each other
; Robert Price - 02/01/2023
;
;---------- Includes ----------
            INCDIR      "include"
            INCLUDE     "hw.i"
            INCLUDE     "funcdef.i"
            INCLUDE     "exec/exec_lib.i"
            INCLUDE 	"graphics/gfxbase.i"
            INCLUDE     "graphics/graphics_lib.i"
            INCLUDE     "hardware/cia.i"
;---------- Const ----------

CIAA        EQU $bfe001

			SECTION logo,DATA_C

screen:		INCBIN	"amiga2.raw"			; 320x256 single bit plane image

            SECTION music,DATA_C

mt_data:    INCBIN	"mod.DoSong"						; our music mod file to play

            SECTION Code,CODE,CHIP

init:
            movem.l     d0-a6,-(sp)
            move.l      4.w,a6							; execbase
            moveq.l		#0,d0

            move.l      #gfxname,a1						; get the name of the graphics library
            jsr         _LVOOldOpenLibrary(a6)
            move.l      d0,a1
            move.l		gb_copinit(a1),d4				; save the current copper list so we can restore it later.
            move.l      d4,CopperSave
            jsr         _LVOCloseLibrary(a6)

            lea         CUSTOM,a6                      ; Load the address of the custom registers indo a6

; initialise the soundtracker player
            movem.l 	d0-a6,-(sp)
            bsr 		mt_init
            movem.l 	(sp)+,d0-a6


            move.w      INTENAR(a6),INTENARSave        ; Save original interupts
            move.w      DMACONR(a6),DMACONSave         ; Save DMACON
            move.w      #$138,d0                       ; wait for eoframe
            bsr.w       WaitRaster                     
            move.w      #$7fff,INTENA(a6)              ; disable interupts
            move.w      #$7fff,INTREQ(a6)              ; disable all bits in INTREQ
;            move.w      #$7fff,INTREQ(a6)              ; disable all bits in INTREQ
;            move.w      #$7fff,DMACON(a6)              ; disable all bits in DMACON
;            move.w      #$87e0,DMACON(a6)              ; Activation classique pour démo

			; SET & BLTPRI & DMAEN & BPLEN & COPEN & BLTEN & SPREN bits
            move.w      #%1000011111100000,DMACON(a6)

; setup bitplane1 in the copper list to point to our 1 bitplane image.
			move.l	#screen,d0
			move.w	d0,copBp1P+6						; set the low word
			swap	d0
			move.w	d0,copBp1P+2						; set the high word

; setup sprite0 in the copper list
			move.l	#sprite0Data,d0
			move.w	d0,sprite0+6						; set hte low word
			swap	d0
			move.w	d0,sprite0+2						; set the high word

; setup sprite1 in the copper list
			move.l	#sprite1Data,d0
			move.w	d0,sprite1+6						; set hte low word
			swap	d0
			move.w	d0,sprite1+2						; set the high word

; setup dummy sprites for sprite2 to sprite7
			lea 	sprite2,a0
			moveq.l	#5,d1								; 6 sprites to setup
.spriteSetupLoop:
			move.l	#dummySpriteData,d0						
			move.w	d0,6(a0)							; set the low word
			swap	d0
			move.w	d0,2(a0)							; set the highword
			add.l	#8,a0								; move to the next sprite
			dbra	d1,.spriteSetupLoop					; loop until all setup

; setup the sprite coordinates
			lea			spriteCircleCoords,a0
			move.l		a0,NextSprite0Coord

			add.l		#720,a0							; offset sprite1 by half the coord table. This should place it opposite sprite0
			move.l		a0,NextSprite1Coord


; install our copper list
            move.l      #myCopperList,COP1LC(a6)
            move.w      #0,COPJMP1(a6)
******************************************************************
mainloop:

; Wait for vertical blank
            move.w      #$0c,d0                        ;No buffering, so wait until raster
            bsr.w       WaitRaster                     ;is below the Display Window.


; play the music
            movem.l 	d0-a6,-(sp)
            bsr 		mt_music
            movem.l 	(sp)+,d0-a6

; draw the copper bars
; copper bar 1
            move.l		#copperBar1,a1		; addres of copper bar in copper list in a1
            move.l 		#$100,d2			; the value to increment or fade a bar by
            move.l 		#$400,d3  			; the initial lowest colour of the bar
            cmp.l		#1,gflag1			; Has note been played
            bne.s		.fadeBar1			; if not skip drawing the bar
            bsr			DrawBar				; draw the bar
            move.l 		#0,gflag1			; clear the played flag (in soundplayer.inc)
            bra			.skipToBar2			; move onto the next copper bar
.fadeBar1:
            bsr 		FadeBar				; fade the bar if no note was played

; copper bar 2
.skipToBar2:
            move.l		#copperBar2,a1
            moveq.l 	#$010,d2
            moveq.l 	#$040,d3
            cmp.l		#1,gflag2		; Has note been played
            bne.s		.fadeBar2
            bsr			DrawBar
            move.l	 	#0,gflag2
            bra			.skipToBar3
.fadeBar2:
            bsr			FadeBar

; copper bar 3
.skipToBar3:
            move.l		#copperBar3,a1
            moveq.l		#$001,d2
            moveq.l 	#$004,d3
            cmp.l		#1,gflag3		; Has note been played
            bne.s		.fadeBar3
            bsr			DrawBar
            move.l 		#0,gflag3
            bra			.skipToBar4
.fadeBar3:
            bsr 		FadeBar

; copper bar 4
.skipToBar4:
            move.l		#copperBar4,a1
            move.l 		#$110,d2
            move.l 		#$440,d3
            cmp.l		#1,gflag4		; Has note been played
            bne.s		.fadeBar4
            bsr			DrawBar
            move.l 		#0,gflag4
            bra			.skip
.fadeBar4:
            bsr 		FadeBar

.skip:

; colour cycle the logo.
			bsr			ColourCycleLogo


; move the sprite using the included coordinate table data
			move.l		NextSprite0Coord,a0			; get the coords of the next position
			move.l		(a0)+,sprite0Data			; save it to the sprite data and incrememt the table offset
			lea			spriteCircleCoordsEnd,a1	; load the address of the end of the coord table
			cmp.l		a0,a1						; compare it to the next position
			bgt			.skipSprite0CoordReset		; if the position has overshot the end of teh table reset it
			lea			spriteCircleCoords,a0
.skipSprite0CoordReset:
			move.l		a0,NextSprite0Coord			; save the next position.

			move.l		NextSprite1Coord,a0			; get the coords of the next position
			move.l		(a0)+,sprite1Data			; save it to the sprite data and incrememt the table offset
			lea			spriteCircleCoordsEnd,a1	; load the address of the end of the coord table
			cmp.l		a0,a1						; compare it to the next position
			bgt			.skipSprite1CoordReset		; if the position has overshot the end of teh table reset it
			lea			spriteCircleCoords,a0
.skipSprite1CoordReset:
			move.l		a0,NextSprite1Coord			; save the next position.


; check if the left mouse button has been pressed
; if it hasn't, loop back.
checkmouse:
            btst        #CIAB_GAMEPORT0,CIAA+ciapra
            bne       	mainloop

exit:
; stop the music
            movem.l 	d0-a6,-(sp)
            bsr 		mt_end
            movem.l 	(sp)+,d0-a6

            move.w      #$7fff,DMACON(a6)              ; disable all bits in DMACON
            or.w        #$8200,(DMACONSave)            ; Bit mask inversion for activation
            move.w      (DMACONSave),DMACON(a6)        ; Restore values
            move.l      (CopperSave),COP1LC(a6)        ; Restore values
            or          #$c000,(INTENARSave)
            move        (INTENARSave),INTENA(a6)       ; interrupts reactivation
            movem.l     (sp)+,d0-a6
            moveq.l     #0,d0                          ; Return code 0 tells the OS we exited with errors.
            rts                                        ; End

;-------------------------
; Wait for a scanline
; d0 - the scanline to wait for
; trashes d1
WaitRaster:
            move.l      CUSTOM+VPOSR,d1
            lsr.l       #1,d1
            lsr.w       #7,d1
            cmp.w       d0,d1
            bne.s       WaitRaster                     ;wait until it matches (eq)
            rts

;------------------------
; Fade a copper bar
; a1 - The bar to fade
; d2 - the value to fade by
; trashes d0 and d1
FadeBar:
            moveq.l		#21,d1
            add.l		#6,a1				; move to the first colour definition
.fadeCopper
            move.w		(a1),d0				; get the colour into register d0
            cmp.w		#0,d0				; has the colour already reached black?
            beq			.skipdec			; yes so skip the fade
            sub.w		d2,d0				; fade the colour
            move.w		d0,(a1)				; save the faded colour back to the copper list
.skipdec
            add.l		#8,a1
            dbra		d1,.fadeCopper
            rts

;------------------------
; Draw a copper bar
; a1 - The bar to draw
; d2 - the value to increment the bar
; d3 - the initial start of the bar
; trashes d0 and d1
DrawBar:
; first the colours get brighter
            moveq.l		#10,d1
            move.w		d3,d0				; move the inital colour into d0
            add.l		#6,a1				; move to the first colour definition
.copperloop1
            add.w		d2,d0				; increment the colour value
            move.w		d0,(a1)				; save the colour to the copper list
            add.l		#8,a1				; move to the next colour
            dbra		d1,.copperloop1

; now the colours need to fade
            moveq.l		#10,d1
.copperloop2
            sub.w		d2,d0				; decrement the colour value
            move.w		d0,(a1)				; save the colour to the copper list
            add.l		#8,a1				; move to the next colour
            dbra		d1,.copperloop2
            rts

;------------------------
; Colour cycle the logo
ColourCycleLogo:
            movem.l 	d0-d3,-(sp)			; save the registers we need to use

			move.b		(LogoR),d0			; load the current red value in d0
			move.b		(LogoG),d1			; load the current green value in d1
			move.b		(LogoB),d2			; load the current blue value in d2

			cmp.b		#0,d0
			beq			.redzero
.checkgreen:
			cmp.b		#0,d1
			beq			.greenzero
.checkblue:
			cmp.b		#0,d2
			beq			.bluezero
			bra			.setcolour

.bluezero:
			cmp.b		#0,d0
			bgt			.rminusgplus
			bra			.setcolour

.greenzero
			cmp.b		#0,d2
			bgt			.bminusrplus
			bra			.checkblue

.redzero:
			cmp.b		#0,d1
			bgt			.gminusbplus
			bra			.checkgreen

.gminusbplus:
			subq.b		#1,d1				; fade green
			addq.b		#1,d2				; brighten blue
			bra			.setcolour			; set the colour

.bminusrplus:
			subq.b		#1,d2				; fade blue
			addq.b		#1,d0				; brighten red
			bra			.setcolour			; set the colour

.rminusgplus:
			subq.b		#1,d0				; fade red
			addq.b		#1,d1				; brighten green
			bra			.setcolour			; set the colour

; calculate the RGB value in d3
.setcolour:
			moveq.l		#0,d3				; clear d3
			add.b		d0,d3				; add the red
			lsl.w		#4,d3				; move left 4 bits
			add.b		d1,d3				; add the green
			lsl.w		#4,d3				; move left 4 bits
			add.b		d2,d3				; add the blue

			move.b		d0,(LogoR)			; save the red value
			move.b		d1,(LogoG)			; save the green value
			move.b		d2,(LogoB)			; save the blue value
			move.w		d3,(LogoColour)		; save the RGB value

			move.w		d3,(copperLogoColour+2)	; set the colour in the copper list

            movem.l 	(sp)+,d0-d3			; restore the saved registers
			rts								; return

******************************************************************
gfxname:
              GRAFNAME                                   ; inserts the graphics library name

              EVEN

DMACONSave:   dc.w        1
CopperSave:   dc.l        1
INTENARSave:  dc.w        1

LogoColour:	dc.w 0					; save the RGB value of the logo
LogoR: 		dc.b $f					; save the red value of the logo
LogoG:		dc.b 0					; save the green value of the logo
LogoB:		dc.b 0					; save the blue value of the logo

			EVEN

NextSprite0Coord:
			dc.l	0				; the address of the next coordinate in the table to use for Sprite0
NextSprite1Coord:
			dc.l	0				; the address of the next coordinate in the table to use for Sprite1

			EVEN

; This is the copper list.
myCopperList:
    dc.w	$1fc,$0				; slow fetch for AGA compatibility
    dc.w	BPLCON0,$0200			; wait for screen start

; setup the screen so we can have our 320x256 bitplane
	dc.w	DIWSTRT,$2c81
	dc.w	DIWSTOP,$2cc1
	dc.w	DDFSTRT,$38
	dc.w	DDFSTOP,$d0
	dc.w	BPL1MOD,$0
	dc.w	BPL2MOD,$0

copBp1P:
	dc.w	BPL1PTH,0			; high word of bitplane1
	dc.w	BPL1PTL,0			; low word of bitplane1
	dc.w	BPLCON0,$1200		; turn on bitplane1

    dc.w	COLOR00,$0			; set COLOUR00 to black
copperLogoColour:
    dc.w	COLOR01,$0			; set COLOUR01, we cycle this value


	dc.w	BPLCON1,$0			; no scroll
	dc.w	BPLCON2,$24			; sprites have priority
spriteColours:
	dc.w	COLOR17,$0ff0
	dc.w	COLOR18,$00ff
	dc.w	COLOR19,$0f0f

sprite0:
    dc.w    SPR0PTH,$0000           ;Sprite 0 pointer 
    dc.w    SPR0PTL,$0000
sprite1:
    dc.w    SPR1PTH,$0000           ;Sprite 1 pointer
    dc.w    SPR1PTL,$0000
sprite2:
    dc.w    SPR2PTH,$0000           ;Sprite 2 pointer
    dc.w    SPR2PTL,$0000
sprite3:
    dc.w    SPR3PTH,$0000           ;Sprite 3 pointer
    dc.w    SPR3PTL,$0000
sprite4:
    dc.w    SPR4PTH,$0000           ;Sprite 4 pointer
    dc.w    SPR4PTL,$0000
sprite5:
    dc.w    SPR5PTH,$0000           ;Sprite 5 pointer
    dc.w    SPR5PTL,$0000
sprite6:
    dc.w    SPR6PTH,$0000           ;Sprite 6 pointer
    dc.w    SPR6PTL,$0000
sprite7:
    dc.w    SPR7PTH,$0000           ;Sprite 7 pointer
    dc.w    SPR7PTL,$0000

; draw the first copper bar - red
copperBar1:
    dc.w	$6107,COPPER_HALT
    dc.w	COLOR00,$500
    dc.w	$6207,COPPER_HALT
    dc.w	COLOR00,$600
    dc.w	$6307,COPPER_HALT
    dc.w	COLOR00,$700
    dc.w	$6407,COPPER_HALT
    dc.w	COLOR00,$800
    dc.w	$6507,COPPER_HALT
    dc.w	COLOR00,$900
    dc.w	$6607,COPPER_HALT
    dc.w	COLOR00,$a00
    dc.w	$6707,COPPER_HALT
    dc.w	COLOR00,$b00
    dc.w	$6807,COPPER_HALT
    dc.w	COLOR00,$c00
    dc.w	$6907,COPPER_HALT
    dc.w	COLOR00,$d00
    dc.w	$6a07,COPPER_HALT
    dc.w	COLOR00,$e00
    dc.w	$6b07,COPPER_HALT
    dc.w	COLOR00,$f00
    dc.w	$6c07,COPPER_HALT
    dc.w	COLOR00,$f00
    dc.w	$6d07,COPPER_HALT
    dc.w	COLOR00,$e00
    dc.w	$6e07,COPPER_HALT
    dc.w	COLOR00,$d00
    dc.w	$6f07,COPPER_HALT
    dc.w	COLOR00,$c00
    dc.w	$7007,COPPER_HALT
    dc.w	COLOR00,$b00
    dc.w	$7107,COPPER_HALT
    dc.w	COLOR00,$a00
    dc.w	$7207,COPPER_HALT
    dc.w	COLOR00,$900
    dc.w	$7307,COPPER_HALT
    dc.w	COLOR00,$800
    dc.w	$7407,COPPER_HALT
    dc.w	COLOR00,$700
    dc.w	$7507,COPPER_HALT
    dc.w	COLOR00,$600
    dc.w	$7607,COPPER_HALT
    dc.w	COLOR00,$500

    dc.w	$7707,COPPER_HALT
    dc.w	COLOR00,$000

; draw the second copper bar - green
copperBar2:
    dc.w	$8107,COPPER_HALT
    dc.w	COLOR00,$050
    dc.w	$8207,COPPER_HALT
    dc.w	COLOR00,$060
    dc.w	$8307,COPPER_HALT
    dc.w	COLOR00,$070
    dc.w	$8407,COPPER_HALT
    dc.w	COLOR00,$080
    dc.w	$8507,COPPER_HALT
    dc.w	COLOR00,$090
    dc.w	$8607,COPPER_HALT
    dc.w	COLOR00,$0a0
    dc.w	$8707,COPPER_HALT
    dc.w	COLOR00,$0b0
    dc.w	$8807,COPPER_HALT
    dc.w	COLOR00,$0c0
    dc.w	$8907,COPPER_HALT
    dc.w	COLOR00,$0d0
    dc.w	$8a07,COPPER_HALT
    dc.w	COLOR00,$0e0
    dc.w	$8b07,COPPER_HALT
    dc.w	COLOR00,$0f0
    dc.w	$8c07,COPPER_HALT
    dc.w	COLOR00,$0f0
    dc.w	$8d07,COPPER_HALT
    dc.w	COLOR00,$0e0
    dc.w	$8e07,COPPER_HALT
    dc.w	COLOR00,$0d0
    dc.w	$8f07,COPPER_HALT
    dc.w	COLOR00,$0c0
    dc.w	$9007,COPPER_HALT
    dc.w	COLOR00,$0b0
    dc.w	$9107,COPPER_HALT
    dc.w	COLOR00,$0a0
    dc.w	$9207,COPPER_HALT
    dc.w	COLOR00,$090
    dc.w	$9307,COPPER_HALT
    dc.w	COLOR00,$080
    dc.w	$9407,COPPER_HALT
    dc.w	COLOR00,$070
    dc.w	$9507,COPPER_HALT
    dc.w	COLOR00,$060
    dc.w	$9607,COPPER_HALT
    dc.w	COLOR00,$050

    dc.w	$9707,COPPER_HALT
    dc.w	COLOR00,$000

; draw the third copper bar - blue
copperBar3:
    dc.w	$a107,COPPER_HALT
    dc.w	COLOR00,$005
    dc.w	$a207,COPPER_HALT
    dc.w	COLOR00,$006
    dc.w	$a307,COPPER_HALT
    dc.w	COLOR00,$007
    dc.w	$a407,COPPER_HALT
    dc.w	COLOR00,$008
    dc.w	$a507,COPPER_HALT
    dc.w	COLOR00,$009
    dc.w	$a607,COPPER_HALT
    dc.w	COLOR00,$00a
    dc.w	$a707,COPPER_HALT
    dc.w	COLOR00,$00b
    dc.w	$a807,COPPER_HALT
    dc.w	COLOR00,$00c
    dc.w	$a907,COPPER_HALT
    dc.w	COLOR00,$00d
    dc.w	$aa07,COPPER_HALT
    dc.w	COLOR00,$00e
    dc.w	$ab07,COPPER_HALT
    dc.w	COLOR00,$00f
    dc.w	$ac07,COPPER_HALT
    dc.w	COLOR00,$00f
    dc.w	$ad07,COPPER_HALT
    dc.w	COLOR00,$00e
    dc.w	$ae07,COPPER_HALT
    dc.w	COLOR00,$00d
    dc.w	$af07,COPPER_HALT
    dc.w	COLOR00,$00c
    dc.w	$b007,COPPER_HALT
    dc.w	COLOR00,$00b
    dc.w	$b107,COPPER_HALT
    dc.w	COLOR00,$00a
    dc.w	$b207,COPPER_HALT
    dc.w	COLOR00,$009
    dc.w	$b307,COPPER_HALT
    dc.w	COLOR00,$008
    dc.w	$b407,COPPER_HALT
    dc.w	COLOR00,$007
    dc.w	$b507,COPPER_HALT
    dc.w	COLOR00,$006
    dc.w	$b607,COPPER_HALT
    dc.w	COLOR00,$005

    dc.w	$b707,COPPER_HALT
    dc.w	COLOR00,$000

; draw the fourth copper bar - yellow
copperBar4:
    dc.w	$c107,COPPER_HALT
    dc.w	COLOR00,$550
    dc.w	$c207,COPPER_HALT
    dc.w	COLOR00,$660
    dc.w	$c307,COPPER_HALT
    dc.w	COLOR00,$770
    dc.w	$c407,COPPER_HALT
    dc.w	COLOR00,$880
    dc.w	$c507,COPPER_HALT
    dc.w	COLOR00,$990
    dc.w	$c607,COPPER_HALT
    dc.w	COLOR00,$aa0
    dc.w	$c707,COPPER_HALT
    dc.w	COLOR00,$bb0
    dc.w	$c807,COPPER_HALT
    dc.w	COLOR00,$cc0
    dc.w	$c907,COPPER_HALT
    dc.w	COLOR00,$dd0
    dc.w	$ca07,COPPER_HALT
    dc.w	COLOR00,$ee0
    dc.w	$cb07,COPPER_HALT
    dc.w	COLOR00,$ff0
    dc.w	$cc07,COPPER_HALT
    dc.w	COLOR00,$ff0
    dc.w	$cd07,COPPER_HALT
    dc.w	COLOR00,$ee0
    dc.w	$ce07,COPPER_HALT
    dc.w	COLOR00,$dd0
    dc.w	$cf07,COPPER_HALT
    dc.w	COLOR00,$cc0
    dc.w	$d007,COPPER_HALT
    dc.w	COLOR00,$bb0
    dc.w	$d107,COPPER_HALT
    dc.w	COLOR00,$aa0
    dc.w	$d207,COPPER_HALT
    dc.w	COLOR00,$990
    dc.w	$d307,COPPER_HALT
    dc.w	COLOR00,$880
    dc.w	$d407,COPPER_HALT
    dc.w	COLOR00,$770
    dc.w	$d507,COPPER_HALT
    dc.w	COLOR00,$660
    dc.w	$d607,COPPER_HALT
    dc.w	COLOR00,$550

.copperEnd:
    dc.w	$d707,COPPER_HALT
    dc.w	COLOR00,$000

    dc.l	COPPER_HALT					; impossible position, so Copper halts.


;
; Sprite data for spaceship sprite.  It appears on the screen at V=65 and H=128.
;
sprite0Data:
    dc.w    $6D60,$7200             ;VSTART, HSTART, VSTOP
    dc.w    $0990,$07E0             ;First pair of descriptor words
    dc.w    $13C8,$0FF0
    dc.w    $23C4,$1FF8
    dc.w    $13C8,$0FF0
    dc.w    $0990,$07E0
    dc.w    $0000,$0000             ;End of sprite data

sprite1Data:
    dc.w    $6D60,$7200             ;VSTART, HSTART, VSTOP
    dc.w    $0990,$07E0             ;First pair of descriptor words
    dc.w    $13C8,$0FF0
    dc.w    $23C4,$1FF8
    dc.w    $13C8,$0FF0
    dc.w    $0990,$07E0
    dc.w    $0000,$0000             ;End of sprite data

; blank sprite for when we don't actually want a sprite.
dummySpriteData:
	dc.w	$0000,$0000

; Include the soundtracker player.
; This includes the 4 flags gflag1, gflag2, gflag3, and gflag4
; that we use to detect if a note has been played.
            INCLUDE "soundplayer.inc"

	SECTION coords,DATA
spriteCircleCoords:
		INCLUDE	"spriteCircleCoords.inc"
spriteCircleCoordsEnd: