; The Path of Limbo: Age of Nomads
;
; Minato Studios (c) 2019


; RAM mapping:

; globals    		: $0000 - $00FF
; stack      		: $0100 - $01FF
; OAM buffer 		: $0200 - $02FF
; sprites data  	: $0300 - $03FF (state)
; free usage  		: $0400 - $04FF (state)
; free usage  		: $0500 - $05FF (state)
; free usage  		: $0600 - $06FF (state)
; action stack  	: $0700 - $07FF

; ****************************************************
; 					iNES header
; ****************************************************

	; INES header
	.inesprg 2  ; 2 PRG ROM
	.ineschr 2  ; 2 CHR ROM
	.inesmir 1	; mirror
	.inesmap 4	; MMC3

; ****************************************************
; 						defines
; ****************************************************

BOTTOM_BAR_BUF 	= $0410 ; 96 bytes
ACTION_STACK_HI = $0700
SPLASH_TEXT_BUF	= $0300 ; 1k bytes

; [ *************************************** PRG 2 ($8000 - $9FFF) *************************************** ]
	.bank 2
	.org $8000

Main: 

	sei                 ; disable irqs
  	cld                 ; disable decimal mode

	ldx #$ff			; set the stack pointer to $01ff
  	txs

	stx asp				; set the action stack pointer to $06ff

	lda #0
	sta ctrl0
	sta ctrl0lh
	sta ctrl0r
	sta ctrl0l
	sta ctrl0d
	sta ctrl0u
	sta ctrl0t
	sta ctrl0e
	sta ctrl0b
	sta ctrl0a
	sta nmi2001
	sta nmiDMA
	sta nmiBB
	sta frmSync
	sta loopCount
	sta dmaReq
	sta nextAction
	sta currentAction
	sta prevAction
	sta retAction
	sta softPpuScrollX

	lda #%00000110 		; disable bkg and sprites
	sta $2001

	lda #%10001000		; init ppu (NMI enabled)
	sta $2000

	lda #2				; init state

.cs:

	sta state
	jsr ChangeState
	
	jsr ChangePrgBanks				; change Prg banks

	jsr ExeInitHandler

	lda #%00000110					; turn off background and sprites
	jsr Set2001

	jsr HideAllSprites
	jsr ReqSpriteDMA				; sprite dma requested

	jsr ChangeChrBanks				; change chr banks

	ldx sprPalette					; draw background
	ldy sprPalette + 1
	jsr LoadSpritePalette

	ldx bkgPalette					
	ldy bkgPalette + 1
	jsr LoadBackgroundPalette

	ldx nametable
	ldy nametable + 1
	jsr LoadNameTable

	lda #%00011110	; turn on background and sprites
	jsr Set2001

.ca:

	jsr ChangeAction
	jsr ExeActionInitHandler

.loop:

	inc loopCount

	lda currentAction				; check for action change request
	cmp nextAction
	bne .ca

.sync:

	lda frmSync						; wait for sync
	beq .sync
	lda #0
	sta frmSync

	jsr ReadController0
	jsr ExeActionInputHandler

	lda currentAction				; check for action change request
	cmp nextAction
	bne .ca

	jsr ExeProcessHandler
	cmp state						; check for state change request
	bne .cs

	jsr ExeActionProcessHandler
	cmp state						; check for state change request
	bne .cs

	lda currentAction				; check for action change request
	cmp nextAction
	bne .ca

	jsr ExeUpdateHandler
	jsr ExeActionUpdateHandler

	lda dmaReq   					; check for sprite dma request (dmaReq != 0)				
	beq .loop

	lda #0							; clear dma request
	sta dmaReq

	jsr ReqSpriteDMA				; sprite dma requested

	jmp .loop


; ****************************************************
; 						NMI
; ****************************************************

NMI:

	pha
	txa
	pha
	tya
	pha

.set2001

	lda nmi2001
	beq .bar		; no set2001 request

	lda soft2001
	sta $2001

	lda #0
	sta nmi2001		; clear set2001 bit

.bar:

	lda nmiBB
	beq .dma		; no draw bar request

	lda $2002		; reset latch to high
	lda #$23
	sta $2006
	lda #$40
	sta $2006
	
	ldx #0
.next0:
	lda BOTTOM_BAR_BUF, x
	sta $2007
	inx
	cpx #96
	bne .next0

					; attribute table
	lda $2002		; reset latch to high
	lda #$23
	sta $2006
	lda #$f0
	sta $2006

	ldx #96
.next1:
	lda BOTTOM_BAR_BUF, x
	sta $2007
	inx
	cpx #112
	bne .next1	

	lda #0
	sta nmiBB		; clear draw bar bit

.dma:

	lda nmiDMA
	beq .done		; no dma request

	lda #2			; dma from $0200
	sta $4014

	lda #0
	sta nmiDMA		; clear dma bit

.done

	lda #1
	sta frmSync

	lda softPpuScrollX		; reset scroll to (softPpuScrollX, 0)
	sta $2005
	lda #0
	sta $2005

	pla
	tay
	pla
	tax
	pla

	rti


; ****************************************************
; 				global functions
; ****************************************************

; compare a chapter password to user input
; arg (ptr) 	- password
; arg (ptrII) 	- input
; ret (a) 		- non-zero if equal
PasswordCompare:

	tya
	pha

	ldy #0
.next:
	lda [ptrLo], y
	beq .eq
	cmp [ptrLoII], y
	bne .ne
	iny
	jmp .next

.eq:

	pla
	tay
	lda #1
	rts

.ne:

	pla
	tay
	lda #0
	rts

; change prg banks
ChangePrgBanks:

	pha
						; change prg banks

	lda #%01000111		; set prg $A000 - $BFFF to bank [prgBank + 1]
	sta $8000
	lda prgBank
	sta $8001

	lda #%01000110		; set prg $C000 - $DFFF to bank [prgBank]
	sta $8000
	lda prgBank + 1
	sta $8001

	pla

	rts

; change chr banks
ChangeChrBanks:

	pha
						; change chr bank

	lda #%01000000		; 2k chr
	sta $8000
	lda chrBank
	sta $8001

	lda #%01000001		; 2k chr
	sta $8000
	lda chrBank + 1
	sta $8001

	lda #%01000010		; 1k chr
	sta $8000
	lda chrBank + 2
	sta $8001

	lda #%01000011		; 1k chr
	sta $8000
	lda chrBank + 3
	sta $8001

	lda #%01000100		; 1k chr
	sta $8000
	lda chrBank + 4
	sta $8001

	lda #%01000101		; 1k chr
	sta $8000
	lda chrBank + 5
	sta $8001

	pla

	rts

; generate a pseudo-random value
; ret (a) - random value
Random:
	tya
	clc
	adc rseed
	eor tempI
	eor loopCount
	eor ctrl0
	eor $0300
	eor $0301
	asl a
	asl a
	clc
	adc rseed
	eor tempII
	adc loopCount
	adc ctrl0
	adc $0300
	adc $0301
	clc
	adc #03
	sta rseed
    rts               ; return high 8 bits

; push action data to action stack
; arg (a) - data
PhA:

	pha
	txa
	pha
	tsx
	lda $0102, x

	ldx asp
	sta ACTION_STACK_HI, x
	dex
	stx asp

	pla
	tax
	pla

	rts

; pop action data from action stack
; ret (a) - data
PoA:

	pha
	txa
	pha

	ldx asp
	inx
	lda ACTION_STACK_HI, x
	stx asp

	tsx
	sta $0102, x

	pla
	tax
	pla


	rts

; change the current (currentAction) action to nextAction
ChangeAction:

	pha
	txa
	pha
	tya
	pha

	lda nextAction
	clc
	asl a
	adc actions
	tax
	lda actions + 1
	adc #0
	tay
	jsr Dereference16
	stx ptrLo
	sty ptrHi

	ldy #0
	ldx #0

.next:
	lda [ptrLo], y
	sta actInit, x
	iny
	inx
	cpx #10
	bne .next

	lda currentAction
	sta prevAction
	lda nextAction
	sta currentAction

	jsr PoA					; set return action
	sta retAction

	pla
	tay
	pla
	tax
	pla

	rts

; clear the bottom bar area
ClearBottomBar:

	pha
	txa
	pha
	tya
	pha

	lda nametable 
	clc
	adc #$40
	sta ptrLo
	lda nametable + 1 
	adc #$03
	sta ptrHi

	ldy #0
	ldx #0

.next0:
	lda [ptrLo], y
	sta BOTTOM_BAR_BUF, x
	iny
	inx
	cpx #96
	bne .next0
		
	lda nametable 		; clear attr table part
	clc
	adc #$f0
	sta ptrLo
	lda nametable + 1 
	adc #$03
	sta ptrHi

	ldy #0
	ldx #96

.next1:
	lda [ptrLo], y
	sta BOTTOM_BAR_BUF, x
	iny
	inx
	cpx #112
	bne .next1

.done:	

	lda nmiBB		; set nmi flag
	bne .done
	lda #1
	sta nmiBB

	pla
	tay
	pla
	tax
	pla

	rts

; clear the buffer
; arg (x) 	- length hi
; arg (y) 	- length lo
; arg (ptr) - buffer address
; arg (a) 	- clear byte
ClearBuffer:

	sty tempI
	stx tempII
	tax
	pha
	lda tempII
	pha
	tya
	pha

	ldy #0

.next:

	txa
	sta [ptrLo], y
	dec tempI
	lda tempI
	bne .cont
	lda tempII
	beq .done

.cont:
	
	iny
	bne .next
	inc ptrHi
	dec tempII
	jmp .next

.done:

	pla
	tay
	pla
	tax
	pla

	rts

; draw text to buffer
; arg (ptr)		- buffer address
; arg (ptrII) 	- string address
DrawTextToBuffer:

	pha
	txa
	pha
	tya
	pha

	ldx #$04		; 4 times 256 = 1kb
	ldy #0
	
.next:

	lda [ptrLoII], y
	beq .done

	cmp #$01
	bne .store
	
	clc
	lda #8
	adc ptrLo
	sta ptrLo
	lda #0
	adc ptrHi
	sta ptrHi
	jmp .inc

.store:
	sta [ptrLo], y
.inc
	iny
	bne .next
	inc ptrHi
	inc ptrHiII
	dex
	bne .next

.done:

	pla
	tay
	pla
	tax
	pla

	rts

; draw a text dialog on the bottom bar (tempI)
; arg (x) - text low address
; arg (y) - text high address
; ret (x) - incremented text low address
; ret (y) - incremented text high address
; ret (a) - non-zero if was trimmed
DrawTextDialog:

	stx ptrLo
	sty ptrHi

	lda #$c2
	sta BOTTOM_BAR_BUF
	lda #$c4
	sta BOTTOM_BAR_BUF + 31
	lda #$c1
	sta BOTTOM_BAR_BUF + 32
	lda #$c5
	sta BOTTOM_BAR_BUF + 63
	lda #$c0
	sta BOTTOM_BAR_BUF + 64
	lda #$c6
	sta BOTTOM_BAR_BUF + 95

	ldx #1
	lda #$c3
.next0:
	sta BOTTOM_BAR_BUF, x
	inx
	cpx #31
	bne .next0

	ldx #65
	lda #$c7
.next1:
	sta BOTTOM_BAR_BUF, x
	inx
	cpx #95
	bne .next1

	ldy #0
	sty tempI 			; space indicator
	ldx #33

.next2:

	lda [ptrLo], y
	bne .char			; not end of string
	lda #$ff
	dey
	jmp .pad

.char:

	cmp #$ff			; space
	bne .nospace
	sta tempI			; used later

.nospace:

	cmp #$01			; new line
	beq .nl

	cpx #63
	beq .trim			; too long
	jmp .store

.nl:
	cpx #63
	beq .nlpe			; new line padding end
	lda #$ff
	dey
	jmp .store

.pad:

	cpx #63
	beq .no_trim

.store:

	sta BOTTOM_BAR_BUF, x
	iny
	inx
	jmp .next2

.nlpe:

	iny
	lda #1
	pha
	jmp .attrs

.no_trim:

	lda #0
	pha
	jmp .attrs

.trim:								; trim a word

	dey

	lda tempI						; check if there was at least
	beq .arrow						; a single space

	ldx #62
.next3:
	lda [ptrLo], y
	cmp #$ff
	beq .arrow
	lda #$ff 
	sta BOTTOM_BAR_BUF, x
	dex
	dey
	jmp .next3
.arrow:
	lda #$fe
	sta BOTTOM_BAR_BUF + 62

	pha				; trimmed

.attrs:
						; set attr table values to palette #0
	
	tya					; save as we are going to change those
	pha
	lda ptrLo
	pha
	lda ptrHi
	pha

	lda nametable 		; clear attr table part
	clc
	adc #$f0
	sta ptrLo
	lda nametable + 1 
	adc #$03
	sta ptrHi

	ldy #0
	ldx #96
.next4:
	lda [ptrLo], y
	and #$0f
	sta BOTTOM_BAR_BUF, x
	iny
	inx
	cpx #104
	bne .next4

	ldx #104
	lda #$00
.next5:
	sta BOTTOM_BAR_BUF, x
	inx
	cpx #112
	bne .next5

.done:			

	lda nmiBB		; set nmi flag
	bne .done
	lda #1
	sta nmiBB

	pla				; restore ptr
	sta ptrHi
	pla
	sta ptrLo
	pla				; restore y

	clc				; calc next yx
	adc ptrLo
	tax
	lda #0
	adc ptrHi
	tay

	pla				; retval

	rts

; move sprite right
; arg (p) - matrix address (saved)
; arg (x) - sprite structure low address (high is $03)
; ret (a) - btaaaaaa
MoveSpriteRight:

	pha
	tya
	pha
	txa
	pha

	lda $0301, x
	tay
	lda $0300, x
	tax
	jsr IsRightWalkable

	tsx						; save result
	sta $0103, x	

	and #%10000000
	bne .wall
	
	pla						; restore x
	tax
	lda $0300, x
	clc						; inc x pos
	adc #1
	sta $0300, x
	lda #%00011000 			; walk right
	jmp .done

.wall:

	pla						; restore x
	tax
	lda #%10011000 			; stand right

.done:

	sta $0302, x
	lda $0304, x
	and #%10111111
	sta $0304, x
	
	pla
	tay
	pla						; result

	rts

; move sprite left
; arg (p) - matrix address (saved)
; arg (x) - sprite structure low address (high is $03)
; ret (a) - btaaaaaa
MoveSpriteLeft:

	pha
	tya
	pha
	txa
	pha

	lda $0301, x
	tay
	lda $0300, x
	tax
	jsr IsLeftWalkable

	tsx						; save result
	sta $0103, x	

	and #%10000000
	bne .wall
	
	pla						; restore x
	tax
	lda $0300, x
	clc						; dec x pos
	adc #$ff
	sta $0300, x
	lda #%00011000 			; walk left
	jmp .done

.wall:

	pla						; restore x
	tax
	lda #%10011000			; stand left

.done:
			
	sta $0302, x
	lda $0304, x
	ora #%01000000
	sta $0304, x

	pla
	tay
	pla						; result

	rts

; move sprite up
; arg (p) - matrix address (saved)
; arg (x) - sprite structure low address (high is $03)
; ret (a) - btaaaaaa
MoveSpriteUp:

	pha
	tya
	pha
	txa
	pha

	lda $0301, x
	tay
	lda $0300, x
	tax
	jsr IsUpWalkable

	tsx						; save result
	sta $0103, x	

	and #%10000000
	bne .wall
	
	pla						; restore x
	tax
	lda $0301, x
	clc						; dec y pos
	adc #$ff
	sta $0301, x
	lda #%00000000 			; walk up
	jmp .done

.wall:

	pla						; restore x
	tax
	lda #%10000000 			; stand up

.done:

	sta $0302, x
	lda $0304, x
	and #%10111111
	sta $0304, x

	pla
	tay
	pla						; result

	rts

; move sprite down
; arg (p) - matrix address (saved)
; arg (x) - sprite structure low address (high is $03)
; ret (a) - btaaaaaa
MoveSpriteDown:

	pha
	tya
	pha
	txa
	pha

	lda $0301, x
	tay
	lda $0300, x
	tax
	jsr IsDownWalkable

	tsx						; save result
	sta $0103, x	

	and #%10000000
	bne .wall
	
	pla						; restore x
	tax
	lda $0301, x
	clc						; inc y pos
	adc #1
	sta $0301, x
	lda #%00001100	 		; walk down
	jmp .done

.wall:

	pla						; restore x
	tax
	lda #%10001100	 		; stand down

.done:

	sta $0302, x
	lda $0304, x
	and #%10111111
	sta $0304, x
	pla
	tay
	pla						; result

	rts

; move the sprite randomly
; arg (p) - matrix address (saved)
; arg (x) - sprite structure low address (high is $03)
MoveSpriteRandomly:

	pha

	lda $0306, x
	bne .step

	jsr Random
	and #%00000011
	sta $0305, x		; mode

	jsr Random
	sta $0306, x		; steps

.step:

	dec $0306, x		; dec steps counter
	lda $0305, x		; load mode
	cmp #0
	beq .msr			; move right
	cmp #1
	beq .msd			; move down
	cmp #2
	beq .msu			; move up
	cmp #3
	beq .msl			; move left
	lda #%10000000  	; stand
	ora $0302, x
	sta $0302, x
	jmp .done

.msu:
	jsr MoveSpriteUp
	jmp .done
.msd:	
	jsr MoveSpriteDown
	jmp .done	
.msr:
	jsr MoveSpriteRight
	jmp .done
.msl:
	jsr MoveSpriteLeft
.done:

	pla

	rts

; update sprite in oam
; arg (x) - sprite structure low address (high is $03)
; arg (y) - oam low address (high is $02)
DrawSprite:

	; sprite struct:   $30xx
	; byte 0: x
	; byte 1: y
	; byte 2: s00dddd0
	;		  76543210
	;
	; 			[d] 43210 - 0 up, 12 down, 24 right, 25 left
	;			[s] stand
	;
	; byte 3: first sprite tile id
	; byte 4: attributes

	pha
	txa
	pha
	tya
	pha

	lda $0301, x	; push ypos
	clc				; y+1 diff fix
	adc #$ff
	pha
	lda $0300, x	; push xpos
	pha
	lda $0304, x	; push attributes
	pha

	lda #%00011110	; push tile id
	and $0302, x	; a <- 000dddd0 (0, 12 or 24)
	clc
	adc $0303, x
	pha

	lda $0302, x	; stand?
	and #%10000000
	bne .stand
	
	lda $0302, x	; walk 0 or walk 1?
	and #%00010000
	bne .loady
.loadx:
	lda $0301, x
	jmp .walk
.loady:
	lda $0300, x

.walk:

	and #%00001000    ; walk speed
	bne .walk1

.walk0

	pla
	clc
	adc #4
	jmp .update

.walk1

	pla
	clc
	adc #8
	jmp .update

.stand:

	pla

.update:
	
	pha

	lda $0304, x
	and #%01000000
	bne .fliph

				; update tile ids
.noflip:

	tya
	tax
	pla
	clc				
	sta $0201, x
	adc #1
	sta $0205, x
	adc #1
	sta $0209, x
	adc #1
	sta $020d, x
	jmp .attrs

.fliph:

	tya
	tax
	pla
	clc
	sta $0205, x
	adc #1
	sta $0201, x
	adc #1
	sta $020d, x
	adc #1
	sta $0209, x

.attrs:
	pla				; update attributes
	sta $0202, x		
	sta $0206, x
	sta $020a, x
	sta $020e, x

	pla				; update xpos
	sta $0203, x		
	sta $020b, x
	clc
	adc #8
	sta $0207, x
	sta $020f, x

	pla				; update ypos
	sta $0200, x
	sta $0204, x
	clc	
	adc #8
	sta $0208, x
	sta $020c, x

.done:

	pla
	tay
	pla
	tax
	pla

	rts

; hide all sprites
HideAllSprites:

	pha				; backup registers
	txa
	pha

	lda #$ff		; set sprites y pos to $ff
	ldx #0

.next

	sta $0200, x
	inx
	inx
	inx
	inx
	bne .next

	pla				; restore registers
	tax
	pla

	rts

; set 2001 via NMI
; arg (a) - value to set
Set2001:

	pha					; backup acc

	sta soft2001
	lda #1				; turn on set2001 bit
	sta nmi2001

.wait
	lda #1				; wait for commit
	and nmi2001
	bne .wait

	pla					; restore acc

	rts

; request sprite DMA in NMI
ReqSpriteDMA:

	pha					; backup acc
	lda #1				; turn on dma bit
	sta nmiDMA
	pla					; restore acc
	rts

; dereference the given pointer address (word)
; arg (x) - low pointer address
; arg (y) - high pointer address
; ret (x) - low byte
; ret (y) - high byte
Dereference16:

	pha				; backup acc

	stx ptrLo
	sty ptrHi
	ldy #0
	lda [ptrLo], y	; load low byte
	tax
	iny
	lda [ptrLo], y	; load high byte
	tay

	pla				; restore acc

	rts

; change state sets the requested state handlers as the current ones
ChangeState:

	pha					; backup acc
	txa
	pha					; backup x
	tya
	pha					; backup y

	lda state

	clc
	asl a				; calc state address in states array
	adc #LOW(states)
	tax
	lda #0
	adc #HIGH(states)
	tay

	jsr Dereference16		; yx <- state structure address
	
	; copy 22 bytes from $yx to bkgPalette

 	stx ptrLo				; set pointer to state structure
  	sty ptrHi

	ldy #0					; init index
	ldx #0

.next
	lda [ptrLo], y			; load byte
	sta bkgPalette, x		; store byte
	inx
	iny
	cpy #22
	bne .next

	pla						; restore y
	tay
	pla						; restore x
	tax
	pla						; restore acc

	rts

; read controller 0 state into memory (ctrl0)
; [7 : A][6 : B][5 : select][4 : start][3 : up][2 : down][1 : left][0 : right]
ReadController0:

	pha			; backup acc
	txa
	pha			; backup index x

	lda ctrl0	; save previos
	sta ctrl0lh

	lda #$01	; tell both the controllers to latch buttons
	sta $4016
	lda #$00
	sta $4016

	sta ctrl0	; clear controller 1 mask
	ldx #8		; 8 buttons to read

.next:

	lda $4016
	and #%00000001
	asl ctrl0
	ora ctrl0
	sta ctrl0
	dex
	bne .next

					; low to high
	eor ctrl0lh
	and ctrl0
	sta ctrl0lh

					; history
	lda ctrl0
	lsr a
	ror ctrl0r
	lsr a
	ror ctrl0l
	lsr a
	ror ctrl0d
	lsr a
	ror ctrl0u
	lsr a
	ror ctrl0t
	lsr a
	ror ctrl0e
	lsr a
	ror ctrl0b
	lsr a
	ror ctrl0a

	pla			; restore index x
	tax
	pla			; restore acc

	rts

; load nametable to ppu (1kb)
; arg (x) - low address of table
; arg (y) - high address of table
LoadNameTable:

	pha				; backup acc
	tya				; backup y
	pha

 	stx ptrLo		; set pointer to table
  	sty ptrHi

	lda $2002		; reset latch to high
	lda #$20
	sta $2006
	ldy #$00
	sty $2006

	ldx #$04		; 4 times 256 = 1kb
	
.next:

	lda [ptrLo], y	; load byte
	sta $2007		; store byte
	iny
	bne .next
	inc ptrHi
	dex
	bne .next

	pla			; restore y
	tay
	pla			; restore acc
	ldx ptrLo	; restore x	

	rts

; load background palette (16b)
; arg (x) - low address of palette
; arg (y) - high address of palette
LoadBackgroundPalette:

	pha					; backup acc
	lda #$00
	jsr LoadPalette		; load
	pla					; restore acc
	rts

; load sprite palette (16b)
; arg (x) - low address of palette
; arg (y) - high address of palette
LoadSpritePalette:

	pha					; backup acc
	lda #$10			; load to $3f10
	jsr LoadPalette		; load
	pla					; restore acc
	rts

; load palette (16b)
; arg (x) - low address of palette
; arg (y) - high address of palette
; arg (a) - #$00 for background, #$10 for sprite
LoadPalette:

	pha				; backup acc

 	stx ptrLo		; set pointer to palette
  	sty ptrHi

	ldx $2002		; reset latch to high
	ldx #$3f
	stx $2006
	sta $2006

	ldy #$00

.next:

	lda [ptrLo], y	; load byte
	sta $2007		; store byte
	iny
	cpy #16
	bne .next

	ldx ptrLo		; restore x
	ldy ptrHi		; restore y
	pla				; restore acc

	rts

; ****************************************************
; 				handler executers
; ****************************************************

; execute the handler stored in initHandler
ExeInitHandler:
	jmp [initHandler]

; execute the handler stored in procHandler
ExeProcessHandler:
	lda state				; do not change state as default
	jmp [procHandler]

; execute the handler stored in updateHandler
ExeUpdateHandler:
	jmp [updateHandler]

; execute the handler stored in actInit
ExeActionInitHandler:
	jmp [actInit]

; execute the handler stored in actInput
ExeActionInputHandler:
	jmp [actInput]

; execute the handler stored in actProc
ExeActionProcessHandler:
	lda state				; do not change state as default
	jmp [actProc]

; execute the handler stored in actUpdate
ExeActionUpdateHandler:
	jmp [actUpdate]

; execute the handler stored in actExt[a]
ExeActionExt:

	pha
	txa
	pha
	tya
	pha

	tsx
	lda $0103, x
	clc
	asl a
	adc actExt
	tax
	lda actExt + 1
	adc #0
	tay
	jsr Dereference16
	stx ptrLo
	sty ptrHi

	pla
	tay
	pla
	tax
	pla

	jmp [ptrLo]

h_Null:
	rts


; [ *************************************** PRG 0 ($A000-$BFFF) ***************************************]
	.bank 0
	.org $A000

; ****************************************************
; 				state 0 (copyright)
; ****************************************************

S0InitHandler:

	pha
	txa
	pha
	tya
	pha

	ldy #$c0							; clear tiles
	ldx #$03
	lda #LOW(SPLASH_TEXT_BUF)
	sta ptrLo
	lda #HIGH(SPLASH_TEXT_BUF)
	sta ptrHi
	lda #$ff
	jsr ClearBuffer

	ldy #$40							; set attrs
	ldx #$00
	lda #LOW(SPLASH_TEXT_BUF + 960) 
	sta ptrLo
	lda #HIGH(SPLASH_TEXT_BUF + 960) 
	sta ptrHi
	lda #%01010101
	jsr ClearBuffer

	lda #$a7							; draw text
	sta ptrLo
	lda #$04
	sta ptrHi
	lda #LOW(text002)
	sta ptrLoII
	lda #HIGH(text002)
	sta ptrHiII
	jsr DrawTextToBuffer

						; action init
	lda #1				; cs to 1
	jsr PhA
	lda #0				; ca to default
	jsr PhA
	lda #$ff			; timer low
	jsr PhA 			
	lda #$00			; timer high
	jsr PhA 			
	lda #1				; ca to a_cs
	jsr PhA

	pla
	tay
	pla
	tax
	pla

	rts

; ****************************************************
; 				state 1 (welcome screen)
; ****************************************************

S1InitHandler:

	pha
	txa
	pha
	tya
	pha

	ldy #$c0							; clear tiles
	ldx #$03
	lda #LOW(SPLASH_TEXT_BUF)
	sta ptrLo
	lda #HIGH(SPLASH_TEXT_BUF)
	sta ptrHi
	lda #$ff
	jsr ClearBuffer

	ldy #$09							; set attrs
	ldx #$00
	lda #LOW(SPLASH_TEXT_BUF + $3dc) 
	sta ptrLo
	lda #HIGH(SPLASH_TEXT_BUF + $3dc) 
	sta ptrHi
	lda #%01010101
	jsr ClearBuffer
	lda #%10101010
	sta SPLASH_TEXT_BUF + $3da
	sta SPLASH_TEXT_BUF + $3db

	lda #$a9							; draw text
	sta ptrLo
	lda #$04
	sta ptrHi
	lda #LOW(text000)
	sta ptrLoII
	lda #HIGH(text000)
	sta ptrHiII
	jsr DrawTextToBuffer

						; action init
	lda #2				; cs to 2
	jsr PhA
	lda #0				; ca to default
	jsr PhA
	lda #$ff			; timer low
	jsr PhA 			
	lda #$00			; timer high
	jsr PhA 			
	lda #1				; ca to a_cs
	jsr PhA

	pla
	tay
	pla
	tax
	pla

	rts

; ****************************************************
; 				state 2 (menu)
; ****************************************************

S2InitHandler:

	pha

						; action init
	lda #0				; set arrow to new game
	jsr PhA
	lda #2				; ca to a_cs
	jsr PhA

	lda #$ff			; scroll x init
	sta softPpuScrollX			
	lda #1				; ca to a_2_menu
	jsr PhA

	pla

	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2_pass
; $0300 - $0307 : entered password
; $0308 		: cursor

a_2_pass: .dw h_2_PassInit, h_2_PassInput, h_2_PassProc, h_2_PassUpdate, 0

h_2_PassInit:

	pha

	lda #$da		; init to AAAAAAAA
	sta $0300
	sta $0301
	sta $0302
	sta $0303
	sta $0304
	sta $0305
	sta $0306
	sta $0307
	lda #0			; init cursor
	sta $0308

	lda #159		; cursor sprite
	sta $0200
	lda #1
	sta $0201
	sta $0202
	lda #96
	sta $0203

					; char sprites
	lda #151		; y
	sta $0204
	sta $0208
	sta $020c
	sta $0210
	sta $0214
	sta $0218
	sta $021c
	sta $0220
	
	lda #$da		; sprite
	sta $0205
	sta $0209
	sta $020d
	sta $0211
	sta $0215
	sta $0219
	sta $021d
	sta $0221

	lda #03			; color
	sta $0206
	sta $020a
	sta $020e
	sta $0212
	sta $0216
	sta $021a
	sta $021e
	sta $0222

	lda #96			; x
	sta $0207
	lda #104
	sta $020b
	lda #112
	sta $020f
	lda #120
	sta $0213
	lda #128
	sta $0217
	lda #136
	sta $021b
	lda #144
	sta $021f
	lda #152
	sta $0223

	pla

	rts

h_2_PassInput:

	pha
	txa
	pha

.up:

	lda #%00001000	; test [up]
	and ctrl0lh
	bne .up_pressed

	lda ctrl0u
	cmp #$ff
	bne .down

.up_pressed:

	; [up] pressed

	ldx $0308
	lda #$d0
	cmp $0300, x	; "0"?	
	beq .char_zero
	dec $0300, x
	jmp .done
.char_zero:
	lda #$f3
	sta $0300, x	; "z"
	jmp .done

.down:

	lda #%00000100	 ; test [down]
	and ctrl0lh
	bne .down_pressed

	lda ctrl0d
	cmp #$ff
	bne .left

.down_pressed:

	; [down] pressed

	ldx $0308
	lda #$f3
	cmp $0300, x	; "z"?	
	beq .char_z
	inc $0300, x
	jmp .done
.char_z:
	lda #$d0
	sta $0300, x	; "0"
	jmp .done

.left:

	lda #%00000010	; test [left]
	and ctrl0lh
	beq .right

	; [left] pressed

	lda $0308
	beq .cur_zero	; 0
	dec $0308
	jmp .done

.cur_zero:

	lda #7
	sta $0308
	jmp .done

.right:

	lda #%00000001	; test [right]
	and ctrl0lh
	beq .select

	; [right] pressed

	lda $0308
	cmp #7
	beq .cur_seven	; 0
	inc $0308
	jmp .done

.cur_seven:

	lda #0
	sta $0308
	jmp .done

.select:

	lda #%00100000	; test [select]
	and ctrl0lh
	beq .start

	; [select] pressed

	jmp .cancel

.start:

	lda #%00010000	; test [start]
	and ctrl0lh
	beq .done

	; [start] pressed

	lda $03
	sta ptrHi
	lda $00
	sta ptrLo

	lda #HIGH(pass001)		; test chapter 1
	sta ptrHiII
	lda #LOW(pass001)
	sta ptrLoII
	jsr PasswordCompare
	beq .cancel
	lda #5					; cs to 5
	jmp .load

	; todo other chapters

.load:

	jsr PhA
	lda #0					; init to def action
	jsr PhA
	lda #2					; next action is a_cs
	sta nextAction
	jmp .done

.cancel:

	lda #$ff			; set arrow to password
	jsr PhA
	lda #2				; ca to a_cs
	jsr PhA
	lda retAction		; return to menu
	sta nextAction

	jsr HideAllSprites

.done:

	pla
	tax
	pla

	rts

h_2_PassProc:

	pha

	lda $0300		; chars
	sta $0205
	lda $0301
	sta $0209
	lda $0302
	sta $020d
	lda $0303
	sta $0211
	lda $0304
	sta $0215
	lda $0305
	sta $0219
	lda $0306
	sta $021d
	lda $0307
	sta $0221

	lda $0308		; cursor
	asl a
	asl a
	asl a
	clc
	adc #96
	sta $0203

	pla

	rts

h_2_PassUpdate:

	pha
	lda #1
	sta dmaReq

	pla

	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2_menu
;

a_2_menu: .dw h_2_MenuInit, h_2_MenuInput, h_2_MenuProc, h_2_MenuUpdate, 0

h_2_MenuInit:

	pha

	jsr	PoA				; selector
	sta $0300

	lda #111			; arrow
	sta $0200
	lda #0
	sta $0201
	lda #01
	sta $0202
	lda #80
	sta $0203

	pla

	rts

h_2_MenuInput:

	pha

.select:

	lda #%00100000	; test [select]
	and ctrl0lh
	beq .start

	; [select] pressed

	lda $0300		; flip bits
	eor #%11111111
	sta $0300

	jmp .done

.start:

	lda #%00010000	; test [start]
	and ctrl0lh
	beq .done

	; [start] pressed

	lda $0300
	bne .pass

						; new game selected

	lda #3				; cs to 3 (new game)
	jsr PhA
	lda #0				; ca to default
	jsr PhA

	lda retAction
	sta nextAction
	jmp .done

.pass:
						; enter password


	lda #1				; ca to a_2_menu
	jsr PhA

	lda #3
	sta nextAction


.done:

	pla

	rts

h_2_MenuProc:

	pha

	lda $0300		; select
	beq .ng

.pass:

	lda #135
	jmp .done

.ng:

	lda #111

.done:

	sta $0200
	pla

	rts

h_2_MenuUpdate:

	pha
	lda #1
	sta dmaReq
	pla

	rts

; ****************************************************
; 				state 3 (password)
; ****************************************************



; ****************************************************
; 				state 4 (opening monologue)
; ****************************************************

S4InitHandler:

	pha
	txa
	pha
	tya
	pha

	ldy #$c0							; clear tiles
	ldx #$03
	lda #LOW(SPLASH_TEXT_BUF)
	sta ptrLo
	lda #HIGH(SPLASH_TEXT_BUF)
	sta ptrHi
	lda #$51
	jsr ClearBuffer

						; action init
	lda #5				; cs to 5
	jsr PhA
	lda #0				; ca to default
	jsr PhA
	
	lda #$ff			; timer low
	jsr PhA 			
	lda #$00			; timer high
	jsr PhA 			
	lda #2				; ca to a_cs
	jsr PhA

	lda #LOW(text003)	; text low
	jsr PhA
	lda #HIGH(text003)	; text high
	jsr PhA
	lda #0				; ca to a_sleep
	jsr PhA

	lda #$ff			; timer low
	jsr PhA 			
	lda #$00			; timer high
	jsr PhA 			
	lda #1				; ca to a_text
	jsr PhA

	pla
	tay
	pla
	tax
	pla

	rts

; ****************************************************
; 				state 5 (chapter I)
; ****************************************************

S5InitHandler:

	pha
	txa
	pha
	tya
	pha

	ldy #$c0							; clear tiles
	ldx #$03
	lda #LOW(SPLASH_TEXT_BUF)
	sta ptrLo
	lda #HIGH(SPLASH_TEXT_BUF)
	sta ptrHi
	lda #$ff
	jsr ClearBuffer

	ldy #$40							; set attrs
	ldx #$00
	lda #LOW(SPLASH_TEXT_BUF + 960) 
	sta ptrLo
	lda #HIGH(SPLASH_TEXT_BUF + 960) 
	sta ptrHi
	lda #%01010101
	jsr ClearBuffer

	lda #$c3							; draw text
	sta ptrLo
	lda #$04
	sta ptrHi
	lda #LOW(text001)
	sta ptrLoII
	lda #HIGH(text001)
	sta ptrHiII
	jsr DrawTextToBuffer

						; action init
	lda #6				; cs to 6
	jsr PhA
	lda #0				; ca to default
	jsr PhA
	lda #$ff			; timer low
	jsr PhA 			
	lda #$00			; timer high
	jsr PhA 			
	lda #1				; ca to a_cs
	jsr PhA

	pla
	tay
	pla
	tax
	pla

	rts

; ****************************************************
; 				state 6 (open space)
; ****************************************************

S6InitHandler:

	pha

	; player sprite data	
	lda #120		; set player x
	jsr PhA
	lda #112		; set player y
	jsr PhA

	lda #%10001100 	; down, stand
	jsr PhA
	lda #0			; first sprite
	jsr PhA
	lda #%00000000	; attributes
	jsr PhA

	lda #LOW(cmap)	; set cmap
	jsr PhA
	lda #HIGH(cmap)
	jsr PhA

	lda #$ff		; set wba to null
	jsr PhA

	; ai_0 sprite data
	lda #40			; x
	jsr PhA
	lda #48	     	; y
	jsr PhA
	lda #%10011000	; stand, look down	
	jsr PhA
	lda #$24		; id
	jsr PhA
	lda #%00000001	; attrs
	jsr PhA
	lda #0
	jsr PhA		; mode
	jsr PhA		; steps

	lda #0			; ca to a_5_openspace_i
	jsr PhA

	pla

	rts

a_6_openspace_i: .dw h_RpgInit, h_RpgInput, h_RpgProc, h_RpgUpdate, a_6_openspace_i_ext
a_6_openspace_i_ext: .dw h_6_OpenSpace_I_Ext_0, h_6_OpenSpace_I_Ext_1, h_6_OpenSpace_I_Ext_2, h_6_OpenSpace_I_Ext_3

h_6_OpenSpace_I_Ext_0:

	pha

	jsr PoA			; load ai_0 sprite
	sta $030e
	jsr PoA
	sta $030d
	jsr PoA
	sta $030c
	jsr PoA
	sta $030b
	jsr PoA
	sta $030a
	jsr PoA
	sta $0309
	jsr PoA
	sta $0308

	pla

	rts

h_6_OpenSpace_I_Ext_1:

	pha

	lda $0305			; cmap address
	sta ptrLo
	lda $0306
	sta ptrHi
	ldx #$08
	jsr MoveSpriteRandomly

	pla

	rts

h_6_OpenSpace_I_Ext_2:

	pha

	lda $0308		; save ai_0 sprite
	jsr PhA
	lda $0309
	jsr PhA
	lda $030a
	jsr PhA
	lda $030b
	jsr PhA
	lda $030c
	jsr PhA
	lda $030d
	jsr PhA
	lda $030e
	jsr PhA

	pla

	rts

h_6_OpenSpace_I_Ext_3:

	pha
	txa
	pha
	tya
	pha

	ldx #$08			; sprite data  - $0308
	ldy #$10			; second sprite - $0210
	jsr DrawSprite

	pla
	tay
	pla
	tax
	pla

	rts

; ****************************************************
; 					actions
; ****************************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; scroll x

a_sx: .dw h_Null, h_Null, h_SxProc, h_Null, 0

h_SxProc:

	pha

	lda softPpuScrollX
	bne .scroll

	lda retAction
	sta nextAction
	jmp .done

.scroll:
	dec softPpuScrollX
.done:
	pla
	
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; change state
; $0300 - state to change to

a_cs: .dw h_CsInit, h_Null, h_CsProc, h_Null, 0

h_CsInit:
	pha
	jsr PoA
	sta $0300
	pla
	rts

h_CsProc:
	lda retAction
	sta nextAction
	lda $0300
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sleep
; $0300 - $0301 - timer (lo - hi)

a_sleep: .dw h_SleepInit, h_Null, h_SleepProc, h_Null, 0

h_SleepInit:

	pha

	jsr PoA
	sta $0301

	jsr PoA
	sta $0300

	pla

	rts

h_SleepProc:

	pha

	dec $0300
	bne .done
	lda $0301
	beq .end
	dec $0301
	jmp .done

.end:

	lda retAction
	sta nextAction

.done:
	
	pla

	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; text
;
; $0300 - $0301 - string (low - high)
; $0302 - next
; $0303 - continue

a_text: .dw h_TextInit, h_TextInput, h_Null, h_TextUpdate, 0


h_TextInit:

	pha

	jsr PoA			; string high
	sta $0301

	jsr PoA			; string low
	sta $0300

	lda #1			; next
	sta $0302

	lda #1			; continue
	sta $0303

	pla

	rts

h_TextInput:

	pha

.a:

	lda #%10000000	; test [A]
	and ctrl0lh
	beq .done

	lda #1			; next
	sta $0302

.done:

	pla

	rts


h_TextUpdate:

	pha
	txa
	pha
	tya
	pha

	lda $0302
	beq .done

	lda #0
	sta $0302

	lda $0303				; end of string
	beq .return

	ldx $0300				; draw text
	ldy $0301
	jsr DrawTextDialog
	stx $0300
	sty $0301
	sta $0303
	jmp .done

.return:

	jsr ClearBottomBar
	lda retAction
	sta nextAction

.done:

	pla
	tay
	pla
	tax
	pla

	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; rpg (abstract)
;
; $0300 - $0304 - player struct
; $0305 - $0306 - cmap (low - high)
; #0307			- walked by action
;
; ext0 - init
; ext1 - proc
; ext2 - save
; ext3 - update

h_RpgInit:

	pha

	lda #0
	jsr ExeActionExt

	jsr PoA			; load wba
	sta $0307

	jsr PoA
	sta $0306
	jsr PoA			; load cmap
	sta $0305

	jsr PoA			; attributes
	sta $0304

	jsr PoA			; first sprite
	sta $0303

	jsr PoA 		; down, stand
	sta $0302

	jsr PoA			; load player y
	sta $0301

	jsr PoA			; load player x
	sta $0300

	pla

	rts

h_RpgInput:

	pha
	txa
	pha
	tya
	pha

.a:

	lda #%10000000	; test [A]
	and ctrl0lh
	beq .b

	; [A] pressed

	lda $0307		; enable action
	ora #%01000000
	sta $0307

.b:

	lda #%01000000	; test [B]
	and ctrl0lh
	beq .select

	; [B] pressed

.select:

	lda #%00100000	; test [select]
	and ctrl0lh
	beq .start

	; [select] pressed

.start:

	lda #%00010000	; test [start]
	and ctrl0lh
	beq .plus

	; [start] pressed

.plus:

	lda $0305			; cmap address
	sta ptrLo
	lda $0306
	sta ptrHi

.right:

	lda #%00000001	; test [right]
	and ctrl0
	beq .left

	; [right] pressed

	ldx #$00			; player low address
	jsr MoveSpriteRight
	sta $0307

.left:

	lda #%00000010	; test [left]
	and ctrl0
	beq .down

	; [left] pressed

	ldx #$00			; player low address
	jsr MoveSpriteLeft
	sta $0307

.down:

	lda #%00000100	 ; test [down]
	and ctrl0
	beq .up

	; [down] pressed

	ldx #$00			; player low address
	jsr MoveSpriteDown
	sta $0307

.up:

	lda #%00001000	; test [up]
	and ctrl0
	beq .idle

	; [up] pressed
	ldx #$00			; player low address
	jsr MoveSpriteUp
	sta $0307

.idle:

	lda #%00001111	; not moving
	and ctrl0
	bne .done

	lda #%10000000  ; stand
	ora $0302
	sta $0302

.done:

	pla
	tay
	pla
	tax
	pla

	rts

h_RpgProc:

	pha
	txa
	pha
	tya
	pha

	lda #1
	jsr ExeActionExt

	lda $0307			; is action?
	and #%00111111
	eor #%00111111
	beq .exit0
	jmp .action
.exit0:
	jmp .done
.action:

	lda $0307			; is action enabled?
	and #%01000000
	beq .exit1
	jmp .enabled
.exit1:
	jmp .done
.enabled:
					; save action data

	lda $0300		; set player x
	jsr PhA
	lda $0301		; set player y
	jsr PhA

	lda $0302 		; down, stand
	jsr PhA
	lda $0303		; first sprite
	jsr PhA
	lda $0304		; attributes
	jsr PhA

	lda $0305		; set cmap
	jsr PhA
	lda $0306
	jsr PhA

	lda #%00111111	; set wba
	jsr PhA

	lda #2
	jsr ExeActionExt

	lda currentAction	; set return action
	jsr PhA

	; push next action data
	; change action (remove change after proc ?)

	clc					; yx = ptr to data
	lda $0305
	adc #$96
	sta tempI
	lda $0306
	adc #$03
	tay
	lda $0307
	and #%00111111
	asl a
	adc tempI
	tax
	tya
	adc #0
	tay

	jsr Dereference16

	stx ptrLo
	sty ptrHi

	ldy #0				; set next action
	lda [ptrLo], y
	sta nextAction

	ldy #1				; load data size
	lda [ptrLo], y
	
	beq .done			; no data
	
	tax

.next:

	iny
	lda [ptrLo], y
	jsr PhA				; push to action stack
	dex
	bne .next

	lda currentAction	; set return action
	jsr PhA

.done:

	pla
	tay
	pla
	tax
	pla

	rts

h_RpgUpdate:
	
	pha
	txa
	pha
	tya
	pha

	ldx #$00			; sprite data  - $0300
	ldy #$00			; first sprite - $0200
	jsr DrawSprite

	lda #3
	jsr ExeActionExt

	lda #1
	sta dmaReq

	pla
	tay
	pla
	tax
	pla

	rts

; returns btaaaaaa value - left (uses tempI and tempII)
; arg (p) - matrix address (saved)
; arg (x) - sprite's x pixel
; arg (y) - sprite's y pixel
; ret (a) - btaaaaaa
IsLeftWalkable:
	lda #0
	jsr IsHorizontalWalkable
	rts

; returns btaaaaaa value - right (uses tempI and tempII)
; arg (p) - matrix address (saved)
; arg (x) - sprite's x pixel
; arg (y) - sprite's y pixel
; ret (a) - btaaaaaa
IsRightWalkable:
	lda #1
	jsr IsHorizontalWalkable
	rts

; returns btaaaaaa value - up (uses tempI and tempII)
; arg (p) - matrix address (saved)
; arg (x) - sprite's x pixel
; arg (y) - sprite's y pixel
; ret (a) - btaaaaaa
IsUpWalkable:
	lda #0
	jsr IsVerticalWalkable
	rts

; returns btaaaaaa value - down (uses tempI and tempII)
; arg (p) - matrix address (saved)
; arg (x) - sprite's x pixel
; arg (y) - sprite's y pixel
; ret (a) - btaaaaaa
IsDownWalkable:
	lda #1
	jsr IsVerticalWalkable
	rts

; returns btaaaaaa value - vertical (uses tempI and tempII)
; arg (p) - matrix address (saved)
; arg (x) - sprite's x pixel
; arg (y) - sprite's y pixel
; arg (a) - zero for up, non-zero for down
; ret (a) - btaaaaaa
IsVerticalWalkable:

	sta tempII

	txa
	pha
	tya
	pha

	lda #%01111111		; default (not blocked, no action)
	sta tempI

	tya					

	and #%00000111		; if (y mod 8) is non-zero skip
	bne .done

	tya					; y <- (y / 8) - 1
	lsr a
	lsr a
	lsr a
	tay

	lda tempII			; up or down?
	bne .down

.up:

	dey
	jmp .endif

.down:

	iny
	iny

.endif:

	stx tempII			; save for later carry check
	txa	
	lsr a				; div by 8
	lsr a
	lsr a
	tax

	inx
	jsr GetColMapXY
	sta tempI
	and #%10000000
	bne .done			; blocked

	inx
	jsr GetColMapXY
	and #%10000000		; set block bit if blocked
	ora tempI
	sta tempI
	and #%10000000
	bne .done			; blocked

	lda tempII			; a <- x	
	and #%00000111		; if (x mod 8) is non-zero need to test the 3rd tile
	beq .done			; no need to test the 3rd tile
	inx
	jsr GetColMapXY
	and #%10000000		; set block bit if blocked
	ora tempI
	sta tempI

.done:

	pla
	tay
	pla
	tax

	lda tempI

	rts


; returns btaaaaaa value - horizontal (uses tempI and tempII)
; arg (p) - matrix address (saved)
; arg (x) - sprite's x pixel
; arg (y) - sprite's y pixel
; arg (a) - zero for left, non-zero for right
; ret (a) - btaaaaaa
IsHorizontalWalkable:

	sta tempII

	txa
	pha
	tya
	pha

	lda #%01111111		; default (not blocked, no action)
	sta tempI

	txa					

	and #%00000111		; if (x mod 8) is non-zero skip
	bne .done

	txa					; x <- (x / 8) + 2 + 1

	lsr a
	lsr a
	lsr a

	tax

	lda tempII			; left or right?
	beq .left

.right:

	inx
	inx
	inx					; world frame

.left:

	sty tempII			; save for later carry check
	tya
	lsr a				; div by 8
	lsr a
	lsr a

	tay
	jsr GetColMapXY
	sta tempI
	and #%10000000
	bne .done			; blocked

	iny
	jsr GetColMapXY
	and #%10000000		; set block bit if blocked
	ora tempI
	sta tempI
	and #%10000000
	bne .done			; blocked

	lda tempII			; a <- y	
	and #%00000111		; if (y mod 8) is non-zero need to test the 3rd tile
	beq .done			; no need to test the 3rd tile
	iny
	jsr GetColMapXY
	and #%10000000		; set block bit if blocked
	ora tempI
	sta tempI

.done:

	pla
	tay
	pla
	tax

	lda tempI

	rts

; get the (x,y) byte of a collision map (uses tempIII)
; arg (p) - matrix address (saved)
; arg (x) - index x
; arg (y) - index y
; ret (a) - value
GetColMapXY:

	lda ptrLo			; save ptr
	pha
	lda ptrHi
	pha

	tya					; save y
	pha
	txa					; save x
	pha

	sty tempIII			; tempIII <- 2y
	asl tempIII

	tya					; ya <- 32y
	ldy #%00001000

.next:
	asl a
	tax
	tya
	rol a
	tay
	txa
	bcc .next

	clc					; ptr <- ptr + 2y + 32y			
	adc ptrLo
	sta ptrLo
	tya
	adc ptrHi
	sta ptrHi

	lda ptrLo
	adc tempIII
	sta ptrLo
	lda ptrHi
	adc #0
	sta ptrHi

	pla					
	tax					; restore x
	tay					; y <- x
	lda [ptrLo], y
	sta tempIII

	pla					; restore y
	tay

	pla					; restore ptr
	sta ptrHi
	pla
	sta ptrLo

	lda tempIII

	rts


; ****************************************************
; 				global const data
; ****************************************************

cmap:
	.incbin "cmap000.cmp"		; open space (state 5)
	.dw test_data

test_data:
	.db $01, $02		; action id, size
	.dw text004			; params

bg_palettes000:
	.incbin "bg_palettes000.pal"		; menu

nametable000:
	.incbin "nametable000.nam"			; menu

bg_palettes001:
	.incbin "bg_palettes001.pal"		; open space

sp_palettes001:
	.incbin "sp_palettes001.pal"		; open space

nametable001:
	.incbin "nametable001.nam"			; open space

text000:
	.incbin "text000.str"				; "mintao studios presents"

text001:
	.incbin "text001.str"				; "chapter I ..."

text002: 
	.incbin "text002.str"				; "licensed by ..."

text003:
	.incbin "text003.str"				; "pre ch 1 monologue"

text004:
	.incbin "text004.str"				; "is asks for a password"

pass001:
	.db $ed, $de, $ec, $ed, $d1, $d2, $d3, $d4, $00 	; "test1234"

; [ *************************************** PRG 1 ($C000 - $DFFF) *************************************** ]
	.bank 1
	.org $C000

; [ *************************************** PRG 3 ($E000 - $FFFF) *************************************** ]
	.bank 3
	.org $E000

Reset:

	lda #%01000110		; set prg $C000 - $DFFF to bank 1
	sta $8000
	lda #1
	sta $8001

	lda #%01000111		; set prg $A000 - $BFFF to bank 0
	sta $8000
	lda #0
	sta $8001

	jmp Main

; ****************************************************
; 				state structures
; ****************************************************

states:
	.dw s0, s1, s2, s3, s4, s5, s6

s0:
	.dw bg_palettes000		; background palettes
	.dw bg_palettes000		; sprite palettes
	.dw SPLASH_TEXT_BUF		; nametable
	.db 0, 2, 4, 5, 6, 7 	; char bank
	.db 0, 1				; prg bank
	.dw S0InitHandler		; init
	.dw h_Null				; proc
	.dw h_Null				; update	
	.dw actions0			; actions

actions0: .dw a_sleep, a_cs

s1:
	.dw bg_palettes000		; background palettes
	.dw bg_palettes000		; sprite palettes
	.dw SPLASH_TEXT_BUF		; nametable
	.db 0, 2, 4, 5, 6, 7 	; char bank
	.db 0, 1				; prg bank
	.dw S1InitHandler		; init
	.dw h_Null				; proc
	.dw h_Null				; update	
	.dw actions1			; actions

actions1: .dw a_sleep, a_cs

s2:
	.dw bg_palettes000		; background palettes
	.dw bg_palettes000		; sprite palettes
	.dw nametable000		; nametable
	.db 0, 2, 4, 5, 6, 7 	; char bank
	.db 0, 1				; prg bank
	.dw S2InitHandler		; init
	.dw h_Null				; proc
	.dw h_Null				; update	
	.dw actions2			; actions

actions2: .dw a_sx, a_2_menu, a_cs, a_2_pass


s3:

s4:
	.dw bg_palettes001			; background palettes
	.dw bg_palettes001			; sprite palettes
	.dw SPLASH_TEXT_BUF			; nametable
	.db 8, 10, 12, 13, 14, 15 	; chr bank
	.db 0, 1					; prg bank
	.dw S4InitHandler			; init
	.dw h_Null					; proc
	.dw h_Null					; update	
	.dw actions4				; actions

actions4: .dw a_sleep, a_text, a_cs

s5:
	.dw bg_palettes000		; background palettes
	.dw bg_palettes000		; sprite palettes
	.dw SPLASH_TEXT_BUF		; nametable
	.db 0, 2, 4, 5, 6, 7 	; char bank
	.db 0, 1				; prg bank
	.dw S5InitHandler		; init
	.dw h_Null				; proc
	.dw h_Null				; update	
	.dw actions5			; actions

actions5: .dw a_sleep, a_cs

s6:
	.dw bg_palettes001
	.dw sp_palettes001
	.dw nametable001
	.db 8, 10, 12, 13, 14, 15 	; chr bank
	.db 0, 1					; prg bank
	.dw S6InitHandler			; init
	.dw h_Null					; proc
	.dw h_Null					; update
	.dw actions6

actions6: .dw a_6_openspace_i, a_text

; ****************************************************
; 				int vector
; ****************************************************

	.org $FFFA
	.dw NMI    		; NMI
	.dw Reset 		; reset
	.dw 0			; IRQ


; [ *************************************** CHR 4 ($0000-$1FFF) *************************************** ]
	.bank 4
	.org $0000
	.incbin "background000.chr"
	.incbin "sprites000.chr"
; [ *************************************** CHR 5 ($0000-$1FFF) *************************************** ]
	.incbin "background001.chr"
	.incbin "sprites001.chr"



; ****************************************************
; 					global variables
; ****************************************************

	.rsset $0000
ptrLo   	.rs 1			; pointer (low)
ptrHi   	.rs 1			; pointer (high)
ptrLoII   	.rs 1			; pointer 2 (low)
ptrHiII   	.rs 1			; pointer 2 (high)
tempI		.rs 1			; for temp things
tempII		.rs 1			; for temp things
tempIII		.rs 1			; for temp things
ctrl0		.rs 1			; controller 1 state
ctrl0lh		.rs 1			; controller 1 state (low to high)
ctrl0l		.rs 1			; controller 1 state (left history)
ctrl0r		.rs 1			; controller 1 state (right history)
ctrl0u		.rs 1			; controller 1 state (up history)
ctrl0d		.rs 1			; controller 1 state (down history)
ctrl0e		.rs 1			; controller 1 state (select history)
ctrl0t		.rs 1			; controller 1 state (start history)
ctrl0a		.rs 1			; controller 1 state (a history)
ctrl0b		.rs 1			; controller 1 state (b history)
state		.rs 1			; current state
nmi2001 	.rs 1			; 2001 reg nmi flag
nmiDMA 		.rs 1			; DMA req nmi flag
nmiBB		.rs 1			; bottom bar nmi flag
soft2001 	.rs 1			; 2001 reg buffer
frmSync 	.rs 1			; used to sync main loop with frame rate
asp 		.rs 1			; action stack ptr
loopCount	.rs 1			; main loop counter
rseed		.rs 1			; random seed

softPpuScrollX  .rs 1		; x scroll soft register

dmaReq			.rs 1			; dma request flag
currentAction 	.rs 1			; action registers
nextAction		.rs 1
prevAction 		.rs 1
retAction 		.rs 1

bkgPalette		.rs 2		; current state
sprPalette		.rs 2
nametable		.rs 2
chrBank			.rs 6
prgBank			.rs 2
initHandler 	.rs 2
procHandler 	.rs 2
updateHandler 	.rs 2
actions			.rs 2

actInit 		.rs 2		; current action
actInput 		.rs 2
actProc 		.rs 2
actUpdate 		.rs 2
actExt			.rs 2
