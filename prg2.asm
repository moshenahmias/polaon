; [ *************************************** PRG 2 ($8000 - $9FFF) *************************************** ]

Main: 

	sei                 ; disable irqs
  	cld                 ; disable decimal mode

	ldx #$ff			; set the stack pointer to $01ff
  	txs

	stx asp				; set the action stack pointer to $07ff  

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

	lda #0				; init state

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
	cmp [ptrLoII], y
	bne .ne
	iny
	cpy #8
	bne .next

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