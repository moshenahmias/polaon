; [ *************************************** PRG 0 ($A000-$BFFF) ***************************************]

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
	bne .clr_ctrl0u

	lda ctrl0u
	cmp #$ff
	bne .down
	jmp .up_pressed

.clr_ctrl0u:

	lda #0
	sta ctrl0u

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
	bne .clr_ctrl0d

	lda ctrl0d
	cmp #$ff
	bne .left
	jmp .down_pressed

.clr_ctrl0d:

	lda #0
	sta ctrl0d

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

	lda #$03
	sta ptrHiII
	lda #$00
	sta ptrLoII

	lda #HIGH(pass001)		; test chapter 1
	sta ptrHi
	lda #LOW(pass001)
	sta ptrLo
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
; 					data (bank 0)
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