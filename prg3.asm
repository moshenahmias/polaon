; [ *************************************** PRG 3 ($E000 - $FFFF) *************************************** ]

Reset:

	lda #%01000110		; set prg $C000 - $DFFF to bank 1
	sta $8000
	lda #1
	sta $8001

	lda #%01000111		; set prg $A000 - $BFFF to bank 0
	sta $8000
	lda #0
	sta $8001

    lda #%10000000      ; cart ram, enable, allow writes
    sta $a001

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
; 				passwords
; ****************************************************

pass001:
	.db $ed, $de, $ec, $ed, $d1, $d2, $d3, $d4, $00 	; "test1234"


; ****************************************************
; 				int vector
; ****************************************************

	.org $FFFA
	.dw NMI    		; NMI
	.dw Reset 		; reset
	.dw 0			; IRQ
