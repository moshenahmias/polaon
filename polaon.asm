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

	.include "prg2.asm"

; [ *************************************** PRG 0 ($A000-$BFFF) ************** switchable *************** ]
	.bank 0
	.org $A000

	.include "prg0.asm"

; [ *************************************** PRG 1 ($C000 - $DFFF) ************ switchable *************** ]
	.bank 1
	.org $C000

	.include "prg1.asm"

; [ *************************************** PRG 3 ($E000 - $FFFF) *************************************** ]
	.bank 3
	.org $E000

	.include "prg3.asm"

; [ *************************************** CHR 4 ($0000-$1FFF) ************** switchable *************** ]
	.bank 4
	.org $0000

	.incbin "background000.chr"
	.incbin "sprites000.chr"

; [ *************************************** CHR 5 ($0000-$1FFF) ************** switchable *************** ]
	.bank 5
	.org $0000

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
