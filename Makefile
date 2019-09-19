
NESASM=nesasm3

build:
	
	.\tools\strdmp\strdmp.exe -s "minato studios|||||||||    presents" -o text000.str
	.\tools\strdmp\strdmp.exe -s "chapter i: 3 years earlier" -o text001.str
	.\tools\strdmp\strdmp.exe -s "licensed by l & l||||       C 2019 all rights reserved" -o text002.str
	.\tools\strdmp\strdmp.exe -s "voice: rise and shine, mr. limbo, rise and shine.|kol: ?????????????|voice: wake up, mr. limbo, wake up and smell the ashes!|kol: what? where am i?|voice: this was your office.|kol: why is it so dark?|voice: don't you remember?|kol: not really...|voice: the prophecy mr.limbo, you were right.|voice: now come, quickly, you must leave this place, there's no limbo left for you here.|kol: ^limbo^?|voice: hurry up!" -o text003.str
	.\tools\strdmp\strdmp.exe -s "it asks for a password" -o text004.str
	$(NESASM) polaon.asm -s

strdmp:

	go build -o tools\strdmp\strdmp.exe tools\strdmp


clean:

	del polaon.fns
	del polaon.nes
	del pol.nes.deb