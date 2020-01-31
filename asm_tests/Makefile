.PHONY: all

all: helloworld.acme.prg rasterbars.acme.prg rasterirq.acme.prg rirqtest.acme.prg codebase64.rasterline.acme.prg derrasterzeileninterrupt_0.acme.prg derrasterzeileninterrupt_1.acme.prg derrasterzeileninterrupt_2.acme.prg derrasterzeileninterrupt_3.acme.prg

# https://github.com/meonwax/acme
ACME := ../../acme-cross-assembler/src/acme

%.acme.prg: %.acme.asm
	$(ACME) -o $@ -f cbm --cpu 6502 --symbollist symbollist.$@ $<

clean:
	rm -f *.prg symbollist.*
