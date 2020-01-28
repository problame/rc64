.PHONY: all

all: hello_world_loop.prg 99beer.prg maze.prg repro.prg colorchart_madness.prg

%.prg: %.basic
	petcat -w2 -o $@ $<

clean:
	rm *.prg
