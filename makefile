.PHONY: all clean

all:
	stack build
	./.stack-work/dist/*/Cabal-*/build/research/research
	gnuplot gpscripts/graph1.gp
	gnuplot gpscripts/graph2.gp
	gnuplot gpscripts/graph3.gp
	gnuplot gpscripts/graph4.gp

clean:
	rm -fr data

