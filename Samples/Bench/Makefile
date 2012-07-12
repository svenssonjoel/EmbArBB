

all:
	ghc --make matrix_mult.hs -i../../
	ghc --make dotprod.hs -i../../ 
	ghc --make sobel.hs -i../../
	ghc --make sobelfull.hs -i../../
	ghc --make blur.hs -i../../
	ghc --make blurpart.hs -i../../

clean: 
	rm matrix_mult
	rm dotprod
	rm sobel
	rm blur
	rm blurpart
	rm *.o
	rm *.hi