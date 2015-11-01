Main: Main.hs SqRefact.hs HParser.hs TypeCheck.hs Types.hs Parser.hs Sq.hs
	ghc -O2 Main.hs -threaded -outputdir build -rtsopts
HParser.hs : HParser.y
	happy -g HParser.y
