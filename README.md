# MAN-AHL
Commands

To build and profile :
cd src/
ghc -prof -fprof-auto -rtsopts -main-is ManAhl.Run.main ManAhl/Run.hs -O2
./ManAhl/Run +RTS -p -s
cat Run.prof 


cabal build
time ./dist/build/MAN-AHL/MAN-AHL -run=Weighted -nSims=1000000 +RTS -s
time cabal test --show-details=streaming

