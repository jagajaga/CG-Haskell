### Library written in haskell.  
#### Visualisations and algorithms.  
[![Build Status](https://travis-ci.org/jagajaga/CG-Haskell.png?branch=master)](https://travis-ci.org/jagajaga/CG-Haskell)

---

Uses [Gloss](http://hackage.haskell.org/package/gloss) library.  

---

Local building using cabal:  
```
cabal sandbox init
cabal install --only-dependencies
cabal build
```
After that you can find library and binary in `dist` folder.  

To test it, you can run `dist/build/CG-Visualisation/CG-Visualisation`  

---

Global installation using cabal:  
```
cabal install
```  

---

##### TODO:  
+ Code refactoring
