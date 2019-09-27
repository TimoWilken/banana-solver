# Bananagrams solver

This program creates a Scrabble-like grid from some given letters and a word list.

The Haskell and Python versions both use the same algorithm. I wrote the Python
version first and then rewrote it in Haskell for speed.

## Try it in Haskell

```{sh}
$ ghc -O2 -dynamic bananas
$ ./bananas all-words letters
```

## Try it in Python

```{sh}
python3 bananas.py all-words letters
```

Run `python3 bananas.py -h` to see possible command-line options.