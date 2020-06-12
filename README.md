# Bananagrams solver

This program creates a Scrabble-like grid from some given letters and a word
list. The problem is that all the given letters have to appear exactly as often
as they are given, and no letters can be left over at the end.

The Haskell and Python versions both use the same algorithm. I wrote the Python
version first and then rewrote it in Haskell for speed, as the algorithm isn't
particularly smart (it's pretty much brute force).

Both executables take a word list and, optionally, some letters to lay out. If
no letters are given, random letters are chosen.

## Try it in Haskell

```{sh}
$ ghc -O2 -dynamic bananas
$ ./bananas all-words letters
```

Run `./bananas -h` after compiling to see possible command-line options.

## Try it in Python

```{sh}
$ python3 bananas.py all-words letters
```

Run `python3 bananas.py -h` to see possible command-line options.
