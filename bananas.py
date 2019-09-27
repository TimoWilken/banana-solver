#!/usr/bin/env python3

'''Bananagrams solver.'''

import argparse
import logging
import random
from collections import Counter
from itertools import chain
from string import ascii_lowercase


DOWN, ACROSS = 'down', 'across'
BLANK_CHAR = '.'


class WordGrid:
    '''Represents a grid of letters and blanks.'''

    def __init__(self, grid_words=()):
        self._grid_words = list(grid_words)

    @property
    def empty(self):
        '''Whether the grid contains any letters.'''
        return not self._grid_words

    @property
    def words(self):
        '''A list of words laid out on this grid.'''
        return [word for word, _, _, _ in self._grid_words]

    def insert_word(self, word, x, y, direction):
        '''Insert a word in the given position. Checks for conflicts.'''
        # check for conflicts
        for i, char in enumerate(word):
            existing = self.letter(x + i if direction == ACROSS else x,
                                   y + i if direction == DOWN else y)
            if existing and existing != char:
                raise ValueError(f'{word} char {i} conflicts with {existing}')
        self._grid_words.append((word, x, y, direction))

    def remove_word(self, x, y, direction, word):
        '''Remove a word from the grid.'''
        self._grid_words.remove((word, x, y, direction))

    def copy(self):
        '''Return a deep copy of the grid.'''
        return WordGrid(self._grid_words)

    def letter(self, x, y):
        '''Get the letter at the given position on the grid.'''
        for word, word_x, word_y, direction in self._grid_words:
            if x == word_x and direction == DOWN:
                word_coord = y - word_y
            elif y == word_y and direction == ACROSS:
                word_coord = x - word_x
            else:
                continue
            if 0 <= word_coord < len(word):
                return word[word_coord]
        return None

    def letters(self, x, y, length, direction):
        '''Get all letters (and blanks) on the given line segment.'''
        if direction == ACROSS:
            for i in range(length):
                yield self.letter(x + i, y)
        elif direction == DOWN:
            for i in range(length):
                yield self.letter(x, y + i)
        else:
            raise ValueError(direction)

    def bounding_box(self):
        '''Calculate the grid's bounding box.

        Returns a tuple with the top-left corner's position as the first two
        elements and the width and height as the remaining two.
        '''
        min_x = min((x for _, x, _, _ in self._grid_words), default=0)
        min_y = min((y for _, _, y, _ in self._grid_words), default=0)
        max_x = max((x + len(word) if direction == ACROSS else x + 1
                     for word, x, _, direction in self._grid_words),
                    default=0)
        max_y = max((y + len(word) if direction == DOWN else y + 1
                     for word, _, y, direction in self._grid_words),
                    default=0)
        return min_x, min_y, max_x - min_x, max_y - min_y

    def __str__(self):
        '''Return a printable representation of the grid.'''
        min_x, min_y, width, height = self.bounding_box()
        grid = [[BLANK_CHAR] * width for _ in range(height)]

        for word, x, y, direction in self._grid_words:
            if direction == ACROSS:
                grid[y-min_y][x-min_x:x-min_x+len(word)] = list(word)
            elif direction == DOWN:
                for i, char in enumerate(word):
                    grid[y-min_y+i][x-min_x] = char
            else:
                raise ValueError(direction)

        return '\n'.join(map(''.join, grid))

    def reachable_letters(self):
        '''Generate letters not completely surrounded by others.

        The grid can be extended by forming words using these letters.
        '''
        min_x, min_y, width, height = self.bounding_box()
        for x in range(min_x, min_x + width):
            for y in range(min_y, min_y + height):
                letter_here = self.letter(x, y)
                if not letter_here:
                    continue
                if not self.letter(x - 1, y) or not self.letter(x + 1, y):
                    yield letter_here, x, y, ACROSS
                if not self.letter(x, y - 1) or not self.letter(x, y + 1):
                    yield letter_here, x, y, DOWN

    def all_words(self):
        '''All words laid out on the grid, including "accidental" ones.'''
        def columns(grid):
            for i in range(min(map(len, grid))):
                yield ''.join(line[i] for line in grid)
        def words(row_or_col):
            return filter(lambda w: len(w) > 1, row_or_col.split(BLANK_CHAR))

        grid = str(self).split('\n')
        return chain(*map(words, chain(grid, columns(grid))))

    def all_words_valid(self, wordlist):
        '''Check that all words laid out on the grid are in the word list.'''
        return all(map(lambda w: w in wordlist, self.all_words()))


def longest_formable_words(have_letters, wordlist):
    '''Return the list of words it is possible to make using the given letters.

    This function returns those words sorted by length in descending order
    (longest first).
    '''
    def is_formable(word):
        return all(n <= have_letters[l] for l, n in Counter(word).items())
    return sorted(filter(is_formable, wordlist), key=len, reverse=True)


def solve_grid(letters, wordlist):
    '''Generate grids using all the given letters.

    This function returns all possible grids using all the given letters, only
    generating words from the given word list.
    '''

    letters = Counter(letters)
    # Eliminate impossible words early, so we don't check them every iteration.
    wordlist = longest_formable_words(letters, wordlist)
    logging.info('word list is %s words long', len(wordlist))

    def solve_grid_stage(grid, letters_left):
        '''Solve a partially completed grid.

        This is a recursive function that takes a partially completed grid and
        a Counter of letters left to use, and tries to complete the grid.

        This does something like a depth-first search on possible word layouts.
        '''
        if not letters_left:
            # We're done! No letters left, return this grid if it is valid.
            logging.debug('no more letters left, grid done!')
            # Check the grid contains only valid words.
            if grid.all_words_valid(wordlist):
                yield grid
            else:
                logging.debug('grid contains invalid words, discarding')
            return

        if grid.empty:
            # Degenerate initial case.
            # Start the grid off by laying out the first word.
            for word in longest_formable_words(letters_left, wordlist):
                this_grid = grid.copy()
                this_grid.insert_word(word, 0, 0, ACROSS)
                logging.debug('starting with longest remaining word %s', word)
                yield from solve_grid_stage(this_grid,
                                            letters_left - Counter(word))
            return

        # Loop through letters we can use to form more words, and try extending
        # the grid using the letters we have left.
        for letter, x, y, reachable_dir in grid.reachable_letters():
            logging.debug('can reach %s (%s), trying to find useful words',
                          letter, reachable_dir)
            usable_letters = letters_left + Counter(letter)
            for word in longest_formable_words(usable_letters, wordlist):
                logging.debug('can form "%s"', word)
                if letter not in word:
                    # Need to connect it to the existing grid somewhere -- if
                    # we're not using the connecting letter, we can't connect
                    # it to the existing grid.
                    logging.debug("ignoring %s as it doesn't contain %s",
                                  word, letter)
                    continue

                this_grid = grid.copy()
                indices_in_word = [word.index(letter)]
                for _ in range(word.count(letter) - 1):
                    next_index = word.index(letter, indices_in_word[-1] + 1)
                    indices_in_word.append(next_index)

                # If the connecting letter occurs multiple times in the word
                # we've chosen, there are multiple ways to connect it to the
                # existing grid. Let's try all of them.
                if reachable_dir == DOWN:
                    possible_coords = [(x, y - i) for i in indices_in_word]
                elif reachable_dir == ACROSS:
                    possible_coords = [(x - i, y) for i in indices_in_word]
                else:
                    raise ValueError(reachable_dir)

                for new_x, new_y in possible_coords:
                    # Find out which letters already exist in the right place,
                    # and make sure we don't take them out of the pile of
                    # letters left to use.
                    existing_letters = this_grid.letters(
                        new_x, new_y, len(word), reachable_dir)
                    overlap = [char for i, char in enumerate(existing_letters)
                               if char and char == word[i]]
                    logging.debug('%s exists in the grid, removing',
                                  ' '.join(overlap))
                    using_letters = Counter(word) - Counter(overlap)
                    logging.debug('letters actually used: %s', using_letters)
                    if not using_letters:
                        logging.debug("%s already exists here on the grid")
                        continue

                    try:
                        # This will throw a ValueError if we pass an invalid
                        # reachable_dir, but we checked that just above this
                        # loop.
                        this_grid.insert_word(word, new_x, new_y, reachable_dir)
                    except ValueError:
                        logging.debug("%s conflicts with existing grid", word)
                        continue

                    if not using_letters:
                        # If we put (part of) an existing word in the same
                        # place on the grid, that wouldn't cause an error above
                        # but we'd be calling solve_grid_stage again with
                        # exactly the same arguments, causing an infinite loop.
                        logging.debug('%s already exists here', word)
                        continue

                    logging.debug('can insert "%s"', word)
                    yield from solve_grid_stage(this_grid,
                                                letters_left - using_letters)

    return solve_grid_stage(WordGrid(), letters)


def parse_args():
    '''Parse command-line arguments.'''
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('wordlist', metavar='WORDLIST',
                        type=argparse.FileType('r'),
                        help='file containing one lowercase word per line')
    parser.add_argument('letters', metavar='LETTERS', nargs='?',
                        default=''.join(random.choices(ascii_lowercase, k=11)),
                        help='letters to lay out (default: 11 random letters)')
    return parser.parse_args()


def main(args):
    '''Main entry point.'''
    logging.basicConfig(level=logging.INFO)
    wordlist = list(map(str.strip, args.wordlist))
    logging.info('using letters: %s', args.letters)

    for i, grid in enumerate(solve_grid(args.letters, wordlist)):
        words = ', '.join(grid.all_words())
        print(f'grid #{i}: ({words})', grid, '-' * 80, sep='\n')


if __name__ == '__main__':
    main(parse_args())
