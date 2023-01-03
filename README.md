## [Advent of Code 2022](https://adventofcode.com) with [Scheme](https://small.r7rs.org/)

The solutions are tested on the [Gambit](https://gambitscheme.org/) and the [Gauche](https://practical-scheme.net/gauche/) r7rs Scheme Interpreters.

The input file and the sample file(s) must be placed within the folder of the
puzzle.

The input file is named `input.txt`, while the sample file is named
`sample.txt`. In a situation where the puzzle provides more than one samples, their
file names can be deduced by reading `main.scm` for that puzzle - they
will mostly be named as `sample.0.txt`, `sample.1.txt`, and so on.

## To run
To run on Gambit:
`gsi -:search=../lib,r7rs main.scm`

To run on Gauche:
`gosh -A../lib -r7 main.scm`

## License
These solutions are distributed under the [MIT](https://spdx.org/licenses/MIT.html) license.
