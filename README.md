# Prob


## Documentation

Read the documentation online here: http://rmculpepper.github.io/prob/prob/

Installing `prob` also builds its documentation on your system, and it is available via `raco docs prob` or Help, Racket Documentation in DrRacket.


## Prerequisites

Racket version 6.0 or later is required.

  * Get Racket here: http://racket-lang.org


## Installation

The following instructions assume you want to write programs using `prob` but not develop `prob` itself. The install and update commands automatically fetch the latest version of `prob` from github.

1. Install this repository as a Racket package.
  * command line: `raco pkg install github://github.com/rmculpepper/prob/master/`
  * or DrRacket: File menu, Install Package, and enter `github://github.com/rmculpepper/prob/master/`.
2. Periodically update to the most recent version of the package.
  * command line: `raco pkg update prob`
  * or DrRacket: File menu, Package Mangager, Currently Installed, Update.


## Development

The following instructions assume you want to develop `prob` or you want to use a snapshot of the source code.

1. If you had already installed `prob` using the instructions above, remove it.
  * command line: `raco pkg remove prob`
2. Clone this repository (http://github.com/rmculpepper/prob)
  * If you have a github account, `git clone git@github.com:rmculpepper/prob.git`
  * If you do not: `git clone https://github.com/rmculpepper/prob.git`
  * If you are using a snapshot, skip this step and use the snapshot directory as the "repository" for the next instruction.
3. Install the checked-out repository as a Racket package.
  * `raco pkg install --link <path-to-prob-checkout>`
4. Periodically update to the the most recent version of `prob`.
  * run `git pull` on the repository
  * run `raco setup prob` to recompile the new code and rebuild the local documentation
