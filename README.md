# Gamble


## Documentation

Read the documentation online here: http://rmculpepper.github.io/gamble/

Installing `gamble` also builds its documentation on your system, and it is available via `raco docs gamble` or Help, Racket Documentation in DrRacket.


## Prerequisites

Racket version 6.0 or later is required.

  * Get Racket here: http://racket-lang.org


## Installation

The following instructions assume you want to write programs using the latest version of `gamble` but not develop `gamble` itself. The install and update commands automatically fetch the latest version of `gamble` from github.

1. Install this repository as a Racket package.
  * command line: `raco pkg install github://github.com/rmculpepper/gamble/master/`
  * or DrRacket: File menu, Install Package, and enter `github://github.com/rmculpepper/gamble/master/`.
2. Periodically update to the most recent version of the package.
  * command line: `raco pkg update gamble`
  * or DrRacket: File menu, Package Mangager, Currently Installed, Update.

## Installing from a snapshot

The following instructions assume you want to write programs using a specific snapshot of `gamble` without updating to the latest available github version.

1. If you had already installed `gamble` using the instructions above, remove it.
  * command line: `raco pkg remove gamble`
2. Extract the snapshot into a directory like gamble-snapshot-vvvvvv" (where vvvvvv is the snapshot version).
  * command line: `tar xzf gamble-snapshot-vvvvvvv.tar.gz`
3. Install the snapshot version of `gamble` as a Racket package.
  * `raco pkg install --link gamble-snapshot-vvvv`

## Development

The following instructions assume you want to develop `gamble`

1. If you had already installed `gamble` using the instructions above, remove it.
  * command line: `raco pkg remove gamble`
2. Clone this repository (http://github.com/rmculpepper/gamble)
  * If you have a github account, `git clone git@github.com:rmculpepper/gamble.git`
  * If you do not: `git clone https://github.com/rmculpepper/gamble.git`
3. Install the checked-out repository as a Racket package.
  * `raco pkg install --link <path-to-gamble-checkout>`
4. Periodically update to the the most recent version of `gamble`.
  * run `git pull` on the repository
  * run `raco setup gamble` to recompile the new code and rebuild the local documentation
