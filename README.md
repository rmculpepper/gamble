# Prob


## Documentation

Read the documentation online here: http://rmculpepper.github.io/prob/prob/


## Installation

1. Download Racket and install it. Version 5.3.6 or later is required.
  * get an official release: http://racket-lang.org 
  * or a nightly build: http://www.cs.utah.edu/plt/snapshots/ or http://plt.eecs.northwestern.edu/snapshots/
  * or build from the git repo: https://github.com/plt/racket
2. Install this repository as a Racket package.
  * command line: `raco pkg install github://github.com/rmculpepper/prob/master/`
  * or DrRacket: File menu, Install Package, and enter `github://github.com/rmculpepper/prob/master/`.
3. Periodically update to the most recent version of the package.
  * command line: `raco pkg update prob`
  * or DrRacket: File menu, Package Mangager, Currently Installed, Update.

## Development

1. If you had already installed `prob` as a package, remove it.
  * command line: `raco pkg remove prob`
2. Clone this repository (http://github.com/rmculpepper/prob)
  * If you have a github account, `git clone git@github.com:rmculpepper/prob.git`
  * If you do not: `git clone https://github.com/rmculpepper/prob.git`
3. Install the checked-out repo as a Racket package.
  * `raco pkg install --link <path-to-prob-checkout>`
4. Periodically update to the the most recent version of the package by running `git pull`.
