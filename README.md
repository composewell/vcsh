vcsh - Version Control System for $HOME - multiple Git repositories in $HOME

[![Build Status](https://travis-ci.org/RichiH/vcsh.svg?branch=master)](https://travis-ci.org/RichiH/vcsh)


# Index

1. [30 Second How-to](#30-second-how-to)
2. [Introduction](#introduction)
3. [Haskell implementation](#haskell-implementation)
4. [Contact](#contact)


# 30 Second How-to

While it may appear that there's an overwhelming amount of documentation and
while the explanation of the concepts behind `vcsh` needs to touch a few gory
details of `git` internals, getting started with `vcsh` is extremely simple.

Let's say you want to version control your `vim` configuration:

    vcsh init vim
    vcsh vim add ~/.vimrc ~/.vim
    vcsh vim commit -m 'Initial commit of my Vim configuration'
    # optionally push your files to a remote
    vcsh vim remote add origin <remote>
    vcsh vim push -u origin master
    # from now on you can push additional commits like this
    vcsh vim push

If all that looks a _lot_ like standard `git`, that's no coincidence; it's
a design feature.


# Introduction

[vcsh][vcsh] allows you to maintain several Git repositories in one single
directory. They all maintain their working trees without clobbering each other
or interfering otherwise. By default, all Git repositories maintained via
`vcsh` store the actual files in `$HOME` but you can override this setting if
you want to.

All this means that you can have one repository per application or application
family, i.e. `zsh`, `vim`, `ssh`, etc. This, in turn, allows you to clone
custom sets of configurations onto different machines or even for different
users; picking and mixing which configurations you want to use where.
For example, you may not need to have your `mplayer` configuration on a server
or available to root and you may want to maintain different configuration for
`ssh` on your personal and your work machines.

A lot of modern UNIX-based systems offer packages for `vcsh`. In case yours
does not, read [INSTALL.md](doc/INSTALL.md) for install instructions or
[PACKAGING.md](doc/PACKAGING.md) to create a package yourself. If you do end
up packaging `vcsh` please let us know so we can give you your own packaging
branch in the upstream repository.

## Talks

Some people found it useful to look at slides and videos explaining how `vcsh`
works instead of working through the docs.
All slides, videos, and further information can be found
[on the author's talk page][talks].


# Haskell implementation

This branch has a Haskell implementation of `vcsh`, built with `cabal`
from the [streamly](https://streamly.composewell.com) ecosystem packages.
The shell script `vcsh` remains the reference implementation.

    cabal build
    cabal install exe:vcsh

`cabal test` runs the same command lines with `./vcsh` and with the
Haskell `vcsh`, in separate sandbox directories, and compares the output
and exit codes.

Differences from the shell implementation:

* Configuration files (`/etc/vcsh/config`, `$XDG_CONFIG_HOME/vcsh/config`,
  `-c <file>`, `config.d/<repo>`) are sourced by `/bin/sh`. Variables
  that they set are exported to git and hooks.
* Overlays cannot be sourced; an error is printed for each overlay found.
* `which` matches a substring, not a `grep` regular expression.
* File names are read with `git ls-files -z`, so names with spaces or
  special characters are not split or quoted.
* `commit` passes its arguments to `git commit`.
* `-c <file>` followed by a command works.
* `help` exits with code 0.
* `write-gitignore` always lists the parent directories of the tracked
  files.


# Contact

There are several ways to get in touch with the author and a small but committed
community around the general idea of version controlling your (digital) life.

* IRC: #vcs-home on irc.oftc.net

* Mailing list: [http://lists.madduck.net/listinfo/vcs-home][vcs-home-list]

* Pull requests or issues on [https://github.com/RichiH/vcsh][vcsh]


[myrepos]: http://myrepos.branchable.com/
[talks]: http://richardhartmann.de/talks/
[vcsh]: https://github.com/RichiH/vcsh
[vcs-home-list]: http://lists.madduck.net/listinfo/vcs-home
