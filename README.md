
Extensible and Modular Generics for the Masses
==============================================

[Extensible and Modular Generics for the Masses] (EMGM) is a library for generic
programming in Haskell.

[Extensible and Modular Generics for the Masses]: http://www.cs.uu.nl/wiki/GenericProgramming/EMGM

Features
--------

The primary features of EMGM include:

*  Datatype-generic programming using sum-of-product views
*  Large collection of ready-to-use generic functions
*  Included support for standard datatypes: lists, Maybe, tuples
*  Easy to add support for new datatypes
*  Type classes make writing new functions straightforward in a structurally
   inductive style
*  Generic functions are extensible with ad-hoc cases for arbitrary datatypes
*  Good performance of generic functions

The features of this distribution include:

*  The API is thoroughly documented with Haddock
*  Fully tested with QuickCheck and HUnit
*  Program coverage ensures that all useful code has been touched by tests
*  Tested on both Mac and Windows systems


Requirements
------------

EMGM has the following requirements:

*  [GHC] version 6.8.1 or later - It has been tested with versions 6.8.3 and 6.10.1.
*  [Cabal] library version 1.2.1 or later - It has been tested with versions 1.2.4.0 and 1.6.0.1.

[GHC]: http://www.haskell.org/ghc/
[Cabal]: http://www.haskell.org/cabal/


Download & Installation
-----------------------

*If you have [cabal-install]*, you should use that to install the package,
because it will handle everything for you.

    cabal install emgm

*If you don't have cabal-install*, you must download the [emgm package] from
the HackageDB and install it manually. Get the `tar.gz` file and decompress it.

Once downloaded, use the following commands for configuring, building, and
installing the library.

    runghc Setup.lhs configure
    runghc Setup.lhs build
    runghc Setup.lhs install

To generate the Haddock documentation, run this commmand:

    runghc Setup.lhs haddock

For more details on the general options available, refer to the [Cabal User's
Guide].

For more details on library-specific options, see the [Development] section.

[emgm package]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/emgm
[cabal-install]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/cabal-install
[Cabal User's Guide]: http://www.haskell.org/cabal/release/latest/doc/users-guide/
[Development]: #development


Documentation
-------------

The API is documented using [Haddock] and available on the [emgm package] site.

[Haddock]: http://www.haskell.org/haddock/
[emgm package]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/emgm


Examples
--------

You can find examples of using EMGM in the [`examples` directory] of the source
distribution.

[`examples` directory]: https://svn.cs.uu.nl:12443/viewvc/dgp-haskell/EMGM/trunk/examples/


Bugs & Support
--------------

To report bugs, use the Google Code [project page for EMGM].

For general concerns and questions, use the [Generics mailing list].

[project page for EMGM]: http://code.google.com/p/emgm/
[Generics mailing list]: http://www.haskell.org/mailman/listinfo/generics


Licensing
---------

EMGM is licensed under the so-called [BSD3 license]. See the included `LICENSE`
file.

[BSD3 license]: http://www.opensource.org/licenses/bsd-license.php


Credits
-------

The research for EMGM originated with [Ralf Hinze]. It was extended with work by
[Bruno Oliveira] and [Andres Löh]. More details of the library functionality
were explored by [Alexey Rodriguez]. We are very grateful to all of these people
for the foundation on which this library was built.

The current authors and maintainers of EMGM are:

*  [Sean Leather]
*  [José Pedro Magalhães]
*  [Alexey Rodriguez]
*  [Andres Löh]

[Ralf Hinze]: http://www.comlab.ox.ac.uk/ralf.hinze/
[Bruno Oliveira]: http://web.comlab.ox.ac.uk/people/Bruno.Oliveira/
[Andres Löh]: http://people.cs.uu.nl/andres/
[Alexey Rodriguez]: http://www.cs.uu.nl/wiki/Alexey
[Sean Leather]: http://www.cs.uu.nl/staff/leather.html
[José Pedro Magalhães]: http://www.dreixel.net/


Development
-----------

If you're interesting in contributing to the development of EMGM or just in
playing with the code, there are some useful things to know.

### Source ###

The source can be checked out from its repository using [Subversion].

    svn checkout https://svn.cs.uu.nl:12443/repos/dgp-haskell/EMGM

You can also [view the files online].

[Subversion]: http://subversion.tigris.org/
[view the files online]: https://svn.cs.uu.nl:12443/viewvc/dgp-haskell/EMGM/

### Requirements ###

In addition to the requirements for using the library, EMGM has the following
requirements for development:

*  Cabal library 1.4.0.1 or later - This is preferred for uploading to HackageDB. Some
   issues were encountered with the current flags setup in emgm.cabal that gave
   errors in an older version of Cabal.
*  [QuickCheck] 2.1 - Required for testing.
*  [HUnit] 1.2 - Required for testing.

[QuickCheck]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/QuickCheck
[HUnit]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/HUnit

### Configuring ###

If you've been changing many files or the `emgm.cabal` file, you should clean
this distribution and build files.

    runghc Setup.lhs clean

In order to test the library, configure it with the following options:

    runghc Setup.lhs configure -ftest -fnolib

This enables building the test executable (while reducing the optimization level
to speed up the build) and disables building the library (thus also speeding up
the build). `nolib` is optional in case you actually do want to build the
library.

To enable program coverage, add the `hpc` flag. This adds coverage only on the
test executable, so the `test` flag is required.

    runghc Setup.lhs configure -ftest -fnolib -fhpc

### Testing ###

After configuring with the `test` flag and building, you can run the test suite.

    runghc Setup.lhs test

You will see some output from both QuickCheck and HUnit. It should all work!

### Program coverage ###

If you have configured the library for HPC (see above), then you can get the
program coverage using the included script after running the test suite. This
uses the `hpc` command in your path and passes a number of flags excluding
modules that should be ignored for coverage purposes.

Run the script from the top-level directory to see its usage.

    runghc util/hpc.lhs

To get a simple report of the coverage, use the `report` option.

    runghc util/hpc.lhs report

To get a set of HTML files with code coverage indications, use the `markup`
option.

    runghc util/hpc.lhs markup

At the end of this run, the command tells you where to find the HTML files.

