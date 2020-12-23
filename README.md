# hdf5-hs

> **Warning:** this package is a work-in-progress. The code is kind of working,
> but documentation and examples are lacking. Also this project was created
> specifically for [SpinED](...) program, so it's going to be "battle-tested"
> rather that a proof-of-concept.

High-level interface to HDF5 for Haskell. It relies extensively on GADTs to keep
the interface polymorphic, but type-safe.


### Related projects

There is [`hs-hdf5`](https://github.com/mokus0/hs-hdf5). It covers more
functionality of HDF5 than this package, but the API is lower-level.
