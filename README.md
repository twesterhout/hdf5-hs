# hdf5-hs [![GitHub CI](https://github.com/twesterhout/hdf5-hs/workflows/CI/badge.svg)](https://github.com/twesterhout/hdf5-hs/actions)[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

> **Warning:** this package is a work-in-progress. The code is kind of working,
> but documentation and examples are lacking. Also this project was created
> specifically for [SpinED](...) program, so it's going to be "battle-tested"
> rather that a proof-of-concept.

High-level interface to HDF5 for Haskell. The idea is to provide a limited
functionality, but in a user-friendly way. It relies extensively on GADTs to
keep the interface polymorphic, but type-safe.


### Related projects

There is [`hs-hdf5`](https://github.com/mokus0/hs-hdf5). It covers more
functionality of HDF5 than this package, but the API is lower-level.
