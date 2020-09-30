[![experimental](http://badges.github.io/stability-badges/dist/experimental.svg)](http://github.com/badges/stability-badges)
[![Travis-CI Build Status](https://travis-ci.org/morloc-project/rmorlocinternals.svg?branch=master)](https://travis-ci.org/morloc-project/rmorlocinternals)
[![Coverage Status](https://img.shields.io/codecov/c/github/morloc-project/rmorlocinternals/master.svg)](https://codecov.io/github/morloc-project/rmorlocinternals?branch=master)

# rmorlocinternals

Serialization boilerplate for `morloc` compatibility

# Dependencies

This library has no unusual dependencies. Just R (v3.5+) and the Rcpp for C++
compatibility. `rmorlocinternals` makes heavy use of the `json` library written
by Niels Lohmann (https://github.com/nlohmann/json). This library is included
internally as a single header file, so no extra dependencies are required.
