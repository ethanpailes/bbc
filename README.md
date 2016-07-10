# Byte Blocks

A dsl to describe ad-hoc data and define language agnostic record structures.

## Build

If you don't already have it installed, install
[stack](https://github.com/commercialhaskell/stack)

Clone the repo

    git clone git@github.com:ethanpailes/bbc.git

or

    git clone https://github.com/ethanpailes/bbc.git

then

    cd bbc
    make  # or just invoke stack build directly

## Test

    make test

## Design for ByteBlocks 2

### Overview

ByteBlocks 2 provides a new C interface for ByteBlocks objects with the primary
goal being zero parsing and serialization overhead. Very often the most expensive
operation is the parsing step, but there is no need to perform this as long as
a convenient access layer is provided. ByteBlocks 2 draws inspiration from
Capt'n Proto.

### Object Handles

ByteBlocks 2 introduces object handles. Instead of representing a block
as a C struct, it is left in its flat binary form and referenced by a
pointer to the first byte of that binary buffer. Assessor methods are
provided for all block members. If a block member is a leaf (basic type)
a direct accessor is provided, otherwise an accessor for an object handle
is provided.

