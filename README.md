# Tokens

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://rryi.github.io/Tokens.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://rryi.github.io/Tokens.jl/dev)
[![Build Status](https://travis-ci.com/rryi/Tokens.jl.svg?branch=master)](https://travis-ci.com/rryi/Tokens.jl)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/rryi/Tokens.jl?svg=true)](https://ci.appveyor.com/project/rryi/Tokens-jl)
[![Codecov](https://codecov.io/gh/rryi/Tokens.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/rryi/Tokens.jl)
[![Build Status](https://api.cirrus-ci.com/github/rryi/Tokens.jl.svg)](https://cirrus-ci.com/github/rryi/Tokens.jl)

Tokens.jl supplies tools to parse text into tokens, build memory-efficient vectors and trees of tokens, and work with mutable strings. It uses shared buffers, avoiding heap allocations for single tokens and tree elements.

Inspirations came from Nicholas Ormrod's talk on [strange details of std::string at Facebook](https://www.youtube.com/watch?v=kPR8h4-qZdk), and  [ShortStrings](https://github.com/xiaodaigh/ShortStrings.jl) for interned strings. 

# Basic structures

## AbstractToken <: AbstractString
A string with additionally attached token category (a value in 0..7). See token interface chapter.

## TinyToken <: AbstractToken
A token flyweight, of only 8 bytes. It consists of a token category (a value between 0 and 7) and text, either stored directly in the remaining 7 bytes, or stored in some external buffer.

## Token <: AbstractToken
holds a category ID  of non-heap storage
. aims on  It also covers mutable strings general string processing tasks and a mutable string dwith mutable  and mutable strings.

A token is a text fragment recognized to be of some token category. . perform string manipulation do string manipulation.

ext fragments structures for parsing and manipulating text fragments which can have a category attached# Intention

# AbstractToken interface
