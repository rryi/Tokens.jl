# Tokens

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://rryi.github.io/Tokens.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://rryi.github.io/Tokens.jl/dev)
[![Build Status](https://travis-ci.com/rryi/Tokens.jl.svg?branch=master)](https://travis-ci.com/rryi/Tokens.jl)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/rryi/Tokens.jl?svg=true)](https://ci.appveyor.com/project/rryi/Tokens-jl)
[![Codecov](https://codecov.io/gh/rryi/Tokens.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/rryi/Tokens.jl)
[![Build Status](https://api.cirrus-ci.com/github/rryi/Tokens.jl.svg)](https://cirrus-ci.com/github/rryi/Tokens.jl)

Tokens.jl supplies tools to work with small text fragments like those produced by a lexer stage during source code processing of computer languages. 
A lexer segments source text into "tokens", usually short sequences which are classified e.g. into identifiers, numbers, whitespace and so forth.
A token in the context of Tokens.jl consists of a category ID (enumeration with 16 predefined values) and a string. 

Design guidelines are memory and runtime efficiency, in particular for very short strings. Strings with up to 7 code units can be stored directly
in a minimal token structure of 8 bytes, tokens with longer strings use a separate buffer. A token vector uses one common buffer for all elements,
avoiding heap allocations per token creation.

Inspirations came from Nicholas Ormrod's talk on [strange details of std::string at Facebook](https://www.youtube.com/watch?v=kPR8h4-qZdk), 
[ShortStrings](https://github.com/xiaodaigh/ShortStrings.jl) for interned strings, and 
[WeakRefStrings](https://github.com/JuliaData/WeakRefStrings.jl) for string arrays using a shared content buffer.

# Basic structures

## AbstractToken <: AbstractString
A string with additionally attached token category. See token interface chapter.

## FlyToken <: AbstractToken
A token flyweight, of only 8 bytes. It consists of a token category (a value between 0 and 15), string length in code units, and either an offset into a content buffer or directly stored code units for up to 7 code units.

## Token <: AbstractToken
A FlyToken plus a content buffer reference as a struct. The variant BToken uses always the content buffer, even for strings with less than 8 code units.

# AbstractToken interface
