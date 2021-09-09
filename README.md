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

# Current State

Tokens.jl compiles and runs defined tests. However test coverage is very incomplete, features are not mature, expect breaking changes in upcoming releases below release 1.0.

# Basic structures

## AbstractToken <: AbstractString
A string with an attached token category. There are 16 categories with predefined meaning, closely related to the lexer methods in the Tokens package.

## DToken, BToken, HToken <: AbstractToken
The type central to this package comes in three flavours, identified with a prefix character.

B in BToken stands for "buffered". Token contents is stored in a separate string buffer, a BToken instance holds a reference to it. memory layout and behavior is very similar to a SubString. An instance consists of 16 bytes (w/o buffer).

D in DToken stands for "direct": token contents is directly encoded within the instance, no separate buffer for contents is used. Instance conststs of 8 bytes. Contents size is limited to 7 code units. Compared to BToken, it saves memory, has increases data locality, and many operations are mich faster, e.g. comparisons.

H in HToken stands for "hybrid". HToken combines DToken and BToken in one type, similar to a julia union type. An instance consists of 16 bytes. On runtime, it is cast to a DToken or BToken instance. In scenarios where the percentage of short tokens, wich fit into a DToken, is high, it saves memory and has better overall performance. 

## SharedIO <: IO
An IOBuffer derivate with elaborated token support. It uses a "copy on write" approach, keeping track of its parts which are
referenced by tokens and token vectors.


## TokenVector <: AbstractVector
Memory-efficient Vector implementation which uses one SharedIO for all of its token elements. Within the structure, each token consumes 8 bytes plus contents bytes if contents size exceeds 7 code units.

## TokenTree
A compact tree structure on top of a TokenVector. , intended as syntax treewenn suited to for tokens stored in 


# AbstractToken interface
