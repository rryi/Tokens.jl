
# basic type declarations and utilities

# abstract type AbstractToken{C} where {C<:Integer} <: AbstractString


"""
A categorized string, supertype of all token types in this module.

In addition to the AbstractString API, an AbstractToken has a
category which roughly classifies its content.
It is accessed by [`category`](@ref)(t::AbstractToken).
Its meaning is context dependent. See  [`Lexer`](@ref)
for a concrete example.

# interface requirements

Every implementation must use Utf8 encoding: token methods operate
mostly on Utf8 code units and rely on its properties.

In addition to string concatenation, concatenation with a code unit
(of type UInt8) must be implemented.

# implementations in this module

This module supplies a very memory efficient implementation
for very short tokens with [`TinyToken`](@ref), a
flyweight string plus category in 8 bytes. It can act as a
memory efficient substitute for short String instances, avoiding
any allocation and indirect (pointer) access. Data locality is
improved, gaining speed advantages by better CPU cache use.

TinyToken can also store larger strings, up to 2^27-1 code units, if an
additional code unit buffer is supplied. Tokens can share the same buffer:
storing many TinyToken but only one buffer reduces memory needs.
See [`TokenVector`](@ref) as an example.

For convenience,  [`Token`](@ref) bundles a TinyToken with a buffer.
Token is quite similar to [`SubString`](@ref), but can operate on short
strings without buffer access, and reduces maximal length in favor of
a category field.

[`MutableToken`](@ref) implements safe content changes without buffer
reallocation. It tracks references to its buffer, changes in a buffer
segment referenced elsewere cause buffer reallocation. Its logic is
also used in [`TokenVector`](@ref) which implements a string array
using one common buffer


"""
abstract type AbstractToken <: AbstractString
end

"""
A flyweight data structure for an immutable Token.

All subtypes must be 64 bit primitive data types which implement a certain
bitmap layout and conventions for processing it. 

"""
abstract type TinyToken <: AbstractToken



"These string types have methods operating with Utf8 code units"
const Utf8String = Union(String,SubString{String},AbstractToken)


#########################################################
################# AbstractToken API #####################
#########################################################


"""
    offset(t::AbstractToken)

number of code units in the buffer associated with an token
before the first code unit belonging to the token.

If code units are stored directly in a TinyToken, offset is 0.

Buffer is not needed in this function, no consistency or bounds
checks are performed.
"""
function offset end


"""
    category(t::AbstractToken) :: UInt8

Current category of given token. A value in 0:15.
Meaning depends on context. [`Lexer`](@ref) uses
the meaning defined in the following constants beginning with "T_"

"""
function category end


################ common category labels  #####################

# treatment of nonascii characters is parser specific, but any parser
# must keep characters within one token.

# token categories which grant direct encoding in TinyToken may be normalized,
# i.e. parser may replace parsed text with some normalized representation
# which ensures uniqueness fpr a semantic meaning. E.g. "<>", "≠", "/neq" and
# "!=" could all be represented by a symbol with string "!=".

# group (1): token category determined by 1st codeunit,
# token contains all following codeunits belonging to
# a codeunit set associated with that category.

"whitespace sequence"
const T_WHITE = UInt64(0)

"Identifier, usually a letter/digit sequence"
const T_IDENT = UInt64(1)

"Special character sequence, no known semantic (no letter, digit, white, symbol)"
const T_SPECIAL = UInt64(2)

"Sequence of digits, may have leading sign"
const T_INT = UInt64(3)

# group (2): token category determined by 1st byte,
# token contains all following bytes until termination byte is
# found, typically the 1st byte again.
# Parser may implement some way to include
# 1st byte in token, by an escape prefix byte or by
# doubling 1st byte.

"string enclosed in double quotes"
const T_QUOTED = UInt64(4)

"string enclosed in single quotes, typically a TinyToken (one character)"
const T_CHAR = UInt64(5)

# group (3): token category determined by 1st byte,
# individual rules for consuming more bytes into token

"symbol (special characters with semantic meaning), typically a TinyToken"
const T_SYMBOL = UInt64(6)

"End of line sequence, 1 or 2 characters, typically a TinyToken"
const T_EOL = UInt64(7)


# group (4): tokens which specialize or extend tokens from group 1-3

"identifier recognized as keyword, typically a TinyToken"
const T_KEY = UInt64(8)

"An optionally signed number with a decimal separator and/or decimal exponent"
const T_FLOAT = UInt64(9)

"comment, pure comment text. Delimiters accessible via lexer context"
const T_COMMENT = UInt64(10)

# group (5): categories for parsers and TokenTree, not lexically deducted
# lexer can use these categories for text which is enclosed by a prefix
# symbol and a suffix symbol and is not lexically analyzed

"end-of-data, marks subtree end in TokenTree, lexer: end of data / nothing"
const T_EOD = UInt64(11)

"Default category: some text, no lexical analysis performed"
const T_TEXT = UInt64(12)

"processing instruction: nonleaf AST node, lexer: symbol-delimited char sequence"
const T_PI = UInt64(13)

"structure: nonleaf AST node, lexer: symbol-delimited char sequence"
const T_STRUCT = UInt64(14)

"sequence: nonleaf AST node, lexer: symbol-delimited char sequence"
const T_SEQ = UInt64(15)


"""
    subtoken(t::AbstractToken, first::Int, last::Int)

Like SubString with same parameters, but returns
a new token of same type with the same category
and content SubString(t,first,last)

"""
subtoken(t::T<:AbstractToken, first::Int, last::Int) = T(t,first,last)



#= try with default implementatio  using isvalid ...

function Base.thisind(t::AbstractToken, i::Int)
    if i<=0
        @boundscheck i==0 || boundserr(t,i)
        return 0
    else
        lenp1 = ncodeunits(t)+1
        if i>=lenp1
            @boundscheck i==lenp1 || boundserr(t,i)
            return i
        end
        # now we have a normal case: i is inbound index
        # copy/paste from strings.jl
        @inbounds b = codeunit(t, i)
        (b & 0xc0 == 0x80) & (i-1 > 0) || return i
        @inbounds b = codeunit(t, i-1)
        between(b, 0b11000000, 0b11110111) && return i-1
        (b & 0xc0 == 0x80) & (i-2 > 0) || return i
        @inbounds b = codeunit(t, i-2)
        between(b, 0b11100000, 0b11110111) && return i-2
        (b & 0xc0 == 0x80) & (i-3 > 0) || return i
        @inbounds b = codeunit(t, i-3)
        between(b, 0b11110000, 0b11110111) && return i-3
        return i
    end
end
=#

#########################################################
#### API implementations which need no specialization ###
#########################################################

Base.codeunit(t::AbstractToken) = UInt8

Base.sizeof(t::AbstractToken) = ncodeunits(t)

# simplified - true does not grant getindex returns a valid unicode
Base.isvalid(t::AbstractToken, i::Int) = codeunit(t, i) & 0xc0 != 0x80



"generic comparison by code units"
function cmp_codeunits(a::AbstractToken, b::AbstractToken)
    al, bl = sizeof(a), sizeof(b)
    ml = min(al, bl)
    i = 1
    while i<=ml
        ai = codeunit(a,i)
        bi = codeunit(b,i)
        ai < bi && return -1
        ai > bi && return 1
        i += 1
    end
    al < bl && return -1
    al > bl && return 1
    0
end

Base.cmp(a::AbstractToken, b::AbstractToken) = cmp_codeunits(a,b)

#= use default
function Base.==(a::AbstractToken, b::AbstractToken)
    cmp_codeunits(a,b) == 0
end
=#


function Base.show(io::IO,t::AbstractToken)
    print(io,'^',category(t))
    tshort = t
    if ncodeunits(t)>30
        tshort = subtoken(t,1,30) * "..."
    end
    Base.print_quoted(io, tshort)
end


function Base.write(io::IO, t::AbstractToken)
    i = 1
    n = ncodeunits(t)
    while (i<=n)
        write(io,codeunit(t,i))
        i += 1
    end
end



#########################################################
####################### helpers #########################
#########################################################


"helper function: bounds check failure"
boundserr(t,i) = throw(BoundsError(t,i))


"an empty string buffer"
const empty::String = ""
