# basic type declarations and utilities

# abstract type AbstractToken{C} where {C<:Integer} <: AbstractString


"""
A categorized string, supertype of all token types in this module.

In addition to the AbstractString API, an AbstractToken has a
category which roughly classifies its content.
It is accessed by [`category`](@ref)(t::AbstractToken).
Its meaning is context dependent. See  [`Lexer`](@ref)
for a concrete example.

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
"""
abstract type AbstractToken <: AbstractString
end

"""
Predefined category semantics for tokens.

The category value range is 0..15.

Its meaning depends on its usage context. This enumeration labels
the usage by [`SimpleParser`](@ref).


 0. T_STRING
    Default category for "some string" without specific lexical meaning.
    Lexer returns a token of this category if text is skipped until
    an end marker is found.
 1. T_SPECIAL
    A token representing a special character. There are Unicode special
    characters which have a specific treatment by Lexer, and result in other
    token categories. This category is for "all the rest" of special
    characters.

 8. T_SYMBOL

    having no specific meaning
    to the lexer. Not all Unicode special
    characters are reported
some string    some string.
1.

Category 0..7 use Utf8 encoding, enum names are prefixed with "U".
Category 8..15 use ISO-8859-1 encoding (also called Latin1), prefixed with "I"

Some token categories imply that only ASCII characters are used. In
these cases Utf8 and ISO-8859-1 encodings are identical, and the
encoding bit is used to specify two different but similar token
categories. This holds for U_WHITE and I_EOL, and U_NUMBER and I_INTEGER.
 e1=1 e2=2


#########################################################
################# AbstractToken API #####################
#########################################################


"parametric type to allow for dispatch on preferred encoding"
struct AsIso{bool}
end

"""
#offset(t::AbstractToken)

number of code units in the buffer associated with an token
before the first code unit belonging to the token.

If code units are stored directly in a TinyToken, offset is 0.

Buffer is not needed in this function, no consistency or bounds
checks are performed.
"""
function offset end

"""
#category(t::AbstractToken) :: TokenCategory

Current category of given token, see  [`TokenCategory`](@ref).

"""
function category :: TokenCategory end


#ensure index is an Int. does that work??!
Base.thisind(t::AbstractToken, i::Integer) = thisind(t,Int(i))

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
        # now we have a normal case: i is valid index, calculate
        isiso(t) && return i # the fast case.
        # poor boy ... we must really find the index. copy/paste from strings.jl
        @inbounds b = codeunit(s, i)
        (b & 0xc0 == 0x80) & (i-1 > 0) || return i
        @inbounds b = codeunit(s, i-1)
        between(b, 0b11000000, 0b11110111) && return i-1
        (b & 0xc0 == 0x80) & (i-2 > 0) || return i
        @inbounds b = codeunit(s, i-2)
        between(b, 0b11100000, 0b11110111) && return i-2
        (b & 0xc0 == 0x80) & (i-3 > 0) || return i
        @inbounds b = codeunit(s, i-3)
        between(b, 0b11110000, 0b11110111) && return i-3
        return i
    end
end

#########################################################
#### API implementations which need no specialization ###
#########################################################

Base.codeunit(t::AbstractToken) = UInt8

Base.sizeof(t::AbstractToken) = ncodeunits(t)

#TODO more base functions
function cmp(a::String, b::String)
    al, bl = sizeof(a), sizeof(b)
    c = ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt),
              a, b, min(al,bl))
    return c < 0 ? -1 : c > 0 ? +1 : cmp(al,bl)
end

function ==(a::String, b::String)
    al = sizeof(a)
    al == sizeof(b) && 0 == ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt), a, b, al)
end



#########################################################
####################### helpers #########################
#########################################################


"helper function: bounds check failure"
boundserr(t,i) = throw(BoundsError(t,i))

"an empty string buffer"
const empty::String = ""
