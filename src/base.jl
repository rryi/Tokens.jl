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
    category(t::AbstractToken) :: TokenCategory

Current category of given token. A value in 0:15.
Meaning depends on context. [`Lexer`](@ref. uses
the meaning defined in the following constants beginning with "L_"

"""
function category end


################ common category labels  #####################


"Default category: a string without defined syntactic meaning"
const L_STRING :: UInt8 = 0

"whitespace sequence"
const L_WHITE :: UInt8 = 1


"Special character like % ! but no recognized symbol or escape character"
const L_SPECIAL :: UInt8 = 2

"string enclosed in quotes"
const L_QUOTED :: UInt8 = 3

"Identifier, usually a letter/digit sequence"
const L_IDENT :: UInt8 = 4

"A optionally signed number with a decimal separator and/or decimal exponent"
const L_FLOAT :: UInt8 = 5

"all characters up to but excluding a termination sequence"
const L_SEQ1 :: UInt8 = 6

"all characters up to but excluding a termination sequence"
const L_SEQ2 :: UInt8 = 7

"A comment, including termination sequences"
const L_COMMENT :: UInt8 = 8

"End of line sequence, 1 or 2 characters"
const L_EOL :: UInt8 = 9

"A symbol, a special character sequence "
const L_SYMBOL :: UInt8 = 10

"string enclosed in single quotes, in many applications a single character"
const L_CHAR :: UInt8 = 11

"identifier recognized as keyword"
const L_KEY :: UInt8 = 12

"Sequence of digits with optional leading sign"
const L_INT :: UInt8 = 13

"all characters up to but excluding a termination sequence"
const L_SEQ3 :: UInt8 = 14

"all characters up to but excluding a termination sequence"
const L_SEQ4 :: UInt8 = 15


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
