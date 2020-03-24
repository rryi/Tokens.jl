
# basic type declarations and utilities

# abstract type AbstractToken{C} where {C<:Integer} <: AbstractString


"""
A categorized string, supertype of all token types in this module.

In addition to the AbstractString API, an AbstractToken has a
category which roughly classifies its content.
It is accessed by [`category`](@ref)(t::AbstractToken).
Token categories are technically restricted to 16 different values.
They are defined as enum type [`TCategory`](@ref), which fits for many
scenarios.

# interface requirements

Every implementation must use Utf8 encoding: token methods operate
mostly on Utf8 code units and rely on its properties.

In addition to string concatenation, concatenation with a code unit
(of type UInt8) must be implemented.

# implementations in this module

This module supplies a very memory efficient implementation
for very short tokens with [`DirectToken`](@ref), a
flyweight string plus category in 8 bytes. It can act as a
substitute for short String instances, avoiding any allocation and
indirect (pointer) access. Data locality is improved, gaining further speed
advantages by better CPU cache use.

[`BufferToken`](@ref) stores larger strings, up to 2^27-1 code units, but needs
an additional code unit buffer. Tokens can share the same buffer, reducing
overhead for heap hamagement and even re-use content.
The token string size limit is far beyond the needs for text tokenizing and
should cover most general string processing needs. Files up to 130 MByte can
be stored in a BufferToken - much more than common recommendations for
processing chunks which suggest some KByte to some MByte.

[`HybridToken`](@ref) is the combination of DirectToken and BufferToken in
one type.

TinyToken is the abstract super type of the types presented above, subtypes
are 64bit flyweight token values. They are used directly in situations where
no buffer is needed or known from context.
See [`TokenVector`](@ref) as an example.

[`PToken`](@ref) bundles a TinyToken with a buffer, [`Token`](@ref) is PToken
using a HybridToken and the recommended immutable token type for general use.
PToken is quite similar to [`SubString`](@ref), with reduced maximal length
in favor of a category field.

[`MutableToken`](@ref) implements safe content changes without buffer
reallocation. It tracks references to its buffer, changes in a buffer
segment referenced elsewere cause buffer reallocation. Its logic is
also used in [`TokenVector`](@ref) which implements a token array
using one common buffer.


"""
abstract type AbstractToken <: AbstractString
end



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
function offset(t::AbstractToken) -> UInt
    throw(MethodError(offset, (t,)))
end


"""
    isDirecty(t::AbstractToken) -> Bool

true if t stores its code units directly in its TinyToken.
"""
function isdirect(t::AbstractToken)
    throw(MethodError(offset, (t,)))
end


"""
    category(t::AbstractToken) -> TCategory

Current category of given token. A value in 0:15.
Meaning depends on context. [`Lexer`](@ref) uses
the meaning defined in the following constants beginning with "T_"

"""
function category (t::AbstractToken)
    throw(MethodError(offset, (t,)))
end
"""
Token category definitions

# group (1): character sequences based on character classes

## T_WHITE = 0

A sequence of whitespace characters.
May include end of line characters, if *T_EOL* is not used in a
certain lexer context.

## T_IDENT = 1

An identifier in the lexer context.
Typical rules are: 1st character is a letter, following characters are
letters or digits. Some special characters like '_' or '$' could also
appear in a T_IDENT token.

## T_SPECIAL = 2

A sequence of special characters, which are not used as delimiters of
other lexical construct like quotes, and not recognized as symbol.

## T_INT = 3

A sequence of digits, may have leading sign. usually '+' or '-', but a lexer
can recognize other characters as a sign, like '$'


# group (2): lexer categories determined by 1st code unit

## T_QUOTED = 4

A string enclosed in double quotes '"'. Lexers will typically remove the leading
and trailing quotes, and will not allow double quotes inside. However,
there are lexer implementations which support some escape mechanism which
allows putting double quotes in a quoted string.

## T_CHAR = 5

A string enclosed in single quotes. Some lexer will require exactly one
character.

## T_EOL = 6
End of line sequence, 1 or 2 characters, typically a TinyToken.
In a context where line breaks have no syntactical meaning, a lexer can
treat end of line characters as whitespace and never report T_EOL.

# group (3): tokens which specialize or extend tokens from group 1-3

## T_FLOAT = 7

An optionally signed number with a decimal separator and/or decimal exponent

## T_COMMENT = 8

comment. Contains the pure comment text, not its delimiters.
Delimitersmay be accessible via lexer context"

## T_TEXT = 9

Some text without defined lexical or semantic properties.
This is the default category for tokens used in general string processing.

In a lexer context it is recommended for text which is excluded from lexical
analysis, e. g. if some escape sequence is found. The escape sequence can
be reported as preceding token (this requires the lexer to maintain ins state
as 'in escape mode'), or as token which can be accessed just after parsing
this token.


# group (4): categories used in parsers and TokenTree, not defined lexically


## T_END = 10

End of some token sequence. It may contain a string, e. g. the end of a node
in XML, or may be an empty token, e. g. if reported by the lexer on an
attempt to parse beyond end of data.

In a TokenTree, for each token having of one of the following
categories, there must be a T_END Token to close the sequence of its
children.

## T_SYMBOL = 11

One or more special characters which form a semantically interpreted
symbol, e.g. "*", ">>>" or "+=". A lexer may accept different notations for the
same symbol, e. g. "<>", "!=" and "≠" for inequality,
and may replace different notations by one of the others.

Symbols usually fit into a DirectToken. Consider to grant this in your
application (and use DirectToken for symbols).

In TokenTree, symbols can have children, to reflect the use of symbols
as operators in common computer languages.

## T_KEY = 12

Identifier recognized as keyword, typically a TinyToken. Lexers may support
different nonations for a keyword, and return a 'canonical' representation,
e. g. converting SQL keywords to uppercase. If all T_KEY strings have a
canonical representation with less than 8 code units, your application could
restrict T_KEY tokens to type DirectToken.

Reserved words in programming languages are usually tokenized as T_KEY.
In a TokenTree, they can have children, e. g. condition and action for
control structures like IF/THEN/ELSE or FUNCTION parameters and code.

## T_PI = 13

processing instruction. In a TokenTree, its children represent the parsed
content of the instruction.

A lexer will typically identify a processing instruction by some unique prefix
and suffix, return the text between as unparsed text in a T_PI
token, and make prefix and suffix accessible via its context.
The caller will then parse the T_PI content (e.g. by calling another Lexer).

Examples are DTD and PI in XML, embedded javascript code in HTML,
processing instructions in text templates like XSLT.

## T_STRUCT = 14

structure: a node in TokenTree with children.
T_STRUCT is recommended if children access is based on a key attribute which
identifies them within the children list, like a field name in a julia struct or the
attribute name in an XML item. Often, children have different data types and
a static list of allowed keys exists.

## T_LIST = 15

list: a node in TokenTree with children.
T_LIST is recommended if children access is based on the sequence order,
like julia vectors or JSON arrays. Often, all children have the same data type

# special string literals for tokens

For every enum value, a special string literal syntax is defined
to create token literals of the appropriate category and most
efficient representation. Exampie:

T_INT"123"

generates a DirectToken(T_INT,"123").

"""
@enum TCategory :: UInt8 begin
    T_WHITE = 0
    T_IDENT = 1
    T_SPECIAL = 2
    T_INT = 3
    T_QUOTED = 4
    T_CHAR = 5
    T_EOL = 6
    T_FLOAT = 7
    T_COMMENT = 8
    T_TEXT = 9
    T_END = 10
    T_SYMBOL = 11
    T_KEY = 12
    T_PI = 13
    T_STRUCT = 14
    T_LIST = 15
end

# Generation of the string literal macros for tokens
for cat in instances(TCategory)
    eval(quote
        macro $(Symbol(Symbol(cat),"_str"))(txt)
            if ncodeunits(txt)>7
                :(BufferToken($($(cat)),$txt))
            else
                :(DirectToken($($(cat)),$txt))
            end
        end
    end)
    export Symbol(cat), @Symbol(Symbol(cat),"_str")
end


"""
    subtoken(offset::UInt32, size::UInt64, t::T<:AbstractToken)

central method to construct a new token of the same type which
holds a sequence of code units of its source t.

It uses an (offset::UInt32,size::UInt64) pair to specify the
code units in the result: skips offset code units and keeps the
following size code units in a newly constructed token.

In most cases, it is a "view" operation like SubString
construction. Exceptions are very short sequences for token
types which allow for direct encoding without buffer.

Specifying concrete Integer subtypes may look uncommon to
Julia, but is for safety and efficieny: in most token
operations, it is exactly the integer type needed, and having
different types for offset and size reduces the risk that these
parameters are mixed-up: it is difficult enough for the user
to distinguish the common string (first,last) index notation
from the performance-oriented lowlevel (offset,size) code unit
segment notation. The need for explicit conversion of offset and
size hopefully helps to to avoid mix-up.

There is another subtoken method which uses an (first,last)
character index sequence like usual String and SubString
operations. The (offset,size) oriented operation is faster than
the form subtoken(t,first,last) , but less safe: you can construct
tokens which are no valid strings, because there is no check
whether t[offset+1] is a valid character index or
codeunit(t,offset+size) is the last code unit of a character.
"""
subtoken(offset::UInt32, size::UInt64, t::T<:AbstractToken) = T(offset,size,t)



"""
    subtoken(t::AbstractToken, first::Int, last::Int)

Like SubString with same parameters, but returns
a new token of same type with the same category
and content SubString(string(t),first,last).

"""
subtoken(t::AbstractToken, first::Int, last::Int)
    first ≤ last || return T(0,0,t)
    @boundscheck begin
        checkbounds(t, first:last)
        @inbounds isvalid(t, first) || string_index_err(t, first)
        @inbounds isvalid(t, last) || string_index_err(t, last)
    end
    nextlast = nextind(t,last)
    subtoken(UInt32(first)-1,UInt64(nextlast-first),t)
end


#= try with default implementation, using isvalid ...

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

Base.ncodeunits(t::AbstractToken) = sizeof(t)


# simplified implementation, assuming t is valid UTF8.
# isvalid only checks current byte. It does not check if all code units
# of the unicode character exist and have valid values.
Base.isvalid(t::AbstractToken, i::Int) = codeunit(t, i) & 0xc0 != 0x80



"generic comparison by code units - specialize on TinyToken-s"
function cmp_codeunits(a::Utf8String, b::Utf8String)
    al, bl = sizeof(a), sizeof(b)
    ml = min(al, bl)
    i = 1
    @inbounds while i<=ml
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


#TODO dump impl.?
"show tokens as custom string constant without token type"
function Base.show(io::IO,t::AbstractToken)
    print(io,category(t))
    Base.print_quoted(io, t)
end


function Base.write(io::IO, t::AbstractToken)
    i = 1
    n = ncodeunits(t)
    @inbounds while (i<=n)
        write(io,codeunit(t,i))
        i += 1
    end
end



#########################################################
####################### helpers #########################
#########################################################


# TODO delete
"helper function: bounds check failure"
boundserr(t,i) = throw(BoundsError(t,i))


"an empty string buffer"
const EMPTYSTRING = ""
