
## basic type declarations and utilities
"""
A categorized string, supertype of all token types in this module.

In addition to the AbstractString API, an AbstractToken has a
category which roughly classifies its content.
It is accessed by [`category`](@ref)(t::AbstractToken).
Token categories are technically restricted to 16 different values.
They are defined as enum type [`TCategory`](@ref), designed to be used in
lexers and parsers.

For comparisons and hashing, tokens are treated as strings. Two tokens with
equal content but different category are treated equal and have the
same hash value.

# Missing and Nothing support

Missing/Nothing support is already built into all AbstractToken subtypes: 
they have an internal encoding for missing and nothing, and all concrete subtypes
must have a constructor with one parameter of type Missing resp. Nothing,
which construct a token with that encoding.
ismissing(t::AbstractToken) and isnothing(t::AbstractToken) will return true
for these encodings. 

This is sufficient for applications which do all tests with ismissing
and isnothing functions. However comparison operators are not overloaded, so t===nothing
will give false for these encodings. For vectors of tokens, you can explicitly specify
a subtype of Union{Token,Missing,Nothing} for element types, enabling explicit
missing and/or nothing values on array read access (internal encodings are 
converted to missing or nothing).


# interface requirements

Tokens support the AbstractString API. Many methods are defined
with specialized, efficient code. Developing a new AbstractToken subtype
requires overloading of many of them.

Every implementation must use Utf8 encoding: token methods operate
mostly on Utf8 code units and rely on its properties.

There is a Token specific interface besides AbstractString API, in the
source code it is characterized by chapter comments

# implementations in this module

This module supplies a very memory efficient implementation
for very short tokens with [`DirectFly`](@ref), a
short string plus category in 8 bytes. It can act as a
substitute for short String instances, avoiding any allocation and
indirect (pointer) access. Data locality is improved, gaining further speed
advantages by better CPU cache use.

[`BToken`](@ref) stores larger strings, but needs an additional code
unit buffer. Tokens can share the same buffer, reducing overhead for heap
management and even re-use content, like SubString does.

There is a technical size limit for tokens: at most 2^27-1 code units.
It applies to *all* AbstractToken implementations. It might restrict usage in
some general text processing tasks, eg processing huge log files.
Recommended countermeasure is processing chunks of some KByte to MByte,
using [`IOShared`](@ref). Tokenizing a text, usually leads to small fragments
of a couple of bytes. For tokens in the sense of "atoms of text", a size in
the range of a couple of bytes is expected. Text written by humans, like
source code and published books, has sizes up to some MByte.

[`HToken`](@ref) is a combination of DirectFly and BToken in
one type, using no buffer for code unit counts below 8.

# related types and APIs in this module

[`IOShared`](@ref) is simular to IOBuffer but designed to interact smoothly
with tokens. Though mutable, its buffer can safely be shared with tokens
(as well as SubString instances).

[`TokenVector`](@ref) acts as vector of tokens, using one shared buffer for
all vector elements. It uses significantly less memory than a Vector{String},
but might require copying token contents into the buffer used by the TokenVector
when a token is put into the vector. 

[`TokenTree`](@ref) implements a simple tree structure upon a TokenVector. 

[`Lexer`](@ref) implementations have functions for lexical analysis and 
tokenizing of text into tokens
"""
abstract type AbstractToken <: AbstractString
end


"strings known to have codeunit type UInt8 and Utf8-Encoding"
const Utf8String = Union{String,SubString{String},AbstractToken}



#########################################################
################# AbstractToken API #####################
#########################################################


"""
    offset(t::AbstractToken)

number of code units in the buffer associated with an token
before the first code unit belonging to the token.

If code units are stored directly in a FlyToken, offset is 0.

Buffer is not needed in this function, no consistency or bounds
checks are performed.
"""
function offset(t::AbstractToken)
    throw(MethodError(offset, (t,)))
end


"""
    isDirecty(t::AbstractToken) -> Bool

true if t stores its code units directly in its FlyToken.
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
function category(t::AbstractToken)
    throw(MethodError(offset, (t,)))
end


"""
Token category definitions

These token categories describe the general semantics of token categories,
as used in package Token. Applications can redefine the meaning of categories 
within their own context - guidelines are given below.


# group (1): character sequences with fixed character class for 1st and subsequent characters

## T_EOL = 0

End of line sequence, 1 or 2 characters, typically a FlyToken.
A T_EOL token of length 0 is  treated as
nothing value, i.e. isnothing(t) returns true. 
Lexers return it on attempts to read beyond end-of-data

end-of-line characters within a token are never reported asT_EOL.

## T_WHITE = 1

A sequence of whitespace characters.
May include end of line characters, if *T_EOL* is not used.

## T_IDENT = 2

An identifier in the lexer context.

Typical rules are: 1st character is a letter, following characters are
letters or digits. Some special characters like '_' could also
appear in a T_IDENT token. 

Lexers working on byte level might treat all Non-ASCII unicode characters
either as letter or as non-letter. 

## T_SPECIAL = 2

A sequence of special characters, which is not used as delimiter of
another lexical construct like quotes, and not recognized as symbol.
Lexers decide to report each special character as its own token, or
report a contiguous sequence of special characters as one token.

A token t with category T_SPECIAL and empty content is treated as
missing value, i.e. ismissing(t) returns true. Lexers can return
such a token on an attempt to read beyond end of data.

## T_INT = 3

A sequence of digits. Lexers can allow a leading sign, like '+' or '-'.

Token constructors with an Integer argument create a token of category 
T_INT and its decimal string representation as content.

# group (2): lexer categories determined by 1st character

## T_QUOTED = 4

A string enclosed in double quotes '"'. Lexers will typically remove the leading
and trailing quotes. Lexers can recognize escape rules, which allow to embed 
double quotes within the string. Typical rules are doubling quotes inside,
or escape prefixes like "\". 

## T_CHAR = 5

Character token. Token content is usually one Utf8-coded character.
Typical lexers recognize a string enclosed in single quotes as category T_CHAR,
and will usually remove the leading and trailing quotes.

Token constructors with a Char argument create a token of category 
T_CHAR and its Utf8 string representation as content.

# group (3): more complex lexer tokens

## T_REAL = 7

An optionally signed number with a decimal separator and/or decimal exponent.

Token constructors with a Real argument create a token of category 
T_REAL and its Utf8 string representation as content.


## T_COMMENT = 8

comment. Typically contains the pure comment text, without delimiters.
Delimiters may be accessible via lexer context. Many computer languages support
different comment flavours like inline, rest of line or multiline comments.


# group (4): higher level categories, mostly used in parsers and TokenTree

## T_TEXT = 9

Some text which was not (fully) tokenized according to the preceding token categories.
This is the default category for tokens used in general string processing.
Example: text entities in XML. 

A token constructor with a single AbstractString argument (which is no token)
creates a token of category T_TEXT.


## T_END = 10

End of some token sequence. It may contain a string, e. g. the end of a node
in XML, or may be an empty token. In many grammars, T_END tokens have a lexical
representation and could be identified by a lexer. Common examples are tokens like
"end", ")", "}", "]", ";". But the end of a semantical sequence can also be 
defined by context data, like indentation level, or operator precedence rules,
which have no lexical characterization. In such cases, T_END tokens are inserted 
by the parser.

In a  [`TokenTree`](@ref), for each token having of one of the following
categories, there must be a T_END Token to close the sequence of its
children.

An empty T_END token could be returned by lexers if an attempt is made to read
beyond end of data.

## T_SYMBOL = 11

One or more special characters which form a semantically interpreted
symbol, e.g. "*", ">>>" or "+=". A lexer may accept different notations for the
same symbol, e. g. "<>", "!=" and "≠" for inequality,
and may return a normalized notation.

Symbols usually fit into a DirectFly. Consider to grant this in your
application (and use DirectFly for symbols).

In TokenTree, symbols can have children, to reflect the use of symbols
as operators in common computer languages.

A token t of category T_SYMBOL and empty content is regarded a missing
value, i.e. ismissing(t) returns true.


## T_KEY = 12

Identifier recognized as keyword, typically a FlyToken. Lexers may support
different nonations for a keyword, and return a unique 'canonical' representation,
e. g. converting SQL keywords to uppercase. If all T_KEY strings have a
canonical representation with less than 8 code units, your application could
restrict T_KEY tokens to type DirectFly. Even if the grammar has longer keywords,
you can define a short form fitting into a DirectFly as canonical representation,
and restrict T_KEY tokens to DirectFly.

Reserved words in programming languages are usually tokenized as T_KEY.
In a TokenTree, they can have children, e. g. condition and action for
control structures like IF/THEN/ELSE or FUNCTION parameters and code.

## T_PI = 13

processing instruction. In a TokenTree, it may have children which represent 
the parsed content of the instruction.

A lexer will typically identify a processing instruction by some unique prefix
and suffix, return the text between as unparsed text in a T_PI
token, and make prefix and suffix accessible via its context.
It-s up to the caller to parse the T_PI content (e.g. by calling another Lexer).

Examples are DTD and PI in XML, embedded javascript code in HTML,
processing instructions in text templates like XSLT.

From a lexer-s view, T_PI is structurally the same as T_TEXT: a sequence of
unparsed content with a delimiter sequence at its begin and end, 
which is excluded from the returned token.

## T_STRUCT = 14

structure: a node in TokenTree with children.

T_STRUCT is recommended if children access is based on a key attribute which
identifies them within the children list, like a field name in a julia struct or the
attribute name in an XML item. Often, children have different data types and
a static list of allowed keys exists.

A specialized lexer may "abuse" T_STRUCT for a lexical structure, e.g. a match 
against a certain regular expression, like a date format.

## T_LIST = 15

list: a node in TokenTree with children.
T_LIST is recommended if children access is based on the sequence order,
like julia vectors or JSON arrays. Often, all children have the same data type.

A specialized lexer may "abuse" T_LIST for a lexical structure, e.g. a match
against a certain regular expression, like a date format.

# special string literals for tokens

For every enum value, a special string literal syntax is defined
to create token literals of the appropriate category. Exampie:

T_INT"123"

generates a Token(T_INT,"123").

"""
@enum TCategory :: UInt8 begin
    T_WHITE = 0
    T_IDENT = 1
    T_SPECIAL = 2
    T_INT = 3
    T_QUOTED = 4
    T_CHAR = 5
    T_EOL = 6
    T_REAL = 7
    T_COMMENT = 8
    T_TEXT = 9
    T_END = 10
    T_SYMBOL = 11
    T_KEY = 12
    T_PI = 13
    T_STRUCT = 14
    T_LIST = 15
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

There is another subtoken method which uses a first:last
character index sequence like usual String and SubString
operations. The (offset,size) oriented operation is faster than
the form subtoken(t,first,last) , but less safe: you can construct
tokens which are no valid strings, because there is no check
whether offset+1 is a valid character index of t or
codeunit(t,offset+size) is the last code unit of a character.
"""
subtoken(offset::UInt32, size::UInt64, t::T) where T<:AbstractToken = T(offset,size,t)


"""
    subtoken(t::AbstractToken, first::Int, last::Int)

Like SubString with same parameters, but returns
a new token of same type with the same category
and content SubString(string(t),first,last).

"""
function subtoken(t::T, first::Int, last::Int) where T<:AbstractToken
    first <= last || return T(category(T))
    @boundscheck begin
        checkbounds(t, first:last)
        @inbounds isvalid(t, first) || string_index_err(t, first)
        @inbounds isvalid(t, last) || string_index_err(t, last)
    end
    nextlast = nextind(t,last)
    subtoken(UInt32(first-1),(nextlast-first)%UInt64,t)
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


## Token API definition and implementations which need no specialization


# needs overloading!
"""
    function usize(t) :: UInt64

the size in bytes of some text contents. Applicable to token Types,
Strings, IOShared and more.

returned as an UInt64, which is the data type for token
sizes used throughout all token implementations.

Functions ncodeunits and sizeof are derived from usize
"""
function usize end

usize32(s::T) where T = usize(s)%UInt32



"""
returns the category of a token
"""
function category end



## AbstractString API implementations which need no specialization

Base.codeunit(t::AbstractToken) = UInt8

Base.ncodeunits(t::AbstractToken) = usize(t)%Int


## special values for missing and nothing

"AbstractToken uses category T_SYMBOL with usize 0 as encoding for missing"
Base.ismissing(t::AbstractToken) = category(t)==T_SPECIAL && usize(t)==0%UInt64

"AbstractToken uses category T_SPECIAL with usize 0 as encoding for nothing"
Base.isnothing(t::AbstractToken) = category(t)==T_EOL && usize(t)==0%UInt64



#das stimmt so nicht!
#Base.sizeof(t::AbstractToken) = usize(t)%Int



# simplified implementation, assuming t is valid UTF8.
# isvalid only checks current byte. It does not check if all code units
# of the unicode character exist and have valid values.
Base.isvalid(t::AbstractToken, i::Int) = codeunit(t, i) & 0xc0 != 0x80

# Not helpful. Julia Base uses optimized c code with pointers.
# Base.cmp(a::Utf8String, b::Utf8String) = cmp_codeunits(a,b)

#= use default?checkbounds
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


## helpers ##

