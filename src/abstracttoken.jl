
## basic type declarations and utilities
"""
A categorized string, supertype of all token types in this module.

In addition to the AbstractString API, an AbstractToken has a
category which roughly classifies its content.
It is accessed by [`category`](@ref)(t::AbstractToken).
Token categories are typed as [`Nibble`](@ref) and internally stored in 4 bits.
For highest performance and still high readability, use named constants 
in your application to indicate semantic meaning in your context.
Even better for readability and debugging is the use of Enum types for
token category. Have a look at the quite generic [`TCategory`](@ref) 
as a starting point

For comparisons and hashing, tokens are treated as strings. Two tokens with
equal content but different category are treated equal and have the
same hash value.

# Missing support

Some Missing and Nothing support is already built into all AbstractToken subtypes: 
they have an internal encoding for missing and nothing, and all concrete subtypes
must have a constructor with one parameter of type Missing resp. Nothing,
which construct a token with that encoding.
ismissing(t::AbstractToken) and isnothing(t::AbstractToken) will return true for this encoding,
and Tests with == and === will compare missing/nothing against these encodings.

This is sufficient for most applications. For vectors of tokens, you can explicitly specify
a subtype of Union{Token,Missing,Nothing} for element types, enabling explicit
missing and/or nothing values on array read access (internal encodings are 
converted to missing or nothing).

# string literals

For the three most important token types  [`DToken`](@ref),
[`BToken`](@ref) and  [`HToken`](@ref), there are special string literal
macros implemented. They use the first letter D,B or H plus a capitalited
hexadecimal character as string prefix to define type and category.
E.g.: D2"hello" is equivalent to DToken(Nibble(2),"hello"), and
HB"hello" will create HToken(Nibble(11),"hello").

# token properties

A token t :: T <: AbstractToken supports the following readonly properties,
implemented by a getproperty method definition

    * t.cat :: Nibble category of token, === category(t)

    * t.ofs :: UInt32 offset into internal buffer, === offset(t)

    * t.len :: UInt64 number of code units, === usize(t)


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
for very short tokens with [`DToken`](@ref), a
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

[`HToken`](@ref) is a combination of DToken and BToken in
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


"""
strings known to have codeunit type UInt8 and Utf8-Encoding.

They support the AbstractString API and additionally 
the methods [`usize`](@ref) and [`offset`](@ref)
"""
const Utf8String = Union{String,SubString{String},AbstractToken}

offset(s::String) = 0

offset(s::SubString) = s.offset

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
    throw(MethodError(isdirect, (t,)))
end


"""
    category(t::AbstractToken) -> Nibble

Current category of given token. A value in 0:15.
Meaning depends on context.

"""
function category(t::AbstractToken)::Nibble
    throw(MethodError(category, (t,)))
end


# Token category default definitions
# use is optional, except in Token constructors
# categories are ordered by groups.
# category constants start with "T_".
# Names in in Tokens which begin with "T_"
# must be a category constant

## Category group 1: used in Token constructors and parsers

"""
# T_END = 0

A token signalling some end of some structure. 

Token constructors with argument *nothing* create a token of this category 
and empty contents. 

An empty T_END token can be returned if an attempt is made to process
beyond end of data.

It may contain a string, e. g. the end of a node
in XML, or an end-of-line sequence.

There are grammars, where a structure end has a lexical
representation and could be identified by a lexer. Common examples are tokens like
"end", ")", "}", "]", ";". But the end of a semantical sequence can also be 
defined by context data, like indentation level, or operator precedence rules,
which have no lexical characterization. In such cases, the parser could generate T_END 
tokens.

In many grammars, whitespace always terminates a token, and is allowed between tokens.
Usually, lexers will skip whitespace. If you need a lexer which returns whitespace
as a token, consider using T_END tokens for it.

In a  [`TokenTree`](@ref), empty T_END tokens are used internally to terminate a list of children.
For this reason, empty T_END tokens are not allowed in a TokenTree as a node.
"""
const T_END = Nibble(0)


"""
# T_INT = 1

An integer value with decimal encoding. Details depend on the lexer: it could be restricted to
a sequence of digits, or allow a leading sign, like '+' or '-', and even group separators
like ',' in English locales.

Token constructors with an Integer argument create a token of category 
T_INT and its decimal string representation as content.
"""
const T_INT = Nibble(1)


"""
## T_REAL = 2

An optionally signed number with a decimal separator and/or decimal exponent.

Token constructors with a Real argument create a token of category 
T_REAL and its default string representation as content.
"""
const T_REAL = Nibble(2)


"""
## T_CHAR = 3

Character token. Token content is usually one Utf8-coded character.
Typical lexers recognize a string enclosed in single quotes as category T_CHAR,
and will usually remove the leading and trailing quotes.

Token constructors with an AbstractChar argument create a token of category 
T_CHAR and its Utf8 string representation as content.
"""
const T_CHAR = Nibble(3)


"""
## T_TEXT = 4

Some text which was not (fully) tokenized according to the preceding token categories.
This is the default category for tokens used in general string processing.
Example: text entities in XML. 

A token constructor with a single AbstractString argument (which is no token)
creates a token of category T_TEXT.


"""
const T_TEXT = Nibble(4)


"""
## T_IDENT = 5

An identifier in a lexer context.

Typical rules are: 1st character is a letter, following characters are
letters or digits. Some special characters like '_' could also
appear in a T_IDENT token. 

Lexers working on byte level might treat all Non-ASCII unicode characters
either as letter or as non-letter. 

A token constructor with a single Bool argument creates a token of category T_IDENT
and content "false" or "true".
"""
const T_IDENT= Nibble(5)


"""
## T_SPECIAL = 6

A sequence of special characters, which has no syntactical meaning to the
lexer. Special characters which identify another lexical construct are
not reported as T_SPECIAL. Examples are characters , like quotes,
or are recognized as operator or symbol
, and not recognized as symbol
or operator token.

Lexers decide to report each special character as its own token, or
report a contiguous sequence of special characters as one token.

Token constructors with argument *missing* create a T_SPECIAL token
with empty content.


"""
const T_SPECIAL = Nibble(6)



## Category group 2: used in lexers and parsers


"""
## T_QUOTE = 7

A string enclosed in some form of quotes. 

Double quotes are a typical case. Parsers will normally remove the leading
and trailing quotes, and resolve escape sequences. Common escape rules are 
doubling quotes inside a quoted string, or the use of a dedicated escape character,
like backslash. 

This category is simple to detect by a simple lexer (determined by its 1st character), 
but may need post processing. See [`QuoteLA`](@ref) for an example.

"""
const T_QUOTE = Nibble(7)





"""
## T_OP = 8

A sequence starting with a special character like '+' and '-', which has several alternative
lexical continuations to a complete token. The name T_OP shall indicate the use as operator,
and the different case of an option (like in command line arguments). 
Typical scenarios for '+' and '-' are:

 * number scenario: a sign is immediately followed by a digit sequence.

 * operator scenario: sign and optionally further special characters define an operator Symbol, e.g. '+='

 * option scenario: a sign followed by an identifier, giving a set or unset option.

Recommendation: a lexer should return the char sequence of one of the given scenarios, and
the parser will test the length and check for the other scenarios if length is 1.  
    
The default lexer definition tries to continue with digits, which is probably the
most common case. If the parser context requires to consider the sign as an operator,
and not part of a number, the parser has to break the raw token into two tokens.
If the raw token has length 1, it implies that no number but an operator has to be parsed.
Alternative: define no continuation bytes for T_OP.

Alternatively, a lexer could return the char sequence of one of the given scenarios, and
the parser will test the length and check for the other scenarios if length is 1. 
See [`OpLA`](@ref) for an implementation.

T_OP is the first category of tokens which can have children in a TokenTree.  
"""
const T_OP = Nibble(8)



"""
## T_COMMENT = 9

comment. Typically contains the pure comment text, without delimiters.
Delimiters may be accessible via lexer context. Many computer languages support
different comment flavours like inline, rest of line or multiline comments.

Comments can be used to implement extensions to some base language. 
They are ignored by the base language parser, but parsed in a later stage.
For this reason, a comment token is allowed to have children in a TokenTree. 
"""
const T_COMMENT = Nibble(9)


# group (3): higher level categories, intended for parsers and token trees


"""
## T_KEY = 10

Identifier recognized as keyword. 

Lexers may support different nonations for a keyword, and return a unique 
'canonical' representation,
e. g. converting SQL keywords to uppercase. If all T_KEY strings have a
canonical representation with less than 8 code units, your application could
restrict T_KEY tokens to type DToken. Even if the grammar has longer keywords,
you can define a short form fitting into a DToken as canonical representation,
to enable use of DToken in the following processing steps.

Reserved words in programming languages are usually tokenized as T_KEY.
In a TokenTree, they can have children, e. g. condition and action for
control structures like IF/THEN/ELSE or FUNCTION parameters and code.
"""
const T_KEY = Nibble(10)


"""
## T_SYM = 11

One or more special characters which form a semantically interpreted
symbol, e.g. "*", ">>>" or "+=". A lexer may accept different notations for the
same symbol, e. g. "<>", "!=" and "≠" for inequality,
and may return a normalized notation.

Symbols usually fit into a DToken. Consider to grant this in your
application (and use DToken for symbols).

In TokenTree, symbols can have children, to reflect the use of symbols
as operators in common computer languages.

A token t of category T_SYM and empty content is regarded a missing
value, i.e. ismissing(t) returns true.


"""
const T_SYM = Nibble(11)




"""
## T_CMD = 12

embedded commands and processing instructions. 

In a TokenTree, it may have children which represent 
the parsed contents of the instruction.

A lexer will typically identify a processing instruction by some unique prefix
and suffix. In the first parser stage, the text between prefix and suffix may be
stored as unparsed text in a T_CMD token. When building a syntax tree,
it might get parsed (probably with a different parser).

Examples are DTD and PI in XML, embedded javascript code in HTML,
processing instructions in text templates like XSLT.

From a lexer-s view, T_CMD is structurally similar to quoted strings and comments: 
a sequence of unparsed content with a delimiter sequence at its begin and end, 
which is excluded from the returned token.
"""
const T_CMD = Nibble(12)


"""
## T_REC = 13

record structure: a node in TokenTree with children.

T_REC is recommended if children access is based on a key attribute which
identifies them within the children list, like a field name in a julia struct or the
attribute name in an XML item. Often, children have different data types and
a static list of allowed keys exists.

A specialized lexer may "abuse" T_REC for a lexical structure, e.g. a match 
against a certain regular expression, like a date format.

"""
const T_REC = Nibble(13)


"""
## T_LIST = 14

list: a node in TokenTree with children.
T_LIST is recommended if children access is based on the sequence order,
like julia vectors or JSON arrays. Often, all children have the same data type.

A specialized lexer may "abuse" T_LIST for a lexical structure, e.g. a match
against a certain regular expression, like a date format.
"""
const T_LIST = Nibble(14)



"""
## T_EXT = 15

Extension category.

No predefined syntax or semantic, define it application-specific. Can have children in 
a TokenTree.
"""
const T_EXT = Nibble(15)



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
    first <= last || return T(t.cat)
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

usize32(s) = usize(s) % UInt32



"""
returns the category of a token as a Nibble
"""
function category end


## pseudo properties

function Base.getproperty(a::AbstractToken, s::Symbol)
    if s === :ofs
        return offset(a)
    elseif s === :len
        return usize(a)
    elseif s === :cat
        return category(a)
    else
        return Core.getproperty(a, s)
    end
end



## AbstractString API implementations which need no specialization

Base.codeunit(t::AbstractToken) = UInt8

Base.ncodeunits(t::AbstractToken) = t.len%Int


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




