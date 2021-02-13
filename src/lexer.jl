# generic Lexer interface and implementation

#using BitFlags


"""
An Iterator for tokens.

#Interface requirements:

  * a field *source::String* holding the text to parse

  * iterate implementation

  * state in iterate is a simple *Int*: the index of the 1st code unit
    of the next token in *source*

  * returned element in iterate is of type *FlyToken*, its buffer
    is *source*

#Optional interface elements

  * iterate temporarily stores additional category-specific state in its lexer
    instance which is valid until the next iterate call. Access by
    category-specific methods

  * iterate may skip code units before and after the text in returned token,
    e.g. delimiters, whitespace, comments. If something is skipped,
    but might be needed by the calling code, it should be stored in some
    temporary state.

  * iterate may or may not encode code units directly in returned FlyToken,
    depending on efficiency considerations: if token's code units are
    frequently accessed afterwards, or if tokens are stored for a longer time
    independently from *source*, direct encoding is recommended.

  * for certain categories, iterate may assure to store code units in
    returned token directly. This has to be documented as a post condition.
    Calling code could probably annotate subsequent token use with @inbounds.

  * iterate may optionally replace a recognized keyword or symbol by a
    normalized code unit representation which is directly encoded in the
    returned FlyToken. This should be clearly documented in the
    iterate method. Example: "<=" and "≤" could be matched onto a single
    representation, allowing to compare against other FlyToken-s with
    fast 64-bit compare primitives (instead of string comparing loops).
    Keywords longer than 7 code units could be replaced by an
    abbreviation (which must be chosen to be unique within the token category).

  * iterate may skip code units before and after the token text,
    e.g. delimiters, whitespace, comments. If something is skipped,
    but might be needed by the calling code, it could be stored in some
    temporary state

"""
abstract type AbstractLexer
end

"Lexer elements are tokens"
Base.eltype(::Type{AbstractLexer}) = Token


## Lexer Type variants
abstract type Asset end

abstract type Property <: Asset end




"""
Lexer is an iterator for tokens
"""
mutable struct Lexer{IO<:MaybeIO, } <: AbstractLexer
    source :: IOShared{IO}
    "class"
    class :: Vector{UInt8}
end



function iterate(l::SimpleLexer, state::Int = 1)
    (state>=1) && (state <= l.source.ncodeunits()) && return nothing

end
