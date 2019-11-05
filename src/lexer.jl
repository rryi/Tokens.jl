# generic Lexer interface and implementation

#using BitFlags

"""
An Iterator for tokens.

Interface requirements:

  * a field *source::String* holding the text to parse
  * iterate implementation
  * state in iterate is a simple *Int*: the index of the 1st code unit
    of the next token in *source*
  * returned element in iterate is of type *TinyToken*
"""
abstract type AbstractLexer
end

"Leser elements are tokens"
Base.eltype(::Type{AbstractLexer}) = TinyToken


@enum codeCategory :: UInt16 begin
    NONE=0
    WHITE=1 # code unit is whitespace
    EOL # code unit is part of EOL sequence (1 or 2 codes)
    START_IDENT # code unit starts identifier
    START_KEY # there is at least one keyword beginning with this
    SPECIAL # a special character
    START_ESCAPE # start an escape sequence, usually " and '
    DIGIT # some digit
    SIGN # allowed as a prefix of a number, usually + -
    IN_IDENT # code is allowed as part of an identifier
end


"""
Lexer is an iterator for tokens
"""
mutable struct Lexer <: AbstractLexer
    "token source "
    source :: String
    "class"
    class :: Vector{UInt8}
end



function iterate(l::SimpleLexer, state::Int = 1)
    (state>=1) && (state <= l.source.ncodeunits()) && return nothing

end
