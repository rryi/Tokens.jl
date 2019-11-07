# generic Lexer interface and implementation

#using BitFlags

################ category constants for lexer #####################


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

"A number with a decimal separator and/or decimal exponent"
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

""
const L_ :: UInt8 = 1

""
const L_ :: UInt8 = 1

""
const L_ :: UInt8 = 1

""
const L_ :: UInt8 = 1

""
const L_ :: UInt8 = 1

""
const L_ :: UInt8 = 1






   # default: some Utf8 string, maybe empty
    T_SPECIAL = 1  # special character (not white,letter, digit, symbol)

    T_WHITE = 1  # whitespace, might contain codes >127
    # delimited token, can contain escapes
    T_CHAR = 2    # delimited token, delimiter removed
    T_COMMENT = 3 # delimited token, delimiter removed
    T_IDENT = 4   # identifier in UTF8
    U_WHITE = 5   # whitespace, might contain codes >127
    T_FLOAT = 6  # decimal and EXP format, might contain codes >127
    T_SYMBOL = 7  #
    I_STRING = 8   # some string, ISO-8851-1 encoded
    I_ESCAPED = 9  #
    I_CHAR = 10   #
    I_COMMENT = 11#
    I_IDENT = 12  # identifier
    T_EOL = 13    # end-of-line marker
    T_INT = 14# integer, all codes <128
    I_SYMBOL = 15 #
end
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
