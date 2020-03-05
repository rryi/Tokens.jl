module Tokens

using BitFlags

include("base.jl")
include("tinytoken.jl")
export AbstractToken, TinyToken
include("token.jl")
export Token, MutableToken
#using TreeOnVectors
include("tokenvector.jl")
export TokenVector, TokenTree
include("lexer.jl")
export AbstractLexer, Lexer
#export

end # module
