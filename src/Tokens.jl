module Tokens

#using BitFlags

include("base.jl")
export AbstractToken, offset, isdirect, category, TCategory
# all TCategory values and its string macros are exported by base.jl
export subtoken, EMPTYSTRING

include("tinytoken.jl")
export TinyToken, FlyToken, DirectToken, HybridToken

include("token.jl")
export Token, BufferToken
#using TreeOnVectors
include("tokenvector.jl")
export TokenVector, TokenTree
include("lexer.jl")
export AbstractLexer, Lexer
#export

end # module
