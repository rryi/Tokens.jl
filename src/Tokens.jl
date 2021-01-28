module Tokens

using MurmurHash3
#using BitFlags

include("checks.jl")
include("abstracttoken.jl")
export AbstractToken, offset, isdirect, category, TCategory, usize
export subtoken, tread, twrite
# all TCategory values and its string macros are exported by abstracttoken.jl

include("substring.jl")
include("packed31.jl")

include("flytoken.jl")
export DirectFly

include("token.jl")
export Token, BToken
include("ioshared.jl")
export IOShared
#using TreeOnVectors
include("tokenvector.jl")
export TokenVector, TokenTree
include("lexer.jl")
export AbstractLexer, Lexer
#export

end # module
