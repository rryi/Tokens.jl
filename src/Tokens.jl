module Tokens

using MurmurHash3
#using BitFlags

include("base.jl")
export AbstractToken, offset, isdirect, category, TCategory, usize
# all TCategory values and its string macros are exported by base.jl
export subtoken, EMPTYSTRING, tread, twrite

include("flytoken.jl")
export DirectFly,

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
