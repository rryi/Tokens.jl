module Tokens

using MurmurHash3
#using BitFlags

include("checks.jl")
export checkrange,checklimit

include("nibble.jl")
export Nibble, nibble

include("abstracttoken.jl")
export AbstractToken, usize, category
export offset, isdirect, subtoken

# all TCategory values and its string macros are exported by abstracttoken.jl

include("substring.jl")
export substring, sswrite

include("packed31.jl")


include("flytoken.jl")
export FlyToken, DirectFly, BufferFly, HybridFly
export DToken, DIRECT_NOTHING, DIRECT_MISSING


include("token.jl")
export Token, BToken, HToken

include("search.jl")


include("ioshared.jl")
export IOShared,modify!
#using TreeOnVectors
include("tokenvector.jl")
export TokenVector, TokenTree
include("match.jl")
export match, Matcher, ExactMatcher, AnyByteMatcher, AnyStringMatcher
#include("nibblevector.jl")

include("lexer.jl")
export AbstractLexer, ByteLexer, next, addcategory
#export

end # module
