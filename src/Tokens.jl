module Tokens

using MurmurHash3
#using BitFlags

include("checks.jl")
export checkrange,checkulimit

include("nibble.jl")
export Nibble, nibble

include("abstracttoken.jl")
export AbstractToken, usize, category
export offset, isdirect, subtoken
#export T_END ... category constants are exported in tokenconstants.jl

include("substring.jl")
export substring, sswrite

include("packed31.jl")
export Packed31

include("flytoken.jl")
export FlyToken, DirectFly, BufferFly, HybridFly
export DToken, DIRECT_NOTHING, DIRECT_MISSING

include("tokenconstants.jl")
export CATEGORYNAMES


include("token.jl")
export Token, BToken, HToken, Token

include("search.jl")
export locate

include("ioshared.jl")
export IOShared,modify!,put, compressOnPut
export shrink!, ensure_writeable, ensure_readable
export fillup



#using TreeOnVectors
include("tokenvector.jl")
export TokenVector
include("match.jl")
export initialize!, Matcher, ExactMatcher, AnyByteMatcher, AnyStringMatcher
export @rs_str

#include("nibblevector.jl")

include("lexer.jl")
export AbstractLexer, ByteLexer, next, addcategory
#export

end # module
