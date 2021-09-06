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
export T_END
export T_INT
export T_REAL
export T_CHAR
export T_TEXT
export T_IDENT
export T_SPECIAL
export T_QUOTE
export T_OP
export T_COMMENT
export T_EXT
export T_KEY
export T_SYM
export T_CMD
export T_REC
export T_LIST


# all TCategory values and its string macros are exported by abstracttoken.jl

include("substring.jl")
export substring, sswrite

include("packed31.jl")


include("flytoken.jl")
export FlyToken, DirectFly, BufferFly, HybridFly
export DToken, DIRECT_NOTHING, DIRECT_MISSING


include("token.jl")
export Token, BToken, HToken, Token
export @D1_str

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

#include("nibblevector.jl")

include("lexer.jl")
export AbstractLexer, ByteLexer, next, addcategory
#export

end # module
