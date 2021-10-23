## implementation of matchers using lexer methods, in particular RestrictedRegex

"lexer syntax definition for parsing RestrictedRegex definitions"
const RR_SYN = Vector{UInt32}(zero(UInt32),256)
{
    # build RestrictedLexer syntax
    l = UnsafeLexer(IOShared(),RR_SYN)
    addcategory(l,"\\",)
}

