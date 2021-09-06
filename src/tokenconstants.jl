# Definition of (default) token category names,
# special string constants to construct tokens
# 
# ATTENTION: keep in sync with abstracttoken Nibble definitions!!

# Idee: lies T_* per reflection aus.

# array of category names without "T_"
const CATEGORYNAMES = fill(DToken(T_IDENT),16)
# do reflection to find "T_" constants
for sym in names(Tokens;all=true) # all names because it is called during compilation
    s = string(sym)
    if ncodeunits(s)>3 && s[1]=='T' && s[2] == '_'
        @eval export $sym
        n = UInt8(eval(sym))
        CATEGORYNAMES[n+1] = DToken(T_IDENT,SubString(s,3))
    end
end


"""
    categoryName(c::Integer) -> DToken

Category name of a category Nibble (without the "T_" prefix in the category constants)
"""
categoryName(c::Integer) = CATEGORYNAMES[UInt8(c)+1]





# Generation of the string literal macros for tokens

# try export in eval             #:(:export, Symbol('@',$($(ttype)),$($(ccat)),"_str"))

# generic string constant types
for cat in 0:15
    for ttype in ['D','H','B']
        ccat = Char(cat<=9 ? '0'+cat : 'A'+cat-10)
        eval(quote
            macro $(Symbol(ttype,ccat,"_str"))(txt)
                :($(Symbol($(ttype),"Token"))(Nibble($($cat)),$txt))
            end
        end)
        s = Symbol('@',ttype,ccat,"_str")
        @eval export $s
    end
    eval(quote
        macro $(Symbol("T_",categoryName(cat),"_str"))(txt)
            :(BToken(Nibble($($cat)),$txt))
        end
    end)
    s = Symbol('@',"T_",categoryName(cat),"_str")
    @eval export $s
end
