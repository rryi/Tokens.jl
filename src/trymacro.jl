@enum TCategory :: UInt8 begin
    T_WHITE = 0
    T_IDENT = 1
    T_SPECIAL = 2
    T_INT = 3
    T_QUOTED = 4
    T_CHAR = 5
    T_EOL = 6
    T_FLOAT = 7
    T_COMMENT = 8
    T_TEXT = 9
    T_END = 10
    T_SYMBOL = 11
    T_KEY = 12
    T_PI = 13
    T_STRUCT = 14
    T_SEQ = 15
end

# just to get it compiled in this demo
struct BufferToken
    cat::TCategory
    longtxt::String
end

# just to get it compiled in this demo
struct DirectToken
    cat::TCategory
    shorttxt::String
end

#= commented out is what should be generated

macro T_WHITE_str(txt)
    if ncodeunits(txt)>7
        :(BufferToken(T_WHITE,$txt))
    else
        :(DirectToken(T_WHITE,$txt))
    end
end

macro T_IDENT_str(txt)
    if ncodeunits(txt)>7
        :(BufferToken(T_IDENT,$txt))
    else
        :(DirectToken(T_IDENT,$txt))
    end
end
=#


#= this is from julia manual: basic idea on how to generate methods
for op = (:sin, :cos, :tan, :log, :exp)
    eval(quote
        Base.$op(a::MyNumber) = MyNumber($op(a.x))
    end)
end
=#


#= compiles, but error on use

for cat in instances(TCategory)
    eval(quote
        macro $(Symbol(Symbol(cat),"_str"))(txt)
            if ncodeunits(txt)>7
                :(BufferToken($cat,$txt))
            else
                :(DirectToken($cat,$txt))
            end
        end
    end)
end
=#

#=
for cat in instances(TCategory)
    eval(quote
        macro $(Symbol(Symbol(cat),"_str"))(txt)
            if ncodeunits(txt)>7
                :(BufferToken(TCategory(Int($cat)),$txt))
            else
                :(DirectToken(TCategory(Int($cat)),$txt))
            end
        end
    end)
end
=#

for cat in instances(TCategory)
    eval(quote
        macro $(Symbol(Symbol(cat),"_str"))(txt)
            if ncodeunits(txt)>7
                :(BufferToken($($(cat)),$txt))
            else
                :(DirectToken($($(cat)),$txt))
            end
        end
    end)
end

# test macro ...
T_INT"123"
