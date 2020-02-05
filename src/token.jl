## Token, MutableToken implementations


"""
private auxiliary type for the AbstractToken implementations here,
to reduce copy/pase for methods of different token types.

Requirement:

Any subtype must have a *tiny::TinyToken* and a *buffer::String*
field.
"""
abstract type TinyBufferToken <: AbstractToken
end



offset(t::TinyBufferToken) = offset(t.tiny)

Base.ncodeunits(t::TinyBufferToken) = ncodeunits(t.tiny)

TCategory(t::TinyBufferToken) = TCategory(t.tiny)





"""
Immutable token, either direct enoded or buffer based
"""
struct Token <: TinyBufferToken
    tiny :: HybridToken # category, size, offset
    buffer :: String # memory with token text data.
end



"""
Immutable token, always buffer based
"""
struct BufferToken <: TinyBufferToken
    tiny :: FlyToken # category, size, offset
    buffer :: String # memory with token text data.
end


"""
Mutable token, able to share its buffer with other tokens
and ['SubString']@ref

DANGER: any function reusing buffer of a TinyBufferToken MUST be overloaded
"""
mutable struct MutableToken <: TinyBufferToken
    tiny :: FlyToken # current value, offset referencing buffer
    buffer :: String # PRIVATE!! memory with token text data.
    shared :: UInt32 # last index in buffer shared with other tokens (0 is valid)
end



# Base._string_n
# allocates and returns a String witn n code units

#= alloc und Schreiben in String
ss = _string_n(n)
p = pointer(ss)
for k = 1:n
    unsafe_store!(p, codeunit(s, i + k - 1), k)
endmethods(length)

# out == IObuffer transferiert String ptr
String(take!(out))

=#



#########################################################
############## Base methods for tokens ##################
#########################################################

function Base.SubString(t::TinyBufferToken,i::Int, j::Int)
    if
    t.tiny<0 ? SubString(t.)
     = SubString()
end


#=
function TinyToken(s::String, category::UInt8=0)
    sz = sizeof(s)
    # try to encode in 7 bytes. tinytoken
    if sz < sizeof(::TinyToken)
        str :: Int64 = 0
        utf8Flag :: UInt8 = 0
        i = 0
        while ++i <= sz
            c = codeunit(s,i)
            (str *= 8) += c
            if c>127
            end

        end
        p :: Ptr{UInt8} = pointer(s)
        (str *= 8) += codeunit(s,i)

        str = (T(s |> pointer |> Ptr{TinyToken} |> Base.unsafe_load |> ntoh)
    end
    # check for UTF8 encoded ISO-8859-1 string: sizeof(s)>=8 could be valid in this case
    throw(ErrorException("supplied string size $sz exceeds size limit for TinyToken - use Token, instead"))

    bits_to_wipe = 8(sizeof(T) - sz)
    content = (T(s |> pointer |> Ptr{TinyToken} |> Base.unsafe_load |> ntoh) >> bits_to_wipe) << bits_to_wipe
    TinyToken{T}(content | T(sz))
end

String(s::TinyToken) = String(reinterpret(UInt8, [s.size_content|>ntoh])[1:sizeof(s)])

Base.lastindex(s::TinyToken) = Int(s.size_content & 0xf)
Base.iterate(s::TinyToken, i::Integer) = iterate(String(s), i)
Base.iterate(s::TinyToken) = iterate(String(s))
Base.sizeof(s::TinyToken) = Int(s.size_content & 0xf)
Base.print(s::TinyToken) = print(String(s))
Base.display(s::TinyToken) = display(String(s))
Base.convert(::TinyToken{T}, s::String) where T = TinyToken{T}(s)
Base.convert(::String, ss::TinyToken) = String(a) #reduce(*, ss)
Base.firstindex(::TinyToken) = 1
Base.ncodeunits(s::TinyToken) = ncodeunits(String(s))
Base.codeunit(s::TinyToken, i) = codeunits(String(s), i)
Base.isvalid(s::TinyToken, i::Integer) = isvalid(String(s), i)

Base.getindex(s::TinyToken{T}, i::Integer) where T = begin
    print(i)
    Char((s.size_content << 8(i-1)) >> 8(sizeof(T)-1))
end
Base.collect(s::TinyToken) = getindex.(s, 1:lastindex(s))

==(s::TinyToken, b::String) = begin
    String(s)  == b
end
=#

end # module
