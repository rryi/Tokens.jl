

"""
    *(t::TinyToken, s::Union{UInt8,Char,AbstractString)

concatenation has to be supported. category is always copied from base
token (first argument). code units are allowed to be concatenated, this
can result in tokens representing an invalid Utf8 code unit sequence.

Every token implementation must supply UInt8 (code unit) concatenation,
other concatenation arguments have a default implementation using
code unit concatenation
"""
function (*)(t::TinyToken, s::Utf8String)


end

function (*)(t::TinyToken, c::Char)


end


function (*)(t::TinyToken, cu::UInt8)
    @boundscheck checkappend(t,1)
    unsafe_set (append(t,1))

end
