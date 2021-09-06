struct Packed{N} 
    val::UInt64
end


function bitsize end

bitsize(::Type{Packed{N}}) where N = N

bitsize(::Type{T}) where T = sizeof(T)*8

S1 = @NamedTuple{ f1::Packed{1}, i1::Packed{7}, i2::Packed{4}, i3::Packed{12}}
S2 = @NamedTuple{ v1::UInt8, v2::UInt8, v3::UInt16, v4::UInt32}

primitive type PackedStruct{T<:NamedTuple} 64 end

PackedStruct{T}(v::UInt64) where T = reinterpret(PackedStruct{T},v)


s1 = PackedStruct{S1}(0x0102030405060708)


Base.@pure function Base.getproperty(x::PackedStruct{T},s::Symbol) where T
    shift = 0
    syms = T.parameters[1]
    types = Tuple(T.parameters[2].parameters)
    bits = bitsize(types[1])
    if syms[1]===s
        mask = (one(UInt64)<<bits) -one(UInt64)
        return types[1]((reinterpret(UInt64,x) >>> shift ) & mask)
    end

    shift = shift + bits
    bits = bitsize(types[2])
    if syms[2]===s
        mask = (one(UInt64)<<bits) -one(UInt64)
        return types[2]((reinterpret(UInt64,x) >>> shift ) & mask)
    end
    shift = shift + bits
    bits = bitsize(types[3])
    if syms[3]===s
        mask = (one(UInt64)<<bits) -one(UInt64)
        return types[3]((reinterpret(UInt64,x) >>> shift ) & mask)
    end
    shift = shift + bits
    bits = bitsize(types[4])
    if syms[4]===s
        mask = (one(UInt64)<<bits) -one(UInt64)
        return (reinterpret(UInt64,x) >>> shift ) & mask
    end
    shift = shift + bits
    bits = bitsize(types[5])
    if syms[5]===s
        mask = (one(UInt64)<<bits) -one(UInt64)
        return (reinterpret(UInt64,x) >>> shift ) & mask
    end
    shift = shift + bits
    bits = bitsize(types[6])
    if syms[6]===s
        mask = (one(UInt64)<<bits) -one(UInt64)
        return (reinterpret(UInt64,x) >>> shift ) & mask
    end
    shift = shift + bits
    bits = bitsize(types[7])
    if syms[7]===s
        mask = (one(UInt64)<<bits) -one(UInt64)
        return (reinterpret(UInt64,x) >>> shift ) & mask
    end
    shift = shift + bits
    bits = bitsize(types[8])
    if syms[8]===s
        mask = (one(UInt64)<<bits) -one(UInt64)
        return (reinterpret(UInt64,x) >>> shift ) & mask
    end
end
#=
show(s1.f1)
println()
show(s1.i1)
println()
show(s1.i2)
println()
show(s1.i3)
println()
=#

s2 = PackedStruct{S2}(0x0102030405060708)
show(s2.v1)
println()
show(s2.v2)
println()
show(s2.v3)
println()
show(s2.v4)
println()

