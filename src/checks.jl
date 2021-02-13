## parameter tchecks, error structures


@noinline function checkrange(val::Int64, min::Int64, max::Int64)
    (min <= value <= max) || error("RangeError: required was $min <= $val <= $max")
    nothing
end


checkrange(val::Integer,min::Integer,max::Integer) = checkrange(Int64(val),Int64(min),Int64(max))


ckecksize(size::Unsigned, limit::Integer) = checkrange(Int64(size),0,Int64(limit))


ckeckbyteofs(ofs:UInt32, size::Unsigned) = ofs<size || error("OffsetError: required was $ofs < $size")


#Base.@propagate_inbounds function checksize(size::Unsigned, maxsize)


"test if index range is valid for given string "
function checkrange(s::AbstractString,first::Integer, last::Integer)
    checkrange(first,1,last)
    checkbounds(s,last)
end 


"test if offset+size range is valid for given string "
function checkrange(offset::UInt32, size::UInt64, s::AbstractString)
    checksize(offset+size,usize(s))
end