## parameter tchecks, error structures


@noinline function checkrange(value::Int64, min::Int64, max::Int64)
    (min <= value <= max) || error("RangeError: required was $min <= $value <= $max")
    nothing
end

@noinline function checklimit(value::UInt64,max::UInt64) 
    value <= max || error("RangeError: required was $value <= $max")
end

checkrange(value::Integer,min::Integer,max::Integer) = checkrange(Int64(value),Int64(min),Int64(max))

checklimit(value::Unsigned,limit::Unsigned) = checklimit(value%UInt64,limit%UInt64) 

"verify value nonnegative and <= limit"
checklimit(value::Integer,limit::Integer) = checkrange(value,0,limit) 

"deprecated - replace by checklimit"
ckecksize(size::Integer, limit::Integer) = checklimit(size,limit)


ckeckbyteofs(ofs::UInt32, size::Unsigned) = ofs<size || error("OffsetError: required was $ofs < $size")


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