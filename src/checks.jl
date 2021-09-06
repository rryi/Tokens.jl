## parameter tchecks, error structures


#@noinline 
function checkrange(value::Int64, min::Int64, max::Int64)
    (min <= value <= max) || throw(BoundsError(min:max),value)
    nothing
end

#@noinline 
function checkulimit(value::UInt64,max::UInt64) 
    value <= max || throw(BoundsError(0:max,value))
end

checkrange(value::Integer,min::Integer,max::Integer) = checkrange(Int64(value),Int64(min),Int64(max))

checkulimit(value::Unsigned,limit::Unsigned) = checkulimit(value%UInt64,limit%UInt64) 

"verify value nonnegative and <= limit"
checkulimit(value::Integer,limit::Integer) = checkrange(value,0,limit) 



ckeckbyteofs(ofs::UInt32, t) = ofs<usize(t) || throw(BoundsError(t,ofs))


#Base.@propagate_inbounds function checksize(size::Unsigned, maxsize)


"test if index range is valid for given string "
checkrange(s::AbstractString,first::Integer, last::Integer) = checkbounds(s,first:last)



"test if offset/size range is valid for given s, defaults to offset+size<=usize(d)"
function checkrange(offset::UInt32, size::UInt64, s)
    checkbounds(s,offset+size)
end
