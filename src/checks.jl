## parameter tchecks, error structures


function error(msg::String){
    throw(ErrorException(msg))
}


@noinline function checkrange(val::Int64, min::Int64, max::Int64)
    (min <= value <= max) || error("RangeError: required was $min <= $val <= $max")
end


checkrange(val::Integer,min::Integer,max::Integer) = checkrange(Int64(val),Int64(min),Int64(max))


ckecksize(size::Unsigned, limit::Integer) = checkrange(Int64(size),0,Int64(limit))

