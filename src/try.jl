mutable struct TA{T}
    buf::T
end
mutable struct TS
    buf::String
end


function read(t::TA{T}, pos::Int) where T
    @inbounds t.buf[pos]
end

function read(t::TS, pos::Int)
    @inbounds codeunit(t.buf,pos)
end

function tryy(size::Int, pos::Int)
    ta = TA(Vector{UInt8}(undef,size))
    ts = TS("0"^size)
    println( "array call ", read(ta,pos))
    #@code_llvm read(ta,pos)
    @code_native read(ta,pos)
    println( "string call ", read(ts,pos))
    #@code_llvm read(ts,pos)
    @code_native read(ts,pos)
end

tryy(100,7)
