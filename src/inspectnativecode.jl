abstract type WOP end

mutable struct WP <: WOP
    s::String
    p:: Ptr{UInt8}
    WP(s::String) = new(s,pointer(s))
end



mutable struct OP <:WOP
    s::String
end

@inline function byte(s::String, ofs::UInt32)
    @boundscheck checkbyteofs(ofs,ncodeunits(s))
    b = GC.@preserve s unsafe_load(pointer(s)+ofs)
    return b
end

@inline function byte1o(w::OP, ofs::UInt32) 
    @inbounds begin
        s = w.s
        return GC.@preserve s unsafe_load(pointer(s)+ofs)
    end
end


@inline function byte1w(w::WP, ofs::UInt32) 
    @inbounds begin
        s = w.s
        return GC.@preserve s unsafe_load(w.p+ofs)
    end
end

@inline function byte2o(w::OP, ofs::UInt32) 
    @inbounds begin
        s = w.s
        b = GC.@preserve s unsafe_load(pointer(s)+ofs)
    end
    return b
end


@inline function byte2w(w::WP, ofs::UInt32) 
    @inbounds begin
        s = w.s
        b = GC.@preserve s unsafe_load(w.p+ofs)
    end
    return b
end


@inline function byte0(w::WOP, ofs::UInt32) 
    @inbounds return codeunit(w.s,ofs+1)
end


function codetest(i::Int,j::Int)
    s = "0123456789"
    op = OP(s)
    wp = WP(s)
    ui = i%UInt32
    uj = jUInt32
    sum0 = byte0(op,ui)+byte0(wp,uj)
    sum1 = byte1o(op,ui)+byte1w(wp,uj)
    sum2 = byte2o(op,ui)+byte2w(wp,uj)
    return sum1+sum2+sum3
end


@code_native codetest(s)

# Ergebnis: pointervariable bringt keinen Vorteil.

#=
julia> @code_native byte1o(op,ui)
        .text
; ┌ @ REPL[45]:1 within `byte1o'
        pushq   %rbp
        movq    %rsp, %rbp
; │ @ REPL[45]:3 within `byte1o'
; │┌ @ Base.jl:33 within `getproperty'
        movq    (%rcx), %rax
; │└
; │ @ REPL[45]:4 within `byte1o'
; │┌ @ pointer.jl:159 within `+'
; ││┌ @ int.jl:478 within `rem'
; │││┌ @ number.jl:7 within `convert'
; ││││┌ @ boot.jl:713 within `UInt64'
; │││││┌ @ boot.jl:687 within `toUInt64'
        movl    %edx, %ecx
; │└└└└└
; │┌ @ pointer.jl:105 within `unsafe_load' @ pointer.jl:105
        movb    8(%rax,%rcx), %al
; │└
        popq    %rbp
        retq
        nop
; └

julia> 

julia> @code_native byte1w(wp,ui)
        .text
; ┌ @ REPL[46]:1 within `byte1w'
        pushq   %rbp
        movq    %rsp, %rbp
; │ @ REPL[46]:4 within `byte1w'
; │┌ @ Base.jl:33 within `getproperty'
        movq    8(%rcx), %rax
; │└
; │┌ @ pointer.jl:159 within `+'
; ││┌ @ int.jl:478 within `rem'
; │││┌ @ number.jl:7 within `convert'
; ││││┌ @ boot.jl:713 within `UInt64'
; │││││┌ @ boot.jl:687 within `toUInt64'
        movl    %edx, %ecx
; │└└└└└
; │┌ @ pointer.jl:105 within `unsafe_load' @ pointer.jl:105
        movb    (%rax,%rcx), %al
; │└
        popq    %rbp
        retq
        nop
; └


=#
