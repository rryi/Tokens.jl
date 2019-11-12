# simple benchmark to decide on tinytoken length decoding
using Random, BenchmarkTools

struct TinyToken
    bits::UInt64
    function TinyToken(data::Integer)
        new(UInt64(data))
    end
end


@inline function withshift(t::TinyToken)
    # PROBLEM: native code calls reinterpret(::Type{T}, x) where {T} = bitcast(T, x)
    # which is a C function. withcmp27 and withcmp24 do not.
    #
    # tricky code without jumps:
    # 3 lowest bits of the size are always stored at bit position 56..58.
    # if NONASCII_BIT is set, we have additional 24 bits for size at bits 32..55
    # We build a mask for bits 32..55 all 1 (bit 63 set, data in buffer)
    # or 0 (bit 63 clear, all data in token, size is 0..7)
    # by arithmetic shift of bit 63. ANDing with t.bits and shift by 29 gives
    # the high bits 3..26 of the size.
    masksize = (reinterpret(Int64,t.bits)>>31) & (((1<<24)-1)<<32) ## length bitfield mask
    ((t.bits &masksize)>>>29) | ((t.bits >>>56) & 7)
end



@inline function withshift2(t::TinyToken)
    m1 ::Int64 = (reinterpret(Int64,t.bits)>>31)
    masksize = ((1<<24-1)<<32) & m1 ## length bitfield mask
    ((t.bits &masksize)>>>29) | ((t.bits >>>56) & 7)
end


@inline function withcmp27(t::TinyToken)
    reinterpret(Int64,t.bits) >=0 && return (t.bits >>>56) & 7
    return (t.bits&(1<<24-1)<<32)>>>29 | ((t.bits >>>56) & 7)
end

@inline function withcmp24(t::TinyToken)
    reinterpret(Int64,t.bits) >=0 && return (t.bits >>>56) & 7
    return (t.bits >> 32) & (1<<27-1)
end

function loopshift(cases, atok)
    result = UInt64(0)
    for i in 1:cases
        result += withshift(atok[i])
    end
    result
end

function loopshift2(cases, atok)
    result = UInt64(0)
    for i in 1:cases
        result += withshift2(atok[i])
    end
    result
end

function loopcmp24(cases, atok)
    result = UInt64(0)
    for i in 1:cases
        result += withcmp24(atok[i])
    end
    result
end
function loopcmp27(cases, atok)
    result = UInt64(0)
    for i in 1:cases
        result += withcmp27(atok[i])
    end
    result
end

function bench(cases::Int)
    Random.seed!(1234)
    abits = rand(UInt64,cases)
    atok = Vector{TinyToken}(undef, cases)
    percent = 0
    for i in 1:cases
        atok[i] = TinyToken(abits[i])
        if abits[i] > (UInt64(1)<<63)
            percent += 1
        end
    end
    for i in 1:cases
        atok[i] = TinyToken(abits[i])
    end
    #show(Float64(percent*100)/cases)
    #show(atok[1:10])



    @benchmark  withshift(v) setup=(v=$(atok[3]))
    @benchmark  withshift2(v) setup=(v=$(atok[3]))
    @benchmark  withcmp24(v) setup=(v=$(atok[3]))
    @benchmark  withcmp27(v) setup=(v=$(atok[3]))

    result = UInt64(0)
    @time @inbounds loopshift(cases,atok)

    result = UInt64(0)
    @time @inbounds loopshift2(cases,atok)

    result = UInt64(0)
    @time @inbounds loopcmp24(cases,atok)

    result = UInt64(0)
    @time @inbounds loopcmp27(cases,atok)

    atok
end

function intshift(v::Int64)
    v >> 17
end
#=
julia> @code_native intshift(v)
        .text
; Function intshift {
; Location: none:3
        pushq   %rbp
        movq    %rsp, %rbp
; Function >>; {
; Location: int.jl:448
; Function >>; {
; Location: int.jl:441
        sarq    $17, %rcx
;}}
        movq    %rcx, %rax
        popq    %rbp
        retq
        nopl    (%rax)
;}
=#


function uintshift(v::UInt64)
    v >> 17
end

function uintmulexp()
    v :: UInt64 = 2^24-1
end

function uintmulshift()
    v :: UInt64 = (1<<24)-1
end

#=
function uintmulshift()

           v :: UInt64 = (1<<24)-1

           end
uintmulshift (generic function with 1 method)

julia> @code_native uintmulshift()
        .text
; Function uintmulshift {
; Location: none:3
        pushq   %rbp
        movq    %rsp, %rbp
        movl    $16777215, %eax         # imm = 0xFFFFFF
        popq    %rbp
        retq
        nopl    (%rax,%rax)
;}

julia>

julia> function uintmulexp()

           v :: UInt64 = 2^24-1

           end
uintmulexp (generic function with 1 method)

julia> @code_native uintmulexp()
        .text
; Function uintmulexp {
; Location: none:3
        pushq   %rbp
        movq    %rsp, %rbp
; Function literal_pow; {
; Location: none
; Function macro expansion; {
; Location: none
; Function ^; {
; Location: intfuncs.jl:220
        subq    $32, %rsp
        movabsq $power_by_squaring, %rax
        movl    $2, %ecx
        movl    $24, %edx
        callq   *%rax
;}}}
; Function convert; {
; Location: number.jl:7
; Function Type; {
; Location: boot.jl:722
; Function toUInt64; {
; Location: boot.jl:692
; Function check_top_bit; {
; Location: boot.jl:581
; Function is_top_bit_set; {
; Location: boot.jl:571
        addq    $-1, %rax
;}
        js      L42
;}}}}
        addq    $32, %rsp
        popq    %rbp
        retq
; Function convert; {
; Location: number.jl:7
; Function Type; {
; Location: boot.jl:722
; Function toUInt64; {
; Location: boot.jl:692
; Function check_top_bit; {
; Location: boot.jl:581
L42:
        movabsq $throw_inexacterror, %r9
        movl    $227233368, %ecx        # imm = 0xD8B4E58
        movl    $234489840, %edx        # imm = 0xDFA07F0
        movq    %rax, %r8
        callq   *%r9
        ud2
        ud2
        nopl    (%rax,%rax)
;}}}}}

julia>

=#


#=
julia> @code_native uintshift(v)
        .text
; Function uintshift {
; Location: none:3
        pushq   %rbp
        movq    %rsp, %rbp
; Function >>; {
; Location: int.jl:448
; Function >>; {
; Location: int.jl:442
        shrq    $17, %rcx
;}}
        movq    %rcx, %rax
        popq    %rbp
        retq
        nopl    (%rax)
;}
=#




#atok = bench(10)

#atok = bench(1000000)


@inline f1(c::Int64) = c>=0 ? c&7 : c&255

@inline f2(c::Int64) = c & ((c>>63) & 255 | 7)

a = 0

@inline function loopf1(data, cases, ret,j)
    result ::Int64 = 0
    for i in 1:cases
        @inbounds result += f1(data[i])
    end
    ret[j] += result
    result
end

@inline function loopf2(data, cases, ret, j)
    result ::Int64 = 0
    for i in 1:cases
        @inbounds result += f2(data[i])
    end
    ret[j] += result
end

function simple_benchmark(cases)
    Random.seed!(1234)
    data = rand(Int64,cases)
    ret = [0,0]
    @time loopf1(data,cases,ret,1)
    @time loopf2(data,cases,ret,2)
    show(ret)
    println()
    println()
    nothing
end

simple_benchmark(9)
simple_benchmark(2000000)
