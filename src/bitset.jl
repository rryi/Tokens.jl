CR aus  20 Nov 2018 von chetega
struct SBitSet{N}<:AbstractSet{Int64}
    chunks::SVector{N,UInt64}
end

@inline function Base.iterate(B::SBitSet{N}) where N
    N==0 && return nothing
    return iterate(B, (1, @inbounds B.chunks[1]))
end

@inline function Base.iterate(B::SBitSet{N}, s) where N
    N==0 && return nothing
    i1, c = s
    while c==0
        i1 % UInt >= N % UInt && return nothing
        i1 += 1
        @inbounds c = B.chunks[i1]
    end
    tz = trailing_zeros(c) + 1
    c = _blsr(c)
    return ((i1-1)<<6 + tz, (i1, c))
end

@inline Base.isempty(B::SBitSet) = iszero(B.chunks)
@inline Base.length(B::SBitSet) = count_ones(B.chunks)
@inline Base.union(B::SBitSet, C::SBitSet) = SBitSet(B.chunks | C.chunks)
@inline Base.intersect(B::SBitSet, C::SBitSet) = SBitSet(B.chunks & C.chunks)
@inline Base.symdiff(B::SBitSet, C::SBitSet) = SBitSet(xor(B.chunks, C.chunks))
@inline Base.setdiff(B::SBitSet, C::SBitSet) = SBitSet(B.chunks & ~C.chunks)
@inline Base.issubset(B::SBitSet, C::SBitSet) = iszero(B.chunks & ~C.chunks)

@inline function Base.in(B::SBitSet{N}, k::Integer) where N
    (0%UInt < k%UInt <= (N<<6)) || return false
    i1,i2 = Base.get_chunks_id(k)
    return !iszero(B.chunks[i1] & (1<<i2))
end

Base.show(io::IO, ::MIME{Symbol("text/plain")}, B::SBitSet) = show(io, B)
Base.show(io::IO, B::SBitSet{N}) where N = print(io, "SBitSet{$N}($(collect(B)))")





# alte Version von


if VERSION >= v"1.0.2"
    import Base._blsr
else
    @inline _blsr(x) = x & (x-1)
end

struct BitVec{N} <:AbstractVector{Bool}
    chunks::NTuple{N,UInt64}
end

@inline Base.length(B::BitVec{N}) where N = N<<6
@inline Base.size(B::BitVec{N}) where N = (N<<6,)
Base.IndexStyle(::BitVec) = IndexLinear()
Base.@propagate_inbounds function Base.getindex(B::BitVec, i)
    i1,i2 = Base.get_chunks_id(i)
    return !iszero(B.chunks[i1] & (1<<i2))
end

@inline function Base.iszero(B::BitVec{N}) where N
    @inbounds for i=1:N
        iszero(B.chunks[i]) || return false
    end
    true
end

@inline function Base.count(B::BitVec{N}) where N
    return sum(count_ones, B.chunks)
end

@inline Base.count_ones(B::BitVec) = count(B)

@inline function Base.:&(B::BitVec{N}, C::BitVec{N}) where N
    @inbounds BitVec(ntuple(i->(B.chunks[i] & C.chunks[i]),N))
end

@inline function Base.:|(B::BitVec{N}, C::BitVec{N}) where N
    @inbounds BitVec(ntuple(i->(B.chunks[i] | C.chunks[i]),N))
end

@inline function Base.:~(B::BitVec{N}) where N
    @inbounds BitVec(ntuple(i->~B.chunks[i],N))
end

@inline function Base.xor(B::BitVec{N}, C::BitVec{N}) where N
    @inbounds BitVec(ntuple(i->xor(B.chunks[i], C.chunks[i]),N))
end

@inline function BitVec{N}(k::Integer) where N
    i1,i2 = Base.get_chunks_id(k)
    u = UInt64(1)<<i2
    BitVec{N}(ntuple(i->ifelse(i==i1, u, UInt64(0)),N))
end

struct iterbits{N}
    mask::BitVec{N}
end

@inline function Base.iterate(L::iterbits{N}) where N
    N==0 && return nothing
    Bc = L.mask.chunks
    return iterate(L, (1, @inbounds Bc[1]))
end

@inline function Base.iterate(L::iterbits{N}, s) where N
    Bc = L.mask.chunks
    i1, c = s
    while c==0
        i1 % UInt >= N % UInt && return nothing
        i1 += 1
        @inbounds c = Bc[i1]
    end
    tz = trailing_zeros(c) + 1
    c = _blsr(c)
    return ((i1-1)<<6 + tz, (i1, c))
end
@inline Base.length(L::iterbits{N}) where N = count(L.mask)
@inline Base.eltype(::iterbits{N}) where N = Int

struct BitMat{N} <: AbstractMatrix{Bool}
    chunks::Matrix{UInt64}
end
function BitMat(nr, nc)
    nr_chunk = Base.num_bit_chunks(nr)
    chunks = zeros(UInt64, nr_chunk, nc)
    return BitMat{nr_chunk}(chunks)
end
function BitMat(::Val{N}, nc) where N
    chunks = zeros(UInt64, N, nc)
    return BitMat{N}(chunks)
end

@inline Base.length(B::BitMat) = length(B.chunks)<<6
@inline Base.size(B::BitMat{N}) where N = (N<<6, size(B.chunks,2))
Base.IndexStyle(::BitMat) = IndexLinear()
Base.@propagate_inbounds function Base.getindex(B::BitMat, i)
    i1,i2 = Base.get_chunks_id(i)
    return !iszero(B.chunks[i1] & (1<<i2))
end
Base.@propagate_inbounds function Base.setindex!(B::BitMat, b::Bool, i)
    i1, i2 = Base.get_chunks_id(i)
    u = UInt64(1) << i2
    c = B.chunks[i1]
    B.chunks[i1] = ifelse(b, c | u, c & ~u)
    b
end

Base.@propagate_inbounds function Base.setindex!(B::BitMat{N}, b::BitVec{N}, ::Colon ,j::Integer) where N
    for i=1:N
        B.chunks[i,j]=b.chunks[i]
    end
    b
end

Base.@propagate_inbounds function Base.getindex(B::BitMat{N}, ::Colon ,j::Integer) where N
    return BitVec{N}(ntuple(i->B.chunks[i,j], N))
end

Base.show(io::IO, ::MIME{Symbol("text/plain")}, bv::BitVec) = show(io, bv)
function Base.show(io::IO, bv::BitVec{N}) where N
    print(io, "BitVec{$N}(", collect(iterbits(bv)), ")")
end

Base.show(io::IO, ::MIME{Symbol("text/plain")}, bm::BitMat) = show(io, bm)
function Base.show(io::IO, bm::BitMat{N}) where N
    print(io, "BitMat{$N} (" )
    for i=1:size(bm,2)
        print(io, "\n$(i)\t")
        print(io, bm[:,i])
    end
    print(io, " )")
end

