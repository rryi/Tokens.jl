"""
Memory-efficient vector of tokens.

Vector elements are tokens, and optionally Nothing and Missing. 
Some Missing/Nothing support is already built into all AbstractToken subtypes: 
they have an internal encoding for missing and nothing, and Functions ismissing and isnothing 
have methods for AbstractToken which test on that encoding. You will need 
Missing and/or Nothing in the element type only if some code does not use ismissing/isnothing
but explicitly tests instances on identity (or equality) to nothing and missing.

To have explicit Missing or Nothing support for vector elements, 
specify the appropriate Union for type parameter T.
This will *not* increase memory consumption per element, but cause some overhead on
array read accesses, to check for nothing or missing and replace a token instance by nothing or missing.
"""
struct TokenVector{T<:Union{Nothing,Missing,Token}}  <: AbstractVector{T} 
    vec :: Vector{HybridFly}
    heap :: IOShared # sharing and appending from outside is ok, other operations not.
    function TokenVector{T}(heap::IOShared) where T
        vec = Vector{HybridFly}()
        push!(heap.registered,vec)
        return new(vec,heap)
    end
end

const HTokenVector = TokenVector{Token{HybridFly}}
const BTokenVector = TokenVector{Token{BufferFly}}
const EXPECTED_TOKENSIZE = 10

TokenVector{T}(undef::UndefInitializer, n) where T = TokenVector{T}(undef,n,EXPECTED_TOKENSIZE)

function TokenVector{T}(undef::UndefInitializer, vectorsize, tokensize) where T
    ret = TokenVector{T}(IOShared(vectorsize*tokensize))
    resize!(ret.vec,vectorsize)
    fill!(ret.vec,T(nothing))
end


function TokenVector{T}(v::AbstractVector{S}, expectedTokensize=EXPECTED_TOKENSIZE) where {T,S}
    size = length(v)
    ret = TokenVector{T}(undef,size, expectedTokensize)
    for i in 1:size
        ret[i] = T(v[i])
    end
end

Base.IndexStyle(::Type{<:TokenVector}) = IndexLinear()

Base.eltype(::Type{TokenVector{T}}) where T = T

Base.sizehint!(tv:: TokenVector{T}, n) where T = sizehint!(tv.vec,n)


function Base.empty!(tv::TokenVector)
    empty!(tv.vec)
    # empty!(tv.heap) # not allowed - heap is designed to be nonexclusive.
    return tv
end

Base.size(tv::TokenVector) = size(tv.vec)
Base.length(tv) = length(tv.vec)
Base.firstindex(tv::TokenVector) = 1
Base.lastindex(tv::TokenVector) = length(tv)


function Base.getindex(tv::TokenVector{T}, i::Int) where T
    v = tv.vec[i]
    if Missing <:T && ismissing(v)
        return missing
    end
    if Nothing <:T && isnothing(v)
        return nothing
    end
    sharedbuf = tv.heap.buffer
    #= commented out because 1-2 checks on any read access
    # should not be necessary, but could prevend unnecessary references to heap.buffer
    sharedbuf = EMPTYSTRING
    if !isdirect(v) && usize(v)>0
        sharedbuf = tv.heap.buffer
    end
    =#
    return HToken <:T ? HToken(v,sharedbuf) : BToken(bf(v),sharedbuf) 
end

""" 

Element assignment with conversion.

t maybe one of the following:

 * AbstractString: stores a token with given content and category T_TEXT
 
 * AbstractToken: stores a token with given content and category

 * Integer: stores a token with decimal coded integer value and category T_INT

 * Real:  stores a token with decimal coded floating point number and ategory T_REAL

 * Missing: stores a token t with Category T_SYMBOL and empty string, giving ismissing(t) == true

 * Nothing: stores a token with Category T_SPECIAL and empty string, giving isnothing(t) == true


 """
function Base.setindex!(tv::TokenVector{T}, s, i::Int) where T 
    tv.vec[i] = hf(put(tv.heap, s, T).fly)
end



"""
    compress(tv::TokenVector, reserve::UInt32)

Build a reorganized copy of a TokenVector with (usually) reduced memory consumption.
Important use case: reorganization before serialization to external storage.

A new IOShared is allocated, a new TokenVector is built using it as buffer,
compression is turned on. reserve specifies the maximum of free space in
the new buffer - small values will probably force a reallocation.
    
Warning

    * worstcase runtime O((sum of token sizes)**2)
    * heap is set to search mode, subsequent assignments are O(heapsize)

New vector is returned. Old vector is cleared (all content is deleted)
"""
function compress(tv::TokenVector{T},reserve::UInt32) where T
    totalsize = 0
    for i in 1:length(tv.vec)
        f = tv.vec[i]
        if !isdirect(f) 
            s = usize(f)
            if (! HToken <: T) || s > MAX_DIRECT_SIZE
                totalsize += s
            end
        end
    end
    heap = IOShared(totalsize)
    ret = TokenVector{T}(heap)
    compressOnPut(heap,true)
    ii = sortperm(tv.vec,by = t -> -(usize(t)%Int) )
    for i in ii
        t = tv[i]
        if !isdirect(t) 
            s = usize(f)
            if HToken <: T && s <= MAX_DIRECT_SIZE
                t = T(DirectFly(t))
            else
                t = put(heap,t)
            end
        end
        ret.vec[i] = hf(t.fly)
    end
    shrink!(heap,reserve,false)
    empty!(tv)
    return ret
end


"Resize tv to contain n elements. If n is smaller than the current collection length, the first n elements will be retained. If n is larger, the new elements are set to nothing."
function Base.resize!(tv::TokenVector{F}, n::Integer) where {F<:FlyToken,T<:Union{Nothing,Missing,Token{F}}}
    oldsize = length(tv.vec)
    resize!(tv.vec,n)
    t = F(nothing) # initialization value
    while oldsize < n
        oldsize += 1
        tv.vec[oldsize] = t
    end
end


"inserts a copy of item with content in tv.heap (or direct)"
Base.insert!(tv::TokenVector{F}, index::Integer, item::Token{F}) where F  = insert!(tv.vek,index,put(tv.heap,item).fly)



function Base.append!(tv::TokenVector{T},items::AbstractVector{S}) where {T, S }
    n = length(items)
    last = lastindex(tv)
    resize!(tv.vec, last+n)
    for i in 1:n
        setindex!(tv,items[i],last+i)
    end
    return tv
end

# push should be defined for AbstractVector using append
#push!(collection, items...) -> collection
# iterate defined for AbstractVector

"copy shares content readonly, source keeps its write rights on its buffer"
Base.copy(tv::TokenVector{F}) where F <: FlyToken = TokenVector{F}(copy(tv.vec),copy(tv.heap))


"fill with anything that can be converted (via constructor) to a Token"
function Base.fill!(tv::TokenVector{F}, s) where F <: FlyToken
    t = Token{F}(s)
    t = put(tv.heap,t)
    fill!(tv.vec,t.fly)
    return tv
end

function Base.sizehint!(tv, n) 
    sizehint!(tv.vec,n)
    return tv
end


function Base.convert(::Type{TokenVector{F}}, arr::AbstractVector{S}) where {F<:FlyToken, S}
    tv = TokenVector{F}(undef, length(arr))
    @inbounds for i in eachindex(arr)
        tv[i] = Token{F}(arr[i])
    end
    return tv
end

#= analog implementieren??

Base.convert(::Type{StringArray}, arr::AbstractArray{T}) where {T<:STR} = StringArray{T}(arr)
Base.convert(::Type{StringArray{T, N} where T}, arr::AbstractArray{S}) where {S<:STR, N} = StringVector{S}(arr)
StringVector{T}() where {T} = StringVector{T}(UInt8[], UInt64[], UInt32[])
StringVector() = StringVector{String}()
StringVector{T}(::UndefInitializer, len::Int) where {T} = StringArray{T}(undef, len)
StringVector(::UndefInitializer, len::Int) = StringArray{String}(undef, len)
# special constructor where a full byte buffer is provided and offsets/lens will be filled in later
StringVector{T}(buffer::Vector{UInt8}, len::Int) where {T} = StringVector{T}(buffer, fill(UNDEF_OFFSET, len), fill(zero(UInt32), len))

(T::Type{<:StringArray})(arr::AbstractArray{<:STR}) = convert(T, arr)



function Base.similar(a::StringArray, T::Type{<:STR}, dims::Tuple{Vararg{Int64, N}}) where N
    StringArray{T, N}(undef, dims)
end


function Base.resize!(arr::StringVector, len)
    l = length(arr)
    resize!(arr.offsets, len)
    resize!(arr.lengths, len)
    if l < len
        arr.offsets[l+1:len] .= UNDEF_OFFSET # undef
        arr.lengths[l+1:len] .= 0
    end
    arr
end

function Base.push!(arr::StringVector, val::AbstractString)
    l = length(arr.buffer)
    resize!(arr.buffer, l + sizeof(val))
    unsafe_copyto!(pointer(arr.buffer, l + 1), pointer(val,1), sizeof(val))
    push!(arr.offsets, l)
    push!(arr.lengths, sizeof(val))
    arr
end

function Base.push!(arr::StringVector{Union{T, Missing}}, val::Missing) where {T}
    push!(arr.offsets, MISSING_OFFSET)
    push!(arr.lengths, 0)
    arr
end

function Base.deleteat!(arr::StringVector, idx)
    deleteat!(arr.lengths, idx)
    deleteat!(arr.offsets, idx)
    arr
end

function Base.insert!(arr::StringVector, idx::Integer, item::AbstractString)
    l = length(arr.buffer)
    resize!(arr.buffer, l + sizeof(item))
    unsafe_copyto!(pointer(arr.buffer, l + 1), pointer(item), sizeof(item))
    insert!(arr.offsets, idx, l)
    insert!(arr.lengths, idx, sizeof(item))
    arr
end

function Base.insert!(arr::StringVector{Union{T, Missing}}, idx::Integer, item::Missing) where {T}
    insert!(arr.offsets, idx, MISSING_OFFSET)
    insert!(arr.lengths, idx, 0)
    arr
end

function Base.permute!(arr::StringArray{String}, p::AbstractVector)
    permute!(convert(StringArray{WeakRefString{UInt8}}, arr), p)
    arr
end

function Base.sortperm(arr::StringArray{String})
    sortperm(convert(StringArray{WeakRefString{UInt8}}, arr))
end

function Base.sort!(arr::StringArray{String})
    permute!(arr, sortperm(arr))
    arr
end

function Base.vcat(a::StringVector{T}, b::StringVector{T}) where T
    StringVector{T}(vcat(a.buffer, b.buffer), vcat(a.offsets, b.offsets .+ length(a.buffer)), vcat(a.lengths, b.lengths))
end

function Base.append!(a::StringVector{T}, b::StringVector) where T
    append!(a.offsets, b.offsets .+ length(a.buffer))
    append!(a.buffer, b.buffer)
    append!(a.lengths, b.lengths)
    a
end

function Base.append!(a::StringVector{T}, b::AbstractVector) where T
    for x in b
        push!(a, x)
    end
    a
end

const _default_splice = []

function Base.splice!(a::StringVector, i::Integer, ins=_default_splice)
    v = a[i]
    m = length(ins)
    if m == 0
        deleteat!(a, i)
    elseif m == 1
        a[i] = ins[1]
    else
        _growat!(a, i, m-1)
        k = 1
        for x in ins
            a[i+k-1] = x
            k += 1
        end
    end
    return v
end

function Base.splice!(a::StringVector, r::UnitRange{<:Integer}, ins=_default_splice)
    v = a[r]
    m = length(ins)
    if m == 0
        deleteat!(a, r)
        return v
    end

    n = length(a)
    f = first(r)
    l = last(r)
    d = length(r)

    if m < d
        delta = d - m
        _deleteat!(a, (f - 1 < n - l) ? f : (l - delta + 1), delta)
    elseif m > d
        _growat!(a, (f - 1 < n - l) ? f : (l + 1), m - d)
    end

    k = 1
    for x in ins
        a[f+k-1] = x
        k += 1
    end
    return v
end

function _growbeg!(a::StringVector, n)
    Base._growbeg!(a.offsets, n)
    Base._growbeg!(a.lengths, n)
    return
end

function Base.prepend!(a::StringVector, items::AbstractVector)
    itemindices = eachindex(items)
    n = length(itemindices)
    _growbeg!(a, n)
    if a === items
        copyto!(a, 1, items, n+1, n)
    else
        copyto!(a, 1, items, first(itemindices), n)
    end
    return a
end

Base.prepend!(a::StringVector, iter) = _prepend!(a, Base.IteratorSize(iter), iter)
Base.pushfirst!(a::StringVector, iter...) = prepend!(a, iter)

function _prepend!(a, ::Union{Base.HasLength,Base.HasShape}, iter)
    n = length(iter)
    _growbeg!(a, n)
    i = 0
    for item in iter
        @inbounds a[i += 1] = item
    end
    a
end

function _prepend!(a, ::Base.IteratorSize, iter)
    n = 0
    for item in iter
        n += 1
        pushfirst!(a, item)
    end
    reverse!(a, 1, n)
    a
end

function Base.pop!(a::StringVector)
    if isempty(a)
        throw(ArgumentError("array must be non-empty"))
    end
    item = a[end]
    deleteat!(a, length(a))
    return item
end

function Base.pushfirst!(a::StringVector, item)
    _growbeg!(a, 1)
    a[1] = item
    return a
end

function Base.popfirst!(a::StringVector)
    if isempty(a)
        throw(ArgumentError("array must be non-empty"))
    end
    item = a[1]
    deleteat!(a, 1)
    return item
end

=#