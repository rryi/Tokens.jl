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
    Base.@propagate_inbounds function TokenVector{T}(vec::Vector{HybridFly},heap::IOShared) where T
        # inform heap about usage
        register(heap,vec,true)
        heap.mark = 0
        @boundscheck begin
            for f in vec
                checkrange(offset(f),usize(f),heap)
            end
        end
        new(vec,heap)
    end
end

const HTokenVector = TokenVector{Token{HybridFly}}
const BTokenVector = TokenVector{Token{BufferFly}}

TokenVector{T}(undef::UndefInitializer, n) where T = 
  TokenVector{T}(Vector{HybridFly}(undef,n),IOShared(max(10*n,DEFAULTLIMIT)))

function TokenVector{T}(v::AbstractVector{S}) where {T,S}
    size = length(v)
    ret = TokenVector{T}(undef,size)
    for i in 1:size
        ret[i] = HToken <: T ? HToken(v[i]) : BToken(v[i])
    end
end

Base.IndexStyle(::Type{<:TokenVector}) = IndexLinear()

Base.eltype(::Type{TokenVector{T}}) where T = T


function Base.empty!(tv::TokenVector)
    empty!(tv.vec)
    empty!(tv.heap)
    return tv
end

Base.size(tv::TokenVector) = size(tv.vec)
Base.length(tv) = length(tv.vec)
Base.firstindex(tv::TokenVector) = 1
Base.lastindex(tv::TokenVector) = length(tv)

"""
    unsafe_get(tv::TokenVector{T}, i::Int)

Returns a Token which is valid as long as no reorganization is performed on the IOShared used by tv as heap.
Does always return a HToken/BToken, no nothing/missing, even if Nothing/Missing are subtypes of T.

Function is unsafe, because returned token is not guaranteed to be protected against buffer changes.
If tv.heap.shared is smaller than offset(t)+usize(t) for the returned Token, a reorganization of tv.heap
may (re-)move content referenced by t. This will result in a content change of t which violates 
immutability of t and may cause severe errors.
"""
unsafe_get(tv::TokenVector{T}, i::Int) where T = HToken <:T ? HToken(tv.vec[i],tv.heap.buffer) : BToken(bf(tv.vec[i]),tv.heap.buffer) 


function Base.getindex(tv::TokenVector{T}, i::Int) where T
    v = tv.vec[i]
    if Missing <:T && ismissing(v)
        return missing
    end
    if Nothing <:T && isnothing(v)
        return nothing
    end
    sharedbuf = EMPTYSTRING
    if !isdirect(v) && usize(v)>0
        sharedbuf = share(tv.heap.buffer,usize(v)+offset(v))
    end
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
    if  HToken <:T
        tv.vec[i] = put(tv.heap, HToken(s)).fly
    else
        tv.vec[i] = hf(put(tv.heap, HToken(s)).fly)
    end
end



"""
    Base.compact(tv::TokenVector)

    Build a reorganized copy of a TokenVector with (usually) reduced memory consumption.
    Important use case: reorganization before serialization to external storage.


    
    try to reduce heap size by looking for shared content.

    Warning
    
     * runtime O(length(tv)**2)
     * heap is set to search mode, subsequent assignments are O(length(tv))
"""
function Base.compact(tv::TokenVector{T}) where T
    newheap = IOShared(usize32(tv.heap))
    vec = copy(tv.vec)
    put(newheap,true)
    # sort by length, put largest tokens first - to increase chance of content reuse
    ii = sortperm(vec,id=(t->typemax(Int32)-usize(t)))
    for i in ii
        t = vec[i]
        newt = put(newheap,Token{F}(t,tv.heap))
        ret.vec[i] = newt.fly
    end
    return TokenVector{F,T}
    ret.heap = newheap

end


"Resize tv to contain n elements. If n is smaller than the current collection length, the first n elements will be retained. If n is larger, the new elements are set to empty tokens of category T_END."
function Base.resize!(tv::TokenVector{F}, n::Integer) where {F<:FlyToken,T<:Union{Nothing,Missing,Token{F}}}
    oldsize = length(tv.vec)
    resize!(tv.vec,n)
    t = F(T_END) # initialization value
    while oldsize < n
        oldsize += 1
        tv.vec[oldsize] = t
    end
end


"inserts a copy of item with content in tv.heap (or direct)"
Base.insert!(tv::TokenVector{F}, index::Integer, item::Token{F}) where F <: FlyToken = insert!(tv.vek,index,put(tv.heap,item).fly)



function Base.append!(tv::TokenVector{F,T},items::AbstractVector{S}) where {F <: FlyToken, T<:Union{Nothing,Missing,Token{F}}, S <: AbstractString}
    itemindices = eachindex(items)
    n = length(itemindices)
    last = lastindex(tv)
    Base._growend!(tv.vec, n)
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

Base.convert(::Type{StringArray}, arr::AbstractArray{T}) where {T<:STR} = StringArray{T}(arr)
Base.convert(::Type{StringArray{T, N} where T}, arr::AbstractArray{S}) where {S<:STR, N} = StringVector{S}(arr)
StringVector{T}() where {T} = StringVector{T}(UInt8[], UInt64[], UInt32[])
StringVector() = StringVector{String}()
StringVector{T}(::UndefInitializer, len::Int) where {T} = StringArray{T}(undef, len)
StringVector(::UndefInitializer, len::Int) = StringArray{String}(undef, len)
# special constructor where a full byte buffer is provided and offsets/lens will be filled in later
StringVector{T}(buffer::Vector{UInt8}, len::Int) where {T} = StringVector{T}(buffer, fill(UNDEF_OFFSET, len), fill(zero(UInt32), len))

(T::Type{<:StringArray})(arr::AbstractArray{<:STR}) = convert(T, arr)

_isassigned(arr, i...) = isassigned(arr, i...)
_isassigned(arr, i::CartesianIndex) = isassigned(arr, i.I...)
@inline Base.@propagate_inbounds function Base.getindex(a::StringArray{T}, i::Integer...) where T
    offset = a.offsets[i...]
    if offset == UNDEF_OFFSET
        throw(UndefRefError())
    end

    if Missing <: T && offset === MISSING_OFFSET
        return missing
    end

    convert(T, WeakRefString(pointer(a.buffer) + offset, a.lengths[i...]))
end

function Base.similar(a::StringArray, T::Type{<:STR}, dims::Tuple{Vararg{Int64, N}}) where N
    StringArray{T, N}(undef, dims)
end

function Base.empty!(a::StringVector)
    empty!(a.buffer)
    empty!(a.offsets)
    empty!(a.lengths)
    a
end

Base.copy(a::StringArray{T, N}) where {T,N} = StringArray{T, N}(copy(a.buffer), copy(a.offsets), copy(a.lengths))

@inline function Base.setindex!(arr::StringArray, val::WeakRefString, idx::Integer...)
    p = pointer(arr.buffer)
    if val.ptr <= p + sizeof(arr.buffer)-1 && val.ptr >= p
        # this WeakRefString points to data entirely within arr's buffer
        # don't add anything to the buffer in this case.
        # this optimization helps `permute!`
        arr.offsets[idx...] = val.ptr - p
        arr.lengths[idx...] = val.len
        val
    else
        _setindex!(arr, val, idx...)
    end
end

@inline function Base.setindex!(arr::StringArray, val::STR, idx::Integer...)
    _setindex!(arr, val, idx...)
end

@inline function Base.setindex!(arr::StringArray, val::STR, idx::Integer)
    _setindex!(arr, val, idx)
end

function _setindex!(arr::StringArray, val::AbstractString, idx...)
    buffer = arr.buffer
    l = length(arr.buffer)
    resize!(buffer, l + sizeof(val))
    unsafe_copyto!(pointer(buffer, l+1), pointer(val,1), sizeof(val))
    arr.lengths[idx...] = sizeof(val)
    arr.offsets[idx...] = l
    val
end

function _setindex!(arr::StringArray, val::AbstractString, idx)
    buffer = arr.buffer
    l = length(arr.buffer)
    resize!(buffer, l + sizeof(val))
    unsafe_copyto!(pointer(buffer, l+1), pointer(val,1), sizeof(val))
    arr.lengths[idx] = sizeof(val)
    arr.offsets[idx] = l
    val
end

function _setindex!(arr::StringArray{Union{T, Missing}, N}, val::Missing, idx) where {T, N}
    arr.lengths[idx] = 0
    arr.offsets[idx] = MISSING_OFFSET
    val
end

function _setindex!(arr::StringArray{Union{T, Missing}, N}, val::Missing, idx...) where {T, N}
    arr.lengths[idx...] = 0
    arr.offsets[idx...] = MISSING_OFFSET
    val
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

function _growat!(a::StringVector, i, len)
    Base._growat!(a.offsets, i, len)
    Base._growat!(a.lengths, i, len)
    return
end

function _deleteat!(a::StringVector, i, len)
    Base._deleteat!(a.offsets, i, len)
    Base._deleteat!(a.lengths, i, len)
    return
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