
"""
common supertype for all token vectors.



"""
abstract type AbstractTokenVector{T <: FlyToken} <: AbstractVector{AbstractToken}


"""
Memory-efficient vector of tokens.

On write access, all tokens with code unit count below 8 are stored as
[`DirectFly`](@ref).

Use this type if you expect a large percentage of tokens having less than
8 code units. If performance matters more than memory efficiency, benchmark
against [`BTokenVector`](@ref). Though TokenVector induces additional
conditional code to test if a token is of type DirectFly, it can be faster,
due to better memory locality and cache efficiency.
"""
struct ParameterizedTokenVector{F} <: AbstractTokenVector{ParameterizedToken{F}} where F <: FlyToken
    vec :: Vector{F}
    pool :: IOShared
end

const TokenVector = ParameterizedTokenVector{HybridFly}
const BTokenVector = ParameterizedTokenVector{BufferFly}

ParameterizedTokenVector{F}(undef, n) = ParameterizedTokenVector{F}(Vector{F}(undef,n),IOShared(max(10*n,DEFAULTLIMIT)))



Base.size(tv::ParameterizedTokenVector) = size(tv.vec)

Base.getindex(tv::ParameterizedTokenVector{F}, i::Int) where F <: FlyToken = ParameterizedToken{F}(vec[i],data.buffer)

function Base.setindex!(tv::ParameterizedTokenVector{F}, t::ParameterizedToken{F}, i::Int) where F <: FlyToken
    vec[i] = put(tv.pool,t)
end

"""
    Base.compact(tv:ParameterizedTokenVector)

    try to reduce pool size by looking for shared content
"""
function Base.compact(tv:ParameterizedTokenVector)
end


"Resize atv to contain n elements. If n is smaller than the current collection length, the first n elements will be retained. If n is larger, the new elements are set to empty tokens of category T_END."
function Base.resize!(tv::ParameterizedTokenVector{F}, n::Integer) 
    oldsize = length(tv.vec)
    Base.resize!(tv.vec,n)
    while oldsize <n
        oldsize += 1
        tv.vec[oldsize] = ParameterizedTokenVector{F}(T_END) # default token
    end
end


"inserts a copy of item with content in tv.pool (or direct)"
Base.insert!(tv::ParameterizedTokenVector{F}, index::Integer, item::ParameterizedToken{F}) where F <: FlyToken = insert!(tv.vek,index,put(tv.pool,item))



push!(collection, items...) -> collection


iterate(iter)		Returns either a tuple of the first item and initial state or nothing if empty
iterate(iter, state)		Returns either a tuple of the next item and next state or nothing if no items remain


firstindex(X)	The first index, used in X[begin]
lastindex(X)	The last index, used in X[end]




copy(A)	copy A
deepcopy(A)	copy A, recursively copying its elements



append!(collection, collection2) -> collection.



fill!(A, x)	fill the array A with the value x

sizehint!(s, n) # Suggest that collection s reserve capacity for at least n elements. This can improve performance.



function Base.resize!(tv::ParameterizedTokenVector{F}, n::Integer) 


Resize a to contain n elements. If n is smaller than the current collection length, the first n elements will be retained. If n is larger, the new elements are not guaranteed to be initialized.
