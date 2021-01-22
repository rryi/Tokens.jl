
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
struct GenericTokenVector{T<:FlyToken} <: AbstractTokenVector{T}
    vec :: Vector{T}
    data :: IOShared
end

const TokenVector = GenericTokenVector{HybridToken}
const BTokenVector = GenericTokenVector{BufferToken}



