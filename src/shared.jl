# this file implements a shared buffer structure.



"""
Mutable buffer, sharing content with token and ['SubString']@ref instances.

content is guaranteed to be immutable when returned in a SubString or Token reference.
implementation is currently NOT threadsafe.
"""
mutable struct SharedIO <: IO
    buffer :: String # PRIVATE!! memory with token/substring text data.
    first :: UInt32 # read position offset of first not consumed byte
    free :: UInt32 # write position offset: offset of first unused byte
    shared :: UInt32 # last index in buffer shared with other objects (0 is valid)
    flags :: UInt32 # bit flags which define behavior
end

"""
Mutable string implementation.


"""
struct MutableString <: AbstractString
    io:: SharedIO
end


# verhaltensvarianten: über spezielle functions, automatische Auswahl
# über smart variante die flags auswertet ==> overhead durch if...
# dann kann anwendung entscheiden ob sie explizit steuert oder nicht
#
#oder doch typvarianten?

# tokenvector: load store in sharedIO. wann shared hochsetzen?
# wenn vector getrennt, dann share beim speichern. bedeutet shared==free



# Base._string_n
# allocates and returns a String witn n code units

#= alloc und Schreiben in String

# allocate Vector{UInt8}s for IOBuffer storage that can efficiently become Strings
StringVector(n::Integer) = unsafe_wrap(Vector{UInt8}, _string_n(n))
# das hat dann aber

unsafe_convert(::Type{Ptr{UInt8}}, s::String) = convert(Ptr{UInt8}, pointer_from_objref(s)+sizeof(Int))


aus strings.jl 253ff
function getindex(s::String, r::UnitRange{Int})
    isempty(r) && return ""
    i, j = first(r), last(r)
    @boundscheck begin
        checkbounds(s, r)
        @inbounds isvalid(s, i) || string_index_err(s, i)
        @inbounds isvalid(s, j) || string_index_err(s, j)
    end
    j = nextind(s, j) - 1
    n = j - i + 1
    ss = _string_n(n) # allocate garbage collected string
    p = pointer(ss) # ist wohl nötig, aber kann ja variable sein!
    for k = 1:n
        unsafe_store!(p, codeunit(s, i + k - 1), k) # schreibt in ss[k]
        # unsafe_store!(p, someUInt8, k) #
    end
    return ss # und fertich!
end

# out == IObuffer transferiert String ptr
String(take!(out))



so liest julia selbst aus ...

@inline function codeunit(s::String, i::Integer)
    @boundscheck checkbounds(s, i)
    GC.@preserve s unsafe_load(pointer(s, i))
end

doc dazu
GC.@preserve x1 x2 ... xn expr
Temporarily protect the given objects from being garbage collected,
even if they would otherwise be unreferenced.

heißt für mich: da wir string im SharedIO gespeichert haben,
kann nix passieren. -> in Forum fragen!!

=#
