## string search in the RegEx style

#=

API for pattern matching and replace operations redefining regular expression functions

The julia Regex type API consists of:

[1] *(r::Regex) in Base at regex.jl:677
[2] *(r1::Union{Regex, AbstractChar, AbstractString}, rs::Union{Regex, AbstractChar, AbstractString}...) in Base at regex.jl:656
[3] ==(a::Regex, b::Regex) in Base at regex.jl:614
[4] ^(r::Regex, i::Integer) in Base at regex.jl:729
[5] count(t::Union{Regex, AbstractString}, s::AbstractString; overlap) in Base at regex.jl:388
[6] eachmatch(re::Regex, str::AbstractString; overlap) in Base at regex.jl:609
[7] endswith(s::SubString, r::Regex) in Base at regex.jl:240
[8] endswith(s::AbstractString, r::Regex) in Base at regex.jl:235
[9] findall(t::Union{Regex, AbstractString}, s::AbstractString; overlap) in Base at regex.jl:361
[10] findfirst(r::Regex, s::AbstractString) in Base at regex.jl:327
[11] findnext(re::Regex, str::Union{String, SubString}, idx::Integer) in Base at regex.jl:299   
[12] findnext(r::Regex, s::AbstractString, idx::Integer) in Base at regex.jl:324
[13] hash(r::Regex, h::UInt64) in Base at regex.jl:620
[14] match(re::Regex, str::Union{SubString{String}, String}, idx::Integer) in Base at regex.jl:274
[15] match(re::Regex, str::Union{SubString{String}, String}, idx::Integer, add_opts::UInt32) in Base at regex.jl:274
[16] match(r::Regex, s::AbstractString) in Base at regex.jl:294
[17] match(r::Regex, s::AbstractString, i::Integer) in Base at regex.jl:295
[18] occursin(r::Regex, s::SubString; offset) in Base at regex.jl:176
[19] occursin(r::Regex, s::AbstractString; offset) in Base at regex.jl:171
[20] show(io::IO, re::Regex) in Base at regex.jl:111
[21] startswith(s::SubString, r::Regex) in Base at regex.jl:208
[22] startswith(s::AbstractString, r::Regex) in Base at regex.jl:203

match(...) returns a MatchResult object.

Package Tokens implements similar methods for the following pattern types:

 1) search using Bloom algorithm, simple search and also extended search with multiple accepted bytes per byte position
 2) simple search using Boyer-Moore algorithm 
 3) search for multiple patterns, each with syntax of 1)

Release 1.0: findnext, findfirst, startswith, endswith, occursin

replace:
 - simple search
 - bloom: jedes gefundene Zeichen kann für replace adressiert werden, Syntax [int:int] Zeichen von index (1 based) bis index. 
   Negative int sind zulässig, -1 ist letztes byte -2 vorletztes ... [1:-1] ist also der gesamte Suchstring wie gefunden.

 - search syntax: ähnlich regex, muss auch uppercase ja/nein includieren




"""

"""
function Base.match(r::Regex, s::AbstractString, idx::Integer)
end

=#


"""
A Matcher supports string matching with precompiled patterns.

It consists of 

 * a pattern definition in source code

 * a 'compiled' pattern structure (specific to an associated pattern matching algorithmn)
 
 * result of the last match attempt (match position, match szie, matched pattern parameters)
  
A Matcher is not thread-safe, it should be constructed and used in one thread only.

In the constructor, pattern definition is 'compiled' in a pattern structure 
best suited for the associated pattern matching algorithm. In contrast to ordinary 
string search, which does all search algorithm setup in every call, a Matcher
separates match initialization from the 'match kernel', which is some loop 
over the positions in the string to search in.

The matching algorithm is implemented in the [`Base.match`](@ref) function, which
returns a match result object, and has a matcher as its first parameter.
All Matchers implemented in package Tokens return itself.



All search variants like findfirst, findnext, occursin, startswith, endswith and match
variants with several type varations on where to search are all mapped on the central
method

    Base.match(m::Matcher,ofs::UInt32, size::UInt64, s::String)

This is the only function (besides constructors) all matchers must implement.
Several signature variants are mapped with generic methods onto that central method.
This holds for Base.AbstractString, and AbstractToken and IOShared of this module.


Every concrete Matcher type must have the following properties:

* a field (or property) *found*::Int which is set by a match call.
  found==0 indicates no match. found>0 gives the position of the match as an 1-based index.


* a field (or property) *length*::Int which is set if found>0. It gives the length
  of the match in bytes.


 * a field (or property) *variant*::Int which is set if found>0. It contains an ID 
   which identifies the pattern variant which matched. In the case of a Matcher which 
   has multiple patterns, it is an index into the pattern list. 


Every concrete Matcher type must have a property "found" which is set by a match call.
found==0 indicates no match. found>0 gives the position of the match as an 1-based index.



"""
abstract type Matcher 
end

# public Matcher API

# postponed [7] endswith(s::SubString, r::Matcher) 
# postponed [8] endswith(s::AbstractString, r::Matcher) 
# postponed [9] findall(t::Union{Matcher, AbstractString}, s::AbstractString; overlap) 
# postponed [13] hash(r::Matcher, h::UInt64) 
# postponed [14] match(re::Matcher, str::Union{SubString{String}, String}, idx::Integer)
# irrelevant[15] match(re::Matcher, str::Union{SubString{String}, String}, idx::Integer, add_opts::UInt32)
# postponed [21] startswith(s::SubString, r::Matcher)
# postponed [22] startswith(s::AbstractString, r::Matcher)

# setected API for release 1 of Tokens
#[10] findfirst(r::Matcher, s::AbstractString) 
#[11] findnext(re::Matcher, str::Union{String, SubString}, idx::Integer) 
#[12] findnext(r::Matcher, s::AbstractString, idx::Integer) 
#[16] match(r::Matcher, s::AbstractString)
#[17] match(r::Matcher, s::AbstractString, i::Integer)
#[18] occursin(r::Matcher, s::SubString; offset)
#[19] occursin(r::Matcher, s::AbstractString; offset)
#[20] show(io::IO, re::Matcher)


Base.match(r::Matcher, s) = match(r,s,firstindex(s))

# no. horrible because makes a copy ...
#Base.match(r::Matcher, s::AbstractString, start::Int) = match(r,string(s),start-firstindex(s)+1)


function Base.match(r::Matcher, s::Utf8String, start::Int) 
    ss = substring(s)
    @boundscheck checkbounds(ss,start)
    match(r,UInt32(start-1+ss.offset),usize(ss),ss.string)
    r.found > 0 && (r.found -= offset(ss))
    return r
end


function Base.match(r::Matcher, s::BToken, start::Int) 
    @boundscheck checkrange(s,start,last)
    match(r,start-1+offset(s),usize(s),s.buffer)
    r.found > 0 && (r.found -= offset(s))
    return r
end


Base.match(r::Matcher, s::AbstractToken, start::Int) = match(r,BToken(s),start)



Base.match(r::Matcher, io::IOShared) = match(r,io.readofs,usize(io),io.buffer)


"""
Simple string search structure using Bloom filters.
Algorithm is very similar to that used for string search in Julia Base.

Little initialization overhead, very fast, but no pattern flexibility.
"""
mutable struct ExactMatcher <: Matcher
  pattern :: String # pattern must match all bytes
  bloommask :: UInt64 # ORed byte hashes 
  lastskip :: Int # bytes to skip if last byte matches but another byte does not 
  bloomskip :: Int # bytes to skip if a byte-s hash is not in bloommask
  found :: UInt32 #  index (1-based) in string to search of last match or 0 (no match found)
  last :: UInt8 # last byte of pattern
  function ExactMatcher(pattern::AbstractString)
    matcher = new(string(pattern),0%UInt64,0,0,0,0x00)
    initialize!(matcher)
    return matcher
  end
end



"can be used to re-initialize after assigning a new pattern"
function initialize!(em::ExactMatcher)
    p = em.pattern
    n = ncodeunits(p)
    # n <= 1 && error("ExactMatcher requires a pattern size of at least 2")
    skip = n
    me.last = codeunit(p,n)
    bloom_mask = UInt64(_search_bloom_mask(tlast))
    bloom_bits = 1 # no. of bits set in bloom filter if <= 32
    bloom_skip = 1 # no. of bytes to skip if byte not in filter
    j = n
    while (j-=1)>=1  # j=n is already processed
        pj = codeunit(p,j)
        if pj == p.last && skip == n
            skip = n - j
        end
        if bloom_bits <= 32 # argument: is near max(p(bloom_skip)*bloom_skip)
            hash = bloomhash(pj)
            if hash&bloom_mask == 0
                if bloom_bits < 32
                    # put in bloom filter up to 32 bits
                    bloom_mask |= hash
                end
                bloom_bits += 1 # gets >32 to terminate bloom filter filling
            end
            if bloom_bits <= 32
                bloom_skip += 1
            end
        else
            if skip<n
                # we have finished adding hashes to bloom filter
                # and we have determined the skip distance if matching last byte
                # nothimg remains to be done in preprocessing - stop work.
                break
            end
        end
    end
    skip -= 1 # add 1 to skip in main loop
    em.bloommask = bloom_mask
    em.bloomskip = bloom_skip
    em.lastskip = skip
end


"""
    match(matcher::ExactMatcher, s::String, sfirst::Int, slast::Int)

    s[sfirst:slast] is the string to be examined for pattern matches 

    match returns matcher, all results are stored in matcher.

    boundscheck will ensure valid sfirst, slast are existing index values

"""
function match(matcher::ExactMatcher,ofs::UInt32,size::UInt64, s::String)
    n = ncodeunits(matcher.pattern)
    @boundscheck checklimit(ofs+size,s)
    m = size%Int
    matcher.found = 0 # assign default value "not found"
    if n<=1
        # special trivial case: switch to byte search or nothing to search
        if n==0
            m>0 && (matcher.found = ofs+1)
        else
            # Base works like this: something(findnext(isequal(codeunit(e.last,1)), s, sfirst), 0)
            # let-s compare performance with an ordinary loop
            while sfirst <= slast
                @inbounds if matcher.last == codeunit(s,sfirst)
                    matcher.found = sfirst
                end
                sfirst += 1
            end
            # result 0 is already assigned as 'default' - not necessary to assign 0
        end
    else
        w = m - n
        if w >= 0 
            # w<0 implies: not found 
            # ### START ### of pattern matching loop
            i = sfirst + n - 1 # position of last byte in matching candidate in search
            while i < m
                if codeunit(s,i) == matcher.last
                    # check candidate
                    j = 1
                    while j < n
                        if codeunit(s,i-n+j) != codeunit(matcher.pattern,j)
                            break
                        end
                        j += 1
                        # match found?
                        if j == n
                            matcher.found = i-n+1
                            return matcher
                        end
                    end
                    # no match: skip and test bloom
                    i += matcher.lastskip
                    if i>=m # main loop finished?
                        break
                    end
                end
                i += 1
                if bloom_mask & _search_bloom_mask(codeunit(s,i)) == 0
                    i += matcher.bloomskip
                end
            end
            if i==m
                # test end match
                j = 1
                while j <= n
                    if codeunit(s,i-n+j) != codeunit(t,j)
                        break # not found
                    end
                    j += 1
                end
                if j > n # means: loop without break , we have a match at the very end
                    matcher.found = i-n+1
                end 
           end
           # ###  END  ### of pattern matching loop
        end      
    end
    return matcher
end



"""
Search for any byte from a given list. 

"""
mutable struct AnyByteMatcher <: Matcher
    pattern :: Vector{UInt8} # pattern matches any of its bytes
    ismatch ::  Vector{UInt8} # 0 or index into pattern. index is 1+(byte value to test)
    found :: UInt32 #  index (1-based) in string to search of last match or 0 (no match found)
    variant :: UInt32 # index into pattern for match
    function AnyByteMatcher(pattern::Vector{UInt8})
        ismatch =  Vector{UInt8}(0%UInt8,256)
        for i in 1::length(pattern)
            ismatch[pattern[i]+1] = i
        end
        new(pattern,ismatch,0,0)
    end
end


function match(matcher::AnyByteMatcher,s::String, sfirst::Int, slast::Int)
    @boundscheck checkrange(s,sfirst,slast)
    @inbounds begin
        while sfirst <= slast
            test = ismatch[codeunit(s,sfirst)]
            if test> 0
                matcher.found = sfirst
                matcher.variant = test
                return matcher
            end
        end
        matcher.found = 0
        return matcher
    end
end




"""
Search for any string from a given list. 

Works best with long strings to match (shortest length determines performance).

Matcher uses a list of patterns, given as Token Vector. 
Pattern processing depends on token category:

 * T_TEXT: pattern is an exact string match

 * T_CHAR: pattern accepts lowercase and uppercase for every ASCII letter
   (does not apply to unicode letters beyond ASCII range)

 * T_STRUCT: for each byte position, a list of allowed byte values is specified. 
   
   All byte values in the pattern are treatet as exact match, except '^' which acts as
   escape for a nonexact match. UTF8 encoded non-ASCII characters are allowed and are treated as an exact match .
    
   The byte following a '^' specifies the type of match:
    
    * ^^ exact match for '^'

    * ^? any byte value

    * ^% any ASCII digit (0..9)

    * ^&  any ASCII letter
    
    * ^l any lower case ASCII letter
    
    * ^U any upper case ASCII letter
    
    * ^x?? a byte value specified in hexadecimal notation with two ASCII hex characters
    * ^b???????? a byte value specified in binary notation with eight ASCII digits

    * ^[list] list is an ASCII character list, non-ASCII bytes are specified by one of
      the preceding escape notations 




"""
mutable struct AnyStringMatcher <: Matcher
    pattern :: Vector{UInt8} # pattern matches any of its bytes
    ismatch ::  Vector{UInt8} # 0 or index into pattern. index is 1+(byte value to test)
    found :: UInt32 #  index (1-based) in string to search of last match or 0 (no match found)
    variant :: UInt32 # index into pattern for match
    function AnyStringMatcher(pattern::BTokenVector)
        error("not yet implemented")
    end
end

AnyStringMatcher(pattern::AbstractVector{T} ) where T <: AbstractString = AnyStringMatcher(BTokenVector(pattern))

#=
function match(matcher::AnyStringMatcher,s::String, sfirst::Int, slast::Int )
    @boundscheck checkrange(s,sfirst,slast)
    @inbounds begin
        while sfirst <= slast
            test = ismatch[codeunit(s,sfirst)]
            if test> 0
                matcher.found = sfirst
                matcher.variant = test
                return matcher
            end
        end
        matcher.found = 0
        return matcher
    end
end
=#

