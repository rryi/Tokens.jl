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
 
 * result of the last match attempt (match position, matched pattern parameters)
  
A Matcher is not thread-safe, it should be constructed and used in one thread only.

In the constructor, pattern definition is 'compiled' in a pattern structure 
best suited for the associated pattern matching algorithm. In contrast to ordinary 
string search, which does all search algorithm setup in every call, a Matcher
separates match initialization from the 'match kernel', which is some loop 
over the positions in the string to search in.

The matching algorithm is implemented in the [`Base.match`](@ref) function, 
with the 1st parameter typed Matcher. All search variants like findfirst, findnext,
occursin, startswith, endswith are specialized match calls.

Every concrete Matcher type must have a field named found of type Int. On return from a
call like match(needle,haystack, ...), needle.found is 0 (not matched)
or the 1-based index into haystack where the first match begins. 

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

Base.match(r::Matcher, s::AbstractString, start::Int) = match(r,s,start,ncodeunits(s))

Base.match(r::Matcher, s::AbstractString) = match(r,s,1)

Base.match(r::Matcher, s::AbstractString, start::Int,last::Int) = match(r,string(s),start,last)


Base.match(r::Matcher, s::Utf8String, start::Int) = match(r,s,start,ncodeunits(s))

Base.match(r::Matcher, s::Utf8String) = match(r,s,1)

Base.match(r::Matcher, io::IOShared, start::Int) = match(r,io,start,usize(io)%Int)

Base.match(r::Matcher, io::IOShared) = match(r,io,1)





function Base.match(r::Matcher, s::SubString{String}, start::Int, last::Int) 
    @boundscheck checkrange(last,1,ncodeunits(s))
    match(r,s.string,start+s.offset,last+s.offset)
    r.found > 0 && r.found -= s.offset
    return r
end


function Base.match(r::Matcher, s::ParameterizedToken, start::Int, last::Int) 
    @boundscheck checkrange(last,1,ncodeunits(s))
    match(r,s.buffer,start+offset(s),last+offset(s))
    r.found > 0 && r.found -= offset(s)
    return r
end




"Match in current buffer content, no fillup! "
function Base.match(r::Matcher, s::IOShared, start::Int, last::Int) 
    match(r,s.buffer,start+s.readofs,last+s.readofs)
    r.found > 0 && r.found -= s.readofs
    return r
end



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
  last :: Uint8 # last byte of pattern
  function ExactMatcher(pattern::String)
    matcher = new(pattern,0%UInt64,0,0,0,0x00)
    initialize!(matcher)
  end
end

"very simple hash function used for ExactMatcher search"
bloomhash(b::UInt8) = UInt64(1) << (b & 0x3f)


function initialize(em::ExactMatcher)
    p = e.pattern
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
    e.bloommask = bloom_mask
    e.bloomskip = bloom_skip
    e.lastskip = skip
end


"""
    match(matcher::ExactMatcher, s::String, sfirst::Int, slast::Int)

    s[sfirst:slast] is the string to be examined for pattern matches 

    match returns matcher, all results are stored in matcher.

    boundscheck will ensure valid sfirst, slast are existing index values

"""
function match(matcher::ExactMatcher,s::String, sfirst::Int, slast::Int )
    n = ncodeunits(matcher.pattern)
    @boundscheck sfirst <1 && sfirst = 1
    @boundscheck slast > ncodeunits(s) && slast = ncodeunits(s)
    m = slast-sfirst+1
    matcher.found = 0 # assign default value "not found"
    if n<=1
        # special trivial case: switch to byte search or nothing to search
        if n==0
            m>0 && matcher.found = sfirst 
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


