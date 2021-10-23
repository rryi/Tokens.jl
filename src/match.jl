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
 
 * result of the last match attempt
  
A Matcher is not thread-safe and not reentrant.
It should be constructed and used in one thread only.
If matcher construction is expensive, it should support
the Base.copy function to create a copy with deep copies
of all fields which are mutable.

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


* a field (or property) *base*::BToken which gives the byte sequence 
  in which the search took place. Its token category is irrelevant 
  (not used in matcher methods). 

  * a field (or property) *matchsize*::UInt64 which is set to the number of
  bytes matched.


* a field (or property) *matchidx*::UInt32 which is set by a match call.
  If matching fails, it is set to 0. On match success,
  it is the index of the match within the buffer of *base*, and must fulfil 
  offset(*base*) < *matchidx* <= offset(*base*)+usize32(*base*). 
  If no match was found, *matchidx* must be equal to offset(*base*)
  its value is undefined.

Every concrete Matcher type which uses the generic search methods implemented 
in package Tokens require additionally the following properties:

* a field (or property) *base*::BToken which gives the byte sequence 
  in which the search took place. Its token category is irrelevant 
  (not used in matcher methods)

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


Base.match(r::Matcher, s) = match(r,BToken(s))

Base.match(r::Matcher, s::BToken) = match(r,BToken(s))



# no. horrible because makes a copy ...
#Base.match(r::Matcher, s::AbstractString, start::Int) = match(r,string(s),start-firstindex(s)+1)


function Base.match(r::Matcher, s::Utf8String, start::Int) 
    ss = substring(s)
    @boundscheck checkbounds(s,start)
    match(r,UInt32(start-1+offset(ss),usize(ss),ss.string))
    r.matchofs > 0 && (r.matchofs -= offset(ss))
    return r
end


function Base.match(r::Matcher, s::BToken, start::Int) 
    @boundscheck checkbounds(s,start:last)
    match(r,start-1+offset(s),usize(s),s.buffer)
    r.matchofs > 0 && (r.matchofs -= offset(s))
    return r
end


Base.match(r::Matcher, s::AbstractToken, start::Int) = match(r,BToken(s),start)



Base.match(r::Matcher, io::IOShared) = match(r,io.readofs,usize(io),io.buffer)


"""
Simple string search structure using Bloom filters.
Algorithm is very similar to that used for string search in Julia Base.

Little initialization overhead, very fast, but no pattern flexibility.
"""
mutable struct BloomMatcher <: Matcher
  pattern :: String # pattern must match all bytes
  bloommask :: UInt64 # ORed byte hashes 
  lastskip :: Int # bytes to skip if last byte matches but another byte does not 
  bloomskip :: Int # bytes to skip if a byte-s hash is not in bloommask
  matchofs :: UInt32 #  offset (0-based) in string buffer to search of last match
  matchsize::UInt64 # 0 (not found) or number of bytes in match
  last :: UInt8 # last byte of pattern
  function BloomMatcher(pattern::AbstractString)
    matcher = new(string(pattern),0%UInt64,0,0,0,0x00)
    initialize!(matcher)
    return matcher
  end
end



"can be used to re-initialize after assigning a new pattern"
function initialize!(bm::BloomMatcher)
    p = bm.pattern
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
    bm.bloommask = bloom_mask
    bm.bloomskip = bloom_skip
    bm.lastskip = skip
end


"""
    match(matcher::ExactMatcher, s::String, sfirst::Int, slast::Int)

    s[sfirst:slast] is the string to be examined for pattern matches 

    match returns matcher, all results are stored in matcher.

    boundscheck will ensure valid sfirst, slast are existing index values

"""
function Base.match(matcher::BloomMatcher,ofs::UInt32,size::UInt64, s::String)
    # TODO matchofs must be changed from index to offset, and matchsize must be set
    n = ncodeunits(matcher.pattern)
    @boundscheck checkrange(ofs,size,s)
    m = size%Int
    matcher.matchsize = 0 # assign default value "not found"
    if n<=1
        # special trivial case: switch to byte search or nothing to search
        if n==0
            m>0 && (matcher.matchofs = ofs+1)
        else
            # Base works like this: something(findnext(isequal(codeunit(e.last,1)), s, sfirst), 0)
            # let-s compare performance with an ordinary loop
            while sfirst <= slast
                @inbounds if matcher.last == codeunit(s,sfirst)
                    matcher.matchofs = sfirst
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
                            matcher.matchofs = i-n+1
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
                        break # not matchofs
                    end
                    j += 1
                end
                if j > n # means: loop without break , we have a match at the very end
                    matcher.matchofs = i-n+1
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
    matchofs :: UInt32 #  index (1-based) in string to search of last match or 0 (no match found)
    matchsize :: UInt64 # 0 or 1
    variant :: UInt32 # index into pattern for match
    function AnyByteMatcher(pattern::Vector{UInt8})
        ismatch =  Vector{UInt8}(0%UInt8,256)
        for i in 1::length(pattern)
            ismatch[pattern[i]+1] = i
        end
        new(pattern,ismatch,0,0)
    end
end


#function Base.match(matcher::AnyByteMatcher,s::String, sfirst::Int, slast::Int)
function Base.match(matcher::AnyByteMatcher,ofs::UInt32,size::UInt64, s::String)
    @boundscheck checkrange(ofs,size,s)
    @inbounds begin
    endofs = ofs + (size%UInt32)
        while ofs < endofs
            test = ismatch[byte(s,ofs)]
            if test> 0
                matcher.matchofs = ofs
                matcher.matchsize = 1
                matcher.variant = test
                return matcher
            end
            sfirst += 1
        end
        matcher.matchsize = 0
        return matcher
    end
end




"""
Restricted but fast RegEx matcher. 

To achieve top search performance, it combines a Boyer-Moore alike 
search which advances usually more than one position when no match
was found, with parallelized pattern tests using set operations.
It really boosts regex search performance for long, complicated 
matches with lots of alternatives.

Matcher is constructed with an nonstandard string literal with
prefix *rr*. It is designed to be compatible with RegEx string constants,
implementing a RegEx subset: the quantifiers *, + and {..} are not supported.

String literal options are

    * i case insensitive matching (Ascii letters, only)

    * x white space (CR, LF, blank, tab) in the pattern definition 
      is ignored, and everything from a '#' until end of line is
      skipped, allowing for comments.

    * l use linux style end-of-line encoding: a single LF. 
      Affects the definitions of \\N and \$, see below


    * o use Apple OS style end-of-line encoding: a single CR. 
      Affects the definitions of \\N and \$, see below

    * w use windows style end-of-line encoding: a CR LF sequence
      Affects the definitions of \\N and \$, see below

 
Most byte values in the pattern are treatet as exact match, 
with the following exceptions:
   

    * \\x.. a byte value in hexadecimal notation, .. denotes two hex digits
      This is nonstandard for Regex implementations, but helpful if the 
      matcher is working with binary patterns

    * \\d any ASCII digit (0..9). 

    * \\D any byte which is NOT an ASCII digit (0..9)

    * \\w any ASCII word character (letter, digit, underscore).

    * \\W any byte which is NOT an ASCII word character (letter, digit, underscore)

    * \\w any ASCII word character (letter, digit, underscore).

    * \\W any byte which is NOT an ASCII word character (letter, digit, underscore)

    * \\s any white space byte: space, tab, newline, carriage return, vertical tab. With lexer: all continuations of raw category 16

    * \\S any byte which is NOT a white space byte: space, tab, newline, carriage return, vertical tab

    * \\r carriage return (0x13)

    * \\n line feed (0x10)

    * \\N neither \\r nor \\n

    * \\i switches for subsequent byte definitions to case unsensitive mode.
      Restriction: only ascii letters are covered, no further unicode letters.

    * \\I switches for subsequent byte definitions to case sensitive mode.
      This is the default, unless option 'i' is used.

    * * unsupported. Will cause a syntax error. 
      In Regex: preceding byte/group may appear 0 or more times 
      Workaround: try with a fixed number of repetitions like {0-5}
    
    * + unsupported. Will cause a syntax error. 
      In Regex: preceding byte/group may appear 1 or more times
      Workaround: try with a fixed number of repetitions like {1-5}
    
    * ^ unsupported. Will cause a syntax error.
      In Regex: matches begin of data/line, depending on the option'm' set or unset.

    * \$ matches the end of line sequence, see options 'l', 'o', 'w'.
      In Regex: matches end of data/line, depending on the option 'm' set or unset.
          
    * . any byte
      In RegEx: same behavior, if option flag 's' is set.
      To match any byte which is not part of the end of line sequence, use \\N

    * \\ followed by any other byte: just that byte. 
      Used to escape bytes which are otherwise interpreted as a command: 
      \\ . * + [ ] ? ( ) { } ^ \$ 
  
    * [ ... ] matches one of the bytes defined in the list of byte definitions the brackets. All escapes above are allowed
      
    * [^...] any byte not in [...]

    * [.-.] matches bytes of a range. range is given by two byte definitions separated by '-'
      
    * [^.-.] any byte not in [.-.]

    * (...) a group definition. ... denotes a list of byte definitions.
      It may contain any escapes and also groups, recursively.
      The groups are numbered sequentially from left to right.
      After a match, all group values in the match can be requested as tokens. 
      Example: the pattern "([a-z])([0-9])" matches "e5", 
      group 1 value is then "e", group 2 value is "5". 

      A group value can be the empty string if the group does not exist 
      in the matching pattern alternative (but in other alternatives),
      or if the group is defined to be empty. Empty groups are used in 
      pattern alternatives, see following chapters for ? and |.
    
    * \\1 .. \\9 Reference to a pattern group (first 9 groups). 
      In a replacement string definition, it gets replaced by the group value.
      In a pattern, it will be replaced by the group definition 
      (introducing an additional group), allowing re-use of lexer constructs. 
      
  
    * ? preceding byte or group specification is optional in the match.
      Every ? is resolved by replacing the original pattern by two alternative
      patterns, one with the optional byte/group, one without it.
      In the latter, all groups are still there, but encoded as empty groups. 
      Example: rr"((\\d)?\\d)" expands to two patterns, rr"((\\d)\\d)" and
      rr"(()\\d)". Group 2 is empty if only one digit was matched.
      
      n occurrences of '?' will result in 2^n patterns. Be aware of the technical
      limit in this implementation of up to 64 patterns. 
    
    * {n} results in repeating the preceding byte or group n times.
    
    * {n-m} results in repeating the preceding byte or group 
      a variable number of times, from n up to m. {0-1} is equivalent to ?.

      Processing is similar to ?: m-n+1 alternative patterns are created,
      one for each number from n to m. And if a group with inner groups
      is repeated, empty groups are added at the end of the repetitions,
      so that all variants have the same number of inner groups.  
        
    * | separates alternative patterns. Can be used on top level
      or in groups.  Having m '|' in the pattern (or group) results in m+1 
      alternative patterns. In each pattern, group index starts with the same value,
      there are no empty groups automatically added as in alternatives defined by '?'.
      If you want to distinguish groups coming from different alternatives, you must
      add sufficient many empty groups at the beginning of each alternative.

      m occurrences of '|' will result in m+1 patterns. This multiplies with the
      alternative patterns resulting from '?'. Be aware of the technical
      limit in this implementation of up to 64 alternative patterns after replacing
      all constructs defined by '?' and '|'.

    * {n} results in repeating the preceding byte or group n times.
    
    * {n-m} results in repeating the preceding byte or group 
      a variable number of times, from n up to m. {0-1} is equivalent to ?.

A simple example for a date format: rs"(\\d?\\d)/\\1/\\1\\1" 
It matches "1/2/21" and "01/02/2021", but also "99/0/389". 
It expands to rr"(\\d?\\d)/(\\d?\\d)/(\\d?\\d)(\\d?\\d)", 
showing that we have four groups. In a match, group 1 and 2 contain month 
and day, group 3 and 4 concatenated give the year.

It is possible to define a matcher which accepts only valid day and month
numbers, and only two and four digit year numbers: 
rr"((0?[123456789])|(1[012])):(\\2|([12]\\d)|(3[01])):(\\d\\d)?\\6". 
Carefully counting the groups, we find the month in group 1 and also group 2,
the day in group 3 and also in group 4, the complete year in group 5,
century (if matched) in group 6 and the last two year digits in group 7. 

There are still a few illegal day values accepted for months 
with fewer days than 31, like "04/31/2021". Covering those 
cases is possible, but requires different subpatterns for day 
depending on the month, and correctly matching only valid dates 
of the 29th of february is even dependent on the last two year digits. 
"""
mutable struct RestrictedRegex <: Matcher
    pattern :: Vector{UInt8} # pattern matches any of its bytes
    ismatch ::  Vector{UInt8} # 0 or index into pattern. index is 1+(byte value to test)
    matchofs :: UInt32 #  index (1-based) in string to search of last match or 0 (no match found)
    variant :: UInt32 # index into pattern for match
    function RestrictedRegex(pattern,options)
      println("pattern size: ",ncodeunits(pattern))
      println("options: ",options)
        
        println("pattern: ",pattern)
        error("not yet implemented.")
    end
end


# TODO RegEx der Datum, Zeit, Int, Real erkennt. Datum: ca 10 Varianten. Zeit: ca. 5 Varianten (mit/ohne Sekunden).
# Zahlen bis 16 Ziffern, danach nix,. oder e/E => 48 Varianten. Parser danach muss selten int und immer real noch fertig parsen
# ggf auch 10 digits und 5 nach Dezimalpunkt, dann werden auch die meisten reals erwischt.

macro rr_str(arg,options)
    RestrictedRegex(arg,options)
end



AnyStringMatcher(pattern::AbstractVector{T} ) where T <: AbstractString = AnyStringMatcher(BTokenVector(pattern))

#=
function match(matcher::AnyStringMatcher,s::String, sfirst::Int, slast::Int )
    @boundscheck checkbounds(s,sfirst:slast)
    @inbounds begin
        while sfirst <= slast
            test = ismatch[codeunit(s,sfirst)]
            if test> 0
                matcher.matchofs = sfirst
                matcher.variant = test
                return matcher
            end
        end
        matcher.matchofs = 0
        return matcher
    end
end
=#



