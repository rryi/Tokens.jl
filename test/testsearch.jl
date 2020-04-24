# include("C:\\Users\\RR\\.julia\\dev\\Tokens\\test\\testsearch4.jl")
# benchmark(128,100,1000000,"R:\\benchmark 100.csv")

#=
Ergebnis:

v1 hat das "RR-Verhalten": die ersten paar iter-s besser,
dann lange deutlich schlechter.

v2 ist wie julia

v3 ist signifikant um die 2% besser, wegen der skip-optimierung

Auffällig: die Stats zu loop,test,skip sind bei ju und v1,v2 IDENTISCH
Das macht das Laufzeitverhalten von v1 noch mysteriöser.

=#
# comparing _searchindex from base.search.jl with variants
# Author Robert Rudolph www.r2c.de
import Base.ByteArray, Base._nthbyte, Base._search_bloom_mask
using Plots

@enum StatsFields SFtime=1 SFloops SFbloomtests SFbloomskips SFbloombits

function bitcount(bitset::UInt64)
    count = 0
    while  bitset>0
        count += bitset&(1%UInt64)
        bitset = bitset >>>1
    end
    count
end

struct Stats
    dict:: IdDict{Function,Array{Int,2}}
    function Stats(size)
        d = IdDict(zeros=>zeros(Int,Int(size),Int(typemax(StatsFields))))
        new(d)
    end
end

function Base.print(io::IO,s::Stats)
    for p in s.dict
        for j in Int(typemin(StatsFields)):Int(typemax(StatsFields))
            print(io,string(p.first),",",StatsFields(j))
            for v in p.second[:,j]
                print(io,", ",v)
            end
            println(io)
        end
    end
end

const nullmat = zeros(Int,2)

function record(stats::Stats, f::Function, i::Int, sv::Vector)
    mat = get(stats.dict,f,nullmat)
    if mat===nullmat
        mat = copy(get(stats.dict,zeros,nullmat))
        push!(stats.dict,f=>mat)
    end
    mat[i,:] = sv
end


"return stats series for function and field"
function line(stats::Stats,f::Function, field::StatsFields)
    m = get(stats.dict,f,nullmat)
    m[:,Int(field)]
end

function _searchindex(s::String, t::String, i::Integer)
    # Check for fast case of a single byte
    lastindex(t) == 1 && return something(findnext(isequal(t[1]), s, i), 0)
    _searchindex(unsafe_wrap(Vector{UInt8},s), unsafe_wrap(Vector{UInt8},t), i)
end

const DOSTATS = false

include("_searchindex_naive.jl")
include("_searchindex_julia.jl")
include("_searchindex_v1.jl")
include("_searchindex_v2.jl")
include("_searchindex_v3.jl")
include("_searchindex_v4.jl")
include("_searchindex_v5.jl")
include("_searchindex_string.jl")


function runbench(f::Function,s::ByteArray,t::ByteArray, stats::Stats)
    #print(string(f),": ")
    sf = zeros(Int,Int(typemax(StatsFields)))
    elapsedtime = time_ns()
    pos=f(s,t,1,sf)
    sf[Int(SFtime)] = (time_ns() - elapsedtime)%Int
    record(stats,f,sizeof(t),sf)
    pos
end


function benchmark(alphabetsize::Int , patternsize::Int, textsize::Int, statsfile::String)
    s = rand(0x00:alphabetsize%UInt8,textsize) # array to search in
    ss = String(copy(s))
    stats = Stats(patternsize)
    for j in 4:patternsize
        tj = s[textsize-j+1:textsize]
        #println()
        #println("benchmark with pattern size $j")
        pn = runbench(_searchindex_naive,s,tj,stats)
        pj = runbench(_searchindex_julia,s,tj,stats)
        p1 = runbench(_searchindex_v1,s,tj,stats)
        p2 = runbench(_searchindex_v2,s,tj,stats)
        p3 = runbench(_searchindex_v3,s,tj,stats)
        p4 = runbench(_searchindex_v4,s,tj,stats)
        p5 = runbench(_searchindex_v5,s,tj,stats)
        ps = runbench(_searchindex_string,ss,String(tj),stats)
        if pn!=pj || pj!=p1 || p1!=p2 || p2!=p3 || p3!=p4 || p4!=p5 || p5 != ps
            println("ERROR pn=$pn, p1=$p1, p2=$p2, p3=$p3, p4=$p4, p5=$p5, pj=$pj, ps=$ps")
        else
            #println("found at $p0")
        end
    end
    if statsfile != ""
        io = open(statsfile, "w")
        print(io,stats)
        close(io)
    end

    # plot
    ##pp =plot(line(stats,_searchindex_naive,SFtime), label=string(_searchindex_naive))
    pp =plot()
    #for f in [_searchindex_naive,_searchindex_julia,_searchindex_v1,_searchindex_v2,_searchindex_v3,_searchindex_v4,_searchindex_v5 ]
    for f in [_searchindex_julia,_searchindex_v3,_searchindex_v4,_searchindex_v5,_searchindex_string ]
    #for f in [_searchindex_julia,_searchindex_v1,_searchindex_v2,_searchindex_v3,_searchindex_v4,_searchindex_v5]
        plot!(line(stats,f,SFtime), label=string(f))
    end
    display(pp)
end

#benchmark(128,3,100,"r:\\testfile.txt")
