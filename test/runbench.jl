include("C:\\Users\\RR\\.julia\\dev\\Tokens\\test\\testsearch.jl")
benchmark(128,180,1000000,"R:\\benchmark 180.csv")

#=
stats = Stats(100)
t = rand(0x00:127%UInt8,100)
s = rand(0x00:127%UInt8,1000000)
sf = zeros(Int,Int(typemax(StatsFields)))
@code_native _searchindex_v1(s,t,1,sf)
=#
