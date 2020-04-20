include("C:\\Users\\RR\\.julia\\dev\\Tokens\\test\\testsearch.jl")
using Profile
Profile.init(n=10^6,delay=0.0001)
@profile benchmark(128,300,10000000,"R:\\profile.csv")
open("r:\\prof.txt", "w") do s
    Profile.print(IOContext(s, :displaysize => (24, 500)))
end