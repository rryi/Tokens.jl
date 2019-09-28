using Documenter, Tokens

makedocs(;
    modules=[Tokens],
    format=Documenter.HTML(),
    pages=[
        "Home" => "index.md",
    ],
    repo="https://github.com/rryi/Tokens.jl/blob/{commit}{path}#L{line}",
    sitename="Tokens.jl",
    authors="Robert Rudolph",
    assets=String[],
)

deploydocs(;
    repo="github.com/rryi/Tokens.jl",
)
