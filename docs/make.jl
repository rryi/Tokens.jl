using Tokens
using Documenter

DocMeta.setdocmeta!(Tokens, :DocTestSetup, :(using Tokens); recursive=true)

makedocs(;
    modules=[Tokens],
    authors="Robert Rudolph",
    repo="https://github.com/rryi/Tokens.jl/blob/{commit}{path}#{line}",
    sitename="Tokens.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://rryi.github.io/Tokens.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/rryi/Tokens.jl",
)
