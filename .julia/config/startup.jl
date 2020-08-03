const SunHasSet = true

import Pkg
# Pkg.add("OhMyREPL")

# using OhMyREPL
using BenchmarkTools, Infiltrator, FreqTables, RDatasets

more(content) = more(repr("text/plain", content))
# using Markdown
# more(content::Markdown.MD) = more(Markdown.term(Core.CoreSTDOUT(), content))
function more(content::AbstractString)
    run(pipeline(`echo $(content)`, `less`))
    nothing
end
macro d(body)
    :(more(Core.@doc($(esc(body)))))
end

macro labeled(body)
    bodystr = string(body)
    quote
        println("$($bodystr) =>\n\t$($(esc(body)))")
    end
end

function sa(x)
    show(IOContext(stdout, :limit=>false), MIME"text/plain"(), x)
    println()
end
function ec(x)
    println(x)
end
function sad(x)
    # ec density
    sa(prop(freqtable(x)))
end
function getdict(;kwargs...)
    return kwargs
end

## From: good macros here https://gist.github.com/MikeInnes/8299575
# Equivalent to @time @dotimes ... Consider using @benchmark.

# Repeat an operation n times, e.g.
# @dotimes 100 println("hi")

macro dotimes(n, body)
    quote
        for i = 1:$(esc(n))
            $(esc(body))
        end
    end
end

macro dotimed(n, body)
  :(@time @dotimes $(esc(n)) $(esc(body)))
end

# Stop Julia from complaining about redifined consts/types -
# @defonce type MyType
#   ...
# end
# or
# @defonce const pi = 3.14

macro defonce(typedef::Expr)
  if typedef.head == :type
    name = typedef.args[2]
  elseif typedef.head == :typealias || typedef.head == :abstract
    name = typedef.args[1]
  elseif typedef.head == :const
    name = typedef.args[1].args[1]
  else
    error("@defonce called with $(typedef.head) expression")
  end

  typeof(name) == Expr && (name = name.args[1]) # Type hints
  
  :(if !isdefined(@__MODULE__, $(Expr(:quote, name)))
      $(esc(typedef))
    end)
end

# Julia's do-while loop, e.g.
# @once_then while x < 0.5
#   x = rand()
# end

macro once_then(expr::Expr)
  @assert expr.head == :while
  esc(quote
    $(expr.args[2]) # body of loop
    $expr # loop
  end)
end

function memoize(f)
  cache = Dict()
  (args...) -> haskey(cache, args) ? cache[args] : (cache[args] = f(args...))
end

# Evaluate expressions at compile time, e.g.
# hard_calculation(n) = (sleep(1); n)
# f(x) = x * @preval hard_calculation(2) # takes 1 second
# f(2) => 4 # instant

macro preval(expr)
  eval(expr)
end

# Speed up anonymous functions 10x
# @dotimed 10^8 (x -> x^2)(rand()) # 3.9 s
# @dotimed 10^8 (@fn x -> x^2)(rand()) # 0.36 s

macro fn(expr::Expr)
  @assert expr.head in (:function, :->)
  name = gensym()
  args = expr.args[1]
  args = typeof(args) == Symbol ? [args] : args.args
  body = expr.args[2]
  @eval $name($(args...)) = $body
  name
end
