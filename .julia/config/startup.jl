reloadStartup() = include(ENV["HOME"] * "/.julia/config/startup.jl")

# using TerminalExtensions

using Pkg
# Pkg.add("OhMyREPL")

# using OhMyREPL
using BenchmarkTools, Infiltrator, FreqTables, RDatasets, Lazy, UUIDs, Printf

using InteractiveCodeSearch
ENV["SHELL"] = @> `which dash` read(String) chomp # necessary for fzf's preview, and nice anyhow
InteractiveCodeSearch.CONFIG.interactive_matcher = `fzf --bind 'shift-up:toggle+up,shift-down:toggle+down,tab:toggle,shift-tab:toggle+beginning-of-line+kill-line,alt-/:toggle-preview,ctrl-j:toggle+beginning-of-line+kill-line,ctrl-t:top,ctrl-a:select-all' --color=light --multi --hscroll-off 99999  --preview 'printf -- "%s " {} | command fold -s -w $FZF_PREVIEW_COLUMNS' --preview-window down:7:hidden`

##
# bello() = run(`brishz.dash redo2 2 bell-greencase`, wait=false)
bello() = run(`brishz.dash awaysh bello`, wait=false)
bellj() = run(`brishz.dash awaysh bellj`, wait=false)
bella() = run(`brishz.dash awaysh bella`, wait=false)
okj() = run(`brishz.dash awaysh okj`, wait=false)
function firstbell()
    if ! @isdefined firstLoad
        bellj()
    else
        bello()
    end
    global firstLoad = true
end
##
using REPL
cmd_start_time = time() # to avoid buzzing on reloadStartup
is_continous_bell = false
bell_single_threshold_seconds = 10
bell_continuous_threshold_seconds = 60
function repl_pre()
    global is_continous_bell
    global cmd_start_time = time() # Get the system time in seconds since the epoch
    if is_continous_bell
        okj()
        is_continous_bell = false
    end
end
function repl_post()
    global cmd_start_time
    global is_continous_bell
    dur = time() - cmd_start_time
    if dur >= bell_continuous_threshold_seconds
        bellj()
        is_continous_bell = true
    elseif dur >= bell_single_threshold_seconds
        bello()
    end
end
function repl_transform_prepost(ex)
    res_sym = gensym("res_sym") # the string will be included in the name, helpful for debugging?
    Expr(:toplevel, :($repl_pre()), :($res_sym = $ex),:($repl_post()),:($res_sym))
end
if ! @isdefined BELL_LOADED
    if isdefined(Base, :active_repl_backend)
        if VERSION >= v"1.5.0-DEV.282"
            pushfirst!(Base.active_repl_backend.ast_transforms, repl_transform_prepost)
        else
            # Unsupported
        end
    elseif isdefined(Main, :IJulia)
        # Unsupported
        Main.IJulia.push_preexecute_hook(repl_pre)
    elseif VERSION >= v"1.5.0-DEV.282"
        pushfirst!(REPL.repl_ast_transforms, repl_transform_prepost)
    end
end
const BELL_LOADED = true
##
vscI() = pushdisplay(VSCodeServer.InlineDisplay())
vscINo() = popdisplay()
##
ensureDir(dest) = mkpath(dirname(dest))
##
macro currentFuncName()
    # Using `local` is optional, but it makes the code more readable
    return quote
        local st = stacktrace(backtrace())
        local myf = ""
        for frm in st
            funcname = frm.func
            if frm.func != :backtrace && frm.func!= Symbol("macro expansion")
                myf = frm.func
                break
            end
        end
        replace(string(myf), r"#?([^#]*)#?.*" => s"\1")
    end
end
macro codeLocation()
    # From https://discourse.julialang.org/t/how-to-print-function-name-and-source-file-line-number/43486/4
    return quote
        "Running function " * $("$(__module__)") * ".$(@currentFuncName) at " * $("$(__source__.file)") * ":" * $("$(__source__.line)")
    end
end

##
more(content) = more(repr("text/plain", content))
# using Markdown
# more(content::Markdown.MD) = more(Markdown.term(Core.CoreSTDOUT(), content))
function more(content::AbstractString)
    run(pipeline(`echo $(content)`, `less`))
    nothing
end
macro h(body)
    :(more(Core.@doc($(esc(body)))))
end

macro labeled(body)
    bodystr = string(body)
    quote
        out = $(esc(body))
        res = @> map(split(string(out),"\n")) do line
            "\t$line"
        end join("\n")
        println("$($bodystr) =>$(res)")
        out
    end
end

macro copycode(name, definition)
    # @def insertme some_code...
    # @insertme => would insert some_code... here
    # from http://www.stochasticlifestyle.com/type-dispatch-design-post-object-oriented-programming-julia/
  return quote
      macro $(esc(name))()
          esc($(Expr(:quote, definition)))
      end
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

# macro dotimes(n, body)
#     quote
#         for i = 1:$(esc(n))
#             $(esc(body))
#         end
#     end
# end

# macro dotimed(n, body)
#   :(@time @dotimes $(esc(n)) $(esc(body)))
# end

# Stop Julia from complaining about redifined consts/types -
# @defonce type MyType
#   ...
# end
# or
# @defonce const pi = 3.14

# macro defonce(typedef::Expr)
#   if typedef.head == :type
#     name = typedef.args[2]
#   elseif typedef.head == :typealias || typedef.head == :abstract
#     name = typedef.args[1]
#   elseif typedef.head == :const
#     name = typedef.args[1].args[1]
#   else
#     error("@defonce called with $(typedef.head) expression")
#   end

#   typeof(name) == Expr && (name = name.args[1]) # Type hints

#   :(if !isdefined(@__MODULE__, $(Expr(:quote, name)))
#       $(esc(typedef))
#     end)
# end

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

###
# if ! @isdefined SunHasSet
#     bello() # Julia's first startup takes forever
# end
const SunHasSet = true ;
