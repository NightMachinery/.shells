####
# This file has been forked from https://github.com/molovo/crash :
# The MIT License (MIT)
# Copyright (c) 2016 James Dinsdale <hi@molovo.co> (molovo.co)
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
####
# Changes:
# * Changed zshexit (a special hook that is run when the shell is about to exit) to zshexit_obs
# * changed all 'exit x' to 'return x'
####
#############################
# Internal helper functions #
#############################

###
# Output help text
###
function _crash_usage() {
  echo "\033[0;33mUsage:\033[0;m"
  echo "  crash [options] <command>"
  echo
  echo "\033[0;33mOptions:\033[0;m"
  echo "  -h, --help          Output this help text and exit"
  echo "  -v, --version       Output version information and exit"
  echo
  echo "\033[0;33mCommands:\033[0;m"
  echo "  register            Register the global error handler"
}

###
# Map a process exit code to its signal name
###
function _crash_map_exit_code() {
  local sig_name code="$1"

  # Note that the real exit codes are in [0, 255]: See `(exit 256)`
  case $code in
    # is this a signal name (error code = signal + 128) ?
    129) sig_name=HUP ;;
    130) sig_name=INT ;;
    131) sig_name=QUIT ;;
    132) sig_name=ILL ;;
    134) sig_name=ABRT ;;
    136) sig_name=FPE ;;
    137) sig_name=KILL ;;
    139) sig_name=SEGV ;;
    141) sig_name=PIPE ;;
    143) sig_name=TERM ;;

    # usual exit codes
    -1)  sig_name=FATAL ;;
    # Miscellaneous errors, such as "divide by zero"
    1)   sig_name=WARN ;;
    # misuse of shell builtins (pretty rare)
    2)   sig_name=BUILTINMISUSE ;;
    # cannot invoke requested command (ex : source script_with_syntax_error)
    126) sig_name=CCANNOTINVOKE ;;
    # command not found (ex : source script_not_existing)
    127) sig_name=CNOTFOUND ;;

    # assuming we are on an x86 system here
    # this MIGHT get annoying since these are in a range of exit codes
    # programs sometimes use.... we'll see.
    19) sig_name=STOP ;;
    20) sig_name=TSTP ;;
    21) sig_name=TTIN ;;
    22) sig_name=TTOU ;;

    # Exit codes used internally by crash
    333) sig_name=EXCEPTION ;;
    666) sig_name=TRACEBACK ;;
    667) sig_name=DEBUG ;;

    # Catch all - we have no idea what happened
    *)  sig_name=FATAL ;;
  esac

  echo $sig_name
}

###
# Format and output trace info
###
function _crash_format_trace_info() {
  local tracetype="$1"
  local -a str; str=("${(s/:/)2}")
  local node=${str[1]} line=${str[2]} color

  if [[ $tracetype = 'file' ]]; then
      if test -n "$node" ; then
         if builtin type grealpath >/dev/null 2>&1 ; then
             local res
             res="$(serr grealpath -e -- "$node")" && node="$res"
         fi
      else
          node="NA"
      fi
    color='\033[0;35m'
  fi

  if [[ $tracetype = 'function' ]]; then
    color='\033[1;33m'
  fi

  # echo -n 'HERE: '
  echo "$color$node\033[0;m \033[0;38;5;242m:\033[0;m line $line"

  if test -e "$node" && ! isbinary "$node" ; then
    local code c=0

    # @obviously if you edit the files then this linenumber can become stale
    code="$(cat "$node" | erase-bicon | gsed -n "$((line - c)),$((line + c))p" | prefixer -a $'\t')" || return 0

    if ! whitespace-is "$code" ; then
      ecngray "$code"
    fi
  fi
}

###
# A function which throws an exception if it hasn't been caught
###
function _crash_catch_all_exception() {
  if [[ ${2:0:5} != 'catch' ]]; then
    preexec_functions=()
    throw "$CRASH_THROWN_EXCEPTION" "$CRASH_THROWN_EXCEPTION_MESSAGE"
  fi
}

###
# A handler for catching global uncaught exceptions
# and other exit codes
###
function _crash_global_exception_handler() {
  # Get the state passed to the handler
  local state="$1" single_trace="${ectrace_single_trace}"
  state_code=( "${(ps.|.)state}" )
  state_code="${state_code[1]}"

  # We only want to report errors
  [[ "$state_code" == 0 ]] && return 0

  # Retrieve exception details stored by crash
  local exception="$CRASH_THROWN_EXCEPTION"
  local message="$CRASH_THROWN_EXCEPTION_MESSAGE"
  local -a trace; trace=($CRASH_FUNCTRACE)
  local -a files; files=($CRASH_FUNCFILETRACE)

  # If no trace is found, revert to ZSH's builtin $functrace
  [[ ${#trace} -eq 0 ]] && trace=($functrace)
  [[ ${#files} -eq 0 ]] && files=($funcfiletrace)

  # Get the signal name for the state_code
  sig_name="$(_crash_map_exit_code $state_code)"

  # Print the signal name as a header
  echo
  echo "  \033[1;37;41m ${sig_name} ${state} \033[0;m"

  # If an exception is defined, print it and its message
  if [[ -n $exception ]]; then
  echo
  echo "  \033[1;31m$exception\033[0;m"
  fi
  if test -n "$message" ; then
  [[ -n $exception ]] || echo
  echo "  \033[0;33m$message\033[0;m"
  fi


  _crash_print_one_trace "${trace[1]}" "${files[1]}" 0 ''


  # Loop backwards through the trace, printing the function
  # and file/line for each step
  integer i=1
  while [[ ${#trace} -gt 1 ]]; do
    shift trace
    shift files


    _crash_print_one_trace "${trace[1]}" "${files[1]}" 1 "$i"

    i=$(( i + 1 ))

    if bool "$single_trace" ; then
      break # only print the first trace
    fi
  done

  # Exit with the same state_code as reported, to ensure any
  # programs relying on the exit state_code receive the right one
  return $state_code
}
##
typeset -ag funcstack_excluded_names=( @opts h_@opts ectrace reval '(eval)' ensure ensure-dbg ensure-args assert assert-args assert-dbg redo redo2 p command_not_found_handler )
typeset -ag funcstack_excluded_prefixes=( reval eval geval rgeval seval memoi-eval ensure assert ec- ecdate ecerr ecnerr redo- )

function funcstack_excluded_prefixes_glob() {
  ec "(${(@j.|.)funcstack_excluded_prefixes})*"
}

function funcstack-isExcluded() {
  local name=("$1")
  name=(${(@)name:|funcstack_excluded_names})


  if test -z "$name" || [[ "${name}" == ${~"$(funcstack_excluded_prefixes_glob)"} ]] ; then
    return 0
  fi
  return 1
}
function _crash_print_one_trace() {
    local name_full=(${1}) file="${2}" formatting="${3:-0}" i="${4}"

    local excluded_names=("${funcstack_excluded_names[@]}")
    name=("${(s/:/)name_full}")
    name=("${name[1]}")

    if ! bool "$TRACE_NO_EXCLUDE" ; then
        name=(${(@)name:|excluded_names})
    fi


    if test -n "${name}" ; then
        if ! bool "$TRACE_NO_EXCLUDE" && ! bool "$TRACE_NO_EXCLUDE_PREFIXES" ; then
            if [[ "${name}" == ${~funcstack_excluded_prefixes_glob} ]] ; then
                return 0
            fi
        fi
        # Print the function and file/line where the exception was thrown
        echo
        if [[ "$formatting" == 0 ]] ; then
            echo "  in $(_crash_format_trace_info 'function' ${name_full})"
        else
            echo "  \033[0;38;5;242m$i\033[0;m   $(_crash_format_trace_info 'function' ${name_full})"
        fi
        if test -n "${file}" ; then
            if [[ "$formatting" == 0 ]] ; then
                echo "  at $(_crash_format_trace_info 'file' ${file})"
            else
                echo "      $(_crash_format_trace_info 'file' ${file})"
            fi
        fi
    fi
}

##################################
# Public facing helper functions #
##################################

###
# Execute a command in a subprocess, and record any exceptions
# which are thrown so that they can be caught later
###
function try() {
  local state

  # Check if an exception is already recorded and clear it
  # so that we have a blank slate
  if [[ -n $CRASH_THROWN_EXCEPTION ]]; then
    CRASH_THROWN_EXCEPTION=''
    CRASH_THROWN_EXCEPTION_MESSAGE=''
  fi

  # If the crash global error handler is set, remove it
  # while we handle anything returned from this function
  local global_handler_set=0
  if (( $+functions[zshexit_obs] )); then
    global_handler_set=1
    unfunction zshexit_obs
  fi

  # Run the passed command so that the crash exit state
  # can be caught
  output=$($@)
  state=$?

  # The exit wasn't caused by a crash exception, so unset any saved exception
  # and message, and call the global error handler directly
  if [[ $state -ne 333 ]]; then
    CRASH_THROWN_EXCEPTION=''
    CRASH_THROWN_EXCEPTION_MESSAGE=''

    # Print the output back to the screen
    echo $output

    # Call the globa error handler
    _crash_global_exception_handler $state
  fi

  # Ensure the crash global error handler is re-registered if necessary
  [[ -n $global_handler_set ]] && crash register

  # Check for the crash exit state, and parse the output to retrieve
  # the exception and message so that catch can handle them
  if [[ $state -eq 333 ]]; then
    # First separate the output into an array of lines
    local IFS
    local -a lines; IFS=$'\n' lines=($(echo "$output"))

    # Get the line count
    integer line_count=${#lines}

    # Get the last two lines of output
    # Since throw was used the exeption and message are printed here
    CRASH_THROWN_EXCEPTION="${lines[-2]}"
    CRASH_THROWN_EXCEPTION_MESSAGE="${lines[-1]}"

    # The remaining output all came from the subprocess, so we print
    # that as if it had been run on the main thread
    echo "${(@F)lines:0:$(( line_count - 2 ))}"

    # Store the functrace and funcfiletrace so that they can be used
    # if the exception is not caught
    CRASH_FUNCTRACE=($functrace)
    CRASH_FUNCFILETRACE=($funcfiletrace)

    # Add a preexec hook, which will execute the error handler for
    # the exception if it is not caught
    add-zsh-hook preexec _crash_catch_all_exception
  fi
}

###
# Catch a thrown exception
###
function catch() {
  local exception="$1" handler="$2"

  # If no exception has been thrown, then no further processing is required
  if [[ $CRASH_THROWN_EXCEPTION != $exception ]]; then
    return
  fi

  # Great! We've caught the right exception. Now we can remove the
  # preexec zsh-hook so that the catch-all handler doesn't kick in
  preexec_functions=()

  # Check that a handler has been passed to catch
  if [[ -z $handler ]]; then
    throw MissingHandlerException "catch must be passed a handler function"
  fi

  # Check that the handler exists as a function
  if ! (( $+functions[$handler] )); then
    throw MissingHandlerException "Handler function '$handler' could not be found"
  fi

  # Execute the handler, passing it the exception and message
  $handler $CRASH_THROWN_EXCEPTION $CRASH_THROWN_EXCEPTION_MESSAGE

  CRASH_THROWN_EXCEPTION=''
  CRASH_THROWN_EXCEPTION_MESSAGE=''
  CRASH_FUNCTRACE=()
  CRASH_FUNCFILETRACE=()
}

###
# Throw an exception
###
function throw() {
    local exception="$1" message="${(@)@:2}"
    local retcode="${throw_ret:-333}" # @warn 333 is (how necessary is this?) used internally by the catch/try system

  # Store the exception, message and traces in their respective global variables
  CRASH_THROWN_EXCEPTION="$exception"
  CRASH_THROWN_EXCEPTION_MESSAGE="$message"
  CRASH_FUNCTRACE=($functrace)
  CRASH_FUNCFILETRACE=($funcfiletrace)

  # Check for the global crash error handler. If it is set, then we don't
  # need to output the exception and message, as they can be read from
  # the variables we set above. If the global handler is not set, then we
  # are probably in a try/catch block, so print them here so that they
  # can be picked up by the subprocess launched by try
  if ! (( $+functions[zshexit_obs] )); then
    # echo $CRASH_THROWN_EXCEPTION
    # echo $CRASH_THROWN_EXCEPTION_MESSAGE
    ##
    _crash_global_exception_handler $retcode
    return $retcode
  else
      zshexit_obs
      return $retcode
  fi

  return $retcode
}

###
# Register the global variables and exception handler
###
function _crash_register() {
  # Autoload preexec hook
  autoload -Uz add-zsh-hook

  # Initialise variables used by the exception handler
  export CRASH_THROWN_EXCEPTION=''
  export CRASH_THROWN_EXCEPTION_MESSAGE=''
  export -a CRASH_FUNCTRACE
  export -a CRASH_FUNCFILETRACE

  # Run the exception hander after any exit signal is received
  zshexit_obs() {
    _crash_global_exception_handler $?
  }
}

function _crash_unload() {
  # Remove the catch all exception in case we are
  # in the middle of a catch block
  add-zsh-hook -D preexec _crash_catch_all_exception

  # Remove the global exception handler
  zshexit_obs() {}
}

###
# The main process
###
function crash() {
  local help version ctx="$1"

  zparseopts -D \
    h=help -help=help \
    v=version -version=version

  if [[ -n $help ]]; then
    _crash_usage
    return
  fi

  if [[ -n $version ]]; then
    echo 0.1.2
    return
  fi

  case $ctx in
    'register' )
      _crash_register "${(@)@:2}"
      ;;
    'unload' )
      _crash_unload
      ;;
    * )
      throw UnrecognisedCommandError "The command '$1' is unrecognised"
      ;;
  esac
}
