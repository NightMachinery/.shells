function eval-dl() 
{ 
    case "$(uname)" in 
        Darwin) 
            eval "$1" 
            ;; 
        Linux) 
            eval "$2"
            ;;esac 
} 
function eval-darwin() 
{ 
    case "$(uname)" in 
        Darwin) 
            eval "${@:q}" 
            ;; 
        Linux) 
            
            ;;esac 
} 
function eval-linux() 
{ 
    case "$(uname)" in 
        Darwin) 

        ;; 
        Linux) 
            eval "${@:q}" 
        ;;esac 
} 
function psource() 
{ 
    if [[ -r $1 ]]; then 
        source $1 
    fi 
} 


function silence() {
    { eval "$@:q"  } &> /dev/null
}
