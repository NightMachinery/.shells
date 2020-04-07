ebook-cover() {
    mdocu '[<ebook>=$jufile <output>]
Saves the cover of the given ebook to <output>.' MAGIC
    local in="${1:-$jufile}" 
    local out="${2:-${in:r}.png}"
    sout ebook-meta --get-cover "$out" "$in"
}
alias jec=ebook-cover
epubsplit() {
	local file="$1"
	local pLn='^\s*Line Number:\s+(\d+)'
	local p1="${esP1:-toc:\s+\['\D*(\d+).*'\]}"
	local p2="${esP2:-id:\s+[cC]\D*(\d+)}"
	local i=0
	local n="${esN:-3}"
	local n1=$((n+1))
	local hasChanged=''
	local lm1=''
	local lm2=''
  local alreadyNoticed=''
	# typeset -A splits
	local currentSplit=0
	local split=()

  ecdbg start loop
	for line in "${(@f)$(epubsplit.py "$file")}"
	do
		hasChanged=''
		
		[[ "$line" =~ "$pLn" ]] && {
			split+="$match[1]"
      alreadyNoticed=''
			continue
		}
		
		[[ "$line" =~ "$p1" ]] && {
			  [[ "$match[1]" != "$lm1" ]] && { test -z "$alreadyNoticed" && hasChanged='y' ; alreadyNoticed=y }
			  lm1="$match[1]"
		}
		
		[[ "$line" =~ "$p2" ]] && {
        [[ "$match[1]" != "$lm2" ]] && { test -z "$alreadyNoticed" && hasChanged='y' ; alreadyNoticed=y }
        lm2="$match[1]"
    }
	
		test -n "$hasChanged" && {
    ecdbg line: "$line"
		i=$(( (i+1) % n1 ))
		[[ "$i" == 0 ]] && {
		i=1
		evaldbg epubsplit.py -p "p${currentSplit} " -o "p${currentSplit} ""$file" "$file" "${(@)split[1,-2]}"
		currentSplit=$((currentSplit+1))
		split=( $split[-1] )
		}
		}
	done
  test -z "${split[*]}" || evaldbg epubsplit.py -p "p${currentSplit} " -o "p${currentSplit} ""$file" "$file" "${split[@]}"
}
