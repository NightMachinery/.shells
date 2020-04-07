ebook-cover() {
    mdocu '[<ebook>=$jufile <output>=cover.jpg]
Saves the cover of the given ebook to <output>.' MAGIC
    local in="${1:-$jufile}" out="${2:-cover.jpg}"
    ebook-meta --get-cover "$out" "$in"
}
alias jec=ebook-cover
epubsplit() {
	local file="$1"
	local pLn='^\s*Line Number:\s+(\d+)'
	local p1="${esP1:-toc:\s+\['\D*(\d+).*'\]}"
	local p2="${esP2:-id:\s+[cC]\D*(\d+)}"
	local i=1
	local n="${esN:-3}"
	local n1=$((n+1))
	local hasChanged=''
	local lm1=''
	local lm2=''
  local alreadyNoticed=''
	# typeset -A splits
	local currentSplit=0
	local split=()
  dact typ i

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
    dact typ i
		i=$(( (i+1) % n1 ))
    dact typ i
		[[ "$i" == 0 ]] && {
		i=1
		dact arger  -o "p${currentSplit} ""$file" "$file" "${split[@]}"
		epubsplit.py -o "p${currentSplit} ""$file" "$file" "${split[@]}"
		currentSplit=$((currentSplit+1))
		split=()
		}
		}
	done
	dact arger  -o "p${currentSplit} ""$file" "$file" "${split[@]}"
  test -z "${split[*]}" || epubsplit.py -o "p${currentSplit} ""$file" "$file" "${split[@]}"
}
