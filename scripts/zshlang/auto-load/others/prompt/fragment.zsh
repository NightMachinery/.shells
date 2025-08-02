##
function fragment-give-suggestions {
    cat-copy-if-tty <<'EOF'
Give suggestions to further improve this.
EOF
}
##
function fragment-summarize-session {
    cat-copy-if-tty <<'EOF'
Summarize our current session, so that I can store it in my notes. I will directly copy your next message, so don't include any preamble.
EOF
}
##
function fragment-sop-correct {
    cat-copy-if-tty <<'EOF'
Please review my attached Statement of Purpose (SoP) and analyze it in the following order:

1. Program Consistency Check
   - Identify which program I am applying to
   - Verify if the program name is mentioned consistently throughout the document

2. Technical Review
   - Check for spelling errors
   - Check for grammatical errors

3. General Issues
   - Identify any other problems or concerns

4. Improvements
   - Provide specific suggestions to strengthen the SoP

Please provide your feedback for each section separately.
EOF
}
##
