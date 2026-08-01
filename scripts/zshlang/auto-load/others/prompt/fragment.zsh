##
function fragment-export-save-session-to-notes {
    cat-copy-if-tty <<'EOF'
Summarize our findings into a single concise but complete note I can put into my notes.

Include the already found sources as links near where they are used. You do not need to browse the web or search.

Organize the notes into structured markdown.
EOF
}
##
function fragment-give-suggestions {
    cat-copy-if-tty <<'EOF'
Give suggestions to further improve this.
EOF
}
##
function fragment-summarize-session {
    cat-copy-if-tty <<'EOF'
Summarize our current session, so that I can store it in my notes. Be concise but thorough. I will directly copy your next message, so don't include any preamble.
EOF
    # Summarize the session for my notes. Be concise but thorough. Output only the summary without any extra commentary.
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
