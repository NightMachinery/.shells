##
aliasfn prompt-audio-transcribe-stt prompt-audio-transcribe-stt-v4

function prompt-audio-transcribe-stt-v4 {
    cat-copy-if-tty << 'EOF'
Transcribe this audio word-for-word, following these rules:

1. Language will be either:
  - Farsi/Persian, or
  - English with an Iranian accent

2. Include only meaningful speech content:
  - Skip filler words and hesitation markers (um, uh, er, like, you know, "P", etc.)
  - Skip false starts, repetitions, and cut-off words
  - Skip all music and sound effects
  - Skip discourse markers (well, I mean, you know)
  - Omit words when in doubt

3. Format:
  - Pure transcription only
  - No comments
  - No timestamps
  - No explanatory notes
EOF
}


function prompt-audio-transcribe-stt-v3 {
    ec "Transcribe this audio word-for-word, following these rules:

1. Language will be either:
   - Farsi/Persian, or
   - English with an Iranian accent

2. Focus only on the speech:
   - Ignore and skip any music
   - Ignore and skip any sound effects
   - Do NOT insert meaningless words. Prefer to omit words when in doubt.
   - Skip

3. Format:
   - Pure transcription only
   - No comments
   - No timestamps
   - No explanatory notes" |
        cat-copy-if-tty
    # - Skip unclear words rather than guessing
    # - Don't add filler words
}


function prompt-audio-transcribe-stt-v2 {
    ec 'Transcribe this audio file exactly as spoken, following these rules:

1. If the speech is in Farsi (Persian):
   - Write it in Persian script
   - For any English words used, write them in Latin alphabet

2. If the speech is in English with an Iranian accent:
   - Write everything in Latin alphabet

Important: Provide only the transcription - no additional comments or notes.' |
        cat-copy-if-tty
}

function prompt-audio-transcribe-stt-v1 {
    ec 'Please provide a word-for-word transcription of the audio file.
The audio contains either:
- Farsi (Persian) language, or
- English spoken with an Iranian accent

Ignore potential background music and sound effects in the audio. Do NOT insert meaningless words. Prefer to omit words when in doubt.

Do not add any additional comments, notes, or timestamps - only transcribe the spoken words.' |
        cat-copy-if-tty
    # The Farsi speaker might occasionally use some English words during their speech. Transcribe the English words using the Latin alphabet.
}
##
function llm-stt-rec {
    #: [[id:6f8e0395-154e-45e7-b34b-a74b2a4ecab7][signal/catch]]
    ##
    setopt localtraps
    trap '' INT

    local audio
    audio="$(sox-record)" || {
        ecgray "$0: recording failed with $?"
        return 1
    }

    trap - INT
    ##

    # var-show audio


    llm-stt-file "${audio}"
}
aliasfn flash-stt-rec with-flash-8b llm-stt-rec

function llm-stt-file {
    local audio="${1}"
    if match-url "${audio}" ; then
        #: =llm= can handle URLs

    else
        assert test -e "${audio}" @RET

        local convert_p="${stt_convert_p:-y}"

        if bool "${convert_p}" && [[ "${audio:e}" != "mp3" ]] ; then
            audio="$(ffmpeg_print_out_p=y to-mp3 "${audio}")" @RET

            var-show audio
        fi

    fi

    if [[ "${llm_model}" =~ 'gpt-4o' ]]  ; then
        local llm_system
        llm_system="$(prompt-audio-transcribe-stt)" @TRET
    fi

    llm_attachments=("${audio}") reval-to-llm prompt-audio-transcribe-stt |
        cat-streaming-copy-rtl-if-tty
}
aliasfn flash-stt-file with-flash-8b llm-stt-file
##
