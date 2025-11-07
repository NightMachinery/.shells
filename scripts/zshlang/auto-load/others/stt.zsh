##
function stt-recordings-get {
    fd . ~tmp/hs_whisper | tac
}

function stt-recordings-rerun {
    stt-recordings-get | fz | inargsf rgeval-env with-g25-maybe llm-stt-file
}
##
