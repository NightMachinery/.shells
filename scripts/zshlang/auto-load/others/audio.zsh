##
function audio-denoise-sox {
    #: Check if SoX and ffmpeg are installed
    assert isdefined-cmd sox @RET
    assert isdefined-cmd ffmpeg @RET

    local input_file="${1}"
    local output_file="${2:-${input_file:r}_denoised.wav}"
    local end_of_noise="${denoise_end:-1.0}"
    local start_of_noise="${denoise_start:-0}"
    local noise_sensitivity="${denoise_s:-0.21}"
    assert-args input_file @RET

    # Check file extension and convert to WAV if necessary
    local actual_input_file="$input_file"
    if [[ "${input_file:e}" != "wav" ]]; then
        actual_input_file="$(gmktemp --dry-run --suffix=".wav")" @TRET
        assert reval-ecgray ffmpeg -i "$input_file" -acodec pcm_s16le -ar 44100 -ac 2 "$actual_input_file" @RET
        # -af "pan=mono|FC=FR"
    fi

    #: Temporary files for noise sample and noise profile
    local noise_sample
    noise_sample="$(gmktemp --suffix=".wav")" @TRET

    local noise_profile
    noise_profile="$(gmktemp --suffix=".noise")" @TRET

    local temp_output_file
    temp_output_file="$(gmktemp --suffix=".wav")" @TRET

    {
        #: Extract noise sample
        assert reval-ecgray sox "$actual_input_file" "$noise_sample" trim "${start_of_noise}" "${end_of_noise}" @RET

        #: Generate noise profile
        assert reval-ecgray sox "$noise_sample" -n noiseprof "$noise_profile" @RET

        #: Perform noise reduction
        assert reval-ecgray sox "$actual_input_file" "$temp_output_file" noisered "$noise_profile" "${noise_sensitivity}" @RET

        #: Convert the temporary WAV output to the desired format if necessary
        if [[ "${output_file:e}" != "wav" ]]; then
            assert reval-ecgray ffmpeg -i "$temp_output_file" "$output_file" @RET
        else
            gmv -i "$temp_output_file" "$output_file"
        fi
    } always {
        #: Clean up temporary files
        trs "$noise_sample" "$noise_profile" "$temp_output_file" || true
        if [[ "$actual_input_file" != "$input_file" ]]; then
            trs "$actual_input_file" || true
        fi
    }

    # ec "$output_file"
}
@opts-setprefix audio-denoise-sox denoise
##
function ffmpeg-trim {
    #: Check if ffmpeg is installed
    assert isdefined-cmd ffmpeg @RET

    local input_file="${1}"
    local start_time="${2}"
    local end_time="${3}"
    local output_file="${4:-${input_file:r}_trimmed.${input_file:e}}"
    assert-args input_file start_time end_time @RET

    #: Trim the file using ffmpeg
    assert reval-ecgray ffmpeg -i "$input_file" -ss "$start_time" -to "$end_time" -c copy "$output_file" @RET

    # ec "$output_file"
}
##
