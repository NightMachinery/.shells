-- [[id:9a133a53-eb25-4715-b560-23620ce685c3][Hammerspoon/STT]]
--
-- might need [[id:628475aa-5096-48e9-8034-0bb3a77ac679][@qoute Allow microphone input for the app]]
local whisper = {}
whisper.state = "off"  -- Can be "off", "recording", or "processing"
whisper.indicator = nil
whisper.tempDir = os.getenv("HOME") .. "/tmp/hs_whisper/"
whisper.whisperDir = os.getenv("HOME") .. "/code/misc/whisper.cpp/"
whisper.model = "distil-large-v3"
whisper.language = "en"
whisper.recorderMode = "ffmpeg"
whisper.recordingTimeout = 3600

whisper.languageConfig = {
    en = {
        model = "distil-large-v3",
        emoji = "🎙️",
    },
    fa = {
        model = "large-v3",
        emoji = "🎤",
    }
}

function whisper.getRecordCommand(outputFile)
    local cmd
    if whisper.recorderMode == "sox-rec" then
        cmd = {
            command = "/opt/homebrew/bin/rec",
            args = {"-c", "1", outputFile}
        }
    else  -- ffmpeg
        cmd = {
            command = "/opt/homebrew/bin/ffmpeg",
            args = {
                "-f", "avfoundation",  -- Input format for macOS
                "-i", ":0",  -- Default audio input device
                "-t", tostring(whisper.recordingTimeout),
                "-ar", "16000",
                "-y", outputFile
            }
        }
    end

    -- Print the complete command for debugging
    print("Recording command: " .. cmd.command .. " " .. table.concat(cmd.args, " "))

    return cmd
end

function whisper.transcribeCommand(inputFile, language, backend)
    backend = backend or "whisper"

    local cmd
    if backend == "whisper" then
        local langConfig = whisper.languageConfig[language]
        cmd = {
            command = whisper.whisperDir .. "main",
            args = {
                "--model", whisper.whisperDir .. "models/ggml-" .. langConfig.model .. ".bin",
                "--language", language,
                "--no-timestamps",
                "--file", inputFile
            }
        }
    else
        cmd = {
            command = brishzq_binary,
            args = {
                "fnswap",
                "ecgray",
                "true",
                backend,
                "llm-stt-file",
                inputFile
            }
        }
    end

    -- Print the complete command for debugging
    print("Transcribe command: " .. cmd.command .. " " .. table.concat(cmd.args, " "))

    return cmd
end
whisper.processing_interrupted_p = false

function whisper_run(language)
    whisper.language = language
    if whisper.state == "off" then
        -- Ensure temp directory exists
        os.execute("mkdir -p " .. whisper.tempDir)

        -- Generate unique filename
        local timestamp = os.date("%Y%m%d_%H%M%S")
        local randomString = tostring(math.random(1000, 9999))
        local wavFile = whisper.tempDir .. "recording_" .. timestamp .. "_" .. randomString .. ".wav"

        local recordCommand = whisper.getRecordCommand(wavFile)
        whisper.task = hs.task.new(recordCommand.command, function(exitCode, stdOut, stdErr)
                                       if exitCode ~= 0 and whisper.state == "recording" then
                                           local msg = "Recording stopped unexpectedly. Exit code: " .. exitCode .. "\nError: " .. stdErr
                                           print(msg)
                                           hs.alert.show(msg, 10)
                                       end
                                       whisper.state = "off"
                                       updateIndicator()
                                       if hs.fs.attributes(wavFile) then
                                           processRecording(wavFile, language)
                                       else
                                           hs.alert.show("Failed to create recording file")
                                       end
        end, recordCommand.args)

        local success = whisper.task:start()
        if not success then
            hs.alert.show("Failed to start recording process")
            whisper.state = "off"
            updateIndicator()
            return
        else
            -- Start recording
            whisper.state = "recording"
            whisper.processing_interrupted_p = false  -- Reset interrupt flag when starting new recording

            -- sleep a bit to allow the recorder to become active
            hs.timer.usleep(0.3 * 1000000) -- usleep takes microseconds

            updateIndicator()
        end

        -- hs.alert.show("Started recording")
        whisper.currentWavFile = wavFile

    else
        -- Stop recording
        whisper.processing_interrupted_p = (whisper.state == "processing")  -- Set flag if stopping during processing
        if whisper.processing_interrupted_p then
            whisper.state = "off"
            updateIndicator()

        else
            whisper.state = "processing"
            updateIndicator()
        end

        if whisper.task then
            if whisper.recorderMode == "ffmpeg" then
                whisper.task:interrupt()
            else
                whisper.task:interrupt()
            end

            whisper.task = nil
        end

        -- hs.alert.show("Stopped recording, processing...")
    end
end

function processRecording(wavFile, language, backend)
    -- backend = backend or "whisper"
    -- backend = backend or "with-g15"
    -- backend = backend or "with-g25"
    -- backend = backend or "with-flash-8b"
    -- backend = backend or "with-flash2"
    backend = backend or "with-flash25"

    whisper.state = "processing"
    updateIndicator()


    local wavLogsDir = os.getenv("HOME") .. "/logs/hs/stt"
    mkdir(wavLogsDir)

    local wavLogsFile = io.open(wavLogsDir .. "/wav_files.txt", "a")
    wavLogsFile:write(wavFile .. "\n")
    wavLogsFile:close()

    local function resetState()
        whisper.state = "off"
        whisper.processing_interrupted_p = false
        updateIndicator()
    end

    local function handleTranscription(content)
        content = tostring(content)
        content = content:gsub("^%s+", "")
        content = content:gsub("%s+$", "")
        content = content .. " "

        hs.pasteboard.setContents(content)
        if not whisper.processing_interrupted_p then
            -- doPaste()
            -- Pasting makes us need to wait for the processing without doing anything.
            -- Let us just copy it and ring a bell.
            brishzeval("bell-transcription-ready")
        end
    end

    local transcribeCommand = whisper.transcribeCommand(wavFile, language, backend)
    local whisperTask = hs.task.new(transcribeCommand.command, function(exitCode, stdOut, stdErr)
        content = stdOut

        if exitCode == 0 then
            if content and not content:match("^%s*$") then
                handleTranscription(content)
            else
                hs.alert.show("Transcription empty")
            end
        else
            hs.alert.show("Transcription failed. Exit code: " .. exitCode .. "\nError: " .. stdErr)
        end
        resetState()
    end, transcribeCommand.args)

    local success = whisperTask:start()
    if not success then
        hs.alert.show("Failed to start transcription process")
        resetState()
    end
end

function updateIndicator()
    if not whisper.indicator then
        whisper.indicator = createIndicator()
    end

    local emoji, fillColor
    fillColor = { white = 1, alpha = 2 / 3 }
    if whisper.state == "recording" then
        emoji = whisper.languageConfig[whisper.language].emoji
        fillColor = {red = 0.5, green = 1, blue = 0.8, alpha = 0.7}
    elseif whisper.state == "processing" then
        emoji = "⚙️"
        fillColor = {red = 1, green = 1, blue = 0, alpha = 0.7}
    else  -- "off"
        emoji = "🔇"
    end

    whisper.indicator.textBox.text = emoji
    whisper.indicator.background.fillColor = fillColor

    if whisper.state == "off" then
        whisper.indicator:hide()
    else
        whisper.indicator:show()
    end
end

function createIndicator()
    local style = {
        fadeInDuration = 0.001,
        fadeOutDuration = 0.001,
        fillColor = {white = 1, alpha = 2/3},
        radius = 24,
        strokeColor = {red = 19/255, green = 182/255, blue = 133/255, alpha = 1},
        strokeWidth = 16,
        textColor = {white = 0.125},
        textSize = 64,
    }

    local indicator = hs.canvas.new{
        x=0,
        y=0,
        w=0,
        h=0}:insertElement{
        id = 'background',
        type = 'rectangle',
        action = 'strokeAndFill',
        fillColor = style.fillColor,
        roundedRectRadii = {xRadius = style.radius, yRadius = style.radius},
        strokeColor = style.strokeColor,
        strokeWidth = style.strokeWidth / 1.5,
        padding = style.strokeWidth / 3,
    }:insertElement{
        id = 'textBox',
        type = 'text',
        text = "🔇",
        textAlignment = 'center',
        textVerticalAlignment = 'center',
        textColor = style.textColor,
        textSize = style.textSize,
    }

    local textBoxSize = indicator:minimumTextSize(2, "🔇")
    local screenFrame = hs.screen.primaryScreen():fullFrame()
    local frame = {}
    frame.w = textBoxSize.w + style.strokeWidth*2 + style.textSize
    frame.h = textBoxSize.h + style.strokeWidth*2 + style.textSize
    frame.x = (screenFrame.w - frame.w)/2
    frame.y = (screenFrame.h - frame.h)/2

    indicator.textBox.frame = {
        x = (frame.w - textBoxSize.w)/2,
        y = ((frame.h - textBoxSize.h)/2) + 5,
        w = textBoxSize.w,
        h = textBoxSize.h,
    }
    indicator:frame(frame)

    indicator:behavior{'canJoinAllSpaces', 'transient', 'fullScreenAuxiliary'}

    return indicator
end

-- Function to toggle between recorder modes
function whisper.toggleRecorderMode()
    whisper.recorderMode = (whisper.recorderMode == "sox-rec") and "ffmpeg" or "sox-rec"
    hs.alert.show("Recorder mode set to: " .. whisper.recorderMode)
end

-- Language-specific run functions
function whisper_run_en()
    whisper_run("en")
end

function whisper_run_fa()
    whisper_run("fa")
end

-- hyper_bind_v1("escape", whisper_run_en)
-- hyper_bind_v1(".", whisper_run_en)
hyper_bind_v2{mods={"cmd"}, key=".", pressedfn=whisper_run_en}
hyper_bind_v2{mods={"ctrl"}, key=".", pressedfn=whisper_run_fa}

hs.hotkey.bind({}, 'F1', whisper_run_en)
hs.hotkey.bind({}, 'F2', whisper_run_fa)

hs.hotkey.bind({}, 'F11', whisper_run_en)
hs.hotkey.bind({}, 'F12', whisper_run_fa)

hyper_bind_v2{mods={}, key="'", pressedfn=whisper_run_en}
