OVERTONE_DIR=~/code/Clj/i-laugh
OVERTONE_PORT=7699
aliasfn in-ot indir "$OVERTONE_DIR"
function ot-server() {
    : "See also ot-server-daemon."

    local dettach="${ot_server_d}"

    local cmd=':start' #':headless'
    # test -n "$attach" && cmd=':start'
    test -n "$dettach" && cmd=':headless'
    { in-ot lein repl $cmd :port "$OVERTONE_PORT" } always { }
}
function ot-server-daemon() {
    tmuxnewsh2 "Overtone Server" ot_server_d='' ot-server # since we are firing up tmux, no need to go headless
    sleep 30 # it takes time for the server to boot up
    ot-loadovertone
}
function ot-loadovertone() {
    ot-rep "(use 'overtone.live)"
}
function ot-rep() {
    ##
    # This needs redis to work, and it's not really necessary as rep errors out itselt.
    # if test -z "$OVERTONE_SERVER_STARTED" ; then
    #     ecerr "$0: You have not started the server. Run ot-server first. Aborting."
    #     return 1
    # fi
    # This one needs redis, too, but we circumvent it by using the tmux trick.
    # if test -z "$OVERTONE_LOADED" ; then
    #     # this, too, needs redis to work, but running it doesn't seem to hurt anyone so let's not bother?
    #     OVERTONE_LOADED='y'
    #     ot-loadovertone
    # fi
    ##
    local code="$(in-or-args "$@")"
    rep --port "$OVERTONE_PORT" "$code"
}
function ot-stop() {
    ot-rep "(stop)"
}
function ot-rep-test() {
    ot-rep '(demo (sin-osc 440))'
}
# Some examples here were forked from https://github.com/overtone/overtone/blob/master/src/overtone/examples/.
ot_test2="$(<<EOF
    (def count-down (sample (freesound-path 71128)))
(defsynth schroeder-reverb-countdown
  [rate 1]
  (let [input    (pan2 (play-buf 1 count-down rate :action FREE) -0.5)
        delrd    (local-in 4)
        output   (+ input [(first delrd) (second delrd)])
        sig      [(+ (first output) (second output)) (- (first output) (second output))
                  (+ (nth delrd 2) (nth delrd 3)) (- (nth delrd 2) (nth delrd 3))]
        sig      [(+ (nth sig 0) (nth sig 2)) (+ (nth sig 1) (nth sig 3))
                  (- (nth sig 0) (nth sig 2)) (- (nth sig 0) (nth sig 2))]
        sig      (* sig [0.4 0.37 0.333 0.3])
        deltimes (- (* [101 143 165 177] 0.001) (control-dur))
        lout     (local-out (delay-c sig deltimes deltimes))
        ]
    (out 0 output)))


(defsynth schroeder-reverb-mic
  [rate 1 dec 1 del 10 out-bus 0]
  (let [input    (pan2 (allpass-c (sound-in) 10  dec del))
        delrd    (local-in 4)
        output   (+ input [(first delrd) (second delrd)])
        sig      [(+ (first output) (second output)) (- (first output) (second output))
                  (+ (nth delrd 2) (nth delrd 3)) (- (nth delrd 2) (nth delrd 3))]
        sig      [(+ (nth sig 0) (nth sig 2)) (+ (nth sig 1) (nth sig 3))
                  (- (nth sig 0) (nth sig 2)) (- (nth sig 0) (nth sig 2))]
        sig      (* sig [0.4 0.37 0.333 0.3])
        deltimes (- (* [101 143 165 177] 0.001) (control-dur))
        lout     (local-out (delay-c sig deltimes deltimes))
        ]
    (out out-bus output)))

;; Spooky!
 (schroeder-reverb-countdown :rate 0.8 :dec 0.8 :del 10)
 (schroeder-reverb-mic :rate 0.8 :dec 0.8 :del 10)
EOF
)"
function ot-rep-test2() {
    : "You need to have gotten the sound (def count-down (sample (freesound-path 71128))) manually in the server, as it requires authentication. This sound also leaves an infinite loop of small noises in its trail, so use ot-stop to stop it."
    ot-rep "$ot_test2"
}
function ot-play-beeps1() {
    ot-rep <<EOF

(def buf (buffer 2048))

; Bounce around cutting a single band out of white noise.
(demo 10
  (let [rate 10
        src (* 0.8 (white-noise))
        freqs (fft buf src)
            filtered (pv-rand-comb freqs 0.95 (impulse:kr rate))]
    (pan2 (ifft filtered))))

EOF
}
function ot-play-helicopter() {
    ot-rep <<EOF
(definst trem [freq 440 depth 10 rate 6 ]
  (* 0.3
     (saw (max 10 (+ freq (* depth (sin-osc:ar rate)))))))

(trem :freq 100 :depth 10000 :rate 100)
EOF
}

##
