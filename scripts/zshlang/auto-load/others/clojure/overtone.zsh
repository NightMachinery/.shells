OVERTONE_DIR=~/code/Clj/i-laugh
OVERTONE_PORT=7699

function ot-enabled-p {
  test -d "$OVERTONE_DIR"
}

function in-ot() {
  indir "$OVERTONE_DIR" "$@"
}

function ot-server() {
    : "See also ot-server-daemon."

    local dettach="${ot_server_d}"

    local cmd=':start' #':headless'
    # test -n "$attach" && cmd=':start'
    test -n "$dettach" && cmd=':headless'
    { in-ot lein repl $cmd :port "$OVERTONE_PORT" } always { }
}
##
redis-defvar ot_server_lock
function ot-server-daemon() {
  assert ot-enabled-p @RET

  if test -n "$(ot_server_lock_get)" ; then
    # the lock is needed to avoid continuously restarting Overtone; I.e., it gives the server the slack it needs to boot.
    tts-glados1-cached "Not restarting Overtone because of lock"
    return 0
  fi

  trapexits
  ot_server_lock_set 1
  {
    (
      tmuxnewsh2 "Overtone Server" ot_server_d='' ot-server # since we are firing up tmux, no need to go headless
      sleep 30 # it takes time for the server to boot up
      ot-loadovertone
    )
  } always {
    ot_server_lock_del
    trapexits-release
  }
}
##
function ot-loadovertone() {
  ot-rep "(use 'overtone.live) (use 'overtone.inst.piano)" && {
    # tts-gateway-i1 'Overtone, Online'
    tts-glados1-cached 'Overtone, Online'
    ot-play-beeps1 3
  }
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
    if isServer || isGuest ; then
        return 0
    fi

    local code="$(in-or-args "$@")"
    local cmd=(rep --port "$OVERTONE_PORT" "$code")
    local noretry="$ot_rep_noretry"

    "$cmd[@]" || {
      if test -z "$noretry" ; then
      ot_rep_noretry=y ot-server-daemon
      "$cmd[@]" && return 0
      fi
      redo2 3 fsay "Overtone is down ..."
      return 1
    }

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
    local dur="${1:-1}"

    ot-rep <<EOF

(def buf (buffer 2048))

; Bounce around cutting a single band out of white noise.
(demo $dur
  (let [rate 10
        src (* 0.8 (white-noise))
        freqs (fft buf src)
            filtered (pv-rand-comb freqs 0.95 (impulse:kr rate))]
    (pan2 (ifft filtered))))

EOF
}
function ot-play-helicopter() {
  : "See bell-helicopter for a durational API"

  ot-rep <<EOF
(definst trem [freq 440 depth 10 rate 6 ]
  (* 0.3
     (saw (max 10 (+ freq (* depth (sin-osc:ar rate)))))))

(trem :freq 100 :depth 10000 :rate 100)
EOF
}
function ot-play-diwhite() {
    local dur="${1:-1}" vol="${2}"
    if test -z "$vol" ; then
      if  (( $(volume-get) <= 60 )) ; then
        vol=30000
      elif  (( $(volume-get) <= 80 )) ; then
        vol=1
      else
        vol=0.07 # lower is better on the ears but it might not be heard then?
      fi
    fi
    # dvar vol


    ot-rep <<EOF
(demo $dur
      (let [vals (dwhite 0 15 INF)
            trig (impulse:kr (mouse-x 1 40 1))
            val (demand:kr trig 0 vals)
            poll (poll trig val "diwhite val:")
            freq (+ 340 (* 30 val))]
        (* $vol (sin-osc freq))))
EOF
}
function ot-play-happybirthday() {
    ot-rep << "EOF"
;; https://github.com/overtone/overtone/blob/master/src/overtone/examples/workshops/resonate2013/ex02_bday.clj
(use 'overtone.inst.piano)
(defn bpm
  "Higher order function. Returns another fn to compute the
  time offset in milliseconds for a beat at given `tempo`."
  [tempo]
  (fn [beat] (* (/ beat tempo) 60000)))

;; Next we encode a well know melody in an abstract manner
;; using musical degrees. Read this if you're unfamiliar with the concept:
;; http://en.wikipedia.org/wiki/Degree_(music)
;; Also highly recommended is this book for further contextualization:
;; http://www.amazon.co.uk/Quadrivium-Number-Geometry-Music-Heaven/dp/190715504X

(def melody
  "The Happy Birthday melody in scale-less musical degrees.
  The keyword :_ identifies a pause.
  Note durations are in bar measure (i.e. 1/4 = quarter note)."
  [;; Hap    py        birth      day        to        you
   [:v- 1/8] [:v- 1/8] [:vi- 1/4] [:v- 1/4]  [:i 1/4]  [:vii- 1/2]
   ;; Hap    py        birth      day        to        you
   [:v- 1/8] [:v- 1/8] [:vi- 1/4] [:v- 1/4]  [:ii 1/4] [:i 1/2]
   ;; Hap    py        birth      day        dear      Ri       car         do
   [:v- 1/8] [:v- 1/8] [:v 1/4]   [:iii 1/4] [:i 1/8]  [:i 1/8] [:vii- 1/4] [:vi- 1/4] [:_ 1/4]
   ;; Hap    py        birth      day        to        you
   [:iv 1/8] [:iv 1/8] [:iii 1/4] [:i 1/4]   [:ii 1/4] [:i 1/2]])

(defn play-tune
  "Takes an instrument, a sequence of notes and tempo (in bpm).
  Plays notes in separate thread."
  [inst bpm# root scale melody]
  (let [tempo (bpm bpm#)
        timings (reductions (fn[t [_ d]] (+ t (tempo (* d 4)))) (now) melody)
        root (note root)
        play-note (fn [timing [degree dur]]
                    (when-not (= :_ degree)
                      (at timing (inst (+ root (degree->interval degree scale))))))]
    (dorun (map play-note timings melody))))

(defn repeat-notes
  "Takes a melody sequence and repeats each note `n` times,
  with 1/n of its original duration. Returns new melody sequence."
  [n melody]
  (mapcat (fn [[deg dur]] (repeat n [deg (/ dur n)])) melody))

(defn arpeggiate
  "Similar to arpeggiate fn in the ex01_phrasestudy ns, but working with degrees
  instead of absolute notes and also supporting pauses. Since degrees are expressed
  as Roman numeral keywords (and not as number), we append `+` as suffix to indicate
  a note of the same degree only one octave higher."
  [n melody]
  (mapcat
   (fn [[deg dur]]
     (if-not (= :_ deg)
       (take n (cycle [[deg (/ dur n)] [(keyword (str (name deg) "+")) (/ dur n)]]))
       [[deg dur]]))
   melody))
(comment
  ;; Play the original Happy Birthday tune in F4 major
  (play-tune piano 120 :f4 :major melody)
  ;; The following experiments go ever further away from the original melody...
  ;; All this is only achieved through manipulating the original sequence
  ;; and/or choosing unusual scales. Since we only specified the melody in
  ;; degrees it will always be "in tune", regardless of scale changes
  (play-tune piano 120 :f4 :minor melody)
  (play-tune piano 120 :c4 :major (repeat-notes 2 melody))
  (play-tune piano 120 :c4 :major (arpeggiate 2 melody))
  (play-tune piano  60 :c4 :egyptian (arpeggiate 3 melody))
  (play-tune piano  60 :c4 :diminished (arpeggiate 4 (reverse melody)))
  )

  (play-tune piano 120 :c4 :major (arpeggiate 2 melody))
EOF
}
##
function ot-mp3() (
    B=$(basename "$1"); D=$(dirname "$1");
    ffmpeg -ss 1.5 -i "$1" -metadata artist="Our Apparitions" -metadata title="${B%.*}" -codec:a libmp3lame -qscale:a 1 "$D/${B%.*}.mp3" "${@:2}"
    ffmpeg -ss 1.5 -i "$1" -metadata artist="Our Apparitions" -metadata title="${B%.*}" -codec:a copy "$D/${B%.*}.trimmed.wav" "${@:2}"
)
function ot-wav() {
    B=$(basename "$1"); D=$(dirname "$1");
    ffmpeg -ss 1.5 -i "$1" -metadata artist="Our Apparitions" -metadata title="${B%.*}" -codec:a copy "$D/${B%.*}.trimmed.wav" "${@:2}"
}
