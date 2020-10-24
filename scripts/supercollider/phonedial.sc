// s.boot;

// https://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Print_version
// 26: DTMF dialling tones
(
fork {
	var cond, runResponder;

	Server.default = s = Server.remote(\remote, NetAddr("127.0.0.1", 57110));

	cond = Condition({ s.serverRunning });

	// 'signal' will allow the forked routine to advance
	// only when 'serverRunning' finally becomes true
	runResponder = SimpleController(s).put(\serverRunning, { cond.signal });

	cond.wait;

	runResponder.remove;

(
// This data structure (like a "hashtable" or "associative array" in other languages)
// maps from a phone key to a pair of frequencies in Hz.
// We can push these frequencies to a synth.
~tbl = IdentityDictionary[
	$1 -> [[697, 1209]],
	$2 -> [[770, 1209]],
	$3 -> [[852, 1209]],
	$4 -> [[697, 1336]],
	$5 -> [[770, 1336]],
	$6 -> [[852, 1336]],
	$7 -> [[697, 1477]],
	$8 -> [[770, 1477]],
	$9 -> [[852, 1477]],
	$* -> [[697, 1633]],
	$0 -> [[770, 1633]],
	$# -> [[852, 1633]],
	$A -> [[941, 1209]],
	$B -> [[941, 1336]],
	$C -> [[941, 1477]],
	$D -> [[941, 1633]]
];

// Here we define a SynthDef which plays a single "number" at a time.
// Note that our strategy here is a bit different from the PD code in the book:
//   there, a single pair of sine-wave oscillators was re-used for each number,
//   whereas here, we create (and later free) an individual synth for each number.
SynthDef(\dtmf, {|freq=#[770, 1633], out=0, amp=0.2, gate=1|
	var son, env;
	son = SinOsc.ar(freq, 0, amp).sum;
	env = EnvGen.ar(Env.asr(0.001, 1, 0.001), gate, doneAction: 2);
	Out.ar(out, Pan2.ar(son * env * amp));
}).add;
) ;

// Check that it works:
// x = Synth(\dtmf) // create
// x.set(\gate, 0)  // free

(
// This pattern generates a random "phone number" and dials it
Pbind(
	\instrument, \dtmf,
	\dur, 0.2, // or for more "human" timing, try   Pwhite(0.2, 0.5, inf)
	\sustain, 0.15,
	\amp, 0.3,
	\freq, Prand(~tbl.asArray, 13)
).play;
) ;

// (
// // You could even dial a specific number:
// Pbind(
// 	\instrument, \dtmf,
// 	\dur, 0.2, // or for more "human" timing, try   Pwhite(0.2, 0.5, inf)
// 	\sustain, 0.15,
// 	\amp, 0.3,
// 	\freq, Pseq("012827743866".collectAs({|digit| ~tbl[digit] }, Array))
// ).play;
// ) ;
};
)
