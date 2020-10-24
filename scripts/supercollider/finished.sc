// https://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Print_version
// Fig 27.7: A multi-ringer
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
SynthDef(\dsaf_multialarm, {
	|length=0.05, freqs=#[600,800,600,800], timbre=1, repeats=inf|
	var freq, out, operations;
	freq = Duty.ar(length, 0, Dseq(freqs, repeats), doneAction: 2);
	freq = LPF.ar(freq, 70);
	out = LeakDC.ar(SinOsc.ar(freq));
	out = Select.ar(timbre, [out, (out * pi).sin, (out * pi).cos, ((out+0.25) * pi).cos]);
	// NOTE: when writing a synthdef always remember the Out ugen!
	// (Handy shortcuts like Ndef and {}.play often add Out on your behalf)
	Out.ar(0, Pan2.ar(out * 0.1))
}).add;
);

// finished
Synth(\dsaf_multialarm, [\length, 0.15, \freqs, [365, 571, 619, 206], \timbre, 3, \repeats, 2])

};
)
