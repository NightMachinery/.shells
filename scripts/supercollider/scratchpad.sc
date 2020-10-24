s.boot;
///
(
Ndef(\alarm, {
	var freq, out;
	freq = Duty.kr(0.5, 0, Dseq([723, 932, 1012, 500], inf));
	freq = LPF.kr(freq, 70);
	out = SinOsc.ar(freq);
	Pan2.ar(out * 0.1)
}).play
)
///
(
Ndef(\alarm, {
	var freq, out, operations;
	freq = Duty.kr(0.05, 0, Dseq([723, 932, 1012], inf));
	freq = LPF.kr(freq, 70);
	out = SinOsc.ar(freq);
	operations = [out, (out * pi).sin, (out * pi).cos, ((out+0.25) * pi).cos];
	out = Select.ar(MouseX.kr(0,4).poll, operations);
	Pan2.ar(out * 0.1)
}).play
)
///
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
)

// happy blips
Synth(\dsaf_multialarm, [\length, 0.1, \freqs, [349, 0, 349, 0], \timbre, 1, \repeats, 1]);
// affirmative
Synth(\dsaf_multialarm, [\length, 0.1, \freqs, [238, 0, 317, 0], \timbre, 2, \repeats, 1]);
// activate
Synth(\dsaf_multialarm, [\length, 0.02, \freqs, [300, 125, 0, 0], \timbre, 2, \repeats, 10]);
// invaders?
Synth(\dsaf_multialarm, [\length, 0.03, \freqs, [360, 238, 174, 158], \timbre, 1, \repeats, 5]);
// information
Synth(\dsaf_multialarm, [\length, 0.05, \freqs, [2000, 2010, 2000, 2010], \timbre, 1, \repeats, 6]);
// message alert
Synth(\dsaf_multialarm, [\length, 0.15, \freqs, [619, 571, 365, 206], \timbre, 1, \repeats, 2]);
// finished
Synth(\dsaf_multialarm, [\length, 0.15, \freqs, [365, 571, 619, 206], \timbre, 3, \repeats, 2]);
// error code
Synth(\dsaf_multialarm, [\length, 0.01, \freqs, [1000, 0, 1000, 0], \timbre, 3, \repeats, 30]);
// wronnnnnnnnnng
(
Pbind(
	\instrument, \dsaf_multialarm,
	\freqs, [[1000, 476, 159, 0]],
	\timbre, 2,
	\repeats, 25,
	\length, Pseq([0.003, 0.005]),
	\dur, 0.5
).play
)
///
(
SynthDef(\dsaf_horn1, { |rate=0.1|
	var freq = LFPulse.kr(rate, 0.99, 0.4).lagud(0.4 / rate, 0.6 / rate) * 800 + 300;
	var son  = LFPulse.ar(freq, 0.99, 0.2).lagud(0.4 / freq, 0.6 / freq) * 2 - 1;

	// This filtering is a simple approximation of the plastic horn acoustics:
	son = BPF.ar(son.clip2(0.2), 1500, 1/4) * 4;

	// delay and reverb, to simulate the environment in which we hear the siren
	son = son + DelayC.ar(son, 0.1, 0.1, 0.3);
	son = son + FreeVerb.ar(son);

	Out.ar(0, Pan2.ar(son * 0.4));
}).add;
)

x = Synth(\dsaf_horn1);

// Choose a rate
x.set(\rate, 3);
///
(
{
	var son;
	son = Klank.ar(`[
		[521, 732, 934],  // freqs
		[0.7, 0.45, 0.25],// amps
		[0.8, 0.8, 0.8]   // ring times
		],
	Impulse.ar(1));
	Pan2.ar(son * 0.2)
}.play
)
///
(
SynthDef(\dsaf_phonebell1, { |freq=465, strength=1, decay=3|
	var son;
	son = Klank.ar(`[
		// frequency ratios
		[0.501, 1, 0.7,   2.002, 3, 9.6,   2.49, 11, 2.571,  3.05, 6.242, 12.49, 13, 16, 24],
		// amps
		[0.002,0.02,0.001, 0.008,0.02,0.004, 0.02,0.04,0.02, 0.005,0.05,0.05, 0.02, 0.03, 0.04],
		// ring times - "stutter" duplicates each entry threefold
		[1.2, 0.9, 0.25, 0.14, 0.07].stutter(3)
		]
	, Impulse.ar(1), freq, 0, decay);
	Out.ar(0, Pan2.ar(son));
}).add
)
x = Synth(\dsaf_phonebell1, [\freq, 500]);
(
SynthDef(\dsaf_phonecase1, { |in=0, out=0, mix=0|
	var casein = In.ar(in, 2);

	var delayA = CombC.ar(casein, 0.00077, 0.00077, 0.1);
	var delayB = CombC.ar(delayA, 0.00088, 0.00088, 0.1);
	var bands = BPF.ar(delayB, [1243, 287, 431], 1/12).sum;
	var son = bands.clip2(0.3);

	ReplaceOut.ar(out, XFade2.ar(casein, son, mix))

}).add
)
y = Synth(\dsaf_phonecase1, target: x, addAction: \addAfter);
///
(
Ndef(\bouncer, {

var bounceperiod, bouncetrigs, amp, fm, mainosc;

bounceperiod = Line.kr(0.3, 0, 3, doneAction: 2);

bouncetrigs = Impulse.kr(bounceperiod.reciprocal.min(30));

amp = EnvGen.ar(Env.perc(0.001, 0.0), bouncetrigs);
amp = Amplitude.ar(amp, 0, bounceperiod) * Line.kr(1, 0.05, 3);

fm =
	SinOsc.ar(120).range(0, Line.ar(1, 0, 3))
		+
	(amp * Line.ar(1, 0, 3).cubed * 130 + 80)
;

mainosc = SinOsc.ar(fm, pi/2);

amp * mainosc;
}).play
)
/// @good
(
x = { |t_trig=0|
	// This line just creates a sharp little spike whenever we want:
	var strike = EnvGen.ar(Env.perc(0.0001, 0.001, 0.1), t_trig);
	// here's the resonances:
	var son = Ringz.ar(strike, [359, 426, 1748, 3150], 0.2).sum;
	// some distortion livens up the spectrum a little:
	son = HPF.ar(son.clip2(0.6), 300);
	son * 0.2
}.play;
)
x.set(\t_trig, 1); // Run this line to hit the can!
/// turned into canRolling.sc:
(
~regularroll = { |rate = 1|
	// In the original code, Andy uses a master phase control,
	// wrapping and re-scaling it before differentiating, to produce
	// a repeating but disorderly set of impulses.
	// Here we do it differently - we use Impulse.kr to generate the
	// impulses directly.
	// We evaluate this function multiple times using .dup so that
	// we get a whole set of impulses with random phase positions.
	{
		Impulse.kr(rate, 1.0.rand, 1.0.bilinrand)
	}.dup(10).sum
};
// ~regularroll.plot(2);
)
(
// This signal contribution to rolling signature based on Mathias Rath's idea - see 'The Sounding Object'
// (ajf2009)
//
~irregularground = { |rate=10|
	var trigs = Dust.kr(rate);
	EnvGen.ar(

		Env([0,0,-1,1.5,-1,1,0], [0.1, 0.1, 0.001, 0.001, 0.1, 0.1], 'sine'),
		trigs
	) * 0.1
};
// ~irregularground.plot(4)
)
(
x = {
	var rate, strike, son;
	// rate of motion starts fast and tails off
	rate = XLine.kr(4, 0.001, 8, doneAction: 2);
	// This rate affects both the regular rolling, and the irregular ground contacts.
	strike =
		~irregularground.(rate*2) * 0.04
			+
		K2A.ar(~regularroll.(rate) * 0.1)
			;
	// Force the strikes to die off in intensity:
	strike = strike * XLine.ar(1, 0.0001, 8);
	// And here are the tin-can resonances as in fig 31.3:
	son = Ringz.ar(strike, [359, 426, 1748, 3150], 0.2).sum;
	son = HPF.ar(son.clip2(0.6), 300);
	son * 0.2
}.play;
)
/// @tosee Fig 32.3: formants for a wooden door