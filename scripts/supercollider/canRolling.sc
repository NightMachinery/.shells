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
);
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
);
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

};
)
