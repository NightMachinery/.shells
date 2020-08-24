// Start server with: scsynth -u 57110 -a 1024 -i 2 -o 2 -R 0 -l 100  -B 127.0.0.1

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

    s.freeAll;
	// a = { SinOsc.ar(440, 0, 0.1).dup }.play;
    play{({|k|({|i|y=SinOsc;y.ar(i*k*k,y.ar(i*k**i/[4,5])*Decay.kr(Dust.kr(1/4**i),y.ar(0.1)+1*k+i,k*999))}!8).product}!16).sum}
};
)
