Engine_Ack : CroneEngine {
	classvar debug = false;
	var useScdBasedAck = false;

	var numChannels = 8;

	var buffers;
	var channelGroups;
	var channelControlBusses;
	var samplePlayerSynths; // TODO: not sure this is needed anymore since synths now are self-freeing and communication is made via channelGroups
	var <muteGroups;

	var sourceGroup;
	var effectsGroup;
	var delayBus;
	var reverbBus;
	var delaySynth;
	var reverbSynth;

	var mainLevelGroup;
	var mainLevelBus;
	var mainLevelSynth;

	var loopEnabled;

	var channelSpecs;
	var filterCutoffSpec;

	var delayTimeSpec;
	var delayFeedbackSpec;
	var delayLevelSpec;

	var reverbRoomSpec;
	var reverbDampSpec;
	var reverbLevelSpec;
	var mainLevelSpec;

	var <scdBasedAckInstance;
	var init, free, loadSampleCommand, multiTrigCommand, trigCommand, multiKillCommand, killCommand, includeInMuteGroupCommand, sampleStartCommand, sampleEndCommand, loopPointCommand, enableLoopCommand, disableLoopCommand, speedCommand, volumeCommand, volumeEnvAttackCommand, volumeEnvReleaseCommand, panCommand, filterCutoffCommand, filterResCommand, filterModeCommand, filterEnvAttackCommand, filterEnvReleaseCommand, filterEnvModCommand, distCommand, sampleRateCommand, bitDepthCommand, delaySendCommand, reverbSendCommand, delayTimeCommand, delayFeedbackCommand, delayLevelCommand, reverbRoomCommand, reverbDampCommand, reverbLevelCommand, mainLevelCommand;

	*new { |context, callback| ^super.new(context, callback) }

	monoSamplePlayerDefName { ^(this.class.name.asString++"_Mono").asSymbol }
	stereoSamplePlayerDefName { ^(this.class.name.asString++"_Stereo").asSymbol }
	delayDefName { ^(this.class.name.asString++"_Delay").asSymbol }
	reverbDefName { ^(this.class.name.asString++"_Reverb").asSymbol }
	mainLevelDefName { ^(this.class.name.asString++"_MainLevel").asSymbol }

	alloc {
		if (useScdBasedAck) {
			this.scdBasedAlloc;
		} {
			this.scBasedAlloc;
		};
	}

	scBasedAlloc {
		loopEnabled = Array.fill(8) { false };
		channelSpecs = IdentityDictionary.new;
		channelSpecs[\sampleStart] = \unipolar.asSpec;
		channelSpecs[\sampleEnd] = \unipolar.asSpec.copy.default_(1);
		channelSpecs[\loopPoint] = \unipolar.asSpec;
		channelSpecs[\loopEnable] = ControlSpec(0, 1, step: 1, default: 0);
		channelSpecs[\speed] = ControlSpec(0, 5, default: 1);
		channelSpecs[\volume] = \db.asSpec.copy.default_(-10);
		channelSpecs[\delaySend] = \db.asSpec;
		channelSpecs[\reverbSend] = \db.asSpec;
		channelSpecs[\volumeEnvAttack] = ControlSpec(0, 1, default: 0.001, units: "secs");
		channelSpecs[\volumeEnvRelease] = ControlSpec(0, 3, default: 3, units: "secs");
		channelSpecs[\filterEnvAttack] = ControlSpec(0, 1, default: 0.001, units: "secs");
		channelSpecs[\filterEnvRelease] = ControlSpec(0, 3, default: 0.25, units: "secs");
		channelSpecs[\pan] = \pan.asSpec;
		channelSpecs[\filterCutoff] = \widefreq.asSpec.copy.default_(\widefreq.asSpec.maxval);
		filterCutoffSpec = channelSpecs[\filterCutoff];
		channelSpecs[\filterRes] = \unipolar.asSpec;
		channelSpecs[\filterLowpassLevel] = ControlSpec(0, 1, step: 1, default: 1);
		channelSpecs[\filterBandpassLevel] = ControlSpec(0, 1, step: 1, default: 0);
		channelSpecs[\filterHighpassLevel] = ControlSpec(0, 1, step: 1, default: 0);
		channelSpecs[\filterNotchLevel] = ControlSpec(0, 1, step: 1, default: 0);
		channelSpecs[\filterPeakLevel] = ControlSpec(0, 1, step: 1, default: 0);
		channelSpecs[\filterEnvMod] = \bipolar.asSpec;
		channelSpecs[\sampleRate] = ControlSpec(0, 44100.0, default: 44100.0);
		channelSpecs[\bitDepth] = ControlSpec(0, 32, default: 32);
		channelSpecs[\dist] = \unipolar.asSpec;

		delayTimeSpec = ControlSpec(0.0001, 5, 'exp', 0, 0.1, "secs");
		delayFeedbackSpec = ControlSpec(0, 1.25);
		delayLevelSpec = \db.asSpec.copy.default_(-10);

		reverbRoomSpec = \unipolar.asSpec.copy.default_(0.75);
		reverbDampSpec = \unipolar.asSpec.copy.default_(0.5);
		reverbLevelSpec = \db.asSpec.copy.default_(-10);
		mainLevelSpec = \db.asSpec.copy.default_(0);

		// TODO: there's too much code duplication between mono and stereo SynthDef, look into using SynthDef.wrap or splitting up SynthDef for DRY
		SynthDef(
			(this.monoSamplePlayerDefName.asString).asSymbol,
			{
				|
				gate,
				out=0,
				delayBus,
				reverbBus,
				bufnum,
				sampleStart, // start point of playing back sample normalized to 0..1
				sampleEnd, // end point of playing back sample normalized to 0..1. sampleEnd prior to sampleStart will play sample reversed
				loopPoint, // loop point position between sampleStart and sampleEnd expressed in 0..1
				loopEnable, // loop enabled switch (1 = play looped, 0 = play oneshot). argument is initial rate so it cannot be changed after a synth starts to play
				speed,
				volume,
				volumeEnvAttack,
				volumeEnvRelease,
				pan,
				filterCutoff,
				filterRes,
				filterLowpassLevel,
				filterBandpassLevel,
				filterHighpassLevel,
				filterNotchLevel,
				filterPeakLevel,
				filterEnvAttack,
				filterEnvRelease,
				filterEnvMod,
				sampleRate,
				bitDepth,
				dist,
				delaySend,
				reverbSend
				|
				var direction = (sampleEnd-sampleStart).sign; // 1 = forward, -1 = backward
				var leftmostSamplePosExtent = min(sampleStart, sampleEnd);
				var rightmostSamplePosExtent = max(sampleStart, sampleEnd);
				var onset = Latch.ar(sampleStart, Impulse.ar(0)); // "fixes" onset to sample start at the time of spawning the synth, whereas sample end and *absolute* loop position (calculated from possibly modulating start and end positions) may vary
				var sweep = Sweep.ar(1, speed/BufDur.kr(bufnum)*direction); // sample duration normalized to 0..1 (sweeping 0..1 sweeps entire sample).
				var oneshotPhase = onset + sweep; // align phase to actual onset (fixed sample start at the time of spawning the synth)

				var fwdOneshotPhaseDone = ((oneshotPhase > sampleEnd) * (direction > (-1))) > 0; // condition fulfilled if phase is above current sample end and direction is positive
				var revOneshotPhaseDone = ((oneshotPhase < sampleEnd) * (direction < 0)) > 0; // condition fulfilled if phase is above current sample end and direction is positive
				var loopPhaseStartTrig = (fwdOneshotPhaseDone + revOneshotPhaseDone) > 0;

				var oneshotSize = rightmostSamplePosExtent-leftmostSamplePosExtent;
				var loopOffset = loopPoint*oneshotSize; // loop point normalized to entire sample 0..1
				var loopSize = (1-loopPoint)*oneshotSize; // TODO: this should be fixed / latch for every initialized loop phase / run
				var absoluteLoopPoint = sampleStart + (loopOffset * direction); // TODO: this should be fixed / latch for every initialized loop phase / run

				var loopPhaseOnset = Latch.ar(oneshotPhase, loopPhaseStartTrig);
				var loopPhase = (oneshotPhase-loopPhaseOnset).wrap(0, loopSize * direction) + absoluteLoopPoint; // TODO
				// var loopPhase = oneshotPhase.wrap(sampleStart, sampleEnd);
		
				var sig = BufRd.ar(
					1, // TODO: Mono, refactor
					bufnum,
					Select.ar(loopPhaseStartTrig, [oneshotPhase, loopPhase]).linlin(0, 1, 0, BufFrames.kr(bufnum)),
					interpolation: 4
				); // TODO: tryout BLBufRd
		
				var killEnv = EnvGen.ar(Env.cutoff(0.01), gate, doneAction: Done.freeSelf);
				var volumeEnv = EnvGen.ar(Env.perc(volumeEnvAttack, volumeEnvRelease), doneAction: Done.freeSelf);
				var filterEnv = EnvGen.ar(Env.perc(filterEnvAttack, filterEnvRelease, filterEnvMod));
		
/*
	TODO: debugging
				loopPhaseStartTrig.poll(label: 'loopPhaseStartTrig');
				absoluteLoopPoint.poll(label: 'absoluteLoopPoint');
				loopPhaseOnset.poll(label: 'loopPhaseOnset');
				oneshotPhase.poll(label: 'oneshotPhase');
				loopPhase.poll(label: 'loopPhase');
				loopSize.poll(label: 'loopSize');
*/

				//FreeSelf.kr(); TODO: if release message is sent from Ack sclang logic to voice *group*, this might be a better option applicable to both oneshots phase done conditions and amp envelope. tho the cutoff envelope still would apply, for voice cutting / stealing

				sig = sig * (((fwdOneshotPhaseDone < 1) + (loopEnable > 0)) > 0); // basically: as long as direction is forward and phaseFromStart < sampleEnd or loopEnable == 1, continue playing (audition sound)
				sig = sig * (((revOneshotPhaseDone < 1) + (loopEnable > 0)) > 0); // basically: as long as direction is backward and phaseFromStart > sampleEnd or loopEnable == 1, continue playing (audition sound)

				sig = Decimator.ar(sig, sampleRate, bitDepth);
				sig = Select.ar(dist > 0, [sig, (sig * (1 + (dist * 10))).tanh.softclip]);

				sig = SVF.ar(
					sig,
					\widefreq.asSpec.map(\widefreq.asSpec.unmap(filterCutoff)+filterEnv), // TODO: use filterCutoffSpec
					filterRes,
					filterLowpassLevel,
					filterBandpassLevel,
					filterHighpassLevel,
					filterNotchLevel,
					filterPeakLevel
				);
				sig = Pan2.ar(sig, pan); // TODO: Mono, refactor
				sig = sig * volumeEnv * killEnv * volume.dbamp;
				Out.ar(out, sig);
				Out.ar(delayBus, sig*delaySend.dbamp);
				Out.ar(reverbBus, sig*reverbSend.dbamp);
			},
			// TODO: some of below \irs are temporary. sample start, end and loop point should be modulatable once audio glitches are fixed
			rates: [nil, nil, nil, nil, nil, \ir, \ir, \ir, \ir], // loopEnable is \ir
			// rates: [nil],
			metadata: (
				specs: (
					// gate: ControlSpec(0, 1, step: 1, default: 0),
					out: \audiobus,
					delayBus: \audiobus,
					reverbBus: \audiobus,
					bufnum: nil,
					sampleStart: channelSpecs[\sampleStart],
					sampleEnd: channelSpecs[\sampleEnd],
					loopEnable: channelSpecs[\loopEnable],
					loopPoint: channelSpecs[\loopPoint],
					speed: channelSpecs[\speed],
					volume: channelSpecs[\volume],
					volumeEnvAttack: channelSpecs[\volumeEnvAttack],
					volumeEnvRelease: channelSpecs[\volumeEnvRelease],
					pan: channelSpecs[\pan],
					filterCutoff: channelSpecs[\filterCutoff],
					filterRes: channelSpecs[\filterRes],
					filterEnvAttack: channelSpecs[\filterEnvAttack],
					filterEnvRelease: channelSpecs[\filterEnvRelease],
					filterEnvMod: channelSpecs[\filterEnvMod],
					sampleRate: channelSpecs[\sampleRate],
					bitDepth: channelSpecs[\bitDepth],
					dist: channelSpecs[\dist],
					delaySend: channelSpecs[\delaySend],
					reverbSend: channelSpecs[\reverbSend]
				)
			)
		).add;

		SynthDef(
			(this.stereoSamplePlayerDefName.asString).asSymbol,
			{
				|
				gate,
				out=0,
				delayBus,
				reverbBus,
				bufnum,
				sampleStart, // start point of playing back sample normalized to 0..1
				sampleEnd, // end point of playing back sample normalized to 0..1. sampleEnd prior to sampleStart will play sample reversed
				loopPoint, // loop point position between sampleStart and sampleEnd expressed in 0..1
				loopEnable, // loop enabled switch (1 = play looped, 0 = play oneshot). argument is initial rate so it cannot be changed after a synth starts to play
				speed,
				volume,
				volumeEnvAttack,
				volumeEnvRelease,
				pan,
				filterCutoff,
				filterRes,
				filterLowpassLevel,
				filterBandpassLevel,
				filterHighpassLevel,
				filterNotchLevel,
				filterPeakLevel,
				filterEnvAttack,
				filterEnvRelease,
				filterEnvMod,
				sampleRate,
				bitDepth,
				dist,
				delaySend,
				reverbSend
				|
				var direction = (sampleEnd-sampleStart).sign; // 1 = forward, -1 = backward
				var leftmostSamplePosExtent = min(sampleStart, sampleEnd);
				var rightmostSamplePosExtent = max(sampleStart, sampleEnd);
				var onset = Latch.ar(sampleStart, Impulse.ar(0)); // "fixes" onset to sample start at the time of spawning the synth, whereas sample end and *absolute* loop position (calculated from possibly modulating start and end positions) may vary
				var sweep = Sweep.ar(1, speed/BufDur.kr(bufnum)*direction); // sample duration normalized to 0..1 (sweeping 0..1 sweeps entire sample).
				var oneshotPhase = onset + sweep; // align phase to actual onset (fixed sample start at the time of spawning the synth)

				var fwdOneshotPhaseDone = ((oneshotPhase > sampleEnd) * (direction > (-1))) > 0; // condition fulfilled if phase is above current sample end and direction is positive
				var revOneshotPhaseDone = ((oneshotPhase < sampleEnd) * (direction < 0)) > 0; // condition fulfilled if phase is above current sample end and direction is positive
				var loopPhaseStartTrig = (fwdOneshotPhaseDone + revOneshotPhaseDone) > 0;

				var oneshotSize = rightmostSamplePosExtent-leftmostSamplePosExtent;
				var loopOffset = loopPoint*oneshotSize; // loop point normalized to entire sample 0..1
				var loopSize = (1-loopPoint)*oneshotSize; // TODO: this should be fixed / latch for every initialized loop phase / run
				var absoluteLoopPoint = sampleStart + (loopOffset * direction); // TODO: this should be fixed / latch for every initialized loop phase / run

				var loopPhaseOnset = Latch.ar(oneshotPhase, loopPhaseStartTrig);
				var loopPhase = (oneshotPhase-loopPhaseOnset).wrap(0, loopSize * direction) + absoluteLoopPoint; // TODO
				// var loopPhase = oneshotPhase.wrap(sampleStart, sampleEnd);
		
				var sig = BufRd.ar(
					2, // TODO: Stereo, refactor
					bufnum,
					Select.ar(loopPhaseStartTrig, [oneshotPhase, loopPhase]).linlin(0, 1, 0, BufFrames.kr(bufnum)),
					interpolation: 4
				); // TODO: tryout BLBufRd
		
				var killEnv = EnvGen.ar(Env.cutoff(0.01), gate, doneAction: Done.freeSelf); // TODO: move to volumeEnv and release with -1
				var volumeEnv = EnvGen.ar(Env.perc(volumeEnvAttack, volumeEnvRelease), doneAction: Done.freeSelf);
				var filterEnv = EnvGen.ar(Env.perc(filterEnvAttack, filterEnvRelease, filterEnvMod));
		
/*
	TODO: debugging
				loopPhaseStartTrig.poll(label: 'loopPhaseStartTrig');
				absoluteLoopPoint.poll(label: 'absoluteLoopPoint');
				loopPhaseOnset.poll(label: 'loopPhaseOnset');
				oneshotPhase.poll(label: 'oneshotPhase');
				loopPhase.poll(label: 'loopPhase');
				loopSize.poll(label: 'loopSize');
*/

				//FreeSelf.kr(); TODO: if release message is sent from Ack sclang logic to voice *group*, this might be a better option applicable to both oneshots phase done conditions and amp envelope. tho the cutoff envelope still would apply, for voice cutting / stealing

				sig = sig * (((fwdOneshotPhaseDone < 1) + (loopEnable > 0)) > 0); // basically: as long as direction is forward and phaseFromStart < sampleEnd or loopEnable == 1, continue playing (audition sound)
				sig = sig * (((revOneshotPhaseDone < 1) + (loopEnable > 0)) > 0); // basically: as long as direction is backward and phaseFromStart > sampleEnd or loopEnable == 1, continue playing (audition sound)

				sig = Decimator.ar(sig, sampleRate, bitDepth);
				sig = Select.ar(dist > 0, [sig, (sig * (1 + (dist * 10))).tanh.softclip]);

				sig = SVF.ar(
					sig,
					\widefreq.asSpec.map(\widefreq.asSpec.unmap(filterCutoff)+filterEnv), // TODO: use filterCutoffSpec
					filterRes,
					filterLowpassLevel,
					filterBandpassLevel,
					filterHighpassLevel,
					filterNotchLevel,
					filterPeakLevel
				);
				sig = Balance2.ar(sig[0], sig[1], pan); // TODO: Stereo, refactor
				sig = sig * volumeEnv * killEnv * volume.dbamp;
				Out.ar(out, sig);
				Out.ar(delayBus, sig*delaySend.dbamp);
				Out.ar(reverbBus, sig*reverbSend.dbamp);
			},
			// TODO: some of below \irs are temporary. sample start, end and loop point should be modulatable once audio glitches are fixed
			rates: [nil, nil, nil, nil, nil, \ir, \ir, \ir, \ir], // loopEnable is \ir
			// rates: [nil],
			metadata: (
				specs: (
					// gate: ControlSpec(0, 1, step: 1, default: 0),
					out: \audiobus,
					delayBus: \audiobus,
					reverbBus: \audiobus,
					bufnum: nil,
					sampleStart: channelSpecs[\sampleStart],
					sampleEnd: channelSpecs[\sampleEnd],
					loopEnable: channelSpecs[\loopEnable],
					loopPoint: channelSpecs[\loopPoint],
					speed: channelSpecs[\speed],
					volume: channelSpecs[\volume],
					volumeEnvAttack: channelSpecs[\volumeEnvAttack],
					volumeEnvRelease: channelSpecs[\volumeEnvRelease],
					pan: channelSpecs[\pan],
					filterCutoff: channelSpecs[\filterCutoff],
					filterRes: channelSpecs[\filterRes],
					filterEnvAttack: channelSpecs[\filterEnvAttack],
					filterEnvRelease: channelSpecs[\filterEnvRelease],
					filterEnvMod: channelSpecs[\filterEnvMod],
					sampleRate: channelSpecs[\sampleRate],
					bitDepth: channelSpecs[\bitDepth],
					dist: channelSpecs[\dist],
					delaySend: channelSpecs[\delaySend],
					reverbSend: channelSpecs[\reverbSend]
				)
			)
		).add;

		SynthDef(
			this.delayDefName,
			{ |in, out, delayTime, feedback, level|
				var sig = In.ar(in, 2);
				var sigfeedback = LocalIn.ar(2);
				sig = DelayC.ar(sig + sigfeedback, maxdelaytime: delayTimeSpec.maxval, delaytime: delayTime); // TODO: - ControlDur.ir
				LocalOut.ar(sig * feedback);
				Out.ar(out, sig * level.dbamp);
			},
			rates: [nil, nil, 0.2, 0.2],
			metadata: (
				specs: (
					in: \audiobus,
					out: \audiobus,
					delayTime: delayTimeSpec,
					feedback: delayFeedbackSpec,
					level: delayLevelSpec
				)
			)
		).add;

		SynthDef(
			this.reverbDefName,
			{ |in, out, room, damp, level|
				var sig = In.ar(in, 2);
				sig = FreeVerb.ar(sig, 1, room, damp);
				Out.ar(out, sig * level.dbamp);
			},
			metadata: (
				specs: (
					out: \audiobus,
					room: reverbRoomSpec,
					damp: reverbDampSpec,
					level: reverbLevelSpec
				)
			)
		).add;

		SynthDef(
			this.mainLevelDefName,
			{ |in, out, level|
				var sig = In.ar(in, 2);
				Out.ar(out, sig * level.dbamp);
			},
			rates: [nil, nil, 0.1],
			metadata: (
				specs: (
					in: \audiobus,
					out: \audiobus,
					level: mainLevelSpec
				)
			)
		).add;

		sourceGroup = Group.tail(context.xg);
		context.server.sync; // TODO: not sure this is needed?
		channelGroups = numChannels collect: { Group.tail(sourceGroup) };
		channelControlBusses = numChannels collect: {
			#[
				sampleStart,
				sampleEnd,
				loopPoint,
				loopEnable,
				speed,
				volume,
				volumeEnvAttack,
				volumeEnvRelease,
				pan,
				filterCutoff,
				filterRes,
				filterEnvAttack,
				filterEnvRelease,
				filterEnvMod,
				sampleRate,
				bitDepth,
				dist,
				delaySend,
				reverbSend,
				filterLowpassLevel,
				filterBandpassLevel,
				filterHighpassLevel,
				filterNotchLevel,
				filterPeakLevel
			].collect { |sym|
				var bus = Bus.control;

				if (debug) {
					postln("channelControlBus for %".format(sym));
				};

				bus.set(channelSpecs[sym].default);

				if (debug) {
					postln("- set to default %".format(channelSpecs[sym].default));
					postln("");
				};

				sym -> bus
			}.asDict
		};
		effectsGroup = Group.tail(context.xg);

		// TODO: weirdness buffers = numChannels collect: { Buffer.new };
		buffers = numChannels collect: { Buffer.alloc(numFrames: 1) };

		delayBus = Bus.audio(numChannels: 2);
		reverbBus = Bus.audio(numChannels: 2);
		mainLevelBus = Bus.audio(numChannels: 2);

		context.server.sync;

		delaySynth = Synth(this.delayDefName, [\out, mainLevelBus, \in, delayBus], target: effectsGroup);
		reverbSynth = Synth(this.reverbDefName, [\out, mainLevelBus, \in, reverbBus], target: effectsGroup);

		mainLevelGroup = Group.tail(context.xg);
		mainLevelSynth = Synth(this.mainLevelDefName, [\out, context.out_b, \in, mainLevelBus], target: mainLevelGroup);

		samplePlayerSynths = Array.fill(numChannels);
		muteGroups = Array.fill(numChannels, false);

		context.server.sync;

		// TODO: validate channelnum and provide better error reporting if invalid channelnum is sent
		this.addCommand(\loadSample, "is") { |msg| this.cmdLoadSample(msg[1], msg[2]) };
		this.addCommand(\multiTrig, "iiiiiiii") { |msg| this.cmdMultiTrig(msg[1], msg[2], msg[3], msg[4], msg[5], msg[6], msg[7], msg[8]) };
		this.addCommand(\trig, "i") { |msg| this.cmdTrig(msg[1]) };
		this.addCommand(\multiKill, "iiiiiiii") { |msg| this.cmdMultiKill(msg[1], msg[2], msg[3], msg[4], msg[5], msg[6], msg[7], msg[8]) };
		this.addCommand(\kill, "i") { |msg| this.cmdKill(msg[1]) };
		this.addCommand(\includeInMuteGroup, "ii") { |msg| this.cmdIncludeInMuteGroup(msg[1], msg[2]) };
		this.addCommand(\sampleStart, "if") { |msg| this.cmdSampleStart(msg[1], msg[2]) };
		this.addCommand(\sampleEnd, "if") { |msg| this.cmdSampleEnd(msg[1], msg[2]) };
		this.addCommand(\loopPoint, "if") { |msg| this.cmdLoopPoint(msg[1], msg[2]) };
		this.addCommand(\enableLoop, "i") { |msg| this.cmdEnableLoop(msg[1]) };
		this.addCommand(\disableLoop, "i") { |msg| this.cmdDisableLoop(msg[1]) };
		this.addCommand(\speed, "if") { |msg| this.cmdSpeed(msg[1], msg[2]) };
		this.addCommand(\volume, "if") { |msg| this.cmdVolume(msg[1], msg[2]) };
		this.addCommand(\volumeEnvAttack, "if") { |msg| this.cmdVolumeEnvAttack(msg[1], msg[2]) };
		this.addCommand(\volumeEnvRelease, "if") { |msg| this.cmdVolumeEnvRelease(msg[1], msg[2]) };
		this.addCommand(\pan, "if") { |msg| this.cmdPan(msg[1], msg[2]) };
		this.addCommand(\filterCutoff, "if") { |msg| this.cmdFilterCutoff(msg[1], msg[2]) };
		this.addCommand(\filterRes, "if") { |msg| this.cmdFilterRes(msg[1], msg[2]) };
		this.addCommand(\filterMode, "ii") { |msg| this.cmdFilterMode(msg[1], msg[2]) };
		this.addCommand(\filterEnvAttack, "if") { |msg| this.cmdFilterEnvAttack(msg[1], msg[2]) };
		this.addCommand(\filterEnvRelease, "if") { |msg| this.cmdFilterEnvRelease(msg[1], msg[2]) };
		this.addCommand(\filterEnvMod, "if") { |msg| this.cmdFilterEnvMod(msg[1], msg[2]) };
		this.addCommand(\sampleRate, "if"){ |msg| this.cmdSampleRate(msg[1], msg[2]) };
		this.addCommand(\bitDepth, "if"){ |msg| this.cmdBitDepth(msg[1], msg[2]) };
		this.addCommand(\dist, "if") { |msg| this.cmdDist(msg[1], msg[2]) };
		this.addCommand(\delaySend, "if") { |msg| this.cmdDelaySend(msg[1], msg[2]) };
		this.addCommand(\reverbSend, "if") { |msg| this.cmdReverbSend(msg[1], msg[2]) };

		this.addCommand(\delayTime, "f") { |msg| this.cmdDelayTime(msg[1]) };
		this.addCommand(\delayFeedback, "f") { |msg| this.cmdDelayFeedback(msg[1]) };
		this.addCommand(\delayLevel, "f") { |msg| this.cmdDelayLevel(msg[1]) };
		this.addCommand(\reverbRoom, "f") { |msg| this.cmdReverbRoom(msg[1]) };
		this.addCommand(\reverbDamp, "f") { |msg| this.cmdReverbDamp(msg[1]) };
		this.addCommand(\reverbLevel, "f") { |msg| this.cmdReverbLevel(msg[1]) };

		this.addCommand(\mainLevel, "f") { |msg| this.cmdMainLevel(msg[1]) };
	}

	cmdLoadSample { |channelnum, path|
		this.loadSample(channelnum, path.asString);
	}

	cmdMultiTrig { |...channels|
		context.server.makeBundle(nil) {
			channels.do { |trig, channelnum|
				if (trig.booleanValue) {this.cmdTrig(channelnum) };
			};
		};
	}

	cmdTrig { |channelnum|
		if (this.sampleIsLoaded(channelnum)) {
			var samplePlayerSynthArgs = [
				\gate, 1,
				\out, mainLevelBus,
				\delayBus, delayBus,
				\reverbBus, reverbBus,
				\bufnum, buffers[channelnum]
			];
			channelControlBusses[channelnum] keysValuesDo: { |key, value|
				samplePlayerSynthArgs = samplePlayerSynthArgs.addAll(
					[key, value.asMap]
				)
			};

			if (this.includedInMuteGroup(channelnum)) {
				this.killMuteGroup;
			} {
				this.killChannel(channelnum);
			};

			samplePlayerSynths[channelnum] = Synth.new(
				(if (this.sampleIsStereo(channelnum), this.stereoSamplePlayerDefName, this.monoSamplePlayerDefName).asString).asSymbol,
				args: samplePlayerSynthArgs,
				target: channelGroups[channelnum]
			);
		};
	}

	cmdMultiKill { |...channels|
		context.server.makeBundle(nil) {
			channels.do { |trig, channelnum|
				if (trig.booleanValue) { this.cmdKill(channelnum) };
			};
		};
	}

	cmdKill { |channelnum|
		if (this.sampleIsLoaded(channelnum)) {
			this.killChannel(channelnum);
		}
	}

	cmdIncludeInMuteGroup { |channelnum, bool|
		muteGroups[channelnum] = bool.asBoolean;
	}

	cmdSampleStart { |channelnum, f|
		channelControlBusses[channelnum][\sampleStart].set(channelSpecs[\sampleStart].constrain(f));
	}

	cmdSampleEnd { |channelnum, f|
		channelControlBusses[channelnum][\sampleEnd].set(channelSpecs[\sampleEnd].constrain(f));
	}

	cmdLoopPoint { |channelnum, f|
		channelControlBusses[channelnum][\loopPoint].set(channelSpecs[\loopPoint].constrain(f));
	}

	cmdEnableLoop { |channelnum|
		channelControlBusses[channelnum][\loopEnable].set(1);
	}

	cmdDisableLoop { |channelnum|
		channelControlBusses[channelnum][\loopEnable].set(0);
	}

	cmdSpeed { |channelnum, f|
		channelControlBusses[channelnum][\speed].set(channelSpecs[\speed].constrain(f));
	}

	cmdVolume { |channelnum, f|
		channelControlBusses[channelnum][\volume].set(channelSpecs[\volume].constrain(f));
	}

	cmdVolumeEnvAttack { |channelnum, f|
		channelControlBusses[channelnum][\volumeEnvAttack].set(channelSpecs[\volumeEnvAttack].constrain(f));
	}

	cmdVolumeEnvRelease { |channelnum, f|
		channelControlBusses[channelnum][\volumeEnvRelease].set(channelSpecs[\volumeEnvRelease].constrain(f));
	}

	cmdPan { |channelnum, f|
		channelControlBusses[channelnum][\pan].set(channelSpecs[\pan].constrain(f));
	}

	cmdFilterCutoff { |channelnum, f|
		channelControlBusses[channelnum][\filterCutoff].set(channelSpecs[\filterCutoff].constrain(f));
	}

	cmdFilterRes { |channelnum, f|
		channelControlBusses[channelnum][\filterRes].set(channelSpecs[\filterRes].constrain(f));
	}

	cmdFilterMode { |channelnum, i|
		var busses = channelControlBusses[channelnum];
		switch (i)
			{ 0 } {
				busses[\filterLowpassLevel].set(1);
				busses[\filterBandpassLevel].set(0);
				busses[\filterHighpassLevel].set(0);
				busses[\filterNotchLevel].set(0);
				busses[\filterPeakLevel].set(0);
			}
			{ 1 } {
				busses[\filterLowpassLevel].set(0);
				busses[\filterBandpassLevel].set(1);
				busses[\filterHighpassLevel].set(0);
				busses[\filterNotchLevel].set(0);
				busses[\filterPeakLevel].set(0);
			}
			{ 2 } {
				busses[\filterLowpassLevel].set(0);
				busses[\filterBandpassLevel].set(0);
				busses[\filterHighpassLevel].set(1);
				busses[\filterNotchLevel].set(0);
				busses[\filterPeakLevel].set(0);
			}
			{ 3 } {
				busses[\filterLowpassLevel].set(0);
				busses[\filterBandpassLevel].set(0);
				busses[\filterHighpassLevel].set(0);
				busses[\filterNotchLevel].set(1);
				busses[\filterPeakLevel].set(0);
			}
			{ 4 } {
				busses[\filterLowpassLevel].set(0);
				busses[\filterBandpassLevel].set(0);
				busses[\filterHighpassLevel].set(0);
				busses[\filterNotchLevel].set(0);
				busses[\filterPeakLevel].set(1);
			}
	}

	cmdFilterEnvAttack { |channelnum, f|
		channelControlBusses[channelnum][\filterEnvAttack].set(channelSpecs[\filterEnvAttack].constrain(f));
	}

	cmdFilterEnvRelease { |channelnum, f|
		channelControlBusses[channelnum][\filterEnvRelease].set(channelSpecs[\filterEnvRelease].constrain(f));
	}

	cmdFilterEnvMod { |channelnum, f|
		channelControlBusses[channelnum][\filterEnvMod].set(channelSpecs[\filterEnvMod].constrain(f));
	}

	cmdSampleRate { |channelnum, f|
		channelControlBusses[channelnum][\sampleRate].set(channelSpecs[\sampleRate].constrain(f));
	}

	cmdBitDepth { |channelnum, f|
		channelControlBusses[channelnum][\bitDepth].set(channelSpecs[\bitDepth].constrain(f));
	}

	cmdDist { |channelnum, f|
		channelControlBusses[channelnum][\dist].set(channelSpecs[\dist].constrain(f));
	}

	cmdDelaySend { |channelnum, f|
		channelControlBusses[channelnum][\delaySend].set(channelSpecs[\delaySend].constrain(f));
	}

	cmdReverbSend { |channelnum, f|
		channelControlBusses[channelnum][\reverbSend].set(channelSpecs[\reverbSend].constrain(f));
	}

	cmdDelayTime { |f|
		delaySynth.set(\delayTime, delayTimeSpec.constrain(f));
	}

	cmdDelayFeedback { |f|
		delaySynth.set(\feedback, delayFeedbackSpec.constrain(f));
	}

	cmdDelayLevel { |f|
		delaySynth.set(\level, delayLevelSpec.constrain(f));
	}

	cmdReverbRoom { |f|
		reverbSynth.set(\room, reverbRoomSpec.constrain(f));
	}

	cmdReverbDamp { |f|
		reverbSynth.set(\damp, reverbDampSpec.constrain(f));
	}

	cmdReverbLevel { |f|
		reverbSynth.set(\level, reverbLevelSpec.constrain(f));
	}

	cmdMainLevel { |f|
		mainLevelSynth.set(\level, mainLevelSpec.constrain(f));
	}

	killChannel { |channelnum|
		channelGroups[channelnum].set(\gate, 0);
	}

	killMuteGroup {
		muteGroups do: { |included, channelnum|
			if (included) {
				channelGroups[channelnum].set(\gate, 0);
			}
		};
	}

	includedInMuteGroup { |channelnum|
		^muteGroups[channelnum]
	}

	free {
		if (useScdBasedAck) {
			this.scdBasedFree;
		} {
			this.scBasedFree;
		}
	}

	scBasedFree {
		samplePlayerSynths do: _.free;
		channelGroups do: _.free;
		channelControlBusses do: { |dict| dict do: _.free };
		buffers do: _.free;

		effectsGroup.free;
		delayBus.free;
		reverbBus.free;
		delaySynth.free;
		reverbSynth.free;

		mainLevelBus.free;
		mainLevelGroup.free;
		mainLevelSynth.free;
	}

	sampleIsLoaded { |channelnum| ^buffers[channelnum].path.notNil }

	sampleIsStereo { |channelnum| ^buffers[channelnum].numChannels == 2 }

	sampleHasLoopEnabled { |channelnum| ^loopEnabled[channelnum] }

	loadSample { |channelnum, path|
		if (channelnum >= 0 and: channelnum < numChannels) {
			var numChannels, soundFile = SoundFile.openRead(path);
			if (soundFile.notNil) {
				numChannels = soundFile.numChannels;
				soundFile.close;
				if (numChannels < 3) {
					var buffer = buffers[channelnum];
					// TODO: stop any current sample playing for channelnum, to omit the 'Buffer UGen channel mismatch: expected 1, yet buffer has 2 channels' and better cleanup before loading the new sample
					fork {
						this.killChannel(channelnum);
						context.server.sync;
						buffer.allocRead(path);
						context.server.sync;
						buffer.updateInfo(path);
						context.server.sync;
						"sample % loaded into channel %"
							.format(path.quote, channelnum).inform;
					};
				} {
					"Only mono and stereo samples are supported, % has % channels"
						.format(path.quote, numChannels).error;
				};
			} {
				"Unable to open file %"
					.format(path.quote).error;
			};
		} {
			"Invalid argument (%) to loadSample, channelnum must be between 0 and %"
				.format(channelnum, numChannels-1).error;
		};
	}

	// new scd based engine logic below. sc based one (above) to be phased out.

	getScdBasedEngine {
		var scdFilePath = (PathName(this.class.filenameSymbol.asString).pathOnly +/+ "ack.scd").standardizePath;

		^thisProcess.interpreter.executeFile(scdFilePath);
	}

	scdBasedAlloc {
		var scdAPI = this.getScdBasedEngine;
		init = scdAPI[\init];
		free = scdAPI[\free];
		loadSampleCommand = scdAPI[\loadSampleCommand];
		multiTrigCommand = scdAPI[\multiTrigCommand];
		trigCommand = scdAPI[\trigCommand];
		multiKillCommand = scdAPI[\multiKillCommand];
		killCommand = scdAPI[\killCommand];
		includeInMuteGroupCommand = scdAPI[\includeInMuteGroupCommand];
		sampleStartCommand = scdAPI[\sampleStartCommand];
		sampleEndCommand = scdAPI[\sampleEndCommand];
		loopPointCommand = scdAPI[\loopPointCommand];
		enableLoopCommand = scdAPI[\enableLoopCommand];
		disableLoopCommand = scdAPI[\disableLoopCommand];
		speedCommand = scdAPI[\speedCommand];
		volumeCommand = scdAPI[\volumeCommand];
		volumeEnvAttackCommand = scdAPI[\volumeEnvAttackCommand];
		volumeEnvReleaseCommand = scdAPI[\volumeEnvReleaseCommand];
		panCommand = scdAPI[\panCommand];
		filterCutoffCommand = scdAPI[\filterCutoffCommand];
		filterResCommand = scdAPI[\filterResCommand];
		filterModeCommand = scdAPI[\filterModeCommand];
		filterEnvAttackCommand = scdAPI[\filterEnvAttackCommand];
		filterEnvReleaseCommand = scdAPI[\filterEnvReleaseCommand];
		filterEnvModCommand = scdAPI[\filterEnvModCommand];
		distCommand = scdAPI[\distCommand];
		sampleRateCommand = scdAPI[\sampleRateCommand];
		bitDepthCommand = scdAPI[\bitDepthCommand];
		delaySendCommand = scdAPI[\delaySendCommand];
		reverbSendCommand = scdAPI[\reverbSendCommand];

		delayTimeCommand = scdAPI[\delayTimeCommand];
		delayFeedbackCommand = scdAPI[\delayFeedbackCommand];
		delayLevelCommand = scdAPI[\delayLevelCommand];
		reverbRoomCommand = scdAPI[\reverbRoomCommand];
		reverbDampCommand = scdAPI[\reverbDampCommand];
		reverbLevelCommand = scdAPI[\reverbLevelCommand];

		mainLevelCommand = scdAPI[\mainLevelCommand];

		scdBasedAckInstance=init.(
			(
				trace: false,
				group: context.xg,
				outBus: context.out_b
			)
		);

		this.addCommands;
	}

	addCommands {
		this.addCommand(\loadSample, "is") { |msg| loadSampleCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\multiTrig, "iiiiiiii") { |msg| multiTrigCommand.value(scdBasedAckInstance, msg[1], msg[2], msg[3], msg[4], msg[5], msg[6], msg[7], msg[8]) };
		this.addCommand(\trig, "i") { |msg| trigCommand.value(scdBasedAckInstance, msg[1]) };
		this.addCommand(\multiKill, "iiiiiiii") { |msg| multiKillCommand.value(scdBasedAckInstance, msg[1], msg[2], msg[3], msg[4], msg[5], msg[6], msg[7], msg[8]) };
		this.addCommand(\kill, "i") { |msg| killCommand.value(scdBasedAckInstance, msg[1]) };
		this.addCommand(\includeInMuteGroup, "ii") { |msg| includeInMuteGroupCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\sampleStart, "if") { |msg| sampleStartCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\sampleEnd, "if") { |msg| sampleEndCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\loopPoint, "if") { |msg| loopPointCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\enableLoop, "i") { |msg| enableLoopCommand.value(scdBasedAckInstance, msg[1]) };
		this.addCommand(\disableLoop, "i") { |msg| disableLoopCommand.value(scdBasedAckInstance, msg[1]) };
		this.addCommand(\speed, "if") { |msg| speedCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\volume, "if") { |msg| volumeCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\volumeEnvAttack, "if") { |msg| volumeEnvAttackCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\volumeEnvRelease, "if") { |msg| volumeEnvReleaseCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\pan, "if") { |msg| panCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\filterCutoff, "if") { |msg| filterCutoffCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\filterRes, "if") { |msg| filterResCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\filterMode, "ii") { |msg| filterModeCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\filterEnvAttack, "if") { |msg| filterEnvAttackCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\filterEnvRelease, "if") { |msg| filterEnvReleaseCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\filterEnvMod, "if") { |msg| filterEnvModCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\dist, "if") { |msg| distCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\bitDepth, "if") { |msg| bitDepthCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\sampleRate, "if") { |msg| sampleRateCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\delaySend, "if") { |msg| delaySendCommand.value(scdBasedAckInstance, msg[1], msg[2]) };
		this.addCommand(\reverbSend, "if") { |msg| reverbSendCommand.value(scdBasedAckInstance, msg[1], msg[2]) };

		this.addCommand(\delayTime, "f") { |msg| delayTimeCommand.value(scdBasedAckInstance, msg[1]) };
		this.addCommand(\delayFeedback, "f") { |msg| delayFeedbackCommand.value(scdBasedAckInstance, msg[1]) };
		this.addCommand(\delayLevel, "f") { |msg| delayLevelCommand.value(scdBasedAckInstance, msg[1]) };
		this.addCommand(\reverbRoom, "f") { |msg| reverbRoomCommand.value(scdBasedAckInstance, msg[1]) };
		this.addCommand(\reverbDamp, "f") { |msg| reverbDampCommand.value(scdBasedAckInstance, msg[1]) };
		this.addCommand(\reverbLevel, "f") { |msg| reverbLevelCommand.value(scdBasedAckInstance, msg[1]) };

		this.addCommand(\mainLevel, "f") { |msg| mainLevelCommand.value(scdBasedAckInstance, msg[1]) };
	 }

	scdBasedFree {
		free.(scdBasedAckInstance);
	}
}
