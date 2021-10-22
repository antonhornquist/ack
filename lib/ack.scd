(
var numChannels = 8;

var init = { |options|
	var instance = ();
	var autostartServer, trace, group, outBus;

	# autostartServer, trace, group, outBus = parseOptions.value(options ? ());

	"Ack is initializing... ".postln;

	instance[\trace] = trace;
	instance[\group] = group;
	instance[\outBus] = outBus;

	instance[\loopEnabled] = Array.fill(8) { false };
	instance[\muteGroups] = Array.fill(numChannels, false);
	instance[\samplePlayerSynths] = Array.fill(numChannels);

	if (Server.default.serverRunning) {
		{
			initServerResources.value(instance);
			"Ack initialized successfully.".postln;
		}.forkIfNeeded;
	} {
		if (autostartServer) {
			postAutobootingServerInformation.value;
			Server.default.waitForBoot {
				initServerResources.value(instance);
				"Ack initialized successfully.".postln;
			};
		} {
			"Ack initialization failed.".postln;
			postServerNotRunningError.value;
		};
	};

	instance;
};

var parseOptions = { |options|
	[
		options[\autostartServer] ? true,
		options[\trace] ? false,
		options[\group] ? Server.default.defaultGroup,
		options[\outBus] ? 0,
	]
};

var initServerResources = { |instance|
	var mainLevelBus = Bus.audio(numChannels: 2);
	instance[\mainLevelBus] = mainLevelBus;
	sendSynthDefsToServer.value(instance);
	initSamplePlayerServerResources.value(instance);
	initEffectsServerResources.value(instance);
	initMainLevelServerResources.value(instance);
	Server.default.sync;
};

var sendSynthDefsToServer = { |instance|
	sendSamplePlayerSynthDefsToServer.value(instance);
	sendEffectSynthDefsToServer.value(instance);
	sendMainLevelSynthDefToServer.value(instance);
};

var sendSamplePlayerSynthDefsToServer = { |instance|
	// TODO: some of below \irs are temporary. sample start, end and loop point should be modulatable once audio glitches are fixed
	var rates = [
		nil, // gate
		nil, // out
		nil, // delayBus
		nil, // reverbBus
		nil, // bufnum
		\ir, // sampleStart
		\ir, // sampleEnd
		\ir, // loopPoint
		\ir // loopEnable
	];

	var metadata = (
		specs: (
			// gate: ControlSpec(0, 1, step: 1, default: 0), TODO
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
	);

	SynthDef(
		monoSamplePlayerDefName,
		spawnUgenGraphFunc.value { |bufnum, loopPhaseStartTrig, oneshotPhase, loopPhase, pan|
			var sig = BufRd.ar(
				1,
				bufnum,
				Select.ar(loopPhaseStartTrig, [oneshotPhase, loopPhase]).linlin(0, 1, 0, BufFrames.kr(bufnum)),
				interpolation: 4
			);
			Pan2.ar(sig, pan);
		},
		rates: rates,
		metadata: metadata
	).add;

	SynthDef(
		stereoSamplePlayerDefName,
		spawnUgenGraphFunc.value { |bufnum, loopPhaseStartTrig, oneshotPhase, loopPhase, pan|
			var sig = BufRd.ar(
				2,
				bufnum,
				Select.ar(loopPhaseStartTrig, [oneshotPhase, loopPhase]).linlin(0, 1, 0, BufFrames.kr(bufnum)),
				interpolation: 4
			);
			Balance2.ar(sig[0], sig[1], pan);
		},
		rates: rates,
		metadata: metadata
	).add;
};

var sendEffectSynthDefsToServer = { |instance|
	if (instance[\trace]) {
		"sending SynthDef %...".format(delayDefName.asString.quote).inform;
	};

	SynthDef(
		delayDefName,
		{ |in, out, delayTime, feedback, level|
			var sig = In.ar(in, 2);
			var sigfeedback = LocalIn.ar(2);
			sig = DelayC.ar(sig + sigfeedback, maxdelaytime: effectSpecs[\delayTime].maxval, delaytime: delayTime); // TODO: - ControlDur.ir
			LocalOut.ar(sig * feedback);
			Out.ar(out, sig * level.dbamp);
		},
		rates: [nil, nil, 0.2, 0.2],
		metadata: (
			specs: (
				in: \audiobus,
				out: \audiobus,
				delayTime: effectSpecs[\delayTime],
				delayFeedback: effectSpecs[\delayFeedback],
				delayLevel: effectSpecs[\delayLevel]
			)
		)
	).add;

	if (instance[\trace]) {
		"...OK.".inform;
		"".inform;
		"sending SynthDef %...".format(reverbDefName.asString.quote).inform;
	};

	SynthDef(
		reverbDefName,
		{ |in, out, room, damp, level|
			var sig = In.ar(in, 2);
			sig = FreeVerb.ar(sig, 1, room, damp);
			Out.ar(out, sig * level.dbamp);
		},
		metadata: (
			specs: ( // TODO: add in: \audiobus ??
				out: \audiobus,
				reverbRoom: effectSpecs[\reverbRoom], // TODO: does not match with ugenGraphFunc arguments!?
				reverbDamp: effectSpecs[\reverbDamp],
				reverbLevel: effectSpecs[\reverbLevel]
			)
		)
	).add;

	if (instance[\trace]) {
		"...OK.".inform;
		"".inform;
	};

};

var sendMainLevelSynthDefToServer = { |instance|
	if (instance[\trace]) {
		"sending SynthDef %...".format(mainLevelDefName.asString.quote).inform;
	};

	SynthDef(
		mainLevelDefName,
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

	if (instance[\trace]) {
		"...OK.".inform;
	};
};

var spawnUgenGraphFunc = { |bufrdPanFunc|
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

		var sig = SynthDef.wrap(
			func: bufrdPanFunc,
			rates: [nil, nil, nil, nil, nil],
			prependArgs: [bufnum, loopPhaseStartTrig, oneshotPhase, loopPhase, pan]
		);

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
		sig = sig * volumeEnv * killEnv * volume.dbamp;
		Out.ar(out, sig);
		Out.ar(delayBus, sig*delaySend.dbamp);
		Out.ar(reverbBus, sig*reverbSend.dbamp);
	}
};

var initSamplePlayerServerResources = { |instance|
	instance[\sourceGroup] = Group.tail(instance[\group]);
	Server.default.sync; // TODO: not sure this is needed?
	instance[\channelGroups] = numChannels collect: { Group.tail(instance[\sourceGroup]) };
	instance[\channelControlBusses] = numChannels collect: {
		channelSpecs.collect { |controlSpec, id|
			var bus = Bus.control;

			if (instance[\trace]) {
				postln("channelControlBus for %".format(id));
			};

			bus.set(controlSpec.default);

			if (instance[\trace]) {
				postln("- set to default %".format(controlSpec.default));
				postln("");
			};

			bus
		};
	};
	// TODO: weirdness buffers = numChannels collect: { Buffer.new };
	instance[\buffers] = numChannels collect: { Buffer.alloc(numFrames: 1) };
};

var initEffectsServerResources = { |instance|
	var effectsGroup = Group.tail(instance[\group]);
	var delayBus = Bus.audio(numChannels: 2);
	var reverbBus = Bus.audio(numChannels: 2);
	Server.default.sync;
	instance[\delaySynth] = Synth(delayDefName, [\out, instance[\mainLevelBus], \in, delayBus], target: effectsGroup);
	instance[\reverbSynth] = Synth(reverbDefName, [\out, instance[\mainLevelBus], \in, reverbBus], target: effectsGroup);
	instance[\delayBus] = delayBus;
	instance[\reverbBus] = reverbBus;
	instance[\effectsGroup] = effectsGroup;
};

var initMainLevelServerResources = { |instance|
	var mainLevelGroup = Group.tail(instance[\group]);
	instance[\mainLevelGroup] = mainLevelGroup;
	Server.default.sync;
	instance[\mainLevelSynth] = Synth(mainLevelDefName, [\out, instance[\outBus], \in, instance[\mainLevelBus]], target: mainLevelGroup);
};

var postAutobootingServerInformation = {
	"Booting Server %".format(Server.default).inform;
};

var postServerNotRunningError = { 
	"Server % is not running".format(Server.default).error;
};

var free = { |self, freeServerNodes=true| // TODO: on CmdPeriod make so that freeServerNodes can be set to false and no server errors will be thrown. does this work as expected?
	"Ack is freed... ".postln;

	if (freeServerNodes) {
		self[\samplePlayerSynths] do: _.free;
		self[\channelGroups] do: _.free;
		self[\effectsGroup].free;
		self[\mainLevelGroup].free;
		self[\delaySynth].free;
		self[\reverbSynth].free;
		self[\mainLevelSynth].free;
	};

	self[\channelControlBusses] do: { |dict| dict do: _.free };
	self[\delayBus].free;
	self[\reverbBus].free;
	self[\mainLevelBus].free;
	self[\buffers] do: _.free;

	"Ack freed successfully.".postln;
};

var loadSampleCommand = { |self, channelnum, path|
	loadSample.value(self, channelnum, path.asString);
};

var multiTrigCommand = { |self ... channels|
	Server.default.makeBundle(nil) {
		channels.do { |trig, channelnum|
			if (trig.booleanValue) { trigCommand.value(self, channelnum) };
		};
	};
};

var trigCommand = { |self, channelnum|
	if (sampleIsLoaded.value(self, channelnum)) {
		var samplePlayerSynthArgs = [
			\gate, 1,
			\out, self[\mainLevelBus],
			\delayBus, self[\delayBus],
			\reverbBus, self[\reverbBus],
			\bufnum, self[\buffers][channelnum]
		];
		self[\channelControlBusses][channelnum] keysValuesDo: { |key, value|
			samplePlayerSynthArgs = samplePlayerSynthArgs.addAll(
				[key, value.asMap]
			)
		};

		if (includedInMuteGroup.value(self, channelnum)) {
			killMuteGroup.value(self);
		} {
			killChannel.value(self, channelnum);
		};

		self[\samplePlayerSynths][channelnum] = Synth.new(
			if (sampleIsStereo.value(self, channelnum), stereoSamplePlayerDefName, monoSamplePlayerDefName),
			args: samplePlayerSynthArgs,
			target: self[\channelGroups][channelnum]
		);
	};
};

var multiKillCommand = { |self ... channels|
	Server.default.makeBundle(nil) {
		self[\channels].do { |trig, channelnum|
			if (trig.booleanValue) { killCommand.value(self, channelnum) };
		};
	};
};

var killCommand = { |self, channelnum|
	if (sampleIsLoaded.value(self, channelnum)) {
		killChannel.value(self, channelnum);
	}
};

var includeInMuteGroupCommand = { |self, channelnum, bool|
	self[\muteGroups][channelnum] = bool.asBoolean;
};

var sampleStartCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \sampleStart, channelnum, value);
};

var sampleEndCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \sampleEnd, channelnum, value);
};

var loopPointCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \loopPoint, channelnum, value);
};

var enableLoopCommand = { |self, channelnum|
	bussedChannelCommand.value(self, \loopEnable, channelnum, 1);
};

var disableLoopCommand = { |self, channelnum|
	bussedChannelCommand.value(self, \loopEnable, channelnum, 0);
};

var speedCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \speed, channelnum, value);
};

var volumeCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \volume, channelnum, value);
};

var volumeEnvAttackCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \volumeEnvAttack, channelnum, value);
};

var volumeEnvReleaseCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \volumeEnvRelease, channelnum, value);
};

var panCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \pan, channelnum, value);
};

var filterCutoffCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \filterCutoff, channelnum, value);
};

var filterResCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \filterRes, channelnum, value);
};

var filterModeCommand = { |self, channelnum, integer|
	bussedChannelCommand.value(self, \filterLowpassLevel, channelnum, (integer == 0).asInteger);
	bussedChannelCommand.value(self, \filterBandpassLevel, channelnum, (integer == 1).asInteger);
	bussedChannelCommand.value(self, \filterHighpassLevel, channelnum, (integer == 2).asInteger);
	bussedChannelCommand.value(self, \filterNotchLevel, channelnum, (integer == 3).asInteger);
	bussedChannelCommand.value(self, \filterPeakLevel, channelnum, (integer == 4).asInteger);
};

var filterEnvAttackCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \filterEnvAttack, channelnum, value);
};

var filterEnvReleaseCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \filterEnvRelease, channelnum, value);
};

var filterEnvModCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \filterEnvMod, channelnum, value);
};

var sampleRateCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \sampleRate, channelnum, value);
};

var bitDepthCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \bitDepth, channelnum, value);
};

var distCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \dist, channelnum, value);
};

var delaySendCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \delaySend, channelnum, value);
};

var reverbSendCommand = { |self, channelnum, value|
	bussedChannelCommand.value(self, \reverbSend, channelnum, value);
};

var delayTimeCommand = { |self, value|
	effectCommand.value(self, \delayTime, value);
};

var delayFeedbackCommand = { |self, value|
	effectCommand.value(self, \delayFeedback, value);
};

var delayLevelCommand = { |self, value|
	effectCommand.value(self, \delayLevel, value);
};

var reverbRoomCommand = { |self, value|
	effectCommand.value(self, \reverbRoom, value);
};

var reverbDampCommand = { |self, value|
	effectCommand.value(self, \reverbDamp, value);
};

var reverbLevelCommand = { |self, value|
	effectCommand.value(self, \reverbLevel, value);
};

var mainLevelCommand = { |self, value|
	self[\mainLevelSynth].set("level", mainLevelSpec.constrain(value));
};

var killChannel = { |self, channelnum|
	self[\channelGroups][channelnum].set(\gate, 0);
};

var killMuteGroup = { |self|
	self[\muteGroups] do: { |included, channelnum|
		if (included) {
			self[\channelGroups][channelnum].set(\gate, 0); // TODO: use killChannel instead
		}
	};
};

var includedInMuteGroup = { |self, channelnum|
	self[\muteGroups][channelnum]
};

var bussedChannelCommand = { |self, id, channelnum, value|
	getChannelBus.value(self, channelnum, id).set(channelSpecs[id].constrain(value));
};

var getChannelBus = { |self, channelnum, id|
	self[\channelControlBusses][channelnum][id]
};

var effectCommand = { |self, id, value|
	self[\effectsGroup].set(id, effectSpecs[id].constrain(value));
};

var sampleIsLoaded = { |self, channelnum| self[\buffers][channelnum].path.notNil };

var sampleIsStereo = { |self, channelnum| self[\buffers][channelnum].numChannels == 2 };

var sampleHasLoopEnabled = { |self, channelnum| self[\loopEnabled][channelnum] };

var loadSample = { |self, channelnum, path|
	var buffers = self[\buffers];
	if (channelnum >= 0 and: channelnum < numChannels) {
		var numChannels, soundFile = SoundFile.openRead(path);
		if (soundFile.notNil) {
			numChannels = soundFile.numChannels;
			soundFile.close;
			if (numChannels < 3) {
				var buffer = buffers[channelnum];
				// TODO: stop any current sample playing for channelnum, to omit the 'Buffer UGen channel mismatch: expected 1, yet buffer has 2 channels' and better cleanup before loading the new sample
				fork {
					killChannel.value(self, channelnum);
					Server.default.sync;
					buffer.allocRead(path);
					Server.default.sync;
					buffer.updateInfo(path);
					Server.default.sync;
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
};

var getChannelSpecs = {
	channelSpecs.copy;
};

var getEffectSpecs = {
	effectSpecs.copy;
};

var getMainLevelSpec = {
	mainLevelSpec.copy;
};

var channelSpecs = (
	sampleStart: \unipolar.asSpec,
	sampleEnd: \unipolar.asSpec.copy.default_(1),
	loopPoint: \unipolar.asSpec,
	loopEnable: ControlSpec(0, 1, step: 1, default: 0),
	speed: ControlSpec(0, 5, default: 1),
	volume: \db.asSpec.copy.default_(-10),
	delaySend: \db.asSpec,
	reverbSend: \db.asSpec,
	volumeEnvAttack: ControlSpec(0, 1, default: 0.001, units: "secs"),
	volumeEnvRelease: ControlSpec(0, 3, default: 3, units: "secs"),
	filterEnvAttack: ControlSpec(0, 1, default: 0.001, units: "secs"),
	filterEnvRelease: ControlSpec(0, 3, default: 0.25, units: "secs"),
	pan: \pan.asSpec,
	filterCutoff: \widefreq.asSpec.copy.default_(\widefreq.asSpec.maxval),
	filterRes: \unipolar.asSpec,
	filterLowpassLevel: ControlSpec(0, 1, step: 1, default: 1),
	filterBandpassLevel: ControlSpec(0, 1, step: 1, default: 0),
	filterHighpassLevel: ControlSpec(0, 1, step: 1, default: 0),
	filterNotchLevel: ControlSpec(0, 1, step: 1, default: 0),
	filterPeakLevel: ControlSpec(0, 1, step: 1, default: 0),
	filterEnvMod: \bipolar.asSpec,
	sampleRate: ControlSpec(0, 44100.0, default: 44100.0),
	bitDepth: ControlSpec(0, 32, default: 32),
	dist: \unipolar.asSpec
);

var effectSpecs = (
	delayTime: ControlSpec(0.0001, 5, 'exp', 0, 0.1, "secs"),
	delayFeedback: ControlSpec(0, 1.25),
	delayLevel: \db.asSpec.copy.default_(-10),
	reverbRoom: \unipolar.asSpec.copy.default_(0.75),
	reverbDamp: \unipolar.asSpec.copy.default_(0.5),
	reverbLevel: \db.asSpec.copy.default_(-10)
);

var mainLevelSpec = \db.asSpec.copy.default_(0);

var monoSamplePlayerDefName = 'Ack_Mono';
var stereoSamplePlayerDefName = 'Ack_Stereo';
var delayDefName = 'Ack_Delay';
var reverbDefName = 'Ack_Reverb';
var mainLevelDefName = 'Ack_MainLevel';

var asDict = { |arrayOfAssociations|
	IdentityDictionary.newFrom( arrayOfAssociations.collect { |assoc| [ assoc.key, assoc.value ] }.flatten );
};

// TODO: namespace

var include_relative = { |relative_path|
	var path = (PathName(thisProcess.nowExecutingPath).pathOnly +/+ relative_path).standardizePath;
	if (File.exists(path)) {
		var result = this.executeFile(path);
		if (result.isNil) { Error("error importing %".format(path)).throw; } { result };
	} {
		Error("include_relative: file % does not exist".format(path.quote)).throw;
	};
};

var formatters = include_relative.value("formatters.scd");

var add_channel_sample_param = { |channel|
	~params.add_(
		(
			type: "file",
			id: channel.asString++"_sample",
			name: channel.asString++": sample",
			action: { |value|
				if (value != "-") {
					~engine.loadSample_(channel-1, value)
				};
			}
		)
	);
};

var add_start_pos_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_start_pos",
			name: channel.asString++": start pos",
			controlspec: channelSpecs[\sampleStart],
			formatter: formatters[\unipolar_as_percentage],
			action: { |value| ~engine.sampleStart_(channel-1, value) }
		)
	);
};

var add_end_pos_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_end_pos",
			name: channel.asString++": end pos",
			controlspec: channelSpecs[\sampleEnd],
			formatter: formatters[\unipolar_as_percentage],
			action: { |value| ~engine.sampleEnd_(channel-1, value) }
		)
	);
};

var add_loop_param = { |channel|
	~params.add_(
		(
			type: "option",
			id: channel.asString++"_loop",
			name: channel.asString++": loop",
			options: ["off", "on"],
			action: { |value|
				if (value == 2) {
					~engine.enableLoop_(channel-1)
				} {
					~engine.disableLoop_(channel-1)
				}
			}
		)
	);
};

var add_loop_point_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_loop_point",
			name: channel.asString++": loop point",
			controlspec: channelSpecs[\loopPoint],
			formatter: formatters[\unipolar_as_percentage],
			action: { |value| ~engine.loopPoint_(channel-1, value) }
		)
	);
};

var add_speed_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_speed",
			name: channel.asString++": speed",
			controlspec: channelSpecs[\speed],
			formatter: formatters[\unipolar_as_percentage],
			action: { |value| ~engine.speed_(channel-1, value) }
		)
	);
};

var add_vol_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_vol",
			name: channel.asString++": vol",
			controlspec: channelSpecs[\vol],
			formatter: formatters[\default],
			action: { |value| ~engine.volume_(channel-1, value) }
		)
	);
};

var add_vol_env_atk_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_vol_env_atk",
			name: channel.asString++": vol env atk",
			controlspec: channelSpecs[\volumeEnvAttack],
			formatter: formatters[\secs_as_ms],
			action: { |value| ~engine.volumeEnvAttack_(channel-1, value) }
		)
	);
};

var add_vol_env_rel_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_vol_env_rel",
			name: channel.asString++": vol env rel",
			controlspec: channelSpecs[\volumeEnvRelease],
			formatter: formatters[\secs_as_ms],
			action: { |value| ~engine.volumeEnvRelease_(channel-1, value) }
		)
	);
};

var add_pan_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_pan",
			name: channel.asString++": pan",
			controlspec: channelSpecs[\pan],
			formatter: formatters[\bipolar_as_pan_widget],
			action: { |value| ~engine.pan_(channel-1, value) }
		)
	);
};

var add_filter_cutoff_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_filter_cutoff",
			name: channel.asString++": filter cutoff",
			controlspec: channelSpecs[\filterCutoff],
			formatter: formatters[\round].value(0.001),
			action: { |value| ~engine.filterCutoff_(channel-1, value) }
		)
	);
};

var add_filter_res_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_filter_res",
			name: channel.asString++": filter res",
			controlspec: channelSpecs[\filterRes],
			formatter: formatters[\unipolar_as_percentage],
			action: { |value| ~engine.filterRes_(channel-1, value) }
		)
	);
};

var add_filter_mode_param = { |channel|
	~params.add_(
		(
			type: "option",
			id: channel.asString++"_filter_mode",
			name: channel.asString++": filter mode",
			options: ["lowpass", "bandpass", "highpass", "notch", "peak"],
			action: { |value| ~engine.filterMode_(channel-1, value-1) }
		)
	);
};

var add_filter_env_atk_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_filter_env_atk",
			name: channel.asString++": filter env atk",
			controlspec: channelSpecs[\filterEnvAttack],
			formatter: formatters[\secs_as_ms],
			action: { |value| ~engine.filterEnvAttack_(channel-1, value) }
		)
	);
};

var add_filter_env_rel_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_filter_env_rel",
			name: channel.asString++": filter env rel",
			controlspec: channelSpecs[\filterEnvRelease],
			formatter: formatters[\secs_as_ms],
			action: { |value| ~engine.filterEnvRelease_(channel-1, value) }
		)
	);
};

var add_filter_env_mod_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_filter_env_mod",
			name: channel.asString++": filter env mod",
			controlspec: channelSpecs[\filterEnvMod],
			formatter: formatters[\bipolar_as_percentage],
			action: { |value| ~engine.filterEnvMod_(channel-1, value) }
		)
	);
};

var add_sample_rate_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_sample_rate",
			name: channel.asString++": sample rate",
			controlspec: channelSpecs[\sampleRate],
			formatter: formatters[\default],
			action: { |value| ~engine.sampleRate_(channel-1, value) }
		)
	);
};

var add_bit_depth_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_bit_depth",
			name: channel.asString++": bit depth",
			controlspec: channelSpecs[\bitDepth],
			formatter: formatters[\default],
			action: { |value| ~engine.bitDepth_(channel-1, value) }
		)
	);
};

var add_dist_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_dist",
			name: channel.asString++": dist",
			controlspec: channelSpecs[\dist],
			formatter: formatters[\unipolar_as_percentage],
			action: { |value| ~engine.dist_(channel-1, value) }
		)
	);
};

var add_mutegroup_param = { |channel|
	~params.add_(
		(
			type: "option",
			id: channel.asString++"_in_mutegroup",
			name: channel.asString++": in mutegroup",
			options: ["no", "yes"],
			action: { |value|
				if (value == 2) {
					~engine.includeInMuteGroup_(channel-1, 1)
				} {
					~engine.includeInMuteGroup_(channel-1, 0)
				};
			}
		)
	);
};

var add_delay_send_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_delay_send",
			name: channel.asString++": delay send",
			controlspec: channelSpecs[\delaySend],
			formatter: formatters[\default],
			action: { |value| ~engine.delaySend_(channel-1, value) }
		)
	);
};

var add_reverb_send_param = { |channel|
	~params.add_(
		(
			type: "control",
			id: channel.asString++"_reverb_send",
			name: channel.asString++": reverb send",
			controlspec: channelSpecs[\reverbSend],
			formatter: formatters[\default],
			action: { |value| ~engine.reverbSend_(channel-1, value) }
		)
	);
};

var add_channel_params = { |channel|
	add_channel_sample_param.value(channel);
	add_start_pos_param.value(channel);
	add_end_pos_param.value(channel);
	add_loop_param.value(channel);
	add_loop_point_param.value(channel);
	add_speed_param.value(channel);
	add_vol_param.value(channel);
	add_vol_env_atk_param.value(channel);
	add_vol_env_rel_param.value(channel);
	add_pan_param.value(channel);
	add_filter_mode_param.value(channel);
	add_filter_cutoff_param.value(channel);
	add_filter_res_param.value(channel);
	add_filter_env_atk_param.value(channel);
	add_filter_env_rel_param.value(channel);
	add_filter_env_mod_param.value(channel);
	add_sample_rate_param.value(channel);
	add_bit_depth_param.value(channel);
	add_dist_param.value(channel);
	add_mutegroup_param.value(channel);
	add_delay_send_param.value(channel);
	add_reverb_send_param.value(channel);
};

var add_delay_time_param = {
	~params.add_(
		(
			type: "control",
			id: "delay_time",
			name: "delay time",
			controlspec: effectSpecs[\delayTime],
			formatter: formatters[\secs_as_ms],
			action: { |value| ~engine.delayTime_(value) }
		)
	);
};

var add_delay_feedback_param = {
	~params.add_(
		(
			type: "control",
			id: "delay_feedback",
			name: "delay feedback",
			controlspec: effectSpecs[\delayFeedback],
			formatter: formatters[\unipolar_as_percentage],
			action: { |value| ~engine.delayFeedback_(value) }
		)
	);
};

var add_delay_level_param = {
	~params.add_(
		(
			type: "control",
			id: "delay_level",
			name: "delay level",
			controlspec: effectSpecs[\delayLevel],
			formatter: formatters[\default],
			action: { |value| ~engine.delayLevel_(value) }
		)
	);
};

var add_reverb_room_size_param = {
	~params.add_(
		(
			type: "control",
			id: "reverb_room_size",
			name: "reverb room size",
			controlspec: effectSpecs[\reverbRoom],
			formatter: formatters[\unipolar_as_percentage],
			action: { |value| ~engine.reverbRoom_(value) }
		)
	);
};

var add_reverb_damp_param = {
	~params.add_(
		(
			type: "control",
			id: "reverb_damp",
			name: "reverb damp",
			controlspec: effectSpecs[\reverbDamp],
			formatter: formatters[\unipolar_as_percentage],
			action: { |value| ~engine.reverbDamp_(value) }
		)
	);
};

var add_reverb_level_param = {
	~params.add_(
		(
			type: "control",
			id: "reverb_level",
			name: "reverb level",
			controlspec: effectSpecs[\reverbLevel],
			formatter: formatters[\default],
			action: { |value| ~engine.reverbLevel_(value) }
		)
	);
};

var add_effects_params = {
	add_delay_time_param.value();
	add_delay_feedback_param.value();
	add_delay_level_param.value();
	add_reverb_room_size_param.value();
	add_reverb_damp_param.value();
	add_reverb_level_param.value();
};

var add_main_level_param = {
	~params.add_(
		(
			type: "control",
			id: "main_level",
			name: "main output level",
			controlspec: mainLevelSpec,
			formatter: formatters[\default],
			action: { |value| ~engine.mainLevel_(value) }
		)
	);
};

var add_params = {
	numChannels.do { |channelnum|
		add_channel_params.value(channelnum+1);
		~params.add_separator_();
	};

	add_effects_params.value();
	add_main_level_param.value();
};

var exports = IdentityDictionary[
	\init -> init,
	\free -> free,
	\loadSampleCommand -> loadSampleCommand,
	\multiTrigCommand -> multiTrigCommand,
	\trigCommand -> trigCommand,
	\multiKillCommand -> multiKillCommand,
	\killCommand -> killCommand,
	\includeInMuteGroupCommand -> includeInMuteGroupCommand,
	\sampleStartCommand -> sampleStartCommand,
	\sampleEndCommand -> sampleEndCommand,
	\loopPointCommand -> loopPointCommand,
	\enableLoopCommand -> enableLoopCommand,
	\disableLoopCommand -> disableLoopCommand,
	\speedCommand -> speedCommand,
	\volumeCommand -> volumeCommand,
	\volumeEnvAttackCommand -> volumeEnvAttackCommand,
	\volumeEnvReleaseCommand -> volumeEnvReleaseCommand,
	\panCommand -> panCommand,
	\filterCutoffCommand -> filterCutoffCommand,
	\filterResCommand -> filterResCommand,
	\filterModeCommand -> filterModeCommand,
	\filterEnvAttackCommand -> filterEnvAttackCommand,
	\filterEnvReleaseCommand -> filterEnvReleaseCommand,
	\filterEnvModCommand -> filterEnvModCommand,
	\sampleRateCommand -> sampleRateCommand,
	\bitDepthCommand -> bitDepthCommand,
	\distCommand -> distCommand,
	\delaySendCommand -> delaySendCommand,
	\reverbSendCommand -> reverbSendCommand,
	\delayTimeCommand -> delayTimeCommand,
	\delayFeedbackCommand -> delayFeedbackCommand,
	\delayLevelCommand -> delayLevelCommand,
	\reverbRoomCommand -> reverbRoomCommand,
	\reverbDampCommand -> reverbDampCommand,
	\reverbLevelCommand -> reverbLevelCommand,
	\mainLevelCommand -> mainLevelCommand,
	\getChannelSpecs -> getChannelSpecs,
	\getEffectSpecs -> getEffectSpecs,
	\getMainLevelSpec -> getMainLevelSpec,
	\lib -> IdentityDictionary[
		\add_params -> add_params,
		\add_channel_params -> add_channel_params,
		\add_effects_params -> add_effects_params,
		\add_channel_sample_param -> add_channel_sample_param,
		\add_start_pos_param -> add_start_pos_param,
		\add_end_pos_param -> add_end_pos_param,
		\add_loop_param -> add_loop_param,
		\add_loop_point_param -> add_loop_point_param,
		\add_speed_param -> add_speed_param,
		\add_vol_param -> add_vol_param,
		\add_vol_env_atk_param -> add_vol_env_atk_param,
		\add_vol_env_rel_param -> add_vol_env_rel_param,
		\add_pan_param -> add_pan_param,
		\add_filter_cutoff_param -> add_filter_cutoff_param,
		\add_filter_res_param -> add_filter_res_param,
		\add_filter_mode_param -> add_filter_mode_param,
		\add_filter_env_atk_param -> add_filter_env_atk_param,
		\add_filter_env_rel_param -> add_filter_env_rel_param,
		\add_filter_env_mod_param -> add_filter_env_mod_param,
		\add_sample_rate_param -> add_sample_rate_param,
		\add_bit_depth_param -> add_bit_depth_param,
		\add_dist_param -> add_dist_param,
		\add_mutegroup_param -> add_mutegroup_param,
		\add_delay_send_param -> add_delay_send_param,
		\add_reverb_send_param -> add_reverb_send_param,
		\add_delay_time_param -> add_delay_time_param,
		\add_delay_feedback_param -> add_delay_feedback_param,
		\add_delay_level_param -> add_delay_level_param,
		\add_reverb_room_size_param -> add_reverb_room_size_param,
		\add_reverb_damp_param -> add_reverb_damp_param,
		\add_reverb_level_param -> add_reverb_level_param,
		\add_main_level_param -> add_main_level_param
	]
];

exports
)
