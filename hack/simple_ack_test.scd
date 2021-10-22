(
var include_relative = { |relative_path|
	var path = (PathName(thisProcess.nowExecutingPath).pathOnly +/+ relative_path).standardizePath;
	if (File.exists(path)) {
		this.executeFile(path);
	} {
		Error("include_relative: file % does not exist".format(path.quote)).throw;
	};
};

include_relative.value("inject_ack_globals.scd");

s.waitForBoot {
	a = ~init.value();
};
)

(
~loadSampleCommand.value(a, 0, "/path/to/sample1.wav");
~loadSampleCommand.value(a, 1, "/path/to/sample2.wav");
~loadSampleCommand.value(a, 2, "/path/to/sample3.wav");
)

(
~trigCommand.value(a, 0);
)

(
~trigCommand.value(a, 1);
)

(
~trigCommand.value(a, 2);
)
