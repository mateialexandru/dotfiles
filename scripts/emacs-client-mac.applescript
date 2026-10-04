-- Launch only a frame attached to the launchd Emacs server.
-- The Homebrew-bundled app also calls `open -a Emacs`, which starts a
-- separate full instance, and falls back to one when the server is absent.

on clientCommand(arguments)
	set authFile to quoted form of ((POSIX path of (path to home folder)) & ".config/emacs/server/server")
	return "PATH='/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin' ALTERNATE_EDITOR=false emacsclient -f " & authFile & " -a false -n " & arguments
end clientCommand

on run
	try
		do shell script my clientCommand("-c")
	on error errorMessage
		display notification errorMessage with title "Emacs Client"
	end try
end run

on open droppedFiles
	set arguments to "-c"
	repeat with oneFile in droppedFiles
		set arguments to arguments & " " & quoted form of POSIX path of oneFile
	end repeat
	try
		do shell script my clientCommand(arguments)
	on error errorMessage
		display notification errorMessage with title "Emacs Client"
	end try
end open

on open location targetURL
	try
		do shell script my clientCommand(quoted form of targetURL)
	on error errorMessage
		display notification errorMessage with title "Emacs Client"
	end try
end open location
