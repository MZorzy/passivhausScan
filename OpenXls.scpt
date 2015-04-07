#!/usr/bin/osascript

# do shell script {"rm -rf /tmp/foo.out"}

on run argv

set shinyfile to item 1 of argv
activate application "Microsoft Excel"
delay 0.5
tell application "System Events"
	tell process "Microsoft Excel"
		delay 0.5
		set foo to open "/tmp/tst.xls"
		try
			set cancel to button "Cancel" of window 1
			click (cancel)
		end try
		try
			set macros to button "Enable Macros" of window 1
			click macros
		end try
		delay 0.5
		try
			set links to button "Ignore Links" of window 1
			click links
		end try
		delay 0.5
	end tell
end tell
tell application "Microsoft Excel"
  unprotect active workbook
end tell
tell application "Microsoft Excel"
	calculate
	tell worksheet "Verification" of active workbook
#    set value of range "C26" to 150
		set dat to get value of range "F39"
	end tell
	save active workbook
	close active workbook

	#set myrange to range ("C26") of sheet {"Verification"} of document 1
	#	set dat to get value of myrange	#	set myrange to range ("J17") of sheet {"Windows"} of document 1
	# set dat to get value of myrange of sheet ("Windows") of document 1
  #
	# set outfile to open for access "/tmp/tmp.xlsb" with write permission
	# set outbook to active workbook
	# set update remote references of newBook to true
	# save workbook as outbook filename outfile file format {"Excel Binary Workbook"}
end tell

set output to (dat as string) 
return output

end run

#set output to (dat as string) & return & linefeed
#do shell script "echo " & quoted form of output
#do shell script "echo " & output

#tell application "System Events"
#	# set dat to "3.14159"
#	set myfile to open for access "/tmp/foo.out" with write permission
#	write (dat as string) & return & linefeed to myfile
#	close access myfile
#end tell
#do shell script {"killall 'Microsoft Excel'"}
