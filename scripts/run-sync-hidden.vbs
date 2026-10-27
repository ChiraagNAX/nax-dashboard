' Runs the sync with no console window (same approach the bot uses). Used by the scheduled task.
Set sh = CreateObject("WScript.Shell")
sh.Run Chr(34) & "C:\Users\nax-trader 2\Documents\nax-dashboard\scripts\run-sync.bat" & Chr(34), 0, False
