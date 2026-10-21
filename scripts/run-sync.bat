@echo off
"C:\Program Files\R\R-4.6.1\bin\Rscript.exe" -e ".libPaths(c(file.path(Sys.getenv('USERPROFILE'),'R-libs'),.libPaths())); source('C:/Users/nax-trader 2/Documents/nax-dashboard/scripts/sync-from-bot.R')" >> "C:\Users\nax-trader 2\Documents\nax-dashboard\logs\sync.log" 2>&1
