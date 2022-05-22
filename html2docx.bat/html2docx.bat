:: This batch file converts HTML files in a folder to docx.
:: It requires Pandoc, and a list of files to convert
:: named file-list, in which each file is on a separate line,
:: and contains no spaces in the filename.
::
:: Don't show these commands to the user
@ECHO off
:: Set the title of the window
TITLE Convert html to docx
:: This thing that's necessary.
Setlocal enabledelayedexpansion
:: What're we doing?
ECHO Converting to .docx...
:: Loop through the list of files in file-list
:: and convert them each from .html to .docx.
:: We end up with the same filenames, 
:: with .docx extensions appended.
FOR /F "tokens=*" %%F IN (file-list) DO (
    pandoc %%F -f html -t docx -s -o %%F.docx
    )
:: What are we doing next?
ECHO Fixing file extensions...
:: What are we finding and replacing?
SET find=.html
SET replace=
:: Loop through all .docx files and remove the .html
:: from those filenames pandoc created.
FOR %%# in (.\*.docx) DO (
    Set "File=%%~nx#"
    Ren "%%#" "!File:%find%=%replace%!"
)
:: Whassup?
ECHO Done.
:: Let the user exit deliberately
:exit
SET exit=
SET /p exit=Hit return to exit...
IF "%repeat%"=="" GOTO:eof
GOTO exit