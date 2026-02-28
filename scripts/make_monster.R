# PowerShell script to munge all .csv files in a directory into monster.csv
#
# TODO: abort with an error msg if headers are not all identical
# TODO: rewrite this ugly PS script (my first).  Would be easier in /bin/sh!

# Get-ChildItem "." -Filter *.csv |
#   Foreach-Object {
#     $content = Get-Content $_.FullName
#   # omit first line (headers), write to .xsv
#     $content | Select-Object -Skip 1 | Set-Content ($_.BaseName + '.xsv')
#   }
# $content | Select-Object -First 1 | Set-Content headers.csv
# Get-Content *.xsv  | Set-Content body.csv
# Get-Content headers.csv, body.csv | Set-Content monster.csv
#
# Remove-Item headers.csv
# Remove-Item body.csv
# Remove-Item *.xsv
