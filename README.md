# json2dir-nim

json2dir in Nim: creates the directory tree a JSON document describes, using only the Nim standard library.

    nim c -d:release -o:json2dir json2dir.nim && ./json2dir < document.json
