# OVERVIEW
`sclang_format` is a code auto-formatter for `sclang`, the SuperCollider programming language.

Its philosophy (for now):
1. Indent only: line breaks and intra-line formatting are up to the programmer.
2. Stick to common conventions in `SCClassLibrary`.
3. Fix issues with the existing sc-ide formatting.

It does not currently get all cases correct, but it does significantly better that the sc-ide auto-intent.

``` 
Process sclang code passed to stdin until an end-of-file, then print the formatted result.

USAGE: 
  sclang_format [options] [<file> ...]

OPTIONS:
  -t                             - If set, indent using tabs
  -i <indent spaces>             - How many spaces to indent
  -w                             - If set, keep waiting for input from stdin. If not set, sclang_format will process stdin until an EOF and then exit.

EXAMPLE:
  cat ~/Desktop/my_code.scd | sclang_format
