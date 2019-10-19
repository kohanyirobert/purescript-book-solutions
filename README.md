# PureScript Workbook

Updated sample code (PureScript compiler version `0.13.4`) seen in _The Eff Monad_ chapter in the book _PureScript by Example_ (version `2017-09-24`) by Phil Freeman.

## Running

1. `npm install -g purescript spago parcel-bundler`
1. `npm install`
1. `spago build`
1. `parcel index.html`

### REPL

Use `spago repl`, however [on Windows this has some issues](https://github.com/spacchetti/spago/issues/483), so instead run the REPL as follows

`purs repl "src\**\*.purs" ".spago\*\*\src\**\*.purs"`
