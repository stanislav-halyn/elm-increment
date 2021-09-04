
# Elm increment

This is an increment application implemented with Elm


## Development Build

[Install Elm](https://guide.elm-lang.org/install.html) (e.g. with `npm install --global elm`), then from the root project directory, run this:

```
$ elm make src/Increment.elm --output dist/elm.js
```

If you want to include the time-traveling debugger, add `--debug` like so:

```
$ elm make src/Increment.elm --output dist/elm.js --debug
```

To view the site in a browser, bring up `index.html` from any local HTTP server, for example [`http-server`](https://www.npmjs.com/package/http-server).

## Production Build

This is a two-step process. First we compile `elm.js` using `elm make` with `--optimize`, and then we Uglify the result.

#### Step 1

```
$ elm make src/Increment.elm --output dist/elm.js --optimize
```

This [generates production-optimized JS](https://elm-lang.org/blog/small-assets-without-the-headache) that is ready to be minified further using Uglify.

#### Step 2

(Make sure you have [Uglify](http://lisperator.net/uglifyjs/) installed first, e.g. with `npm install --global uglify-js`)

```
$ uglifyjs dist/elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output=dist/elm.js && uglifyjs dist/elm.js --mangle --output=dist/elm.js
```

This one lengthy command (make sure to scroll horizontally to get all of it if you're copy/pasting!) runs `uglifyjs` twice - first with `--compress` and then again with `--mangle`.

> It's necessary to run Uglify twice if you use the `pure_funcs` flag, because if you enable both `--compress` and `--mangle` at the same time, the `pure_funcs` argument will have no effect; Uglify will mangle the names first and then not recognize them when it encounters those functions later.
