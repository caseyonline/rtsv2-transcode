# Tests

running tests is achieved by running the following from `./tests/`

`> spago run`

currently there is a slight issue with browser tests & WebRTC therefore we have to set an env variable in terminal to tell Puppeteer to use local Chrome instead of Chromium.

`export PUPPETEER_EXECUTABLE_PATH="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"`

if you are wanting to run just specific tests the only sane option is to comment out specific tests. These can be done at a Case level inside of `./tests/src/Main.purs` at the beginning of file or in a similar location inside the Case files located `./tests/src/Cases/*.purs`.
