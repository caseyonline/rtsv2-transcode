# Tests

running tests is achieved by running the following from `./tests/`

install puppeteer using nmp
`> npm install`

currently there is a slight issue with browser tests & WebRTC therefore we have to set an env variable in terminal to tell Puppeteer to use local Chrome instead of Chromium.

`export PUPPETEER_EXECUTABLE_PATH="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"`

now run the purescript test but using spago which will build and run the suite
`> spago run`



## Helpers

Everytime you run the tests it is a good idea to clean up the `/logs` folder in root, as running the tests will still create logs and that is a good place to look if trying to work out what is happening.

If you are wanting to run single tests you can replace `describe`/`it` with `describeOnly`/`itOnly`

If you need to a have a longer time out to play with things then check out `test/src/Main.purs` at this line:
`timeout: Just (Milliseconds 30000.0)` extended it to anythign nice and long.

The browser tests will automatically take you to the correct URL where there are 2 primary pages:
  1 player with messaging dialog `aggregatorPlayer.html`
  2 egest reference player with manual Webrtc authentication, webcam video sharing and messaging dialog `egestReferenceIngest.html`


## Configs

the tests use config files which can be found inside `test/config/`. When looking at `sys.config` you will be able to see that a lot of timing options are set here.
