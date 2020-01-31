# RTSV2 Front End

-------------------------------------------------------------------------------

## Setup

If you are using Nix there is a shell.nix file in route of project which you
can use to install all needed deps, otherwise you will need to install:

`> npm install -g purescript spago`
(-g at your own risk!)

having installed purescript there is a one-off installation of the dev dependencies
`> make install`

build project:
`> spago build`
or
`> make` (for production workflow - see below)


## Dev workflow

Once you have everything set up you can work on the frontend by using spago and
parcel to watch file for changes and automatic rebuilds.

in `./client` folder run the following commands in different terminal windows

  * `> make watch-spago`
  * `> make watch-parcel`

Next you need to go to the project route and run:

`> ./run.sh`

This will start the purerl (rstv2) servers in a tmux instance, select a window and an IP
address from it, then on browser go to `*****:3000/app/` for example:

`http://172.16.169.1:3000/app/`

Looking at the Make file you will see that spago does an initial build into
`./client/dist/` and that file is then used by parcel to compile where it is
outputed to `./priv/www/assets/js/rtsv2AppBundle.js`

This file is imported inside `./priv/www/index.html` and this is then used by our
backend to serve. You will notice that inside `./priv/www/assets/*` are where
all frontend related files are stored. (css, images, js, fonts).


## Production workflow

Bundle frontend for production simply by running:

`> make`

inside `./client` folder.


## More information

This project is based on [purescript-halogen-realword](https://github.com/thomashoneyman/purescript-halogen-realworld)
