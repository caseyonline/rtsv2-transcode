# RTS V2

-------------------------------------------------------------------------------

## Setup / Getting started

Assuming you have nix installed go to root of the repo and run

``` shell
> nix-shell
```
This will download/install everytthing related to the project, please be aware it may take some time first time around.


## Building the project

From root of repo, go into a nix enviroment by running:
``` shell
> nix-shell
```
Now you will need to build the project by:
``` shell
> rebar3 compile
```
Once this has completed you are ready to start the servers.


## Starting the servers

To get a simplified version running, from withing a nix enviroment run the following from root of repo:
``` shell
> ./run.sh simple
```
*to run the full version simply remove "simple" from the command*

Open a new terminal window and after 10-15 seconds you can start a stream by running:

``` shell
> ./scripts/run_slot1_1000.sh
```
*remember that you will most likely need to have a local version of [id3as.media](https://github.com/id3as/id3as.media) as the script uses a video file from there to start a stream.*

To check things are indeed working open browser and navigate to:
[http://172.16.171.1:3000/public/client/00000000-0000-0000-0000-000000000001/primary/player](http://172.16.171.1:3000/public/client/00000000-0000-0000-0000-000000000001/primary/player)

On the top left you should see a video running and to the right of that some stats.
