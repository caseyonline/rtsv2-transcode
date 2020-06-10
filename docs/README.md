# RTSV2 Documentation

-------------------------------------------------------------------------------

## Setup

The documentations are made using ReDoc.
To get setup you will need to run:

``` shell
> npm install
```

This will install the redoc-cli which we will use to build and run the docs.
Next we need to serve the documentation, which is done by:

``` shell
> npm run makedocs
```

Now simply navigate to [http://127.0.0.1:8080/](http://127.0.0.1:8080/) using your favourite browser.


## Dev workflow

installation is the same as but you have the ability to edit the files and it to automatically update the documentation.
This can be achieved by running:

``` shell
> npm run watchdocs

```
