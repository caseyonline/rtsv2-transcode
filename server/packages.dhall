let upstream =
      https://raw.githubusercontent.com/purerl/package-sets/erl-0.13.2-20190808/src/packages.dhall sha256:9bf5fa72c656bde6888aeb33cf93047cda65ab53c7e8f29cc37c4dcade5a89b1

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
