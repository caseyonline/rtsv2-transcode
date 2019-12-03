#!/usr/bin/env bash

pkill -f rtsv2TestRunner > /dev/null 2>&1 || true
pkill serf > /dev/null 2>&1 || true
