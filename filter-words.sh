#!/bin/sh

tr A-Z a-z < "${1-/dev/stdin}" \
    | LC_ALL=C grep -Ex '[a-z]+' \
            > "${2-/dev/stdout}"
