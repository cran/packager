#!/bin/sh
tags=$(git tag -l)
version=$(grep "Version" DESCRIPTION | cut -f2 -d " ")
for tag in $tags
do
    if test "$tag" = "$version"
    then
        echo "You have tagged this version, use a development version!"
        echo "You can do so by running"
        echo "  git stash"
        echo "first, then"
        echo "  R -e 'packager::use_dev_version()'" 
        echo "or"
        echo "  make use_dev_version"
        echo "and finally"
        echo "  git stash pop"
        echo "Use 'git commit --no-verify' to override this check."
        exit 1
    fi
done
