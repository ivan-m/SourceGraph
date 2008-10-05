#!/bin/sh

NewVer=$1

sed -i -e "s/^\(Version:\s*\)\S*$/\1$NewVer/" SourceGraph.cabal
sed -i -e "s/^\(programVersion = \"\).*\(\"\)/\1$NewVer\2/" Main.hs

darcs record && darcs tag $NewVer && echo Version set to: $NewVer

