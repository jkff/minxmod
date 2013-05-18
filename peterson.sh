#!/bin/sh

runhaskell Peterson.hs > peterson.dot
dot -Tpdf peterson.dot > peterson.pdf
open peterson.pdf

