#!/bin/sh

runhaskell Deadlock.hs > deadlock.dot
dot -Tpdf deadlock.dot > deadlock.pdf
open deadlock.pdf

