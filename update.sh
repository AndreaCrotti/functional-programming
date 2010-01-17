#!/bin/bash

# get the slides updated
URL="http://verify.rwth-aachen.de/fp09/"

get_slides.py -r "solution.*(?:pdf|hs)" -d official $URL
get_slides.py -r "folie.*.pdf" -d folie $URL
get_slides.py -r "sheet.*.pdf" -d exercises $URL