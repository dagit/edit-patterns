#!/bin/bash

pandoc -t beamer --template=slide-template.t --listings slides.md > slides.tex 
rubber -d slides.tex
