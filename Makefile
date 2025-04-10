.PHONY: all
.DEFAULT_GOAL := all

all:
	python3 main.py

check:
	python3 main.py | FileCheck main.py
