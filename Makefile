.PHONY: all
.DEFAULT_GOAL := all

all:
	python3 hm.py

check:
	lit -svv test
