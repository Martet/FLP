all: flp23-log

flp23-log: flp23-log.pl
	swipl -q -g main -o flp23-log -c flp23-log.pl

clean:
	rm -f flp23-log

zip:
	zip flp-log-xzmitk01.zip flp23-log.pl Makefile README.md

run: flp23-log
	./flp23-log

.PHONY: all clean zip run
