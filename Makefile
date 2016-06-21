all :

setup :
	mkdir -p data-raw
	mkdir -p data
	mkdir -p temp
	mkdir -p output
	mkdir -p R

clean :
	rm -rf temp

clobber : clean
	rm -rf output

.PHONY : all setup clean clobber
