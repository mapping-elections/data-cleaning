all : data

# Download and unzip raw data
# -------------------------------------------------------------------
data : data-raw/nnv-xml data-raw/nnv-tsv/all-votes.tsv data-raw/nnv-office data-raw/nnv-names data-raw/nnv-party/party-authority.xml

# XML records for each election in NNV
data-raw/nnv-xml : temp/nnv-xml.zip
	mkdir -p $@
	mkdir -p temp/nnv-xml
	unzip -o $^ -d temp/nnv-xml
	find temp/nnv-xml/ -type f -iname *.xml -exec mv -t $@ -i '{}' +

temp/nnv-xml.zip :
	curl http://lincolnmullen.com/files/nnv-xml.zip -o $@

# NNV export of all votes
# http://hdl.handle.net/10427/70452
data-raw/nnv-tsv/all-votes.tsv : temp/all-votes.zip
	unzip -o $^ -d $(@D)
	touch $@

temp/all-votes.zip :
	curl http://dl.tufts.edu/file_assets/generic/tufts:MS115.003.001.00001/0 -o $@

# Election records candidate office authority records
# http://hdl.handle.net/10427/70465
data-raw/nnv-office : temp/office-authority.zip
	unzip -o $^ -d $@

temp/office-authority.zip :
	curl http://dl.tufts.edu/file_assets/generic/tufts:MS115.003.001.00003/0 -o $@

# Election records candidate name authority records
# http://hdl.handle.net/10427/70466
data-raw/nnv-names : temp/name-authority.zip
	unzip -o $^ -d $@

temp/name-authority.zip :
	curl http://dl.tufts.edu/file_assets/generic/tufts:MS115.003.001.00002/0 -o $@

# Party authorities
# http://hdl.handle.net/10427/002163
data-raw/nnv-party/party-authority.xml :
	mkdir -p $(@D)
	curl http://dl.tufts.edu/file_assets/generic/tufts:party-authority/0 -o $@

# Utilities
# -------------------------------------------------------------------
setup :
	@mkdir -p R
	@mkdir -p data
	@mkdir -p data-raw
	@mkdir -p output
	@mkdir -p scripts
	@mkdir -p temp

clean :
	rm -rf temp/*

clobber : clean
	rm -rf output/*

.PHONY : all setup clean clobber data

