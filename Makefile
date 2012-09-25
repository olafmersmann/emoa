GITVERSION := $(shell sh -c ./gitversion)

.SILENT:
.PHONEY: clean test check build install package data usage help

usage:
	echo "Available targets:"
	echo ""
	echo " install  - install the package, writing the output into install.log"
	echo " test     - install package and run unit tests"
	echo " check    - run R CMD check on the package"
	echo " help     - shows all available targets"

help: usage
	echo " clean    - clean up package cruft"
	echo " package  - build source package of last commit"
	echo " pkg      - roxygenize skel/ into pkg/"
	echo " data     - generate R data files from raw text files"

unexport MAKEFLAGS
install: clean data
	echo "Installing package..."
	R CMD INSTALL --no-multiarch pkg > install.log 2>&1

test: install
	echo "Running unit tests..."
	Rscript pkg/inst/unittests/runner.r

unexport MAKEFLAGS
check: clean data
	echo "Running R CMD check..."
	R CMD check pkg && rm -fR pkg.Rcheck

data: pkg/data/cec2007.rda

clean:
	echo "Cleaning up..."
	rm -fR skel/src/*.o skel/src/*.so skel.Rcheck skel/src/weight_vectors.h
	rm -fR pkg
	rm -fR .RData .Rhistory build.log install.log roxygen.log

package: clean data
	echo "Building package..."
	-git stash save -q
	echo "Date: $(date +%Y-%m-%d)" >> pkg/DESCRIPTION
	-git log --no-merges -M --date=iso skel/ > pkg/ChangeLog
	R CMD build pkg > build.log 2>&1
	-git stash pop -q
	rm -f pkg/ChangeLog

pkg: skel/src/weight_vectors.h
	echo "Updating 'Version' field..."
	sed -i'' -e "s/^Version: UNKNOWN/Version: ${GITVERSION}/g" skel/DESCRIPTION
	echo "Roxygenizing package..."
	./roxygenize > roxygen.log 2>&1
	sed -i'' -e "s/^Version:.*/Version: UNKNOWN/g" skel/DESCRIPTION

pkg/data: pkg
	mkdir pkg/data

pkg/data/cec2007.rda: pkg/data data/cec2007/convert.r $(ls data/cec2007/*.txt)
	echo "Creating CEC2007 dataset..."
	(cd data/cec2007/ ; Rscript convert.r)

skel/src/weight_vectors.h: skel/src/r_ind.c skel/src/precomputed_weight_vectors.h
	echo "Precomputing weight vectors for R indicator..."
	$(CC) -std=c99 -lm -DGENERATE_WV_HEADER -o skel/src/gen_header $<
	skel/src/gen_header > $@
	rm skel/src/gen_header
