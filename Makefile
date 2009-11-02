.SILENT:
.PHONEY: clean test check build install pkg data roxygen

install: clean data
	echo "Installing package..."
	R CMD INSTALL --no-multiarch pkg > install.log 2>&1

test: install
	echo "Running unit tests..."
	Rscript pkg/inst/unittests/runner.r

check: clean data
	echo "Running R CMD check..."
	R CMD check pkg && rm -fR pkg.Rcheck

data: pkg/data/cec2007.rda

pkg: skel
	echo "Roxygenizing package..."
	./roxygenize > roxygen.log 2>&1

pkg/data: pkg
	mkdir pkg/data

pkg/data/cec2007.rda: data/cec2007/convert.r $(ls data/cec2007/*.txt) pkg/data
	echo "Creating CEC2007 dataset..."
	(cd data/cec2007/ ; Rscript convert.r)

clean:
	echo "Cleaning up..."
	rm -fR skel/src/*.o skel/src/*.so skel.Rcheck
	rm -fR pkg
	rm -fR .RData .Rhistory build.log install.log roxygen.log

package: clean data
	echo "Building package..."
	-git stash save -q
	echo "Date: $(date +%Y-%m-%d)" >> pkg/DESCRIPTION
	git log --no-merges -M --date=iso --format=medium pkg/ > pkg/ChangeLog
	R CMD build pkg > build.log 2>&1
	-git stash pop -q
	rm -f pkg/ChangeLog
