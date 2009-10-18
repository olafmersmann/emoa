.PHONEY: clean test check build install pkg data

install: clean data
	R CMD INSTALL --no-multiarch pkg

test: install
	Rscript pkg/inst/unittests/runner.r

check: clean data
	R CMD check pkg && rm -fR pkg.Rcheck

data: pkg/data/cec2007.rda

pkg/data/cec2007.rda: data/cec2007/convert.r $(ls data/cec2007/*.txt) pkg/data
	(cd data/cec2007/ ; Rscript convert.r)

pkg/data:
	mkdir pkg/data

clean:
	rm -fR pkg/src/*.o pkg/src/*.so pkg.Rcheck .RData .Rhistory

pkg: clean data
	./bump-version
	echo "Date: $(date +%Y-%m-%d)" >> pkg/DESCRIPTION
	git log --no-merges -M --date=iso --format=medium pkg/ > pkg/ChangeLog
	R CMD build pkg
	R CMD build --binary pkg
	git checkout pkg/DESCRIPTION
	rm -f pkg/ChangeLog
