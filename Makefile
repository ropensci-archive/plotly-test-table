index.html: index.R commits.csv
	R --vanilla < $<
	git add data/*/*.png

