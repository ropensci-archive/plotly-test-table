index.html: index.R commits.csv
	R --vanilla < $<
	git add data/*/*.png
	git push origin gh-pages
