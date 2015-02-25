index.html: table.html header.html
	cat header.html table.html > index.html
table.html: table.R code_commits.csv
	R --vanilla < $<


