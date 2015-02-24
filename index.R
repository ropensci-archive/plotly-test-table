require(plotly)
require(testthat)
require(parallel)

data.dir <- normalizePath("data")

## This file specifies the list of all the versions of the ggplotly
## code (not testing code) for which we want to run the current tests.
code_commits <- read.csv("code_commits.csv", as.is=TRUE, comment.char="#")

## Parse the first occurance of pattern from each of several strings
## using (named) capturing regular expressions, returning a matrix
## (with column names).
str_match_perl <- function(string,pattern){
  stopifnot(is.character(string))
  stopifnot(is.character(pattern))
  stopifnot(length(pattern)==1)
  parsed <- regexpr(pattern,string,perl=TRUE)
  captured.text <- substr(string,parsed,parsed+attr(parsed,"match.length")-1)
  captured.text[captured.text==""] <- NA
  captured.groups <- if(is.null(attr(parsed, "capture.start"))){
    NULL
  }else{
    do.call(rbind,lapply(seq_along(string),function(i){
      st <- attr(parsed,"capture.start")[i,]
      if(is.na(parsed[i]) || parsed[i]==-1)return(rep(NA,length(st)))
      substring(string[i],st,st+attr(parsed,"capture.length")[i,]-1)
    }))
  }
  result <- cbind(captured.text,captured.groups)
  colnames(result) <- c("",attr(parsed,"capture.names"))
  result
}

### Named capture regular expression for parsing git log
### --pretty=format:'%H %ci %s' vis str_match_perl.
commit.line.pattern <-
  paste0("(?<SHA1>[0-9a-f]{40})",
         " ",
         "(?<datetime>",
         "(?<year>[0-9]{4})",
         "-",
         "(?<month>[0-9]{2})",
         "-",
         "(?<day>[0-9]{2})",
         " ",
         "(?<hour>[0-9]{2})",
         ":",
         "(?<minute>[0-9]{2})",
         ":",
         "(?<second>[0-9]{2})",
         ")",
         " ",
         "(?<gmt_offset_sign>[-+])",
         "(?<gmt_offset_hours>[0-9]{2})",
         "(?<gmt_offset_minutes>[0-9]{2})",
         " ",
         "(?<subject>.*)")

### Parse a git log character vector, returning data.frame.
commits <- function(git.lines){
  stopifnot(is.character(git.lines))
  m <- str_match_perl(git.lines, commit.line.pattern)
  stopifnot(!is.na(m[,1]))
  commit.time <- strptime(m[,"datetime"], "%Y-%m-%d %H:%M:%S", "GMT") + 0
  gmt.offset.sign <- ifelse(m[,"gmt_offset_sign"]=="+", 1, -1)
  gmt.offset.hours <- as.numeric(m[,"gmt_offset_hours"])
  minutes.only <- as.numeric(m[,"gmt_offset_minutes"])
  gmt.offset.minutes <- minutes.only + gmt.offset.hours * 60
  gmt.offset.seconds <- gmt.offset.minutes * 60
  gmt.offset <- gmt.offset.sign * gmt.offset.seconds
  gmt.time <- commit.time - gmt.offset
  subject <- m[, "subject"]
  SHA1 <- m[, "SHA1"]
  abbrev <- substr(subject, 1, 18)
  commits <- data.frame(SHA1, subject, abbrev, gmt.time,
                        stringsAsFactors = FALSE)
  rownames(commits) <- commits$SHA1
  class(commits) <- c("commits", "data.frame")
  stopifnot(nrow(commits) == length(git.lines))
  commits
}

### Print commits 80 characters per line.
print.commits <- function(commits){
  print(as.data.frame(commits)[, c("gmt.time", "abbrev")])
}

### Use convert on the command line to convert file.png to
### file-thumb.png.
thumb <- function(png.file){
  stopifnot(is.character(png.file))
  stopifnot(length(png.file) == 1)
  if(file.exists(png.file)){
    thumb.file <- sub("[.]png$", "-thumb.png", png.file)
    if(!file.exists(thumb.file)){
      ##mcparallel({
      cmd <- paste("convert", png.file, "-resize 230", thumb.file)
      cat("\n", cmd, "\n", sep="")
      system(cmd)
      ##})
    }
    thumb.file
  }else{
    NA
  }
}

### Run all tests in test.file, defining save_outputs in a way which
### just saves ggplots.
test.ggplots <- function(test.file){
  result.list <- list()
  save_outputs <- function(gg, name, ...) {
    filesystem_name <- gsub(' ', '-', name)
    fs.png <- paste0(filesystem_name, ".png")
    gg.png.file <- file.path(data.dir, "ggplot2", fs.png)
    ggplot.dir <- dirname(gg.png.file)
    dir.create(ggplot.dir, showWarnings = FALSE, recursive = TRUE)
    if(!file.exists(gg.png.file)){
      cat(sprintf("\npng(%s)\n", gg.png.file)) 
      png(gg.png.file, width=700, h=500, type="cairo")
      print(gg)
      dev.off()
    }
    result.list[[name]] <<- name
  }
  e <- new.env()
  tryCatch({
    testthat:::test_file(test.file, env=e)
  }, error=function(e){
    cat("\nError in testthat::test_file\n")
    print(e)
  })
  as.character(do.call(c, result.list))
}

## Assume plotly repos is in the same directory as plotly-test-table
## repos.
plotly.pkg <- file.path("..", "plotly")
testthat.dir <- file.path(plotly.pkg, "tests", "testthat")
test.files <- Sys.glob(file.path(testthat.dir, "test-*.R"))

## get the SHA1 of the tests on the current branch.
old.wd <- setwd(testthat.dir)
test.SHA1 <- system("git rev-parse HEAD", intern=TRUE)
setwd(old.wd)

## Load database which records test SHA1, test files, and test names.
load("testfileDB.RData")

## For every test-*.R file, run it and see what ggplots are produced.
for(test.file in test.files){
  test.base <- basename(test.file)
  if(is.null(testfileDB[[test.SHA1]][[test.base]])){
    testfileDB[[test.SHA1]][[test.base]] <- test.ggplots(test.file)
  }
}

## Save the testfileDB to avoid having to re-run these tests just to
## find out which plots are produced.
save(testfileDB, file="testfileDB.RData")

### After having installed the version of plotly specified in
### code_commits.csv, and run all of the tests and save the generated
### plotlys to disk.
test.plotlys <- function(test.file){
  ## This is run from within the plotly/tests/testthat directory, so
  ## data.dir should be a full path.
  save_outputs <- function(gg, name, ...) {
    filesystem_name <- gsub(' ', '-', name)
    fs.png <- paste0(filesystem_name, ".png")
    plotly.png.file <- file.path(data.dir, SHA1, fs.png)
    SHA1.dir <- dirname(plotly.png.file)
    dir.create(SHA1.dir, showWarnings = FALSE, recursive = TRUE)
    if(!file.exists(plotly.png.file)){
      ##mcparallel({
        py <- plotly("TestBot", "r1neazxo9w")
        kwargs <-
          list(filename=paste0("ggplot2/", SHA1, "/", name),
               fileopt="overwrite",
               auto_open=FALSE)
        u <- py$ggplotly(gg, kwargs=kwargs)
        plotly.png.url <- paste0(u$response$url, ".png")
        cat(sprintf("\ndownloading %s -> %s\n",
                    plotly.png.url, plotly.png.file))
        pngdata <- getURLContent(plotly.png.url)
        writeBin(as.raw(pngdata), plotly.png.file)
      ##})
    }
  }
  e <- new.env()
  tryCatch({
    test_file(test.file, env=e)
  }, error=function(e){
    cat("\nError in testthat::test_file\n")
    print(e)
  })
}

## Check to see if we need to run any of the tests, by checking to see
## if the PNG files exist. If not, run the test and make the plotlys.
test.info <- testfileDB[[test.SHA1]]
for(commit.i in 1:nrow(code_commits)){
  code_commit <- code_commits[commit.i, ]
  SHA1 <- code_commit$SHA1
  commit.dir <- file.path("data", SHA1)
  commit.files <- dir(commit.dir)
  to.run <- list()
  for(test.base in names(test.info)){
    test.names <- test.info[[test.base]]
    if(length(test.names)){
      test.pngs <- paste0(test.names, ".png")
      if(!all(test.pngs %in% commit.files)){
        to.run[[test.base]] <- test.base
      }
    }
  }
  if(length(to.run)){
    install.str <- paste0(code_commit$user, "/plotly@", SHA1)
    devtools::install_github(install.str)
  }
  for(test.base in to.run){
    test.file <- file.path(testthat.dir, test.base)
    test.plotlys(test.file)
  }
}

## Make test table with one row for every element of current
## testfileDB (test.info) and one column for every line in
## code_commits.csv.
test.names <- as.character(unlist(test.info))
rev.commits <- code_commits[nrow(code_commits):1, ]
columns.df <- with(rev.commits, {
  data.frame(dir=c("ggplot2", SHA1), label=c("ggplot2", label))
})
td.mat <- 
  matrix(NA, length(test.names), nrow(columns.df))
rownames(td.mat) <- test.names
colnames(td.mat) <- columns.df$label
png.mat <- big.mat <- td.mat

table.dir <- file.path("tables", test.SHA1)
dir.create(table.dir, recursive=TRUE)
## First go to the directory where we will be making the table, so
## thumb() works fine.
old.wd <- setwd(table.dir)
for(column.i in 1:nrow(columns.df)){
  column.info <- columns.df[column.i, ]
  png.file <-
    file.path("..", "..", "data", column.info$dir, paste0(test.names, ".png"))
  thumb.file <- png.file
  for(thumb.i in seq_along(thumb.file)){
    thumb.file[[thumb.i]] <- thumb(thumb.file[[thumb.i]])
  }
  td.mat[, column.i] <-
    sprintf('<a href="%s"><img src="%s" /></a>', png.file, thumb.file)
  png.mat[, column.i] <-
    sprintf('<img src="%s" />', png.file)
  big.mat[, column.i] <-
    sprintf('<img src="%s" />', png.file)
}
setwd(old.wd)

library(xtable)
for(test.name in rownames(td.mat)){
  img.tag <- png.mat[test.name, ]
  df <- data.frame(label=names(img.tag), img.tag)
  xt <- xtable(t(df))
  html.file <- file.path(table.dir, paste0(test.name, ".html"))
  print(xt, type="html", file=html.file, sanitize.text.function=identity,
        include.rownames=FALSE, include.colnames=FALSE)
}
## Make big.html which shows big images directly.
big.df <-
  data.frame(test=rownames(td.mat),
             big.mat)
xt <- xtable(big.df)
print(xt, type="html", file=file.path(table.dir, "big.html"),
      sanitize.text.function=identity,
      include.rownames=FALSE)
## Make index.html which shows small -thumb.png images and links to
## the bigger ones.
td.df <-
  data.frame(test=sprintf('<a href="%s.html">%s</a>',
               test.names, test.names),
             td.mat)
xt <- xtable(td.df)
print(xt, type="html", file=file.path(table.dir, "index.html"),
      sanitize.text.function=identity,
      include.rownames=FALSE)

table.dirs <- file.info(Sys.glob(file.path("tables", "*")))
sorted.tables <- table.dirs[order(table.dirs$mtime, decreasing = TRUE), ]
mtime <- sorted.tables[, "mtime"]
index <- paste0(rownames(sorted.tables), "/index.html")
thumbs <- sprintf('<a href="%s">%s</a>', index, index)
index.big <- paste0(rownames(sorted.tables), "/big.html")
big <- sprintf('<a href="%s">%s</a>', index.big, index.big)
df <- data.frame(mtime=format(mtime), thumbs, big)
xt <- xtable(df)
## TODO: maybe add git commit log message to this table.
print(xt, type="html", file="index.html", include.rownames=FALSE,
      sanitize.text.function=identity)
