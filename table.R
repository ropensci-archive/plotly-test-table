require(plotly)
require(testthat)
require(httr)
##require(parallel)

data.dir <- normalizePath("data")

## This file specifies the list of all the versions of the ggplotly
## code (not testing code) for which we want to run the current tests.
code_commits <- read.csv("code_commits.csv", as.is=TRUE, comment.char="#")

## Parse the first occurance of pattern from each of several strings
## using (named) capturing regular expressions, returning a matrix
## (with column names).
str_match_perl <- function(string,pattern,...){
  stopifnot(is.character(string))
  stopifnot(is.character(pattern))
  stopifnot(length(pattern)==1)
  parsed <- regexpr(pattern,string,perl=TRUE,...)
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

## Parse several occurances of pattern from each of several strings
## using (named) capturing regular expressions, returning a list of
## matrices (with column names).
str_match_all_perl <- function(string,pattern,...){
  stopifnot(is.character(string))
  stopifnot(is.character(pattern))
  stopifnot(length(pattern)==1)
  parsed <- gregexpr(pattern,string,perl=TRUE, ...)
  lapply(seq_along(parsed),function(i){
    r <- parsed[[i]]
    full <- substring(string[i],r,r+attr(r,"match.length")-1)
    starts <- attr(r,"capture.start")
    if(is.null(starts)){
      m <- cbind(full)
      colnames(m) <- ""
      m
    }else{
      if(r[1]==-1)return(matrix(nrow=0,ncol=1+ncol(starts)))
      names <- attr(r,"capture.names")
      lengths <- attr(r,"capture.length")
      subs <- substring(string[i],starts,starts+lengths-1)
      m <- matrix(c(full,subs),ncol=length(names)+1)
      colnames(m) <- c("",names)
      m
    }
  })
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
    if(file.exists(thumb.file)){
      thumb.file
    }else{
      cmd <- paste("convert", png.file, "-resize 230", thumb.file)
      cat("\n", cmd, "\n", sep="")
      status <- system(cmd)
      if(status == 0){
        thumb.file
      }else{
        NA
      }
    }
  }else{ # no png file so we cant make a thumb.
    NA
  }
}

### We need to actually attempt conversion here, since sometimes
### an invalid PNG file gets downloaded, and file.exists() is not
### able to determine that it is bad.
png.exists <- function(png.file){
  !is.na(thumb(png.file))
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

## get the SHA1 of the tests on the current branch; and the SHA1,
## time, and subjects of all commits.
old.wd <- setwd(testthat.dir)
test.SHA1 <- system("git rev-parse HEAD", intern=TRUE)
all.cmd <- "git log --pretty=format:'%H %ci %s' --all"
all.txt <- system(all.cmd, intern=TRUE)
all.commits <- commits(all.txt)
setwd(old.wd)

## Load database which records test SHA1, test files, and test names.
load("testfileDB.RData")

## Need to initialize elements as lists, in case there are single
## elements in the result of test.ggplots.
if(is.null(testfileDB[[test.SHA1]])){
  testfileDB[[test.SHA1]] <- list()
}

## For every test-*.R file, run it and see what ggplots are produced.
for(test.file in test.files){
  test.base <- basename(test.file)
  if(is.null(testfileDB[[test.SHA1]][[test.base]])){
    result <- test.ggplots(test.file)
    testfileDB[[test.SHA1]][[test.base]] <- result
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
    SHA1.dir <- file.path(data.dir, SHA1)
    plotly.png.file <- file.path(SHA1.dir, paste0(filesystem_name, ".png"))
    log.file <- file.path(SHA1.dir, paste0(filesystem_name, ".log"))
    dir.create(SHA1.dir, showWarnings = FALSE, recursive = TRUE)
    py <- plotly("TestBot", "r1neazxo9w")
    download.tries <- if( png.exists(plotly.png.file) ) 0 else 3
    while (download.tries > 0) {
      kwargs <-
        list(filename=paste0("ggplot2/", SHA1, "/", name),
             fileopt="overwrite",
             auto_open=FALSE)
      u <- py$ggplotly(gg, kwargs=kwargs)
      plotly.png.url <- paste0(u$response$url, ".png")
      cat(sprintf("\ndownloading %s -> %s\n",
                  plotly.png.url, plotly.png.file))

      ## RCURL version:
      ## pngdata <- getURLContent(plotly.png.url, ssl.verifypeer=FALSE)
      ## raw.png <- as.raw(pngdata)

      ## httr version:
      conf <- config(ssl.verifypeer=FALSE)
      GET.time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S", tz="GMT")
      response <- GET(plotly.png.url, config=conf)
      warn_for_status(response)

      ## append to a log file, 1 line per attempt.
      log.line <- paste(GET.time, plotly.png.url, response$status_code)
      cat(log.line, "\n", sep="", file=log.file, append=TRUE)
      
      raw.png <- content(response, "raw")
      writeBin(raw.png, plotly.png.file)

      download.tries <- if ( png.exists(plotly.png.file) ) {
        0
      } else {
        cat("\nDOWNLOAD FAILED, RETRYING\n")
        Sys.sleep(1)
        download.tries - 1
      }
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
  to.run <- list()
  for(test.base in names(test.info)){
    test.names <- test.info[[test.base]]
    if(length(test.names)){
      test.pngs <- paste0(test.names, ".png")
      test.paths <- file.path(commit.dir, test.pngs)
      test.thumbs <- rep(NA, length(test.paths))
      for(test.i in seq_along(test.paths)){
        test.thumbs[[test.i]] <- thumb(test.paths[[test.i]])
      }
      if(any(is.na(test.thumbs))){
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
  thumb.file <- 
    msg <- rep(NA, length(png.file))
  for(thumb.i in seq_along(thumb.file)){
    this.png <- png.file[[thumb.i]]
    if(file.exists(this.png)){
      thumb.file[[thumb.i]] <- thumb(this.png)
    }
  }
  ## NA in thumb.file means the png does not exist, or if it exists,
  ## it has some problem and could not be converted into a -thumb.png
  ## image.
  for(thumb.i in which(is.na(thumb.file))){
    this.png <- png.file[[thumb.i]]
    log.file <- sub("png$", "log", this.png)
    msg[[thumb.i]] <- if(file.exists(log.file)){
      ##log.lines <- readLines(log.file)
      sprintf('could not download .png <a href="%s">log</a>',
              log.file)
    }else{
      "ggplotly error"
    }
  }
  MSG <- function(inside.td){
    ifelse(is.na(msg), inside.td, msg)
  }
  td.mat[, column.i] <-
    MSG(sprintf('<a href="%s"><img src="%s" /></a>', png.file, thumb.file))
  png.mat[, column.i] <-
    MSG(sprintf('<img src="%s" />', png.file))
  big.mat[, column.i] <-
    MSG(sprintf('<img src="%s" />', png.file))
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

table.SHA1 <- dir("tables")
table.commits <- all.commits[table.SHA1, ]
index <- paste0("tables/", table.SHA1, "/index.html")
th.pattern <-
  paste0("<TH> ",
         "(?<label>[^ <]+)")
column.labels <- c()
for(index.file in index){
  ilines <- readLines(index.file)
  th.line <- grep("<TH>", ilines, value=TRUE, ignore.case = TRUE)
  stopifnot(length(th.line) == 1)
  label.mat <-
    str_match_all_perl(th.line, th.pattern, ignore.case=TRUE)[[1]]
  label <- label.mat[-1, "label"]
  label <- label[label != "ggplot2"]
  column.labels[[index.file]] <- paste(label, collapse = "<br />")
}
table.commits$column.labels <- column.labels
table.commits$thumbs <- sprintf('<a href="%s">thumbs</a>', index)
index.big <- paste0("tables/", table.SHA1, "/big.html")
table.commits$big <- sprintf('<a href="%s">big</a>', index.big)
table.commits$commit.gmt.time <- format(table.commits$gmt.time)
table.commits$commit.subject <- table.commits$subject
table.commits
df <- data.frame(table.commits)[, c("commit.gmt.time",
                        "column.labels",
                        "thumbs", "big",
                        "commit.subject")]

## Sort table so that the most recent commit is on top.
ord <- order(table.commits$gmt.time, decreasing = TRUE)
xt <- xtable(df[ord, ])

print(xt, type="html", file="table.html", include.rownames=FALSE,
      sanitize.text.function=identity)
