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

### Install ropensci/plotly@SHA1 from github, and run all of the
### tests.
test.commit <- function(SHA1, plotly.pkg="~/R/plotly"){
  stopifnot(is.character(SHA1))
  stopifnot(length(SHA1) == 1)
  old.wd <- setwd(plotly.pkg)
  on.exit(setwd(old.wd))
  old.SHA1 <- system("git rev-parse HEAD", intern=TRUE)
  on.exit({
    system(paste("git checkout", old.SHA1))
    setwd(old.wd)
  })
  cmd <- paste("git checkout", SHA1)
  system(cmd)
  devtools::load_all(".")
  ## This is run from within the plotly/tests/testthat directory, so
  ## data.dir should be a full path.
  result.list <- list()
  save_outputs <- function(gg, name, data.dir="~/R/plotly-test-table/data") {
    filesystem_name <- gsub(' ', '-', name)
    fs.png <- paste0(filesystem_name, ".png")
    plotly.png.file <- file.path(data.dir, SHA1, fs.png)
    SHA1.dir <- dirname(plotly.png.file)
    dir.create(SHA1.dir, showWarnings = FALSE, recursive = TRUE)
    if(!file.exists(plotly.png.file))mcparallel({
      py <- plotly("TestBot", "r1neazxo9w")
      kwargs <-
        list(filename=paste0("ggplot2/", name),
             fileopt="overwrite",
             auto_open=FALSE)
      u <- py$ggplotly(gg, kwargs=kwargs)
      plotly.png.url <- paste0(u$response$url, ".png")
      cat(sprintf("downloading %s -> %s\n", plotly.png.url, plotly.png.file))
      pngdata <- getURLContent(plotly.png.url)
      writeBin(as.raw(pngdata), plotly.png.file)
    })
    gg.png.file <- file.path(data.dir, "ggplot2", fs.png)
    ggplot.dir <- dirname(gg.png.file)
    dir.create(ggplot.dir, showWarnings = FALSE, recursive = TRUE)
    if(!file.exists(gg.png.file))mcparallel({
      ggsave(gg.png.file, plot=gg, w=7, h=5)
    })
    result.list[[name]] <<-
      data.frame(SHA1, name, plotly=plotly.png.file, ggplot2=gg.png.file)
  }
  require(testthat)
  e <- new.env()
  test_dir(file.path("tests", "testthat"), env=e)
  do.call(rbind, result.list)
}

commits <- read.csv("commits.csv", as.is=TRUE)

commits.list <- list()
for(commit.i in 1:nrow(commits)){
  SHA1 <- commits$SHA1[commit.i]
  commit.df <- test.commit(SHA1)
  commits.list[[SHA1]] <- commit.df
}
