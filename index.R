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
test.commit <- function(SHA1, plotly.pkg=file.path("..", "plotly")){
  require(parallel)
  require(plotly)
  require(testthat)
  stopifnot(is.character(SHA1))
  stopifnot(length(SHA1) == 1)
  thumb <- function(png.file){
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
  devtools::install_github(paste0("ropensci/plotly@", SHA1))
  ## This is run from within the plotly/tests/testthat directory, so
  ## data.dir should be a full path.
  result.list <- list()
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
          list(filename=paste0("ggplot2/", name),
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
    gg.png.file <- file.path(data.dir, "ggplot2", fs.png)
    ggplot.dir <- dirname(gg.png.file)
    dir.create(ggplot.dir, showWarnings = FALSE, recursive = TRUE)
    if(!file.exists(gg.png.file)){
      cat(sprintf("\npng(%s)\n", gg.png.file)) 
      png(gg.png.file, width=700, h=500, type="cairo")
      print(gg)
      dev.off()
    }
    result.list[[name]] <<-
      data.frame(SHA1, name,
                 plotly=plotly.png.file,
                 ggplot2=gg.png.file,
                 plotly.thumb=thumb(plotly.png.file),
                 ggplot2.thumb=thumb(gg.png.file))
  }
  e <- new.env()
  testthat.dir <- file.path(plotly.pkg, "tests", "testthat")
  tryCatch({
    test_dir(testthat.dir, env=e)
  }, error=function(e){
    cat("\nError in testthat::test_dir\n")
    print(e)
  })
  do.call(rbind, result.list)
}

data.dir <- normalizePath("data")

commits <- read.csv("commits.csv", as.is=TRUE)

commits.list <- list()
for(commit.i in 1:nrow(commits)){
  SHA1 <- commits$SHA1[commit.i]
  commit.df <- test.commit(SHA1)
  commits.list[[SHA1]] <- commit.df
}

columns.list <- commits.list
names(columns.list) <- commits$label
recent.df <- commits.list[[length(columns.list)]]
recent.df$plotly <- recent.df$ggplot2
recent.df$plotly.thumb <- recent.df$ggplot2.thumb
columns.list$ggplot2 <- recent.df
td.mat <- 
  matrix(NA, nrow(recent.df), length(columns.list))
rownames(td.mat) <- recent.df$name
colnames(td.mat) <- names(columns.list)
png.mat <- td.mat
for(column.name in names(columns.list)){
  df <- columns.list[[column.name]]
  png.file <- sub(".*plotly-test-table/", "", df$plotly)
  thumb.file <- sub(".*plotly-test-table/", "", df$plotly.thumb)
  td.mat[as.character(df$name), column.name] <-
    sprintf('<a href="%s"><img src="%s" /></a>', png.file, thumb.file)
  png.mat[as.character(df$name), column.name] <-
    sprintf('<img src="../%s" />', png.file)
}
library(xtable)
dir.create("html")
details.page <- rep(NA, nrow(td.mat))
names(details.page) <- rownames(td.mat)
for(test.name in rownames(td.mat)){
  img.tag <- png.mat[test.name, ]
  df <- data.frame(label=names(img.tags), img.tag)
  xt <- xtable(t(df))
  html.file <- file.path("html", paste0(test.name, ".html"))
  print(xt, type="html", file=html.file, sanitize.text.function=identity,
        include.rownames=FALSE, include.colnames=FALSE)
  details.page[[test.name]] <- html.file
}
td.df <-
  data.frame(test=sprintf('<a href="%s">%s</a>',
               details.page, rownames(td.mat)),
             td.mat)
xt <- xtable(td.df)
print(xt, type="html", file="index.html", sanitize.text.function=identity,
      include.rownames=FALSE)
