works_with_R("3.0.2", animint="2014.3.19", xtable="1.7.3")
viz.path <- Sys.glob(file.path("examples", "*", "viz.R"))
sub.dirs <- dirname(viz.path)
base <- basename(sub.dirs)
n.vars <- n.interactive <- name.vars <- name.interactive <- code.lines <-
  comment.lines <- n.plots <- n.rows <- n.onscreen <- seconds <- 
  rep(NA, length(base))
animated <- rep("no", length(base))
for(sub.dir.i in seq_along(sub.dirs)){
  sub.dir <- sub.dirs[[sub.dir.i]]
  cat(sprintf("%4d / %4d %s\n", sub.dir.i, length(sub.dirs), sub.dir))
  viz.f <- file.path(sub.dir, "viz.R")
  source(viz.f)
  n.plots.i <- 0
  n.onscreen.i <- 0
  n.rows.i <- 0
  for(L in viz){
    if(is.ggplot(L)){
      n.plots.i <- n.plots.i + 1
      for(l in L$layers){
        if(!is.null(nrow(l$data))){
          n.rows.i <- n.rows.i + nrow(l$data)
          aes.names <- names(l$mapping)
          is.ss <- grepl("showSelected", aes.names)
          chunk.rows <- if(any(is.ss)){
            split.by <- l$data[as.character(l$mapping[is.ss])]
            do.call(table, split.by)
          }else{
            nrow(l$data)
          }
          n.onscreen.i <- n.onscreen.i + median(chunk.rows)
        }
      }
    }
  }
  stopifnot(n.onscreen.i < n.rows.i)
  n.onscreen[[sub.dir.i]] <- n.onscreen.i
  n.rows[[sub.dir.i]] <- n.rows.i
  n.plots[[sub.dir.i]] <- n.plots.i
  seconds[[sub.dir.i]] <- system.time({
    meta <- gg2animint(viz, open.browser=FALSE)
  })[["elapsed"]]
  vizLines <- readLines(viz.f)
  no.comments <- sub("#.*", "", vizLines)
  no.whitespace <- gsub(" ", "", no.comments)
  no.blank <- no.whitespace[no.whitespace != ""]
  comment.lines[[sub.dir.i]] <- length(vizLines)-length(no.blank)
  code.lines[[sub.dir.i]] <- length(no.blank)
  vizCode <- paste(vizLines, collapse="\n")
  aes.list <- lapply(meta$geoms, "[[", "aes")
  i.aes <- c("clickSelects", "showSelected", "showSelected2", "showSelected3")
  ivars <- sapply(aes.list, "[", i.aes)
  uniq <- function(x){
    y <- unique(as.vector(unlist(x)))
    not.na <- !is.na(y)
    not.trivial <- !y %in% c("x", "y", "label", "xmin", "xmax", "")
    not.derived <- !grepl("interaction", y)
    y[not.na & not.trivial & not.derived]
  }
  uivars <- uniq(ivars)
  uvars <- uniq(aes.list)
  n.interactive[[sub.dir.i]] <- length(uivars)
  n.vars[[sub.dir.i]] <- length(uvars)
  name.interactive[[sub.dir.i]] <- paste(uivars, collapse=", ")
  name.vars[[sub.dir.i]] <- paste(uvars, collapse=", ")
  if(!is.null(meta$time)){
    animated[[sub.dir.i]] <- "yes"
  }
}
tab <- data.frame(LOC=code.lines,
                  plots=as.integer(n.plots),
                  rows=as.integer(n.rows),
                  onscreen=as.integer(n.onscreen),
                  ##"rows/onscreen"=n.rows/n.onscreen,
                  variables=n.vars,
                  interactive=n.interactive,
                  "animated?"=animated,
                  seconds=round(seconds, 1),
                  ##comments=comment.lines,
                  check.names=FALSE,
                  row.names=base)
tab <- tab[order(tab$LOC, decreasing=FALSE),]
tab$Fig <- NA
tab[c("WorldBank", "tornado", "climate"), "Fig"] <- c(1L, 2L, 3L)
print(tab)
xt <- xtable(tab, digits=1)
print(xt, file="table-examples.tex", floating=FALSE)
