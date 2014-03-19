works_with_R("3.0.2", animint="2014.3.19", xtable="1.7.3",
             microbenchmark="1.3.0")
viz.path <- Sys.glob(file.path("examples", "*", "viz.R"))
sub.dirs <- dirname(viz.path)
base <- basename(sub.dirs)
n.vars <- n.interactive <- name.vars <- name.interactive <- code.lines <-
  comment.lines <- 
  rep(NA, length(base))
animated <- rep("no", length(base))
for(sub.dir.i in seq_along(sub.dirs)){
  sub.dir <- sub.dirs[[sub.dir.i]]
  cat(sprintf("%4d / %4d %s\n", sub.dir.i, length(sub.dirs), sub.dir))
  viz.f <- file.path(sub.dir, "viz.R")
  source(viz.f)
  ## TODO: microbenchmark this!!
  meta <- gg2animint(viz, open.browser=FALSE)
  vizLines <- readLines(viz.f)
  no.comments <- sub("#.*", "", vizLines)
  no.whitespace <- sub(" ", "", no.comments)
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
tab <- data.frame(data=base,
                  vars=n.vars,
                  int=n.interactive,
                  anim=animated,
                  LOC=code.lines,
                  ##comments=comment.lines,
                  row.names=base)
tab <- tab[order(tab$LOC, decreasing=FALSE),]
tab$figure <- NA
tab$seconds <- NA
tab[c("WorldBank", "tornado", "climate"), "figure"] <- c(1L, 2L, 3L)
xt <- xtable(tab)
print(xt, file="table-examples.tex", include.rownames=FALSE, floating=FALSE)
