load("~/R/animint-examples/data/chip.seq.RData")

hgTracks <- "http://ucscbrowser.genap.ca/cgi-bin/hgTracks"
## Some constants for the labels.
ann.colors <- c("noDifference"='#f6f4bf',
                "difference"="#ff7d7d")
fp.fn.map <- c(false.positive="noDifference",
               false.negative="difference")
fp.fn.colors <- ann.colors[fp.fn.map]
names(fp.fn.colors) <- names(fp.fn.map)
total.vars <- 168
set.linetypes <- c(validation="dotted", train="solid")
## nonzero pair errors, for dots on the select sample plot.
pair.errors <-
  rbind(data.frame(chip.seq$fp.pairs, error.type="false.positive"),
        data.frame(chip.seq$fn.pairs, error.type="false.negative"))
nonzero.pairs <- subset(pair.errors, errors > 0)
nonzero.pairs$error.type <-
  factor(nonzero.pairs$error.type, c("false.positive", "false.negative"))
## false positive/negative rates for the error/model complexity plot.
fp.fn <-
  rbind(data.frame(chip.seq$fp.set, error.type="false.positive"),
        data.frame(chip.seq$fn.set, error.type="false.negative"))
fp.fn$error.type <-
  factor(fp.fn$error.type, c("false.positive", "false.negative"))
fp.fn.nonzero <- subset(fp.fn, errors > 0)
## Combined probability/signal plot.
chip.seq$bases$facet <- "probability"
fp.fn.pairs <- with(chip.seq, {
  rbind(data.frame(fp.pairs, error.type="false positive"),
        data.frame(fn.pairs, error.type="false negative"))
})
fp.fn.pairs$facet <- "probability"
chip.seq$probability$facet <- "probability"
fp.fn.set <- with(chip.seq, {
  rbind(data.frame(fp.set, error.type="false positive", y=40),
        data.frame(fn.set, error.type="false negative", y=33))
})
fp.fn.set$label <- with(fp.fn.set, {
  sprintf("%d/%d=%.1f%% %s bases",
          errors, bases, percent, error.type)
})
facet.data <- chip.seq
for(data.name in c("signal.max", "signal.segments")){
  for(selector.name in c("sample1", "sample2")){
    facet.data[[data.name]][[selector.name]]$facet <- "signal"
  }
}
viz <-
  list(chroms=ggplot()+
       theme_animint(width=250, height=220)+
       geom_segment(aes(0, chr.int, xend=bases/1e6, yend=chr.int),
                    data=chip.seq$chroms, color="grey")+
       geom_text(aes(0, chr.int, label=paste0("chr", chr)),
                 data=chip.seq$set.info, hjust=1)+
       ggtitle("select annotated set")+
       guides(color="none")+
       xlim(-25, 250)+
       xlab("position on chromosome (mega base pairs)")+
       theme(axis.line.y=element_blank(),
             axis.text.y=element_blank(),
             axis.title.y=element_blank(),
             axis.ticks.y=element_blank())+
       geom_point(aes((chromEnd+chromStart)/2/1e6, chr.int, 
                      clickSelects=set.name),
                  data=chip.seq$set.info, size=5),
       
       samples=ggplot()+
       scale_x_continuous("false positive/negative rate (percent)",
                          breaks=c(0, 50, 100), limits=c(-250, 100))+
       theme_animint(width=300, height=220)+
       scale_fill_manual(values=fp.fn.colors)+
       ggtitle("select samples")+
       ylab("sample")+
       theme(axis.line.y=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank()),
       
       error=ggplot()+
       theme_animint(height=220)+
       ggtitle("select model complexity and set")+
       xlab("model complexity -log(lambda)")+
       ylab("incorrect/total annotations (percent)")+
       geom_vline(aes(xintercept=complexity,
                      clickSelects=complexity.i),
                  data=chip.seq$models, size=15, alpha=1/2)+
       scale_color_manual(values=fp.fn.colors)+
       geom_line(aes(complexity, percent.error,
                     linetype=set.name, group=set.name,
                     clickSelects=set.name),
                 data=chip.seq$error, size=5, alpha=3/4)+
       scale_linetype_manual(values=set.linetypes)+
       geom_line(aes(complexity, percent/2, 
                     showSelected=set.name, group=error.type),
                 data=fp.fn, size=4.5, color="grey")+
       geom_line(aes(complexity, percent/2, color=error.type, size=error.type,
                     showSelected=set.name, group=error.type),
                 data=fp.fn)+
       scale_size_manual(values=c(false.negative=1.5, false.positive=3.5))+
       geom_text(aes(6, y, label=label,
                     showSelected=complexity.i,
                     showSelected4=set.name),
                 data=fp.fn.set)+
       geom_text(aes(6, 26,
                     label=sprintf("%d/%d nonzero coefficients",
                       variables, total.vars),
                     showSelected=complexity.i),
                 data=chip.seq$nonzero),

       roc=ggplot()+
       theme_animint(width=200, height=220)+
       scale_linetype_manual(values=set.linetypes)+
       guides(size="none", linetype="none")+
       geom_path(aes(FPR, TPR, group=set.name,
                     linetype=set.name, size=set.name,
                     showSelected=complexity.i, clickSelects=set.name),
                 data=chip.seq$roc.curves)+
       geom_point(aes(FPR, TPR, showSelected=complexity.i,
                      clickSelects=set.name),
                  data=chip.seq$roc.points, color="violet", size=5)+
       scale_size_manual(values=c(train=3, validation=5))+
       ggtitle("ROC curves")+
       xlab("False positive rate")+
       ylab("True positive rate"),
       
       probSignal=ggplot()+
       theme_animint(width=1300, height=400)+
       ggtitle("ChIP-seq signal pair and learned difference function")+
       theme(axis.line.x=element_blank(),
             axis.text.x=element_blank(),
             axis.title.x=element_blank(),
             axis.ticks.x=element_blank())+
       geom_rect(aes(xmin=min.norm, xmax=max.norm,
                     ymin=ymin, ymax=1,
                     fill=annotation,
                     showSelected=set.name),
                     data=rbind(data.frame(chip.seq$regions,
                       ymin=0, facet="probability"),
                       data.frame(chip.seq$regions,
                                  ymin=-1, facet="signal")))+
       geom_text(aes(0.5, 0.55, label=paste0(width.bp, " bases on chr", chr),
                     href=sprintf("%s?db=hg19&position=chr%s:%d-%d",
                       hgTracks, chr,
                       as.integer(first.base), as.integer(last.base)),
                     showSelected=set.name),
                 data=chip.seq$bases)+
       geom_text(aes(0, 0.55, label=first.base,
                     showSelected=set.name),
                 data=chip.seq$bases, hjust=0)+
       geom_text(aes(1, 0.55, label=last.base,
                     showSelected=set.name),
                 data=chip.seq$bases, hjust=1)+
       scale_fill_manual(values=ann.colors)+
       geom_text(aes(mid.norm, 1.05,
                     label=sprintf("%d/%d=%.1f%% %s bases",
                       errors, bases, percent, error.type),
                     showSelected=complexity.i,
                     showSelected2=sample1,
                     showSelected3=sample2,
                     showSelected4=set.name),
                 data=fp.fn.pairs)+
       ylab("scaled signal ------ probability of difference")+
       geom_text(aes(0, 1, label=sprintf("%s %s max=%.1f",
                             cell.type, sample1, max),
                     showSelected=set.name,
                     showSelected2=sample1),
                 data=data.frame(chip.seq$signal.max$sample1,
                   facet="signal"), hjust=0)+
       geom_rect(aes(xmin=normStart, xmax=normEnd,
                     ymin=0, ymax=signal.norm,
                     showSelected=set.name,
                     showSelected2=sample1),
                 data=data.frame(chip.seq$signal.segments$sample1,
                   facet="signal"), size=0)+
       geom_text(aes(0, -1, label=sprintf("%s %s max=%.1f",
                             cell.type, sample2, max),
                     showSelected=set.name,
                     showSelected2=sample2),
                 data=data.frame(chip.seq$signal.max$sample2,
                   facet="signal"), hjust=0)+
       geom_rect(aes(xmin=normStart, xmax=normEnd,
                     ymin=-signal.norm, ymax=0,
                     showSelected=set.name,
                     showSelected2=sample2),
                 data=data.frame(chip.seq$signal.segments$sample2,
                   facet="signal"), size=0)+
       geom_hline(aes(yintercept=y),
                  data=data.frame(y=0, facet="signal"),
                  color="white")+
       geom_hline(aes(yintercept=y),
                  data=data.frame(y=1/2, facet="probability"),
                  color="grey")+
       facet_grid(facet ~ ., scales="free_y")+
       theme(panel.margin=grid::unit(0, "cm"))+
       geom_ribbon(aes(mid.norm, ymin=min.prob, ymax=max.prob,
                       showSelected=sample1,
                       showSelected2=sample2,
                       showSelected3=complexity.i,
                       showSelected4=set.name),
                   chunk_vars=c("sample1","sample2","complexity.i","set.name"),
                   data=chip.seq$probability, color="blue"),
       
       title=paste("L1-regularized logistic regression model",
         "predicts differences between ChIP-seq profiles"),
       
       duration=list(complexity.i=2000))
for(selector.name in names(chip.seq$samples)){
  sample.df <- chip.seq$samples[[selector.name]]
  sample.df$x <- -5
  y.fact <- if(selector.name=="sample1")-1 else 1
  other.name <- if(selector.name=="sample1")"sample2" else "sample1"
  sample.df$y <- sample.df$y * y.fact
  sample.id <- sample.df[[selector.name]]
  sample.df$label <- paste(sample.df$cell.type, sample.id)
  rownames(sample.df) <- sample.id
  nonzero.names <- as.character(nonzero.pairs[[selector.name]])
  nonzero.pairs$y <- sample.df[nonzero.names, "y"]
  sample.df$xmin <- 0
  sample.df$xmax <- 100
  sample.df$ymin <- sample.df$y-1/2
  sample.df$ymax <- sample.df$y+1/2
  text.df <- sample.df
  text.df$y <- text.df$y-1/2
  nonzero.pairs$key <- with(nonzero.pairs, {
    paste(sample1, sample2, error.type)
  })
  viz$samples <- viz$samples+
    geom_text(aes_string(clickSelects=selector.name, x="x", y="y",
                         showSelected="set.name", label="label"),
              data=text.df, hjust=1)+
    geom_rect(aes_string(xmin="xmin", xmax="xmax", ymin="ymin", ymax="ymax",
                         showSelected="set.name",
                         clickSelects=selector.name),
              data=sample.df, alpha=1/2)+
    geom_point(aes_string(clickSelects=other.name, x="percent", y="y",
                          key="key",
                          showSelected="set.name", showSelected2="complexity.i",
                          fill="error.type"),
               data=nonzero.pairs, color="black", size=3, alpha=0.55)
}##animint2gist(viz, "chip-seq-aligned")
