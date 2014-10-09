load("~/R/animint-examples/data/scaffolds.RData")

scaffolds$nonzero.boxes$inserts.rank <- rank(scaffolds$nonzero.boxes$inserts)
n.rank.entries <- 3
to.show <- as.integer(seq(1, nrow(scaffolds$nonzero.boxes), l=n.rank.entries))
rank.entries <- scaffolds$nonzero.boxes$inserts.rank[to.show]
names(rank.entries) <- scaffolds$nonzero.boxes$inserts[to.show]

box.bp <- subset(scaffolds$nonzero.boxes, box.chromStart==1)$box.chromEnd[1]-1

sample.name <- "T268"

n.square <- max(scaffolds$square$row)

insert.colors <- c(#None="#E41A1C", #Red
                   None="#A65628", #brown
                   Human="#377EB8", #blue
                   Virus="#4DAF4A", #green
                   Other="#F781BF") #pink
type.colors <- c(Discordant="#E41A1C", #Red
                 Mapped="#377EB8", #blue
                 Orphan="#377EB8", 
                 OEA="#4DAF4A", #green
                 "OEA BQ"="#984EA3", #purple
                 ## Orphan="#FF7F00", #orange
                 ## Mapped="#FF7F00",
                 "#FFFF33", #yellow
                 scOEA="#A65628", #brown
                 BadQ="#F781BF", #pink
                 sclip="#999999", #grey
                 Sclip="#999999")
viz <-
  list(chroms=ggplot()+
       theme_animint(width=800, height=300)+
       scale_color_manual(values=insert.colors)+
       theme(axis.line.y=element_blank(),
             axis.text.y=element_blank(), 
             axis.ticks.y=element_blank())+
       ylab("chromosome")+
       xlab("Position on chromosome (mega base pairs)") +
       geom_text(aes(x=120, y=25, color=insert.type,
                     label=scaffold.name,
                     showSelected=insert),
                 data=scaffolds$other.counts)+
       geom_text(aes(x=200, y=18,
                     showSelected=insert,
                     color=insert.type,
                     label=paste(insert.type, "scaffold", ID_SCA,
                       "has", others,
                       ifelse(others==1, "insert", "inserts"))),
                 data=scaffolds$other.counts)+
       geom_rect(aes(xmin=box.chromStart/1e6, xmax=box.chromEnd/1e6,
                     ymin=chr.int-1/2, ymax=chr.int+1/2,
                     fill=inserts.rank,
                     clickSelects=box.id),
                 data=scaffolds$nonzero.boxes)+
       geom_text(aes(bases/1e6 + 5, chr.int-1/2, label=chr.fac),
                 size=10, data=scaffolds$chroms)+
       ## TODO: on compile, assign insert.show to the insert selector.
       geom_point(aes(REF_START/1e6, chr.int,
                      showSelected=insert.show,
                      clickSelects=insert),
                  data=scaffolds$other.inserts)+
       ggtitle("selected insert and others on scaffold")+
       scale_fill_continuous("inserts",
                             low="grey90",
                             high="grey70",
                             breaks=rank.entries,
                             labels=names(rank.entries)),
       
       box=ggplot()+
       xlab(sprintf("position in %.0fkbp box (kilo base pairs)", box.bp/1e3))+
       ggtitle("Zoom to selected chromosomal box")+
       geom_point(aes(box.chromStart/1e3,
                      log10(SCORE+5),
                      color=insert.type,
                      showSelected=box.id,
                      showSelected2=insert.show),
                  size=10, data=scaffolds$other.inserts)+
       geom_text(aes(box.bp/1e3/2, max(log10(scaffolds$box.inserts$SCORE+5)),
                     label=sprintf("%d %s in box on %s",
                       inserts,
                       ifelse(inserts==1, "insert", "inserts"),
                       chrom),
                     showSelected=box.id),
                 data=scaffolds$nonzero.boxes)+
       geom_point(aes(box.chromStart/1e3,
                      log10(SCORE+5),
                      fill=insert.type,
                      showSelected=box.id,
                      clickSelects=insert),
                  data=scaffolds$box.inserts, color="black", size=4)+
       geom_text(aes(box.chromStart/1e3,
                     log10(SCORE+5),
                     label=sprintf("SCORE=%d scaffold=%dbp",
                       SCORE, scaffold.length),
                     showSelected=insert,
                     showSelected2=box.id),
                 data=scaffolds$box.inserts)+
       scale_fill_manual(values=insert.colors)+
       scale_color_manual(values=insert.colors),

       insertSquare=ggplot()+
       ggtitle(paste("all inserts for sample", sample.name))+
       ylab(sprintf("%d rows/cols, %d inserts",
                    n.square, nrow(scaffolds$square)))+
       xlab("ordered by type and inserts per scaffold")+
       theme(axis.line=element_blank(),
             axis.text=element_blank(),
             axis.ticks=element_blank())+
       scale_color_manual(values=insert.colors)+
       geom_segment(aes(row+1/2, col-1/2,
                        xend=row+1/2, yend=col+1/2),
                    data=scaffolds$square.segments, size=1, color="grey")+
       geom_point(aes(row, col, color=insert.type,
                      showSelected=insert.show),
                  data=scaffolds$other.inserts, alpha=1/4)+
       geom_point(aes(row, col, color=insert.type,
                      clickSelects=insert),
                  data=scaffolds$square, alpha=3/4))
