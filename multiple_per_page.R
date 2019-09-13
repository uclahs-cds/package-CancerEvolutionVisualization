pdf("test.pdf", height=6, width=6, onefile=FALSE)
grid.newpage()
lay0 <- grid.layout(2,2,heights=unit(c(4,2),c("in","in")),widths=unit(c(3,3),c("in","in")))
vp0 <- viewport(layout=lay0,name="top.vp",height=unit(3.4,"in"),width=unit(6,"in"))
pushViewport(vp0)

pushViewport(viewport(layout.pos.row=1,layout.pos.col=1,name="vp1", yscale=c(0,1),just=c("centre","top")))
grid.rect(gp=gpar(fill="pink"))
inputs <- prep.tree(samp="RB5662S",trees=trees,cnas=genes,pga=pga,axis.type="both")

vp = "vp1"
draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename="test.pdf",scale2=0.5/362,wid=60,extra.len=20,len=10,
             scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type=inputs$axis.type,rad=3.2,gene.cex=1.5,
             add.genes=inputs$add.genes,samp_name="RB-5662",circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=1, axis.space.right=1)
upViewport()
pushViewport(viewport(layout.pos.row=1,layout.pos.col=2,name="vp2", just=c("centre","top")))
grid.rect(gp=gpar(fill="lightblue"))
inputs <- prep.tree(samp="KP2",trees=trees,cnas=genes,pga=pga,axis.type="both")

vp = "vp2"
draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename="test.pdf",scale2=0.5/362,wid=60,extra.len=20,len=10,
             scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type=inputs$axis.type,rad=3.2,gene.cex=1.5,
             add.genes=inputs$add.genes,samp_name="RB-5662",circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=1, axis.space.right=1)
dev.off()