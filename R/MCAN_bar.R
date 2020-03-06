# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


MCAN_bar <- function(data,data.norm,width=14,height=7,xaxt = 'n',xlab='Sample',ylab='Relative amount') {
    pdf("normalize.pdf",width,height)
    library(cowplot)
    par(mfrow=c(1,2))
    boxplot(data, outline=F,xaxt='n',xlab='Sample',ylab='Relative amount')
    mtext("Before Normalization",side = 3)
    mtext("A.",side = 3,line=1,at=-18,cex=1.5)
    boxplot(as.data.frame(data.norm), outline=F,xaxt = 'n',xlab='Sample')
    mtext("After Normalization",side = 3)
    mtext("B.",side = 3,line=1,at=-28,cex=1.5)
    dev.off()
}
