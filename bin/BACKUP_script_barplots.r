if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

library(ggplot2)
digits = 2

# Confusion matrix barplot
barplot_creator <- function(inputText, saveFileName) {

    confusionMatrixDataFrame <- read.table(header=TRUE, text=inputText)
  
    confusionMatrixDataFrame$category = factor(confusionMatrixDataFrame$category, levels=unique(confusionMatrixDataFrame$category))

    p <- ggplot(confusionMatrixDataFrame, aes(x=category, y=amount)) + geom_bar(aes(fill=category), stat="identity", position=position_dodge()) + ylim(0,100)
    p + scale_colour_brewer(palette="RdBu")
    p
    ggsave(saveFileName)

}


# Confusion matrix barplot
confusion_matrix_barplot <- function(tp, fn, tn, fp, saveFileName) {

    thisText <- paste("\n category amount\n  TP ", tp,"\n  FN ", fn,"\n\n  TN ", tn,"\n  FP ", fp,"\n", sep="")

    # confusionMatrixDataFrame <- read.table(header=TRUE, text='
    #  category amount
    #   TP 47
    #   FN 3
    # 
    #   TN 5
    #   FP 45
    # ')

    barplot_creator(thisText, saveFileName)

}

# Positives negatives barplot
positives_negatives_barplot <- function(pos, neg, saveFileName) {

    thisText <- paste("\n category amount\n  positives ", pos,"\n  negatives ", neg,"\n", sep="")

    barplot_creator(thisText, saveFileName)

}


tp = 47
fn = 3

tn = 5
fp = 45

pos = tp + fn
neg = tn + fp

confusion_matrix_barplot(tp, fn, tn, fp, "confusion_matrix_barplot.pdf")

positives_negatives_barplot(pos, neg, "positives_negatives_barplot.pdf")



if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")