#!/usr/bin/env Rscript

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

library(ggplot2)
digits = 2


# accuracy
accuracy <- function (tp, fn, tn, fp) {

 accuracy_result <- (tp+tn)/(tn + tp + fp + fn)
 
 cat("accuracy = ", round(accuracy_result, digits), "\t\t in the [0; 1] interval \n")
 return(accuracy_result)
}


# F1 score
f1_score <- function (tp, fn, tn, fp) {

 f1_score_result <- (2*tp)/(2*tp + fp + fn)
 
 cat("F1 score = ", round(f1_score_result, digits), "\t\t in the [0; 1] interval\n")
 return(f1_score_result)
}


# Matthews correlation coefficient
mcc <- function (tp, fn, tn, fp) {

  TP <- tp
  FN <- fn
  
  TN <- tn
  FP <- fp
  
  #TP;TN;FP;FN # for debugging
  sum1 <- TP+FP; sum2 <-TP+FN ; sum3 <-TN+FP ; sum4 <- TN+FN;
  denom <- as.double(sum1)*sum2*sum3*sum4 # as.double to avoid overflow error on large products
  if (any(sum1==0, sum2==0, sum3==0, sum4==0)) {
    denom <- 1
  }
  mcc <- ((TP*TN)-(FP*FN)) / sqrt(denom)
  
  cat("MCC = ", round(mcc, digits), "\t\t\t in the [-1; +1] interval\n")
  return(mcc)
}



# Confusion matrix pie
confusion_matrix_pie <- function(tp, fn, tn, fp, saveFileName) {

    thisDafaFrame <- data.frame(
      category = c(paste("TP = ", tp, ""), paste("FN = ", fn, ""), paste("TN = ", tn, ""), paste("FP = ", fp, "")),
      amount = c(tp, fn,   tn, fp)
      )
    thisDafaFrame$category = factor(thisDafaFrame$category, levels=unique(thisDafaFrame$category)) # we set as levels to respect this order in the plot legend
    head(thisDafaFrame)

    # Barplot
    thisBarPlot<- ggplot(thisDafaFrame, aes(x="", y=amount, fill=category))+
    geom_bar(width = 1.0, stat = "identity")

    thisPie <- thisBarPlot + coord_polar("y") + xlab("") + ylab("") + scale_fill_brewer(palette="RdBu")
    thisPie
    ggsave(saveFileName)

}

# positive_negative pie
positive_negative_pie <- function(pos, neg, saveFileName) {

    thisDafaFrame <- data.frame(
      category = c(paste("positives = ", pos, ""), paste("negatives = ", neg, "")),
      amount = c(pos, neg)
      )
    thisDafaFrame$category = factor(thisDafaFrame$category, levels=unique(thisDafaFrame$category)) # we set as levels to respect this order in the plot legend
    head(thisDafaFrame)

    # Barplot
    thisBarPlot<- ggplot(thisDafaFrame, aes(x="", y=amount, fill=category))+
    geom_bar(width = 1.0, stat = "identity")
    
    thisPie <- thisBarPlot + coord_polar("y") + xlab("") + ylab("") + scale_fill_brewer(palette="RdBu")
    thisPie
    ggsave(saveFileName)

}



tp = 47
fn = 3

tn = 5
fp = 45

positives = tp + fn
negatives = tn + fp

confusion_matrix_pie(tp, fn, tn, fp, "confusion_matrix_pie.pdf")
positive_negative_pie(positives, negatives, "positive_negative_pie.pdf")

current_accuracy = accuracy(tp, fn, tn, fp)
current_f1_score = f1_score(tp, fn, tn, fp)
current_MCC = mcc(tp, fn, tn, fp)

current_normMCC = (current_MCC+1)/2 # normalized MCC
cat("normMCC = ", round(current_normMCC, digits), "\t\t in the [0; 1] interval\n")



if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
