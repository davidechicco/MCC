#!/usr/bin/env Rscript

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

library(grid)
library(gridExtra)
library(gtable)
library(ggplot2)
library(cowplot)

SAVE_FILES = FALSE

SAVE_GENERAL_PLOT = FALSE

digits = 2

generalTextSize = 12

# Barplot matrix barplot
barplot_creator <- function(inputText, saveFileName, upperYlim) {

    confusionMatrixDataFrame <- read.table(header=TRUE, text=inputText)

    confusionMatrixDataFrame$category = gsub("_", " ", confusionMatrixDataFrame$category)
    confusionMatrixDataFrame$category = gsub("=", " = ", confusionMatrixDataFrame$category)
    confusionMatrixDataFrame$category = factor(confusionMatrixDataFrame$category, levels=unique(confusionMatrixDataFrame$category))


    p <- ggplot(confusionMatrixDataFrame, aes(x=category, y=amount)) + geom_bar(aes(fill=category), stat="identity", position=position_dodge()) + ylim(0,upperYlim) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.title=element_blank(), legend.text=element_text(size=generalTextSize), text = element_text(size=generalTextSize), axis.text.x = element_text(size=generalTextSize), axis.text.y = element_text(size=generalTextSize))
    p + scale_colour_brewer(palette="RdBu", name="") + xlab("") + ylab("") 
    # + guides(colour=guide_legend(title=""))
    p
    
    if (SAVE_FILES==TRUE) {
        ggsave(saveFileName)
    }
  
    return(p)
}


# Three elements barplot
accuracy_f1score_mcc_barplot <- function(accuracy, f1_score, normMCC, saveFileName) {

    thisText <- paste("\n category amount\n  accuracy=",accuracy, " ", accuracy,"\n  F1_score=", f1_score, " ", f1_score,"\n\n  normMCC=", normMCC," ", normMCC,"\n", sep="")
    
    
    upperYlim = 1.0
    return(barplot_creator(thisText, saveFileName, upperYlim))

}


# Confusion matrix barplot
confusion_matrix_barplot <- function(tp, fn, tn, fp, saveFileName) {

    thisText <- paste("\n category amount\n  TP =",tp," ", tp,"\n  FN= ", fn," ", fn,"\n\n  TN =",tn," ", tn,"\n  FP =", fp," ", fp,"\n", sep="")

    # confusionMatrixDataFrame <- read.table(header=TRUE, text='
    #  category amount
    #   TP 47
    #   FN 3
    # 
    #   TN 5
    #   FP 45
    # ')

    upperYlim = 100
    return(barplot_creator(thisText, saveFileName, upperYlim))

}

# Positives negatives barplot
positives_negatives_barplot <- function(pos, neg, saveFileName) {

    thisText <- paste("\n category amount\n  positives =",pos, " ", pos,"\n  negatives =", neg, " ", neg,"\n", sep="")

    upperYlim = 100
    return(barplot_creator(thisText, saveFileName, upperYlim))

}



# accuracy
accuracy <- function (tp, fn, tn, fp) {

 accuracy_result <- (tp+tn)/(tn + tp + fp + fn)
 
 accuracy_result <- round(accuracy_result, digits)
 
 cat("accuracy = ", round(accuracy_result, digits), "\t\t in the [0; 1] interval \n")
 return(accuracy_result)
}


# F1 score
f1_score <- function (tp, fn, tn, fp) {

 f1_score_result <- (2*tp)/(2*tp + fp + fn)
 
 f1_score_result <- round(f1_score_result, digits)
 
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
  
  mcc <- round(mcc, digits)
  
  cat("MCC = ", round(mcc, digits), "\t\t\t in the [-1; +1] interval\n")
  return(mcc)
}



# Confusion matrix pie
confusion_matrix_pie <- function(tp, fn, tn, fp, saveFileName) {

    thisDataFrame <- data.frame(
      category = c(paste("TP =", tp, ""), paste("FN =", fn, ""), paste("TN =", tn, ""), paste("FP =", fp, "")),
      amount = c(tp, fn,   tn, fp)
      )
    thisDataFrame$category = factor(thisDataFrame$category, levels=unique(thisDataFrame$category)) # we set as levels to respect this order in the plot legend
    head(thisDataFrame)

    # Barplot
    thisBarPlot<- ggplot(thisDataFrame, aes(x="", y=amount, fill=category))+
    geom_bar(width = 1.0, stat = "identity") + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.title=element_blank(), legend.text=element_text(size=generalTextSize), text = element_text(size=generalTextSize), axis.text.x = element_text(size=generalTextSize))

    thisPie <- thisBarPlot + coord_polar("y") + xlab("") + ylab("") + scale_fill_brewer(palette="RdBu")
    thisPie + theme(legend.text=element_text(size=generalTextSize))
    thisPie
    
    if (SAVE_FILES==TRUE) {
        ggsave(saveFileName)
    }
    
    return(thisPie)

}

# positive_negative pie
positive_negative_pie <- function(pos, neg, saveFileName) {

    thisDataFrame <- data.frame(
      category = c(paste("positives =", pos, ""), paste("negatives =", neg, "")),
      amount = c(pos, neg)
      )
    thisDataFrame$category = factor(thisDataFrame$category, levels=unique(thisDataFrame$category)) # we set as levels to respect this order in the plot legend
    head(thisDataFrame)

    # Barplot
    thisBarPlot<- ggplot(thisDataFrame, aes(x="", y=amount, fill=category))+
    geom_bar(width = 1.0, stat = "identity") + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.title=element_blank(), legend.text=element_text(size=generalTextSize), text = element_text(size=generalTextSize), axis.text.x = element_text(size=generalTextSize))
    
    thisPie <- thisBarPlot + coord_polar("y") + xlab("") + ylab("") + scale_fill_brewer(palette="RdBu")
    thisPie + theme(legend.text=element_text(size=generalTextSize))
    thisPie
    if (SAVE_FILES==TRUE) {
        ggsave(saveFileName)
    }

    return(thisPie)
}


#
# Function that generates all the plot and their files
#
generate_all_the_plots <- function(tp, fn, tn, fp, addTitle) {

    randomValue = sample(1:10000000, 1)
    #test_title = paste(addTitle, "_test",randomValue,"__TP",tp,"_FN",fn,"_TN",tn,"_FP",fp,  sep="")
    test_title = paste(addTitle, "_test",randomValue, sep="")

    print(test_title)
    
    positives = tp + fn
    negatives = tn + fp

    confusion_matrix_pie_file_name = paste("../plots/", test_title, "_confusion_matrix_pie.pdf", sep="")
    plot_cf <- confusion_matrix_pie(tp, fn, tn, fp, confusion_matrix_pie_file_name)
    positive_negative_pie_file_name = paste("../plots/", test_title, "_positive_negative_pie.pdf", sep="")
    plot_pos_neg<-positive_negative_pie(positives, negatives, positive_negative_pie_file_name)

    current_accuracy = accuracy(tp, fn, tn, fp)
    current_f1_score = f1_score(tp, fn, tn, fp)
    current_MCC = mcc(tp, fn, tn, fp)

    current_normMCC = (current_MCC+1)/2 # normalized MCC
    current_normMCC = round(current_normMCC, digits)
    cat("normMCC = ", round(current_normMCC, digits), "\t\t in the [0; 1] interval\n\n")

    cat(positives, " & ", negatives, " & ", tp, " & ", fn, " & ", tn, " & ", fp, " & ", current_accuracy, " & ", current_f1_score, " & ", current_MCC, "\n\n")
    
    
    confusion_matrix_barplot_file_name = paste("../plots/", test_title, "_confusion_matrix_barplot.pdf", sep="")
    # confusion_matrix_barplot(tp, fn, tn, fp, confusion_matrix_barplot_file_name)
    positives_negatives_barplot_file_name = paste("../plots/", test_title, "_positives_negatives_barplot.pdf", sep="")
    # positives_negatives_barplot(positives, negatives, positives_negatives_barplot_file_name)

    accuracy_f1score_mcc_barplot_file_name = paste("../plots/", test_title, "_accuracy_f1score_mcc.pdf", sep="")
    plot_scores <- accuracy_f1score_mcc_barplot(current_accuracy, current_f1_score, current_normMCC, accuracy_f1score_mcc_barplot_file_name)


    ## Side by side
    right_col_plot = plot_grid(plot_cf, plot_pos_neg, labels=c("b", "c"), align="v", ncol=1)
    general_plot = plot_grid(plot_scores, right_col_plot, labels=c("a", ""),  ncol = 2, rel_widths=c(1.3, 1))

    #theme_set(theme_cowplot(font_size=12))
    # general_plot <- plot_grid(plot_cf, plot_pos_neg, plot_scores, ncol = 2, align="h", labels=c("A", "B", "C"), label_size=5)
    
    # general_plot <- plot_grid(plot_cf, plot_pos_neg, plot_scores, ncol = 3, align="h", labels=c("A", "B", "C"), label_size=5)
    
    general_file = paste("../plots/", test_title, "_general.pdf", sep="")
    
    if (SAVE_GENERAL_PLOT) {
      ggsave(general_file, general_plot, height = 8, width = 12, units = "in", dpi = 150)
    }
    
    #save_plot("../plots/general.pdf", general_plot)
        
    }

#	    tp, fn,   tn, fp
exampleA1 = c(90, 1,   0, 9)	# good on TP, bad on TN: A1_case_pos_imb_yesTP_noTN
exampleA2 = c(5, 70,   19, 6)	# good on TP, bad on TN: A2_case_pos_imb_noTP_yesTN

exampleB1 = c(47, 3,   5, 45)	# good on TP, bad on TN: B1_case_bal_yesTP_noTN
exampleB2 = c(10, 40, 46, 4)	# bad on TP, good on TN: B2_case_bal_noTP_yes_TN

exampleC1 = c(9, 1,    1, 89)	# bad on TP, good on TN: C1_case_neg_imb_yesTP_noTN
exampleC2 = c(2, 9,    88, 1)	# bad on TP, good on TN: C2_case_neg_imb_noTP_yesTN

# selected_example <- exampleA1
# addTitle = "A1_case_pos_imb_yesTP_noTN"
# generate_all_the_plots(selected_example[1], selected_example[2], selected_example[3], selected_example[4], addTitle)
# 
# selected_example <- exampleA2
# addTitle = "A2_case_pos_imb_noTP_yesTN"
# generate_all_the_plots(selected_example[1], selected_example[2], selected_example[3], selected_example[4], addTitle)
# 
# 
# selected_example <- exampleB1
# addTitle = "B1_case_bal_yesTP_noTN"
# generate_all_the_plots(selected_example[1], selected_example[2], selected_example[3], selected_example[4], addTitle)
# 
# selected_example <- exampleB2
# addTitle = "B2_case_bal_noTP_yes_TN"
# generate_all_the_plots(selected_example[1], selected_example[2], selected_example[3], selected_example[4], addTitle)
# 
# 
# selected_example <- exampleC1
# addTitle = "C1_case_neg_imb_yesTP_noTN"
# generate_all_the_plots(selected_example[1], selected_example[2], selected_example[3], selected_example[4], addTitle)
# 
# selected_example <- exampleC2
# addTitle = "C2_case_neg_imb_noTP_yesTN"
# generate_all_the_plots(selected_example[1], selected_example[2], selected_example[3], selected_example[4], addTitle)

#	    tp, fn,   tn, fp
exampleA3 = c(90, 1,   9, 1)	# good on TP, bad on TN: A3_case_pos_imb_yesTP_yesTN
exampleA4 = c(5, 70,   6,19)	# good on TP, bad on TN: A4_case_pos_imb_noTP_noTN

exampleB3 = c(47, 3,   45, 5)	# good on TP, bad on TN: B3_case_bal_yesTP_yesTN
exampleB4 = c(10, 40, 4, 46)	# bad on TP, good on TN: B4_case_bal_noTP_no_TN

exampleC3 = c(9, 1,    89, 1)	# bad on TP, good on TN: C3_case_neg_imb_yesTP_yesTN
exampleC4 = c(2, 9,    1, 88)	# bad on TP, good on TN: C4_case_neg_imb_noTP_noTN

selected_example <- exampleA3
addTitle = "A3_case_pos_imb_yesTP_yesTN"
generate_all_the_plots(selected_example[1], selected_example[2], selected_example[3], selected_example[4], addTitle)

selected_example <- exampleA4
addTitle = "A4_case_pos_imb_noTP_noTN"
generate_all_the_plots(selected_example[1], selected_example[2], selected_example[3], selected_example[4], addTitle)

selected_example <- exampleB3
addTitle = "B3_case_bal_yesTP_yesTN"
generate_all_the_plots(selected_example[1], selected_example[2], selected_example[3], selected_example[4], addTitle)

selected_example <- exampleB4
addTitle = "B4_case_bal_noTP_no_TN"
generate_all_the_plots(selected_example[1], selected_example[2], selected_example[3], selected_example[4], addTitle)

selected_example <- exampleC3
addTitle = "C3_case_neg_imb_yesTP_yesTN"
generate_all_the_plots(selected_example[1], selected_example[2], selected_example[3], selected_example[4], addTitle)

selected_example <- exampleC4
addTitle = "C4_case_neg_imb_noTP_noTN"
generate_all_the_plots(selected_example[1], selected_example[2], selected_example[3], selected_example[4], addTitle)


if (file.exists("./Rplots.pdf")) file.remove("./Rplots.pdf")
