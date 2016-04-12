setwd("D:/Oht/R/Resume_char_freq")
library(ggplot2)
library(stringr)

# read text file into a vector
cc <- scan("res.txt", what = "", sep = "")
# clean the vector
cc_split <- strsplit(cc, "")
cc_vect <- rle(unlist(cc_split))$values
cc_vect_strip <- str_replace_all(cc_vect, "[^[:alnum:]]|\\s", " ")
cc_clean <- gsub(" ", "", cc_vect_strip)
cc_new <- cc_clean[cc_clean != ""]

barplot(table(cc_new),main = "Frequency of Characters in Resume",
        xlab = "Characters", ylab="Frequency",las=2,
        col=c("#FF99FF","#0066FF","#00FF4D"))


