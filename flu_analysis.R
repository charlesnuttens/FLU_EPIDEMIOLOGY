setwd("C:/Users/nuttec/Desktop/R/Epidemiology/Flu Analysis")

##Read data frame, create Age factor and order weeks
df <- read.csv2("AgeViewByWeek.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE, colClasses = c("numeric", "numeric", "character", rep("numeric", 9)))
df$Age.Group <- factor(df$Age.Group, levels = c("0-4 yr", "5-24 yr", "25-64 yr", "65+ yr"))
df$total_cases <- apply(df[4:12], 1, function(x) sum(x, na.rm = TRUE))
df_order <- df[order(df[,1], df[,2], df[,3]), ]


##by agegroup
age1 <- subset(df_order, Age.Group == "0-4 yr")$total_cases
age2 <- subset(df_order, Age.Group == "5-24 yr")$total_cases
age3 <- subset(df_order, Age.Group == "25-64 yr")$total_cases
age4 <- subset(df_order, Age.Group == "65+ yr")$total_cases
df_age = data.frame(age1, age2, age3, age4)
names(df_age) <- c("0-4 yr", "5-24 yr", "25-64 yr", "65+ yr")

week <- subset(df_order, Age.Group == "0-4 yr")$Week
week[week != 1 & week != 26] <- NA

barplot(t(df_age), main="Number of laboratory confirmed cases per week and by age group from 40/2007 to 40/2016", xlab="Week", ylab="Number of confirmed cases", ylim = c(0,8000), names.arg=week, cex.names=0.7, col=c("firebrick", "dodgerblue", "gold", "forestgreen"))
legend(400, 8000, legend = names(df_age), fill =c("firebrick", "dodgerblue", "gold", "forestgreen"), cex=0.6, box.lty=0, x.intersp = 0.2,  y.intersp = 0.8)


##by serotype
library(tidyr)
df_order$Yweek <- df_order$Yweek <- do.call(paste, c(df_order[c("Year", "Week")], sep="-"))
df_serotype <- aggregate(df_order[c(4:12)], by=list(Category=df_order$Yweek), FUN=sum)
df_serotype2 <- separate(df_serotype, Category, into = c("year", "week"), sep = "-")
df_serotype2$week <- as.numeric(df_serotype2$week)
df_serotype3 <- df_serotype2[order(df_serotype2[1], df_serotype2[2]), ]

week <- subset(df_order, Age.Group == "0-4 yr")$Week
week[week != 1 & week != 26] <- NA

barplot(t(df_serotype3[c(3:11)]), main="Number of laboratory confirmed cases per week and by serotype from 40/2007 to 40/2016", xlab="Week", ylab="Number of confirmed cases", ylim= c(0,8000), names.arg=week, cex.names=0.7, col=c("firebrick", "dodgerblue", "gold", "forestgreen", "orange1", "darkorchid1", "#A7A7A7", "blueviolet", "magenta"))
legend(400, 8000, legend = names(df_serotype3)[3:11], fill =c("firebrick", "dodgerblue", "gold", "forestgreen", "orange1", "darkorchid1", "#A7A7A7", "blueviolet", "magenta"), cex=0.6, box.lty=0, x.intersp = 0.2,  y.intersp = 0.8)

#by serogroup (65+ yr)

df_65 <- subset(df_order, Age.Group == "65+ yr")

week <- subset(df_order, Age.Group == "0-4 yr")$Week
week[week != 1 & week != 26] <- NA

barplot(t(df_65[c(4:12)]), main="Number of laboratory confirmed cases per week and by serotype from 40/2007 to 40/2016 (65+ yr)", xlab="Week", ylab="Number of confirmed cases", ylim= c(0,2000), names.arg=week, cex.names=0.7, col=c("firebrick", "dodgerblue", "gold", "forestgreen", "orange1", "darkorchid1", "#A7A7A7", "blueviolet", "magenta"))
legend(0, 2000, legend = names(df_65)[4:12], fill =c("firebrick", "dodgerblue", "gold", "forestgreen", "orange1", "darkorchid1", "#A7A7A7", "blueviolet", "magenta"), cex=0.6, x.intersp = 0.2,  y.intersp = 0.8, bty = "n")