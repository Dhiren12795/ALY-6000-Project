
#1. PRINT  "Plotting Basics: Your last name"  
print("Plotting Basics: Pagrani")

#2 IMPORT Libraries: FSA, FSAdata, magrittr,  dplyr, plotrix, ggplot2, moments

library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)
library(shades)

#3.LOAD The dataset "BullTroutRML2.csv" Note the dataset is already imported into your project when you added  the libraries FSA and FSAdata

data("BullTroutRML2")


#4.PRINT  first and last 3 records dataset BullTroutRML2

print(head(BullTroutRML2, n=3))
print(tail(BullTroutRML2, n=3))

#5 REMOVE all the records from BullTroutRML2 EXCEPT those from Harrison Lake (hint: use the < filterD( ) function)

Harrison <- filterD(BullTroutRML2, lake == "Harrison")

#6 DISPLAY again the first and last 5 records from dataset BullTroutRML2

print(rbind(head(Harrison, n=5), tail(Harrison, n=5)))

#7 DISPLAY the structure of the filtered BullTroutRML2 dataset

str(Harrison)

#8 DISPLAY the summary of the filtered BullTroutRML2 dataset

summary(Harrison)

#9 PLOT A SCATTERPLOT ( The spec:  age (y variable) and fl (x variable) from the "filtered-Harrison " BullTroutRML2. Set limits on x axis 0,500 and y axis 0,15.  #Title is "Plot1: Harrison Lake Trout Scatter"
#Label the y axis "Age (yrs)" and x axis "Fork Length (mm)",
#Use a filled small circle for the plotted data points)

ggplot(Harrison, aes(x=fl,y=age)) + geom_point() + ggtitle("Plot1: Harrison Lake Trout Scatter") + xlab("Fork Length (mm)") + ylab("Age (yrs)")

#Reference: https://www.r-graph-gallery.com/272-basic-scatterplot-with-ggplot2.html
#Reference: http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles

#10 PLOT2 HISTOGRAM: a BullTroutRML2 age histogram with y axis label: #Frequency, x axis label:Age (yrs),  Title: Plot2: Harrison Fish Age Histogram,Both x and y axis limits 0 15

hist(Harrison$age, main = "Plot2: Harrison Fish Age Histogram", xlab = "Age (yrs)", ylab = "Frequency", xlim = c(0,15), ylim = c(0,15), col = "blue")


#11 PLOT3 OVERDENSE SCATTERPLOT: Solution: use 2 levels of shading with 2	 #levels of green data points, y axis limits:0 to 15, x axis limits 0 to 500, y axis label: #Age (yr), X axis label: Fork Length (mm), Title: Plot3: Harrison Density Shaded By #Era, plot solid circles as data points

shades<- c("lightgreen", "darkgreen")

plot(Harrison$fl, Harrison$age, xlim = c(0,500), ylim = c(0,15), main = "Plot3: Harrison Density Shaded By Era", xlab="Fork Length (mm)", ylab="Age (yrs)", pch=20, col=shades[Harrison$era])

#12 CREATE tmp object with the first 3 and last 3 records of BullTroutRML2

tmp <- rbind(head(Harrison, n=3), tail(Harrison, n=3))

#13 DISPLAY the "era" column (variable) of the tmp object

tmp[,"era", drop=FALSE]
#This will extract the era column from the newly created tmp dataset

#14 CREATE a pchs vector with numerical  arguments for + and x 

pchs=c(3,4)

#14 CREATE a cols vector with the two elements: "red" and "gray60" 

cols <- c("red", "gray60")
cols

#15 CONVERT the tmp era values to numeric
tmp$era <- as.numeric(tmp$era)

#16 INITIALIZE the cols vector with tmp era values

cols[tmp$era] <- tmp$era

#17 NOW PLOT4: (The spec: age(y variable) versus fl (x variable), Title Plot:"Plot4: Symbol & #Color By Era, Set x variable limits 0, 500 and y variable limits 0, 15, Set y axis label equal to "Age" and x axis label equal to Fork Length (mm). set pch equal to pchs era values and col equal to cols era values

plot(Harrison$fl, Harrison$age, xlab = "Fork Length (mm)", ylab = "Age (yrs)", main = "Plot4: Symbol & Color By Era", xlim = c(0,500), ylim = c(0,15), pch =pchs[Harrison$era], col=cols[Harrison$era]) 

#18 PLOT a regression line overlay on PLOT4:  Title PLOT5: "Plot5: Regression Overlay

plot(Harrison$fl, Harrison$age, xlab = "Fork Length (mm)", ylab = "Age (yrs)", main = "Plot5: Regression Overlay", xlim = c(0,500), ylim = c(0,15), pch=pchs[Harrison$era], col=cols[Harrison$era])
abline(lm(Harrison$age~Harrison$fl), col="black")
#19 PLOT6: Place a Legend overlay on PLOT5

plot(Harrison$fl, Harrison$age, xlab = "Fork Length (mm)", ylab = "Age (yrs)", main = "PLOT6: Legend overlay", xlim = c(0,500), ylim = c(0,15), pch=pchs[Harrison$era], col= cols[Harrison$era])


legend("topleft", inset=.07, c("1977-80", "1997-01"), pch=pchs, col=c("red", "gray60"))