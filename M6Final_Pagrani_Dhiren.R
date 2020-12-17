install.packages("psych")
install.packages("readr")
install.packages("RColorBrewer")
library(readr)
library(plyr)
library(DescTools)
library(psych)

M6Set1 <- read_csv("Northeastern University/Intro to Analytics-ALY6000/Week 6/M6Set1.csv", 
                   col_types = cols(RowID = col_skip(), 
                                    OrderID = col_skip(), OrderDate = col_date(format = "%m/%d/%Y"), 
                                    ShipDate = col_skip(), CustomerID = col_skip(), 
                                    CustomerName = col_skip(), PostalCode = col_skip(), 
                                    ProductID = col_skip(), ProductName = col_skip(), 
                                    Sales = col_number(), Quantity = col_integer(), 
                                    Discount = col_number(), Profit = col_number(), 
                                    ShippingCost = col_number()))



M6Set2 <- mutate(M6Set1, Totalsales = (M6Set1$Sales*M6Set1$Quantity))
M6Set2 <- mutate(M6Set2, Cumulativesales = cumsum(M6Set2$Totalsales))

M6Set3 <- as.data.frame(M6Set2[,c("Country", "Region", "Market", "Segment", "Category", "Sales", "Profit", "ShippingCost", "Totalsales", "Cumulativesales")])
write.table(M6Set3, file = "M6Finalset.csv", row.names = F, sep=",")
View(M6Set3)
Desc(M6Set3)

totalsales <- aggregate.data.frame(M6Set3$Sales,by=list(Market=M6Set3$Market), FUN="sum")
names(totalsales)= c("Market","Totalsales")
slices<- totalsales$Totalsales
lbls <- totalsales$Market
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,pct)
lbls <- paste(lbls,"%",sep="")
pie(slices,labels=lbls,
    col = terrain.colors(5),
    main="Total Sales by Market")
    legend("topright",legend=totalsales$Market, cex=0.6, bty="n", fill=terrain.colors(5))
    




A1 <- aggregate.data.frame(M6Set3$Cumulativesales, by=list(M6Set3$Region),sum)
A2 <- arrange(A1,desc(A1$x))
A3 <- mutate(A2,Cumtotalsales=cumsum(x))
A4 <- arrange(A3,A3$Cumtotalsales)
View(A4)
par(mar=c(10,1,5,3))
pc <- barplot(A2$x,
              names.arg = A2$Group.1,col=rainbow(10),
              las=2,
              axes = F,
              ylim = c(0,8*max(A4$x,na.rm = TRUE)))

axis(4,at=c(0,pretty(A4$Cumtotalsales)),
     col = "black" ,
     col.axis="black",
     cex.axis=0.8)
axis(1,at=c(0,A4$x),
     col = "black" ,
     col.axis="black",
     cex.axis=0.8)

     lines(pc,A4$Cumtotalsales, type = "b", pch = 19, col="blue")
     mtext("Cumulative Sales", side = 4, line = 2, col = "black", adj = 0.1, cex = 1)
     mtext("Region", side = 1, line = 8, col = "black", adj = 0.5, cex = 1)
     #not adding legend as figures are self explanatory
