DF <- read.table(text="Rank F1     F2     F3
1    500    250    50
                 2    400    100    30
                 3    300    155    100
                 4    200    90     10", header=TRUE)

library(reshape2)
DF1 <- melt(DF, id.var="Rank")

library(ggplot2)
ggplot(DF1, aes(x = Rank, y = value, fill = variable)) + 
  geom_bar(stat = "identity")