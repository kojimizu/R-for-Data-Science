
##Chapter5 Review
x <- 10:1
y <- 4:5
q <- c("Hokey", "Football", "Baseball", "Curling", "Rugby", "Lacrosse", "Bascketball", "Tennis", "Cricket", "Soccer")

theDF <- data.frame(x,y,q)
theDF
theDF <- data.frame(First = x, Second = y, Sport = q)
head(theDF)
#行のカウント: 
nrow(theDF)
#列のカウント: 
ncol(theDF)
#行列のカウント: 
Dim (theDF)

theUrl <- ”http://www.jaredlander.com/data/Tomato%20First.csv”
tomato <- read.table(file=theUrl, header=TRUE, sep=“,”)
header(tomato)　#内容を確認
