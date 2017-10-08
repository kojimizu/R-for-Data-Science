
##Chap13 - word conversion
#13.1 Paste function
paste ("Hello","Jared","and Others")
paste ("Hello","Jared","and Others",sep = "/")
paste(c("Hello","Hej","Howdy"), c("Jared","Bob","David"))
       
vectorOfText <- c("Hello","Everyone","out there",".")
paste(vectorOfText, collapse = " ")
paste(vectorOfText, collapse= "*")

#13.2 Sprintf function
person <- "Jared"
partySize <- "eight"
waitTime <- 25
sprintf("Hello %s your party of %s will be seated in %s minutes",person,partySize,waitTime)

sprintf("Hello %s, your party of %s will be seated in %s minutes", 
        c("Jared","Bob"),c("eight",16,"four",10), waitTime)

#13.3 Text Extract 

require (XML)
load("data/presidents.rdata")
theURL <- "http://www.loc.gov/rr/print/list/057_chron.html"
pr
esindents <- readHTMLTable((theURL, which=3, as.data.frame=TRUE, skip.rows=1, header=TRUE, stringsAsFactors=FALSE))
  

#14 Probaility Distribution
#14.1 Normal (Gaus) Distribution
rnorm(n=10)
randNorm10 <- rnorm(10)
randNorm10
dnorm(randNorm10)
dnorm(c(-1,0,1))

# data generation from normal distribution
randNorm <- rnorm(30000)
# distribution density calculation
randDensity <- dnorm(randNorm)
#ggplot2 download
require(ggplot2)
#chart generation
ggplot(data.frame(x=randNorm, y=randDensity)) + aes(x=x, y=y) + geom_point() + labs(x="Random Normal Variables", y="Density")

pnorm(randNorm10)
pnorm(c(-3,0,3))
pnorm(-1)
pnorm(1) - pnorm(0)
pnorm(1) - pnorm(-1)

#pnorm function visualization
p <- ggplot(data.frame(x=randNorm, y=randDensity)) + aes(x=x, y=y) + geom_point() + labs(x="x", y="Density")
# calculation of left edge to -1 for shadow space generation
neg1Seq <- seq(from=min(randNorm), to=-1, by=.1)
# x-axix by data.frame / calcultion of y values based on x-axix
lessThanNeg1 <- data.frame(x=neg1Seq,y=dnorm(neg1Seq))
head (lessThanNeg1)

#connection of end-point from left-edge and right-edge at height 0
lessThanNeg1 <- rbind(c(min(randNorm), 0),lessThanNeg1,c(max(lessThanNeg1$x),0))
p + geom_polygon(data=lessThanNeg1,aes(x=x,y=y))

#sequential values generation from -1to 1
neg1Pos1Seq <- seq(from=-1, to=1, by=.1)

neg1To1 <-data.frame(x=neg1Pos1Seq,y=dnorm(neg1Pos1Seq))
head(neg1To1)

neg1To1 <-rbind(c(min(neg1To1$x), 0),neg1To1,c(max(neg1To1$x),0))
p +geom_polygon(data=neg1To1,aes(x=x,y=y))
