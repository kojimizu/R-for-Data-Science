
#Introduction to Statistical Modeling

#データの概要把握
data
summary(data)
length(data)
table(data)

# Histgramの作成
hist(data, breaks = seq(-0.5, 9.5, 1))
hist(data)

#標本分散
var(data)
sd(data)
sqrt(var(data))


#Chap.2
y <- 0:9
prob <- dpois(y, lambda = 3.56)
plot(y,prob,type = "b", lty=2)

#2-4 ML method using Poisson distribution
logL <- function(m) sum(dpois(data, m, log = TRUE))
lambda <- seq(2, 5, 0.1)
plot (lambda, sapply(lambda, logL), type = "l")

#Chap.3 
#データの読み込み
d <- read.csv("http://hosho.ees.hokudai.ac.jp/~kubo/stat/iwanamibook/fig/poisson/data3a.csv")

#3.2 観測データの概要
d$x
d$f
class(d) #data.frameクラス
class(d$y) #y列は整数だけのintegerクラス
class(d$x) #x列は実数も含むのでnumericクラス
class(d$f) #f列はfactorクラス

#3.3 データ図示
plot(d$x, d$y,pch = c(21,19)[d$f])
legend("topleft", legend=c("C","T"),pch=c(21,19))

#3.4 GLM関数
fit <- glm(y ~x, data = d, family = poisson)
## fit : 結果を格納するオブジェクト
## y ~ x: モデル式
## poisson (link = “log”): 確率分布の指定/リンク関数の指定(省略可能)
## )data.frameの指定

fit #又はprint(fit)
summary(fit)
logLik(fit) #最大対数尤度（パラメータが最尤推定値/logLが最大化）を評価

#3.4.3


