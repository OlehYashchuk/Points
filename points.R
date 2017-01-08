###############################################################################
# Проверка на нормальность
###############################################################################
pearson.test(points$y)
cvm.test(points$x)
ad.test(points$x)
lillie.test(points$x)
sf.test(points$x)
shapiro.test(points$x)
tapply(pointsUn$coord, pointsUn$student, pearson.test)

pv <- 0
pvX <- 0
pvY <- 0
for (i in 1:st) {
        pvX[i] <- tapply(points$x, points$student, shapiro.test)[[i]][[2]]
        pvY[i] <- tapply(points$y, points$student, shapiro.test)[[i]][[2]]
        pv[i] <- tapply(pointsUn$coord, pointsUn$student, shapiro.test)[[i]][[2]]
}

sum(pvX < 0.05) / st
sum(pvY < 0.05) / st
sum(pv < 0.05)
which(pv < 0.05)
###############################################################################
# dnorm = p(X)
# qnorm = F(x)
# pnorm = p-value(x)

plot(dnorm(seq(-4, 4, by=0.01)), type = 'l'); abline(h=0)
pnorm(3)
qnorm(pnorm(3))

###############################################################################
# Корреляция 
###############################################################################
col3 <- colorRampPalette(c("red", "white", "blue"))


mCor <- list()
mCor$xP <- cor(pointsCor$x)
mCor$yP <- cor(pointsCor$y)
mCor$xS <- cor(pointsCor$x, method = 'spearman')
mCor$yS <- cor(pointsCor$y, method = 'spearman')

# sum(abs(mCor$xP))
# sum(abs(mCor$xS))
# sum(abs(mCor$yP))
# sum(abs(mCor$yS))

corrplot(mCor$yP, method = "ellipse",  type="full", col=col3(200), 
         # order="hclust", 
         title = "Кореляція Пирсона, y")
# dev.cur()
# ggsave("Графики/Pearson_x.png", dev.cur(), height = 7, width = 10)

corrplot(mCor$yS, method = "ellipse", col=col3(200), 
         # type="lower", order="hclust", 
         title = "Кореляція Спірмена, y")



########################################
# Рассчет примера
########################################
mCor$xS[12, 11]

cor(points$x[points$student==11], points$x[points$student==12])

covv <- mean(points$x[points$student==11] * points$x[points$student==12]) -
mean(points$x[points$student==11]) * mean(points$x[points$student==12])

mn <- sqrt(14 / 15)

sdd <- (sd(points$x[points$student==11])*mn) *
(sd(points$x[points$student==12])*mn)

covv / sdd

plot(points$x[points$student==11], points$x[points$student==12])

sort(a)
sort(b)
ar <- rank(a)
br <- rank(b)
1-6*sum((ar-br)^2)/(15*(15^2-1))


sd(a)*mn
a <- points$x[points$student==11]
b <- points$x[points$student==12]
am <- mean(a)
an <- (a-am)^2
sqrt(sum(an)/14)*mn

sum(a*b/15)

# wb <- c("white","black")
# corrplot(Mx, order="hclust", addrect=2, col=wb, bg="gold2")
# corrplot(Mx, order="hclust", addrect=3)
# corrplot.mixed(Mx, lower="ellipse", upper="circle")
# corrplot(Mx, order="AOE", cl.pos="b", tl.pos="d", tl.srt=60)
###############################################################################

mCor$xP
pointsCor$x
pointsCor$y

mCor <- list()
mCor$xP <- as.matrix(cor(pointsCor$x))
mCor$yP <- cor(pointsCor$y)
mCor$xS <- cor(pointsCor$x, method = 'spearman')
mCor$yS <- cor(pointsCor$y, method = 'spearman')

mnk <- function(data, user, k = 2, ...) {
        mCor <- list()
        mCor$xP <- cor(data$x)
        mCor$yP <- cor(data$y)
        mCor$xS <- cor(data$x, method = 'spearman')
        mCor$yS <- cor(data$y, method = 'spearman')
        
        which(mCor$xP)
}

# sort(mCor$xP[-1,1], decreasing = T)[1:2]

mCor$xP[51,52]
a <- which(mCor$xP[-1,1] %in% sort(mCor$xP[-1,1], decreasing = T)[1:2])
pointsCor$x[,a]

ru <- mean(unlist(pointsCor$x[1,]))



x <- pointsCor$x[,a] 


q <- 2 / dim(pointsCor$x)[1]
which(pointsCor$x[1] >= quantile(unlist(pointsCor$x[1]), probs = 1-q))

intersect(mCor$xP[-1,1], sort(mCor$xP[-1,1], decreasing = T)[1:2])



###############################################################################
# Оценка значений
###############################################################################

pointsCor <- list()
pointsCor$x <- pointsUn %>% spread(student, coord) %>% 
        filter(xy == 'x') %>% select(-c(k, xy))
pointsCor$y <- pointsUn %>% spread(student, coord) %>% 
        filter(xy == 'y') %>% select(-c(k, xy))

source("knn.R")
source("rmse.R")



range(c$x[15,])

kRMSE <- 0
for (i in 1:61) {
    c <- knn(pointsCor, i, corMethod = "spearman")
    # print(paste(range(c$x[15,]), range(c$y[15,])))
    kRMSE[i] <- rmse(c, pointsCor)
}
min(kRMSE)

plot(kRMSE, type = 'o', xlim = c(1, 61))

par(mfrow=c(1,1))
c <- knn(pointsCor, 1, corMethod = "pearson")
rmse(c, pointsCor$x)

plot(unlist(c$x[15,]), unlist(c$y[15,]), col = 'orange', pch = 19, 
     xlim = c(0, 600), ylim = c(0, 600))
plot(unlist(pointsCor$x[15,]), unlist(pointsCor$y[15,]), col = 'blue', pch = 19, 
     xlim = c(0, 600), ylim = c(0, 600))


