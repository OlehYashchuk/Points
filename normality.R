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