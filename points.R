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

###############################################################################
# Корреляция 
###############################################################################
selectedSteps <- 52

pointsCor <- list()
pointsCor$x <- pointsUn %>% spread(student, coord) %>%
    mutate(k = as.numeric(k)) %>%
    filter(xy == 'x', k <= selectedSteps) %>% select(-c(k, xy))
pointsCor$y <- pointsUn %>% spread(student, coord) %>% 
    mutate(k = as.numeric(k)) %>%
    filter(xy == 'y', k <= selectedSteps) %>% select(-c(k, xy))

mCor <- list()
mCor$xP <- cor(pointsCor$x)
mCor$yP <- cor(pointsCor$y)
mCor$xS <- cor(pointsCor$x, method = 'spearman')
mCor$yS <- cor(pointsCor$y, method = 'spearman')

col3 <- colorRampPalette(c("red", "white", "blue"))

png(file = 'Графики/Spearman_correlation_y.png', width = 800, height = 800)
corrplot(mCor$yS, method = "ellipse",  type="full", col=col3(200)) 
dev.off()
# dev.cur()

         # order="hclust", 
         # title = "Кореляція Пирсона, y")
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








###############################################################################
# Оценка значений
###############################################################################
source("knn.R")
source("rmse.R")

# listRMSE <- list()

# for (j in 10:15) {
    selectedSteps <- 10
    pointsCor <- list()
    
    pointsCor$x <- pointsUn %>% spread(student, coord) %>%
        mutate(k = as.numeric(k)) %>%
        filter(xy == 'x', k <= selectedSteps) %>% select(-c(k, xy))
    pointsCor$y <- pointsUn %>% spread(student, coord) %>% 
        mutate(k = as.numeric(k)) %>%
        filter(xy == 'y', k <= selectedSteps) %>% select(-c(k, xy))

    stepsK <- c(1:5, seq(10, 50, 5))
    
    kRMSE <- list()
    s <- 0

    for (i in stepsK) {
        s <- s + 1
        
        # c <- knn(pointsCor, i, corMethod = "pearson")
        kRMSE$k[s] <- i
        # kRMSE$pearson[s] <- rmse(c, pointsCor)
        kRMSE$pearson[s] <- rmse(knn(pointsCor, i, corMethod = "pearson"), 
                                 pointsCor)
        kRMSE$spearman[s] <- rmse(knn(pointsCor, i, corMethod = "spearman"), 
                                  pointsCor)
    }
    
    
    
    # kRMSE$k <- kRMSE$k[-c(1:14)]
    # kRMSE$pearson <- kRMSE$pearson[-c(1:14)]
    # kRMSE$spearman <- kRMSE$spearman[-c(1:14)]

    # listRMSE$Pearson[j] <- min(kRMSE)
    listRMSE$Spearman[i] <- min(kRMSE)
    # listRMSE$Kendall[i] <- rmse(c, pointsCor)
    
    print(paste('j = ', j, '| rmse = ', min(kRMSE, na.rm = T)))
    plot(kRMSE, type = 'o', xlim = c(1, max(stepsK)))
# }



plot(kRMSE$k, kRMSE$pearson, type = 'o', xlim = c(1, max(stepsK)))
plot(kRMSE$k, kRMSE$spearman, type = 'o', xlim = c(1, max(stepsK)))

min <- 1
max <- length(stepsK)
ggplot() + 
    geom_point(alpha=0.7, size=1.5, aes(x = kRMSE$k[-min], y = kRMSE$pearson[-min])) +
    geom_point(alpha=0.7, size=1.5, aes(x = kRMSE$k[-min], y = kRMSE$spearman[-min])) +
    geom_line(aes(x = kRMSE$k[-min], y = kRMSE$pearson[-min]), color = "red") +
    geom_line(aes(x = kRMSE$k[-min], y = kRMSE$spearman[-min]), color = "blue") +
    scale_x_discrete(limits = kRMSE$k[-min]) + 
    coord_cartesian(xlim = c(0, 52)) + 
    geom_point(alpha=1, size=3, color = 'red',
               aes(x = kRMSE$k[which(kRMSE$pearson[-min] == min(kRMSE$pearson[-min]))+1], 
                   y = min(kRMSE$pearson[-min]))) +
    geom_point(alpha=1, size=3, color = 'blue',
               aes(x = kRMSE$k[which(kRMSE$spearman[-min] == min(kRMSE$spearman[-min]))+1], 
                   y = min(kRMSE$spearman[-min]), label = kRMSE$spearman[-min])) +
    
    geom_text(aes(x = kRMSE$k[which(kRMSE$pearson[-min] == min(kRMSE$pearson[-min]))+1], 
                  y = min(kRMSE$pearson[-min]),
                  label = round(kRMSE$pearson[which(kRMSE$pearson[-min] == min(kRMSE$pearson[-min]))+1])),
              hjust=0,vjust=-1, size = 4) +
    
    geom_text(aes(x = kRMSE$k[which(kRMSE$spearman[-min] == min(kRMSE$spearman[-min]))+1], 
                  y = min(kRMSE$spearman[-min]),
                  label = round(kRMSE$spearman[which(kRMSE$spearman[-min] == min(kRMSE$spearman[-min]))+1])),
              hjust=0,vjust=-1, size = 4) +
    
    # theme(legend.position = "left") + guides(shape = "legend", fill = 'none') +
    
    labs(x = 'k Nearest Neighbors', y = 'RMSE')

ggsave(paste("Графики/kNeighbors.png", sep=""), last_plot(), 
       height = 7, width = 12)

par(mfrow=c(1,2))
plot(unlist(c$x[15,]), unlist(c$y[15,]), col = 'orange', pch = 19, 
     xlim = c(0, 600), ylim = c(0, 600))
plot(unlist(pointsCor$x[15,]), unlist(pointsCor$y[15,]), col = 'blue', pch = 19, 
     xlim = c(0, 600), ylim = c(0, 600))


