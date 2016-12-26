rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(nortest)

getwd()
setwd("d:/Oleh/Репетиторство/Points-master")

points <- read.table("points.txt", header = F, col.names = c("x", "y"), sep = ";")
p <- 15
st <- dim(points)[1]/p
summary(points)
k <- rep(1:15, times = dim(points)[1]/p)
student <- rep(1:st, each = dim(points)[1]/st)
points[which(points$y>600),]
points[which(points$student==48),]
points <- tbl_df(cbind(points, k, student))
colnames(points) <- c("x", "y", "k", 'student')
summary(points)

ggplot(data = points, aes(x, y)) +
        geom_point(alpha=0.7, size=2.5, shape=21,
                   fill=student) +
                   # aes(fill=factor(student))) + #, shape=student
        scale_x_discrete(limits=seq(0, 600, by = 600/15)) +
        scale_y_discrete(limits=seq(0, 600, by = 600/15)) +
        # theme(legend.position="bottom") +
        # guides(fill = 'legend') +
        theme_bw()
# ?ggplot
# ?theme

# ggsave(paste("Points.png", sep=""), last_plot(), height = 6, width = 6)


ggplot(points, aes(x, y)) + 
        geom_point(alpha=0.7, size=2.5, aes(color=k)) +
        # geom_point(alpha=0.4, size=2.5, color=student) + #, shape=student
        # scale_x_continuous(limits = c(0, 600)) +
        # scale_y_continuous(limits = c(0, 600)) +
        # scale_x_discrete(limits=seq(0, 600, by = 600/4)) +
        scale_y_discrete(limits=seq(0, 600, by = 600/8)) +
        facet_wrap(~student, labeller = label_both)+
        # theme(legend.position = 'none')
        theme_bw()
# points[which((points$k==15)),]
# ggsave(paste("Students.png", sep=""), last_plot(), height = 7, width = 10)

##########
pointsUn <- gather(points, "xy", "coord", 1:2)

ggplot(pointsUn, aes(coord)) + 
        geom_density() +
        facet_wrap(xy~k, ncol = 5, 
                   labeller = labeller(.multi_line = FALSE)) +#"label_both", 
        theme_bw()
# ggsave(paste("StepsDensity.png", sep=""), last_plot(), height = 7, width = 10)

########## Описательные статистики
# sapply(pointsUn[which(pointsUn$student==9),], mean)
# sapply(pointsUn, mean)
# c1 <- tapply(pointsUn$coord, pointsUn$student, mean)
# 
# ggplot() + aes(c1) + geom_histogram(binwidth = 15)
# ggplot() + aes(c1) + geom_density()

########## Проверка на нормальность
pearson.test(points$y)
cvm.test(points$x)
ad.test(points$x)
lillie.test(points$x)
sf.test(points$x)
shapiro.test(points$x)
tapply(pointsUn$coord, pointsUn$student, pearson.test)

pv <- 0
for (i in 1:63) {
        pv[i] <- tapply(pointsUn$coord, pointsUn$student, shapiro.test)[[i]][[2]]
}
sum(pv < 0.05)
14/47
# rm(list = ls())
# pv$X <- 0
# pv$Y <- 0
# pv$un <- 0
# pv <- data.frame()
# pv <- list()
pv <- 0
pvX <- 0
pvY <- 0
for (i in 1:63) {
        pvX[i] <- tapply(points$x, points$student, shapiro.test)[[i]][[2]]
        pvY[i] <- tapply(points$y, points$student, shapiro.test)[[i]][[2]]
        pv[i] <- tapply(pointsUn$coord, pointsUn$student, shapiro.test)[[i]][[2]]
}

sum(pvX < 0.05)
sum(pvY < 0.05)
sum(pv < 0.05)


# Построение графиков ку-ку
qqplot.data <- function (vec) # argument: vector of numbers
{
        # following four lines from base R's qqline()
        y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
        x <- qnorm(c(0.25, 0.75))
        slope <- diff(y)/diff(x)
        int <- y[1L] - slope * x[1L]
        
        d <- data.frame(resids = vec)
        
        ggplot(d, aes(sample = resids)) + 
                stat_qq() + 
                geom_abline(slope = slope, intercept = int) 
}        

qqplot.data(points$x)
qqplot.data(points$y)
qqplot.data(pointsUn$coord)

tapply(pointsUn$coord, pointsUn$student, qqplot.data) 

ggplot(pointsUn[which(pointsUn$xy=="y"),]) +
        geom_qq(aes(sample = coord)) +
        # geom_abline(intercept=quantile(c(0.25, 0.75))[1]-
        #                     diff(quantile(c(0.25, 0.75)))/
        #                     diff(qnorm(c(0.25, 0.75)))*
        #                     quantile(c(0.25, 0.75)), 
        #             slope=diff(quantile(c(0.25, 0.75)))/
        #                     diff(qnorm(c(0.25, 0.75)))) + 
        facet_wrap(xy~student, labeller = labeller(.multi_line = F)) +
        theme_bw()
# qqline()

ggsave(paste("qqY.png", sep=""), last_plot(), height = 7, width = 10)

plot(dnorm(seq(-4, 4, by=0.01)), type = 'l')
pnorm(3)
qnorm(pnorm(3))
