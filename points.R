rm(list = ls())

# Подключение пакетов
library(dplyr)
library(tidyr)
library(ggplot2)
library(nortest)
library(corrplot)

# Указание рабочей директории

getwd()
# setwd("d:/Oleh/Репетиторство/Points-master")
# setwd("D:/D/Репетиторство/Points")

# Считка данных
points <- read.table("points.txt", header = F, col.names = c("x", "y"), sep = ";")
# Границы области
limMin <- 0
limMax <- 600
# Кол-во точек для каждого респондента
p <- 15
# Кол-во респондентов
st <- dim(points)[1]/p
# Вспомогательные вектора для пометки номера шага и респондента 
k <- rep(1:15, times = dim(points)[1]/p)
student <- rep(1:st, each = dim(points)[1]/st)

# Объединение данных
points <- tbl_df(cbind(points, k, student))
colnames(points) <- c("x", "y", "k", 'student')
# Базовые описательные статистики
summary(points)
# Поиск выбросов
points[which(points$x>600 | points$y>600),]
# Чистка выбросов
points <- points %>% 
        filter(x <= 600, y <= 600, student != 48) %>% 
        mutate(student = as.factor(student), k =  as.factor(k))

# Рассеивание точек
ggplot(data = points, aes(x, y)) +
        geom_point(alpha=0.7, size=2.5, shape=21, 
                   aes(fill=factor(student))) + 
        scale_x_discrete(limits=seq(limMin, limMax, by = limMax/p)) +
        scale_y_discrete(limits=seq(limMin, limMax, by = limMax/p)) +
        theme(legend.position = "none") + guides(fill = 'none') +
        labs(title = 'Рассеивание точек') +
        theme_bw()
# ggsave(paste("Points.png", sep=""), last_plot(), height = 6, width = 6)

ggplot(points, aes(x, y)) + 
        geom_point(alpha=0.7, size=2.5, aes(color=k)) +
        facet_wrap(~student, labeller = label_both)+
        # theme(legend.position = 'none') +
        labs(title = 'Поведение респондентов') +
        theme(legend.position="bottom") +
        theme_bw()

# ggsave(paste("Students.png", sep=""), last_plot(), height = 7, width = 10)

# На основе визуального анализа убираем данные по некоторым респондентам
# points <- points %>% filter(student != c(5, 14, 15, 20, 37, 44))

##########
pointsUn <- gather(points, "xy", "coord", 1:2) %>% 
        mutate(k = as.factor(k), student = as.factor(student), xy = as.factor(xy))
pointsUn[which(pointsUn$xy=="x" & pointsUn$student <= 30),]

ggplot(pointsUn[which(pointsUn$xy=="x" & pointsUn$student <= 30),], aes(coord)) + 
        geom_density() +
        facet_wrap(xy~student, ncol = 5, labeller = labeller(.multi_line = FALSE)) +
        theme_bw()
# ggsave(paste("./Графики/StudentsDensity.png", sep=""), last_plot(), height = 7, width = 10)

########## Описательные статистики
# sapply(pointsUn[which(pointsUn$student==9),], mean)
# sapply(pointsUn, mean)
# c1 <- tapply(pointsUn$coord, pointsUn$student, mean)
# 
# ggplot() + aes(c1) + geom_histogram(binwidth = 15)
# ggplot() + aes(c1) + geom_density()

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
# Построение графиков ку-ку
###############################################################################
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

tapply(pointsUn$coord, pointsUn$xy, qqplot.data)
tapply(points$x, points$student, qqplot.data) 

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

# ggsave(paste("qqY.png", sep=""), last_plot(), height = 7, width = 10)

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

pointsCor <- list()
pointsCor$x <- pointsUn %>% spread(student, coord) %>% 
        filter(xy == 'x') %>% select(-c(k, xy))
pointsCor$y <- pointsUn %>% spread(student, coord) %>% 
        filter(xy == 'y') %>% select(-c(k, xy))

mCor <- list()
mCor$xP <- cor(pointsCor$x)
mCor$yP <- cor(pointsCor$y)
mCor$xS <- cor(pointsCor$x, method = 'spearman')
mCor$yS <- cor(pointsCor$y, method = 'spearman')

# sum(abs(mCor$xP))
# sum(abs(mCor$xS))
# sum(abs(mCor$yP))
# sum(abs(mCor$yS))

corrplot(mCor$xP, method = "ellipse",  type="lower", col=col3(200), 
         order="hclust", 
         title = "Кореляція Пирсона")
corrplot(mCor$xS, method = "ellipse",  type="lower", col=col3(200), 
         order="hclust", 
         title = "Кореляція Спірмена")


########################################
# Рассчет примера
########################################
mCor$xP[12, 11]

cor(points$x[points$student==11], points$x[points$student==12])

covv <- mean(points$x[points$student==11] * points$x[points$student==12]) -
mean(points$x[points$student==11]) * mean(points$x[points$student==12])

mn <- sqrt(14 / 15)

sdd <- (sd(points$x[points$student==11])*mn) *
(sd(points$x[points$student==12])*mn)

covv / sdd

plot(points$x[points$student==11], points$x[points$student==12])

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
mCor$xP <- cor(pointsCor$x)
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

a <- which(mCor$xP[-1,1] %in% sort(mCor$xP[-1,1], decreasing = T)[1:2])
pointsCor$x[,a] 

x <- pointsCor$x[,a] 


q <- 2 / dim(pointsCor$x)[1]
which(pointsCor$x[1] >= quantile(unlist(pointsCor$x[1]), probs = 1-q))

intersect(mCor$xP[-1,1], sort(mCor$xP[-1,1], decreasing = T)[1:2])


















