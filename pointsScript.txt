rm(list = ls())

# Подключение пакетов
library(dplyr)
library(tidyr)
library(ggplot2)
library(nortest)

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
points <- points %>% filter(x <= 600, y <= 600)

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
pointsUn <- gather(points, "xy", "coord", 1:2)
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

########## Проверка на нормальность
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

plot(dnorm(seq(-4, 4, by=0.01)), type = 'l')
pnorm(3)
qnorm(pnorm(3))
