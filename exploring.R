rm(list = ls())

# Подключение пакетов
library(dplyr)
library(tidyr)
library(ggplot2)
library(nortest)
library(corrplot)

# Указание рабочей директории
getwd()
# setwd("d:/Oleh/Репетиторство/Points")
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
# На основе визуального анализа убираем данные по некоторым респондентам
exclude <- c(5, 14, 15, 20, 22, 24, 34, 37, 42, 44, 48)
'%!in%' <- function(x,y)!('%in%'(x,y))
points <- points[,-5]
points <- points %>% 
        filter(x <= 600, y <= 600) %>% 
        filter(student %!in%  exclude) %>%
        select(-student) %>%
        mutate(student = as.factor(rep(1:(dim(points)[1]/p), 
                                       each = dim(points)[1]/st)),
               k =  as.factor(k))

st <- dim(points)[1]/p
# student <- rep(1:st, each = dim(points)[1]/st)
# points <- tbl_df(cbind(points, student))


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

########## Плотность распределения данных 
pointsUn <- gather(points, "xy", "coord", 1:2) %>% 
        mutate(k = as.factor(k), student = as.factor(student), xy = as.factor(xy))

ggplot(pointsUn[which(pointsUn$xy=="x"),], aes(coord)) + 
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
