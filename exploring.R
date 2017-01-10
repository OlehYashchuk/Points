rm(list = ls())

# Подключение пакетов
library(dplyr)
library(tidyr)
library(ggplot2)
library(nortest)
library(corrplot)
library(tictoc)

# Указание рабочей директории
getwd()
# setwd("d:/Oleh/Репетиторство/Points")
# setwd("D:/D/Репетиторство/Points")

# Считка данных
points <- read.table("Данные/points.txt", 
                     header = F, col.names = c("x", "y"), sep = ";")
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
exclude <- c(5, 14, 15, 20, 22, 24, 34, 37, 42, 44, 48)
'%!in%' <- function(x,y)!('%in%'(x,y))

points <- points %>% 
        filter(x <= 600, y <= 600) %>% 
        filter(student %!in%  exclude) %>%
        select(-student) 

st <- dim(points)[1]/p
student <- rep(1:st, each = dim(points)[1]/st)

points <- points %>% mutate(student = as.factor(student), k =  as.factor(k))
    
# Генерируем случайное поведение респондентов
addStudents <- 1000

# set.seed - для воспроизводимости случайного набора данных
set.seed(998)
x <- rnorm(addStudents * p, 300, 73)
set.seed(999)
y <- rnorm(addStudents * p, 300, 73)
# plot(x, y)
range(x)
range(y)

st <- st + addStudents
student <- rep((st-addStudents+1):st, each = p)
knew <- rep(1:15, times = addStudents)
pointsNew <- tbl_df(cbind(x, y, knew, student))
colnames(pointsNew) <- c("x", "y", "k", 'student')
pointsNew %>% mutate(x = as.integer(x), y = as.integer(y), 
                     k = as.factor(k), student = as.factor(student))
pointsNew$x <- as.integer(pointsNew$x)
pointsNew$y <- as.integer(pointsNew$y)
pointsNew$k <- as.factor(pointsNew$k)
pointsNew$student <- as.factor(pointsNew$student)

points <- points %>% bind_rows(pointsNew)
points$student <- as.factor(points$student)
# levels(points$student)

###############################################################################
# Разведочный анализ данных
###############################################################################
# Рассеивание точек
ggplot(data = points, aes(x, y)) +
        geom_point(alpha=0.7, size=2.5, shape=21, aes(fill=factor(student))) + 
        scale_x_discrete(limits=seq(limMin, limMax, by = limMax/p)) +
        scale_y_discrete(limits=seq(limMin, limMax, by = limMax/p)) +
        theme(legend.position = "none") + guides(fill = 'none') +
        # labs(title = 'Рассеивание точек') +
        theme_bw()
# Сохранить график
# ggsave(paste("Графики/Points.png", sep=""), last_plot(), height = 6, width = 6)

# Рассеивание точек по шагам
ggplot(points, aes(x, y)) + 
        geom_point(alpha=0.7, size=2.5, aes(color=student),show.legend = FALSE) +
        facet_wrap(~k, labeller = label_both)+
        # labs(title = 'Поведение респондентов') +
        theme_bw()
# Сохранить график
# ggsave(paste("Графики/Steps.png", sep=""), last_plot(), height = 7, width = 12)

# Плотность распределения
pointsUn <- gather(points, "xy", "coord", 1:2) %>% 
        mutate(k = as.factor(k), 
               student = as.factor(student), 
               xy = as.factor(xy))

ggplot(pointsUn[which(pointsUn$xy=="y"),], aes(coord)) + 
    labs(x = 'y') +    
    geom_density() +
    facet_wrap(xy~student, labeller = labeller(.multi_line = FALSE)) +
    theme_bw()
# Сохранить график
# ggsave(paste("./Графики/StudentsDensity_y.png", sep=""), last_plot(),
#        height = 7, width = 10)

########## Описательные статистики
# sapply(pointsUn[which(pointsUn$student==9),], mean)
# sapply(pointsUn, mean)
# c1 <- tapply(pointsUn$coord, pointsUn$student, mean)
# 
# ggplot() + aes(c1) + geom_histogram(binwidth = 15)
# ggplot() + aes(c1) + geom_density()
