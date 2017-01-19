rm(list = ls())

# Подключение пакетов
library(dplyr)
library(tidyr)
library(ggplot2)
library(nortest)
library(corrplot)
library(tictoc)
library(irr)

# Указание рабочей директории
getwd()
# setwd("d:/Oleh/Репетиторство/Points")
# setwd("D:/D/Репетиторство/Points")

# Считывание данных
points <- read.table("Данные/points.txt", 
                     header = F, 
                     col.names = c("x", "y"), 
                     sep = ";")

###############################################################################
# Обработка исходных данных
###############################################################################
limMin <- 0                      # Границы области
limMax <- 600                    # Границы области
p <- 15                          # Кол-во точек для каждого респондента
st <- dim(points)[1]/p           # Кол-во респондентов
k <- rep(1:15, times = dim(points)[1]/p)        # номера шага
student <- rep(1:st, each = dim(points)[1]/st)  # номер респондента 

# Объединение данных
points <- tbl_df(cbind(points, k, student))
colnames(points) <- c("x", "y", "k", 'student')

summary(points)                  # Базовые описательные статистики

points[which(points$x>600 | points$y>600),] # Поиск выбросов
# Чистка данных
exclude <- c(5, 14, 15, 20, 22, 24, 34, 37, 42, 44, 48)
'%!in%' <- function(x,y)!('%in%'(x,y)) # задание оператора тип "not in"

# Исключение респондентов со списка "exclude"
points <- points %>% 
        filter(x <= 600, y <= 600) %>% 
        filter(student %!in%  exclude) %>%
        select(-student) 

# Перезапись номера шага и номера респондента
st <- dim(points)[1]/p
student <- rep(1:st, each = dim(points)[1]/st)

points <- mutate(points, 
                 student = student,
                 k =  k)

###############################################################################
# Генерация новых случайных респондентов
###############################################################################
addStudents <- 1000 # кол-во новых респондентов

set.seed(998)           # для воспроизводимости случайного набора данных
x <- rnorm(addStudents * p, 300, 73) # координата "x" из норм. распр.
set.seed(999)           # для воспроизводимости случайного набора данных
y <- rnorm(addStudents * p, 300, 73) # координата "y" из норм. распр.
range(x); range(y)

# Формируем данные с новыми респондентами
st <- st + addStudents # кол-во респондентов после добавления новых
student <- rep((st-addStudents+1):st, each = p) # номера новых респондентов
k <- rep(1:15, times = addStudents) # номер шага для новых респондентов

pointsNew <- tbl_df(cbind(x, y, k, student)) # объединяем массив новых данных
points <- bind_rows(points, pointsNew) # объединяем новые данные со старыми

# задаём типы данных
points <- mutate(points,
                 x = as.integer(x),
                 y = as.integer(y),
                 k = as.factor(k),
                 student = as.factor(student))

# Задание новых факторов

xMin <- min(points$x)
xMax <- max(points$x)

yMin <- min(points$y)
yMax <- max(points$y)

range(points$x * (xMax - xMin) + xMin)
range(points$y * (yMax - yMin) + yMin)

points <- mutate(points,

                 k = as.factor(k),
                 r = sqrt(x^2+y^2),
                 alpha = acos(y/r) * 180 / pi
                 
                 # cosAlpha = y/r,
                 # xn = (x - min(x))/(max(x)-min(x)),
                 # yn = (y - min(y))/(max(y)-min(y)),
)


points
range(points$x); range(points$y)

check <- mutate(points,
       
        xReinc = r*sin(alpha * pi / 180),
        yReinc = r*cos(alpha * pi / 180)
       # k = as.factor(k),
       # r = sqrt(x^2+y^2),
       # alpha = acos(y/r) * 180 / pi
       
       # cosAlpha = y/r,
       # xn = (x - min(x))/(max(x)-min(x)),
       # yn = (y - min(y))/(max(y)-min(y)),
)
check

sum(check$x - check$xReinc)
identical(check$x, as.integer(check$xReinc))

# создадим линейный вид координат
pointsUn <- gather(points, "xy", "coord", 1:2) %>%
    mutate(k = as.factor(k),
           student = as.factor(student),
           xy = as.factor(xy))

# создадим линейный вид переменных
pointsUnFeature <- gather(points, "feature", "value", 5:6) %>%
        mutate(k = as.factor(k),
               student = as.factor(student),
               feature = as.factor(feature))