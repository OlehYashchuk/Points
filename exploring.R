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
                 xn = (x - min(x))/(max(x)-min(x)),
                 yn = (y - min(y))/(max(y)-min(y)),
                 k = as.factor(k),
                 # r = sqrt((x-300)^2+(y-300)^2),
                 r = sqrt((xn-0)^2+(yn-0)^2),
                 angle = acos(yn)#  * 180 / pi
                 
                 # angle = acos((0-0)*(x-0)+(1-0)*(y-0))  * 180 / pi,
                 # angle = acos(c(0-0, 1-0) %*% c(x-0, y-0)) * 180 / pi
                 # angle = (atan2(600, 300) - atan2(y, x)) * 360 / (2*pi),
) #%>% mutate(x = r, y = angle) %>%
  #  select(-r, -angle)

points
range(points$x); range(points$y)



points$x[1]
acos(c(0-0, 1-0) %*% c(points$x[1]-0, points$y[1]-0)) * 180 / pi

A <- c(0,1); O <- c(0,0); B <- c(.5,.5) 
acos((A-O) %*% (B-O)) * 180 / pi 


c(B[1] - O[1], B[2] - O[2])
acos((c(0,1)-c(0,0)) %*% (c(x, y) - c(0,0)))
c(0-0, 1-0)

range(ui$xn);range(ui$yn)

acos(sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
acos(sum(c(x,xc)*c()) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )

angle = atan2(a.y, a.x) - atan2(b.y, b.x);
range(ui$angle)

if (ui$angle < 0) {
        ui$angle <- ui$angle + 360
}

cos((x*300+y*0)/
            sqrt((x^2 + y^2)*(300^2+0^2)))*100



# создадим линейный вид данных
pointsUn <- gather(points, "xy", "coord", 1:2) %>%
    mutate(k = as.factor(k),
           student = as.factor(student),
           xy = as.factor(xy))

###############################################################################
# Разведочный анализ данных
###############################################################################
# Рассеивание точек
ggplot(data = points, aes(x, y)) +
        geom_point(aes(fill = factor(student)),
                   alpha=0.7, size=2.5, shape=21) + 
        # scale_x_discrete(limits = seq(limMin, limMax, by = limMax/p)) +
        # scale_y_discrete(limits = seq(limMin, limMax, by = limMax/p)) +
        theme(legend.position = "none") + guides(fill = 'none') +
        theme_bw() # + labs(title = 'Рассеивание точек')

# ggsave(paste("Графики/Points.png", sep=""), last_plot(), height = 6, width = 6)

# Рассеивание точек по шагам
ggplot(points, aes(x, y)) + 
        geom_point(aes(color=student),
                   alpha=0.7, size=2.5, 
                   show.legend = FALSE) +
        facet_wrap(~k, labeller = label_both)+
        theme_bw() # + labs(title = 'Поведение респондентов')
        
# ggsave("Графики/Steps.png", last_plot(), height = 7, width = 12)

###############################################################################
# Плотность распределения
###############################################################################
ggplot(pointsUn[which(pointsUn$xy=="x"),], aes(coord)) + 
        labs(x = "x") +    
        geom_density() +
        facet_wrap(xy~k, labeller = labeller(.multi_line = FALSE)) +
        theme_bw()

# ggsave(paste("./Графики/StudentsDensity_y.png", sep=""), last_plot(),
#        height = 7, width = 10)

# Описательные статистики
# sapply(pointsUn, mean)

# ggplot() + aes(c1) + geom_histogram(binwidth = 15)
# ggplot() + aes(c1) + geom_density()
