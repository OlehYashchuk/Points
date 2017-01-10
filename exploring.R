rm(list = ls())

# ����������� �������
library(dplyr)
library(tidyr)
library(ggplot2)
library(nortest)
library(corrplot)
library(tictoc)

# �������� ������� ����������
getwd()
# setwd("d:/Oleh/�������������/Points")
# setwd("D:/D/�������������/Points")

# ������ ������
points <- read.table("������/points.txt", 
                     header = F, col.names = c("x", "y"), sep = ";")
# ������� �������
limMin <- 0
limMax <- 600
# ���-�� ����� ��� ������� �����������
p <- 15
# ���-�� ������������
st <- dim(points)[1]/p
# ��������������� ������� ��� ������� ������ ���� � ����������� 
k <- rep(1:15, times = dim(points)[1]/p)
student <- rep(1:st, each = dim(points)[1]/st)

# ����������� ������
points <- tbl_df(cbind(points, k, student))
colnames(points) <- c("x", "y", "k", 'student')
# ������� ������������ ����������
summary(points)
# ����� ��������
points[which(points$x>600 | points$y>600),]
# ������ ��������
exclude <- c(5, 14, 15, 20, 22, 24, 34, 37, 42, 44, 48)
'%!in%' <- function(x,y)!('%in%'(x,y))

points <- points %>% 
        filter(x <= 600, y <= 600) %>% 
        filter(student %!in%  exclude) %>%
        select(-student) 

st <- dim(points)[1]/p
student <- rep(1:st, each = dim(points)[1]/st)

points <- points %>% mutate(student = as.factor(student), k =  as.factor(k))
    
# ���������� ��������� ��������� ������������
addStudents <- 1000

# set.seed - ��� ����������������� ���������� ������ ������
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
# ����������� ������ ������
###############################################################################
# ����������� �����
ggplot(data = points, aes(x, y)) +
        geom_point(alpha=0.7, size=2.5, shape=21, aes(fill=factor(student))) + 
        scale_x_discrete(limits=seq(limMin, limMax, by = limMax/p)) +
        scale_y_discrete(limits=seq(limMin, limMax, by = limMax/p)) +
        theme(legend.position = "none") + guides(fill = 'none') +
        # labs(title = '����������� �����') +
        theme_bw()
# ��������� ������
# ggsave(paste("�������/Points.png", sep=""), last_plot(), height = 6, width = 6)

# ����������� ����� �� �����
ggplot(points, aes(x, y)) + 
        geom_point(alpha=0.7, size=2.5, aes(color=student),show.legend = FALSE) +
        facet_wrap(~k, labeller = label_both)+
        # labs(title = '��������� ������������') +
        theme_bw()
# ��������� ������
# ggsave(paste("�������/Steps.png", sep=""), last_plot(), height = 7, width = 12)

# ��������� �������������
pointsUn <- gather(points, "xy", "coord", 1:2) %>% 
        mutate(k = as.factor(k), 
               student = as.factor(student), 
               xy = as.factor(xy))

ggplot(pointsUn[which(pointsUn$xy=="y"),], aes(coord)) + 
    labs(x = 'y') +    
    geom_density() +
    facet_wrap(xy~student, labeller = labeller(.multi_line = FALSE)) +
    theme_bw()
# ��������� ������
# ggsave(paste("./�������/StudentsDensity_y.png", sep=""), last_plot(),
#        height = 7, width = 10)

########## ������������ ����������
# sapply(pointsUn[which(pointsUn$student==9),], mean)
# sapply(pointsUn, mean)
# c1 <- tapply(pointsUn$coord, pointsUn$student, mean)
# 
# ggplot() + aes(c1) + geom_histogram(binwidth = 15)
# ggplot() + aes(c1) + geom_density()