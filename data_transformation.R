rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(nortest)
library(corrplot)
library(tictoc)
library(irr)

getwd()
# setwd("d:/Oleh/Репетиторство/Points")
# setwd("D:/D/Репетиторство/Points")

points <- read.table("Данные/points.txt", 
                     header = F, 
                     col.names = c("x", "y"), 
                     sep = ";")

###############################################################################
# data preprocessing

p <- 15                          # number of points
st <- dim(points)[1]/p           # numbers of users
k <- rep(1:15, times = dim(points)[1]/p)        # numbers of steps
student <- rep(1:st, each = dim(points)[1]/st)  # numbers of students

# mergin data
points <- tbl_df(cbind(points, k, student))
colnames(points) <- c("x", "y", "k", 'student')

summary(points)                  # base statistics

points[which(points$x>600 | points$y>600),] # looking for outliers
# Чистка данных
exclude <- c(5, 14, 15, 20, 22, 24, 34, 37, 42, 44, 48)
'%!in%' <- function(x,y)!('%in%'(x,y)) # define a new operator like "!in"

# excluding outliers
points <- points %>% 
        filter(x <= 600, y <= 600) %>% 
        filter(student %!in%  exclude) %>%
        select(-student) 

# data processing technical step
st <- dim(points)[1]/p
student <- rep(1:st, each = dim(points)[1]/st)

points <- mutate(points, 
                 student = student,
                 k =  k)

###############################################################################
# generation of new points

addStudents <- 1000 # number of new users

set.seed(998)           # for reproducibility
x <- rnorm(addStudents * p, 300, 73) # Gaussian x coord
set.seed(999)           
y <- rnorm(addStudents * p, 300, 73) # Gaussian y coord
range(x); range(y)

# data processing technical steps
st <- st + addStudents # total number of users
student <- rep((st-addStudents+1):st, each = p) # numbers of new users
k <- rep(1:15, times = addStudents) # numbers of steps of new users
pointsNew <- tbl_df(cbind(x, y, k, student)) # merging new dataset

points <- bind_rows(points, pointsNew) # merging all data in one dataset

# assignment data types
points <- mutate(points,
                 x = as.integer(x),
                 y = as.integer(y),
                 k = as.factor(k),
                 student = as.factor(student))

# creating new variables
points <- mutate(points,

                 k = as.factor(k),
                 r = sqrt(x^2+y^2),
                 alpha = acos(y/r) * 180 / pi

                 # cosAlpha = y/r,
                 # xn = (x - min(x))/(max(x)-min(x)),
                 # yn = (y - min(y))/(max(y)-min(y)),
)

# tidy set of x an y
pointsUn <- gather(points, "xy", "coord", 1:2) %>%
        select(-c(r, alpha)) %>%
        mutate(k = as.factor(k),
               student = as.factor(student),
               xy = as.factor(xy))

# tidy set of new variables
pointsUnFeature <- gather(points, "feature", "value", 5:6) %>%
        select(-c(x, y)) %>%
        mutate(k = as.factor(k),
               student = as.factor(student),
               feature = as.factor(feature))









###############################################################################
# test space

# normalization
# points <- mutate(points,
#                  
#                  x = x / 600,
#                  y = y / 600
# )
# points <- mutate(points,
#                  
#                  k = as.factor(k),
#                  r = sqrt(x^2+y^2),
#                  alpha = acos(y/r) #* 180 / pi
# )


# checking the correctness of the new variables and normalization
points
range(points$x); range(points$y)
range(points$r); range(points$alpha)

check <- mutate(points,
                
                # xReinc = r*sin(alpha * pi / 180),
                # yReinc = r*cos(alpha * pi / 180)
                
                xReinc = r*sin(alpha) * 600,
                yReinc = r*cos(alpha) * 600
                
                # k = as.factor(k),
                # r = sqrt(x^2+y^2),
                # alpha = acos(y/r) * 180 / pi
                
                # cosAlpha = y/r,
                # xn = (x - min(x))/(max(x)-min(x)),
                # yn = (y - min(y))/(max(y)-min(y)),
)

sum(check$x - check$xReinc)
identical(check$x, as.integer(check$xReinc))
