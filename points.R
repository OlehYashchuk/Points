source("knn.R")
source("rmse.R")

selectedSteps <- 10

pointsCor <- list()
# Данные в матричном виде
# матрица (k, x)
pointsCor$x <- pointsUn %>% 
        spread(student, coord) %>%
        mutate(k = as.numeric(k)) %>%
        filter(xy == 'x', k <= selectedSteps) %>% 
        select(-c(k, xy))

# матрица (k, y)
pointsCor$y <- pointsUn %>% 
        spread(student, coord) %>% 
        mutate(k = as.numeric(k)) %>%
        filter(xy == 'y', k <= selectedSteps) %>% 
        select(-c(k, xy))


a <- list()
a$x <- matrix(runif(40, 0, 600), 5, 8)
a$y <- matrix(runif(40, 0, 600), 5, 8)
# ggplot() + geom_line(aes(x = c(1:5), y = a[1, ])) +
#         geom_line(aes(x = c(1:5), y = a[2, ]))

concord <- matrix(ncol = 8, nrow = 8)
concord1 <- matrix(ncol = 8, nrow = 8)
a$x <- t(a$x)
a$y <- t(a$y)

for (i in 1:8) {
        for (j in 1:8) {
                concord1[j, i] <- kendall((cbind(data.frame(a$x[c(j,i),]),
                                                 data.frame(a$y[c(j,i),]))))$value
        }
}

isSymmetric(concord)

concord
students1 <- as.factor(c(1:1052))
trueStudents1 <- which(colnames(c$x) %in% students1)
pointsCor
sum(trueStudents1)

knn(pointsCor, 1, corMethod = "concord")
kendall(c(pointsCor$x[,1:2], pointsCor$y[,1:2]))

cp <- knn(pointsCor, 5, corMethod = "pearson")
# c <- knn(pointsCor, 5, corMethod = "concord")
tic()
# c1 <- knn(pointsCor, 5, corMethod = "concord") 
toc()

rmse(cp, pointsCor)

sum(is.na(c$x)) + sum(!is.na(c$x))
sum(is.na(concord)) + sum(!is.na(concord))


tic()
listRMSE <- list()
pointsCor <- list()
s <- 0
for (j in 10:10) {
selectedSteps <- 10

# Данные в матричном виде
# матрица (k, x)
pointsCor$x <- pointsUn %>% 
        spread(student, coord) %>%
        mutate(k = as.numeric(k)) %>%
        filter(xy == 'x', k <= selectedSteps) %>% 
        select(-c(k, xy))

# матрица (k, y)
pointsCor$y <- pointsUn %>% 
        spread(student, coord) %>% 
        mutate(k = as.numeric(k)) %>%
        filter(xy == 'y', k <= selectedSteps) %>% 
        select(-c(k, xy))

# набор кол-ва ближайших соседей k
# stepsK <- c(1:5, seq(10, 15, 5))

stepsK <- c(seq(40, 50, 5))
stepsK <- 500
# kRMSE <- list()
# s <- 0
tic()
for (i in stepsK) {
        s <- s + 1
        # kRMSE$k[s] <- list()
        # c <- knn(pointsCor, i, corMethod = "pearson")
        # kRMSE$j[s] <- j
        kRMSE$k[s] <- i
        # kRMSE$pearson[s] <- rmse(knn(pointsCor, i, corMethod = "pearson"), pointsCor)
        #                              # pointsCor, from = 1, to = 52)
        # kRMSE$spearman[s] <- rmse(knn(pointsCor, i, corMethod = "spearman"), pointsCor)
        #                               # pointsCor, from = 1, to = 52)
        kRMSE$concord[s] <- rmse(knn(pointsCor, i, corMethod = "concord"), pointsCor)
}
}
save( kRMSE, file = "kRMSE_concord.Rdata")
toc()

data.frame(round(kRMSE$concord, 3))

sapply(kRMSE, round)

# listRMSE$Pearson[j] <- min(kRMSE)
# listRMSE$Spearman[j] <- min(kRMSE)
# listRMSE$Kendall[j] <- rmse(c, pointsCor)

# print(paste('j = ', j, '| rmse = ', min(kRMSE, na.rm = T)))
# plot(kRMSE, type = 'o', xlim = c(1, max(stepsK)))



load("Данные/kRMSEp.RData", verbose = TRUE)

kRMSEp$concord <- kRMSE$concord[1:14]
RMSE <- kRMSEp
save(RMSE, file = "Данные/RMSE.Rdata")

plotFrom <- 2

kRMSEtbl <- kRMSEp %>% tbl_df %>% filter(k >= plotFrom)

ggplot(kRMSEtbl, aes(x = k)) + 
        
        geom_point(alpha=0.7, size=1.5, aes(y = pearson)) +
        geom_point(alpha=0.7, size=1.5, aes(y = spearman)) +
        geom_point(alpha=0.7, size=1.5, aes(y = concord)) +
        
        geom_line(aes(y = pearson), color = "red") +
        geom_line(aes(y = spearman), color = "blue") +
        geom_line(aes(y = concord), color = "dark blue") +
        
        scale_x_discrete(limits = kRMSEtbl$k) + 
        coord_cartesian(xlim = c(0, 52)) + 
        
        # geom_point(alpha=1, size=3, color = 'red',
        #            aes(x = k[which(pearson == min(pearson))],
        #                y = min(pearson))) +
        # 
        # geom_point(alpha=1, size=3, color = 'blue',
        #            aes(x = k[which(spearman == min(spearman))],
        #                y = min(spearman), label = spearman)) +
        # 
        # geom_point(alpha=1, size=3, color = 'blue',
        #            aes(x = k[which(concord == min(concord))],
        #                y = min(concord), label = concord)) +
        # 
        # geom_text(aes(x = k[which(pearson == min(pearson))],
        #               y = min(pearson),
        #               label = round(pearson[which(pearson == min(pearson))])),
        #           hjust=0,vjust=-1, size = 4) +
        # 
        # geom_text(aes(x = k[which(spearman == min(spearman))],
        #               y = min(spearman),
        #               label = round(spearman[which(spearman == min(spearman))])),
        #           hjust=0,vjust=-1, size = 4) +
        # 
        # geom_text(aes(x = k[which(concord == min(concord))],
        #               y = min(concord),
        #               label = round(concord[which(concord == min(concord))])),
        #           hjust=0,vjust=-1, size = 4) +

        labs(x = 'k Nearest Neighbors', y = 'RMSE')

# ggsave(paste("Графики/kNeighbors_3-RMSE.png", sep=""), last_plot(),
#        height = 7, width = 12)


# Графики по простому
# plot(kRMSE$k, kRMSE$pearson, type = 'o', xlim = c(1, max(stepsK)))
# plot(kRMSE$k, kRMSE$spearman, type = 'o', xlim = c(1, max(stepsK)))