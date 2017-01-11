source("knn.R")
source("rmse.R")
a <- list()
a$x <- matrix(runif(40, 0, 600), 5, 8)
a$y <- matrix(runif(40, 0, 600), 5, 8)
# ggplot() + geom_line(aes(x = c(1:5), y = a[1, ])) +
#         geom_line(aes(x = c(1:5), y = a[2, ]))

concord <- matrix(ncol = 8, nrow = 8)

for (i in 1:5) {
        for (j in 1:8) {
concord[i, j] <- kendall(t(cbind(data.frame(a$x[,c(i,j)]),
                                 data.frame(a$y[,c(i,j)]))))$value
        # concord[j, i] <- concord[i, j]
        }
}
for (i in 1:8) {
        for (j in 1:8) {
                concord[j, i] <- kendall(t(cbind(data.frame(a$x[,c(j,i)]),
                                                 data.frame(a$y[,c(j,i)]))))$value
                # concord[j, i] <- concord[i, j]
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


sqrt(
        mean(
                sqrt(
                        # [c_x^(i,j)-c_x(i,j)]^2
                        (c$x[15,] - 
                                 c$x[15,])^2 +
                                
                                # [c_y^(i,j)-c_y(i,j)]^2
                                (pointsCor$y[15,] - 
                                         pointsCor$y[15,])^2
                )^2
        )
)



sum(is.na(c$x)) + sum(!is.na(c$x))
sum(is.na(concord)) + sum(!is.na(concord))








tic()
listRMSE <- list()
pointsCor <- list()

for (j in 5:15) {
selectedSteps <- j

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
stepsK <- c(1:5, seq(10, 50, 5))

kRMSE <- list()
# s <- 0
for (i in stepsK) {
        s <- s + 1
        # kRMSE$k[s] <- list()
        # c <- knn(pointsCor, i, corMethod = "pearson")
        kRMSE$j[s] <- j
        kRMSE$k[s] <- i
        kRMSE$pearson[s] <- rmse(knn(pointsCor, i, corMethod = "pearson"))
                                     # pointsCor, from = 1, to = 52)
        kRMSE$spearman[s] <- rmse(knn(pointsCor, i, corMethod = "spearman"))
                                      # pointsCor, from = 1, to = 52)
        kRMSE$concord[s] <- rmse(knn(pointsCor, i, corMethod = "concord"))
}
}
save( kRMSE, file = "kRMSE_concord.Rdata")
toc()



sapply(kRMSE, min)

# listRMSE$Pearson[j] <- min(kRMSE)
# listRMSE$Spearman[j] <- min(kRMSE)
# listRMSE$Kendall[j] <- rmse(c, pointsCor)

# print(paste('j = ', j, '| rmse = ', min(kRMSE, na.rm = T)))
# plot(kRMSE, type = 'o', xlim = c(1, max(stepsK)))

plotFrom <- 2

kRMSEtbl <- kRMSE %>% tbl_df %>% filter(k >= plotFrom)

ggplot(kRMSEtbl, aes(x = k)) + 
        
        geom_point(alpha=0.7, size=1.5, aes(y = pearson)) +
        geom_point(alpha=0.7, size=1.5, aes(y = spearman)) +
        
        geom_line(aes(y = pearson), color = "red") +
        geom_line(aes(y = spearman), color = "blue") +
        
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
        # geom_text(aes(x = k[which(pearson == min(pearson))],
        #               y = min(pearson),
        #               label = round(pearson[which(pearson == min(pearson))])),
        #           hjust=0,vjust=-1, size = 4) +
        # 
        # geom_text(aes(x = k[which(spearman == min(spearman))],
        #               y = min(spearman),
        #               label = round(spearman[which(spearman == min(spearman))])),
        #           hjust=0,vjust=-1, size = 4) +

        labs(x = 'k Nearest Neighbors', y = 'RMSE')

# ggsave(paste("Графики/kNeighbors.png", sep=""), last_plot(), 
#        height = 7, width = 12)


# Графики по простому
# plot(kRMSE$k, kRMSE$pearson, type = 'o', xlim = c(1, max(stepsK)))
# plot(kRMSE$k, kRMSE$spearman, type = 'o', xlim = c(1, max(stepsK)))