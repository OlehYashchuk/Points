source("knn.R")
source("rmse.R")

# mCor$y <- cor(pointsCor$x)

head(pointsCor$x, 5)

a <- matrix(runif(25, 0, 600), 5, 5)
b <- ccf(a[1,], a[2,])

# plot(a[1,], a[2,])

ggplot() + geom_line(aes(x = c(1:5), y = a[1, ])) +
        geom_line(aes(x = c(1:5), y = a[2, ]))

b <- acf(a)
b$acf

acf(matrix(pointsCor$x))

str(unlist(pointsCor$x))

as.data.frame(pointsCor$x)
b <- acf(pointsCor$x, lag.max = 0)


install.packages("irr")
library(irr)

kendall(a[2:3,])

t(a)
a

kendall(t(pointsCor$x))




listRMSE <- list()
pointsCor <- list()

selectedSteps <- 15

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
s <- 0

tic()
for (i in stepsK) {
        s <- s + 1

        # c <- knn(pointsCor, i, corMethod = "pearson")
        kRMSE$k[s] <- i
        kRMSE$pearsonReal[s] <- rmse(knn(pointsCor, i, corMethod = "pearson"), 
                                     pointsCor, from = 1, to = 52)
        kRMSE$spearmanReal[s] <- rmse(knn(pointsCor, i, corMethod = "spearman"), 
                                      pointsCor, from = 1, to = 52)
}
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