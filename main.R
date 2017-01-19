source("knn.R")
source("rmse.R")
source("concordance.R")
# source("massCor.R")

# Число точек учавтсвующих в предсказании
selectedSteps <- 10

# Данные в матричном виде
pointMatrix <- list()

pointMatrix$x <- pointsUn %>% 
        mutate(k = as.numeric(k)) %>%
        filter(xy == 'x', k <= selectedSteps) %>% 
        select(-c(r, alpha)) %>% #, xn, yn
        spread(student, coord) %>%
        select(-c(k, xy))
    
pointMatrix$y <- pointsUn %>% 
        mutate(k = as.numeric(k)) %>%
        filter(xy == 'y', k <= selectedSteps) %>% 
        select(-c(r, alpha)) %>% #, xn, yn
        spread(student, coord) %>%
        select(-c(k, xy))
 
pointMatrix$r <- pointsUnFeature %>% 
        mutate(k = as.numeric(k)) %>%
        filter(feature == 'r', k <= selectedSteps) %>% 
        select(-c(x, y)) %>% #, xn, yn
        spread(student, value) %>%    
        select(-c(k, feature))

pointMatrix$alpha <- pointsUnFeature %>% 
        mutate(k = as.numeric(k)) %>%
        filter(feature == 'alpha', k <= selectedSteps) %>% 
        select(-c(x, y)) %>% #, xn, yn
        spread(student, value) %>%    
        select(-c(k, feature))

corMatrix <- list()
corMatrix$r <- cor(pointMatrix$r, method = 'spearman')
corMatrix$alpha <- cor(pointMatrix$alpha, method = 'spearman')

# corMatrix$r <- concordance(pointMatrix$r, pointMatrix$alpha)

# predicted <- list()
# predicted$r <- knn(pointMatrix$r, corMatrix$r, 5)
# predicted$alpha <- knn(pointMatrix$alpha, corMatrix$alpha, 5)
# predicted$x <- predicted$r * sin(predicted$alpha * pi / 180)
# predicted$y <- predicted$r * cos(predicted$alpha * pi / 180)
# sapply(predicted, range)

# набор кол-ва ближайших соседей k
stepsK <- c(1:5, seq(10, 50, 5))

# RMSE <- list()
s <- 0
tic()
for (i in stepsK) {
        s <- s + 1
        
        predicted <- list()
        predicted$r <- knn(pointMatrix$r, corMatrix$r, i)
        predicted$alpha <- knn(pointMatrix$alpha, corMatrix$alpha, i)
        predicted$x <- predicted$r * sin(predicted$alpha * pi / 180)
        predicted$y <- predicted$r * cos(predicted$alpha * pi / 180)
        
        RMSE$k[s] <- i
        RMSE$pearson[s] <- rmse(true = pointMatrix, estimate = predicted)
        # print(RMSE$pearson[s])
}
# save(RMSE, file = "RMSE_new_features.Rdata")
toc()
sapply(RMSE, min)

# save(RMSE_modified, file = "Данные/RMSE_modified.Rdata")
# load("Данные/RMSE_modified.Rdata", verbose = TRUE)




################################################################################

plotFrom <- 2

kRMSEtbl <- RMSE_initial %>% tbl_df %>% filter(k >= plotFrom)
kRMSEtbl$pearson_mod <- RMSE_modified$pearson[RMSE_modified$k >= plotFrom]

ggplot(kRMSEtbl, aes(x = k)) + 
        
        geom_point(alpha=0.7, size=1.5, aes(y = pearson)) +
        # geom_point(alpha=0.7, size=1.5, aes(y = spearman)) +
        # geom_point(alpha=0.7, size=1.5, aes(y = concord)) +
        geom_point(alpha=0.7, size=1.5, aes(y = pearson_mod)) +
        
        
        geom_line(aes(y = pearson), color = "red") +
        # geom_line(aes(y = spearman), color = "blue") +
        # geom_line(aes(y = concord), color = "dark blue") +
        geom_line(aes(y = pearson_mod), color = "blue") +
        
        
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