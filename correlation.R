###############################################################################
# Корреляция 
###############################################################################
selectedSteps <- 52

pointsCor <- list()
pointsCor$x <- pointsUn %>% spread(student, coord) %>%
        mutate(k = as.numeric(k)) %>%
        filter(xy == 'x', k <= selectedSteps) %>% select(-c(k, xy))
pointsCor$y <- pointsUn %>% spread(student, coord) %>% 
        mutate(k = as.numeric(k)) %>%
        filter(xy == 'y', k <= selectedSteps) %>% select(-c(k, xy))

mCor <- list()
mCor$xP <- cor(pointsCor$x)
mCor$yP <- cor(pointsCor$y)
mCor$xS <- cor(pointsCor$x, method = 'spearman')
mCor$yS <- cor(pointsCor$y, method = 'spearman')

col3 <- colorRampPalette(c("red", "white", "blue"))

png(file = 'Графики/Spearman_correlation_y.png', width = 800, height = 800)
corrplot(mCor$yS, method = "ellipse",  type="full", col=col3(200)) 
dev.off()
# dev.cur()

# order="hclust", 
# title = "Кореляція Пирсона, y")
# dev.cur()
# ggsave("Графики/Pearson_x.png", dev.cur(), height = 7, width = 10)

corrplot(mCor$yS, method = "ellipse", col=col3(200), 
         # type="lower", order="hclust", 
         title = "Кореляція Спірмена, y")



########################################
# Рассчет примера
########################################
mCor$xS[12, 11]

cor(points$x[points$student==11], points$x[points$student==12])

covv <- mean(points$x[points$student==11] * points$x[points$student==12]) -
        mean(points$x[points$student==11]) * mean(points$x[points$student==12])

mn <- sqrt(14 / 15)

sdd <- (sd(points$x[points$student==11])*mn) *
        (sd(points$x[points$student==12])*mn)

covv / sdd

plot(points$x[points$student==11], points$x[points$student==12])

sort(a)
sort(b)
ar <- rank(a)
br <- rank(b)
1-6*sum((ar-br)^2)/(15*(15^2-1))


sd(a)*mn
a <- points$x[points$student==11]
b <- points$x[points$student==12]
am <- mean(a)
an <- (a-am)^2
sqrt(sum(an)/14)*mn

sum(a*b/15)

# wb <- c("white","black")
# corrplot(Mx, order="hclust", addrect=2, col=wb, bg="gold2")
# corrplot(Mx, order="hclust", addrect=3)
# corrplot.mixed(Mx, lower="ellipse", upper="circle")
# corrplot(Mx, order="AOE", cl.pos="b", tl.pos="d", tl.srt=60)
###############################################################################