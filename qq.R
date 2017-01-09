###############################################################################
# Построение графиков ку-ку
###############################################################################
qqplot.data <- function (vec) # argument: vector of numbers
{
        # following four lines from base R's qqline()
        y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
        x <- qnorm(c(0.25, 0.75))
        slope <- diff(y)/diff(x)
        int <- y[1L] - slope * x[1L]
        
        d <- data.frame(resids = vec)
        
        ggplot(d, aes(sample = resids)) + 
                stat_qq() + 
                geom_abline(slope = slope, intercept = int) 
}        

qqplot.data(points$x)
qqplot.data(points$y)
qqplot.data(pointsUn$coord)

tapply(pointsUn$coord, pointsUn$xy, qqplot.data)
tapply(points$x, points$student, qqplot.data) 

ggplot(pointsUn[which(pointsUn$xy=="x"),]) +
        geom_qq(aes(sample = coord)) +
        # geom_abline(intercept=quantile(c(0.25, 0.75))[1]-
        #                     diff(quantile(c(0.25, 0.75)))/
        #                     diff(qnorm(c(0.25, 0.75)))*
        #                     quantile(c(0.25, 0.75)), 
        #             slope=diff(quantile(c(0.25, 0.75)))/
        #                     diff(qnorm(c(0.25, 0.75)))) + 
        facet_wrap(xy~student, labeller = labeller(.multi_line = F)) +
        theme_bw()
# qqline()

ggsave(paste("Графики/qqX.png", sep=""), last_plot(), height = 7, width = 10)
