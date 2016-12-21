rm(list = ls())

getwd()
setwd("d:/D/Репетиторство")

points <- read.table("points.txt", header = F, col.names = c("x", "y"), sep = ";")
k <- rep(1:15, times = dim(points)[1]/15)
student <- rep(1:15, each = dim(points)[1]/15)

points <- tbl_df(cbind(points, k, student))
colnames(points) <- c("x", "y", "k", 'student')
summary(points)

points

ggplot(points, aes(x, y)) + 
        geom_point(alpha=0.4, size=2.5, color=k) + #, shape=student
        # scale_x_continuous(limits = c(0, 600)) +
        # scale_y_continuous(limits = c(0, 600)) +
        # scale_x_discrete(limits=seq(0, 600, by = 600/4)) +
        scale_y_discrete(limits=seq(0, 600, by = 600/8)) +
        facet_wrap(~student, labeller = label_both)+
        theme_bw()
ggsave(paste("plot.png", sep=""), last_plot(), height = 7, width = 7)

ggplot(data = points, aes(x, y)) +
        geom_point(alpha=0.2, size=2, color=k) + #, shape=student
        # scale_x_continuous(limits = c(0, 600)) +
        # scale_y_continuous(limits = c(0, 600)) +
        scale_x_discrete(limits=seq(0, 600, by = 600/15)) +
        scale_y_discrete(limits=seq(0, 600, by = 600/15)) +
                theme_gray()

ggsave(paste("plot.png", sep=""), last_plot(), height = 4, width = 4)

# ggsave(paste("Графики/power_p",p,"_d",d,".png", sep=""), last_plot(), scale = 1)



