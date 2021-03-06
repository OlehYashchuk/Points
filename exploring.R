###############################################################################
# ����������� ������ ������
###############################################################################
limMin <- 0                      # ������� �������
limMax <- 600                    # ������� �������

# ����������� �����
ggplot(data = points, aes(x, y)) +
        geom_point(aes(fill = factor(student)),
                   alpha=0.7, size=2.5, shape=21) + 
        # scale_x_discrete(limits = seq(limMin, limMax, by = limMax/p)) +
        # scale_y_discrete(limits = seq(limMin, limMax, by = limMax/p)) +
        theme(legend.position = "none") + guides(fill = 'none') +
        theme_bw() # + labs(title = '����������� �����')

# ggsave(paste("�������/Points.png", sep=""), last_plot(), height = 6, width = 6)

# ����������� ����� �� �����
ggplot(points, aes(x, y)) + 
        geom_point(aes(color=student),
                   alpha=0.7, size=2.5, 
                   show.legend = FALSE) +
        facet_wrap(~k, labeller = label_both)+
        theme_bw() # + labs(title = '��������� ������������')

# ggsave("�������/Steps.png", last_plot(), height = 7, width = 12)

###############################################################################
# ��������� �������������
###############################################################################
ggplot(pointsUn[which(pointsUn$xy=="x"),], aes(coord)) + 
        labs(x = "x") +    
        geom_density() +
        facet_wrap(xy~k, labeller = labeller(.multi_line = FALSE)) +
        theme_bw()

# ggsave(paste("./�������/StudentsDensity_y.png", sep=""), last_plot(),
#        height = 7, width = 10)

# ������������ ����������
# sapply(pointsUn, mean)

# ggplot() + aes(c1) + geom_histogram(binwidth = 15)
# ggplot() + aes(c1) + geom_density()
