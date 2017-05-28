par(family = "HCR Dotum LVT")
Years <- c(1404, 1406, 1432, 1648, 1657, 1669, 1672, 1678, 1717, 1724, 1726, 1777)
Households <- c(153403, 180246, 201853, 441321, 658771, 1313453, 1178144, 1342428, 1560561, 1572086, 1576598, 1715371)
Population <- c(322746, 370365, 692475, 1531365, 2290083, 5018644, 4701359, 5246972, 6846568, 6865286, 7032425, 7238546)
chosun.df <- data.frame(Years, Households, Population)
str(chosun.df)
plot(Population / 10000 ~ Years, 
     data = chosun.df, 
     type = "b", 
     pch = 21, 
     col = "red", 
     bg = "white",
     ylim = c(0, 800),
     xaxt = "n",
     yaxt = "n",
     ann = FALSE)
lines(Households / 10000 ~ Years, 
      data = chosun.df, 
      type = "b",
      pch = 21,
      col = "blue",
      bg = "white")
Years.ticks <- c(1404, 1432, 1648, 1669, 1678, 1717, 1777)
Years %in% Years.ticks
Households.ticks <- Households[Years %in% c(1404, 1669, 1777)]
Population.ticks <- Population[Years %in% c(1404, 1432, 1648, 1657, 1669, 1717, 1777)]
y.breaks <- c(Population.ticks, Households.ticks) / 10000
y.labels <- format(c(Population.ticks, Households.ticks) / 10000, digits = 3, nsmall = 0)
axis(side = 1, 
     at = Years.ticks, 
     labels = Years.ticks, 
     las = 2)
axis(side = 2, 
     at = c(Population.ticks, Households.ticks) / 10000, 
     labels = format(c(Population.ticks, Households.ticks) / 10000, digits = 3, nsmall = 0),
     las = 2)
legend("topleft", 
       inset = 0.05, 
       legend = c("인구", "호수"), 
       lty = 1,
       col = c("red", "blue"))
text(x = 1700, 
     y = c(500, 100),  
     labels = c("인구", "호수"))
main.title <- "조선시대 호수와 인구수의 변화"
x.lab <- "연도"
y.lab <- "호수와 인구수(단위 만)"
title(main = main.title, 
      xlab = x.lab, 
      ylab = y.lab)
dev.copy(png, 
         file = "../pics/chosun_demo.png", 
         width = 800, 
         height = 450)
dev.off()
library(ggplot2)
library(reshape2)
source("./theme_kr.R")
chosun.melt <- melt(chosun.df, 
                    id.vars = "Years", 
                    measure.vars = c("Households", "Population"),
                    variable.name = "Variable", 
                    value.name = "Counts")
str(chosun.melt)
g1 <- ggplot(data = chosun.melt, 
             mapping = aes(x = Years, 
                           y = Counts / 10000, 
                           colour = Variable)) + 
  geom_line() + 
  geom_point(shape = 21, 
             fill = "white", 
             size = 3, 
             show.legend = FALSE) 
g1
g2 <- g1 + 
  theme_bw() + 
  theme.kr 
g2
g3 <- g2 + 
#  theme(panel.grid.major = element_line(linetype = "dotted", colour = "black")) +
  scale_x_continuous(name = x.lab, 
                     breaks = Years.ticks, 
                     labels = Years.ticks) +
  theme(axis.text.x = element_text(angle = 90))
g3
g4 <- g3 +
  scale_y_continuous(name = y.lab,
                     breaks = y.breaks,
                     labels = y.labels) +
  theme(axis.text.y = element_text(angle = 0))
g4
g5 <- g4 + scale_colour_manual(name = "구분", 
                               values = c("red", "blue"), 
                               labels = c("호수", "인구수")) 
g5
g6 <- g5 + 
  theme(legend.position = c(0.2, 0.8), 
        legend.background = element_rect(colour = "black", 
                                         linetype = "solid"))
g6
g7 <- g6 + 
  ggtitle(main.title) +
  annotate("text", 
           x = 1700, 
           y = c(500, 100),  
           label = c("인구", "호수"),
           family = "HCR Dotum LVT")
g7
ggsave("../pics/chosun_demo_ggplot.png", width = 9, height = 81/16, units = "in", dpi = 72)
