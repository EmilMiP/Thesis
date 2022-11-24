library(dplyr)
library(ggplot2)

color_wrapped = function(x) {
  y = dnorm(x)
  y[x < qnorm(0.05, lower.tail = F)] <- NA
  return(y)
}

p1 <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = color_wrapped, n = 10001, geom = "area", fill = "blue", alpha = 0.2) +
  stat_function(fun = dnorm, n = 10001) +
  ylab("") +
  scale_y_continuous(breaks = NULL,expand = c(0,0)) +
  scale_x_continuous(breaks = c(-2, 0, qnorm(0.05, lower.tail = F), 2), labels = c("-2","0", "T", "2")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Liability", title = "Liability threshold model") +
  geom_segment(x = qnorm(0.05, lower.tail = F),
               xend = qnorm(0.05, lower.tail = F),
               y = 0,
               yend = dnorm(qnorm(0.05, lower.tail = F)),
               size = .4,
               lty = "dashed") +
  annotate("text", label = "Cases", x = 2.4, y = 0.075) +
  annotate("text", label = "Controls", x = 0, y = 0.075) +
  coord_cartesian(ylim = c(0,dnorm(0)+0.01))

p1
setwd("U:/afhandling/methods")
ggsave(filename = "LTM.png",
       plot = p1,
       device = "png",
       width = 5,
       height = 3.5,
       dpi = 600)
