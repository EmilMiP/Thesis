library(dplyr)
library(ggplot2)
library(LTFHPlus)


sims = simulate_under_LTM(
  fam_vec = c("m", "f", "s1"),
  h2 = .5,
  n_sim = 100e3)


hist(sims$sim_obs$o)
hist(sims$sim_obs$m)

plot(sims$sim_obs[,c("o", "m")])

sims_obs2 = mutate(sims$sim_obs, 
                   FH = sims$sim_obs %>% select(contains("status"), -o_status) %>% rowSums())

ds1 = sims$sim_obs %>% 
  slice_sample(n = 20e3) %>% 
  ungroup()

p_normal = ggplot(ds1, aes(x = o)) +
  geom_histogram(bins = 60, aes(y = ..density..), alpha = .6) +
  theme_bw() +
  theme(panel.grid = element_blank()) + 
  ylab("") +
  scale_y_continuous(breaks = NULL,expand = c(0,0)) +
  scale_x_continuous(breaks = c(-2, 0, qnorm(0.1, lower.tail = F), 2), labels = c("-2","0", "T", "2")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = "", title = "Liability threshold model") +
  geom_vline(xintercept = qnorm(0.1, lower.tail = F),
             lty = "dashed", size = 1,
             color = "red") +
  coord_cartesian(xlim = c(-3,3))

# sampled on o status
ds = sims$sim_obs %>% 
  group_by(o_status) %>% 
  slice_sample(n = 10e3) %>% 
  ungroup()

mean(ds$o_status)

p_ds = ggplot(ds, aes(x = o )) +
  geom_histogram(bins = 60, aes(y = ..density..), alpha = .6) +
  theme_bw() +
  theme(panel.grid = element_blank()) + 
  ylab("") +
  scale_y_continuous(breaks = NULL,expand = c(0,0)) +
  scale_x_continuous(breaks = c(-2, 0, qnorm(0.1, lower.tail = F), 2), labels = c("-2","0", "T", "2")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = "", title = "Over Sampling") +
  geom_vline(xintercept = qnorm(0.1, lower.tail = F),
             lty = "dashed", size = 1,
             color = "red") +
  coord_cartesian(xlim = c(-3,3))

us = sims$sim_obs %>% 
  mutate(sampling_weight = ifelse(o_status == 1, 1, 4)) %>% 
  slice_sample(n = 20e3, weight_by = sampling_weight) %>% 
  ungroup()

mean(us$o_status)

p_us = ggplot(us, aes(x = o )) +
  geom_histogram(bins = 60, aes(y = ..density..), alpha = .6) +
  theme(panel.grid = element_blank()) + 
  ylab("") +
  scale_y_continuous(breaks = NULL,expand = c(0,0)) +
  scale_x_continuous(breaks = c(-2, 0, qnorm(0.1, lower.tail = F), 2), labels = c("-2","0", "T", "2")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = "", title = "Under Sampling") +
  geom_vline(xintercept = qnorm(0.1, lower.tail = F),
             lty = "dashed", size = 1,
             color = "red") +
  coord_cartesian(xlim = c(-3,3))

# ds2 = sims_obs2 %>% 
#   mutate(FH2 = FH > 0) %>% 
#   group_by(FH2) %>% 
#   slice_sample(n = 10e3) %>% 
#   ungroup()
# 
# hist(ds2$o, breaks = 50)
# hist(ds2$g, breaks = 50)
# mean(ds2$o_status)
setwd("U:/afhandling/methods")

#combiplot = cowplot::plot_grid(p_us, p_ds, ncol = 1)
combiplot = cowplot::plot_grid(p_us, p_ds, nrow = 1)

ggsave(filename = "liablity_sampling_plot.png",
       plot = combiplot,
       device = "png",
       dpi = 300,
       width = 6,
       height = 3)

special_dots = tibble(
  x = c(0, 4, 11),
  y = c(4, 3, 2),
  shape = 4,
  type = "End"
)

start_points = tibble(
  x = c(rep(1, 4), 5),
  y = c(1:4, 3),
  shape = 16,
  type = "Start of follow-up"
)
end_points = tibble(
  x = c(rep(10, 4), 3),
  y = c(1:4, 3),
  shape = 17,
  type = "End of follow-up"
)

# tbl = tibble(x = rep(0:11, 5), y = rep(0:4, each = 12)) %>% 
#   left_join(special_dots) %>% 
#   left_join(start_points) %>% 
#   left_join(end_points) %>% 
#   mutate(shape = stringr::str_replace_all(paste0(shape1, shape2, shape3), "NA", ""),
#          shape = ifelse(shape == "", NA, shape)) %>% 
#   select(-shape1, -shape2, -shape3) %>% 
#   filter(!is.na(shape)) %>%
#   mutate(shape = as.integer(shape)) #%>% 
# #  mutate(shape = factor(shape))


censoring = bind_rows(special_dots, start_points, end_points) %>% 
ggplot(aes(x = x, y = y, fill = as.factor(shape))) +
  geom_point(data = special_dots, size = 5, shape = 4, color = "black", stroke = 1.5) +
  geom_point(data = start_points, size = 5, shape = 16, color = "black") +
  geom_point(data = end_points,   size = 5, shape = 17, color = "black") +
#  geom_point(size = 5) +
  geom_vline(xintercept = 10, lty = "dashed") +
  geom_vline(xintercept = 1 , lty = "dashed") +
  scale_x_continuous(breaks = c(1,10), labels = paste0(c("Start of study", "End of Study"))) +
  scale_fill_discrete(labels = c("Start of follow-up", "End of follow-up", "Event")) +
  guides(fill = guide_legend(override.aes = list(shape = c(16,17, 4)))) +
  scale_y_continuous(breaks = 1:4, labels = c("Control", "Right", "Interval", "Left")) +
  labs(y = "Censoring type",
       x = "",
       fill = "") +
  geom_segment(x = 1, xend = 10, y = 1, yend = 1) +
  geom_segment(x = 1, xend = 3 , y = 3, yend = 3) +
  geom_segment(x = 5, xend = 10, y = 3, yend = 3) +
  geom_segment(x = 1, xend = 10, y = 4, yend = 4) +
  geom_segment(x = 1, xend = 10, y = 2, yend = 2) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "right")

ggsave(filename = "censoring_plot.png",
       plot = censoring,
       device = "png",
       dpi = 600,
       width = 6,
       height = 4)
