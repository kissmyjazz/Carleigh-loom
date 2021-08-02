library(tidyverse)
library(here)
library(rjags)
library(JointAI)
library(jcolors)
library(papaja)
library(glmmTMB)
library(performance)

theme_set(theme_apa(base_size = 14) + theme(legend.position = "bottom"))

path <- here("raw_data", "loom_data.csv")
df <- read_csv(path, col_types = "fffiniiinnnnn") %>%
  dplyr::mutate(center_cumulative_duration_sec_pos =
                  (center_cumulative_duration_sec + 0.01))


g_distribution <- JointAI::plot_all(as.data.frame(df[, 4:12]),
                        allNA = FALSE, breaks = 20,
                        fill = '#e30f41',
                        border ='#e30f41', ncol = 3,
                        cex = 0.8, cex.main = 0.4,
                        ylab = NULL)
g_distribution

df_tall <- df %>% tidyr::pivot_longer(!id:treatment, names_to = "variable",
                                      values_to = "value",
                                      names_transform = list(variable = factor))

g_violin <- ggplot(df_tall, aes(x = treatment, y = value, color = treatment,
                                fill = treatment)) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  geom_jitter(shape = 16, position = position_jitter(0.1), color = "black") +
  stat_summary(fun = median, geom = "point", size = 2.5, color = "red") +
  facet_wrap(vars(variable), scales = "free_y") +
  scale_color_jcolors(palette = "pal3") +
  scale_fill_jcolors(palette = "pal3")

g_violin

g_center_cumdur <- ggplot(df, aes(x = center_cumulative_duration_sec,
                                  color = treatment,
                                  fill = treatment)) +
  geom_density(alpha = 0.4) + scale_color_jcolors(palette = "pal3") +
  scale_fill_jcolors(palette = "pal3")

g_center_cumdur_hist <- ggplot(df, aes(x = center_cumulative_duration_sec,
                                  color = treatment,
                                  fill = treatment)) +
  geom_histogram(alpha = 0.4, binwidth = 1, position = "dodge") +
  scale_color_jcolors(palette = "pal3") +
  scale_fill_jcolors(palette = "pal3")

g_center_cumdur_hist

center_cumdur_vehicle <- df %>% dplyr::filter(treatment == "Vehicle") %>%
  dplyr::select(center_cumulative_duration_sec) %>% pull()

center_cumdur_FG7142 <- df %>% dplyr::filter(treatment == "FG7142") %>%
  dplyr::select(center_cumulative_duration_sec) %>% pull()

ks.test(center_cumdur_vehicle, center_cumdur_FG7142, alternative = "less")

m_tweedie <- glmmTMB(center_cumulative_duration_sec ~ treatment + (1|id),
                     data = df, family = tweedie())
summary(m_tweedie)

m_gaussian <- glmmTMB(center_cumulative_duration_sec ~ treatment + (1|id),
                   data = df, family = gaussian)
summary(m_gaussian)

m_ziGamma <- glmmTMB(center_cumulative_duration_sec_pos ~ treatment + (1|id),
                      data = df, family = ziGamma(link = "log"))
summary(m_ziGamma)

m_ziGamma_noRefx <- glmmTMB(center_cumulative_duration_sec_pos ~ treatment,
                     data = df, family = ziGamma(link = "log"))
summary(m_ziGamma_noRefx)

compare_performance(m_tweedie, m_gaussian, m_Gamma, m_ziGamma_noRefx)

check_model(m_Gamma)
