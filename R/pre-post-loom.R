library(tidyverse)
library(here)
library(readxl)

path <- here("raw_data", "looming.xlsx")

df <- readxl::read_excel(path, trim_ws = TRUE, na = '-') %>%
  dplyr::rename(animal_id = ...1, group = ...2) %>%
  dplyr::mutate_at(1, factor)


df_post <- df %>% dplyr::filter(str_detect(group, "Post"),
                                !animal_id %in% c("442"))
df_pre <- df %>% dplyr::filter(str_detect(group, "Pre"))

grp_post <- df_post %>% dplyr::arrange(group) %>% dplyr::pull(group)
grp_pre <- df_pre %>% dplyr::arrange(group) %>% dplyr::pull(group)

df_post_ttest <- df_post %>%
  dplyr::select_if(function(col) is.numeric(col) && !any(is.na(col))) %>%
  dplyr::select(!dplyr::matches("^Activity state")) %>%
  purrr::map_df(~ broom::tidy(t.test(. ~ grp_post, paired = FALSE)),
                .id = 'variable')

names(df_post_ttest)[3:4] <- c("mean_FG7142", "mean_Vehicle")


df_pre_ttest <- df_pre %>%
  dplyr::select_if(function(col) is.numeric(col) && !any(is.na(col))) %>%
  dplyr::select(!dplyr::matches("^Activity state")) %>%
  purrr::map_df(~ broom::tidy(t.test(. ~ grp_pre, paired = FALSE)),
                .id = 'variable')

names(df_pre_ttest)[3:4] <- c("mean_FG7142", "mean_Vehicle")

summ <- list(PreLoom = df_pre_ttest, PostLoom = df_post_ttest) %>%
  dplyr::bind_rows(.id = "Pre/Post") %>% dplyr::arrange(p.value)

path <- here("summary_data", "t-tests-loom.csv")
readr::write_csv(summ, path)
