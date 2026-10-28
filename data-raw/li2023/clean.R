library(here)
library(tidyverse)

# also has system dependencies that probably require manual installation
# local_pkg_path <- here("data-raw", "li2023", "RSienaTest_1.2-25.tar.gz")
# pak::pak(local_pkg_path)

# not needed for data munging, only for data analysis
# library(RSienaTest)

data_pkg <- here("data-raw", "li2023", "JPSP_rep_data.RData")
load(data_pkg)

JPSP_SAOM_pss_df_rep |>
  janitor::clean_names() |>
  rename(
    conscientiousness = p_con,
    neuroticism = p_neu,
    locus_of_control = p_loc,
    extraversion = p_ext,
    agreeableness = p_agr,
    openness = p_ope,
    outside_support_count_t1 = outsup_t1,
    outside_support_count_t2 = outsup_t2,
    outside_support_count_t3 = outsup_t3,
    outside_support_count_t4 = outsup_t4,
    workload_t1 = hrs_per_week_mini1_total,
    workload_t2 = hrs_per_week_mini2_total,
    workload_t3 = hrs_per_week_mini3_total
  ) |>
  mutate(
    sex = if_else(female == 1, "female", "male"),
    white = if_else(white == 1, "white", "non-white"),
    us = if_else(us == 1, "us-citizen", "non-us-citizen")
  ) |>
  select(-female) |>
  mutate(across(c(sex, white, us), as.factor)) |>
  write_csv(
    here("data", "li2023", "node_data.csv")
  )

matrix_to_edgelist <- function(A, time) {
  A |>
    as_tibble(rownames = "row") |>
    gather(col, value, -row) |>
    mutate_all(as.integer) |>
    rename(
      from = row,
      to = col
    ) |>
    mutate(time = time)
}

# includes NAs
edgelist <- bind_rows(
  matrix_to_edgelist(t1_expr_p315_na, time = 1),
  matrix_to_edgelist(t2_expr_p315_na, time = 2),
  matrix_to_edgelist(t3_expr_p315_na, time = 3),
  matrix_to_edgelist(t4_expr_p315_na, time = 4)
)

write_csv(
  edgelist,
  here("data", "li2023", "edgelist.csv")
)
