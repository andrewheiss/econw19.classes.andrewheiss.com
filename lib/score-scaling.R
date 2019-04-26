library(tidyverse)
library(patchwork)
library(fuzzyjoin)
library(here)

grades <- tribble(
  ~grade, ~lower, ~upper, ~gpa,
  "A",    93,     150,    4.0,
  "A-",   90,     93,     3.7,
  "B+",   87,     90,     3.4,
  "B",    83,     87,     3.0,
  "B-",   80,     83,     2.7,
  "C+",   77,     80,     2.4,
  "C",    73,     77,     2.0,
  "C-",   70,     73,     1.7,
  "D+",   67,     70,     1.4,
  "D",    63,     67,     1.0,
  "D-",   60,     63,     0.7,
  "F",    0,      60,     0.0
) %>% 
  mutate_at(vars(lower, upper), list(~. * 0.01)) %>% 
  mutate(grade = fct_inorder(grade, ordered = TRUE))

midterm_1_raw <- read_csv(here("private", "midterm_1.csv"))

midterm_1_adjusted <- midterm_1_raw %>% 
  rename(score = `Exam 1`) %>% 
  mutate(pct = score / 100) %>% 
  mutate(adjusted = score + 10,
         pct_new = adjusted / 100) %>% 
  fuzzy_left_join(grades, by = c("pct_new" = "lower", "pct_new" = "upper"),
                  match_fun = list(`>=`, `<`))

plot_midterm_1 <- midterm_1_adjusted %>% 
  count(grade)

midterm_1_grades <- ggplot(midterm_1_adjusted, aes(x = adjusted)) +
  geom_histogram(binwidth = 2) +
  theme_light()

midterm_1_scores <- ggplot(plot_midterm_1, aes(x = fct_rev(grade), y = n)) + 
  geom_bar(stat = "identity") +
  theme_light()

midterm_1_grades + midterm_1_scores + plot_layout(ncol = 1)

#

# midterm_2_raw <- read_csv(here("private", "midterm_2.csv"))
midterm_2_raw <- read_csv(here("private", "midterm_2_new.csv"))

midterm_2_adjusted <- midterm_2_raw %>% 
  mutate(pct = score / 100) %>% 
  mutate(adjusted = score + 7,
         pct_new = adjusted / 100) %>% 
  fuzzy_left_join(grades, by = c("pct_new" = "lower", "pct_new" = "upper"),
                  match_fun = list(`>=`, `<`))

plot_midterm_2 <- midterm_2_adjusted %>% 
  count(grade)

midterm_2_grades <- ggplot(midterm_2_adjusted, aes(x = adjusted)) +
  geom_histogram(binwidth = 2) +
  theme_light()

midterm_2_scores <- ggplot(plot_midterm_2, aes(x = fct_rev(grade), y = n)) + 
  geom_bar(stat = "identity") +
  theme_light()

midterm_2_grades + midterm_2_scores + plot_layout(ncol = 1)

#

midterm_2_raw <- read_csv(file.path(here(), "private", "midterm_2.csv"))

midterm_2_adjusted <- midterm_2_raw %>% 
  rename(score = `Midterm 2`) %>% 
  mutate(pct = score / 150) %>% 
  mutate(adjusted = score + 1.5,
         pct_new = adjusted / 150) %>% 
  fuzzy_left_join(grades, by = c("pct_new" = "lower", "pct_new" = "upper"),
                  match_fun = list(`>=`, `<`))

plot_midterm_2 <- midterm_2_adjusted %>% 
  count(grade)

midterm_2_grades <- ggplot(midterm_2_adjusted, aes(x = adjusted)) +
  geom_histogram(binwidth = 2) +
  theme_light()

midterm_2_scores <- ggplot(plot_midterm_2, aes(x = fct_rev(grade), y = n)) + 
  geom_bar(stat = "identity") +
  theme_light()

midterm_2_grades + midterm_2_scores + plot_layout(ncol = 1)


final_grades_raw <- read_csv(file.path(here(), "private", "final_points.csv")) %>% 
  rename(points = `Total Points`) 

final_grades <- final_grades_raw %>% 
  mutate(pct = points / 1148,
         pct_rnd = round(pct, 2)) %>% 
  fuzzy_left_join(grades, by = c("pct" = "lower", "pct" = "upper"),
                  match_fun = list(`>=`, `<`))

mean(final_grades$gpa)
