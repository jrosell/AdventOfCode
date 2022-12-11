library(tidyverse)
file <- here::here("2022/10_input")

# Jordi drob based 
file %>% 
  read_csv(col_names = "x", show_col_types = FALSE) %>% 
  mutate(line = row_number(), .before = 1) %>% 
  mutate(x = if_else(x == "noop", "noop 0", x)) %>% 
  separate(x, c("instruction", "value"), convert = TRUE) %>% 
  mutate(step = ifelse(instruction == "addx", 2, 1)) %>% # Duplicate addx row
  uncount(step, .id = "step") %>% 
  group_by(line) %>% 
  mutate(is_last = instruction == "noop" | (instruction == "addx" & step == 2)) %>% # Last addx step
  ungroup() %>% 
  mutate(score = cumsum(c(1, head(is_last * value, -1)))) %>% # Calculate score for each cycle
  mutate(cycle = row_number()) %>% 
  filter(cycle %in% seq(20, 220, 40)) %>% 
  summarise(sum(cycle * score)) %>% 
  pull() %>% 
  print()
  

file %>% 
  read_csv(col_names = "x", show_col_types = FALSE) %>% 
  mutate(line = row_number(), .before = 1) %>% 
  mutate(x = if_else(x == "noop", "noop 0", x)) %>% 
  separate(x, c("instruction", "value"), convert = TRUE) %>% 
  pull(value)


result <- file %>%
  read_csv(col_names = "x", show_col_types = FALSE) %>% 
  pull() %>%
  str_split(" ") %>%
  unlist() %>% 
  as.integer() %>% 
  replace_na(0) %>% 
  c(1, .) %>% 
  cumsum()

result[seq(20, 220, 40)]


input <- file %>%
  read_csv(col_names = "x", show_col_types = FALSE) %>% 
  pull() %>%
  str_split(" ") %>%
  unlist() %>% 
  as.integer() %>% 
  replace_na(0) 
begin <- 1
result <- cumsum(c(begin, input))
sum(result[seq(20,220,40)]*seq(20,220,40))

#Drob
file %>% 
  read_csv(col_names = "x", show_col_types = FALSE) %>% 
  separate(x, c("op", "num"), fill = "right", convert = TRUE) %>% 
  mutate(
    op_group = row_number(),
    num = coalesce(num, 0), 
    cycles = ifelse(op == "addx", 2, 1)
  ) %>% 
  uncount(cycles) %>%
  group_by(op_group) %>% 
  mutate(is_last = row_number() == n()) %>% 
  ungroup() %>% 
  mutate(x = cumsum(c(1, head(is_last * num, -1)))) %>% 
  mutate(cycle = row_number()) %>% 
  filter(cycle %in% c(20, 60, 100, 140, 180, 220)) %>% 
  summarise(sum(cycle * x)) %>% 
  pull() %>% 
  print()

mutate(
  op_group = row_number(),
  num = coalesce(num, 0), 
  cycles = ifelse(op == "addx", 2, 1)
) %>% 
  uncount(cycles) %>%
  group_by(op_group) %>% 
  mutate(is_last = row_number() == n()) %>% 
  ungroup() %>% 
  mutate(x = cumsum(c(1, head(is_last * num, -1)))) %>% 
  mutate(cycle = row_number()) %>% 
  filter(cycle %in% c(20, 60, 100, 140, 180, 220)) %>% 
  summarise(sum(cycle * x)) %>% 
  pull()
}


rerun_elapsed_times <- function(times, fn, ...) {
    results <- rerun(times, system.time(fn(...))) %>% enframe() %>% unnest_wider(value) %>% pull(elapsed)
    std_err <- sd(results, na.rm = TRUE) / sqrt(sum(!is.na(results)))
    cat("Elapsed each: ", mean(results),"s (+-",std_err,"s).\nTotal elapsed: ", sum(results))
}

drob_day9 <- function(...) {
    file %>% 
        read_csv(col_names = "x", show_col_types = FALSE) %>% 
        separate(x, c("op", "num"), fill = "right", convert = TRUE) %>% 
        mutate(
          op_group = row_number(),
          num = coalesce(num, 0), 
          cycles = ifelse(op == "addx", 2, 1)
        ) %>% 
        uncount(cycles) %>%
        group_by(op_group) %>% 
        mutate(is_last = row_number() == n()) %>% 
        ungroup() %>% 
        mutate(x = cumsum(c(1, head(is_last * num, -1)))) %>% 
        mutate(cycle = row_number()) %>% 
        filter(cycle %in% c(20, 60, 100, 140, 180, 220)) %>% 
        summarise(sum(cycle * x)) %>% 
        pull()
}

antoine_fabri_day9 <- function(...) {
    input <- read.table(h=F, text = gsub("noop", "noop 0", readLines(file)))
    all_vals <- rep(1 + cumsum(input$V2), c((input$V1 == "addx")[-1], 0) + 1)
    i <- seq(20, 220, 40)
    sum(all_vals[i-1]*i)
}

jordi_day9 <- function(...) {
  input <- file %>% 
    read_csv(col_names = "x", show_col_types = FALSE) %>% 
    mutate(x = if_else(x == "noop", "noop 0", x)) %>% 
    separate(x, c("instruction", "v"), convert = TRUE)
  all_vals <- rep(1 + cumsum(input$v), c((input$instruction == "addx")[-1], 0) + 1)
  i <- seq(20, 220, 40)
  sum(all_vals[i-1]*i)
}

rerun_elapsed_times(10, drob_day9)
rerun_elapsed_times(10, antoine_fabri_day9)
rerun_elapsed_times(10, jordi_day9)




antoine_fabri_day9_input <- function(...) {
  input <- read.table(h=F, text = gsub("noop", "noop 0", readLines(file)))
}

drob_day9_input <- function(...) {
  file %>% 
    read_csv(col_names = "x", show_col_types = FALSE) %>% 
    separate(x, c("op", "num"), fill = "right", convert = TRUE) %>% 
    mutate(
      op_group = row_number(),
      num = coalesce(num, 0)
    )
}


jordi_day9_input <- function(...) {
  input <- file %>% 
    read_csv(col_names = "x", show_col_types = FALSE) %>% 
    mutate(x = if_else(x == "noop", "noop 0", x)) %>% 
    separate(x, c("instruction", "v"), convert = TRUE)
}

rerun_elapsed_times(20, antoine_fabri_day9_input)
rerun_elapsed_times(20, drob_day9_input)
rerun_elapsed_times(20, jordi_day9_input)


antoine_fabri_day9_values <- function(...) {
  input <- antoine_fabri_day9_input()
  all_vals <- rep(1 + cumsum(input$V2), c((input$V1 == "addx")[-1], 0) + 1)
  i <- seq(20, 220, 40)
  sum(all_vals[i-1]*i)
}

drob_day9_values <- function(...) {
  drob_day9_input() %>%  
    mutate(cycles = ifelse(op == "addx", 2, 1)) %>% 
    uncount(cycles) %>%
    group_by(op_group) %>% 
    mutate(is_last = row_number() == n()) %>% 
    ungroup() %>% 
    mutate(x = cumsum(c(1, head(is_last * num, -1)))) %>% 
    mutate(cycle = row_number()) %>% 
    filter(cycle %in% c(20, 60, 100, 140, 180, 220)) 
}

rerun_elapsed_times(20, antoine_fabri_day9_values)
rerun_elapsed_times(20, drob_day9_values)
