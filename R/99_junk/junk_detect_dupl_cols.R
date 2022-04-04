df = data.table::fread("99_junk/mtcars_dups.csv") %>%
  head %>%
  setDF()
set.seed(2022)
df = df[,sample(names(df)[1:ncol(df)], ncol(df))]

df %>%
  detect_dupl_cols()

#### Stray Codes ####
# df = mtcars %>%
#   dplyr::mutate(mpg_2 = mpg) %>%
#   head

# mtcars %>% data.table::fwrite("99_junk/mtcars_dups.csv")


# df = df %>% data.table
# col_combinations = expand.grid(x = names(df), y = names(df)) %>%
#   dplyr::filter(x!=y) %>%
#   arrange(x)

# col_combinations$x
# col_combinations$y


# Next steps
# Return positions of columns, data itself instead of column names
# Progress bar

