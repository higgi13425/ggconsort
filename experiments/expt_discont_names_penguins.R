# palmerpenguins example
# helpful for reasons discontinued,
# reasons not_analyzed
# note function needs fixing for when there are no exclusion reasons (aka all on one island) - an if_else case.

library(tidyverse)
library(palmerpenguins)
library(janitor)
library(glue)

data <- palmerpenguins::penguins

species_vec <- data %>% pull(species) %>% unique()


make_table <- function(chosen_species){
  data %>%
    filter(species == chosen_species) %>%
    tabyl(island) %>%
    adorn_totals("row") %>%
    arrange(desc(n)) ->
    table1

  table1[1,1] <- "All Islands"


  table2 <- table1 %>%
    mutate(new_col = paste0(n, " ", island, "\n")) %>%
    select(new_col)

  # store max character width for later use in box sizing
  islands_char_width <- max(nchar(table2$new_col))


  island_label <- glue_collapse(table2$new_col)
  island_label
}

purrr::map(species_vec,
           ~make_table(.)) %>%
  unlist()

# alternate
purrr::map(species_vec, make_table) %>%
  unlist() %>%
  tibble() %>%
  set_names("island_label")

# alternate
purrr::map_chr(species_vec, make_table) %>%
  tibble() %>%
  set_names("island_label")

# alternate with column of species names
# (use with arm, discont_reason in place of species, island in ggconsort)
purrr::map(species_vec, make_table) %>%
  set_names(species_vec) %>%
  bind_rows() %>%
  pivot_longer(cols = species_vec, names_to = "species") %>%
  set_names('species', 'island_label')
