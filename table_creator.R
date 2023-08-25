library(tidyverse)

conflicted::conflict_prefer("filter", "dplyr")

outputDir <- "data"

`%notin%`<- negate(`%in%`)

deck_parser <- function(deck_path) {
  deck <- read.delim(deck_path, header = FALSE, blank.lines.skip = FALSE) %>%
    filter(!str_detect(.$V1, regex("deck|Sideboard", ignore_case = TRUE))) %>%
    mutate(Side = str_detect(.$V1, regex("^$", ignore_case = TRUE))) %>%
    mutate(
      quantite = as.numeric(str_extract_all(.$V1, "^[:digit:]*\\S*")),
      Card_name = tolower(str_extract(.$V1, "(?<=[:digit:]\\s).*"))
    ) %>%
    select(-V1)
}

deck_list <- deck_parser("data/FB_joute_23_08.txt")



deck_list$Side[which(deck_list$Side)[1]:nrow(deck_list)] <- TRUE

Main_deck_a_side <- deck_list %>% 
  filter(!Side)

Side_deck_a_side <- deck_list %>% 
  filter(Side) %>% drop_na()









