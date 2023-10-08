library(shiny)
library(tidyverse)
library(datamods)
library(data.table)
library(DT)
library(digest)
library(rlang)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(DT::renderDataTable)
conflicted::conflicts_prefer(shiny::renderDataTable)
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

Decks_common_cards <- read_rds("data/Decks_common_cards.rds")
data_matchup <-
  read_rds(
  file.path(
    outputDir,
    "modern_deck.rds"
  )
)


# saveRDS(data_matchup %>%
#           arrange(match_up),
#         file = file.path(outputDir, sprintf("%s.rds", "modern_deck")))


# saveRDS(Decks_common_cards,
#         file = file.path(outputDir, sprintf("%s.rds", "Decks_common_cards")))



df_Side_table <-
  read_rds(file.path(outputDir, "df_Side_table.rds"))




 # saveRDS(df_Side_table,file.path(outputDir, "df_Side_table.rds"))


correction <- c(
  "brazen borrower" = "brazen borrower // petty theft",
  # "dead\\b" = "dead // gone",
  "^dead/" = "dead // gone",
  "^fable of the mirror-breaker" = "fable of the mirror-breaker // reflection of kiki-jiki",
  "^fire/" = "fire // ice",
  "^wear/"  = "wear // tear"
     )





correction_expression <- c()

for (i in seq_along(correction)){

  
  
  correction_expression <-
    paste0(
      correction_expression,
      "ifelse(
  str_detect(Card_name,",
      '"',
      names(correction)[i],'")',
      ",",'"',
      correction[i],
      '",'
  )
  
  if (i == length(correction)){
    correction_expression <- 
      paste0(
        correction_expression,"Card_name",
        paste0(
          rep(")",length(correction)),collapse = ""
          )
        )
  }
}







Decks_common_cards <- 
  Decks_common_cards %>% 
  # distinct(Card_name,.keep_all = TRUE) %>% 
  mutate(Card_name = !!parse_quo(correction_expression,env = caller_env())
         )
# %>% 
#   filter(
#     str_detect(
#       Card_name,
#       paste0(names(correction),collapse = "|")
#       ) 
#     )











