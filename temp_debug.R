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



# df_Side_table <- df_Side_table %>%
#   mutate(link_deck_list = str_replace(link_deck_list,"/Titan/","/Amulet Titan/")) 



  saveRDS(df_Side_table ,file.path(outputDir, "df_Side_table.rds"))


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

list.files("data/deck_list/",recursive = TRUE)























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








# path_of_deck_list <- df_Side_table %>%
#   distinct(link_deck_list) %>%
#   pull(link_deck_list)
# 
# 
# 
# list_of_deck_list <- lapply(path_of_deck_list, function(x){
# 
#   read.csv(x) %>% mutate(link_deck_list = x)
# }) %>%
#   bind_rows() %>%
#   mutate(
#     Side = ifelse(
#       Side,
#       "IN",
#       "OUT"
#     )
#   )
# 
# df_Side_table <- df_Side_table %>% mutate(
#   IN = ifelse(IN =="" ,"0 No side",IN),
#   OUT = ifelse(OUT == ""|OUT == "0","0 No side",OUT)
#                          )
# 
# 
# 
# 
# 
# 
# long_side_table<-   df_Side_table %>%
#   mutate(
#     IN = str_split(IN," ; "),
#     OUT = str_split(OUT," ; ")
#   ) %>%
#   rownames_to_column() %>%
#   pivot_longer(cols = c(IN,OUT)
# 
#   ) %>%
#   unnest_longer(col = c(value)
#   ) %>%
#   mutate(
#     quantite = as.numeric(str_extract(value,"^\\d{1}")),
#     value = trimws(str_remove(value,"^\\d{1}")),
#     value = ifelse(rowname == 236 & value == "pact of negation","summoner's pact",value),
#     value = ifelse(value == "brazen borrower","brazen borrower // petty theft",value)
#   )
# 
# 
# 
# 
# temp1 <- long_side_table %>%
#   # perform join
#   left_join(list_of_deck_list,
#                              # define join columns
#                              by=c("link_deck_list" = "link_deck_list",
#                                   "name" = "Side",
#                                   "value" = "Card_name")) %>% 
#   
#   mutate(
#     quantite.y = ifelse(value=="No side",0,quantite.y),
#     quantite =paste0(quantite.x,"/", quantite.y)) %>% 
#   filter(!is.na(quantite.y)) %>% 
#   select(-quantite.x, -quantite.y)
# 
# 
# 
# rebus1 <- long_side_table %>%
#   # perform join
#   left_join(list_of_deck_list,
#             # define join columns
#             by=c("link_deck_list" = "link_deck_list",
#                  "name" = "Side",
#                  "value" = "Card_name")) %>% 
#   mutate(
#     quantite.y = ifelse(value=="No side",0,quantite.y),
#     quantite =paste0(quantite.x,"/", quantite.y)) %>% 
#   filter(is.na(quantite.y))
# 
# 
# 
# 
# 
# 
# temp2 <- rbind(
#   temp1,
#   rebus1 %>% 
#   select(-quantite.y,-quantite ) %>% 
#   rename(quantite = quantite.x) %>% 
#   # perform join
#   fuzzyjoin::fuzzy_left_join(list_of_deck_list,
#                              # define join columns
#                              by=c("link_deck_list" = "link_deck_list",
#                                   "name" = "Side",
#                                   "value" = "Card_name"),
#                              # list of match functions (first should be clear)
#                              match_fun = list(`==`, `==`,
#                                               # function which returns boolean vector where maximum allowed string distance is 2 using levenshtein
#                                               function(x,y)
#                                                 stringdist::stringdist(x, y) <= 3))  %>%
#   mutate(quantite =paste0(quantite.x,"/", quantite.y)) %>% 
#   filter(!is.na(quantite.y)) %>%
#    select( -c(Side, Card_name,link_deck_list.y,quantite.x, quantite.y)) %>%
#     rename(link_deck_list = link_deck_list.x)
#   )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# long_side_table_merge_with_quantity <- temp2
# 
# 
# 
# wide_format_merge_table <- long_side_table_merge_with_quantity %>%
#   mutate(Card = paste0( quantite," ",value )) %>%
#    select(-quantite ,-value) %>%
#   pivot_wider(names_from = name,
#               values_from = Card,
#               values_fn = ~ paste0(.x, collapse = " ; ")) %>%
#   mutate(IN = ifelse(str_detect(IN,"NA/NA |0/NA ")  ,"",IN),
#          OUT = ifelse(str_detect(OUT,"NA/NA |0/NA ") ,"",OUT)) 




saveRDS(wide_format_merge_table,file.path(outputDir, "df_Side_table.rds"))





