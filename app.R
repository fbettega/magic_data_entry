library(shiny)
library(tidyverse)
library(datamods)


outputDir <- "data"


deck_parser <- function(deck_path){
  
  deck <- read.delim(deck_path,header = FALSE ,blank.lines.skip = FALSE) %>%
    filter( !str_detect(.$V1,regex('deck|Sideboard', ignore_case = TRUE))) %>%
    mutate(Side = str_detect(.$V1,regex('^$', ignore_case = TRUE))) %>% 
    mutate(quantite = as.numeric(str_extract_all(.$V1,"^[:digit:]*\\S*" )),
           Card_name = tolower(str_extract(.$V1,"(?<=[:digit:]\\s).*") ), 
    ) %>% 
    select(-V1) 
}


data_matchup <- read_rds(file.path(outputDir,"modern_deck.rds"))


# a <- read_rds("data/Decks_common_cards.rds")














ui <-   navbarPage("Dashboard",
                                      tabPanel("Matchup",
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   wellPanel(
                                                     h3("Save table"),
                                                     actionButton("save", "Save")
                                                             )
                                                 ),
                                                 mainPanel(


                                                   wellPanel(
                                                     edit_data_ui(id = "matchup"
                                                                  )

                                                   )
                                                     )
                                                 )

                                      ),
                                      tabPanel("Common card",
                                               sidebarPanel(
                                                 wellPanel(
                                                   shinyjs::useShinyjs(),
                                                   uiOutput("deck_common_cards"),
                                                   fileInput('deck_list_common_upload', NULL, buttonLabel = "Upload...",
                                                             accept=c('text','.txt'))
                                                 ),
                                                 # Add single cards
                                                 wellPanel(
                                                 selectInput(
                                                   "one_card_add_common_cards_side","Side",
                                                   c(TRUE,FALSE)
                                                 ),
                                                 textInput(
                                                   "one_card_add_common_cards_Card_name", 
                                                   "Card name",
                                                   value = ""
                                                 ),
                                                 selectInput(
                                                   "one_card_add_common_cards_deck", "Deck name",
                                                   c("",unique(read_rds(file.path(outputDir,"modern_deck.rds"))$match_up))
                                                 ),
                                                 actionButton("add_one_card_common_cards", "Add cards one cards")
                                                 )
                                                 
                                                 
                                               
                                      ),mainPanel(
                                        wellPanel(
                                          h3("Save table"),
                                          actionButton("save_new_common_cards", "Save new cards")
                                        ),
                                        wellPanel(
                                        tableOutput("preview")
                                        )
                                        )
                   ))


server = function(input, output, session) {
   
######### matchup panel #####################################

  edited_r_mathup <- edit_data_server(
    id = "matchup",
    data_r = reactive(data_matchup),
    add = TRUE,
    update = TRUE,
    delete = TRUE,
    download_csv = FALSE,
    download_excel = FALSE,
    var_mandatory = c("match_up")

  )
  

  
  output$result <- renderPrint({
    
    str(edited_r_mathup())
  })
  
  
  observeEvent(input$save, {

     saveRDS(edited_r_mathup(), file=file.path(outputDir, sprintf("%s.rds", "modern_deck")))
  }
  )
  
######### Common cards panel #####################################
  
  # Add list of cards
  my_list_deck <- reactive({
    data <- read_rds(file.path(outputDir,"modern_deck.rds"))$match_up
    my_list <- as.character(data)
    
  })
  
  output$deck_common_cards <- renderUI({
    
    selectInput(inputId = "deck_common_cards",
                label = "Deck",
                choices = c("",my_list_deck()),
                selected=FALSE)
  })
  
  new_card_import_df <-   reactive({
    req(input$deck_list_common_upload)

    if (input$deck_common_cards == "") {
      validate(paste0("List is not selected"))
    }
    
    file_deck_list_common_upload = input$deck_list_common_upload
    list_import <- deck_parser(
      file_deck_list_common_upload$datapath
      ) %>%
      select(-quantite)
    
    list_import$Side[which(list_import$Side)[1]:nrow(list_import)] <- TRUE

    
    new_card_import_df <- 
      list_import %>% 
      drop_na() %>%
      mutate(
        deck = input$deck_common_cards
      )


  })
  
  output$preview <- renderTable(new_card_import_df(),striped = TRUE, hover= TRUE, bordered= TRUE
  )
  
  observeEvent(input$save_new_common_cards, {
    
    Decks_common_cards <- read_rds("data/Decks_common_cards.rds")
    
    
    Decks_common_cards <- rbind(Decks_common_cards,new_card_import_df()) %>%
      distinct(pick(everything()), .keep_all = TRUE)
    
    
    
    
    saveRDS(Decks_common_cards, file=file.path(outputDir, sprintf("%s.rds", "Decks_common_cards")))
    
    # Reset after saving
    shinyjs::runjs("history.go(0)")

    
  }
  )
  # Add single cards
  observeEvent(
    input$add_one_card_common_cards,
    {
      Decks_common_cards <- read_rds("data/Decks_common_cards.rds")
      new_card_import_df <- data.frame(
        Side = isolate(input$one_card_add_common_cards_side),
        Card_name =isolate(input$one_card_add_common_cards_Card_name),
        deck = isolate(input$one_card_add_common_cards_deck)
        
      )
      
      Decks_common_cards <- rbind(Decks_common_cards,new_card_import_df) %>%
        distinct(pick(everything()), .keep_all = TRUE)

          saveRDS(Decks_common_cards, file=file.path(outputDir, sprintf("%s.rds", "Decks_common_cards")))
      # reset value
      updateTextInput(
        session = getDefaultReactiveDomain(),
        inputId= "one_card_add_common_cards_Card_name", 
        label  = "Card name",
        value = ""
      )
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "one_card_add_common_cards_deck", label = "Deck name",
        choices = c("",unique(read_rds(file.path(outputDir,"modern_deck.rds"))$match_up))
      )
  
    }
    )
}




  shinyApp(ui, server)
