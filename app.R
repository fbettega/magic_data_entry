library(shiny)
library(tidyverse)
library(datamods)
library(data.table)
library(DT)
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





Colors_choice <-   data.frame(color = unlist(lapply((do.call(
  "c",
  lapply(
    seq_along(c("U", "B", "G", "W", "R")),
    function(i) {
      combn(c("U", "B", "G", "W", "R"),
        i,
        FUN = list
      )
    }
  )
)), function(x) paste0(x, collapse = ""))),
name = c("Mono U",
         "Mono B",
         "Mono G",
         "Mono W",
         "Mono R",
         "Dimir",
         "Simic",
         "Azorius",
         "Izzet",
         "Golgari",
         "Orzhov",
         "Rakdos",
         "Selesnya",
         "Gruul",
         "Boros",
         "Sultai",
         "Esper",
         "Grixis",
         "Bant",
         "Temur",
         "Jeskai",
         "Abzan",
         "Jund",
         "Mardu",
         "Naya",
         "Witch",
         "Glint",
         "Yore",
         "Ink",
         "Dune",
         "5 Colors"
         )
) %>% 
  rbind(
    c(
      "Any",
      "Any"
      ),
    c(
      "Uncolor",
      "Uncolor"
      )
    )


Companion_list <- c(
  "No companion",
  "Umori, the Collector",
  "Keruga, the Macrosage",
  "Zirda, the Dawnwaker",
  "Lutri, the Spellchaser",
  "Gyruda, Doom of Depths",
  "Obosh, the Preypiercer",
  "Kaheera, the Orphanguard",
  "Jegantha, the Wellspring",
  "Yorion, Sky Nomad",
  "Lurrus of the Dream-Den"
                    )

wish_board_type <- c(
  "No wish board",
  "Karn, the Great Creator",
  "Wish"
)


options(DT.options = list(
  pageLength = 50,
  lengthMenu = c(5, 50, 100, 1000), 
  language = list(search = 'Filter:')
  )
  )




ui <- navbarPage(
  "Dashboard",
    tabPanel(
    "Side_table",
    sidebarPanel(
      # Add side plan
      wellPanel(
        shinyjs::useShinyjs(),
        selectInput(
          "Deck_en_cours_side", "Deck name played",
          c("", unique(read_rds(file.path(outputDir, "modern_deck.rds"))$match_up))
        ),
        uiOutput("Color_deck"),
        
        selectInput(
          "companion", "Companion associate with deck",
          c(unique(Companion_list))
        ),
        selectInput(
          "wish_board", "Wish board in deck",
          c(unique(wish_board_type))
        ),
        textInput(
          "Player_side",
          "Player who played",
          value = ""
        ),
        dateInput(
          inputId = "date_side_plan",
          label = "Date du plan de side",
          value =  Sys.Date()
        ),
        fileInput("deck_list_side_plan_file", NULL,
                  buttonLabel = "Upload...",
                  accept = c("text", ".txt")
        ),
        textInput(
          "source_side_link",
          "Source side link if exist",
          value = ""
        ),
        textInput(
          "Note_sources_side",
          "Note on sources if exist",
          value = ""
        ),
        selectInput(
          "Deck_matchup_side", "Deck opponent played",
          c("", unique(read_rds(file.path(outputDir, "modern_deck.rds"))$match_up))
        ),
        
        uiOutput("Color_opponent"),
        
        selectInput(
          "Play_or_draw_side", "Play or Draw",
          c("Both", "Play", "Draw")
        ),
        textInput(
          "Side_plan_number_of_IN",
          "Enter a vector (comma delimited) of IN in side plan"
        ),
        uiOutput("IN_card_of_deck"),
        # verbatimTextOutput("text_in"),
        textInput(
          "Side_plan_number_of_OUT",
          "Enter a vector (comma delimited) of OUT in side plan"
        ),
        uiOutput("OUT_card_of_deck"),
        # verbatimTextOutput("text_out"),
        textInput(
          "Note_on_side_plan",
          "Note on current side plan",
          value = ""
        ),
        sliderInput("Fiabilite",
                    "Choose a fiability ",
                    min = 0L,
                    max = 10L,
                    step = 1L,
                    value = 5L
        ) # ,
      )
    ),
    mainPanel(
      wellPanel(
        shinyjs::useShinyjs(),
        actionButton("save_new_side_plan", "Save side plan"),
        actionButton("reset_side_plan", "Reset side plane"),
        hr(),
        verbatimTextOutput("text_in"),
        verbatimTextOutput("text_out"),
        #ADD control sum
        verbatimTextOutput("text_control_sum")
      ),
      wellPanel(
        DTOutput('edited_r_side_plan')
      )
    )
  ),
  # add matchup deck
  tabPanel(
    "Matchup",
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          textInput(
            "one_matchup_add",
            "Add matchup",
            value = ""
          ),
          selectInput(
            "Add_color", "Add color",
            c("", Colors_choice$color)
          ),
          selectInput(
            "Add_color_name", "Add name",
            c("", Colors_choice$name)
          ),
          actionButton("add_one_matchup", "Add one matchup")
        )
      ),
      mainPanel(
        wellPanel(
          DTOutput('result_matchup_table')
        )
      )
    )
  ),
  # add card to a matchup
  tabPanel(
    "Common card",
    sidebarPanel(
      wellPanel(
        shinyjs::useShinyjs(),
        uiOutput("deck_common_cards"),
        fileInput("deck_list_common_upload", NULL,
          buttonLabel = "Upload...",
          accept = c("text", ".txt")
        )
      ),
      # Add single cards
      wellPanel(
        selectInput(
          "one_card_add_common_cards_side", "Side",
          c(TRUE, FALSE)
        ),
        textInput(
          "one_card_add_common_cards_Card_name",
          "Card name",
          value = ""
        ),
        selectInput(
          "one_card_add_common_cards_deck", "Deck name",
          c("", unique(read_rds(file.path(outputDir, "modern_deck.rds"))$match_up))
        ),
        actionButton("add_one_card_common_cards", "Add cards one cards")
      )
    ), mainPanel(
      wellPanel(
        h3("Save table"),
        actionButton("save_new_common_cards", "Save new cards")
      ),
      wellPanel(
        DTOutput("preview")
      )
    )
  ),
  tabPanel(
    "Manual edit table",
    sidebarPanel(
      # Manual edit table
      wellPanel(
        shinyjs::useShinyjs(),
        h3("Save table"),
        actionButton("save_manual_edit_table", "Save edit")
      )
    ), mainPanel(
      wellPanel(
        edit_data_ui(id = "side_plan_editable_df")
      )
    )
  ),
  id = "Tab_active_nav_bar"
)




server <- function(input, output, session) {
  


reload_data_switch_tab <- eventReactive(input$Tab_active_nav_bar, {
    data_matchup <- reactive(read_rds(
      file.path(
        outputDir,
        "modern_deck.rds"
      )
    ))
    
    df_Side_table <- reactive({
      read_rds(file.path(outputDir, "df_Side_table.rds"))
    })
    
    return(list(
      data_matchup = data_matchup,
      df_Side_table = df_Side_table
    ))
    
  })
  
  
  ######### matchup panel #####################################

output$result_matchup_table <-  renderDT(
  datatable(reload_data_switch_tab()$data_matchup())
)

  observeEvent(input$add_one_matchup, {
    validate(
      need(input$one_matchup_add != "", "No deck"),
      need(input$Add_color != "" | input$Add_color_name != "", "No color"),
    )
    


    # Gere ajouter le nom ou la couleur 
    if(input$Add_color_name != "" ){

      
      Add_color_temp <- Colors_choice$color[Colors_choice$name == input$Add_color_name]
      
      add_matchup_df <- data.frame(
        match_up = input$one_matchup_add,
        color = Add_color_temp,
        color_name = input$Add_color_name
      )
      
    } else if(input$Add_color != ""){
      Add_color_name_temp <- Colors_choice$name[Colors_choice$color == input$Add_color]
      
      add_matchup_df <- data.frame(
        match_up = input$one_matchup_add,
        color = input$Add_color,
        color_name = Add_color_name_temp
      )
      
    } else {
      add_matchup_df <- data.frame(
        match_up = input$one_matchup_add,
        color = input$Add_color,
        color_name = input$Add_color_name
      )
    }
    # Rajoute Any si non deja présent 
    if (add_matchup_df$color != "Any" & (all(reload_data_switch_tab()$data_matchup()$match_up %notin% input$one_matchup_add) |
                                         "Any" %notin% reload_data_switch_tab()$data_matchup()$color[reload_data_switch_tab()$data_matchup()$match_up == input$one_matchup_add]) 
        ){
      add_matchup_df <- 
        rbind(add_matchup_df,
              data.frame(
                match_up = input$one_matchup_add,
                color = "Any",
                color_name = "Any"
      )
      )
      
      
    }

    new_matchup_df <- rbind(add_matchup_df,
                            reload_data_switch_tab()$data_matchup()) %>%
      mutate(match_up = trimws(match_up)) %>% 
      distinct(match_up,color,.keep_all = TRUE)
    
    

    output$result_matchup_table <-  renderDT(
      data.table(new_matchup_df)
    )
    
    saveRDS(new_matchup_df %>% 
              arrange(match_up),
            file = file.path(outputDir, sprintf("%s.rds", "modern_deck")))
  })

  ######### Common cards panel #####################################

  
  
  
  # Add list of cards
  my_list_deck <- reactive({
    data <- read_rds(file.path(outputDir, "modern_deck.rds"))$match_up
    my_list <- as.character(data)
  })

  output$deck_common_cards <- renderUI({
    selectInput(
      inputId = "deck_common_cards",
      label = "Deck",
      choices = c("", my_list_deck()),
      selected = FALSE
    )
  })

  new_card_import_df <- reactive({
    req(input$deck_list_common_upload)

    if (input$deck_common_cards == "") {
      validate(paste0("List is not selected"))
    }

    file_deck_list_common_upload <- input$deck_list_common_upload
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


  output$preview <- renderDT(
    data.table(new_card_import_df())
  )
  
  observeEvent(input$save_new_common_cards, {
    Decks_common_cards <- read_rds("data/Decks_common_cards.rds")
    
    
  

    Decks_common_cards <- rbind(Decks_common_cards, new_card_import_df()) %>% 
      mutate(Card_name = tolower(Card_name)) %>% 
      distinct(pick(everything()), .keep_all = TRUE)




    saveRDS(Decks_common_cards, file = file.path(outputDir, sprintf("%s.rds", "Decks_common_cards")))

    # Reset after saving
    shinyjs::runjs("history.go(0)")
  })
  # Add single cards
  observeEvent(
    input$add_one_card_common_cards,
    {
      Decks_common_cards <- read_rds("data/Decks_common_cards.rds")
      new_card_import_df <- data.frame(
        Side = isolate(input$one_card_add_common_cards_side),
        Card_name = isolate(input$one_card_add_common_cards_Card_name),
        deck = isolate(input$one_card_add_common_cards_deck)
      )

      Decks_common_cards <- rbind(Decks_common_cards, new_card_import_df) %>% 
        mutate(Card_name = tolower(Card_name)) %>%
        distinct(pick(everything()), .keep_all = TRUE)

      saveRDS(Decks_common_cards, file = file.path(outputDir, sprintf("%s.rds", "Decks_common_cards")))
      # reset value
      updateTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "one_card_add_common_cards_Card_name",
        label = "Card name",
        value = ""
      )
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "one_card_add_common_cards_deck", label = "Deck name",
        choices = c("", unique(read_rds(file.path(outputDir, "modern_deck.rds"))$match_up))
      )
    }
  )
  ########## Side_in_out #####################################
  # Adaptable UI for color in both deck
 
  
  output$Color_deck <- renderUI({
    data_matchup <- read_rds(
      file.path(
        outputDir,
        "modern_deck.rds"
      )
    )

    possible_color_user_deck <- data_matchup %>% 
      filter(
        match_up  == input$Deck_en_cours_side #"Omnath Control"
      ) %>%
      pull(color)
    selectInput(
      inputId = "possible_color_deck_user",
      label = "Color deck",
      choices = unique(c("Any",possible_color_user_deck
                  )),
      multiple = FALSE
    )
    
    
  })
  
  
  output$Color_opponent <- renderUI({
    data_matchup <- read_rds(
      file.path(
        outputDir,
        "modern_deck.rds"
      )
    )
    possible_color_user_deck <- data_matchup %>% 
      filter(
        match_up  == input$Deck_matchup_side #"Omnath Control"
      ) %>%
      pull(color)
    selectInput(
      inputId = "possible_color_deck_opponent",
      label = "Color deck",
      choices = unique(c("Any",possible_color_user_deck
      )),
      multiple = FALSE
    )
    
    
  })
  
  
  
  output$IN_card_of_deck <- renderUI({
    Decks_common_cards <- read_rds("data/Decks_common_cards.rds")
    selected_chard_possibly_IN <- Decks_common_cards %>%
      filter(
        deck == input$Deck_en_cours_side, # "Omnath Control",
        as.logical(Side)
      ) %>%
      pull(Card_name)
    
    selectInput(
      inputId = "Side_in_card",
      label = "Card IN",
      choices = c("", unique(selected_chard_possibly_IN)),
      multiple = TRUE
    )
  })
  
  
  
  # Adaptable UI for side in and out.
  # Side IN
  output$IN_card_of_deck <- renderUI({
    Decks_common_cards <- read_rds("data/Decks_common_cards.rds")
    selected_chard_possibly_IN <- Decks_common_cards %>%
      filter(
        deck == input$Deck_en_cours_side, # "Omnath Control",
        as.logical(Side)
      ) %>%
      pull(Card_name)

    selectInput(
      inputId = "Side_in_card",
      label = "Card IN",
      choices = c("", unique(selected_chard_possibly_IN)),
      multiple = TRUE
    )
  })



  # Side out
  output$OUT_card_of_deck <- renderUI({
    Decks_common_cards <- read_rds("data/Decks_common_cards.rds")


    selected_chard_possibly_OUT <- Decks_common_cards %>%
      filter(
        deck == input$Deck_en_cours_side, # "Omnath Control",
        !as.logical(Side)
      ) %>%
      pull(Card_name)

    selectInput(
      inputId = "Side_out_card",
      label = "Card OUT",
      choices = c("", unique(selected_chard_possibly_OUT)),
      multiple = TRUE
    )
  })

  
  
  
  
  output$text_in  <- renderText ({
      paste0("IN :",
      paste(as.numeric(strsplit(input$Side_plan_number_of_IN,split = ",")[[1]]),
                input$Side_in_card,
                sep = " ",
                collapse = " ; "
    )
    )
    
  })

  
  output$text_out <- renderText ({
      paste0("OUT : ",
             paste(as.numeric(strsplit(input$Side_plan_number_of_OUT,split = ",")[[1]]),
                input$Side_out_card,
                sep = " ",
                collapse = " ; "
    )
    )
  })
  # ADD side plan
  output$text_control_sum <- renderText({
     sum(as.numeric(strsplit(input$Side_plan_number_of_IN,split = ",")[[1]])) - 
       sum(as.numeric(strsplit(input$Side_plan_number_of_OUT,split = ",")[[1]]))
  })
  
  
  output$edited_r_side_plan <-  renderDT(
    data.table(
      reload_data_switch_tab()$df_Side_table() %>% 
        mutate(
          Deck = as.factor(Deck),
          color_deck = as.factor(color_deck),
          Companion = as.factor(Companion),
          Wish_board = as.factor(Wish_board),
          Player = as.factor(Player),
          Matchup = as.factor(Matchup),
          color_opponent = as.factor(color_opponent),
          Play_Draw = as.factor(Play_Draw)
        )
      ),
    filter = "top" ,
    options = list(columnDefs = list(list(
      targets = c(7,8),
      
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 6 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
        "}")
    ))), callback = JS('table.page(3).draw(false);'))
  

  observeEvent(input$save_new_side_plan, {

    validate(
      need(
        length(as.numeric(strsplit(input$Side_plan_number_of_IN,split = ",")[[1]])) == length(input$Side_in_card),
        "Number don't match IN cards number"
      ),
      need(length(as.numeric(strsplit(input$Side_plan_number_of_OUT,split = ",")[[1]])) == length(input$Side_out_card),
           "Number don't match OUT cards number"
           ),
      need(input$Deck_en_cours_side, "No deck"),
      need(input$Player_side, "No player"),
      need(input$date_side_plan, "No date"),
      need(input$Deck_matchup_side, "No opponent deck"),
      need(input$Play_or_draw_side, "No play draw"),
      need(input$Fiabilite, "No fiability")
    )
    
    
################################################################################
################################## Save deck_list ##############################

    if(!is.null(input$deck_list_side_plan_file)){
      
      if(!file.exists(file.path(outputDir,
                                "deck_list",
                                fs::path_sanitize(
                                  input$Deck_en_cours_side
                                ),
                                fs::path_sanitize(
                                  paste(
                                    input$possible_color_deck_user,
                                    input$companion,
                                    input$wish_board,
                                    input$Player_side,
                                    input$date_side_plan,
                                    substr(input$Note_sources_side,1,10),
                                    ".csv",
                                    sep = "_"
                                  )
                                )
                                )
                      )
         ){
      
      dir.create(
        file.path(outputDir,
                  "deck_list",
                  fs::path_sanitize(
                    input$Deck_en_cours_side
                    ) 
                  ), 
        showWarnings = FALSE
        )
      
      
      import_side_board_deck_list <- deck_parser(
          # "data/FB_joute_23_08.txt"
        input$deck_list_side_plan_file$datapath
        ) 
      
      import_side_board_deck_list$Side[which(import_side_board_deck_list$Side)[1]:nrow(import_side_board_deck_list)] <- TRUE
      
      import_side_board_deck_list <- import_side_board_deck_list %>% 
        drop_na()
      
      
      write.csv(import_side_board_deck_list,
                file.path(outputDir,
                          "deck_list",
                          fs::path_sanitize(
                            input$Deck_en_cours_side
                          ),
                          fs::path_sanitize(
                            paste(
                              input$possible_color_deck_user,
                              input$companion,
                              input$wish_board,
                              input$Player_side,
                              input$date_side_plan,
                              substr(input$Note_sources_side,1,10),
                              ".csv",
                              sep = "_"
                            )
                          )
      ) , 
                row.names = FALSE)

      }
      
    }
      
    In_en_cours <- paste(as.numeric(strsplit(input$Side_plan_number_of_IN,split = ",")[[1]]),
                         input$Side_in_card,
                         sep = " ",
                         collapse = " ; "
    )
    OUT_en_cours <- paste(as.numeric(strsplit(input$Side_plan_number_of_OUT,split = ",")[[1]]),
                          input$Side_out_card,
                          sep = " ",
                          collapse = " ; "
    )
    
    

    Side_plan_add <- data.frame(
      Deck = input$Deck_en_cours_side,
      color_deck = input$possible_color_deck_user,
      Companion = input$companion,
      Wish_board = input$wish_board,
      Player = trimws(input$Player_side),
      Date = input$date_side_plan,
      link_deck_list = file.path(outputDir,
                                 "deck_list",
                                 fs::path_sanitize(
                                   input$Deck_en_cours_side
                                 ),
                                 fs::path_sanitize(
                                   paste(
                                     input$possible_color_deck_user,
                                     input$companion,
                                     input$wish_board,
                                     input$Player_side,
                                     input$date_side_plan,
                                     substr(input$Note_sources_side,1,10),
                                     ".csv",
                                     sep = "_"
                                   )
                                 )
      ),
      link_source = trimws(input$source_side_link),
      Note_on_list = trimws(input$Note_sources_side),
      Matchup = input$Deck_matchup_side,
      color_opponent = input$possible_color_deck_opponent,
      Play_Draw = input$Play_or_draw_side,
      IN = In_en_cours,
      OUT = OUT_en_cours,
      Note_side_plan = trimws(input$Note_on_side_plan),
      Fiability = input$Fiabilite
    )
    
    df_Side_table <- read_rds(file.path(outputDir, "df_Side_table.rds"))
    
    
    df_Side_table_new <- rbind(
      Side_plan_add, 
      df_Side_table
      ) 
      
      
    


    
    output$edited_r_side_plan <-  renderDT(
      data.table(
        df_Side_table_new %>% 
          mutate(
            Deck = as.factor(Deck),
            color_deck = as.factor(color_deck),
            Companion = as.factor(Companion),
            Wish_board = as.factor(Wish_board),
            Player = as.factor(Player),
            Matchup = as.factor(Matchup),
            color_opponent = as.factor(color_opponent),
            Play_Draw = as.factor(Play_Draw)
          )
                 ),
      filter = "top",
                 options = list(columnDefs = list(list(
                   targets = c(7,8),
                   render = JS(
                     "function(data, type, row, meta) {",
                     "return type === 'display' && data.length > 6 ?",
                     "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                     "}")
                 ))), callback = JS('table.page(3).draw(false);'))
    
    
    
    saveRDS(df_Side_table_new,
      file = file.path(outputDir, "df_Side_table.rds")
    )
    
    
    
    
    # Gestion play draw sur les reset input 
    if(input$Deck_matchup_side != df_Side_table_new$Matchup[2] & 
       input$Play_or_draw_side == "Play"){
      
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "Play_or_draw_side", "Deck opponent played",
        c("Draw","Both", "Play")
      )
      
      
      
      
      
    } else if(input$Deck_matchup_side != df_Side_table_new$Matchup[2] & 
              input$Play_or_draw_side == "Draw"){
      
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "Play_or_draw_side", "Deck opponent played",
        c("Play","Draw","Both")
      )
      
      
      
      
    } else {
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "Play_or_draw_side", "Deck opponent played",
        c("Both", "Play", "Draw")
      )
      
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "Deck_matchup_side", "Deck opponent played",
        c("", unique(read_rds(file.path(outputDir, "modern_deck.rds"))$match_up))
      )
      
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "Side_in_card",
        selected = ""
      )
      
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "possible_color_deck_opponent",
        selected = "Any"
      )
      
      
      updateTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "Side_plan_number_of_IN",
        label = "Enter a vector (comma delimited) of IN in side plan",
        value = ""
      )
      
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "Side_out_card",
        selected = ""
        
      )
      
      
      
      updateTextInput(
        session = getDefaultReactiveDomain(),
        inputId = "Side_plan_number_of_OUT",
        label = "Enter a vector (comma delimited) of OUT in side plan",
        value = ""
      )
      
      updateTextInput(
        session = getDefaultReactiveDomain(),
        "Note_on_side_plan",
        "Note on current side plan",
        value = ""
      )
      }
    
    

    

  })
  
  
  observeEvent(input$reset_side_plan, {
    # Reset after saving
    shinyjs::runjs("history.go(0)")
    
    
  })
  
  
  
  
  editedable_r_side_plan <- edit_data_server(
    id = "side_plan_editable_df",
    data_r = reload_data_switch_tab()$df_Side_table,
    add = FALSE,
    update = TRUE,
    delete = TRUE,
    download_csv = FALSE,
    download_excel = FALSE,
    var_mandatory = c(
      "Deck",
      "Player",
      "Date",
      "Matchup",
      "Play_Draw",
      "IN",
      "OUT",
      "Fiabilite"
    )
  )
  
  observeEvent(input$save_manual_edit_table, {
    # Reset after saving
    saveRDS(editedable_r_side_plan(),
            file = file.path(outputDir, "df_Side_table.rds")
    )
    
    
  })

  
  
  


  
  
  
  
}






shinyApp(ui, server)




# # 
# df_Side_table <-  read_rds(file.path(outputDir, "df_Side_table.rds"))
# 
# # 
# saveRDS(df_Side_table,file.path(outputDir, "df_Side_table.rds"))


# data_matchup <- read_rds(
#   file.path(
#     outputDir,
#     "modern_deck.rds"
#     )
#   )


# Edit side plan
# df_Side_table <-  read_rds(file.path(outputDir, "df_Side_table.rds"))
# xlsx::write.xlsx(df_Side_table,row.names = FALSE,"temp.xlsx")
# df_Side_table <- xlsx::read.xlsx("temp.xlsx",sheetIndex = 1)

# df_Side_table <- df_Side_table  %>%  mutate(
#   Player = trimws(Player)
#   ,Fiability = ifelse(Player == "PieGonti",9,Fiability),
#    Matchup = str_replace(Matchup,"Jund","Saga party"))
#   
# 
# 
# saveRDS(df_Side_table,file.path(outputDir, "df_Side_table.rds"))


# Edit matchup
# data_matchup <- read_rds(
#   file.path(
#     outputDir,
#     "modern_deck.rds"
#   )
# )
# saveRDS(data_matchup %>%
#           arrange(match_up),
#         file = file.path(outputDir, sprintf("%s.rds", "modern_deck")))



# data_matchup <- data_matchup #%>% mutate(match_up = str_replace(match_up,"Jund","Saga party"))










