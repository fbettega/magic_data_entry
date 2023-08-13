library(shiny)
library(tidyverse)
library(datamods)
library(data.table)

outputDir <- "data"




data_matchup <- read_rds("data/modern_deck.rds")

# ui <-   navbarPage("Dashboard",
#                    tabPanel("Matchup",
#  
#                             sidebarLayout(
#                               sidebarPanel(
#                                 wellPanel(
#                                   h3("Save table"),
#                                   actionButton("save", "Save")
#                                           )
#                               ),
#                               mainPanel(
# 
#                                 
#                                 wellPanel(
#                                   h3("test"),
#                                   edit_data_ui(id = "matchup"
#                                                )
# 
#                                 )
#                                   )
#                               )
#                             
#                    ),
#                    tabPanel("Common card",
#                             edit_data_ui(id = "matchup"),
#                             # sidebarLayout(
#                             #   sidebarPanel(
#                             #     fileInput("list_common_cards",
#                             #               label="Upload FCM Data"
#                             #     ),
#                             #     uiOutput('choose_TEC'),
#                             #     uiOutput('choose_OP'),
#                             #     uiOutput('choose_APL'),
#                             #     uiOutput('choose_PRD'),
#                             #     uiOutput('choose_DEV'),
#                             #     uiOutput('choose_NACol')
#                             #   ),
#                             #   mainPanel(
#                             #     wellPanel(h3("Save table"),actionButton("save", "Save")),
#                             #     wellPanel(
#                             #       edit_data_ui(id = "matchup")
#                             #     )
#                             #   )
#                             # )
#                    )
# )

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
                             edit_data_ui(id = "matchup"),
                            # sidebarLayout(
                            #   sidebarPanel(
                            #     fileInput("list_common_cards",
                            #               label="Upload FCM Data"
                            #     ),
                            #     uiOutput('choose_TEC'),
                            #     uiOutput('choose_OP'),
                            #     uiOutput('choose_APL'),
                            #     uiOutput('choose_PRD'),
                            #     uiOutput('choose_DEV'),
                            #     uiOutput('choose_NACol')
                            #   ),
                            #   mainPanel(
                            #     wellPanel(h3("Save table"),actionButton("save", "Save")),
                            #     wellPanel(
                            #       edit_data_ui(id = "matchup")
                            #     )
                            #   )
                            # )
                   )
)




server = function(input, output, session) {

  edited_r <- edit_data_server(
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
    
    str(edited_r())
  })

  
  


  
  observeEvent(input$save, {
    print("aaaaaaaaaaaaaaaaaaaaaaaaaaa")
      # saveRDS(edited_r(), file=file.path(outputDir, sprintf("%s.rds", "modern_deck")))
  }
  )
  
  
  
}




  shinyApp(ui, server)
