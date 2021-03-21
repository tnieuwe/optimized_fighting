#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(pheatmap)
library(shiny)
library(tidyverse)
library(plotly)
library(shinyHeatmaply)
library(heatmaply)
load("all_tier_matrices.rda")
all_games <- names(all_character_matrix)

game_char_list <- lapply(all_character_matrix, colnames)

shinyApp(
    ui = fluidPage( 
        sidebarLayout(
            sidebarPanel(
        selectInput("game", "Choose a game:", names(game_char_list)),
        selectInput("character", "Choose a character", choices = NULL), 
        
        ),
        mainPanel(tabsetPanel(type = "tab",
                              tabPanel("Matchup Chart",
                                       plotlyOutput("hmap")),
                              tabPanel("Secondary Selector", textOutput("result")),
                              tabPanel("Secondary Chart"),
                              tabPanel("Votes per Matchup")
            
        )
        )
        )
    ),
    server = function(input, output, session) {
        output$result <- renderText({
            paste("You chose", paste(input$game,input$character, sep = ":"))
        })
        
        output$hmap <- renderPlotly({
            dat <- as.matrix(all_character_matrix[[input$game]])   
            match_frame <- ((dat - mean(dat,na.rm = T))/sd(dat,na.rm = T))
            heatmaply(match_frame,na_col = "black",
                     fontsize = 10,
                     )
        })
        ## Observe event allows for changing options
        observeEvent(input$game, {
            game <- input$game
            characters <- game_char_list[[game]]
            updateSelectInput(session, "character", "Choose a character", characters)
        })
    }
)
