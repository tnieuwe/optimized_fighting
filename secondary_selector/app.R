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
        textInput("plotheight", "Plot Height (px)", "800"),
        textInput("plotwidth", "Plot Width (px)", "")
        
        ),
        mainPanel(tabsetPanel(type = "tab",
                              tabPanel("Matchup Chart",
                                       plotlyOutput("hmap"
                                                    )
                                       ),
                              tabPanel("Secondary Selector", textOutput("result")),
                              tabPanel("Secondary Chart"),
                              tabPanel("Votes per Matchup",
                                       plotlyOutput("vmap"))
            
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
            ## Change font size based on char num
            font = 10
            font <-  case_when(
                nrow(match_frame) > 80 ~ font * .57,
                nrow(match_frame) > 50 ~ font * .7,
                TRUE ~ font)
            
            heatmaply(match_frame,na_col = "black",
                      fontsize_row = font,
                      fontsize_col = font,
                     Rowv = FALSE,
                     Colv = FALSE,
                     main = "Matchup favorability by Z-scores; How character X does vs character Y",
                     ylab = "Character Y",
                     xlab = "Character X"
                     ) %>%
                ## Added reactive heights
                layout(height = input$plotheight,
                       width = input$plotwidth)
        })
        
        output$vmap <- renderPlotly({
            vote_frame <- as.matrix(all_vote_matrix[[input$game]])   
            ## Change font size based on char num
            font = 10
            font <-  case_when(
                nrow(vote_frame) > 80 ~ font * .6,
                nrow(vote_frame) > 50 ~ font * .7,
                TRUE ~ font)
            
            heatmaply(vote_frame,na_col = "black",
                      fontsize_row = font,
                      fontsize_col = font,
                      Rowv = FALSE,
                      Colv = FALSE,
                      main = "Votes per Matchup"
            ) %>%
                ## Added reactive heights
                layout(height = input$plotheight,
                       width = input$plotwidth)
        })
        
        ## Observe event allows for changing options
        observeEvent(input$game, {
            game <- input$game
            characters <- game_char_list[[game]]
            updateSelectInput(session, "character", "Choose a character", characters)
        })
    }
)
