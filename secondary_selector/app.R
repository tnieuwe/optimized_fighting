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
load("secondary_matrix_all_games.rda")
all_games <- names(all_character_matrix)

game_char_list <- lapply(all_character_matrix, colnames)


secondary_ordered <- function(second_tabl, char){
    ### By using the previously generated table we can use this functiont to select
    ### a character and pull their best secondaries, and how the tier list changes
    ### compared to the optimal secondary list
    
    ## Pull main character, order on decreasing, remove main character
    temp_tab <- second_tabl[,char, drop = FALSE]
    temp_tab <- temp_tab[order(temp_tab,decreasing = T),, drop = FALSE]
    temp_tab <- temp_tab[!is.na(temp_tab),,drop = FALSE]
    ## Get tier list, but remove character again
    tier_filt <- rownames(second_tabl)[!(rownames(second_tabl) %in% char)]
    ## Combine numeric with teir list, and make sorted secondary list a column
    temp_tab <- cbind(temp_tab, tier_filt)
    temp_tab <- rownames_to_column(temp_tab, "Secondary Rank")
    colnames(temp_tab)[2:3] <- c("Secondary Score", "Current Tier List")
    ## Move around columns
    temp_tab <- select(temp_tab, `Secondary Score`, `Secondary Rank`, `Current Tier List`)
    ## Get the secondary rank change when compared to tier list
    `Tier Rank Change` <- match(temp_tab[,2], temp_tab[,3])- seq(nrow(temp_tab))
    ## Bind data together
    temp_tab <- cbind(temp_tab, `Tier Rank Change`) #%>%
    #mutate(`Tier Rank Change` = ifelse(`Current Tier List` == char, NA, `Tier Rank Change`))
    ## Return final output
    return(temp_tab)
}

shinyApp(
    ui = fluidPage( 
        sidebarLayout(
            sidebarPanel(
        selectInput("game", "Choose a game:", names(game_char_list)),
        selectInput("character", "Choose a character", choices = NULL),
        selectInput("second_alg", "What secondary algorithm do you want?", c("Complimentary Characters",
                                                                             "Pocket Characters")),
        textInput("plotheight", "Plot Height (px)", "800"),
        textInput("plotwidth", "Plot Width (px)", "")
        
        ),
        mainPanel(tabsetPanel(type = "tab",
                              tabPanel("Matchup Chart",
                                       plotlyOutput("hmap"
                                                    )
                                       ),
                              tabPanel("Secondary Selector", dataTableOutput("second")),
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
        
        output$second <- renderDataTable({
            # second_matrix_selected <- ifelse(input$second_alg %in% "Complimentary Characters",
            #                                  second_matrix_games,
            #                                  second_matrix_games_jon)
            if (input$second_alg == "Complimentary Characters") {
                second_matrix_selected <- second_matrix_games
            } else{
                second_matrix_selected <- second_matrix_games_jon
            }
            secondary_ordered(second_matrix_selected[[input$game]], input$character)
            })
        
        ## Observe event allows for changing options
        observeEvent(input$game, {
            game <- input$game
            characters <- game_char_list[[game]]
            updateSelectInput(session, "character", "Choose a character", characters)
        })
    }
)
