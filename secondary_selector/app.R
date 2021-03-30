#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(plotly)
library(shinyHeatmaply)
library(heatmaply)
library(faq)

load("all_tier_matrices.rda")
load("secondary_matrix_all_games.rda")
all_games <- names(all_character_matrix)

game_char_list <- lapply(all_character_matrix, colnames)

intro <- "This tool uses data acquired from Eventhub's publically voted upon fighting game matchup data. Using this data we can generate full matchup charts, matchup based tier lists, and a tool that helps a person pick their main's best secondary character. Publically voted upon data is of course not perfect, but it is interesting to see what people think."


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
        theme = shinythemes::shinytheme("readable"),
        titlePanel("Eventhubs Fighting Game Data"),
        ## Saved from here
        tabsetPanel(type = "tabs",
                    tabPanel("Matchup_chart",
                             sidebarPanel(intro,
                                 selectInput("game", "Choose a game:", names(game_char_list)),
                                 checkboxInput("t_f_tiers", "Cluster heatmap on tiers?", FALSE),
                                 sliderInput("num_tiers_heat", "How many tiers?", 2, 7, 3),
                                 textInput("plotheight_heat", "Heatmap Height (px)", "800"),
                                 textInput("plotwidth_heat", "Heatmap Width (px)", "")
                             ),
                             mainPanel(
                                 plotlyOutput("hmap"
                                 )
                             )
                             ),
                    tabPanel("Secondary Selector",
                             sidebarPanel(
                                 intro,
                                 selectInput("game", "Choose a game:", names(game_char_list)),
                                 selectInput("character", "Choose a character", choices = NULL),
                                 selectInput("second_alg", "What secondary algorithm do you want?", c("Complimentary Characters",
                                                                                                      "Pocket Characters"))
                             ),
                             mainPanel( p("Secondary Score: A calculate value of how good a secondary a character is to a main, the higher the better. Used to order Secondary Rank."),
                                        p("Tier Rank Change: Determines if a character outperforms their position in the tier list by secondary score"),
                                        "See FAQ and https://github.com/tnieuwe/optimized_fighting for further information",
                                        dataTableOutput("second")
                                        )),
                    tabPanel("Calculate Tiers",
                             sidebarPanel(
                                 selectInput("game", "Choose a game:", names(game_char_list)),
                                 sliderInput("num_tiers", "How many tiers?", 2, 7, 3)
                             ),
                             mainPanel(
                                 dataTableOutput("tiers_out"),
                                 "Tiers are generated using hierarchical clustering on the mean matchup for every character. These tiers exclusively look at all matchups and do not take into account the meta which greatly influences tier lists due to character popularity."
                             )
                             ),
                    tabPanel("Matchup Variance per Game (balance)",
                             sidebarPanel(
                                 selectInput("game", "Choose a game:", names(game_char_list))
                             ),
                             mainPanel(
                                 plotOutput("hist"),
                                 "By using the variance of the matchup matrices we can see how 'balanced' a game is. If matchups are relatively even across the board variance will be low, if there are many skewed matdchups variance will be high. See Smash Melee/Brawl."
                             )
                             ),
                    tabPanel("Vote per Matchup",
                             sidebarPanel(
                                 selectInput("game", "Choose a game:", names(game_char_list)),
                                 
                             ),
                             mainPanel(
                                 plotlyOutput("vmap")
                             )
                             ),
                    tabPanel("FAQ",
                             sidebarPanel(
                                 intro
                             ),
                             mainPanel(faqOutput("faq_out")))
                             )
        ),
    server = function(input, output, session) {
        output$result <- renderText({
            paste("You chose", paste(input$game,input$character, sep = ":"))
        })
        ### Matchup heatmap
        output$hmap <- renderPlotly({
            dat <- as.matrix(all_character_matrix[[input$game]])
            ## Removing Z-score for the time being
            #match_frame <- ((dat - mean(dat,na.rm = T))/sd(dat,na.rm = T))
            match_frame <- dat
            ## Change font size based on char num
            font = 10
            font <-  case_when(
                nrow(match_frame) > 80 ~ font * .57,
                nrow(match_frame) > 50 ~ font * .7,
                TRUE ~ font)
            if (input$t_f_tiers == FALSE) {
            heatmaply(match_frame,na_col = "black",
                      fontsize_row = font,
                      fontsize_col = font,
                     Rowv = FALSE,
                     Colv = FALSE,
                     main = "Matchup Chart; How character X does vs character Y (X-Y notation)",
                     ylab = "Character Y",
                     xlab = "Character X"
                     ) %>%
                ## Added reactive heights
                layout(height = input$plotheight_heat,
                       width = input$plotwidth_heat)
            } else{
            dendo <- hclust(d=dist(colMeans(match_frame, na.rm = TRUE), method = "euclidian"), method = "complete")
            
            clusts <- cutree(dendo, k = input$num_tiers_heat)
            
            average_matchup <- colMeans(match_frame, na.rm = TRUE)
            
            initial_tiers <- as.data.frame(cbind(average_matchup, clusts)) %>%
                rownames_to_column(var = "Characters") %>%
                group_by(clusts) %>%
                mutate(characters_per_tier = paste0(Characters, collapse = ", ")) %>%
                group_by(characters_per_tier)
            initial_tiers$Clusters <- LETTERS[initial_tiers$clusts]
            
            initial_tiers_row <- initial_tiers %>% ungroup() %>%
                select(Characters, Clusters) %>% 
                column_to_rownames(var = "Characters")
            colnames(initial_tiers_row) <- "Tiers"
            heatmaply(match_frame,na_col = "black",
                      fontsize_row = font,
                      fontsize_col = font,
                      Rowv = dendo,
                      Colv = FALSE,
                      row_side_colors = (initial_tiers_row),
                      main = "Matchup Favorability; How character X does vs character Y (X-Y notation)",
                      ylab = "Character Y",
                      xlab = "Character X" 
                ) %>%
                ## Added reactive heights
                layout(height = input$plotheight_heat,
                       width = input$plotwidth_heat)
            }
        })
        ### Vote map
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
        ### Variance histogram
        output$hist <- renderPlot({
            list_var  <- lapply(all_character_matrix, FUN = function(x){
                var(as.vector(as.matrix(x)), na.rm = T)
            })
            df_var <- as.data.frame(t(as.data.frame(list_var)))
            game_var <-  round(var(as.vector(as.matrix(all_character_matrix[[input$game]])), na.rm = TRUE), 3)
            ggplot(df_var, aes(V1)) +
                geom_histogram(stat="bin", color = "black", fill = "white") +
                theme_classic(base_size = 15) +
                geom_vline(xintercept = game_var,
                           color = "red",
                           linetype="dotted", size = 1.5) +
                labs(title = "Variance of Matchups Histogram",
                     subtitle = "How balanced a game is across the cast") +
                xlab("Variation") +
                ylab("Count of Games") +
                geom_text(x = game_var + .1, y = 9, angle = 90,
                          label = paste("Variance: ", game_var), size = 7.5,)
        }, height = 400,
        width = 700)
        
        ### Secondary Selector
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
        ### Tier List Generator
        output$tiers_out <- renderDataTable({
            match_frame <- all_character_matrix[[input$game]]
            dendo <- hclust(d=dist(colMeans(match_frame, na.rm = TRUE), method = "euclidian"), method = "complete")
            
            clusts <- cutree(dendo, k = input$num_tiers)
            
            average_matchup <- colMeans(match_frame, na.rm = TRUE)
            
            initial_tiers <- as.data.frame(cbind(average_matchup, clusts)) %>%
                rownames_to_column(var = "Characters") %>%
                group_by(clusts) %>%
                mutate(characters_per_tier = paste0(Characters, collapse = ", ")) %>%
                group_by(characters_per_tier)
            final_tier_list <- initial_tiers %>%
                summarise(`Tier Mean Matchup` = mean(average_matchup),
                          `Tiers` = mean(clusts)) %>%
                select(Tiers, `Tier Mean Matchup`, `Ordered Tier Characters` = "characters_per_tier") %>%
                arrange(desc(`Tier Mean Matchup`))
            final_tier_list$Tiers <- LETTERS[seq(nrow(final_tier_list))]
            final_tier_list
        })
        
        ## Observe event allows for changing options
        observeEvent(input$game, {
            game <- input$game
            characters <- game_char_list[[game]]
            updateSelectInput(session, "character", "Choose a character", characters)
        })
        ## Make FAQ dataframe
       output$faq_out <- renderFaq({
                            faq_df <- data.frame(
                                question = c("What are fighting games and what is this?",
                                              "Where is the data from?",
                                              "Is there a github repository for this project?",
                                              "How are secondary scores calculated?",
                                             "What's the difference between 'Complimentary Characters' and 'Pocket Characters'",
                                             "What is the Tier Rank Change column and why does it exist?"
                                             ),
                                answer = c("Basically, fighting games are zero-sum videogames that often pit two players against each other in virtual combat. There are many subgenres of fighting games but a common theme across them is multiple characters to select as fighters. One of the goals in developing fighting games is to make characters have unique playstyles, this is to give players more options and to generate unique character interactions While this makes the games more interesting it also results in purposefully unequal gameplay as different characters have different tools. Therefore, whenever two different characters fight eachother, often one character has the edge over the other in the 'matchup' as they either have better tools or their playstyle/gameplan is more effective against the character they are playing against (Zoners beat Grapplers who beat Rush Down who beat Zoners). As a result each character has a unique matchup againast every other character in the game and understanding these matchups is often vital to doing well. However, some people often find there are matchups so bad for their character that they decide to pick up a second character to play those unfavorable matches with. And finding the best characters to cover another characters weaknesses is the exact reason I made this shiny app.",
                                           "The data is from EventHubs.com, Eventhubs tier lists are generated through players voting on how favorable or unfavorable a given matchup is for a character in a game. You vote by giving how much in favor a matchup is eg. an even matchup is 5-5, a favored matchup is 6-4 and a lopsided matchup is 8-2. I heavily suggest exploring the website as it is a great source of information, an account is required to vote, but if you have a unique understanding of a matchup its a good place to vote. Though realize these are not expert opinions in this data, because anyone can vote.",
                                           "Yes: https://github.com/tnieuwe/optimized_fighting",
                                           "This formula goes through every character in the matchup matrix and subtracts their matchup score from the main character's matchup score. So if the secondary character has a matchup agaisnt another character that is good (say a score of 7) when compared to the main character (say a score of 4), the resulting number would be -3. We then change the sign of all the differences (the number would now be 3) and then sum across all matchups. We do this for every character in the matchup chart. This results in a number that we can order the characters on based on how they perform in matchups relative to the main character.",
                                           "Complimentary Characters is made to find characters that compliment eachother well across both their matchup spreads and is described in the above formula. Pocket Characters is better at selecting a character that exclusively covers the weaknesses of the main characters, not really focusing on if the main character covers the secondary character's weaknesses. This is done by removing all negative matchups in the secondary score analysis before summing.",
                                           "The purpose of secondary rank change is to deal with the inherent problem of this project. No matter who you're playing, logically speaking the best secondary for you to pick up in fighting games is normally the best character in the game, because they have the best matchups across the entire cast. We realized this problem early on, and so we came up with this solution. We compare every character to their position in the current tier list, and changes in their position reflect if the outperform or underperform your character in certain matchups. For example if a character moves up two spots in the secondary rank, that means given the main character's matchup chart, that secondary character outperforms other characters in covering the main character's weakness.")
                            )
                            faq(faq_df)
        })
    }
)
