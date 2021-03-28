library(tidyverse)
load("all_games_n_chars.rda")
load("all_tier_matrices.rda")

second_matrix_games <- list()

for (j in seq(all_character_matrix)) {
  game_mat <- all_character_matrix[[j]]
  game_name <- names(all_character_matrix[j])
  
  for (k in seq(nrow(game_mat) )) {
    ## Get char data and car columns
    char = colnames(game_mat)[k]
    loop_table <- game_mat[,-k]
    compare_col <-  game_mat[,k,drop = FALSE]
    ## This apply isn't what we want it basically sorts on a pseudo tier list
    ## I need to find a way to properly do this with weighting peoples scores
    #secondary_out <- apply(loop_table, 2, function(x){sum(abs(compare_col - x),na.rm = T)})
    ## Idea, if birdie has a matchup that is 5.5 and Vega is 5.2 it should result in -0.3 to the score
    ## and if birdie has a matchup that is 4.8 and vega is 5 it should add .2
    
    ## This apply compares the loops character to all other characters, finding the
    ## difference between their matchup spread, if their matchups are more favorable
    ## they get a postive score, if less favorable they get a negative score
    ## This score is then added together to determine how well the other characters
    ## compliments the main character of the loop.
    secondary_out <- apply(loop_table, 2, function(x){sum(-(compare_col - x),na.rm = T)})
    ## Fixing names
    secondary_out <- c(NA, secondary_out)
    names(secondary_out)[1] <- char
    if (k == 1) {
      #Make table
      secondary_table <- as.data.frame(secondary_out)
      colnames(secondary_table)[k] <- char
    }else{
      #Sort table so it fits the laready made table
      ind <- match(rownames(secondary_table), names(secondary_out))
      sorted_sec <- secondary_out[ind]
      secondary_table <- cbind(secondary_table, sorted_sec)
      colnames(secondary_table)[k] <- char
    }
  }
  second_matrix_games[[game_name]] <- secondary_table 
}

second_matrix_games_jon <- list()

for (j in seq(all_character_matrix)) {
  game_mat <- all_character_matrix[[j]]
  game_name <- names(all_character_matrix[j])
  
  for (k in seq(nrow(game_mat) )) {
    ## Get char data and car columns
    char = colnames(game_mat)[k]
    loop_table <- game_mat[,-k]
    compare_col <-  game_mat[,k,drop = FALSE]
    ## This apply isn't what we want it basically sorts on a pseudo tier list
    ## I need to find a way to properly do this with weighting peoples scores
    #secondary_out <- apply(loop_table, 2, function(x){sum(abs(compare_col - x),na.rm = T)})
    ## Idea, if birdie has a matchup that is 5.5 and Vega is 5.2 it should result in -0.3 to the score
    ## and if birdie has a matchup that is 4.8 and vega is 5 it should add .2
    
    ## This apply compares the loops character to all other characters, finding the
    ## difference between their matchup spread, if their matchups are more favorable
    ## they get a positive score, if less favorable they get a negative score
    ## This score is then added together to determine how well the other characters
    ## compliments the main character of the loop.
    secondary_out <- apply(loop_table, 2, function(x){
      init_compare <- -(compare_col - x)
      jon_compare <- init_compare[init_compare > 0]
      sum(jon_compare, na.rm = TRUE)
    })
    
    ## Fixing names
    secondary_out <- c(NA, secondary_out)
    names(secondary_out)[1] <- char
    if (k == 1) {
      #Make table
      secondary_table <- as.data.frame(secondary_out)
      colnames(secondary_table)[k] <- char
    }else{
      #Sort table so it fits the laready made table
      ind <- match(rownames(secondary_table), names(secondary_out))
      sorted_sec <- secondary_out[ind]
      secondary_table <- cbind(secondary_table, sorted_sec)
      colnames(secondary_table)[k] <- char
    }
  }
  second_matrix_games_jon[[game_name]] <- secondary_table 
}

save(second_matrix_games, second_matrix_games_jon, file = "secondary_matrix_all_games.rda")
save(second_matrix_games, second_matrix_games_jon, file = "secondary_selector/secondary_matrix_all_games.rda")