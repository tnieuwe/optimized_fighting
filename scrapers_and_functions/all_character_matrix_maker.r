library(tidyverse)
library(rvest)
all_character_matrix <- list()
all_vote_matrix <- list()
## Games that can't be analyzed with the current technique
skip_games <- c("Marvel vs. Capcom 2", "Skullgirls 2nd Encore",
                "Ultimate Marvel vs. Capcom 3",
                "Tatsunoko vs. Capcom", "Under Night In-Birth Exe:Late[st]")
for (j in seq(game_chars_list)) {
  game_name <- names(game_chars_list)[j]
  if (game_name %in% skip_games) {
    next
  }
  for (k in seq(game_chars_list[[j]])) {
    
    char_html = game_chars_list[[j]][k]
    char = game_chars_name[[j]][k]
    
    #Get the html and pull out the tables
    #html_hold <- character_links[k]
    webpage <- read_html(char_html)
    
    tbls <- html_nodes(webpage, "table") 
    
    table_out <- tbls %>% html_table()
    ## Filter tables specifically to tables that include characters 
    filter_table <- table_out[lapply(table_out,
                                     function(x){colnames(x)[2]}) == "Character"]
    
    
    results_table <- do.call(rbind, filter_table)
    #colnames(results_table) <- c("Character", "match avg.")
    ## Generate the column that will be added into the final frame
    final_column <- results_table %>% select(Character, `Match avg.`) %>%
      column_to_rownames(var = "Character")
    colnames(final_column) <- char
    ##Add data frame for total votes
    final_vote_column <- results_table %>% select(Character, `Tot. votes`) %>%
      column_to_rownames(var = "Character")
    colnames(final_vote_column) <- char
    ## If this is the first column we add an extra row including the current
    ## character as an NA. That column then becomes the first `final_table`
    if (char == game_chars_name[[j]][1]) {
      missing_col <- NA
      final_column <- rbind(final_column, missing_col)
      rownames(final_column)[nrow(final_column)]  <- char
      final_table <- final_column
      ## Final vote table
      missing_col <- NA
      final_vote_column <- rbind(final_vote_column, missing_col)
      rownames(final_vote_column)[nrow(final_vote_column)]  <- char
      final_vote_table <- final_vote_column
    }else{
      ## Else just bind the tables together after properly sorting on rownames
      ind <- match(rownames(final_table), rownames(final_column))
      sorted_col <- final_column[ind,,drop = F]
      final_table <- cbind(final_table,sorted_col)
      ## Final vote version
      sorted_col <- final_vote_column[ind,,drop = F]
      final_vote_table <- cbind(final_vote_table,sorted_col)
    }
    
    
  }
  ind <- match(colnames(final_table), rownames(final_table))
  
  ordered_final_table <- final_table[ind,] 
  ordered_final_votes <- final_vote_table[ind,]
  all_character_matrix[[game_name]] <- ordered_final_table
  all_vote_matrix[[game_name]] <- ordered_final_votes
  
}

lapply(all_vote_matrix, function(x){
  colnames(x) <- rownames(x)
})

save(all_character_matrix, all_vote_matrix, file = "all_tier_matrices.rda")
save(all_character_matrix, all_vote_matrix, file = "secondary_selector/all_tier_matrices.rda")