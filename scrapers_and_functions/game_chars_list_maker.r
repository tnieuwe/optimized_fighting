library(tidyverse)
library(rvest)

tier_games_url <- read_html("https://www.eventhubs.com/tiers/")
tbls <- html_nodes(tier_games_url, "table") 
all_games <- tbls[3] %>%
  html_nodes("td a") %>%
  html_attr('href')
table_out <- tbls %>% html_table()
game_titles <- table_out[[3]][,2]

game_links <- paste0("https://www.eventhubs.com", all_games)
## Remove extra /
game_links <- unique(gsub("[/]$", "",game_links))

game_chars_list <- list()
game_chars_name <- list()
for (k in seq(game_links)) {
  game_title <- game_titles[k]
  game_link <- game_links[k]
  
  game_url <- read_html(game_link)
  tbls <- html_nodes(game_url, "table") 
  
  all_characters <- tbls[1] %>%
    html_nodes("td a") %>%
    html_attr('href')
  
  character_links <- paste0("https://www.eventhubs.com", all_characters)
  ## Remove extra /votes
  character_links <- character_links[!str_detect(character_links, "/vote")]
  game_chars_list[[game_title]] <- character_links
  table_out <- tbls %>% html_table()
  teir_table <- table_out[[1]]
  game_chars_name[[game_title]] <- teir_table$Character
}
## Don't uncomment the code below until eventhubs fixes the smash webpages
#save(game_chars_list, game_chars_name, file = "all_games_n_chars.rda")
#save(game_chars_list, game_chars_name, file = "secondary_selector/all_games_n_chars.rda")

