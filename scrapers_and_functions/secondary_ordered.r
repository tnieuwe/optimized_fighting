## Function to get personal secondary table

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