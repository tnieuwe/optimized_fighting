# optimized_fighting
An R project to determine who is an optimal secondary character in fighting games using matchup knowledge webscraped from https://www.eventhubs.com/tiers/.

 The shiny app can be accessed here: https://32tim32.shinyapps.io/Fighting_Game_Secondary_Selector/

## What are fighting games and what is this?
Basically, fighting games are zero-sum videogames that often pit two players against each other in virtual combat. There are many subgenres of fighting games but a common theme across them is multiple characters to select as fighters. One of the goals in developing fighting games is to make characters have unique playstyles, this is to give players more options and to generate unique character interactions. While this makes the games more interesting it also results in purposefully unequal gameplay as different characters have different tools. Therefore whenever two different characters fight eachother, often one character has the edge over the other in the "matchup" as they either have better tools or their playstyle/gameplan is more effective against the character they are playing against (Zoners beat Grapplers who beat Rush Down who beat Zoners). As a result each character has a unique matchup against every other character in the game and understanding these matchups is often vital to doing well. However, some people often find there are matchups so bad for their character that they decide to pick up a second character to play those unfavorable matches with. And finding the best characters to cover another characters weaknesses is the exact reason I made this shiny app.

## What does this Shiny App have?
This Shiny App has several functions that I will list below:
### A interactive matchup heatmap for 40+ fighting games:
  * Using data gathered from Eventhubs this is a visual representation of the character matchups. In the context of a matchup (5-5, 6-4, 4-6), the character on the X axis is on the right and the character on the Y axis is on the left and the more yellow the color the more favorable the outcome. Basically if in a matchup the value is 6, that means character X has a 6-4 matchup against character Y. You can also think of it that if you summed all opposite values across the white line they would all sum to 10.
  * You can select to cluster the heatmaps into tier lists, more info on this in the **Calculate Tiers** section.
### Secondary Selector:
  * This tool allows one to pick a game and a character to find out who the best secondary character would be based on matchups.
  * Secondary Score: The secondary score is generated by a formula that has two possible options.
    * Complimentary Characters: An option that finds the character that is most "complimentary" to the one selected. This formula goes through every character in the matchup matrix and subtracts their matchup score from the main character's matchup score. So if the secondary character has a matchup against another character that is good (say a score of 7) when compared to the main character (say a score of 4), the resulting number would be -3. We then change the sign of all the differences (the number would now be 3) and then sum across all matchups. We do this for every character in the matchup chart. This results in a number that we can order the characters on based on how they perform in matchups relative to the main character. We believe that the complimentary character formula is best for picking two characters that cover each others weaknesses well.
    * Pocket Characters: This formula follows the same idea as complimentary characters, but with one change. It only considers matchups that the secondary character does better in. This is done by removing all initial positive scores when you find the difference between the main character and the secondary character. Those scores are added up similairly, however the secondary score cannot be lower than 0, and 0 means that there are no matchups where the secondary character is better than the main character.
![](https://github.com/tnieuwe/optimized_fighting/blob/main/pictures/formula_image_v2.png)
 * Secondary Tier list: The order of characters based on the secondary score
 * Tier Rank Change: The purpose of secondary rank change is to deal with the inherent problem of this project. No matter who you're playing, logically speaking the best secondary for you to pick up in fighting games is normally the best character in the game, because they have the best matchups across the entire cast. We realized this problem early on, and so we came up with this solution. We compare every character to their position in the current tier list, and changes in their position reflect if the outperform or underperform your character in certain matchups. For example if a character moves up two spots in the secondary rank, that means given the main character's matchup chart, that secondary character outperforms other characters in covering the main character's weakness.
### Calculate Tiers:
 *  This tool allows you to "mathmatically" determine the tier placements of characters based on their mean matchup.
 *  The tool uses the mean matchup value for every character, and through hierarchical clustering on the Euclidian distance between the values (literally just grouping numbers by how close they are) generates clusters of characters. Using the **How many tiers?** option you are allowed to explore what the distribution of characters look like for a given number of tiers.
 *  This functionality is also available on the Matchup Chart if you select **Cluster heatmap on tiers?**
### Matchup Variance per Game (balance)
 * For every game we find the variance of the entire matchup matrix. You can basically think about this as how "balanced" a game is. If a game has low variance that means most matchups are relatively similiar, and likely relatively balanced, while high variance games (looking at smash) have many skewed matchups.
### Votes per Matchup
 * A heatmap that simply shows how many votes each matchup has
## Data source:
 Eventhubs tier lists are generated through players voting on how favorable or unfavorable a given matchup is for a character in a game. You vote by giving how much in favor a matchup is eg. an even matchup is 5-5, a favored matchup is 6-4 and a lopsided matchup is 8-2. I heavily suggest exploring the website as it is a great source of information, an account is required to vote, but if you have a unique understanding of a matchup its a good place to vote.

## Files in scrapers_and_functions and order they need to be ran in
1. **game_chars_list_maker.r**: This file scrapes the main Eventhubs website to get data on game names and character names required for later analyses. Output file is all_games_n_chars.rda. 
*Note*: Currently this file should not be ran because the smash melee and smash brawl pages are broken on Eventhubs.
2. **all_character_matrix_maker.r**: This file scrapes the matchup data from Eventhub for ever character in everygame. This generates the matrices used in the matchup heatmaps. The output of this file is all_tier_matrices.rda.
3. **secondary_matrix_generator.r**: This file uses the matchup matrices for each game to generate a matrix of how well each character performs as a secondary to a given main character. The output of this file is secondary_matrix_all_games.rda
4. **secondary_ordered.r**: This file is the function used to properly order the secondary info. It is the function version of the **Secondary Selector** tab.

## Packages used
* **Shiny**: To make the shiny app
* **Tidyverse**:
  * **dplyr**: Data manipulation
  * **tibble**: Data manipulation
  * **ggplot2**: Plotting the variance
* **shinyHeatmaply** and **heatmaply**: Used to generate interactable Shiny heatmaps
* **viridis**: To make heatmaps color blind friendly
* **rvest**: Used to webscrape the data from Eventhubs

## Goals
- [x] Get a simple version of the analysis for a single character from a single game (Birdie)
- [x] Get the matchup matrix for a single game (SFV)
- [x] Find the best secondary for a single character (Birdie)
- [x] Find the best secondary for a single game (SFV)
- [X] Acquire data for how many votes are used in each matchup
- [X] make seperate tabs for each quest eg. Best secondary, game heatmap, game character, secondary chart?
- [X] Add Jon's formula as an optional secondary selector
- [X] Get the shiny app to work on just one game (SFV)
- [ ] Contact reddit?
- [ ] For SFV: Have an option in the shiny for official data or eventhub
