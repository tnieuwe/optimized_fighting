# optimized_fighting
An R project to determine who is an optimal secondary character in fighting games using matchup knowledge webscraped from https://www.eventhubs.com/tiers/.

## What are fighting games and what is this?
Basically, fighting games are videogames that most often pit two players against each other in virtual combat where there is one winner who emerges. There are many subgenres of fighting games but what is common in all modern iterations are multiple characters to select as fighters. One of the goals in developing fighting games is to make characters that have unique playstyles, as not everyone wants to play the same character, while this makes the games more interesting it also results in purposefully unequal gameplay as different characters have different tools. As a result whenever two different characters fight eachother, often one character has the edge over the other in the "matchup" as they have either better tools or their playstyle/gameplan is more effective against the character they are playing (Zoners beat Grapplers who beat Rush Down who beat Zoners). As a result each character has a unique matchup againast every other character in the game and understanding these matchups is often vital to doing well. However, some people often find there are matchups so bad for their character that they decide to pick up a second character to play those unfavorable matches with. And finding the best characters to cover another characters weaknesses is the exact reason I made this shiny app.

## Data source:
 Eventhubs tier lists are generated through players voting on how favorable or unfavorable a given matchup is for a character in a game. You vote by giving how much in favor a matchup is eg. an even matchup is 5-5, a favored matchup is 6-4 and a lopsided matchup is 8-2. I heavily suggest exploring the website as it is a great source of information, an account is required to vote, but if you have a unique understanding of a matchup its a good place to vote.


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
