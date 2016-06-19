library(engsoccerdata)
data("england")
library(data.table)
df <- as.data.table(england)
rm(england)
library(lubridate)
df$Date <- ymd(df$Date)
head(df)

home <- function(team = NULL){
        home <- df[home == team]
        return(home)
}

homeStats <- function(team = NULL){
        home <- df[home == team]
        wins <- nrow(home[result == "H"]) / nrow(home)
        losses <- nrow(home[result == "A"]) / nrow(home)
        draws <- nrow(home[result == "D"]) / nrow(home)
        wdl <- data.frame(wins = wins, draws = draws, losses = losses, total = sum(wins, losses, draws))
        temp <- home[, .(wins = sum(result == "H"), 
                 draws = sum(result == "D"), 
                 losses = sum(result == "A")), 
             by = .(visitor)]
        results <- temp[, .(pwins = round(wins / sum(wins, draws, losses), 2), 
                            pdraws = round(draws / sum(wins, draws, losses), 2), 
                            plosses = round(losses / sum(wins, draws, losses), 2)), 
                        by = .(visitor)]
        resultsSummary <- colMeans(results[, !c("visitor"), with = FALSE])
        return(wdl)
        return(resultsSummary)
        
}