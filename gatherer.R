library("R.utils")
library(tidyverse)

# Implementation of algorithm by Derek Gatherer in R
#
#
# Given a range of years from 1975-2019 (similar voting schemes)
# For each pair of countries
#   Get actual votes from donor to recipient country in range of years
#   Calculate actual average vote
#   
#   Run Simulation for x times:
#     For each year in year range:
#       Caclulate scoring probabilities per year
#       Assign simulated vote
#     Calculate average simulated vote for 1 simulation 
#     Append to average simulated votes
#   Get 5th percentile of average simulated votes
# If actual average vote > 5th percentile of average simulated votes
#   Donor votes significantly for recipient at 0.05
# If same holds for recipient to donor, then there is collusive voting at 5% sig. lvl

setwd(getwd())

gatherer <- function(start_year, end_year, sig=0.05, scores=c(12,10,8,7,6,5,4,3,2,1), simulations=10000) {
  # Validation that end year is greater than start year
  stopifnot(end_year > start_year)
  
  # read csv and change column names
  all_df <- read_csv('data/ESC Country Votes 1975-2019.csv',
                     col_names = c("year", "finaltype", "edition", "jurytv", "from", "to", "points", "duplicate"))
  print(head(all_df))
  
  # consider only votes within range of years for finals only
  range_df <- all_df %>% filter(year >= start_year, year <= end_year, is.na(duplicate), finaltype == 'f', jurytv == 'J')
  range_df$points <- as.integer(range_df$points)
  range_df
  
  country_pairs <- unique(range_df[c('from', 'to')])
  country_pairs
  
  # remove countries that only gave votes in finals and never participated
  country_pairs <- country_pairs %>% filter(from %in% range_df$to)
  
  # Lists and vecotrs to create dataframe
  from_list <- c()
  to_list <- c()
  
  year_votes_list <- list()
  time_window <- (end_year - start_year) + 1 + 1
  for(i in 1:time_window) {
    year_votes_list[i] <- c()
  }
  
  print(year_votes_list)
  
  avg_actual_list <- c()
  sim_threshold_list <- c()
  
  print(lengths(country_pairs)[['from']])
  
  # for length of number of country pairs
  for (i in 1:lengths(country_pairs)[['from']]) {
    # get from and to countries
    print(i)
    
    country_pair <- country_pairs %>% slice(i)
    from_country <- country_pair[['from']]
    to_country <- country_pair[['to']]
    
    from_list <- c(from_list, from_country)
    to_list <- c(to_list, to_country)
    
    
    # get mean actual points from one country to another
    actual_mean <- range_df %>% filter(from == from_country, to == to_country) %>% summarise(mean_points = mean(points))
    
    avg_actual_list <- c(avg_actual_list, actual_mean[['mean_points']])
    
    average_simulation <- c()
    
    # Start simuilation
    for(j in 1:1000) {
      single_scores <- c()
      for(my_year in start_year:end_year) {
        index_for_list <- (my_year- start_year) + 1
        # check if pair exists in year
        if (lengths(range_df %>% filter(from == from_country, to == to_country, year == my_year))[[1]] > 0) {
          #print(my_year)
          
          if(j==1){
            points_for_year <- range_df %>% filter(from == from_country, to == to_country, year == my_year)
            year_votes_list[[index_for_list]] <- c(year_votes_list[[index_for_list]], points_for_year$points)
          }
          
          
          # get number of countries participating in final
          num_countries <- length(unique(range_df %>% filter(year == my_year) %>% select(c('to')))[[1]])
          
          # Get probabilities of getting points and not getting points
          prob_points <- 1/(num_countries-1)
          prob_no_points <- 1 - (length(scores))/(num_countries-1)
          # print(paste('Probability of getting points:',toString(prob_points), sep = ' '))
          # print(paste('Probability of NOT getting points:',toString(prob_no_points), sep = ' '))
          
          # Set probabilities for getting a score > 0 and score == 0
          scores_sim <- sample(c(scores, 0), size = 1, replace = TRUE, prob = c(rep(prob_points, each= 10), prob_no_points))
          single_scores <- c(single_scores, scores_sim)
        } else {
          if (j==1) {
            year_votes_list[[index_for_list]] <- c(year_votes_list[[index_for_list]], NA)
          }
          
        }
      }
      average_simulation <- c(average_simulation, mean(single_scores))
    }
    
    #print(average_simulation)
    
    quant <- quantile(average_simulation, probs = seq(0, 1, by= 0.05))
    # print(quant)
    
    print(paste('Actual average score:', toString(actual_mean[['mean_points']]), sep = ' '))
    print(paste('Simulated top 5% score:', toString(quant[['95%']]), sep = ' '))
    
    sim_threshold_list <- c(sim_threshold_list, quant[['95%']])
    
    if(actual_mean[['mean_points']] > quant[['95%']]) {
      print(paste('Collusion detected from:',from_country,'to:',to_country, sep = ' '))
    } else {
      print(paste('NO Collusion detected from:',from_country,'to:',to_country, sep = ' '))
    }
    
  }
  
  #create dataframe

  print(from_list)
  print(to_list)
  
  print(year_votes_list)
  
  print(avg_actual_list)
  print(sim_threshold_list)
  
  df <- data.frame(from_list, to_list, avg_actual_list, sim_threshold_list)
  print(df)
  
  print(c('To', 'From', start_year:end_year, '5%sim'))
  
  for(i in start_year:end_year) {
    list_index <- (i - start_year) + 1
    df[[toString(i)]] <- year_votes_list[[list_index]]
  }
  
  print(' ')
  colnames(df) <- c('To', 'From', 'Average', 'Sim.Threshold', start_year:end_year)
  
  return(df)
}

start_year <- 1975
end_year <- 1979

df <- gatherer(start_year, end_year)
print(df)

file_name <- paste("results/ESC",toString(start_year),toString(end_year),'.csv', sep = '')
print(file_name)
write.csv(df, file_name, row.names = FALSE)
