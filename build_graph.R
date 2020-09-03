library(tidyverse)
library(igraph)

# build collusion graph
# If country A gives country B large scores, and vice versa, collusion is detected

setwd(getwd())

# change start and end years to detect collusion in different year ranges
start_year <- 1975
end_year <- 1979

graph_title <- paste(toString(start_year),'-',toString(end_year), sep = '')

file_name <- paste("results/ESC",toString(start_year),toString(end_year),'.csv', sep = '')

df <- read_csv(file_name)

sim_df <- df %>% filter(Average > Sim.Threshold) %>% arrange(desc(Sim.Threshold))

sim_df$na_count <- apply(sim_df, 1, function(x) sum(is.na(x)))
sim_df <- sim_df %>% filter(na_count <= 2)
head(sim_df)


country_a <- c()
country_b <- c()

for (i in 1:lengths(sim_df)[['From']]) { 
  country_pair <- sim_df %>% slice(i)
  dup_df <- sim_df %>% filter(To == country_pair[['From']], From == country_pair[['To']])
  
  if(lengths(dup_df)[['From']] > 0) {
    country_a <- c(country_a, dup_df[['To']])
    country_b <- c(country_b, dup_df[['From']])
  }
  
}

to_save_df <- sim_df %>% 
  filter(To %in% country_a, From %in% country_b)%>%
  select(From, To, Average, Sim.Threshold)
to_save_df

file_name <- paste("results/Collusion",toString(start_year),toString(end_year),'.csv', sep = '')
write.csv(to_save_df, file_name, row.names = FALSE)

# create edge list from countries
links <- data.frame(
  target=country_a,
  source=country_b
)

# create and plot collusion networks
network <- graph_from_data_frame(d=links, directed=F) 

# plot it
set.seed(41)
plot(network, 
     vertex.size=30,
     vertex.label.color="black",
     vertex.color="#fbfd8a",
     # edge.arrow.size=10,
     edge.color="#222222",
     layout = layout.fruchterman.reingold(network))
title(graph_title,cex.main=1,col.main="black")

par(bg="white")


# To Plot more Dense graphs
# tkplot(network, 
#        vertex.size=30,
#        vertex.label.color="black",
#        vertex.color="#fbfd8a",
#        edge.arrow.size=1,
#        edge.color="black",)
