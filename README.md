# Eurovision Song Contest Collusion Detection

The aim of this project is to identify collusive patterns that occurred within the Eurovision Song Contest (ESC) from 1975 to 2019 via simulations. 
In some cases, collusive networks of three or more countries are identified, e.g. Eastern European countries and Nordic countries.

This project adopts the approach described by [Gatherer](http://jasss.soc.surrey.ac.uk/9/2/1.html).

## Requirements:
* R 3.6.3+
* tidyverse
* igraph

## Usage:

This repository contains 2 R scripts:

1. `gatherer.R` - Calculate actual averages and simulated averages for all country pairs for a given time window
2. `build_graph.R` - Create graph and table for collusive voting patterns in a given time window

To run an R script from the terminal use:
```
Rscript <filename>.R
```

The _Images_ folder contains the images of visualizations created for this part of the assignment.

The _data_ folder contains a CSV file containing voting scores for the Eurovision Song Contests betwen 1975 and 2019. Source: https://data.world/datagraver/eurovision-song-contest-scores-1975-2019

The _results_ folder contains 2 types of CSVs: 
- `CollusionXXXXYYYY.csv` contains countries that award a higher average of points to each other for the time window from year XXXX to year YYYY.
- `ESCXXXXYYYY.csv` the results of simulating Eurovision Song Contest from year XXXX to year YYYY, as well as the actual results and average of the actual results for the specified time window
