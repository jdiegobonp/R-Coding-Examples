# Import library
library(data.table)

# Dataset taken from https://footballcsv.github.io/
dt = fread("../data/eng.1.csv")
print(dt)
names(dt)
head(dt)

# Split goals per team in separate columns
dt[, c("G_T1", "G_T2") := tstrsplit(FT, "–")]
# Convert goals to numeric
dt[, c("G_T1", "G_T2") := lapply(.SD, as.numeric), .SDcols = c("G_T1", "G_T2")]
names(dt)
head(dt)

# Create Standings Table
standings <- data.table(
  Team = unique(c(dt$`Team 1`, dt$`Team 2`)), 
  Points = 0, 
  GoalDifference = 0, 
  GoalsFor = 0
)

str(standings)

# Update Standings Table Function
update_standings <- function(st, row) {
  
  if (row$G_T1 > row$G_T2) {
    st[Team == row$`Team 1`, Points := Points + 3]  # Winner
  } else if (row$G_T2 > row$G_T1) {
    st[Team == row$`Team 2`, Points := Points + 3]  # Winner
  } else {
    st[Team == row$`Team 1`, Points := Points + 1]  # Tie
    st[Team == row$`Team 2`, Points := Points + 1]  # Tie
  }
  
  st[Team == row$`Team 1`, GoalsFor := GoalsFor + row$G_T1]
  st[Team == row$`Team 2`, GoalsFor := GoalsFor + row$G_T2]
  
  st[Team == row$`Team 1`, GoalDifference := GoalDifference + (row$G_T1 - row$G_T2)]
  st[Team == row$`Team 2`, GoalDifference := GoalDifference + (row$G_T2 - row$G_T1)]
  
  return(st)
}

# Iterate over the dataset
for (i in 1:nrow(dt)) {
  # Call the function to update the standings table
  standings <- update_standings(standings, dt[i])
}

# Sort the standings table
setorderv(standings, c("Points", "GoalDifference"), order = c(-1, -1))

print(standings)
