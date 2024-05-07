# Function to create an accuracy table for the GA runs
create_accuracy_table <- function(data1, data2, data3) {
  # Calculate metrics for each dataset
  metrics <- function(data) {
    c(
      Best = min(data[,2]), # Assuming column 2 is the fitness value
      Mean = mean(data[,2]),
      SD = sd(data[,2])
    )
  }
  
  # Apply the metrics function to each GA dataset
  results1 <- metrics(data1)
  results2 <- metrics(data2)
  results3 <- metrics(data3)
  
  # Combine the results into a data frame
  accuracy_table <- data.frame(
    GA1 = results1,
    GA2 = results2,
    GA3 = results3
  )
  
  # Transpose the data frame to get the desired format
  accuracy_table <- t(accuracy_table)
  colnames(accuracy_table) <- c("Best Fitness", "Mean Fitness", "Standard Deviation")
  
  # Print the table
  print(accuracy_table)
}

# Assuming parsemydata1, parsemydata2, parsemydata3 are the final generation data for each GA
create_accuracy_table(parsed_data1, parsed_data1, parsed_data1)







extract_fitness_at_generations <- function(data1, data2, data3, generations) {
  fitness_values <- sapply(generations, function(gen) {
    c(
      data1[gen, 2], # Assuming the second column is the fitness value
      data2[gen, 2],
      data3[gen, 2]
    )
  })
  
  # Convert to data frame
  fitness_df <- as.data.frame(t(fitness_values))
  colnames(fitness_df) <- c("GA1 Fitness", "GA2 Fitness", "GA3 Fitness")
  rownames(fitness_df) <- paste(generations)
  
  return(fitness_df)
}

# Sample generations where you want to extract fitness values
generations <- c(5, 10, 15, 20, 25, 30)

# Assuming parsemydata1, parsemydata2, parsemydata3 are data frames containing the GA results with rows indexed by generations
fitness_table <- extract_fitness_at_generations(parsed_data1, parsed_data1, parsed_data1, generations)

# Print the table
print(fitness_table)





