laggedDataProcessing = function(processed_data, state, location){
  # First 39 cols
  n = nrow(processed_data)
  lagged_data = processed_data[1:(n-1),(3 * location + -2) : (3 * location)]
  # Next 52 cols
  empirical_data = processed_data[1:(n-1),(40 + 4 * location + -3) : (40 + 4 * location)]
}