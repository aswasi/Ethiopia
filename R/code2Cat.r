code2Cat = function(df, dict, oldVar, newVarName){
  # Assumes that the dictionary will be a 2-column data frame with key in first column and value in the second.
  # Third column is an optional column explaining the relationship.  Should be some sort of logical operator, 
  # e.g. '==', '<', '<=', ...
  
  if(ncol(dict) == 2) {
    # Assumes all relationships are equalities
    dict = dict %>% mutate(rel = '==')
  } else if (ncol(dict) != 3) {
    stop('Not enough columns in dictionary.  Dictionary should be 2 or 3 columns.')
  }
  
  nestedCondit = ""
  
  for (i in 1:nrow(dict)) {
    nestedCondit = paste0(nestedCondit, 
                          'ifelse(', oldVar, dict[i,3], ' "', dict[i,1], '" , "', dict[i,2], '" , ')
  }
  
  closure = paste(rep(')', nrow(dict)), collapse = '')
  
  nestedCondit = paste0(nestedCondit, "NA", 
                        closure)
  
  newDF = mutate_(df, .dots= setNames(list(nestedCondit), newVarName))
  
  return(newDF)
}

     