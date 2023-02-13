schemaJoin <- function(data, schema) {
  
  checks <- lapply(seq_along(1:nrow(schema)), function(i) {
    
    join <- schema$joinTypes[[i]](data[[schema$parentTable[[i]]]], data[[schema$childTable[[i]]]], 
                                  by = setNames(schema$foreignKey[[i]], schema$primaryKey[[i]]))
    
    check <- checkJoin(data, join, schema, schemaPosition = i)
  })
}

checkJoin <- function(data, joinedTable, schema, schemaPosition) {
  
  # First, check to see if there are any duplications of columns, which if happens, must be resolved before comparing
  duplicatedNames <- names(joinedTable)[grepl("\\.[xy]$", names(joinedTable))]
  if (length(duplicatedNames) > 0) stop("Duplicated names in schema position ", sQuote(schemaPosition), " specifically for columns ", sQuote(paste(duplicatedNames, collapse = ", ")), call. = F)
  
  parentTable <- data[[schema$parentTable[[schemaPosition]]]] %>% 
    arrange(across(all_of(schema$primaryKey[[schemaPosition]])))
  parentName <- schema$parentTable[[schemaPosition]]
  
  parentTableCheck <- joinedTable %>% 
    select(names(parentTable)) %>% 
    distinct() %>% 
    arrange(across(all_of(schema$primaryKey[[schemaPosition]])))
  
  parentCheck <- all.equal(parentTable, parentTableCheck)
  
  childTable <- data[[schema$childTable[[schemaPosition]]]] %>% 
    arrange(across(all_of(schema$primaryKey[[schemaPosition]])))
  childName <- schema$childTable[[schemaPosition]]
  
  childTableCheck <- joinedTable %>% 
    select(names(childTable)) %>% 
    distinct() %>% 
    arrange(across(all_of(schema$primaryKey[[schemaPosition]])))
  
  childCheck <- all.equal(childTable, childTableCheck)
  
  if (all(parentCheck == T) & all(childCheck == T)) {
    T
  } else {
    if (any(parentCheck != T)) {
      warning("Potential join errors in schema position ", sQuote(schemaPosition), ", parent table ", sQuote(schema$parentTable[[schemaPosition]]), " and child table ", sQuote(schema[["childTable"]][[schemaPosition]]), ".", call. = F)
      returnList <- list(parentName = parentTable,
                         parentTableCheck = parentTableCheck,
                         allEqual = parentCheck)
      names(returnList) <- c(parentName, "parentTableCheck", "allEqual")
      
      return(returnList)
    }
    if (any(childCheck != T)) {
      warning("Potential join errors in schema position ", sQuote(schemaPosition), ", child table ", sQuote(schema[["childTable"]][[schemaPosition]]), " and parent table ", sQuote(schema[["parentTable"]][[schemaPosition]]), ".", call. = F)
      returnList <- list(childName = childTable,
                         childTableCheck = childTableCheck,
                         allEqual = parentCheck)
      names(returnList) <- c(childName, "childTableCheck", "allEqual")
      
      return(returnList)
    }
  }
}
