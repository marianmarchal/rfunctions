#Generate lookup table
makeLookupTable <- function(data, id.var, key.length) {
  if (anyDuplicated(data[, id.var])) warning('Duplicate id values in data.')
  aliases <- c(1,1) # Allow the while loop to begin
  while (any(duplicated(aliases))) { # Loop until all keys are unique
    aliases <- replicate(length(unique(data[, id.var])), 
                         paste(sample(c(LETTERS, 0:9), key.length, replace = T), collapse = ''))
  }
  lookup.table <- data.frame(id = unique(data[, id.var]), key = aliases)
  return(lookup.table)
}
# Replace names with aliases
addAlias_old <- function(data, id.var, lookup.table) {
  data[, id.var] <- lookup.table[, 'key'][match(data[, id.var], lookup.table[, 'id'])]
  return(data)
}

#addAlias sometimes gave errors, so this is an updated version which also raises a warning if a key is missing for one of the workers.
addAlias <- function(expdat, lookupdat, expdatid = "workerid", lookupid = "id", lookupkey = "key"){
  dat <- merge(expdat, lookupdat, by.x = expdatid, by.y = lookupid, all.x = T)
  dat <- dat[ , -which(names(dat) == expdatid)]
  names(dat)[names(dat) == lookupkey] <- expdatid
  dat <- dat[,c(ncol(dat),1:(ncol(dat)-1))]
  if(sum(is.na(dat[expdatid])) > 0){warning("Could not identify key for all workers.")}
  if(nrow(expdat) != nrow(dat)){warning("Number of rows changed")}
  return(dat)
}

# Replace aliases with names
replaceAlias <- function(data, id.var, lookup.table) {
  data[, id.var] <- lookup.table[, 'id'][match(data[, id.var], lookup.table[, 'key'])]
  return(data)
}

hashed_id <- function(x, salt) {
  y <- paste(x, salt)
  y <- sapply(y, function(X) digest(X, algo="sha1"))
  as.character(y)
}

# Example use

# Step 1 ----

## Option 1: Create lookup from scratch ----

# aliasLookup <- dat %>% makeLookupTable('workerid', 8)
# # - The first argument is your dataset name, the second argument is the column you are pseudonymizing, in our case the workerid. The number is the length of your pseudonymized ID, in this case 8 characters.
# # - Note that the function prompts a warning if there are multiple rows for one participant and only unique identifiers are written to the lookup table. The warning is therefore not problematic.


## Option 2: Combine with old lookup ----

#load previous lookup table
# previousLookup <- read.csv("data/00-raw/alias_lookup.csv")

#create additional lookup table only for workerids not already in lookup
# addLookup <-
#   dat %>% filter(!workerid %in% previousLookup$id) %>% makeLookupTable('workerid', 8)

#combine previous and new Lookup
# aliasLookup <- rbind(addLookup, previousLookup)
# 
# while (nrow(distinct(aliasLookup, key)) != nrow(distinct(dat, workerid))) {
#   warning("aliasLookup did not provide the correct number of keys")
#   addLookup <-
#     dat %>% filter(!workerid %in% previousLookup$id) %>% makeLookupTable('workerid', 8)
#   #combine previous and new Lookup
#   aliasLookup <- rbind(addLookup, previousLookup)
# }

# Step 2 ----

# Replace names with aliases
# datPseudo <- dat %>% addAlias(aliasLookup, "workerid")

# ### If necessary: undo pseudonymization
# 
# #Replace aliases with names that returns the original data
# datNames <- replaceAlias(datPseudo, 'workerid', aliasLookup)
