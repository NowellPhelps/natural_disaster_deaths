# Nowell Phelps 
# Aug 15 2024
# JHU Biostat PhD Assessment - Data Analysis Question 1 Code

# -----------------------
# Functions 
# -----------------------

clean_deathtoll <- function(string){
  
  # Function to clean string such that:
  
  # - We first remove any characters enclosed by [] (including the brackets), as some of the table entries contain these. 
  # - We then classify the string into one of the two below cases and return an integer as outlined below
  
  #  (a) If a range defined by an upper and lower bound is given, 
  #    return the midpoint of the upper and lower bound
  #  (b) If a range defined by an upper or lower bound is given, 
  #    return the bound.
  
  if (!(typeof(string)) == "character"){
    stop("The function clean_deathtoll takes character arguments only")
  }
  
  # Remove all commas
  string <- str_replace_all(string, ",", "")
  
  # If string contains "[", remove "[" character and all subsequent characters
  if (str_detect(string, "\\[")) {
    string <- substring(string, 1, str_locate(string,"\\[")[1,1]-1) 
    
  }
  
  # If string contains "+", remove "+" character and all subsequent characters
  if (str_detect(string, "\\+")) {
    string <- substring(string, 1, str_locate(string,"\\+")[1,1]-1) # Substring which contains elements preceding "+"
  }
  
  # If string contains "-", calculate mid-point of range
  if(str_detect(string, "\\–") | str_detect(string, "\\-")){
    
    if(str_detect(string, "\\–")){
      string1 <- substring(string, 1, str_locate(string,"\\–")[1,1]-1)
      string2 <- substring(string, str_locate(string,"\\–")[1,1]+1, str_length(string))
      
    } else if (str_detect(string, "\\-")) {
      string1 <- substring(string, 1, str_locate(string,"\\-")[1,1]-1)
      string2 <- substring(string, str_locate(string,"\\-")[1,1]+1, str_length(string))
    }
    
    num1 <- as.integer(string1)
    num2 <- as.integer(string2)
    
    if(num1 > num2){
      stop("Lower bound of range is greater then upper bound")
    }
    number <- (num1 + num2)/2
    
  } else{
    # If string does not contain "-", convert directly to integer
    number <- as.integer(string)
  }
  
  return(number)
  
}


clean_distasterType <- function(string){
  
  # Function to clean up disaster type categorizations
  
  if(string == "Heat Wave"){
    string <- "Heat wave"
  } else if (string == "Earthquake, Tsunami"){
    string <- "Earthquake and tsunami"
  } else if (string == "Tropical cyclone, Flood"){
    string <- "Tropical cyclone and flood"
  }
  
  return(string)
  
}
