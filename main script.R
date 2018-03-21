################################################################################
# 
# TASK:
# Create a single .csv containing the following columns:
# - Event ('5k' or '20k' for all rows)
# - Year (numeric)
# - Name ('Firstname Lastname' format)
# - Age (may be missing for recent years)
# - Sex ('M' or 'F')
# - Div (something like 'M20-29')
# - Nettime (numeric, instead of 59:05, convert this to decimal minutes
#            like 59.08)

# Import years, 6 in total
y2017 <- "http://www.newhavenroadrace.org/wp-content/uploads/2015/04/NHRR-2017-5k-results-update-3.txt"
y2016 <- "http://www.newhavenroadrace.org/wp-content/uploads/2015/03/NH16-5k-Overall.txt"
y2015 <- "http://www.newhavenroadrace.org/wp-content/uploads/2015/04/new-haven-5K-results1.txt"
y2014 <- "http://www.jbsports.com/wp-content/uploads/RESULTS-5K-overall1.txt"
y2013 <- "http://www.jbsports.com/wp-content/uploads/RESULTS-NH13-5k.txt"
y2012 <- "http://www.newhavenroadrace.org/wp-content/uploads/2015/04/NH12-5k-overall.txt"

# Combine years into list
ylist <- list(y2017, y2016, y2015, y2014, y2013, y2012) 

# Function that returns the data frame of interest given a URL input (for 1 year)

grabData <- function(website){
  x <- scan(website, what = "", sep = "\n")
  
  # Grabbing year
  year <- as.numeric(gsub(".*[0-9], ", "", x[grep(", 201[0-9]", x)]))
  
  # Setting columns widths
  equals <- grep("Name.*City", x)+1                                      
  spaces <- as.numeric(gregexpr(" ", x[equals])[[1]])
  spaces <- c(1,spaces)
  widths <- diff(spaces)
  
  # Removing tabs for year 2015
  if(website == "http://www.newhavenroadrace.org/wp-content/uploads/2015/04/new-haven-5K-results1.txt"){
    x <- gsub("\\t", "     ", x)                                         
    write(x, "2015.html")
    df <- read.fwf("2015.html", widths = widths, stringsAsFactors = FALSE)
  } else{
    df <- read.fwf(website, widths = widths, stringsAsFactors = FALSE)
  }
  
  # Grabbing names for the data frame
  names(df) <- gsub("^ *| *$", "", df[grep("==", df[,1])-1,])
  
  # Removing header from data, adding in Year and Event
  df <- df[-c(1:grep("==", df[,1])),] 
  df$year <- year
  df$Event <- "5k"
  
  # Removing spaces, merging First and Last names, setting Age and Sex to NA for Year < 2016
  if(year < 2016){
    df$`First Name` <- gsub("^ *| *$", "", df$`First Name`)
    df$`Last Name`  <- gsub("^ *| *$", "", df$`Last Name`)
    df$Name <- paste(df$`First Name`, df$`Last Name`)
    df$Age <- as.numeric(df$Age)
    df$Sex <- gsub(" ", "", df$Sex)
    
  } else{
    df$Name <- gsub("^ *| *$", "", df$Name) # Removing spaces and cleaning
    df$Age <- as.numeric(NA)                # Setting Age and Sex to NA for Year > 2015
    df$Sex <- as.character(NA)
  }
  
  # Removing spaces from Nettime
  df$Nettime <- gsub(" ", "", df$Nettime)
  
  # Converting Nettime into decimal minutes, splitting data into 2 cases: hashours and !hashours
  hours <- rep(0, length(df$Nettime))
  mins  <- rep(0, length(df$Nettime))
  secs <- rep(0, length(df$Nettime))
  hashours <- grepl("[0-9]*:[0-9]*:[0-9]", df$Nettime)
  hours[hashours] <- as.numeric(gsub(":[0-9]*:[0-9]*", "", df$Nettime[hashours]))
  mins[hashours] <- as.numeric(gsub("^[0-9]:|:[0-9][0-9]$", "", df$Nettime[hashours]))
  secs[hashours] <- as.numeric(gsub("^[0-9]:[0-9][0-9]:", "", df$Nettime[hashours]))
  hours[!hashours] <- 0
  mins[!hashours] <- as.numeric(gsub(":[0-9][0-9]$", "", df$Nettime[!hashours]))
  secs[!hashours] <- as.numeric(gsub("^[0-9][0-9]:", "", df$Nettime[!hashours]))
  df$Nettime <- 60*hours + mins + secs/60
  
  # Fixing Div, adding in a dash
  if(year < 2014){
    df <- df[,-2]
    rhs <- gsub("^ *[A-Z][0-9][0-9]", "", df$Div)
    lhs <- gsub("^ *|[0-9][0-9]$", "", df$Div)
    df$Div <- paste0(lhs, "-",rhs)
    finaldf <- data.frame(Event=df$Event, Year = df$year, Name = df$Name, Age = df$Age, 
                          Sex = df$Sex, Div = df$Div , Nettime = df$Nettime, stringsAsFactors = FALSE)
  } else{
    finaldf <- data.frame(Event=df$Event, Year = df$year, Name = df$Name, Age = df$Age, 
                          Sex = df$Sex, Div = df[, duplicated(colnames(df))] , Nettime = df$Nettime, stringsAsFactors = FALSE)
  }
  
  # Remove last footer row
  finaldf <- finaldf[-length(finaldf$Event),]
  
  # Return a data frame
  return(finaldf)
}

# Run on the list
result <- lapply(ylist, grabData)

# Combine into one data frame
final <- do.call(rbind, result)

# Final cleaning
final$Sex[which(final$Sex == "")] <- NA
final$Sex[which(final$Sex == "?")] <- NA
final$Div[which(final$Div == "-      ")] <- NA
final$Div[which(final$Div == "       ")] <- NA
final$Age[which(final$Age == 0)] <- NA
final$Name[which(final$Name== "" | final$Name ==" ")] <- NA

write.csv(final, "scrape_results.csv", row.names = FALSE)
