
toxswa2table <- function(SaveDirectory, ProjectsDirectory = "C:/SwashProjects",  
                         SearchString = "", saveFullResults = FALSE){
  
  
  
  directories <- list.files(ProjectsDirectory) # Run for each directory in the SwashProjects folder.
  
  my_directories <- directories[str_detect(directories, "\\....", negate = TRUE)]
  my_directories <- my_directories[str_detect(my_directories, SearchString)]
  
  full_results <- data.frame()
  
  for(directory in my_directories) # For each of those folders
  {
    
    
    projDirectory <-  paste0(ProjectsDirectory, "/",directory) #save the full address, just incase
    
    
    reportFile <- read.delim(paste0(projDirectory,"/" , directory, "_report.txt")) # REad the report file to get the run numbers and corresponding crops
    
    index <- which(sapply(reportFile[,1], function(x){str_detect(x, "(?<=\\*\\s)\\d+(?=\\s)")})) # Index is a vector of the line numbers in the file that match the regex
    
    runNo <- sapply(index, function(x){str_extract(reportFile[x,1], pattern = "(?<=\\*\\s)\\d+(?=\\s)" )}) # runNo saves a list of the run numbers
    Crop <- sapply(index, function(x){str_extract(reportFile[x,1], pattern = "(?<=\\*\\s\\d{1,5})\\s+.*(?=[DR]\\d)" )}) # Crop is the corresponding crop types
    
    
    cropLookup = cbind(runNo, Crop) 
    cropLookup = data.frame(cropLookup, stringsAsFactors = F) # Save cropLookup as a table of run numbers and crops to check later
    
    
    myDirectory <- paste0(ProjectsDirectory, "/",directory,"/TOXSWA") # the working directory now is the TOXSWA folder
    files <- list.files(myDirectory, pattern = "^.*\\.sum$") # files is the list of ".sum" files in that folder
    
    
    if(length(files > 0)) # if there are files in the directory: analyse them, if not: skip it.
    {
      
      
      my_results <- data.frame()
      
      for(i in 1:length(files)) # For each of the files, we are adding a row to my_results
      {
        
        
        thisFile <- read.delim(paste0(myDirectory,"/",files[i])) # thisFile is the current .sum file
        
        SubstancesIndeces <- which(sapply(thisFile, function(x){str_detect(x, "Molar mass")})) - 1
        Substances <- str_trim(str_extract(thisFile[SubstancesIndeces,1], "\\s[A-Z|a-z|0-9]*$"))
        
        for(Subs in Substances)
        {
          
          
          thisRow <- c()
          
          Crop <- cropLookup[cropLookup$runNo == str_extract(files[i], "\\d{1,5}(?=\\.sum)"),]$Crop # Save the crop name, from cropLookup first
          thisRow <- c(thisRow, Crop)
          
          #Scenario
          index <- which(sapply(thisFile, function(x){str_detect(x, "Scenario")}))
          Scenario = str_extract(thisFile[index,1], pattern = "(?<=:\\s)[RD]\\d?.+\\b" ) # Then find the Scenario
          thisRow <- c(thisRow, Scenario)
          
          
          #App Details
          index <- which(sapply(thisFile, function(x){str_detect(x, "\\* Application pattern and deposition by drift on water surface")})) + 1 #Find the App date table
          APP_Rate = ""
          apps <- 0
          count  <- 1
          cont <- TRUE
          while(cont == TRUE) # Count the number of apps but reading lines until you hit a blank
          {
            if(str_detect(thisFile[index+count, 1], pattern = "^\\s+$")){cont = FALSE}
            else{
              apps = apps + 1
              count = count + 1
            }
            
          }
          thisRow <- c(thisRow, apps)
          
          apprates <- c()
          
          for(j in 1:apps){
            thisrate = str_extract(thisFile[index+j, 1], pattern = "\\s\\d+\\.?\\d+\\s")
            apprates <- c(apprates, thisrate)
            APP_Rate = paste(APP_Rate, j, ":", thisrate)
          }
          
          
          
          
          thisRow <- c(thisRow, APP_Rate)
          
          
          
          # Active Substance
          index <- min(which(sapply(thisFile, function(x){str_detect(x, "Substance")})))# Then the substance
          Substance = str_extract(thisFile[index,1], pattern = "(?<=:\\s).+\\b" )
          thisRow <- c(thisRow, Substance)
          thisRow <- c(thisRow, Subs)
          
          # StartDate
          index <- which(sapply(thisFile, function(x){str_detect(x, "Simulation Period")})) # Then the timulation period
          StartDate = str_extract(thisFile[index,1], pattern = "(?<=:\\s).+(?=\\s+to)" )   # And split it into start and end
          thisRow <- c(thisRow, StartDate)
          EndDate = str_extract(thisFile[index,1], pattern = "(?<=to\\s).+\\b" )
          thisRow <- c(thisRow, EndDate)
          
          
          
          
          
          
          
          # SW
          index <- which(sapply(thisFile, function(x){str_detect(x, paste0(" Table: PEC in water layer of substance: ", Subs))}))  
          
          
          
          # SW PEC and Date
          SW_PEC = str_trim(str_extract(thisFile[(index+5),1], pattern = "(?<=Global max)\\s+\\d+\\.?\\d+(?=\\s)" ))
          thisRow <- c(thisRow, SW_PEC)
          SW_DATE <- str_trim(str_extract(thisFile[(index+5),1], pattern = "\\d\\d-.+\\s" ))
          thisRow <- c(thisRow, SW_DATE)
          
          
          
          
          
          
          #TWA
          index <- which(sapply(thisFile, function(x){str_detect(x, paste0("Table: Maximum Time Weighted Averaged Exposure Concentrations substance: ", Subs))}))
          TWA7 = str_trim(str_extract(thisFile[index + 9, 1], pattern = "\\s\\d+\\.?\\d+\\s"))
          thisRow <- c(thisRow, TWA7)
          TWA21 <- str_trim(str_extract(thisFile[index + 11,1], pattern = "\\s\\d+\\.?\\d+\\s"))
          thisRow <- c(thisRow, TWA21)
          TWA28 <- str_trim(str_extract(thisFile[index + 12,1], pattern = "\\s\\d+\\.?\\d+\\s"))
          thisRow <- c(thisRow, TWA28)
          
          
          
          
          
          
          #SEDindex <- which(sapply(thisFile, function(x){str_detect(x, "Scenario")}))
          index <- which(sapply(thisFile, function(x){str_detect(x, paste0(" Table: PEC in sediment of substance: ", Subs))}))
          
          # SED PEC and Date
          SED_PEC = str_trim(str_extract(thisFile[(index+5),1], pattern = "(?<=Global max)\\s+\\d+\\.?\\d+(?=\\s)" ))
          thisRow <- c(thisRow, SED_PEC)
          SED_DATE <- str_trim(str_extract(thisFile[(index+5),1], pattern = "\\d\\d-.+\\s" ))
          thisRow <- c(thisRow, SED_DATE)
          
          
          
          #App Dates
          index <- which(sapply(thisFile, function(x){str_detect(x, "\\* Application pattern and deposition by drift on water surface")})) + 1 #Find the App date table
          APP_DATE = ""
          apps <- 0
          count  <- 1
          cont <- TRUE
          while(cont == TRUE) # Count the number of apps but reading lines until you hit a blank
          {
            if(str_detect(thisFile[index+count, 1], pattern = "^\\s+$")){cont = FALSE}
            else{
              apps = apps + 1
              count = count + 1
            }
            
          }
          
          appdates <- c()
          
          
          # Save the app numbers and relevant dates
          for(j in 1:apps){
            thisdate = str_extract(thisFile[index+j, 1], pattern = "\\d\\d\\-.+\\-\\d\\dh\\d\\d")
            appdates <- c(appdates, thisdate)
            APP_DATE = paste(APP_DATE, j, ":", thisdate)
          }
          
          
          
          
          thisRow <- c(thisRow, APP_DATE)
          
          method <- "Drift"
          
          if(SW_DATE %in% appdates == FALSE){
            print("test")
            print(Scenario)
            if(grepl(x = Scenario[1], pattern = "^D")) {
              method <- "Drainage"
            }
            if(grepl(x = Scenario[1], pattern = "^R")) {
              method <- "Runoff"
              
            }
          }
          
          
          
          thisRow <- c(thisRow, method)
          
          thisRow <- c(thisRow, directory)
          
          my_results <- rbind(my_results, thisRow)
        }}}
    
    
    
    
    # Save it all!
    colnames(my_results) = c( "Crop", "Scenario","Applications", "App. Rates", "Active Substance", "Measured Substance", "Start of app. window", "End of app. window", 
                              "Max PEC_sw ",
                              "Date of Max PEC_sw ",
                              "7 day TWA ","21 day TWA ", "28 day TWA ",
                              
                              "Max PEC_sed, ",
                              "Date of Max PEC_sed ",
                              
                              "Application Dates", "Method", 
                              "Directory")
    full_results <- rbind(full_results, my_results)
    write.csv(my_results, paste0(save_directory,directory,".csv"))
  }
  
  if(saveFullResults)
  {
    
    colnames(full_results) = c( "Crop", "Scenario","Applications", "App. Rates", "Active Substance", "Measured Substance", "Start of app. window", "End of app. window", 
                                "Max PEC_sw ",
                                "Date of Max PEC_sw ",
                                "7 day TWA ","21 day TWA ", "28 day TWA ",
                                
                                "Max PEC_sed, ",
                                "Date of Max PEC_sed ",
                                
                                "Application Dates", "Method", 
                                "Directory")
    
    write.csv(full_results, paste0(save_directory,"FULLRESULTS",Sys.Date(),".csv"))
  }
}

