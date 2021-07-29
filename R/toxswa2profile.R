


toxswa2profile <- function(SaveDirectory, ProjectsDirectory = "C:/SwashProjects", 
                           SearchString = "",  saveGraph = FALSE){
  
  directories <- list.files(ProjectsDirectory) # Run for each directory in the SwashProjects folder.
  
  my_directories <- directories[str_detect(directories, "\\.\\w+$", negate = TRUE)]
  my_directories <- my_directories[str_detect(my_directories, SearchString)]
  
  for(directory in my_directories) # For each of those folders
  {
    
    
    projDirectory <-  paste0(ProjectsDirectory, "/",directory) #save the full address, just incase
    
    
    reportFile <- read.delim(paste0(projDirectory,"/" , directory, "_report.txt")) # REad the report file to get the run numbers and corresponding crops
    
    
    index <- which(sapply(reportFile[,1], function(x){str_detect(x, "(?<=\\*\\s)\\d+(?=\\s)")})) # Index is a vector of the line numbers in the file that match the regex
    
    runNo <- sapply(index, function(x){str_extract(reportFile[x,1], pattern = "(?<=\\*\\s)\\d+(?=\\s)" )}) # runNo saves a list of the run numbers
    Crop <- sapply(index, function(x){str_extract(reportFile[x,1], pattern = "(?<=\\*\\s\\d{1,5})\\s+.*(?=[DR]\\d)" )}) # Crop is the corresponding crop types
    Scenario <- sapply(index, function(x){str_extract(reportFile[x,1], pattern = "[DR]\\d+\\w+(?=\\s)" )})
    
    cropLookup = cbind(runNo, Crop, Scenario) 
    cropLookup = data.frame(cropLookup, stringsAsFactors = F) # Save cropLookup as a table of run numbers and crops to check later
    
    
    myDirectory <- paste0(ProjectsDirectory, "/",directory,"/TOXSWA") # the working directory now is the TOXSWA folder
    files <- list.files(myDirectory, pattern = "^.*\\.out$") # files is the list of ".sum" files in that folder
    
    
    
    
    if(length(files > 0)) # if there are files in the directory: analyse them, if not: skip it.
    {
      
      for(j in 1:length(files)) # For each of the files, we are adding a row to my_results
      {
        
        
        thisFile <- read.delim(paste0(myDirectory,"/",files[j])) # thisFile is the current .sum file
        
        
        index <- which(sapply(thisFile[,1], function(x){str_detect(x, "ConLiqWatLay")})) 
        
        
        data <- data.frame( Day <- integer(), Month <- character(), Year <- integer(), Hour <- character(), Concentration <- double())
        for(i in 2:length(index))
        {
          
          datestrings <- unlist(str_split(str_extract(thisFile[index[i],1], "\\d\\d-\\w\\w\\w-\\d\\d\\d\\d-\\d\\d\\w\\d\\d") , "-"))
          values <- unlist(str_extract_all(thisFile[index[i], 1], "\\d+\\.(\\w{7}-\\d\\d|\\w{6})"))
          Conc<- values[length(values)]
          
          row <- data.frame( as.integer(datestrings[1]), datestrings[2], as.integer(datestrings[3]), 
                             datestrings[4], as.double(Conc))
          
          # colnames(row) <- c("Day", "Month", "Year", "Hour", "Concentration")
          
          data <- rbind(data, row)
          
        }
        rm(thisFile)
        rm(index)
        colnames(data) <- c( "Day", "Month", "Year", "Hour", "Concentration")
        
        
        
        data2 <- data %>% unite(Day, Month, Year, col = "DayofYear", sep = "-") %>%
          group_by(DayofYear) %>% summarise(Conc = max(as.double(Concentration))) %>% 
          mutate(Date=dmy(DayofYear)) %>% arrange(Date) %>% select(-Date)
        
        data2$Day <- 1:nrow(data2)
        data2 <- data2 %>% select(-DayofYear)
        data2 <- data2[,c("Day", "Conc")]
        
        if(saveGraph){}
        data2 %>% ggplot(aes(x = Day, y = Conc)) + geom_line() + theme_bw() + ggtitle(paste(directory, thisScenario))
        ggsave(paste0(save_directory,directory,thisScenario,".png"), width = 6, height = 4, units = "in")
      }
      thisRunno <- unlist(str_split(files[j], ".out"))[1]
      thisScenario <- unlist(cropLookup %>% filter(runNo == as.integer(thisRunno)) %>% select(Scenario))
      
      write.csv(data2, paste0(save_directory,directory,thisScenario,".csv"))
      
      
    }}
}