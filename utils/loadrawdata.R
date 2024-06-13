library(tidyverse)
library(dplyr)

LoadFromFilePaths <- function(filePathMeta, filePathEvent, filePathSample) {
  print(filePathMeta)
  dataset_meta <- read.csv(filePathMeta, na.strings="NULL", sep=";")
  dataset_event <- read.csv(filePathEvent, na.strings="NULL", sep=";")
  dataset_sample <- read.csv(filePathSample, na.strings="NULL", sep=";")
  dataset_meta <- PreprocessMeta(dataset_meta, filePathMeta)
  dataset <- MergeDatasets(dataset_meta, dataset_event, dataset_sample)
  return(dataset)
}

LoadFromDirectory <- function(dir, event = "Event", sample = "Sample", meta = "Meta", sep=';') {
  #dir = "Kiwi"
  #event = "Game"
  #sample = "Sample"
  #meta = "Meta"
  #sep = ";"
  #paste("*",event,".csv",sep="")
  #main game event data
  #exclude <- list.files(path = dir, pattern = "^[^~]")
  
  df_event = data.frame()
  df_sample = data.frame()
  df_meta = data.frame()
  
  if (!is.null(event)){
  df_event <- list.files(recursive=TRUE ,path = dir,
                      pattern = paste0(event,"\\.csv$"),
                      full.names = T) %>% 
    tibble(filename = ., fID = .) %>%   
    mutate(file_contents = map(filename,~ read_delim(file.path(.), delim = sep, na = "NULL", col_types = cols(.default = "c"))))  %>% 
    unnest(cols=-filename) %>%
    mutate(file_contents = NULL) %>%
    mutate(fID = dirname(fID))
  }
  if (!is.null(sample)){
  #sample data
  df_sample <- list.files(recursive=TRUE ,path = dir,
                            pattern = paste0(sample,"\\.csv$"), 
                            full.names = T) %>% 
    tibble(filename = ., fID = .) %>%   
    mutate(file_contents = map(filename,~ read_delim(file.path(.), delim = sep, na = "NULL", col_types = cols(.default = "c"))))  %>% 
    unnest(cols=-filename) %>%
    mutate(file_contents = NULL) %>%
    mutate(fID = dirname(fID))
  }
  if(!is.null(meta)){
  #meta data
  df_meta <- list.files(recursive=TRUE ,path = dir,
                             pattern = paste0(meta,"\\.csv$"), 
                             full.names = T) %>%
    tibble(filename = ., fID = .) %>%   
    mutate(file_contents = map(filename,~ read_delim(file.path(.), delim = sep, na = "NULL", col_types = cols(.default = "c"))))  %>% 
    unnest(cols=-filename) %>% 
    separate(col=filename,sep="_",into=c("i5","i6","i7","i8","i9"), remove=F) %>%
    separate(col=filename,sep="/",into=c("i0","i1","i2","i3","i4"), remove=T) %>%
    mutate(file_contents = NULL) %>%
    mutate(fID = dirname(fID))
    
  #browser()
  df_meta <- PreprocessMeta(df_meta)
  } else{
    
    df_meta <- list.files(recursive=TRUE ,path = dir,
                          pattern = meta, 
                          full.names = T) %>% 
      tibble(filename = ., fID = .) %>%   
      unnest(cols=-filename) %>% 
      separate(col=filename,sep="_",into=c("i5","i6","i7","i8","i9"), remove=F) %>% 
      separate(col=filename,sep="/",into=c("i0","i1","i2","i3","i4"), remove=F) %>% 
      add_column(SessionID= NA) %>%
      mutate(fID = dirname(fID))
    
  }

  dataset <- MergeDatasets(df_meta, df_event, df_sample)
  return(dataset)
}

PreprocessMeta <- function(dataset_meta) {
  
  req_cols = c("Timestamp","Email","Framecount")
  if(!FALSE %in% (req_cols %in% names(dataset_meta))) {
    dataset_meta <- dataset_meta %>%
      rename(MetaTimestamp = Timestamp,
             MetaEmail = Email,
             MetaFramecount = Framecount)
  }
  return(dataset_meta)
}

MergeDatasets <- function(dataset_meta, dataset_event, dataset_sample) {
  df = data.frame()

  test = !is.null(dataset_meta[["SessionID"]])
  joinstring = ifelse(test, "SessionID", "fID")
  test = length(na.omit(unique(dataset_meta[["SessionID"]]))) > 1
  joinstring = ifelse(test, "SessionID", "fID")

  if(joinstring != "fID") {
    # Remove fID from dataset_meta if we dont use it.
    dataset_meta = dataset_meta %>% mutate(fID = NULL) 
  } else {
    # Remove SessionID from dataset_meta if we dont use it.
    dataset_meta = dataset_meta %>% mutate(SessionID = NULL) 
  }
  
  # if event dataset is not null, use it.
  if (!is.null(dataset_event)) {
    df <- df %>% bind_rows(dataset_event)
  }
  # if sample dataset is not null, bind it.
  if (!is.null(dataset_sample)) {
    df <- df %>% bind_rows(dataset_sample)
  }
  # if meta dataset is not null, join it.
  if (!is.null(dataset_meta)) {
    if (!is.null(df[[joinstring]])) {
      df <- df %>% left_join(dataset_meta, by=joinstring)
    } else {
      # if no filename column is present, metadata is the only data.
      df <- dataset_meta
    }
  }
  
  return(df)
}
