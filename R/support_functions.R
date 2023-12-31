# similarity analysis -----------------------------------------------------
# for each survey, it finds the closest matching survey with the minimum number of different columns
calculateDifferences <- function(data, input_df_survey){
  # 1) convert all columns to character
  data <- mutate_all(data, as.character)
  # 2) remove columns that are naturally different in each survey
  cols <- data.frame(column=colnames(data)) %>% 
    left_join(select(input_df_survey, name, type), by=c("column"="name")) %>% 
    filter(!(type %in% c("_uuid", "enumerator_id")) &
             !str_starts(column, "_other$") &
             !str_detect(column, "_specify$"))
  data <- data[, all_of(cols$column)]
  # 3) convert NA to "NA" and all columns to factor
  data[is.na(data)] <- "NA"
  data <- data %>% mutate_if(is.character, factor)
  # 4) calculate gower distance
  gower_dist <- daisy(data, metric="gower", warnBin=F, warnAsym=F, warnConst=F)
  gower_mat <- as.matrix(gower_dist)
  # 5) convert distance to number of differences and determine closest matching survey
  r <- c()
  for (i in 1:nrow(data)) r <- c(r, sort(gower_mat[i,]*ncol(data))[2])
  # 6) store results
  data[["n.col.not.NA"]] <- rowSums(data!="NA")
  data[["survey.id"]] <- 1:dim(data)[1]
  data[["most.similar.survey"]] <- names(r)
  data[["number.different.columns"]] <- as.numeric(r)
  data <- data %>% arrange(number.different.columns, survey.id)
  return(data)
}


# silhouette analysis based on gower distance between surveys -------------

calculateEnumeratorSimilarity <- function(data, input_df_survey, col_enum, col_admin){
  
  # helper function
  convertColTypes <- function(data, input_df_survey){
    # select_multiple: numeric or factor?
    col.types <- data.frame(column=colnames(data)) %>% 
      left_join(select(input_df_survey, name, type), by=c("column"="name")) %>% 
      mutate(type.edited = case_when(type %in% c("integer", "decimal", "calculate") ~ "numeric",
                                     str_starts(type, "select_") ~ "factor",
                                     str_detect(column, "/") ~ "factor",
                                     TRUE ~ "text"))
    
    cols <- col.types[col.types$type.edited=="numeric", "column"]
    data[,cols] <- lapply(data[,cols], as.numeric)
    cols <- col.types[col.types$type.edited=="text", "column"]
    data[,cols] <- lapply(data[,cols], as.character)
    cols <- col.types[col.types$type.edited=="factor", "column"]
    data[,cols] <- lapply(data[,cols], as.factor)
    
    return(data)
  }
  
  # convert columns using the tool
  data <- convertColTypes(data, input_df_survey)
  # keep only relevant columns
  cols <- data.frame(column=colnames(data)) %>% 
    left_join(select(input_df_survey, name, type), by=c("column"="name")) %>% 
    filter(!(type %in% c("_uuid", "enumerator_id")) &
             !str_starts(column, "_other$") &
             !str_detect(column, "_specify$"))
  # convert character columns to factor and add enum.id
  data <- data[, all_of(cols$column)] %>% 
    mutate_if(is.character, factor) %>% 
    arrange(!!sym(col_enum)) %>%
    mutate(enum.id=as.numeric(!!sym(col_enum)), .after=!!sym(col_enum))
  # add "SY" column in case col_admin is not specified
  #if (col_admin=="adm") data <- mutate(data, adm="DRC", .before=cols$column[1])
  # calculate similarity (for enumerators who completed at least 5 surveys)
  res <- data %>% split(data[[col_admin]]) %>% 
    lapply(function(gov){
      df <- gov %>% 
        group_by(enum.id) %>% 
        mutate(n=n()) %>% 
        filter(n>=5) %>% 
        ungroup() %>% 
        select_if(function(x) any(!is.na(x)))
      
      if (length(unique(df$enum.id)) > 1){
        # calculate gower distance
        gower_dist <- daisy(select(df, -c(!!sym(col_enum), enum.id)), 
                            metric = "gower", warnBin = F, warnAsym = F, warnConst = F)
        # gower_mat <- as.matrix(gower_dist)
        # calculate silhouette
        si <- silhouette(df$enum.id, gower_dist)
        res.si <- summary(si)
        # create output
        r <- data.frame(enum.id=as.numeric(names(res.si$clus.avg.widths)), si=res.si$clus.avg.widths) %>% 
          left_join(distinct(select(df, !!sym(col_admin), !!sym(col_enum), enum.id)), by="enum.id") %>% 
          left_join(group_by(df, enum.id) %>% summarise(num.surveys=n(), .groups="drop_last"), by="enum.id") %>% 
          select(!!sym(col_admin), !!sym(col_enum), num.surveys, si) %>% 
          arrange(-si)
        return(r)
      }
    })
  do.call(rbind, res)
}



# download audit files ----------------------------------------------------

download_audit_files <- function(df, uuid_column = "_uuid", audit_dir, usr, pass){
  if (!"httr" %in% installed.packages()) 
    stop("The package is httr is required!")
  
  if (is.na(audit_dir) || audit_dir == "") 
    stop("The path for storing audit files can't be empty!")
  
  if (is.na(usr) || usr == "") 
    stop("Username can't be empty!")
  
  if (is.na(pass) || pass == "") 
    stop("Password can't be empty!")
  
  # checking if the output directory is already available
  if (!dir.exists(audit_dir)) {
    dir.create(audit_dir)
    if (dir.exists(audit_dir)) {
      cat("Attention: The audit file directory was created in", audit_dir,"\n")
    }
  }
  
  # checking if creating output directory was successful
  if (!dir.exists(audit_dir))
    stop("download_audit_fils was not able to create the output directory!")
  # checking if uuid column exists in data set
  if (!uuid_column %in% names(df))
    stop("The column ", uuid_column, " is not available in data set.")
  # checking if column audit_URL exists in data set
  if (!uuid_column %in% names(df))
    stop("The column ", uuid_column, " is not available in data set.")
  if (!"audit_URL" %in% names(df))
    stop("Error: the column audit_URL is not available in data set.")
  
  # getting the list of uuids that are already downloaded
  available_audits <- dir(audit_dir)
  
  # excluding uuids that their audit files are already downloaded
  df <- df[!df[[uuid_column]] %in% available_audits,]
  
  audits_endpoint_link <- df[["audit_URL"]]
  names(audits_endpoint_link) <- df[[uuid_column]]
  audits_endpoint_link <- na.omit(audits_endpoint_link)
  
  if (length(audits_endpoint_link) > 0) {
    # iterating over each audit endpoint from data
    for (i in 1:length(audits_endpoint_link)) {
      uuid = names(audits_endpoint_link[i])
      endpoint_link_i <- audits_endpoint_link[i]
      cat("Downloading audit file for", uuid, "\n")
      
      # requesting data
      audit_file <- content(GET(endpoint_link_i,
                                authenticate(usr, pass),
                                timeout(1000),
                                progress()), "text", encoding = "UTF-8")
      
      if (!is.na(audit_file)) {
        if (length(audit_file) > 2) {
          dir.create(paste0(audit_dir, "/", uuid), showWarnings = F)
          write.csv(audit_file, paste0(audit_dir, "/", uuid, "/audit.csv"), row.names = F)
        }else if(!audit_file == "Attachment not found"){
          if (grepl("[eventnodestartend]", audit_file)) {
            dir.create(paste0(audit_dir, "/", uuid), showWarnings = F)
            write.table(audit_file, paste0(audit_dir, "/", uuid, "/audit.csv"), row.names = F, col.names = FALSE, quote = F)
          } else{
            cat("Error: Downloading audit was unsucessful!\n")
          }
        }
      } else{
        cat("Error: Downloading audit was unsucessful!\n")
      }
    }
  } else{
    cat("Attention: All audit files for given data set is downloaded!")
  }
}