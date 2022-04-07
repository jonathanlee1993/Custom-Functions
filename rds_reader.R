rds_reader <- 
  function(dir) {
    
    # SECTION 1: FUNCTIONS, LIBRARIES, AND ARGUMENTS ----
    
    # * Checking arguments provided for potential errors ----
    
    # ** is.list(dir) ----
    if (is.list(dir)) stop('The argument dir must not be provided as a list. Use c() notation or simple pass one string value')
    
    # ** is.character(dir) ----
    if (!is.character(dir)) stop('The argument dir must be a vector of length one, with the value provided of character type')
    
    # ** length(dir) == 1----
    if (length(dir) != 1) stop('The argument dir must be a vector of length one')
    
    # ** fs::dir_exists(dir) ----
    if (!fs::dir_exists(dir)) stop('The argument dir must be of an already existing directory')
    
    # ** Directory provided has >=1 RDS files (Input later) ----
    
    # SECTION 2: READING RDS FILES ----
    
    rds_paths_tbl <- fs::dir_info(dir)
    
    rds_paths_tbl %<>%
      mutate(obj_name_chr = path %>% {gsub(dir, '', ., fixed = TRUE)} %>% str_replace_all('.rds', ''))
    
    rds_files_tbl <- rds_paths_tbl %>%
      mutate(obj = path %>% map(~ read_rds(.))) 
    
    # SECTION 3: ASSIGNING RDS FILES WITHIN GLOBAL ENVIRONMENT ----
    
    invisible(pmap(list(rds_files_tbl$obj_name_chr, rds_files_tbl$obj), ~assign(.x, .y, envir = .GlobalEnv)))
    
    # SECTION 4: OUTPUT MESSAGE ----
    
    message(str_glue('All RDS files located in the: \n"{dir}"\ndirectory have now been loaded into the Global Environment'))
    
  }