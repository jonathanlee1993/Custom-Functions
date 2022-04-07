rds_writer <- 
  function(dir, obj, obj_name) {
    
    # SECTION 1: FUNCTIONS, LIBRARIES, AND ARGUMENTS ----
    
    # SECTION 2: CREATING DIRECTORY IF APPLICABLE ----
    
    # SECTION 3: WRITING RDS FILES TO PROVIDED DIRECTORY ----
    
    obj %>%
      write_rds(str_glue("{dir}{ifelse(substr(dir,nchar(dir),nchar(dir)) == '/','','/')}{obj_name}.rds"))
    
    # SECTION 4: OUTPUT MESSAGE ----
    
    message(str_glue('Object {obj_name} has been written as an RDS file to the directory: \n"{dir}"'))
    cat('\n')
    
  }