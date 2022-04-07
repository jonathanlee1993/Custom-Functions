read_sql_query_RODBC <- 
  function(dir, query) {
  
  return(
    RODBC::sqlQuery(con, {str_glue('{dir}/{query}.sql') %>% readr::read_file()}) %>%
      as_tibble() %>%
      mutate(across(where(is.factor),as.character))
  )
  
}

