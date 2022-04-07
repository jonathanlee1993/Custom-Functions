modify_and_read_sql_query_RODBC <- 
  function(dir, query, chgs) {
    
    # This will break if there are 10 or more arguments, I need to fix this up if that is the case
    
    query <- str_glue('{dir}/{query}.sql') %>% readr::read_file()
    arglength <- query %>%
      str_extract_all(pattern = 'arg[0-9]',
                      simplify = TRUE) %>%
      as.vector() %>%
      unique() %>% 
      sort()
    arg_tbl <- tibble(ARGS = c(arglength) %>% unique %>% sort, CHGS = chgs)
    
    get_replacement <- function(arg) {
      return( {arg_tbl %>% filter(ARGS == arg) %>% pull(CHGS)} )
    }
    
    query %<>% str_replace_all(pattern = 'arg[0-9]', replacement = get_replacement)
    
    return(
      RODBC::sqlQuery(con, query) %>%
        as_tibble() %>%
        mutate(across(where(is.factor), as.character))
    )
    
  }