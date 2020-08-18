#' Setup results database
#'
#' @param pcfg 
#'
#' @return
#' @export
#' 
#' @name PE.db
PE.db.connection <- function(pcfg) {
   db.path <- file.path(pcfg@scratch.dir,sprintf("%s.sqlite",pcfg@project.name))
   dbConnect(RSQLite::SQLite(), db.path)
}


#' @export
#' @rdname PE.db
PE.db.setup <- function(pcfg) {
  this.db <- PE.db.connection(pcfg)
  if(!PE.cfg$db$extract %in% dbListTables(this.db)) {
    extract.cols <-  
      c("fragId INTEGER NOT NULL PRIMARY KEY",
        "srcName",
        "srcType",
        "realization",
        "startDate",
        "date",
        "leadIdx",
        "data") 
    tbl.cmd <- 
      sprintf("CREATE TABLE %s(%s)", 
                PE.cfg$db$extract,
                paste(extract.cols, collapse = ", "))
    dbExecute(this.db, tbl.cmd)
  }
  dbDisconnect(this.db)
}

#' @export
#' @rdname PE.db
PE.db.clear.datasource <- function(this.db,this.datasrc) {
  this.tbl <- tbl(this.db, PE.cfg$db$extract)
  #Get row IDs where we
  row.ids <- 
    this.tbl %>%
    filter(srcName == !!this.datasrc@name,
           srcType == !!this.datasrc@type) %>%
    select(fragId) %>%
    collect()
    
  #Delete rows
  SQL.cmd <- sprintf("DELETE FROM %s WHERE fragID IN (%s)",
                     PE.cfg$db$extract,
                     paste(row.ids$fragId,collapse=" , "))
  n <- dbExecute(this.db,SQL.cmd)
  log_msg("Deleted %i rows from results database...\n",n)
  
}

#' @export
#' @rdname PE.db
PE.db.calc.realMeans <- function(this.db,this.datasrc) {
  #Internal averaging function
  calc.realMean <- function(frags) {
    fragstack <- raster::brick(frags)
    realmean <- raster::mean(fragstack) #Dispatching can be a bit strange here sometimes
    return(list(realmean))
  }
  
  #Extract data and perform averaging
  frag.dat <- 
    tbl(this.db,PE.cfg$db$extract) %>%
    filter(srcName == !!this.datasrc@name,
           srcType == !!this.datasrc@type) %>%
    collect() %>%
    PE.db.unserialize()
  
  realMeans <- 
    frag.dat %>%
    group_by(srcName,srcType,startDate,date,leadIdx,.drop=TRUE) %>%
    summarise(data=calc.realMean(data),
              duplicate.realizations=any(duplicated(realization))) %>% #Check for duplicated realization codes
    ungroup()
  if(any(realMeans$duplicate.realizations)) stop("Duplicate realizations detected in database. Rebuild.")

  #Write to database 
  realMeans %>%
    select(-duplicate.realizations) %>%
    add_column(realization="realmean",.after="srcType") %>%
    PE.db.appendTable(this.db, PE.cfg$db$extract)
  log_msg("Wrote %i realisation means...\n",nrow(realMeans))
  
  return(invisible(NULL))
}

#' @details PE.db.appendTable serialises the data column and writes the data to the specified table
#' @export
#' @rdname PE.db
PE.db.appendTable <- function(this.dat,this.db,this.tbl) {
    this.dat %>%
    mutate(data=map(data,serialize,NULL)) %>%
    dbWriteTable(conn=this.db, name=this.tbl, append = TRUE)
}

#' @details PE.db.unserialize  unserialises the data column
#' @export
#' @rdname PE.db
PE.db.unserialize <- function(this.dat) {
    mutate(this.dat,
           data=map(data,unserialize))
}


