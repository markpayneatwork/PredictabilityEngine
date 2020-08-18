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
