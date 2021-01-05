#' Setup results database
#'
#' @param pcfg 
#'
#' @return
#' @export
#' 
#' @name PE.db
PE.db.connection <- function(pcfg,results.db=FALSE) {
  if(!results.db) {
    db.path <- here(pcfg@scratch.dir,sprintf("%s.sqlite",pcfg@project.name))
  } else {
    db.path <- here(pcfg@scratch.dir,sprintf("%s_results.sqlite",pcfg@project.name))
  }
   dbConnect(RSQLite::SQLite(), db.path,synchronous=NULL)
}


#' @export
#' @rdname PE.db
PE.db.setup <- function(pcfg,results.only=FALSE) {
  #Setup connection
  this.db <- PE.db.connection(pcfg,results.db=results.only)
  
  # Data extraction tables -----------------------------------------------------------
  if(!results.only) {
    #Setup Extraction table and indices
    if(!PE.cfg$db$extract %in% dbListTables(this.db)) {
      tbl.cols <-  
        c("pKey INTEGER NOT NULL PRIMARY KEY",
          "srcFname TEXT",
          "srcType TEXT",
          "srcName TEXT",
          "realization TEXT",
          "startDate TEXT",
          "date TEXT",
          "leadIdx INTEGER",
          "field BLOB") 
      tbl.cmd <- 
        sprintf("CREATE TABLE %s(%s)", 
                PE.cfg$db$extract,
                paste(tbl.cols, collapse = ", "))
      dbExecute(this.db, tbl.cmd)
      idx.cmd <- 
        sprintf("CREATE INDEX idx_%s ON %s(srcType,srcName)",
                PE.cfg$db$extract,
                PE.cfg$db$extract)
      dbExecute(this.db,idx.cmd)
    }
    #Setup climatology table
    if(!PE.cfg$db$climatology %in% dbListTables(this.db)) {
      tbl.cols <-  
        c("pKey INTEGER NOT NULL PRIMARY KEY",
          "srcType TEXT",
          "srcName TEXT",
          "leadIdx INTEGER",
          "month INTEGER",
          "nYears INTEGER",
          "statistic TEXT",
          "field BLOB")  
      tbl.cmd <- 
        sprintf("CREATE TABLE %s(%s)", 
                PE.cfg$db$climatology,
                paste(tbl.cols, collapse = ", "))
      dbExecute(this.db, tbl.cmd)
    }
    #Setup calibration table and indices
    if(!PE.cfg$db$calibration %in% dbListTables(this.db)) {
      tbl.cols <-  
        c("pKey INTEGER NOT NULL PRIMARY KEY",
          "srcType TEXT",
          "srcName TEXT",
          "calibrationMethod TEXT",
          "realization TEXT",
          "startDate TEXT",
          "date TEXT",
          "leadIdx INTEGER",
          "field BLOB")  
      tbl.cmd <- 
        sprintf("CREATE TABLE %s(%s)", 
                PE.cfg$db$calibration,
                paste(tbl.cols, collapse = ", "))
      dbExecute(this.db, tbl.cmd)
      idx.cmd <- 
        sprintf("CREATE INDEX idx_%s ON %s(srcType,srcName)",
                PE.cfg$db$calibration,
                PE.cfg$db$calibration)
      dbExecute(this.db,idx.cmd)
      idx.cmd <- 
        sprintf("CREATE INDEX idx_%s_calMethod ON %s(calibrationMethod,realization,srcType)",
                PE.cfg$db$calibration,
                PE.cfg$db$calibration)
      dbExecute(this.db,idx.cmd)
      idx.cmd <- 
        sprintf("CREATE INDEX idx_%s_realization ON %s(realization)",
                PE.cfg$db$calibration,
                PE.cfg$db$calibration)
      dbExecute(this.db,idx.cmd)
    }
  }
    
  # Results tables ------------------------------------------------------------------
  #Setup statistics table
  if(!PE.cfg$db$stats %in% dbListTables(this.db)) {
    tbl.cols <-  
      c("pKey INTEGER NOT NULL PRIMARY KEY",
        "srcType TEXT",
        "srcName TEXT",
        "calibrationMethod TEXT",
        "realization TEXT",
        "startDate TEXT",
        "date TEXT",
        "leadIdx INTEGER",
        "spName TEXT",
        "statName TEXT",
        "resultName TEXT",
        "field BLOB",
        "value REAL")  
    tbl.cmd <- 
      sprintf("CREATE TABLE %s(%s)", 
              PE.cfg$db$stats,
              paste(tbl.cols, collapse = ", "))
    dbExecute(this.db, tbl.cmd)
    idx.cmd <- 
      sprintf("CREATE INDEX idx_%s ON %s(srcType,srcName)",
              PE.cfg$db$stats,
              PE.cfg$db$stats)
    dbExecute(this.db,idx.cmd)    
    idx.cmd <- 
      sprintf("CREATE INDEX idx_%s_stat ON %s(statName,spName)",
              PE.cfg$db$stats,
              PE.cfg$db$stats)
    dbExecute(this.db,idx.cmd)    
  }
  
  #Enable support for parallel read/write, based on this SO reply:
  #https://stackoverflow.com/questions/36831302/parallel-query-of-sqlite-database-in-r
  RSQLite::dbClearResult(RSQLite::dbSendQuery(this.db, "PRAGMA journal_mode=WAL;"));
  dbDisconnect(this.db)
}

PE.db.safe.try <- function(expr,silent=TRUE,n.max=100) {
  i <- 0
  while(i<n.max) {
    rtn <- try(expr,silent=silent)  
    if(!is(rtn, "try-error")) return(rtn)
    Sys.sleep(runif(1,0.01,0.1)+i/10)  #Avoid constant polling. Add some stochasticity to break synchronisation
    i <- i+1
  }
  stop(sprintf("Maximum number of tries exceeded after %i attempts. %s\n",
               n.max,rtn))
}


#' @export
#' @rdname PE.db
PE.db.delete.by.pKey <- function(pcfg,tbl.name,pKeys,silent=TRUE) {
  #Delete rows
  SQL.cmd <- sprintf("DELETE FROM %s WHERE pKey IN (%s)",
                     tbl.name,
                     paste(pKeys,collapse=" , "))
  
  #Poll until can get access to DB
  db.con <- PE.db.connection(pcfg)
  PE.db.safe.try(n <- dbExecute(db.con,SQL.cmd),silent=silent)  
  dbDisconnect(db.con)
  if(!silent) { log_msg("Deleted %i rows from %s table...\n",n,tbl.name)}
  return(invisible(n))
}


#' @export
#' @rdname PE.db
PE.db.delete.by.datasource <- function(pcfg,tbl.name=PE.cfg$db$extract,datasrc,silent=TRUE) {
  #Setup query
  SQL.cmd <- sprintf("SELECT pKey FROM %s WHERE `srcType` = '%s' AND `srcName` = '%s'",
                     tbl.name,
                     datasrc@type,
                     datasrc@name)
  #Fetch list to delete
  del.these <- PE.db.getQuery(pcfg,SQL.cmd,silent=silent)
  
  #Delete
  n <- PE.db.delete.by.pKey(pcfg,tbl.name,del.these$pKey,silent=silent)
  return(invisible(n))
}


#' @details PE.db.appendTable serialises the data column and writes the data to the specified table
#' @export
#' @rdname PE.db
PE.db.appendTable <- function(dat,pcfg,tbl.name,silent=TRUE) {
  #Serialise data
  serial.dat <- 
    dat %>%
    mutate(across(where(is.list),function(cl) map(cl,serialize,NULL))) 
  #Write
  db.con <- PE.db.connection(pcfg)
  n <- PE.db.safe.try(dbWriteTable(conn=db.con, name=tbl.name, value=serial.dat, append = TRUE),silent=silent)  
  #Fin
  dbDisconnect(db.con)
  return(invisible(n))
}


#' @details PE.db.unserialize  unserialises the data column
#' @export
#' @rdname PE.db
PE.db.unserialize <- function(this.dat) {
  this.dat %>%
    as_tibble() %>%
  mutate(across(where(~is(.x,"blob")),function(cl) map(cl,unserialize)))
}



#' @details PE.db.getQuery performs a concurrency-safe query
#' @export
#' @rdname PE.db
PE.db.getQuery <- function(pcfg,this.sql,silent=TRUE) {
  #Open connection
  db.con <- PE.db.connection(pcfg)
  #Get query
  PE.db.safe.try(rtn <- dbGetQuery(conn=db.con, this.sql),silent=silent)  
  #Fin
  dbDisconnect(db.con)
  return(rtn)
}

