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
  #Setup connection
  this.db <- PE.db.connection(pcfg)
  #Setup Extraction table
  if(!PE.cfg$db$extract %in% dbListTables(this.db)) {
    tbl.cols <-  
      c("pKey INTEGER NOT NULL PRIMARY KEY",
        "srcType",
        "srcName",
        "realization",
        "startDate",
        "date",
        "leadIdx",
        "data") 
    tbl.cmd <- 
      sprintf("CREATE TABLE %s(%s)", 
                PE.cfg$db$extract,
                paste(tbl.cols, collapse = ", "))
    dbExecute(this.db, tbl.cmd)
  }
  #Setup climatology table
  if(!PE.cfg$db$climatology %in% dbListTables(this.db)) {
    tbl.cols <-  
      c("pKey INTEGER NOT NULL PRIMARY KEY",
        "srcType",
        "srcName",
        "leadIdx",
        "month",
        "data",
        "nYears")  #Number of years in the climatology
    tbl.cmd <- 
      sprintf("CREATE TABLE %s(%s)", 
              PE.cfg$db$climatology,
              paste(tbl.cols, collapse = ", "))
    dbExecute(this.db, tbl.cmd)
  }
  #Setup calibration table
  if(!PE.cfg$db$calibration %in% dbListTables(this.db)) {
    tbl.cols <-  
      c("pKey INTEGER NOT NULL PRIMARY KEY",
        "srcType",
        "srcName",
        "calibrationMethod",
        "realization",
        "startDate",
        "date",
        "leadIdx",
        "data")  
    tbl.cmd <- 
      sprintf("CREATE TABLE %s(%s)", 
              PE.cfg$db$calibration,
              paste(tbl.cols, collapse = ", "))
    dbExecute(this.db, tbl.cmd)
  }
  #Setup statistics table
  if(!PE.cfg$db$stats %in% dbListTables(this.db)) {
    tbl.cols <-  
      c("pKey INTEGER NOT NULL PRIMARY KEY",
        "srcType",
        "srcName",
        "calibrationMethod",
        "realization",
        "startDate",
        "date",
        "leadIdx",
        "sdName",
        "statName",
        "field",
        "value")  
    tbl.cmd <- 
      sprintf("CREATE TABLE %s(%s)", 
              PE.cfg$db$stats,
              paste(tbl.cols, collapse = ", "))
    dbExecute(this.db, tbl.cmd)
  }
  
  #Enable support for parallel read/write, based on this SO reply:
  #https://stackoverflow.com/questions/36831302/parallel-query-of-sqlite-database-in-r
  RSQLite::dbClearResult(RSQLite::dbSendQuery(this.db, "PRAGMA journal_mode=WAL;"));
  dbDisconnect(this.db)
}

#' @export
#' @rdname PE.db
PE.db.delete.by.pKey <- function(pcfg,tbl.name,pKeys) {
  #Delete rows
  SQL.cmd <- sprintf("DELETE FROM %s WHERE pKey IN (%s)",
                     tbl.name,
                     paste(pKeys,collapse=" , "))
  
  #Poll until can get access to DB
  db.con <- PE.db.connection(pcfg)

  #Write
  repeat{
    rtn <- try(  n <- dbExecute(db.con,SQL.cmd),silent=TRUE)  
    if(!is(rtn, "try-error")) break
    Sys.sleep(runif(1,0.01,0.1))  #Avoid constant polling. Add some stochasticity to break synchronisation
  }

  #Fin
  dbDisconnect(db.con)
  log_msg("Deleted %i rows from %s table...\n",n,tbl.name)
  return(invisible(NULL))
}

#' @export
#' @rdname PE.db
PE.db.delete.by.datasource <- function(pcfg,tbl.name=PE.cfg$db$extract,datasrc) {
  #Connect
  db.con <- PE.db.connection(pcfg)
  this.tbl <- tbl(db.con, tbl.name)
  #Get row IDs where we want to delete
  row.ids <- 
    this.tbl %>%
    filter(srcName == !!datasrc@name,
           srcType == !!datasrc@type) %>%
    select(pKey) %>%
    collect()
  
  #Disconnect
  dbDisconnect(db.con)

  #Delete rows
  PE.db.delete.by.pKey(pcfg,tbl.name,row.ids$pKey)
}

#' @export
#' @rdname PE.db
PE.db.calc.realMeans <- function(pcfg,this.datasrc) {
  this.db <- PE.db.connection(pcfg)
  #Extract data and perform averaging
  frag.dat <- 
    tbl(this.db,PE.cfg$db$extract) %>%
    filter(srcName == !!this.datasrc@name,
           srcType == !!this.datasrc@type) %>%
    collect() %>%
    PE.db.unserialize()
  dbDisconnect(this.db)
  
  realMeans <- 
    frag.dat %>%
    group_by(srcName,srcType,startDate,date,leadIdx,.drop=TRUE) %>%
    summarise(data=raster.list.mean(data),
              duplicate.realizations=any(duplicated(realization))) %>% #Check for duplicated realization codes
    ungroup()
  if(any(realMeans$duplicate.realizations)) stop("Duplicate realizations detected in database. Rebuild.")

  #Write to database 
  realMeans %>%
    select(-duplicate.realizations) %>%
    add_column(realization="realmean",.after="srcType") %>%
    PE.db.appendTable(pcfg, PE.cfg$db$extract)
  log_msg("Wrote %i realisation means...\n",nrow(realMeans))
  
  return(invisible(NULL))
}

#' @details PE.db.appendTable serialises the data column and writes the data to the specified table
#' @export
#' @rdname PE.db
PE.db.appendTable <- function(dat,pcfg,tbl.name) {
  #Serialise data
  serial.dat <- 
    dat %>%
    mutate(across(where(is.list),function(cl) map(cl,serialize,NULL))) 
  #Write
  db.con <- PE.db.connection(pcfg)
  repeat{
    rtn <- try(dbWriteTable(conn=db.con, name=tbl.name, value=serial.dat, append = TRUE),silent=TRUE)  
    if(!is(rtn, "try-error")) break
    Sys.sleep(runif(1,0.01,0.1))  #Avoid constant polling. Add some stochasticity to break synchronisation
  }
  #Fin
  dbDisconnect(db.con)
  return(invisible(NULL))
}

#' @details PE.db.getQuery performs a concurrency-safe query
#' @export
#' @rdname PE.db
PE.db.getQuery <- function(pcfg,this.sql) {
  #Open connection
  db.con <- PE.db.connection(pcfg)
  #Get query
  repeat{
    rtn <- try(dbGetQuery(conn=db.con, this.sql),silent=TRUE)  
    if(!is(rtn, "try-error")) break
    Sys.sleep(runif(1,0.01,0.1))  #Avoid constant polling. Add some stochasticity to break synchronisation
  }
  #Fin
  dbDisconnect(db.con)
  return(rtn)
}


#' @details PE.db.unserialize  unserialises the data column
#' @export
#' @rdname PE.db
PE.db.unserialize <- function(this.dat) {
    mutate(this.dat,
           across(where(~is(.x,"blob")),function(cl) map(cl,unserialize)))
}


#' Raster List functions
#' 
#' Helper functions to work with lists of rasters
#'
#' @param l List of raster layers
#'
#' @return
#' @export
raster.list.mean <- function(l) {
  l.stack <- raster::brick(l)
  l.mean <- raster::mean(l.stack) #Dispatching can be a bit strange here sometimes
  return(list(l.mean))
}
