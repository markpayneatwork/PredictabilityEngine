#' Setup results databases
#'
#' @param object PredEng configuration object 
#' @param table Name of the table to connect to. Each table is stored in a separate file
#'
#' @return
#' @export
#' 
#' @name PE.db
PE.db.connection <- function(object,table) {
  this.db.path <- PE.db.path(object,table)
  this.db <- dbConnect(RSQLite::SQLite(), this.db.path,synchronous=NULL)
  return(this.db)
}


#' @export
#' @rdname PE.db
PE.db.tbl <- function(object,table) {
  this.db <- PE.db.connection(object,table)
  this.tbl <- tbl(this.db,table)
  return(this.tbl)
}

#' @export
#' @rdname PE.db
setMethod("dbDisconnect","tbl_SQLiteConnection", function(conn,...) {
  dbDisconnect(conn$src$con)
})


#' @export
#' @rdname PE.db
PE.db.path <- function(object,table) {
  here(object@scratch.dir,sprintf("%s_%s.sqlite",object@project.name,table))
}

#' @export
#' @rdname PE.db
PE.db.setup <- function(object) {
  # Create database file ------------------------------------------------------------
  create.db <- function(this.path,these.cmds) {
    #Create the file by opening it
    this.db <- dbConnect(RSQLite::SQLite(), this.path,synchronous=NULL)
    #Run commands
    for(this.cmd in these.cmds) {
      dbExecute(this.db, this.cmd)      
    }
    #Disconnect
    dbDisconnect(this.db)
  }
  
  
  # Data extraction tables -----------------------------------------------------------
  #Setup Extraction table and indices
  this.fname <- PE.db.path(object,PE.cfg$db$extract)
  if(!file.exists(this.fname)) {
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
    cmd.l <- list()
    cmd.l$tbl <- 
      sprintf("CREATE TABLE %s(%s)", 
              PE.cfg$db$extract,
              paste(tbl.cols, collapse = ", "))
    cmd.l$idx <- 
      sprintf("CREATE INDEX idx_%s ON %s(srcType,srcName)",
              PE.cfg$db$extract,
              PE.cfg$db$extract)
    create.db(this.fname,cmd.l)
  }


  #Setup climatology table
  this.fname <- PE.db.path(object,PE.cfg$db$climatology)
  if(!file.exists(this.fname)) {
    tbl.cols <-  
      c("pKey INTEGER NOT NULL PRIMARY KEY",
        "srcType TEXT",
        "srcName TEXT",
        "leadIdx INTEGER",
        "month INTEGER",
        "nYears INTEGER",
        "statistic TEXT",
        "field BLOB")  
    cmd.l <- list(sprintf("CREATE TABLE %s(%s)", 
                          PE.cfg$db$climatology,
                          paste(tbl.cols, collapse = ", ")))
    create.db(this.fname,cmd.l)
  }

  #Setup calibration table and indices
  this.fname <- PE.db.path(object,PE.cfg$db$calibration)
  if(!file.exists(this.fname)) {
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
    cmd.l <- list()
    cmd.l$tbl <- 
      sprintf("CREATE TABLE %s(%s)", 
              PE.cfg$db$calibration,
              paste(tbl.cols, collapse = ", "))
    cmd.l$idx.cmd <- 
      sprintf("CREATE INDEX idx_%s ON %s(srcType,srcName)",
              PE.cfg$db$calibration,
              PE.cfg$db$calibration)
    cmd.l$idx2.cmd <- 
      sprintf("CREATE INDEX idx_%s_calMethod ON %s(calibrationMethod,realization,srcType)",
              PE.cfg$db$calibration,
              PE.cfg$db$calibration)
    cmd.l$idx3.cmd <- 
      sprintf("CREATE INDEX idx_%s_realization ON %s(realization)",
              PE.cfg$db$calibration,
              PE.cfg$db$calibration)
    create.db(this.fname,cmd.l)
    
  }

  # Results tables ------------------------------------------------------------------
  #Setup statistics table
  this.fname <- PE.db.path(object,PE.cfg$db$stats)
  if(!file.exists(this.fname)) {
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
        "value REAL",
        "field BLOB")  
    cmd.l <- list()
    cmd.l$tbl.cmd <- 
      sprintf("CREATE TABLE %s(%s)", 
              PE.cfg$db$stats,
              paste(tbl.cols, collapse = ", "))
    cmd.l$idx.cmd <- 
      sprintf("CREATE INDEX idx_%s ON %s(srcType,srcName)",
              PE.cfg$db$stats,
              PE.cfg$db$stats)
    cmd.l$idx2 <- 
      sprintf("CREATE INDEX idx_%s_stat ON %s(statName,spName)",
              PE.cfg$db$stats,
              PE.cfg$db$stats)
    create.db(this.fname,cmd.l)
  }
  
  #Enable support for parallel read/write, based on this SO reply:
  #https://stackoverflow.com/questions/36831302/parallel-query-of-sqlite-database-in-r
  # RSQLite::dbClearResult(RSQLite::dbSendQuery(this.db, "PRAGMA journal_mode=WAL;"));
  # dbDisconnect(this.db)
}

PE.db.safe.try <- function(expr,silent=TRUE,n.max=2) {
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
PE.db.delete.by.pKey <- function(object,table,pKeys,silent=TRUE) {
  #Delete rows
  SQL.cmd <- sprintf("DELETE FROM %s WHERE pKey IN (%s)",
                     table,
                     paste(pKeys,collapse=" , "))
  
  #Poll until can get access to DB
  db.con <- PE.db.connection(object,table)
  PE.db.safe.try(n <- dbExecute(db.con,SQL.cmd),silent=silent)  
  dbDisconnect(db.con)
  if(!silent) { log_msg("Deleted %i rows from %s table...\n",n,table)}
  return(invisible(n))
}


#' @export
#' @rdname PE.db
PE.db.delete.by.datasource <- function(object,table=PE.cfg$db$extract,datasrc,silent=TRUE) {
  #Setup query
  SQL.cmd <- sprintf("SELECT pKey FROM %s WHERE `srcType` = '%s' AND `srcName` = '%s'",
                     table,
                     datasrc@type,
                     datasrc@name)
  #Fetch list to delete
  del.these <- PE.db.getQuery(object,table,SQL.cmd,silent=silent)
  
  #Delete
  n <- PE.db.delete.by.pKey(object,table,del.these$pKey,silent=silent)
  return(invisible(n))
}


#' @details PE.db.appendTable serialises the data column and writes the data to the specified table
#' @export
#' @rdname PE.db
PE.db.appendTable <- function(object,table,dat,silent=TRUE,serialize.first=TRUE) {
  #Serialise data
  if(serialize.first) {
    dat <- 
      dat %>%
      mutate(across(where(is.list),function(cl) map(cl,qserialize))) 
  }
  #Write
  db.con <- PE.db.connection(object,table)
  n <- PE.db.safe.try(dbWriteTable(conn=db.con, name=table, value=dat, append = TRUE),silent=silent)  
  #Fin
  dbDisconnect(db.con)
  return(invisible(n))
}


#' @details PE.db.unserialize  unserialises the data column
#' @export
#' @rdname PE.db
PE.db.unserialize <- function(dat) {
  dat %>%
    as_tibble() %>%
  mutate(across(where(~is(.x,"blob")),function(cl) map(cl,qdeserialize)))
}



#' @details PE.db.getQuery performs a concurrency-safe query
#' @export
#' @rdname PE.db
PE.db.getQuery <- function(object,table,this.sql,silent=TRUE) {
  #Open connection
  db.con <- PE.db.connection(object,table)
  #Get query
  PE.db.safe.try(rtn <- dbGetQuery(conn=db.con, this.sql),silent=silent)  
  #Fin
  dbDisconnect(db.con)
  return(rtn)
}

