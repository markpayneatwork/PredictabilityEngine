#' Setup results databases
#'
#' @param object PredEng configuration object 
#' @param table Name of the table to connect to. 
#' @param src Data source object which is to be stored in the table. Each table for each src is stored in a separate file. 
#' 
#' @return
#' @export
#' 
#' @name PE.db
PE.db.path <- function(object,table,src) {
  assert_that(is(object,"PredEng.config"),
              msg="object argument must be a PredEng.config object")
  #For the moment, only split the extraction phase into individual files
  if(table %in% c(PE.cfg$db$extract)) { #Then split by source
    assert_that(is(src,"data.source"),
                msg="Accessing extraction tables requires a data.source object as src")
    here(object@scratch.dir,sprintf("%s_%s_%s_%s.sqlite",object@project.name,table,src@type,src@name))
  } else { #Don't split by source
    here(object@scratch.dir,sprintf("%s_%s.sqlite",object@project.name,table))
  }
}

#' @export
#' @rdname PE.db
PE.db.list <- function(object,table) {
  dir(object@scratch.dir,pattern=sprintf("^%s_%s",object@project.name,table),full.names = TRUE)
}


#' @export
#' @rdname PE.db
PE.db.connection <- function(object,table,src) {
  this.db.path <- PE.db.path(object,table,src)
  this.db <- dbConnect(RSQLite::SQLite(), this.db.path,synchronous=NULL)
  return(this.db)
}

#' @export
#' @rdname PE.db
PE.db.tbl <- function(object,table,src) {
  this.db <- PE.db.connection(object,table,src)
  this.tbl <- tbl(this.db,table)
  return(this.tbl)
}

#' @export
#' @rdname PE.db
setMethod("dbDisconnect","tbl_SQLiteConnection", function(conn,...) {
  dbDisconnect(conn$src$con)
})


# Create database file ------------------------------------------------------------
# Internal function to create a database
PE.db.create <- function(this.path,these.cmds) {
  #Create the file by opening it
  this.db <- dbConnect(RSQLite::SQLite(), this.path,synchronous=NULL)
  #Run commands
  for(this.cmd in these.cmds) {
    dbExecute(this.db, this.cmd)      
  }
  #Disconnect
  dbDisconnect(this.db)
}


#' @export
#' @rdname PE.db
PE.db.setup.extraction <- function(object,src) {
  this.fname <- PE.db.path(object,PE.cfg$db$extract,src)
  if(!file.exists(this.fname)) {
    #Setup Extraction table and indices
    tbl.cols <-  
      c("pKey INTEGER NOT NULL PRIMARY KEY",
        "srcFname TEXT",
        "srcType TEXT",
        "srcName TEXT",
        "realization TEXT",
        "startDate TEXT",
        "date TEXT",
        "lead INTEGER",
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
    PE.db.create(this.fname,cmd.l)
    
  }
}

#' @export
#' @rdname PE.db
PE.db.setup.climatology <- function(object,src) {
  this.fname <- PE.db.path(object,PE.cfg$db$climatology,src)
  if(!file.exists(this.fname)) {
    #Setup climatology table
    tbl.cols <-  
      c("pKey INTEGER NOT NULL PRIMARY KEY",
        "srcType TEXT",
        "srcName TEXT",
        "lead INTEGER",
        "month INTEGER",
        "nYears INTEGER",
        "statistic TEXT",
        "field BLOB")  
    cmd.l <- list(sprintf("CREATE TABLE %s(%s)", 
                          PE.cfg$db$climatology,
                          paste(tbl.cols, collapse = ", ")))
    PE.db.create(this.fname,cmd.l)
  }
}

#' @export
#' @rdname PE.db
PE.db.setup.calibration <- function(object,src) {
  #Setup calibration table and indices
  this.fname <- PE.db.path(object,PE.cfg$db$calibration,src)
  if(!file.exists(this.fname)) {
    tbl.cols <-  
      c("pKey INTEGER NOT NULL PRIMARY KEY",
        "srcType TEXT",
        "srcName TEXT",
        "calibrationMethod TEXT",
        "realization TEXT",
        "startDate TEXT",
        "date TEXT",
        "lead INTEGER",
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
    PE.db.create(this.fname,cmd.l)
}}

#' @export
#' @rdname PE.db
PE.db.setup.statistics <- function(object,src) {
  #Setup statistics table
  this.fname <- PE.db.path(object,PE.cfg$db$stats,src)
  if(!file.exists(this.fname)) {
    tbl.cols <-  
      c("pKey INTEGER NOT NULL PRIMARY KEY",
        "srcType TEXT",
        "srcName TEXT",
        "calibrationMethod TEXT",
        "realization TEXT",
        "startDate TEXT",
        "date TEXT",
        "lead INTEGER",
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
      sprintf("CREATE INDEX idx_%s_real ON %s(srcType,srcName,calibrationMethod,realization)",
              PE.cfg$db$stats,
              PE.cfg$db$stats)
    cmd.l$idx2 <- 
      sprintf("CREATE INDEX idx_%s_stat ON %s(statName,spName)",
              PE.cfg$db$stats,
              PE.cfg$db$stats)
    PE.db.create(this.fname,cmd.l)
  }
}

#' @export
#' @rdname PE.db
PE.db.delete.by.pKey <- function(object,table,src,pKeys,silent=FALSE) {
  #Delete rows
  SQL.cmd <- sprintf("DELETE FROM %s WHERE pKey IN (%s)",
                     table,
                     paste(pKeys,collapse=" , "))
  
  #Poll until can get access to DB
  db.con <- PE.db.connection(object,table,src)
  n <- dbExecute(db.con,SQL.cmd)
  dbDisconnect(db.con)
  if(!silent) { log_msg("Deleted %i rows from %s table...\n",n,table)}
  return(invisible(n))
}


#' @export
#' @rdname PE.db
PE.db.delete.by.datasource <- function(object,table,src,silent=FALSE) {
  #Setup query
  SQL.cmd <- sprintf("SELECT pKey FROM %s WHERE `srcType` = '%s' AND `srcName` = '%s'",
                     table,
                     src@type,
                     src@name)
  #Fetch list to delete
  del.these <- PE.db.getQuery(object,table,src,SQL.cmd)
  
  #Delete
  PE.db.delete.by.pKey(object,table,src,del.these$pKey,silent=silent)
  return(invisible(n))
}


#' @details PE.db.appendTable serialises the data column and writes the data to the specified table
#' @export
#' @rdname PE.db
PE.db.appendTable <- function(object,table,src,dat,serialize.first=TRUE) {
  #Serialise data
  if(serialize.first) {
    dat <- 
      dat %>%
      mutate(across(where(is.list),function(cl) map_if(cl,
                                                       function(x) !is.null(x),
                                                       qserialize))) 
  }
  #Write
  db.con <- PE.db.connection(object,table,src)
  dbWriteTable(conn=db.con, name=table, value=dat, append = TRUE)
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
  mutate(across(where(~is(.x,"blob")),function(cl) map_if(cl,
                                                          function(x) !is.null(x),
                                                          qdeserialize))) 
}

#' @details PE.db.getQuery performs a concurrency-safe query
#' @export
#' @rdname PE.db
PE.db.getQuery <- function(object,table,src,this.sql) {
  #Open connection
  db.con <- PE.db.connection(object,table,src)
  #Get query
  rtn <- dbGetQuery(conn=db.con, this.sql)
  #Fin
  dbDisconnect(db.con)
  return(rtn)
}

