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
                paste(tbl.cols, collapse = ", "))
    dbExecute(this.db, tbl.cmd)
  }
  #Setup climatology table
  if(!PE.cfg$db$climatology %in% dbListTables(this.db)) {
    tbl.cols <-  
      c("climId INTEGER NOT NULL PRIMARY KEY",
        "srcName",
        "srcType",
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
      c("calFragId INTEGER NOT NULL PRIMARY KEY",
        "srcName",
        "srcType",
        "calibrationMethod",
        "realization",
        "startDate",
        "date",
        "leadIdx",
        "data")  #Number of years in the climatology
    tbl.cmd <- 
      sprintf("CREATE TABLE %s(%s)", 
              PE.cfg$db$calibration,
              paste(tbl.cols, collapse = ", "))
    dbExecute(this.db, tbl.cmd)
  }
  
  dbDisconnect(this.db)
}

#' @export
#' @rdname PE.db
PE.db.delete.rows <- function(this.db,this.tbl,primaryKey,IDs) {
  #Delete rows
  SQL.cmd <- sprintf("DELETE FROM %s WHERE %s IN (%s)",
                     this.tbl,
                     primaryKey,
                     paste(IDs,collapse=" , "))
  n <- dbExecute(this.db,SQL.cmd)
  log_msg("Deleted %i rows from %s table...\n",n,this.tbl)
  
}

#' @export
#' @rdname PE.db
PE.db.delete.extractions <- function(this.db,this.datasource) {
  this.tbl <- tbl(this.db, PE.cfg$db$extract)
  #Get row IDs where we want to delete
  row.ids <- 
    this.tbl %>%
    filter(srcName == !!this.datasrc@name,
           srcType == !!this.datasrc@type) %>%
    select(fragId) %>%
    collect()

  #Delete rows
  PE.db.delete.rows(this.db,PE.cfg$db$extract,"fragId",row.ids$fragId)
}

#' @export
#' @rdname PE.db
PE.db.calc.realMeans <- function(this.db,this.datasrc) {
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
    summarise(data=raster.list.mean(data),
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
