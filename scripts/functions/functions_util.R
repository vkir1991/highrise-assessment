# refresh all SQL connections (and define them)
refreshSQL <- function(){
  # SQL
  closeAllCons()
  
  sql.con <<- dbConnect(RSQLite::SQLite(), "data/data.db")
  
}
