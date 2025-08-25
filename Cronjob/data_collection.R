# Veri Tabanı -------------------------------------------------------------
suppressWarnings(suppressMessages(library(RPostgres)))  

con <- dbConnect(
  Postgres(),
  host = "localhost",
  port = 5435,
  dbname = "sports_analytics",
  user = "youtube",
  password = 123456
)


# Örnek Data --------------------------------------------------------------
sample_df <- data.frame(
  Timestamp = Sys.time(),
  Category = sample(LETTERS[1:3], size = 1),
  Quantity = sample(1:10, 1)
)

# Veri Tabanına Yazma -----------------------------------------------------
if(dbExistsTable(con, "sample_table") == FALSE){
  dbWriteTable(con, "sample_table", sample_df)
}else{
  dbWriteTable(con, "sample_table", sample_df, append = TRUE)
}
print("Veri yazdırıldı!")

dbDisconnect(con)

