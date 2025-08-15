# install.packages("RPostgres")
library(RPostgres)

con <- dbConnect(
  Postgres(),
  host = "localhost",
  port = 5435,
  dbname = "sports_analytics",
  user = "youtube",
  password = 123456
)

con

dbListTables(con)


dbExistsTable(con, "test")


mtcars

dbWriteTable(con, "test", mtcars)

dbListTables(con)

dbExistsTable(con, "test")

dim(mtcars)

dbWriteTable(con, "test", tail(mtcars, 1), append = TRUE)


query <- 'SELECT * FROM test'
dbGetQuery(con, query)


dbRemoveTable(con, "test")

dbListTables(con)


dbExistsTable(con, "test")
