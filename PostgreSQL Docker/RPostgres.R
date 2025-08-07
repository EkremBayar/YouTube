# Kütüphane
# install.packages("RPostgres")
library(RPostgres)

# Veri tabanı bağlantısı
con <- dbConnect(RPostgres::Postgres(),
                 host = "localhost",
                 port = 5435,
                 dbname = "my_database",
                 user = "my_user",
                 password = "123456")

con

# DB'deki tabloların listesi
dbListTables(con)

# DB'de test tablosu var mı?
dbExistsTable(con, "test")

# Veri tabanına veri seti yazdırılması
head(mtcars)
dbWriteTable(con, "test", mtcars, overwrite = TRUE, row.names = FALSE)

dbListTables(con)
dbExistsTable(con, "test")

# Veri tabanından tablonun okunması
mt <- dbReadTable(con, "test")
mt
dim(mt)

# Veri tabanına yeni gözlem eklenmesi
dbWriteTable(con, "test", head(mtcars, 1), append = TRUE, row.names = FALSE)

mt2 <- dbReadTable(con, "test")
dim(mt); dim(mt2)

# Tablonun veri tabanından silinmesi
dbRemoveTable(con, "test")
dbListTables(con)

# Veri tabanına sorgu atmak
dbWriteTable(con, "test", mtcars, overwrite = TRUE, row.names = FALSE)

query <- "SELECT * FROM test;"
dbGetQuery(con, query)


query <- 'SELECT * FROM test WHERE "carb" = 2;'
dbGetQuery(con, query)


# Veri tabanı bağlantısı kapatma
dbDisconnect(con)
