# Cron & Task Scheduler ---------------------------------------------------

# cronR ve Task Scheduler Rscriptlerini programlanan zamana göre otomatik çalıştırır.
# Programlanan görevleri 'BİR KEZ', 'HER DAKİKA', 'HER SAAT', 'HER GÜN', 'HER HAFTA', 'HER AY' veya 
# herhangi bir karmaşık programa göre planlayabilirsiniz.

# Linux + Mac
# https://github.com/bnosac/cronR

# Cron Deamon
# sudo apt-get update
# sudo apt-get install -y cron
# sudo cron start


# install.packages("cronR")
# remotes::install_github("bnosac/cronR")
# install.packages('miniUI')
# install.packages('shiny')
# install.packages('shinyFiles')

# Task Scheduler 
# https://github.com/bnosac/taskscheduleR
# remotes::install_github("bnosac/taskscheduleR")
# library(taskscheduleR)


# Kütüphane ---------------------------------------------------------------
library(cronR)


# Kütüphane Fonksiyonları
# cronR::

# Görev Listesi
cronR::cron_ls()

# RStudio Eklentisi - Addin
cron_rstudioaddin()

# print("Spor Analitiği") scripti hazırla


# cronR kod
root_path <- getwd()
full_path <- paste0(root_path, "/Cronjob/print.R")

cmd <- cron_rscript(full_path)
cron_id <- "job_print"
cron_tag <- "sports analytics"
cron_description <- "Cronjob test uygulaması"

cron_add(
  cmd,
  id = cron_id,
  tags = cron_tag,
  description = cron_description,
  frequency = "minutely",
  ask = F
)

cron_ls()
