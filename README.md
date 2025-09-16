at.sea.lobster package is used for entry of At-Sea-Sampling (lobster) data. The main working function for inputting data is input.trip() which provides an R-Shiny based GUI to enter data by trip.
Entering a trip creates a local .db (SQL database) file named by the vessel registration number and the boarding date. The file is stored in a location of the user's choosing.
.db files are SQL-based and work like an Oracle table stored directly on the user's hard drive. SQL language or RSQLite can be used for downstream processing of the data tables.
After data entry, the .db file will contain four relational tables: TRIP\_INFO, SET\_INFO, TRAP\_INFO, FISH\_INFO
Data tables can be checked in R with the check.table() function (ex. check.table("fish")). The function defaults to the last trip worked with in input.trip() but the user can manually choose a different trip file by changing the choose.trip argument to TRUE (example: check.table("fish", choose.trip = T)  ).





Exporting Data as CSV:



The data is meant to be maintained in SQL format (the .db files) for integration with the central database. However, if the user needs csv files of the data tables they can easily create them with the export.tables() function. Like check.table(), this function defaults to the last trip file edited in input.trip() but if the user wants to manually choose a trip file, they can use the choose.trip argument (example: export.tables("fish", choose.trip = T) ). To export multiple tables set tables = c('fish','trap'...) or to export all 4 tables for the chosen trip, set tables = "all"  





To install the package:

devtools::install\_github("LobsterScience/at.sea.lobster")



Load package with library(at.sea.lobster)

