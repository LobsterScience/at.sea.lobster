at.sea.lobster package is used for entry of At-Sea-Sampling (lobster) data. The main working function for inputting data is input.trip() which provides an R-Shiny based GUI to enter data by trip.
Entering a trip creates a local .db (SQL database) file named by the vessel registration number and the boarding date. The file is stored in a location of the user's choosing.
.db files are are SQL-based and work like an Oracle table stored directly on the user's hard drive. SQL language or RSQLite can be used for downstream processing of the data tables.
After data entry, the .db file will contain four relational tables: TRIP\_INFO, SET\_INFO, TRAP\_INFO, FISH\_INFO
Data tables can be checked in R with the check.table() function (ex. check.table("fish"))



To install the package:

devtools::install\_github("LobsterScience/at.sea.lobster")



Load package with library(at.sea.lobster)

