Bycatch package is used for entry of At-Sea-Sampling (lobster) data. The main working function for inputting data is input.trip() which provides an R-Shiny based GUI to enter data by trip.
Entered data exists in a file called input\_data.db which as stored in a location of the user's choosing.
.db files are are SQL-based and work like an Oracle table stored directly on the user's hard drive. SQL language or RSQLite can be used for downstream processing of the data tables.
After data entry, .db will contain four relational tables: TRIP\_INFO, SET\_INFO, TRAP\_INFO, FISH\_INFO
Data tables can be checked in R with the check.table() function (ex. check.table("fish"))

To install the package:

devtools::install\_github("LobsterScience/Bycatch")



Load package with library(Bycatch)

