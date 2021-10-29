# noric 2.4.0

## New features
* use statistics report
* database export for development and data processing purposes

## Improvements
* selectable start date for subscriptions
* replaced local functions with general modules from rapbase reducing the number of code lines

# noric 2.3.0

* new table in utforsker : "Angio PCI med utledete variabler, 3 siste aar"
* creation of functions used to ease datamanagment (e.g. fjern_tulleregistrering, fikse_sykehusnavn)
* creation of functions used to add new variables in noric datasets (e.g. year, month, week, waiting times, age-class)
* creation of functions used to add new variables for quality indicators and which data to analyse for each indicator (e.g. indik_trykkmaaling_utfort + indik_trykkmaaling_utfoert_data)
* creation of functions used to add information from "SegmentStent"-table or "annen diagnostikk"-table into "angioPCI"-table (e.g. antall_stent, stent_i_lms, antall_ffr_lms, kar)
* use of testthat/tests for all new functions
* general cleanup in code
* Added a `NEWS.md` file to track changes to the package.
