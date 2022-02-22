# noric 2.5.0

## Oppdatering kvalitetsindikatorer
* Trykkmålinger: 4 nye trykkmålinger skal være med i indikatoren 
(IMR, Pd, Pa, Pd/Pa)
* Foreskrevet kolesterolsenkende og blodfortynnende. Vi har nå 3 nivåer dersom 
forløpet er i datagrunnlaget : ja, nei og ikke ferdigstilt. Synliggjør de 
ikke-ferdigstilte
* Oppdatere månedlig utsending med disse endringene
* Oppdatere månedlig utsendign med bruk av noric funksjonene fra versjon 2.3.0
* Bugfix i IVUS-OCT tabellen i KI-rapporten  (riktig i ki_ivus_oct_ved_stenting_lms())

## Nytt
* Ny funksjon: reportProcessor() 
* bruke rapbase-layout med reportProcessor for utsending av KI-rapporten
* testmiljø for getData-funksjonene

## Bugfix
* Rette getMk og GetPs. Filter på riktig dato-variabel


# noric 2.4.2
* missing export of getPrepSoData() hopefully fixed
* removed spinner from local reports
* use stats now only for national registry with sc role 

# noric 2.4.1

## Bugfix monthly quality indicator-reports
* more than 10 TAVI needed last 3 years to create pacemaker-table
* use mst from noric library

## Improvements
* split get functions in two functions: One function does the SQL query, 
the other processes the tables. 
* both functions have arguments fromDate and toDate. Allows us to ask for 
data in restrained time-interval. 



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
