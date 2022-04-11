## code to prepare `def_utledete_var` dataset goes here

def_utledete_var <-read_excel("data-raw/definisjoner_utledete_variabler_noric.xlsx")

usethis::use_data(def_utledete_var, overwrite = TRUE)