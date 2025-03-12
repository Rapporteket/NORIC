## code to prepare `def_utledete_var` dataset goes here

def_utledete_var <- readxl::read_excel(
  path = "data-raw/definisjoner_utledete_variabler_noric_20231128.xlsx")

usethis::use_data(def_utledete_var, overwrite = TRUE)



kb <-readxl::read_excel("data-raw/klokebokNoric20230511_tilRapporteket.xlsx")

usethis::use_data(kb, overwrite = TRUE)
