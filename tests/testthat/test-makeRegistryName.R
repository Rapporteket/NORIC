conf <- rapbase::getConfig("rapbaseConfig.yml")
conf$reg <- list(noric = list(nationalAccess = list(reshId = 100000)))
conf$reg$noric$nationalAccess$nameKey <- "Nat"
yaml::write_yaml(conf, file = "./rapbaseConfig.yml")

orig_path <- Sys.getenv("R_RAP_CONFIG_PATH")
Sys.setenv(R_RAP_CONFIG_PATH = getwd())

test_that("a registry name can be provided", {
  expect_equal(makeRegistryName("noric", 100000), "noricNat")
  expect_equal(makeRegistryName("noric", 100001), "noric100001")
})

# clean-up and recreate environment
file.remove("./rapbaseConfig.yml")
Sys.setenv(R_RAP_CONFIG_PATH = orig_path)
