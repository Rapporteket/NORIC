conf <- rapbase::getConfig()
conf$reg <- list(noric=list(nationalAccess=list(reshId=100000)))
yaml::write_yaml(conf, file = "./rapbaseConfig.yml")

orig_path <- Sys.getenv("R_RAP_CONFIG_PATH")
Sys.setenv(R_RAP_CONFIG_PATH = getwd())

test_that("national status can be decided", {
  expect_true(isNationalReg(100000))
  expect_false(isNationalReg(100001))
})

# clean-up and recreate environment
file.remove("./rapbaseConfig.yml")
Sys.setenv(R_RAP_CONFIG_PATH = orig_path)
