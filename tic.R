do_package_checks()

if (Sys.getenv("DEV_VERSIONS") != "") {
  get_stage("install") %>%
    add_step(step_install_github(c("r-lib/rlang", "tidyverse/dplyr")))
}

if (Sys.getenv("BUILD_PKGDOWN") != "" && ci()$get_branch() == "master") {
  # pkgdown documentation can be built optionally. Other example criteria:
  # - `inherits(ci(), "TravisCI")`: Only for Travis CI
  # - `ci()$is_tag()`: Only for tags, not for branches
  # - `Sys.getenv("BUILD_PKGDOWN") != ""`: If the env var "BUILD_PKGDOWN" is set
  # - `Sys.getenv("TRAVIS_EVENT_TYPE") == "cron"`: Only for Travis cron jobs
  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("deploy") %>%
    add_code_step(
      pkgbuild::compile_dll(),
      prepare_call = remotes::install_github("r-lib/pkgbuild")
    ) %>%
    add_step(step_setup_push_deploy(path = "docs", branch = "gh-pages")) %>%
    add_step(step_build_pkgdown(run_dont_run = TRUE)) %>%
    add_step(step_do_push_deploy(path = "docs"))
}
