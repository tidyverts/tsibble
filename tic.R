do_package_checks()

if (Sys.getenv("DEV_VERSIONS") != "") {
  get_stage("install") %>%
    add_step(step_install_github(c("r-lib/rlang", "tidyverse/dplyr")))
}

if (Sys.getenv("BUILD_PKGDOWN") != "" && ci()$get_branch() == "master") {
  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("deploy") %>%
    add_step(step_setup_push_deploy(path = "docs", branch = "gh-pages")) %>%
    add_step(step_build_pkgdown(run_dont_run = TRUE)) %>%
    add_step(step_do_push_deploy(path = "docs"))
}
