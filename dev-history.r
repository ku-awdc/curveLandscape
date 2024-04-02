usethis::create_package(".")
usethis::use_author("Mossa", "Reimert",
    email = "mossa@sund.ku.dk"
)
usethis::use_package_doc()
usethis::use_mit_license("Mossa Merhi Reimert")
lintr::use_lintr()
usethis::use_pipe(export = FALSE)
usethis::use_directory("notebooks", ignore = TRUE)
usethis::edit_r_profile("project")
