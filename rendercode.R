library(rmarkdown)

render("poke_api_vignette.rmd",output_file= "README.md", output_format = "github_document")

render("plan.rmd", output_file = "plan.md", output_format = "github_document")
