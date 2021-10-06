library(rmarkdown)

render("poke_api_vignette.rmd",output_file= "README.md", output_format = "github_document", 
       output_options = list(toc = TRUE, toc_depth = 2))

