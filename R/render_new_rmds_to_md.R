#' Prep Hugo pages by converting Rmds to Md's
#' 
#' Assumes anything without a corresponding markdown file
#' needs to be renderd to md. Hugo (the static site generator)
#' then converts these md files to html on the site. Use the
#' preview option in Rstudio to quickly see how your md has worked.
#'
#' @param dir Directory to search for new Rmds in. Defaults to the blog post location of Hugo.
#'
#' @return Used for pure side effect
#' @export
render_new_rmds_to_md <- function(dir = "content/post") {
  content = dir
  
  # get info about all files
  info <- fs::dir_info(content)
  rmds <- info[stringr::str_detect(info$path, "\\.[Rr]md"),]
  mds <- info[stringr::str_detect(info$path, "\\.md"),]
  
  rmds$slug <- fs::path_ext_remove(rmds$path)
  mds$slug <- fs::path_ext_remove(mds$path)
  
  # Rmd without md
  unbuilt <- rmds$path[!rmds$slug %in% mds$slug]
  
  # Rmd with a too old md
  dplyr::left_join(rmds, mds, by = "slug",
                   suffix = c("_rmd", "_md")) %>%
    dplyr::filter(change_time_md < change_time_rmd) %>%
    dplyr::pull(path_rmd) -> too_old
    
  
  to_build <- c(too_old, unbuilt)
  
  # build only the ones to be built
  if(length(to_build) > 0){

    for (b in to_build) {
      rmd <- b
      rmarkdown::render(rmd,
                        rmarkdown::md_document(variant = "markdown_github",
                                               preserve_yaml = TRUE))
    }
  }else{
    message("Nothing to build, all .md up-to-date")
  }
  
  
}