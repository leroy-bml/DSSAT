fmt_comments <- function(x_in){
  comments <- attr(x_in, "comments")

  if(is.null(comments) & is.character(x_in)) comments <- x_in

  if(is.list(comments)){
    gen_comments <- reduce(comments, intersect)
    post_comments <- lapply(comments, function(x) setdiff(x, gen_comments))
    post_comments <- Filter(function(x) length(x) > 0, post_comments)  # drop empty comments


    if(!is.null(gen_comments)){
      gen_comments <- gsub(" +$", "", # remove trailing spaces
                           gsub("^[ ]{0,1}", "! ", # add initial !
                                gsub("^ *!", "", gen_comments))) # strip off any initial !
    }
    if(!is.null(post_comments) | length(post_comments) == 0){
      post_comments <- lapply(post_comments, function(lines) {
        gsub(" +$", "",
             gsub("^[ ]{0,1}", "! ",
                  gsub("^ *!", "", lines)))
      })
    }
    comments <- list(general = gen_comments, pedon = post_comments)
  } else {
    if(!is.null(comments)){
      comments <- gsub(" +$", "", # remove trailing spaces
                       gsub("^[ ]{0,1}", "! ", # add initial !
                            gsub("^ *!", "", comments))) # strip off any initial !
    }
  }

  return(comments)
}
