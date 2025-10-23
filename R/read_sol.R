#' Reads soil parameters from a single DSSAT soil parameter file (*.SOL)
#'
#' @export
#'
#' @inheritParams read_dssat
#'
#' @param id_soil a length-one character vector containing the soil ID code for a
#' single soil profile
#' @param nested a logical to set whether layer data should be nested in the output
#' (one row per soil profile)
#'
#' @return a tibble containing the data from the raw DSSAT file
#'
#' @importFrom dplyr "%>%" first
#' @importFrom stringr str_subset str_replace str_extract str_which str_c
#' @importFrom purrr map reduce
#'
#' @examples
#'
#' # Extract file path for sample soil file
#' sample_sol <- system.file('extdata','SAMPLE.SOL',package='DSSAT')
#'
#' sol <- read_sol(sample_sol)
#'
read_sol <- function(file_name, id_soil = NULL, nested = TRUE){

  # Read in raw data from file
  # exclude lines that are all spaces or lines with EOF in initial position
  raw_lines <- grep("^(?!\032) *([^ ]+)",
                    readLines(file_name, warn = FALSE),
                    perl = TRUE,
                    value = TRUE)

  # Specify left-justified columns
  left_justified <- c('SITE','COUNTRY',' SCS FAMILY', ' SCS Family')

  # Specify column types
  col_types <- cols(`      LAT`=col_double(),
                    `     LONG`=col_double(),
                    SSAT=col_double(),
                    ` SCS FAMILY`=col_character(),
                    ` SCS Family`=col_character(),
                    SCOM=col_character(),
                    COUNTRY=col_character(),
                    SITE=col_character(),
                    SMHB=col_character(),
                    SMPX=col_character(),
                    SMKE=col_character(),
                    SLMH=col_character(),
                    SLB=col_double()) %>%
    {.$cols <- c(.$cols,col_types$cols);.}

  # Store title and comments
  basename <- basename(file_name)
  title <- gsub("\\*SOILS", "", raw_lines[1])
  title <- trimws(gsub(":", "", title))

  # Find start/end positions for each soil profile (PEDON)
  pedon_raw_start_end <- find_pedon(raw_lines)
  comments_lines <- find_comments(raw_lines)
  comments <- link_soil_comments(comments_lines, pedon_raw_start_end)

  # Drop comments and empty lines
  clean_lines <- drop_empty_lines( drop_comments(raw_lines) )
  pedon_clean_start_end <- find_pedon(clean_lines)

  # Filter profiles based on id_soil
  if(!is.null(id_soil)){
    pedon_clean_start_end <- pedon_clean_start_end[pedon_clean_start_end$PEDON %in% id_soil, ]
  }

  # Extract general information for each PEDON
  gen_info <- read_sol_gen_info(clean_lines[pedon_clean_start_end$start])

  # Strip out and concatenate the lines for each PEDON
  pedon_lines <- concat_lines(clean_lines, pedon_clean_start_end)

  # Find start/end positions for each soil data tier within each PEDON
  tier_start_end <- find_tier(pedon_lines)

  # Strip out and concatenate the lines for each soil data tier
  tier_lines <- concat_lines(pedon_lines$lines, tier_start_end)

  # Remove empty lines
  tier_lines <- with(tier_lines,
                     tier_lines[lines != "", ])

  # Read all lines by the same header
  tiers_out <- lapply(unique(tier_lines$header),
                      function(h){
                        raw_lines <- c(h,
                                       with(tier_lines, lines[header == h]))

                        pedon <- with(tier_lines, PEDON[header == h])

                        tier_data <- read_tier_data(raw_lines,
                                                    left_justified = left_justified,
                                                    col_types = col_types,
                                                    tier_fmt = sol_v_fmt(),
                                                    convert_date_cols = FALSE)

                        tier_data$PEDON <- pedon

                        colnames(tier_data) <- toupper(colnames(tier_data))

                        return(tier_data)
                      })

  layer_ind <- sapply(tiers_out, is_sol_layer)

  # Create layer-specific data frame with rows nested by PEDON
  layer_data <- nest_rows(
    # Recursively merge layer-specific data
    recursive_merge(
      # Subset for list elements with layer-specific data
      tiers_out[layer_ind],
      by = c("PEDON", "SLB")),
    by = "PEDON")

  # Create whole profile data frame with one row per PEDON
  profile_data <- recursive_merge(
    c(list(gen_info),
      tiers_out[!layer_ind]),
    by = c("PEDON"))

  # Merge whole-profile and layer-specific data
  tiers_out <- coalesce_merge(profile_data, layer_data)

  # Return layers as nested list if nest set to TRUE
  if (!nested) {
    list_cols <- names(tiers_out)[sapply(tiers_out, is.list)]
    tiers_out <- as.data.frame(
      unnest(tiers_out, cols = all_of(list_cols))
    )
  }

  # Attach metadata
  attr(tiers_out, "file_name") <- basename
  attr(tiers_out, "title") <- title
  attr(tiers_out, "comments") <- comments

  return(tiers_out)
}


#' Helper function to link comments to pedon
#'
#'
link_soil_comments <- function(comments_lines, pedon_start_end) {

  if (nrow(comments_lines) == 0 || nrow(pedon_start_end) == 0) {
    return(list())
  }

  first_pedon_start <- min(pedon_start_end$start)
  all_pedon_names <- pedon_start_end$PEDON

  # Separate general and post comments
  is_general <- comments_lines$line_number < first_pedon_start
  gen_comments <- comments_lines[is_general, ]
  post_comments <- comments_lines[!is_general, ]

  # Assign post comments to closest pedon
  if (nrow(post_comments) > 0) {
    pedon_starts <- pedon_start_end$start
    pedon_names <- pedon_start_end$PEDON

    pedon_ind <- sapply(post_comments$line_number, function(comment_line) {
      distances <- abs(comment_line - pedon_starts)
      which.min(distances)
    })

    post_comments$PEDON <- pedon_names[pedon_ind]
  } else {
    post_comments$PEDON <- character(0)
  }

  # Build output list
  names(all_pedon_names) <- all_pedon_names

  out <- lapply(all_pedon_names, function(current_pedon) {

    specific_comments_df <- post_comments[post_comments$PEDON == current_pedon, ]

    combined_df <- rbind(
      gen_comments[, c("line_number", "comment_text")],
      specific_comments_df[, c("line_number", "comment_text")]
    )
    combined_df_sorted <- combined_df[order(combined_df$line_number), ]

    return(combined_df_sorted$comment_text)
  })

  return(out)
}
