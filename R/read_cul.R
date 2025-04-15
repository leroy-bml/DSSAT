#' Reads parameters from a single DSSAT cultivar parameter file (*.CUL)
#'
#' @export
#'
#' @inheritParams read_dssat
#'
#' @inheritParams read_filex
#'
#' @return a tibble containing the data from the raw DSSAT output
#'
#' @importFrom dplyr "%>%"
#' @importFrom readr cols col_character
#' @importFrom stringr str_detect str_subset str_which
#' @importFrom purrr map reduce
#'
#' @examples
#'
#' # Extract file path for sample cultivar file path
#' sample_cul_file <- system.file('extdata','SAMPLE.CUL',package='DSSAT')
#'
#' # Read sample cultivar file
#' cul <- read_cul(sample_cul_file)
#'
#'

read_cul <- function(file_name, col_types=NULL, col_names=NULL,
                     left_justified=c('VAR#', 'VARNAME\\.*', 'VAR-NAME\\.*','VRNAME\\.*'),
                     use_std_fmt = TRUE){

  cul_col_types <- cols(`VAR#`=col_character(),
                        `VARNAME\\.*`=col_character(),
                        `VAR-NAME\\.*`=col_character(),
                        `VRNAME\\.*`=col_character(),
                        `  ECO#`=col_character(),
                        ` EXPNO`=col_character(),
                        `  EXP#`=col_character())

  if(str_detect(file_name,'SCCSP')){
    col_names <- col_names %>%
      c(.,'Stalk','Sucro','Null1',
          'TB(1)','TO1(1)','TO2(1)',
          'TB(2)','TO1(2)','TO2(2)',
          ' *TM(1)',' *TM(2)',
          'StHrv','RTNFAC','Null7',
          'RES30C','RLF30C') %>%
      unique()
  }

  if(!is.null(col_types)){
    col_types$cols <- c(cul_col_types$cols, col_types$cols)
  }else{
    col_types <- cul_col_types
  }

  # Read in raw data from file
  raw_lines <- readLines(file_name, warn = FALSE)

  first_line <- raw_lines %>%
    head(1)

  comments <- extract_comments(raw_lines)

  switches_index <- c(grep("^Coeff", comments), grep("^Calibr", comments))
  switches <- comments[switches_index]
  comments <- comments[-switches_index]

  begin <- raw_lines %>%
    str_which('^@')

  end <- begin %>%
    tail(-1) %>%
    {. - 1} %>%
    c(.,length(raw_lines))

  if(use_std_fmt){
    tier_fmt <- cul_v_fmt(file_name)
  }else{
    tier_fmt <- NULL
  }

  cul <- map(1:length(begin),
             ~read_tier_data(raw_lines[begin[.]:end[.]],
                        col_types = cul_col_types,
                        col_names = col_names,
                        left_justified = left_justified,
                        tier_fmt = tier_fmt,
                        convert_date_cols = FALSE,
                        # Workaround for current issue leading to incorrect v_fmt
                        # being linked to the cul table. V_fmt input used only
                        # to set column width and data type. Recalculated with
                        # 'construct_variable_format' regardless of whether it
                        # was provided as an arg or not. In the case of CUL file this
                        # can produce deviation from the actual cul_v_fmt template
                        # For now wporkaround rather than direct correction in
                        # read_tier_data, uncertain what impact this may have in other instances
                        store_v_fmt = FALSE)) %>%
    reduce(combine_tiers)

  attr(cul,'v_fmt') <- tier_fmt
  attr(cul,'first_line') <- first_line
  attr(cul,'switches') <- switches
  attr(cul,'comments') <- comments

  cul <- as_DSSAT_tbl(cul)

  return(cul)
}
