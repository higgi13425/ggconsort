library(DiagrammeR)
library(glue)
library(janitor)

#' Make the consort diagram with DiagrammeR package
#'
#' @param df Data frame containing status table
#' @return Consort diagram
#' @example
#' make_consort(df)
make_consort <- function(df) {
  grViz(
    glue(
      "digraph consort_diagram {{
      graph[splines = ortho]
      node [fontname = Helvetica, height = 1, width = 4, shape = box]

        assessed [label = <{nrow(df)} Individuals assessed for eligibility >]
        blank[label = '', width = 0.01, height = 0.01]
        assessed -> blank[ dir = none ];

        randomized [label = <{sum(na.omit(df$randomized) == 'yes')} Randomized >]
        excluded[label = <{create_text_for_box(df$excluded_reason, 'Excluded', breakdown1 = T)}>]

        blank -> excluded[ minlen = 2 ];
        blank -> randomized;
        {{ rank = same; blank excluded }} {create_arms(df)}
      }}
    ")
  )
}

# Create diagram arms with boxes
create_arms <- function(df) {
  # Only include if randomized to arms
  arm_data <- df[!is.na(df$arm), ]

  # List of arm strings
  arm_strings <- list()

  # For each arm create box text
  for (arm in unique(arm_data$arm)) {
    arm_name <- make_clean_names(arm)
    completed_arm_name <- glue("completed_{arm_name}")
    analyzed_arm_name <- glue("analyzed_{arm_name}")
    arm_strings[[arm]] <- glue("{arm_name} [label = <
                                {create_text_for_box(arm_data$received_int[arm_data$arm == arm & arm_data$received_int == 'yes'],
                                                     glue('Randomized to receive {arm}'), breakdown1 = F,
                                                     arm_data$received_int[arm_data$arm == arm & arm_data$received_int == 'No'],
                                                     glue('Did not receive {arm}'), breakdown2 = F)}
                                >, group = {arm_name}] randomized -> {arm_name};
                                {completed_arm_name} [label = <
                                {create_text_for_box(arm_data$completed[arm_data$arm == arm],
                                                     'Completed trial', breakdown1 = F,
                                                     arm_data$discont_reason[arm_data$arm == arm],
                                                     'Did not complete trial', breakdown2 = T)}
                                >, group = {arm_name}] {arm_name} -> {completed_arm_name};
                                {analyzed_arm_name} [label = <
                                {create_text_for_box(arm_data$analyzed[arm_data$arm == arm],
                                                     'Included in the primary analysis', breakdown1 = F,
                                                     arm_data$not_analyzed[arm_data$arm == arm],
                                                     'Excluded from analysis', breakdown2 = T)}
                                >, group = {arm_name}] {completed_arm_name} -> {analyzed_arm_name};"
    )
  }
  return(paste(unlist(arm_strings), collapse = ''))
}

# Create table text for an individual box
create_text_for_box <- function(vec1, box_label1, breakdown1, vec2 = NA, box_label2 = NA, breakdown2 = F) {
  items1 <- na.omit(vec1)
  items2 <- na.omit(vec2)
  t1 <- table(items1)
  t2 <- table(items2)
  s <- paste0('<table border="0" cellborder="0" cellspacing="0">', create_box_header(items1, box_label1), if(breakdown1) create_box_rows(t1))
  if (length(items2) > 0) {
    s <- paste0(s, create_box_header(items2, box_label2), if(breakdown2) create_box_rows(t2))
  }
  s <- paste0(s, '</table>')
  return(s)
}

# Create table header text for a box
create_box_header <- function(items, label) {
  glue('<tr><td align = "right">{length(items)}</td><td align = "left" colspan = "2">{label}</td></tr>')
}

# Create table row text for a box
create_box_rows <- function(t) {

  # Empty data frame to store rows of details
  df <- data.frame(nm = as.character(), col1 = as.character(), col2 = as.character())

  # For each detail, add additional rows, with strwrap to wrap text
  for (nm in names(t)) {
    df <- rbind(df, data.frame(nm = nm, col1 = as.character(t[nm]), col2 = strwrap(nm, 40), row.names = NULL))
  }

  # The first row will have the frequency value in case of wrapped lines (other lines will be blank)
  df[unlist(with(df, by(order(nm), nm, function(x) tail(x, length(x) - 1)))), "col1"] <- ""

  # Set up the table rows in the HTML table
  s <- unlist(apply(df, 1, function(x) { glue('<tr><td></td><td align = "right">{x[["col1"]]}</td><td align = "left">{x[["col2"]]}</td></tr>') }))

  return(paste(s, collapse = ""))
}

