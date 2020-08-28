
library(tidyverse)
library(glue)
library(janitor)
source('functions.R')

# build status table examples:
source('create_status_tables.R')


# set plotting constants -----
v_space_const = 6 # set adjustment between vertical boxes
h_space_const = 6 # set adjustment between horizontal boxes
h_const = 6 # set adjustment for box height by # of lines
w_const = 3.5 # set adjustment for box width by # of characters

# set status_table
status_table <- status2

# detect number of arms
n_arms <- status_table %>%
          filter(!is.na(arm)) %>%
          distinct(arm) %>%
          nrow()

# set up top_table ----
box <- c("assessment_box", "exclusion_box", "randomization_box")
box_num <- c(10,20,30)
label <- rep("",3)
lines <- rep(0,3)
char_wide <- rep(0,3)
top_tbl <- tibble(box, box_num, label, lines, char_wide)


# set up for exclusion box row ----
# build text for exclusion box label
status_table %>%
  filter(is.na(randomized)) %>%
  tabyl(excluded_reason) %>%
  adorn_totals("row") %>%
  select(n, excluded_reason) %>%
  arrange(desc(n)) ->
  exclusion_table

exclusion_table[1,2] <- "Patients excluded"

exclusion_table2 <- mutate(exclusion_table,
                           col_new = paste0(n, " ",excluded_reason, "\n")) %>%
  select(col_new)

# measure max_length of string in col_new
ex_char_wide <- max(nchar(exclusion_table2$col_new)) - 2
# later will put this width into top_boxes_tbl row 2

# glue into single string with line breaks
label_ex <- glue_collapse(exclusion_table2$col_new) %>% #collapses to single string
  str_trim() #removes the last \n
# later will put into top_boxes_tbl$label[2]

# fill in text labels for boxes 10-30 ---
# assessment label
top_tbl$label[1] <- glue(status_table %>% tally() %>% pull(), " Patients Assessed for Eligibility")
# excluded label
top_tbl$label[2] <- label_ex # created above
# randomized label
top_tbl$label[3] <- glue(status_table %>% filter(randomized == "Yes") %>% tally() %>% pull(), " Patients randomly assigned\nand included in the intention-to-treat analysis")

# calculate true # of lines for all 3 ----
top_tbl$lines <- count_lines(top_tbl$label)

# add width in characters to each
top_tbl$char_wide[1] <- str_split(top_tbl$label[1], "\n") %>% unlist() %>% str_length() %>% max()
top_tbl$char_wide[2] <- ex_char_wide
top_tbl$char_wide[3] <- str_split(top_tbl$label[3], "\n") %>% unlist() %>% str_length() %>% max()

# add box x,y (roughly)
top_tbl %>%
  mutate(xmin = -char_wide * w_const,
         xmax = char_wide * w_const,
         ymin = 0,
         ymax = 0) ->
  top_tbl2

# offset exclusion box to the right
# by width of box 2 plus
# half width of box 3 plus 2 h_spaces
r_offset =  top_tbl2$char_wide[2]*w_const +
  top_tbl2$char_wide[3]*w_const/2 +
  h_space_const*2

# right adjust xmin and xmax for exclusion box by offset
top_tbl2$xmin[2] <- top_tbl2$xmin[2]+ r_offset
top_tbl2$xmax[2] <- top_tbl2$xmax[2]+ r_offset

# set ymins for each box
top_tbl2$ymin[1] <- 6*v_space_const +
  top_tbl$lines[3]* h_const +
  top_tbl$lines[2]* h_const

top_tbl2$ymin[2] <- 4*v_space_const +
  top_tbl$lines[3]* h_const

top_tbl2$ymin[3] <- 2*v_space_const

top_tbl2 %>%
  mutate(ymax = ymin + lines*h_const) %>%
  mutate(ycenter = ymin + (ymax-ymin)/2) %>%
  mutate(xcenter = xmin + (xmax-xmin)/2) %>%
  mutate(xstart = xcenter,
         xend = xcenter,
         ystart = ymin,
         yend = 0) ->
  top_tbl3

# line adjustments
top_tbl3$xstart[2] <- top_tbl3$xcenter[1]
top_tbl3$xend[2] <- top_tbl3$xmin[2]
top_tbl3$ystart[2] <- mean(c(top_tbl3$ymin[1],
                             top_tbl3$ymax[3]))
top_tbl3$ystart[3] <- top_tbl3$ymin[3]
top_tbl3$yend[1] <- top_tbl3$ymax[3]
top_tbl3$yend[2] <- mean(c(top_tbl3$ymin[1],
                           top_tbl3$ymax[3]))

# TEST draw top boxes with geom_rect ----

ggplot(top_tbl3) +
  geom_rect(aes(xmin=xmin, xmax = xmax,
                ymin = ymin, ymax = ymax),
            fill = "white", color = "black") +
  geom_text(aes(x=xcenter, y = ycenter, label = label),
            size =3, hjust = "center") +
  geom_segment(aes(x=xstart, y=ystart,
                   xend=xend, yend=yend),
               lineend = "round", linejoin = "round",
               arrow = arrow(length = unit(0.2, "cm"),
                             ends = "last", type = "closed"),
               arrow.fill = "black") +
  geom_segment(aes( x = - n_arms * h_space_const*16,
                    xend = n_arms * h_space_const*16,
                    y = 0, yend = 0)) +
  theme_void()
  #coord_cartesian(xlim = c(-300, 300), ylim = c(-200, 200))
