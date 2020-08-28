# Once more, Programmatically
# Let's try this again, but with a programmatic approach.

# The status table has 5 variables:

# study_id
# randomized
# excluded_reason
# arm
# completed

library(tidyverse)
library(glue)
library(janitor)

# build status table for 5-arm Upa UC
status5upa <- tibble(randomized = c(rep("Yes", 250),
                                rep("No", 196)),
                 excluded_reason = c(rep("None", 250),
                                     rep("Did not meet inclusion criteria", 172),
                                     rep("Withdrew consent", 17),
                                     rep("Lost to follow up", 2),
                                     rep("Other reasons", 5)),
                 arm = c(rep("Placebo", 46),
                         rep("Upa 7.5 mg QD", 47),
                         rep("Upa 15 mg QD", 49),
                         rep("Upa 30 mg QD", 52),
                         rep("Upa 45 mg QD", 56),
                         rep("None", 196)),
                 completed = c(rep("Adverse Event", 3),
                               rep("Loss of Effect", 2),
                               rep("Completed", 41),
                               rep("Adverse Event", 1),
                               rep("Loss of Effect", 1),
                               rep("Completed", 45),
                               rep("Adverse Event", 2),
                               rep("Loss of Effect", 1),
                               rep("Other", 1),
                               rep("Completed", 45),
                               rep("Adverse Event", 4),
                               rep("Loss of Effect", 1),
                               rep("Other", 1),
                               rep("Completed", 46),
                               rep("Adverse Event", 4),
                               rep("Loss of Effect", 2),
                               rep("Completed", 50),
                               rep("None", 196)))
# now shuffle rows
set.seed(42)
rows <- sample(nrow(status5upa))
status5upa <- status5upa[rows, ]
# now add study_id, formatted as "000X"
study_id <- str_pad(1L:446L, width = 4,
                    side = "left", pad = "0")
status5upa <- cbind(study_id, status5upa)
status5upa


# Take a look at the status table data for the 446 screeened patients.
# Then we will look at tabyls for each of the four categorical variables
status <- status5
head(status)
status5upa %>% tabyl(randomized)
status %>% tabyl(excluded_reason)
status %>% tabyl(arm)
status %>% tabyl(completed)

# Now let's see if we can rebuild the consort diagram programmatically. We will build up each piece.
# But we will not use any of the raw numbers.
# Instead, this time we will extract each one from the status table

data <- tibble(x= 1:100, y= 1:100)
data %>%
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
  theme_linedraw() ->
  p2
p2


#### Generate text for each box ----

# The first box is the number of patients assessed for eligibility.
# assessed_box
assessed_box <- glue(status5 %>% tally() %>% pull(), " Patients Assessed for Eligibility")

assessed_box


p2 + geom_rect(xmin = 36, xmax=64, ymin=94, ymax=100, color='black', fill='white', size=0.25, size=0.25) +
  annotate('text', x= 50, y=97,label= top_box, size=2.5) ->
  p2
# note that this actually looks fine when you zoom, not so much in the preview below.
p2

### Now let's try to build the patients excluded table
# exclusion_box

status %>%
  filter(is.na(randomized)) %>%
  tabyl(excluded_reason) %>%
  adorn_totals("row") %>%
  select(n, excluded_reason) %>%
  arrange(desc(n)) ->
  exclusion_table

exclusion_table[1,2] <- "Patients excluded"

mutate(exclusion_table,
       col_new = paste(n, excluded_reason)) %>%
  select(col_new) ->
exclusion_box

exclusion_box

### Now let's build the text for the assignment box
# assign_box
status %>%
  filter(randomized == "yes") %>%
  tally() %>%
  pull() ->
  num_assigned

assign_box <- glue(num_assigned, " Patients randomly assigned\nand included in the intention-to-treat analysis")

assign_box

### Now let's build the text for boxes for assignment to each arm

# note that ordering is tricky, often may want to customize
# this version arranges by ascending dose
# considering Placebo as dose = 0

# build a vector of arms, arranged in order
# might need this later
status %>%
  distinct(arm) %>%
  filter(arm != "None") %>%
  mutate(num = parse_number(arm)) %>%
  replace_na(list(num = 0)) %>%
  arrange(num) %>%
  pull(arm) ->
  arms_vec

# tally n for assignment boxes by arm, ordered ----
status %>%
  filter(arm != "None") %>%
  count(arm) %>%
  mutate(num = parse_number(arm)) %>% # extract dose
  replace_na(list(num = 0)) %>% # make placebo dose 0
  arrange(num) %>% #ordering by dose
  mutate(col_new = paste(n, "Patients assigned\nto ", arm)) %>%
  select(col_new)




### Now let's try to build the text for boxes for reasons discontinued by arm

arms_vec <- list_arms(status)

status %>%
  filter(randomized == "yes") %>%
  filter(is.na(completed)) %>%
  select(study_id, arm, completed, discont_reason) %>%
  arrange(arm, completed) ->
  discontinued_table
discontinued_table

# use build_discont_text function, run across arms_vec with purrr

purrr::map_dfr(.f = build_discont_text, .x = arms_vec)

output = NULL
for (i in arms_vec)
{
  discontinued_table %>%
    filter(arm == i) %>%
    tabyl(discont_reason) %>%
    adorn_totals("row") %>%
    arrange(desc(n)) ->
    discontinued_group
  discontinued_group[1,1] <- "Discontinued"

  mutate(discontinued_group,
         discont_label = paste(n, discont_reason))%>%
    select(discont_label) %>%
    glue_data("{discont_label}") -> out
  output = bind_rows(output, out)
}

output = NULL
for (i in arms_vec)
  {
  discontinued_table %>%
    filter(arm == i) %>%
    tabyl(discont_reason) %>%
    adorn_totals("row") %>%
    arrange(desc(n)) ->
    discontinued_group
  discontinued_group[1,1] <- "Discontinued"
  mutate(discontinued_group,
         discont_label = paste(n, discont_reason))%>%
    select(discont_label) %>%
    collapse_to_string() %>%
    print()
} %>%
  tibble() ->
  discont

### Now let's build the text for the completion boxes for each arm

# note that ordering is tricky, often may want to customize
# this version arranges by ascending dose
# considering Placebo as dose = 0

# build a vector of arms, arranged in order
# might need this later
status %>%
  distinct(arm) %>%
  filter(arm != "None") %>%
  mutate(num = parse_number(arm)) %>%
  replace_na(list(num = 0)) %>%
  arrange(num) %>%
  pull(arm) ->
  arms_vec

# tally n for completion boxes by arm, ordered
status %>%
  filter(completed == "Completed") %>%
  count(arm) %>%
  mutate(num = parse_number(arm)) %>% # extract dose
  replace_na(list(num = 0)) %>% # make placebo dose 0
  arrange(num) %>% #ordering by dose
  mutate(col_new = paste(n, "Patients completed\nthe study")) %>%
  select(col_new) %>%
  pull()




#### Starting Over - Organized ----

## now let us try to do the top 3rd programmatically
# with 3 chunks of text, 3 boxes, 3 lines.
# Build the top_tbl dataframe

# Conceptual Model
# set up tbl_top for 3 boxes, 4 lines, including crossbar
# crossbar length reflects n_arms
# build all text labels - into tbl_top
# measure max_char_width
# measure num_lines
# calculate xmin, xmax, ymin, ymax for each box
# calculate xstart, xend, ystart, yend for each line
#
# set up arms_tbl for bottom half
# with one row per arm
# cols for arm_num, arm_name
# cols for x, y at connection to crossbar
#
# cols for assignment arrows
# xstart_assign, xend_assign, ystart_assign, yend_assign
# cols for assignment boxes
# xmin_assign, xmax_assign, ymin_assign, ymax_assign
# col for assignment labels
#
# # cols for completion arrows
# xstart_complete, xend_complete, ystart_complete, yend_complete
# cols for completion boxes
# xmin_complete, xmax_complete, ymin_complete, ymax_complete
# col for completion labels
#
# cols for discontinuation arrows
# xstart_discont, xend_discont, ystart_discont, yend_discont
# cols for discontinuation boxes
# xmin_discont, xmax_discont, ymin_discont, ymax_discont
# col for discontinuation labels
#
# find overall ymin, ymax, in both tables - add for total height
# find overall xmin, xmax in both tables, compare for max width
#
# begin drawing
# generate data, draw grid based on total height and total width
# use top_tbl to draw first 3 boxes
# use top_tbl to apply first 3 labels
# use top_tbl to draw 4 line segments
#
# use arms_tbl to draw assignment boxes
# use arms_tbl to apply assignment labels
# use arms_tbl to draw assignment arrows
#
# use arms_tbl to draw completion boxes
# use arms_tbl to apply completion labels
# use arms_tbl to draw completion arrows
#
# use arms_tbl to draw discontinuation boxes
# use arms_tbl to apply discontinuation labels
# use arms_tbl to draw discontinuation arrows
#
# wrap into draw_top function
# wrap into draw_n_arms function
# wrap these 2 to into draw_consort function
#
#
box <- c("assessment_box", "exclusion_box", "randomization_box")
box_num <- c(1:3)
label <- rep("",3)
lines <- rep(0,3)
char_wide <- rep(0,3)
top_tbl <- tibble(box, box_num, label, lines, char_wide)

# build text for exclusion box label
status %>%
  filter(randomized == "No") %>%
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
exclusion_table2$char_wide <- max(nchar(exclusion_table2$col_new)) - 2
# will later put this width into top_tbl row 2

# glue into single string with line breaks
label_ex <- glue_collapse(exclusion_table2$col_new) %>% #collapses to single string
  str_trim() #removes the last \n
# put into top_tbl$label[2]

# create text labels for boxes 1-3
top_tbl$label[1] <- glue(status %>% tally() %>% pull(), " Patients Assessed for Eligibility")
top_tbl$label[2] <- label_ex # created above
top_tbl$label[3] <- glue(status %>% filter(randomized == "Yes") %>% tally() %>% pull(), " Patients randomly assigned\nand included in the intention-to-treat analysis")

# now calculate true # of lines for all 3
top_tbl$lines <- count_lines(top_tbl$label)

# now add width in characters to each
top_tbl$char_wide[1] <- str_split(top_tbl$label[1], "\n") %>% unlist() %>% str_length() %>% max()
top_tbl$char_wide[2] <- exclusion_table2$char_wide[1]
top_tbl$char_wide[3] <- str_split(top_tbl$label[3], "\n") %>% unlist() %>% str_length() %>% max()


### set up boxes, text, lines


# try with geom_rect
top = 100 #set top
center = 50 # set center
offset_r = 25 #set offset to Right for exclusion
h_const = 2.3 # set adjustment for box height by # of lines
w_const = 0.39 # set adjustment for box width by # of characters
v_space_const = 2 # set adjustment between vertical boxes
top_tbl %>%
  mutate(xmin = center - char_wide * w_const,
         xmax = center + char_wide * w_const,
         ymin = top - lines * h_const,
         ymax = top) ->
  top_tbl2

top_tbl2$xmin[2] <- (center + offset_r - top_tbl2$char_wide[2] * w_const)
top_tbl2$xmax[2] <- (center + offset_r  + top_tbl2$char_wide[2] * w_const)
top_tbl2$ymin[2] <- (top_tbl2$ymin[1] - v_space_const - top_tbl2$lines[2]*h_const)
top_tbl2$ymax[2] <- (top_tbl2$ymin[1] - v_space_const)

top_tbl2$ymin[3] <- (top_tbl2$ymin[2] - v_space_const - top_tbl2$lines[3]*h_const)
top_tbl2$ymax[3] <- (top_tbl2$ymin[2] - v_space_const)

ggplot(top_tbl2) +
  geom_rect(aes(xmin=xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "white", color = "black") +
  annotate('text', x= center, y= top_tbl2$ymax[1]-
             (top_tbl2$ymax[1]- top_tbl2$ymin[1])/2,
           label= top_tbl2$label[1], size=3) +
  # note for exclusions annotate - divide offset_r by 2 to allow
  # for left alignment
  # create left alignment with hjust = 0
  # add 0.5 as left padding
  annotate('text', x= center + offset_r/2 + 0.5, y= top_tbl2$ymax[2]-
             (top_tbl2$ymax[2]- top_tbl2$ymin[2])/2,
           label= top_tbl2$label[2], size=3, hjust = 0) +
  annotate('text', x= center, y= top_tbl2$ymax[3]-
             (top_tbl2$ymax[3]- top_tbl2$ymin[3])/2,
           label= top_tbl2$label[3], size=3) +
  xlim(0,100) + ylim(0,100) ->
  q

# generate lines - start by set all to zero
top_tbl2$xstart <- top_tbl2$xend <-
  top_tbl2$ystart <- top_tbl2$yend <- 0

top_tbl2$xstart[1] = center
top_tbl2$ystart[1] = top_tbl2$ymin[1]
top_tbl2$xend[1] = center
top_tbl2$yend[1] = top_tbl2$ymax[3]
top_tbl2$xstart[2] = center
top_tbl2$ystart[2] = (top_tbl2$ymin[1] + top_tbl2$ymax[3])/2
top_tbl2$xend[2] = top_tbl2$xmin[2]
top_tbl2$yend[2] = (top_tbl2$ymin[1] + top_tbl2$ymax[3])/2
top_tbl2$xstart[3] = center
top_tbl2$ystart[3] = top_tbl2$ymin[3]
top_tbl2$xend[3] = center
top_tbl2$yend[3] = top_tbl2$ymin[3]- 2*v_space_const

# problem - line 3 DOES NOT have an arrowhead.
# need to build separately
#
# now add lines/arrowheads
q +   geom_segment(data = top_tbl2, aes(x=xstart, y=ystart,
                                        xend=xend, yend=yend),
                   lineend = "round", linejoin = "round",
                   arrow = arrow(length = unit(0.2, "cm"),
                                 ends = "last", type = "closed"), arrow.fill = "black") ->
  r
r + theme_void() # show it


## Now add line 4 across


n_arms = 5
r +
  geom_segment(x = center - n_arms*8, xend = center + n_arms*8,
               y = top_tbl2$ymin[3]- 2*v_space_const,
               yend = top_tbl2$ymin[3]- 2*v_space_const) ->
  s

s + theme_void() # show it


## TODO List


n_arms = 5
center = 50
arm <- paste0(rep("arm_", 5), 1:5)
x <- c(center - 8*n_arms, center - 4*n_arms, center, # varies with n_arms
       center + 4*n_arms, center + 8*n_arms)
ystart <- rep(top_tbl2$ymin[3]- 2*v_space_const, 5)
arms_tbl <- tibble(arm, x, ystart)

arms_tbl <- arms_tbl %>%
  mutate(xstart =x,
         xend = x,
         yend_short = ystart - 2*v_space_const)


s +
  geom_segment(data = arms_tbl, aes(x = xstart, xend = xend,
                                    y = ystart, yend = yend_short),
               lineend = "round", linejoin = "round",
               arrow = arrow(length = unit(0.2, "cm"),
                             ends = "last", type = "closed"), arrow.fill = "black") ->
  t
t + theme_void() # show it


### build function to convert table to collapsed text with line breaks, removing the last one
Check
### build function to count character width - single or multiline
Check
### build function to count lines
Check

### fix line 3 problem (has arrow, should not) in line 636??
## add 2 arm trial - rectal indomethacin
## https://www.nejm.org/doi/full/10.1056/NEJMoa1111103
##
## add 3 arm trial - SONIC
## https://www.nejm.org/doi/full/10.1056/nejmoa0904492
##
## add 4 arm tofa crohn in Gut
## https://gut.bmj.com/content/66/6/1049
##

# change multiplier for arms - relate to total width
# instead of 8, use width/ 12.5?
### Draw N vertical arrows, spaced at center - 8* Narms, 4*narms, center +4, +8*narms. Build as tibble with narms rows (1:N)
### for 4 arms, x = center -10*narms, center-5*narms, center +5, center +10
### for 3 arms, x = center - 8* narmrs, center, center + 8* narms
### for 2 arms, x = center - 8*narms, center + 8* narms
### for 1 arm, x = center, without line 4

### build assignment text boxes by arm, add labels to arms_tbl
### count lines_assign, count char_width_assign for each - make columns
### draw boxes - create xmin_assign, xmax_assign, ymin_assign, ymax_assign columns
### annotate text centered
### place on plot

### Build boxes for discontinued by arm, add text labels to arms_tbl
### count lines_disc, count char_width_disc for each - make columns
### set up boxes with vspacing, r offset
###  - create xmin_disc, xmax_disc, ymin_disc, ymax_disc columns
### draw N boxes
### annotate text x N , left justified
### Place on plot

### Build boxes for completed by arm, add labels to tibble
### count lines_comp, count char_width_comp for each - make columns
### draw boxes with vspacing, r offset
###  - create xmin_comp, xmax_comp, ymin_comp, ymax_comp columns
### draw N boxes
### annotate text x N centered
### add downward arrows
### add xstart_down, xend_down, ystart_down, yend_down columns
### geom_segment with arrowheads
### add rightward arrows to discontinuation box
### add xstart_r, xend_r, ystart_r, yend_r columns
### geom_segment with arrowheads
### add theme_void

### Options - for single arm - leave out horizontal line
### assert checks - is n_arm = actual number of arms listed?
### optional argument - do you want to fct_lump discontinuation reasons if there are more than N? (discontinuation_limit = 10)
### optional argument - do you want to fct_lump exclusion reasons if more there are more than N? (exclusion_limit = 10)
### option to make wider - related to # of arms?

# Edge cases
## handle no exclusions
## handle no discontinuations
## Handle up to 9 reasons for discontinue
## option to fct_lump if >9 discontinue reasons - lump to other
## option to fct_lump if >9 exclusion reasons - lump to other
## option to chance fct_lump threshold in arguments
## auto str_wrap (insert \n) if exclusion > 30 char
## auto str_wrap (insert \n) if discontinue > 30 char
## auto str_wrap (insert \n) if assignment > 30 char
## auto str_wrap (insert \n) if rand box > 50 char
## auto str_wrap (insert \n) if assess box > 50 char
##
## Consider add hanging exdent to discontinuation reasons
## with str_wrap exdent argument
## cat(str_wrap(text, width = 30, exdent = 2), "\n")


# try with ggtext
# # seems possible if you count lines and chars
library(ggtext)

df <- tibble(
  label = c(
    "Some text **in bold.**",
    "Linebreaks<br>Linebreaks<br>Linebreaks",
    "one line<br>two lines<br>three lines",
    "Some <span style='color:blue'>blue text **in bold.**</span><br>And *italics text.*<br>
    And some <span style='font-size:18pt; color:black'>large</span> text."
  ),
  x = c(.2, .1, .5, .9),
  y = c(.8, .4, .1, .5),
  hjust = c(0.5, 0.5, 0.5, 0.5),
  vjust = c(0.5, 0.5, 0.5, 0.5),
  angle = c(0, 0, 0, 0),
  xstart = c(.2, .1, .5, .9),
  xend = c(.9, .2, .1, .5),
  ystart = c(.8, .4, .1, .5),
  yend = c(.5, .8, .4, .1),
  color = c("black", "blue", "black", "red"),
  fill = c("cornsilk", "white", "lightblue1", "white")
)


ggplot(df) +
  aes(x, y, label = label, angle = angle, color = color, fill = fill,
    hjust = hjust, vjust = vjust
  ) +
  geom_richtext() +
  geom_point(color = "black", size = 2) +
  scale_color_identity() +
  scale_fill_identity() +
  xlim(0, 1) + ylim(0, 1) +
  geom_segment(aes(x = xstart, xend = xend,
               y = ystart, yend = yend),
               lineend = "round", linejoin = "round",
               arrow = arrow(length = unit(0.2, "cm"),
ends = "last", type = "closed"), arrow.fill = "black")

### build arms_tbls ----

## build arms_boxes_tbl
# start with assignment boxes, group 4
# set which status table (will later be an argument)
status <- status5

## count arms (could be a function)
n_arms <- status %>%
  filter(!is.na(arm)) %>%
  distinct(arm) %>%
  count() %>% pull()
n_arms #display result

##
box_num <- 1:n_arms
group = 4
name <- paste0("assign_box_", {group}, {box_num})
loc_num <- dplyr::case_when(
  n_arms %% 2 == 0 & box_num > n_arms/2 ~ box_num-n_arms/2,
  n_arms %% 2 == 0 & box_num <= n_arms/2 ~ box_num-n_arms/2-1,
  n_arms %% 2 != 0 ~ box_num - ceiling(n_arms/2))
# note have a function for the above
box_num <- box_num + group*10
label <- status %>%
  filter(!is.na(arm)) %>%
  count(arm) %>%
  mutate(label = paste(n, "Patients assigned\nto ", arm)) %>%
  select(label)
assign_boxes <-  tibble(box_num, group, name, loc_num, label)



## add discontinuation boxes, group 5
box_num <- 1:n_arms
group = 5
name <- paste0("discont_box_", {group}, {box_num})
loc_num <- dplyr::case_when(
  n_arms %% 2 == 0 & box_num > n_arms/2 ~ box_num-n_arms/2,
  n_arms %% 2 == 0 & box_num <= n_arms/2 ~ box_num-n_arms/2-1,
  n_arms %% 2 != 0 ~ box_num - ceiling(n_arms/2))
box_num <- box_num + group*10
label <- status %>%
  filter(randomized == "Yes") %>%
  filter(completed != "Completed") %>%
  select(study_id, arm, completed) %>%
  arrange(arm, completed)

# build discont labels
arms_vec <- list_arms(status)


status %>%
  filter(randomized == "yes") %>%
  filter(is.na(completed)) %>%
  filter(arm == arms_vec[i]) %>%
  tabyl(discont_reason) %>%
  adorn_totals("row") %>%
  arrange(desc(n)) ->
  discont_table
discont_table

discont_table[1,1] <- "Discontinued"

discont_table2 <- discont_table %>%
  mutate(col_new = paste0(n, " ",discont_reason, "\n")) %>%
  select(col_new)

# measure max_length of string in col_new
discont_char_wide <- max(nchar(discont_table2$col_new)) - 2
# later will put this width into top_boxes_tbl row 2

# glue into single string with line breaks
label_discont <- glue_collapse(discont_table2$col_new) %>%
  tibble()
label_discont



# with loop
discont_labels = list()

for(i in seq_along(arms_vec)){
  status %>%
    filter(randomized == "yes") %>%
    filter(is.na(completed)) %>%
    filter(arm == arms_vec[i]) %>%
    tabyl(discont_reason) %>%
    adorn_totals("row") %>%
    arrange(desc(n)) ->
    discont_table
  discont_table

  discont_table[1,1] <- "Discontinued"

  discont_table2 <- discont_table %>%
    mutate(col_new = paste0(n, " ",discont_reason, "\n")) %>%
    select(col_new)

  # measure max_length of string in col_new
  discont_char_wide <- max(nchar(discont_table2$col_new)) - 2
  # later will put this width into top_boxes_tbl row 2

  # glue into single string with line breaks
  discont_labels[i] <- glue_collapse(discont_table2$col_new)
}
label_discont <- bind_rows(discont_labels)

#collapses to single string
  #str_trim() #removes the last \n
# later will put into top_boxes_tbl$label[2]

# build tibble
discont_boxes <-  tibble(box_num, group, name, loc_num)

## add completed boxes, group 6
box_num <- 1:n_arms
group = 6
name <- paste0("complete_box_", {group}, {box_num})
loc_num <- dplyr::case_when(
  n_arms %% 2 == 0 & box_num > n_arms/2 ~ box_num-n_arms/2,
  n_arms %% 2 == 0 & box_num <= n_arms/2 ~ box_num-n_arms/2-1,
  n_arms %% 2 != 0 ~ box_num - ceiling(n_arms/2))
box_num <- box_num + group*10
label <- status %>%
  filter(completed == "Completed") %>%
  count(arm) %>%
  mutate(label = paste(n, "Patients completed\nthe study")) %>%
  select(label) %>%
  pull()


complete_boxes <-  tibble(box_num, group, name, loc_num, label)

# put together all arms_boxes
arms_boxes_tbl <- bind_rows(assign_boxes,
                            discont_boxes,
                            complete_boxes)


## build arms_lines_tbl
# start with short vertical assignment lines, group 5
n_arms <- status %>%
  filter(!is.na(arm)) %>%
  distinct(arm) %>%
  count() %>% pull()

line_num <- 1:n_arms
group = 5
name <- paste0("assign_ln_", {group}, {line_num})
loc_num <- dplyr::case_when(
  n_arms %% 2 == 0 & line_num > n_arms/2 ~ line_num-n_arms/2,
  n_arms %% 2 == 0 & line_num <= n_arms/2 ~ line_num-n_arms/2-1,
  n_arms %% 2 != 0 ~ line_num - ceiling(n_arms/2))
ln_num <- line_num + group*10

assign_lines <-  tibble(ln_num, group, name, loc_num)

# add long vertical completed lines, group 6
line_num <- 1:n_arms
group = 6
name <- paste0("complete_ln_", {group}, {line_num})
loc_num <- dplyr::case_when(
  n_arms %% 2 == 0 & line_num > n_arms/2 ~ line_num-n_arms/2,
  n_arms %% 2 == 0 & line_num <= n_arms/2 ~ line_num-n_arms/2-1,
  n_arms %% 2 != 0 ~ line_num - ceiling(n_arms/2))
ln_num <- line_num + group*10

complete_lines <-  tibble(ln_num, group, name, loc_num)

# add short horizontal discontinued lines, group 7
line_num <- 1:n_arms
group = 7
name <- paste0("discont_ln_", {group}, {line_num})
loc_num <- dplyr::case_when(
  n_arms %% 2 == 0 & line_num > n_arms/2 ~ line_num-n_arms/2,
  n_arms %% 2 == 0 & line_num <= n_arms/2 ~ line_num-n_arms/2-1,
  n_arms %% 2 != 0 ~ line_num - ceiling(n_arms/2))
ln_num <- line_num + group*10

discont_lines <-  tibble(ln_num, group, name, loc_num)

# put together all arms_lines
arms_lines_tbl <- bind_rows(assign_lines,
                            complete_lines,
                            discont_lines)
