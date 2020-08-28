library(tidyverse)

## notes on hex config on hexmake
## name x pos: 1, name y pos: 1.4,
## sans, size 8
## image x:1  y: 0.67
## width 0.82, height 0.82
## hex fill color: 3rd purple from top
## hex border color: 3rd purple from bottom


data <- tibble(x= 1:100, y= 1:100)

data %>%
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
  theme_linedraw() +
  # fill transparent, color NA at end
  theme(panel.background = element_rect(fill = "purple1",
              colour = "purple1"))->
  p
p

p +
  geom_rect(xmin = 32, xmax=68, ymin=94, ymax=100, color='white',
             size=0.5, fill = "transparent") +
  annotate('text', x= 50, y=97, color = "white",
    label= 'NNN Patients assessed for eligibility', size=2.5) ->
  p


p +
  geom_rect(xmin = 31, xmax=69, ymin=73, ymax=83, color='white',
            fill='transparent', size=0.5) +
  annotate('text', x= 50, y=78, color = "white",
           label= 'NNN Patients randomly assigned\n and included in the intention-to-treat analysis', size=2.3) +
  geom_rect(xmin = 70, xmax=100, ymin=80, ymax=97, color='white',
            fill='transparent', size=0.5) +
  annotate('text', x= 85, y=89, color = "white",
  label= 'NNN Patients excluded\n 172 Did not meet inclusion criteria\n 17 Withdrew consent\n 2 Lost to follow up', size=2.3) ->p
p

p +
  geom_segment(color = "white",
    x=50, xend=50, y=94, yend=83.3,
    size=0.5, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(color = "white",
    x=50, xend=69.7, y=89, yend=89,
    size=0.5, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) ->
  p
p

p +
  geom_segment(color = "white",
    #middle arrow first
    x=50, xend=50, y=73, yend=58.3,
    size=0.5, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  # then left arrow, x and xend=30
  geom_segment(color = "white",
    x=30, xend=30, y=65, yend=58.3,
    size=0.5, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  # then right arrow, x and xend=70
  geom_segment(color = "white",
    x=70, xend=70, y=65, yend=58.3,
    size=0.5, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  # then horizontal line, but remove the arrow
  geom_segment( color = "white",
    x=30, xend=70, y=65, yend=65,
    size=0.5, linejoin = "mitre", lineend = "butt")->
  p
p

p +
  #Left box
  geom_rect(xmin = 21, xmax=39, ymin=49, ymax=58,
            color='white', fill='transparent', size=0.5) +
  annotate('text', x= 30, y=54, size=2, color='white',
           label= 'NN Patients assigned\nto Placebo') +
  #Middle box
  geom_rect(xmin = 41, xmax=59, ymin=49, ymax=58,
            color='white', fill='transparent', size=0.5) +
  annotate('text', x= 50, y=54, size=2, color='white',
           label= 'NN Patients assigned\nto Low Dose') +
  #3rd box
  geom_rect(xmin = 61, xmax=79, ymin=49, ymax=58,
            color='white', fill='transparent', size=0.5) +
  annotate('text', x= 70, y=54, size=2, color='white',
           label= 'NN Patients assigned\nto High Dose') ->
  p
p

p +
 #Left arrow
  geom_segment(
    x=22, xend=22, y=49, yend=25.3, color = "white",
    size=0.5, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  #middle arrow
  geom_segment(color = "white",
    x=42, xend=42, y=49, yend=25.3,
    size=0.5, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  #right arrow
  geom_segment(color = "white",
    x=62, xend=62, y=49, yend=25.3,
    size=0.5, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) ->
  p
p

p +
 #Left box
  geom_rect(xmin = 25, xmax=39, ymin=30, ymax=46,
            color='white', fill='transparent', size=0.5) +
  annotate('text', x= 32, y=38, size=2, color='white',
           label= '1 Discontinued    \n1 Adverse event\n1 Lack of efficacy') +
  #Middle box
  geom_rect(xmin = 45, xmax=59, ymin=30, ymax=46,
            color='white', fill='transparent', size=0.5) +
  annotate('text', x= 52, y=38, size=2, color='white',
           label= '4 Discontinued    \n2 Adverse event\n1 Lack of efficacy\n1 Other') +
  #right box
  geom_rect(xmin = 65, xmax=79, ymin=30, ymax=46,
            color='white', fill='transparent', size=0.5) +
  annotate('text', x= 72, y=38, size=2, color='white',
           label= '6 Discontinued    \n4 Adverse event\n1 Lack of efficacy\n1 Other') ->p
p

p +
  #left arrow
  geom_segment(color = "white",
    x=22, xend=24.7, y=38, yend=38,
    size=0.5, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  #middle arrow
  geom_segment(color = "white",
    x=42, xend=44.7, y=38, yend=38,
    size=0.5, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  #right arrow
  geom_segment(color = "white",
    x=62, xend=64.7, y=38, yend=38,
    size=0.5, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) -> p
p

p +
  #left box
  geom_rect(xmin = 21, xmax=39, ymin=17, ymax=25,
            color='white', fill='transparent', size=0.5) +
  annotate('text', x= 30, y=21, size=2, color='white',
           label= 'NN Patients completed\nthe study') +
  #middle box
  geom_rect(xmin = 41, xmax=59, ymin=17, ymax=25,
            color='white', fill='transparent', fill='transparent', size=0.5) +
  annotate('text', x= 50, y=21, size=2, color='white',
           label= 'NN Patients completed\nthe study') +
  #right box
  geom_rect(xmin = 61, xmax=79, ymin=17, ymax=25,
            color='white', fill='transparent', size=0.5) +
  annotate('text', x= 70, y=21, size=2, color='white',
           label= 'NN Patients completed\nthe study') +
  theme_void()->
  p
p

ggsave(filename = "hexmake_logo.png", bg = "transparent")
