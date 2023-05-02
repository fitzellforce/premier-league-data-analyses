# Below are the variable names chosen to represent each player stat table

pl.stand <- read_excel("Player Stats per90.xlsx", skip = 1)[-c(1)]
pl.gk <- read_excel("Player Stats per90.xlsx", sheet="Pl GK", skip = 1)[-c(1)]
pl.gk_adv <- read_excel("Player Stats per90.xlsx", sheet="Pl GK Adv", skip = 1)[-c(1)]
pl.shoot <- read_excel("Player Stats per90.xlsx", sheet="Pl Shoot", skip = 1)[-c(1)]
pl.pass <- read_excel("Player Stats per90.xlsx", sheet="Pl Pass", skip = 1)[-c(1)] %>% rename('1/3'='44929')
pl.pass_type <- read_excel("Player Stats per90.xlsx", sheet="Pl Pass Type", skip = 1)[-c(1)]
pl.gscreate <- read_excel("Player Stats per90.xlsx", sheet="Pl G&S Create", skip = 1)[-c(1)]
pl.def <- read_excel("Player Stats per90.xlsx", sheet="Pl Def Act", skip = 1)[-c(1)]
pl.poss <- read_excel("Player Stats per90.xlsx", sheet="Pl Poss", skip = 1)[-c(1)]
pl.play_time <- read_excel("Player Stats per90.xlsx", sheet="Pl Play Time", skip = 1)[-c(1)]
pl.misc <- read_excel("Player Stats per90.xlsx", sheet="Pl Misc", skip = 1)[-c(1)]


# Below are the variable names chosen to represent each team stat table
sq.stand <- read_excel("Squad Stats.xlsx", sheet="Sq Standard", skip = 1)
sq.gk <- read_excel("Squad Stats.xlsx", sheet="Sq GK", skip = 1)
sq.gk_adv <- read_excel("Squad Stats.xlsx", sheet="Sq Adv GK", skip = 1)
sq.shoot <- read_excel("Squad Stats.xlsx", sheet="Sq Shoot", skip = 1)
sq.pass <- read_excel("Squad Stats.xlsx", sheet="Sq Pass", skip = 1) %>% rename('1/3'='44929')
sq.pass_type <- read_excel("Squad Stats.xlsx", sheet="Sq Pass Type", skip = 1)
sq.gscreate <- read_excel("Squad Stats.xlsx", sheet="Sq G&S Create", skip = 1)
sq.def <- read_excel("Squad Stats.xlsx", sheet="Sq Def Act", skip = 1)
sq.poss <- read_excel("Squad Stats.xlsx", sheet="Sq Poss", skip = 1)
sq.play_time <- read_excel("Squad Stats.xlsx", sheet="Sq Play Time", skip = 1)
sq.misc <- read_excel("Squad Stats.xlsx", sheet="Sq Misc", skip = 1)

#############################

## Long Passes Attempted per 90 vs. Percentage Successfully Completed: Analysis

# Data Cleaning: Separated name column into two name columns: first and last name.
# Data Cleaning: Sometimes players only go by one name. In cases where last name column was null, I duplicated the first name value to fill in the null last name.
# Filtering Data: I filtered out records where the player has played less than 10 full games (10 "90s") and selected only players considered midfielders.

pl.pass.clean <- pl.pass %>% separate(Player, into=c('first','last'),sep=' ', extra="merge")

pl.pass.clean$last[is.na(pl.pass.clean$last)] <- pl.pass.clean$first[is.na(pl.pass.clean$last)]

pl.pass.mids_10_90s <- pl.pass.clean %>% filter (`90s▼`>10 & (Pos == "MF" | Pos == "MF,DF" | Pos == "MF,FW" | Pos == "DF,FW" | Pos == "FW"))



# The two functions below return the mean values for the x and y-axis, which are used to create a horizontal and vertical line, diving the plot into quadrants.

pl.pass.mids_10_90s %>% summarize(mean(Att...21))
pl.pass.mids_10_90s %>% summarize(mean((Cmp...20/Att...21)))



## Scatter Plot Visualization of Long Passes Attempted per 90 vs. Percentage Successfully Completed
# ggplot: from the data frame created above, I chose attempted long passes per 90 as x-axis & long pass completion percent as y-axis
# geom_point: to display a scatter plot
# geom_text_repel: used last name column text as the label for data points that met specific criteria (if every data point was labelled, it would be too crowded)
# xlim: to set x-axis from 0 to 15. Allowed ylim to automatically set itself
# geom_hline and vline: were set to the mean x and y values to create quadrants (calculated in line 47 & 48)
# labs (to create title, subtitle, caption), annotate (to create 4 text labels to describe each quadrant)
# scale_x/y_continuous: set axis names, change unit of y-axis ticks to percent, used pretty_breaks to set number of data ticks across each axis

pl.pass.mids_longpass_att.v.cmp <-
  ggplot(data=pl.pass.mids_10_90s, aes(x=Att...21, y=(Cmp...20/Att...21)))+
  geom_point()+
  geom_text_repel(data=subset(pl.pass.mids_10_90s, Att...21>7 | (Cmp...20/Att...21)>.75 | (Cmp...20/Att...21)<.375),
                  aes(x=Att...21, y=(Cmp...20/Att...21), label=last), family="Helvetica", size=3.3, vjust=-.7)+
  xlim(0,15)+
  geom_hline(yintercept=.59, size=.2)+
  geom_vline(xintercept=4.38, size=.2)+
  labs(title="Premier League Long Passing (2022-23 Season)",
       subtitle="Long Passes Attempted per 90 vs. Percentage Successfully Completed",
       caption="Statistics gathered from FBref.com")+
  annotate("text", x=11.5, y=.67, label="Many Attempts,\nHigh Success Rate",
           family="Helvetica", fontface="bold.italic", size=3.5, color="green4")+
  annotate("text", x=2.5, y=.30, label="Few Attempts,\nLow Success Rate",
           family="Helvetica", fontface="bold.italic", size=3.5, color="tomato4")+
  annotate("text", x=.9, y=.665, label="Few Attempts,\nHigh Success Rate",
           family="Helvetica", fontface="bold.italic", size=3.5, color="green4")+
  annotate("text", x=9.2, y=.38, label="Many Attempts,\nLow Success Rate",
           family="Helvetica", fontface="bold.italic", size=3.5, color="tomato4")+
  scale_x_continuous(name="Long Passes Attempted (per 90 Minutes)", breaks = scales::pretty_breaks(n = 7)) +
  scale_y_continuous(name="Long Pass Completion Percentage", labels=percent, breaks = scales::pretty_breaks(n = 5))  

pl.pass.mids_longpass_att.v.cmp
