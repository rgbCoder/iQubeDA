#install.packages for the respective packages used, more in readme.txt
library(dplyr)
library(ggplot2)
library(magrittr)
library(plotly)
library(installr)
#install.pandoc() to install pandoc
library(plotly)
library(dplyr)
library(installr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

#Visualise trends in terrorist attacks on worldmap
#extract data and filter
fullgtd <- read.csv("globalterrorismdb_0617dist.csv")
ataf <- subset(fullgtd, select = c("iyear", "country_txt"))
ataf <- na.omit(ataf)

#count number of attacks for each country every year
df <- as.data.frame(rename(count(ataf, iyear, country_txt), num_attacks = n))
#break the timeline into four quantiles
df$q <- with(df, cut(iyear, quantile(iyear)))
levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
df$q <- as.ordered(df$q)

#read data to get coordinates for each country
coord <- read.csv("coordinates.csv")
colnames(coord) <- c("country_txt", "lat", "lon")
df <- merge(df, coord, by = "country_txt")
df <- na.omit(df)

#Mention various parameters for the graph
g <- list(
  scope = 'world',
  projection = list(type = 'world'),
  showcoastline = TRUE,
  coastlinecolor = toRGB("black"),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("red"),
  countrycolor = toRGB("yellow")
)

#plot graph
p <- plot_geo(df, locationmode = 'world', sizes = c(5,750)) %>%
  add_markers(
    x = ~lon, y = ~lat, size = ~num_attacks, color = ~q
  ) %>%
  layout(title = 'Number of attacks from 1970 to 2016 globally (Click legend to toggle)', geo = g)

#To view this visualisation, open TimelineOfAttacks.html
htmlwidgets::saveWidget(p, "TimelineOfAttacks.html")

#Visualise terrorist group activities
#from full data from gtd, select required features and remove NAs
filt <- subset(fullgtd, select = c("iyear", "gname"))
filt <- na.omit(filt)
#count number of attacks by each terrorist group, filter the result with threshold and remove Unknowns
sdf <- as.data.frame(rename(count(filt, iyear, gname), num_attacks = n))
f_sdf <- sdf[!(sdf$num_attacks < 200 | sdf$gname == 'Unknown'),]

#generate color palettes
n <- nrow(as.data.frame(unique(f_sdf$gname)))
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
colour <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

#plot multiple lines on graph with specific scale
png(filename = "TerroristGroupActivity.png", width = 1200, height = 1000)
pp <- ggplot() + geom_line(aes(y = num_attacks, x = iyear, colour = gname),
 	data = f_sdf, stat="identity", size = 1.25) + 
	scale_x_continuous(breaks=seq(1970,2016,5))+ 
	scale_y_continuous(breaks=seq(200, max(f_sdf$num_attacks),200)) + 
	ggtitle("Activity of terrorist groups between 1970-2016") + 
	labs(x = "Year", y = "Number of attacks by the group") + 
	scale_colour_manual(values=colour)
dev.off()

#To view this visualisation, open TerroristGroupActivity.png

#Visualise relation between targetType and attackType of terrorist attacks
#from full data, extract required features, with threshold applied
filt <- subset(fullgtd, iyear >= 1996, select = c("targtype1_txt", "attacktype1_txt"))
#Drop rows with NAs and Unknowns
filt <- na.omit(filt)
filt1 <- filt[!(filt$attacktype1_txt == 'Unknown' | filt$targtype1_txt == 'Unknown'),]
colnames(filt1) <- c("target", "attack")

#Find count of number of attacks for each combination of attack type and target type
df_summary <-
    filt1                       %>% 
    group_by(attack,target)            %>% 
    summarise(df_count = n()) 

#Restructure data to obtain better visualisation
sdf <- as.data.frame(df_summary)
f_sdf <- subset(sdf, df_count > 10)
tg <- as.data.frame(unique(f_sdf$target))
colnames(tg) <- c("target")

y1 <- subset(f_sdf, attack == 'Armed Assault', select = c("target", "df_count"))
y1 <- merge(x = y1, y = tg, by = "target", all = TRUE)
y2 <- subset(f_sdf, attack == 'Assassination', select = c("target", "df_count"))
y2 <- merge(x = y2, y = tg, by = "target", all = TRUE)
y3 <- subset(f_sdf, attack == 'Bombing/Explosion', select = c("target", "df_count"))
y3 <- merge(x = y3, y = tg, by = "target", all = TRUE)
y4 <- subset(f_sdf, attack == 'Facility/Infrastructure Attack', select = c("target", "df_count"))
y4 <- merge(x = y4, y = tg, by = "target", all = TRUE)
y5 <- subset(f_sdf, attack == 'Hijacking', select = c("target", "df_count"))
y5 <- merge(x = y5, y = tg, by = "target", all = TRUE)
y6 <- subset(f_sdf, attack == 'Hostage Taking (Barricade Incident)', select = c("target", "df_count"))
y6 <- merge(x = y6, y = tg, by = "target", all = TRUE)
y7 <- subset(f_sdf, attack == 'Hostage Taking (Kidnapping)', select = c("target", "df_count"))
y7 <- merge(x = y7, y = tg, by = "target", all = TRUE)
y8 <- subset(f_sdf, attack == 'Unarmed Assault', select = c("target", "df_count"))
y8 <- merge(x = y8, y = tg, by = "target", all = TRUE)


dataf <- data.frame(matrix(NA, nrow = 21, ncol = 8))
dataf <- data.frame(target = unique(f_sdf$target), y1=y1$df_count,y2=y2$df_count,y3=y3$df_count,y4=y4$df_count,y5=y5$df_count,y6=y6$df_count,y7=y7$df_count,y8=y8$df_count)

#Plot graph with plotly
ppp <- plot_ly(data = dataf, x =~target, type = "bar") %>%
	add_trace(y = ~y1, name = 'Armed Assault') %>%
	add_trace(y = ~y2, name = 'Assassination') %>%
	add_trace(y = ~y3, name = 'Bombing/Explosion') %>%
	add_trace(y = ~y4, name = 'Facility/Infrastructure Attack') %>%
	add_trace(y = ~y5, name = 'Hijacking') %>%
	add_trace(y = ~y6, name = 'Hostage Taking (Barricade Incident)') %>%
	add_trace(y = ~y7, name = 'Hostage Taking (Kidnapping)') %>%
	add_trace(y = ~y8, name = 'Unarmed Assault') %>%
	layout(
		xaxis = list(title = 'Target type'),
		yaxis = list(title = 'Count of target casualties for given attack type'),
		title = 'Attack type and Target type preview', 
		updatemenus = list(
			list(
				x = -0.08,
				buttons = list(
					list(method = "restyle", args = list("visible", list(F,F,F,F,F,F,F,F,F,F)), label = 'Select'),
                    		list(method = "restyle", args = list("visible", list(F,T,F,F,F,F,F,F,F,F)), label = "Armed Assault"),
               			list(method = "restyle", args = list("visible", list(F,F,T,F,F,F,F,F,F,F)), label = "Assassination"),
               			list(method = "restyle", args = list("visible", list(F,F,F,T,F,F,F,F,F,F)), label = "Bombing/Explosion"),
               			list(method = "restyle", args = list("visible", list(F,F,F,F,T,F,F,F,F,F)), label = "Facility/Infrastructure Attack"),
               			list(method = "restyle", args = list("visible", list(F,F,F,F,F,T,F,F,F,F)), label = "Hijacking"),
               			list(method = "restyle", args = list("visible", list(F,F,F,F,F,F,T,F,F,F)), label = "Hostage Taking (Barricade Incident)"),
               			list(method = "restyle", args = list("visible", list(F,F,F,F,F,F,F,T,F,F)), label = "Hostage Taking (Kidnapping)"),
               			list(method = "restyle", args = list("visible", list(F,F,F,F,F,F,F,F,T,F)), label = "Unarmed Assault")
					)
				)
			)
		)


#To view this visualisation, open AttackVsTarget.html
htmlwidgets::saveWidget(ppp, "AttackVsTarget.html")

