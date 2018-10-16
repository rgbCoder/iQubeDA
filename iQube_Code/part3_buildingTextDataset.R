mydata = read.csv('globalterrorismdb_0617dist.csv')
cols <- c("country_txt", "summary", "attacktype1_txt","targtype1_txt","motive","weapdetail","propcomment","addnotes")
textdata <- mydata[cols]

concatColumn <- c()
for (i in 1:ncol(textdata)) 
	concatColumn <- paste(concatColumn,textdata[,i])

concat = data.frame(mydata$country_txt,concatColumn)
write.csv(concat,'text_gtd.csv')
