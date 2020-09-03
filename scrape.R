library(rvest) 
library(tidyverse)
library(gsubfn)
library(fuzzyjoin)


index<-read_html("https://newdiscourses.com/translations-from-the-wokish/") #main index page



pages <- index %>%
  html_nodes(xpath = '//*[@id="page-2200"]/div/div/div/div/ul/li/p/a') %>%
  html_attr('href')  # grabbing article links
sourcelist<-list()
i<-1

#loop through list of pages and extract sources cited in each one
for(i in i:length(pages)){
  page<-read_html(pages[i])
  txt<-page%>%html_nodes(xpath='//div[@class="entry-content"]/p')%>%html_text()
  sources<-txt[grep("Source:", txt, ignore.case=T)]
  sourcelist[[i]]<-sources
  print(i)
  
}


sourceframe<-data.frame('entryurl' = rep(pages, lengths(sourcelist)), 
                        'source'= paste(unlist(sourcelist)))






#write.csv(sourceframe, file='./ndframe.csv', row.names = TRUE) #save output as csv


#creating author plot------


sourceframe$author<-strapplyc(as.character(sourceframe$source),"^Source:\\s+?(.+?)[\\.0-9]")%>% #extract authors
  lapply(function(x) ifelse(is.null(x), "NA" ,x))%>%
  unlist()

#kludgey solution to combine some inconsistent spellings
df<-sourceframe%>%  #grab names used more than 15 times
  filter(nchar(author)>10)%>%
  group_by(author)%>%
  summarize('count' = n())%>%
  ungroup()%>%
  filter(count>15)

sourceframe<-sourceframe%>% #replace less common spellings with the more common version
  filter(nchar(author)>10)%>%
  stringdist_left_join(df, by='author',max_dist=2)%>%
  mutate('author_comb' = ifelse(is.na(author.y), author.x, paste(author.y)) )



freq<-sourceframe%>%group_by(author_comb)%>% #get frequencies for each author
  summarize("freq" = n(),
            "authors" = iconv(unique(author_comb)[1], 'latin1', "UTF-8")[[1]])%>%
  ungroup()%>%
  mutate("auth2" = ifelse(freq>4, authors, "everyone else"))%>%
  group_by(auth2)%>%
  summarize("freq" = sum(freq))

plt<-freq%>% #create horizontal bar plot of top authors
  filter(auth2!="NA", auth2!='https://www')%>%
  ggplot(aes(x=freq,y=relevel(reorder(auth2, freq), ref="everyone else"), label=freq)) + geom_bar(stat='identity') + 
  ylab('author') + ggtitle("Authors/Websites cited five or more times on the \n New Discourses Social Justice Encylopedia") +
  theme_bw()
plt

#ggsave(plt, file="ndplot.png", width=7, height=10) save plot

