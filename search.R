
#load packages----
library(ASySD)
library(tidyverse)
library(rcrossref)
library(stringr)

# add evaluation name here
evaluation_name <- "SR1_elicit"

#load files----
original_search <- read.csv('data/SR1/original/all_studies_0224.csv')
original_Tiab_include <- read.csv('data/SR1/original/screen_include_0224.csv')
original_ft_exclude <- read.csv('data/SR1/original/screened_excluded_0224.csv')
elicit <- read.csv("data/SR1/elicit/sr1_elicit_screen_results.csv")
elicit <- elicit %>% mutate(record_id = paste0("sr1_elicit_", row_number()),
                            label = ifelse(Screening.judgement == "Include", 'elicit_include', 'elicit_exclude')
                            )

# get metadata from crossref----
for(i in 1:nrow(elicit)){
  doi_i <- elicit[i, ]$DOI 
  if(!is.na(doi_i)){
    bib <- cr_cn(dois = doi_i, format = "bibentry")
    try(dat <- data.frame(DOI = doi_i, 
                          pages = bib$pages,
                          volume = bib$volume, 
                          number = bib$number,
                          isbn = bib$ISSN))
    if(!exists("dat")) dat <- data.frame(DOI = doi_i, 
                                       pages = NA,
                                       volume = NA,
                                       number = NA,
                                       isbn = NA)
    try(abst <- cr_abstract(doi_i))
    if(!exists('abst')) dat <- dat%>%mutate(abstract = NA) else dat <- dat%>%mutate(abstract = abst)
    if(!exists('crossrefdata')) crossrefdata <- dat else crossrefdata <- rbind(crossrefdata, dat)
       
    try(rm(bib, dat))
    try(rm(abst))
    
  }
}

elicit_with_crossref <- left_join(elicit, crossrefdata, by = "DOI")

# wrangle to asysd compatible format----
elicit_asysd_format <-elicit_with_crossref %>% 
  select(author = Authors,
         year = Year,
         journal = Venue,
         doi = DOI,
         title = Title,
         pages,
         volume,
         number,
         abstract,
         record_id,
         isbn,
         label)%>%
  mutate(source = 'elicit')


#review overlap between original search and elicit
original_search_asysd <- original_search %>%
  mutate(
    pages = '',
    volume = '',
    number = '',
    isbn = '',
    label = ifelse(StudyId %in% original_ft_exclude$StudyId, 'original_ft_exclude',
                   ifelse(StudyId%in% original_Tiab_include$StudyId, 'original_ft_include', 'original_Tiab_exclude')),
    source = 'original')%>%
  select(author = Authors,
         year = Year,
         journal = PublicationName,
         doi = Doi,
         title = Title,
         pages,
         volume,
         number,
         abstract = Abstract,
         record_id = StudyId,
         isbn,
         label,
         source)

todedup <- rbind(elicit_asysd_format, original_search_asysd)

# use asysd to identify overlaps ----
results <-dedup_citations(todedup, merge_citations = TRUE, keep_source = 'elicit')
#asysd have deemed these unique - with deduplicates collapsed into one row each with record_ids concatenated
unique <- results$unique

manual_dedup<- results$manual_dedup

#write csv file for record keeping
write.csv(manual_dedup, paste0('output/manual_dedup_', evaluation_name, '.csv'))

# use shiny interface to identify if these overlap
manual_review <- manual_dedup_shiny(results$manual_dedup)

# Complete deduplication
final_result <- dedup_citations_add_manual(results$unique, additional_pairs = manual_review)
final_result <- final_result%>%
  mutate(label = str_replace_all(label, 'original_Tiab_include', 'original_ft_include'))

overlap <- filter(final_result, grepl(',', record_ids))

write_citations(final_result, type = 'csv', paste0('output/', evaluation_name, '_search_screen.csv'))

#write results out
write.csv(overlap, paste0('output/overlapping_citations_', evaluation_name, '.csv'), row.names=FALSE)
