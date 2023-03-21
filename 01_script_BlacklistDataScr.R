install.packages("stringdist")
install.packages("RColorBrewer")
library(stringdist)

library(tidyverse)
library(rvest)
library(data.table)
library(RColorBrewer)

# list of urls used:

# 1) 31.03.1995. "Noteikumi par beznodokļu vai zemu nodokļu valstīm un zonām". https://likumi.lv/ta/id/34462-noteikumi-par-beznodoklu-vai-zemu-nodoklu-valstim-un-zonam 
# 2) 06.05.1997. "Grozījums Ministru kabineta 1995.gada 31.marta noteikumos nr.69 "Noteikumi par beznodokļu vai zemu nodokļu valstīm un zonām"" https://likumi.lv/ta/id/43367-grozijums-ministru-kabineta-1995-gada-31-marta-noteikumos-nr-69-noteikumi-par-beznodoklu-vai-zemu-nodoklu-valstim-un-zonam- 
# 3) 26.06.2001. "Noteikumi par zemu nodokļu vai beznodokļu valstīm un teritorijām" https://m.likumi.lv/doc.php?id=25839&version_date=30.06.2001
# 4) 22.04.2004. "Grozījums Ministru kabineta 2001.gada 26.jūnija noteikumos Nr.276 "Noteikumi par zemu nodokļu vai beznodokļu valstīm un teritorijām"" https://likumi.lv/ta/id/87710-grozijums-ministru-kabineta-2001-gada-26-junija-noteikumos-nr-276-noteikumi-par-zemu-nodoklu-vai-beznodoklu-valstim-un-teritori...
# 5) 12.01.2010. "Grozījumi Ministru kabineta 2001.gada 26.jūnija noteikumos Nr.276 "Noteikumi par zemu nodokļu vai beznodokļu valstīm un teritorijām"" https://likumi.lv/ta/id/203679-grozijumi-ministru-kabineta-2001-gada-26-junija-noteikumos-nr-276-noteikumi-par-zemu-nodoklu-vai-beznodoklu-valstim-un-teritori…
# 6) 05.03.2013. "Grozījumi Ministru kabineta 2001.gada 26.jūnija noteikumos Nr.276 "Noteikumi par zemu nodokļu vai beznodokļu valstīm un teritorijām"" https://likumi.lv/ta/id/255241-grozijumi-ministru-kabineta-2001-gada-26-junija-noteikumos-nr-276-noteikumi-par-zemu-nodoklu-vai-beznodoklu-valstim-un-teritori...
# 7) 07.11.2017. "Noteikumi par zemu nodokļu vai beznodokļu valstīm un teritorijām" https://m.likumi.lv/doc.php?id=294935
# 8) 17.12.2020. "Noteikumi par zemu nodokļu vai beznodokļu valstīm un teritorijām" https://m.likumi.lv/doc.php?id=319726
# 9) 11.01.2022. "Grozījumi Ministru kabineta 2020. gada 17. decembra noteikumos Nr. 819 "Noteikumi par zemu nodokļu vai beznodokļu valstīm un teritorijām"" https://likumi.lv/ta/id/329124-grozijumi-ministru-kabineta-2020-gada-17-decembra-noteikumos-nr-819-noteikumi-par-zemu-nodoklu-vai-beznodoklu-valstim-un-terito...
# 10) 20.12.2022. "Grozījums Ministru kabineta 2020. gada 17. decembra noteikumos Nr. 819 "Noteikumi par zemu nodokļu vai beznodokļu valstīm un teritorijām"" https://m.likumi.lv/doc.php?id=338298


# Scraping data from Regulations and creating dataframe width 2 columns (1) No of point in the Regulation; 2) name of jurisdiction)

# ***** I. YEAR 1995 VERSION *****
# Define the URL to scrape
url <- "https://likumi.lv/ta/id/34462-noteikumi-par-beznodoklu-vai-zemu-nodoklu-valstim-un-zonam"

# Use rvest to scrape the website and extract the relevant text
webpage <- read_html(url)
text <- webpage %>%
  html_nodes("div p") %>%
  html_text() %>%
  str_extract("\\d+\\.\\s.*")

# Print the extracted text
cat(text)

# Extract strings width the pattern of "number(s)-dot-space-string(ends width string)"
valstis95 <- str_extract_all(text, pattern = "^([0-9]+)\\.\\s+(.*)$") %>% 
  map_chr(toString)

# filter only jurisdictions
valstis95 <- valstis95[5:59]

# split into 2 columns based on space
valstis95 <- str_split_fixed(valstis95, " ", 2)
# assign new column names
valstis95 <- `colnames<-`(valstis95, c("pkt.95", "teritorija"))
valstis95 <- as.data.frame(valstis95)
view(valstis95)

# typeof(valstis95)
# summary(valstis95)
# is.data.frame(valstis95)


# ***** II. YEAR 1997 VERSION (AMENDMENTS 1 TO YEAR 95 VERSION) *****
# (excluding from the list a jurisdiction that was excluded form the blacklist)

valstis97 <- valstis95 %>% 
  filter(pkt.95 != "48.")

# ***** III. YEAR 2001 VERSION (NEW REGULATION) *****

# Define the URL to scrape
# url1 <- "https://www.vestnesis.lv/ta/id/25839"
url1 <- "https://m.likumi.lv/doc.php?id=25839&version_date=30.06.2001"

# Use rvest to scrape the website and extract the relevant text
webpage1 <- read_html(url1)
text1 <- webpage1 %>%
  html_nodes("div p") %>%
  html_text() %>%
  str_extract("^([0-9]+)\\.([0-9]+).*")
  # str_extract("^\\d+\\.\\d+\\.\\s.*+|\\(|\\)")

# Print the extracted text
cat(text1)

# Extract strings width the pattern of "number(s)-dot-space-string(ends width string)"
valstis2001 <- str_extract_all(text1, pattern = "^([0-9]+)\\.([0-9]+).*") %>% 
  map_chr(toString)

# filter only jurisdictions
valstis2001 <- valstis2001[2:69]

# split into 2 columns based on space
valstis2001 <- str_split_fixed(valstis2001, " ", 2)
# assign new column names
valstis2001 <- `colnames<-`(valstis2001, c("pkt.2001", "teritorija"))
valstis2001 <- as.data.frame(valstis2001)
view(valstis2001)

# typeof(valstis2001)
# summary(valstis2001)
# is.data.frame(valstis2001)


# ***** IV. YEAR 2004 VERSION (AMENDMENTS 1 TO YEAR 2001 VERSION) *****

valstis2004 <- valstis2001 %>% 
  filter(pkt.2001 != "1.32.")

# ***** V. YEAR 2010 VERSION (AMENDMENTS 2 TO YEAR 2001 VERSION) *****

valstis2010 <- valstis2004 %>% 
  filter(!pkt.2001 %in% c("1.29.", "1.40.", "1.54."))

# ***** VI. YEAR 2013 VERSION (AMENDMENTS 3 TO YEAR 2001 VERSION) *****

valstis2013 <- valstis2010 %>% 
  filter(!pkt.2001 %in% c("1.4."))

valstis2013 <- valstis2013 %>% 
  add_row(pkt.2001 = "1.31.1", 
          teritorija = "Kirasao (Nīderlandes Karaliste)")

valstis2013 <- valstis2013 %>% add_row(pkt.2001 = "1.60.1",
        teritorija = "Sintmārtena (Nīderlandes Karaliste)")

valstis2013 <- valstis2013 %>% 
  arrange(teritorija)


# ***** VII. YEAR 2017 VERSION (NEW REGULATION) *****

# Define the URL to scrape
url2 <- "https://m.likumi.lv/doc.php?id=294935"

# Use rvest to scrape the website and extract the relevant text
webpage2 <- read_html(url2)
text2 <- webpage2 %>%
  html_nodes("div p") %>%
  html_text() %>%
  str_extract("^([0-9]+)\\.([0-9]+).*")
# str_extract("^\\d+\\.\\d+\\.\\s.*+|\\(|\\)")

# Print the extracted text
cat(text2)

# Extract strings width the pattern of "number(s)-dot-space-string(ends width string)"
valstis2017 <- str_extract_all(text2, pattern = "^([0-9]+)\\.([0-9]+).*") %>% 
  map_chr(toString)

# filter only jurisdictions
valstis2017 <- valstis2017[3:27]

# split into 2 columns based on space
valstis2017 <- str_split_fixed(valstis2017, " ", 2)
# assign new column names
valstis2017 <- `colnames<-`(valstis2017, c("pkt.2017", "teritorija"))
valstis2017 <- as.data.frame(valstis2017)
# remove the unnecessary symbols at the end of the string (for country names to be better comparable)
valstis2017$teritorija <- str_remove_all(valstis2017$teritorija, ";")
valstis2017$teritorija <- str_remove_all(valstis2017$teritorija, "\\.")

view(valstis2017)


# ***** VIII. YEAR 2020 VERSION (NEW REGULATION) *****

# Define the URL to scrape
url3 <- "https://m.likumi.lv/doc.php?id=319726"

# Use rvest to scrape the website and extract the relevant text
webpage3 <- read_html(url3)
text3 <- webpage3 %>%
  html_nodes("div p") %>%
  html_text() %>%
  str_extract("^([0-9]+)\\.([0-9]+).*")
# str_extract("^\\d+\\.\\d+\\.\\s.*+|\\(|\\)")

# Print the extracted text
cat(text3)

# Extract strings width the pattern of "number(s)-dot-space-string(ends width string)"
valstis2020 <- str_extract_all(text3, pattern = "^([0-9]+)\\.([0-9]+).*") %>% 
  map_chr(toString)

# filter only jurisdictions
valstis2020 <- valstis2020[3:14]


# use gsub() to add a space after the second dot. In the end semicolon or dot (otherwise the problem width the dot)
valstis2020 <- gsub("(\\..*\\.)(.*;|*.)", "\\1 \\2", valstis2020)


# split into 2 columns based on space
valstis2020 <- str_split_fixed(valstis2020, " ", 2)



# assign new column names
valstis2020 <- `colnames<-`(valstis2020, c("pkt.2020", "teritorija"))
valstis2020 <- as.data.frame(valstis2020)

valstis2020$teritorija <- str_remove_all(valstis2020$teritorija, ";")
valstis2020$teritorija <- str_remove_all(valstis2020$teritorija, "\\.")

view(valstis2020)

# ***** IX. YEAR 2022 VERSION (AMENDMENTS 1 TO YEAR 2020 VERSION) *****

valstis2022_1 <- valstis2020 %>% 
  filter(!pkt.2020 %in% c("2.1.", "2.5.", "2.10."))

view(valstis2022_1)



# ***** X. YEAR 2022_2 VERSION (AMENDMENTS 2 TO YEAR 2020 VERSION) *****


# Define the URL to scrape
url4 <- "https://m.likumi.lv/doc.php?id=338298"

# Use rvest to scrape the website and extract the relevant text
webpage4 <- read_html(url4)
text4 <- webpage4 %>%
  html_nodes("div p") %>%
  html_text() %>%
  str_extract("^([0-9]+)\\.([0-9]+).*")
# str_extract("^\\d+\\.\\d+\\.\\s.*+|\\(|\\)")

# Print the extracted text
cat(text4)

# Extract strings width the pattern of "number(s)-dot-space-string(ends width string)"
valstis2022_2 <- str_extract_all(text4, pattern = "^([0-9]+)\\.([0-9]+).*") %>% 
  map_chr(toString)

# filter only jurisdictions
valstis2022_2 <- valstis2022_2[7:18]

# split into 2 columns based on space
valstis2022_2 <- str_split_fixed(valstis2022_2, " ", 2)
# assign new column names
valstis2022_2 <- `colnames<-`(valstis2022_2, c("pkt.2020", "teritorija"))
valstis2022_2 <- as.data.frame(valstis2022_2)

# remove the unnecessary symbols at the end of strings (this will help to find unique variables and join data width other dataframes later on)
valstis2022_2$teritorija <- str_remove_all(valstis2022_2$teritorija, ";")
valstis2022_2$teritorija <- str_remove_all(valstis2022_2$teritorija, ".\"")

view(valstis2022_2)


# *** EXTRACT UNIQUE NAMES OF JURISDICTIONS (USING DATA.TABLE LIST OF DF)***
# teritorijas_kopa <- rbindlist(list(valstis95$teritorija, valstis97$teritorija, valstis2001$teritorija,
#                                    valstis2004$teritorija, valstis2010$teritorija, valstis2013$teritorija,
#                                    valstis2017$teritorija, valstis2020$teritorija, valstis2022_1$teritorija,
#                                    valstis2022_2$teritorija))

teritorijas_kopa <- rbindlist(list(valstis95, valstis97, valstis2001,
                                   valstis2004, valstis2010, valstis2013,
                                   valstis2017, valstis2020, valstis2022_1,
                                   valstis2022_2))

ter_kop_tab <- teritorijas_kopa %>% 
  select(teritorija)

ter_unik <- unique(ter_kop_tab) %>% 
  arrange(teritorija)

# Fuzzy matches width stringdist
# sources https://statisticsglobe.com/fuzzy-matching-r
# https://www.youtube.com/watch?v=txHkNr29w20
# https://journal.r-project.org/archive/2014-1/loo.pdf The stringdist Package for Approximate String Matching


df1 <- ter_unik
df2 <- ter_unik

# adist(df1$teritorija, df2$teritorija)
# agrep(df1$teritorija, df2$teritorija, max.distance = 10, value = TRUE)
# amatch(df1$teritorija, df2$teritorija, maxDist = 10)
# vec1 <- as.vector(df1)
# vec2 <- as.vector(df2)
# amatch(vec1, vec2, maxDist = 10)

# this code seems will not be used in the project (the idea was to classify jurisdictions
# and find duplicated, however other methods are expected to be more efficient):
# stringdistmatrix(df1$teritorija, df2$teritorija, useNames = TRUE)
# p <- stringdistmatrix(df1$teritorija, df2$teritorija, useNames = TRUE) # uses default method "lv"
# p_osa <- stringdistmatrix(df1$teritorija, df2$teritorija, method = "osa", useNames = TRUE)
# p_dl <- stringdistmatrix(df1$teritorija, df2$teritorija, method = "dl", useNames = TRUE)
# p_hamming <- stringdistmatrix(df1$teritorija, df2$teritorija, method = "hamming", useNames = TRUE)
# p_lcs <- stringdistmatrix(df1$teritorija, df2$teritorija, method = "lcs", useNames = TRUE)
# p_qgram <- stringdistmatrix(df1$teritorija, df2$teritorija, method = "qgram", useNames = TRUE)
# p_cosine <- stringdistmatrix(df1$teritorija, df2$teritorija, method = "cosine", useNames = TRUE)
# p_jaccard <- stringdistmatrix(df1$teritorija, df2$teritorija, method = "jaccard", useNames = TRUE)
# p_jw <- stringdistmatrix(df1$teritorija, df2$teritorija, method = "jw", useNames = TRUE)
# p_soundex <- stringdistmatrix(df1$teritorija, df2$teritorija, method = "soundex", useNames = TRUE)
# 
# heatmap(p, col = cm.colors(256))
# heatmap(p_osa, col = cm.colors(256))
# heatmap(p_dl, col = cm.colors(256))
# heatmap(p_hamming, col = cm.colors(256))
# heatmap(p_lcs, col = cm.colors(256))
# heatmap(p_qgram, col = cm.colors(256))
# heatmap(p_cosine, col = cm.colors(256))
# heatmap(p_jaccard, col = cm.colors(256))
# heatmap(p_jw, col = cm.colors(256)) # this seemes to be the most useful
# heatmap(p_soundex, col = cm.colors(256))
# 
# heatmap(p_jw, col = terrain.colors(256)) # this seemes to be the most useful


# now width removed common strings which are not of interest (to see the contrast better in heatmaps)

# txt_to_remove <- c("(Lielbritānijas un Ziemeļīrijas Apvienotā Karaliste)", "Republika")

# df1_rem_str <- as.data.frame(gsub("(Lielbritānijas un Ziemeļīrijas Apvienotā Karaliste)", "", ter_unik$teritorija))

# df1_rem_str <- df1
# df1_rem_str$teritorija <- unlist(gsub("(Lielbritānijas un Ziemeļīrijas Apvienotā Karaliste)", "", df1_rem_str$teritorija))
# df1_rem_str$teritorija <- unlist(gsub("Republika", "R.", df1_rem_str$teritorija))
# df1_rem_str$teritorija <- unlist(gsub("Nīderlandes Karaliste", "NK", df1_rem_str$teritorija))


# df2_rem_str <- df1_rem_str
# 
# p_rem_jw <- stringdistmatrix(df1_rem_str$teritorija, df2_rem_str$teritorija, method = "jw", useNames = TRUE)
# 
# heatmap(p_rem_jw, col = terrain.colors(256)) # this seemes to be the most useful
# 
# str(p_jw)
# str(p_rem_jw)
# 
# # seems not the best method used. Can some words be ignored?
# p1 <- as.data.frame(stringdistmatrix(df1$teritorija, df2$teritorija, useNames = TRUE))


# Starts/Ends on
# setdiff(x, y) finds all rows in x that aren't in y.



# * Batch 1
Starts_01.04.1995 <- valstis95$teritorija

# * Batch 2
Ends_10.05.1997 <- setdiff(valstis95$teritorija, valstis97$teritorija)
Starts_10.05.1997 <- setdiff(valstis97$teritorija, valstis95$teritorija)

# * Batch 3
Ends_30.06.2001 <- setdiff(valstis97$teritorija, valstis2001$teritorija)
Starts_30.06.2001 <- setdiff(valstis2001$teritorija, valstis97$teritorija)

# * Batch 4
Ends_01.05.2004 <- setdiff(valstis2001$teritorija, valstis2004$teritorija)
Starts_01.05.2004 <- setdiff(valstis2004$teritorija, valstis2001$teritorija)

# * Batch 5
Ends_16.01.2010 <- setdiff(valstis2004$teritorija, valstis2010$teritorija)
Starts_01.05.2004 <- setdiff(valstis2010$teritorija, valstis2004$teritorija)

# * Batch 6
Ends_08.03.2013 <- setdiff(valstis2010$teritorija, valstis2013$teritorija)
Starts_08.03.2013 <- setdiff(valstis2013$teritorija, valstis2010$teritorija)

# * Batch 7
Ends_01.01.2018 <- setdiff(valstis2013$teritorija, valstis2017$teritorija)
Starts_01.01.2018 <- setdiff(valstis2017$teritorija, valstis2013$teritorija)

# * Batch 8
Ends_01.01.2021 <- setdiff(valstis2017$teritorija, valstis2020$teritorija)
Starts_01.01.2021 <- setdiff(valstis2020$teritorija, valstis2017$teritorija)

# * Batch 9
Ends_14.01.2022 <- setdiff(valstis2020$teritorija, valstis2022_1$teritorija)
Starts_14.01.2022 <- setdiff(valstis2022_1$teritorija, valstis2020$teritorija)

# * Batch 10
Ends_01.01.2023 <- setdiff(valstis2022_1$teritorija, valstis2022_2$teritorija)
Starts_01.01.2023 <- setdiff(valstis2022_2$teritorija, valstis2022_1$teritorija)

# * Batch 11 - to show the situation of today
Ends_01.03.2023 <- c()

# MAYBE BETTER TO START WIDTH FUZZY MATCHING WIDTH COUNTRY CODES? Special package for countrycodes?
# that would help to find dubpicated easier
# World Bank data to use?
# https://joenoonan.se/post/country-code-tutorial/

# Using rvest to Scrape an HTML table https://www.r-bloggers.com/2015/01/using-rvest-to-scrape-an-html-table/
# Par pasaules valstu un teritoriju nosaukumiem latviešu valodā https://www.vestnesis.lv/op/2018/218.9
# this table is from the official publication of the names of countries in Latvian as well as width corresponding
# country codes (ISO). This will help to connect data width other publicly available data.

url5 <- "https://www.vestnesis.lv/op/2018/218.9"

webpage5 <- read_html(url5)
text5 <- webpage5 %>%
  html_nodes(xpath= '//*[contains(concat( " ", @class, " " ), concat( " ", "mk_table", " " ))]') %>%
  html_table()

iso2_valstis_LV <- data.frame(text5)

new_colnames <- as.character(iso2_valstis_LV[2,])

# remove the first two rows from the data frame
iso2_valstis_LV <- iso2_valstis_LV[-1:-2,]

# transpose the selected row into a one-row data frame
new_colnames <- t(data.frame(new_colnames))

# set the transposed row as column names of the data frame
colnames(iso2_valstis_LV) <- new_colnames

View(iso2_valstis_LV)


# DF FOR GANTT CHART


df_gantt <- ter_unik %>% 
  mutate(BlacklistedFrom = case_when(
    
  ))
# will that automatically duplicate rows in case a country starts/ends more than once?

# stringdistmatrix(df1$teritorija)

# Compute distance metrics between strings
# https://search.r-project.org/CRAN/refmans/stringdist/html/stringdist.html

# String metrics in stringdist
# https://search.r-project.org/CRAN/refmans/stringdist/html/stringdist-metrics.html


# teritorijas_unikalas <- unique(teritorijas_kopa$teritorija)

# NEXT STEP - CREATE VECTORS TO REPLACE NAMES (1) SCRAPE LV OFFICIAL TABLE & USE OFFICIAL NAMES
# REPLACE NAMES IN ALL DF (CREATE A LIST)
# JOIN ISO-2/3
# ADD MISSING VALUES WHERE POSSIBLE


# For each regulaton/amendment "Starts" and "Ends" vectors need to be created
# *** in the beginning DF for each version of the list over time, and then use anti_join() where necessary
# ***need to check all the names to be consistent
# *** iso-3 and iso-2 need to be added as columns
# is it worth to colorcode based on the regulation? Or rectangles? Or vline()?
# to use igraph for indirect control?

# in Datacamp about dates and time ymd function?
# linegraph width the numbers of jurisdictions?

# How about numbering? Ordered factor? Alphabetical order is used anyway, but in Latvian, so better to use ordered factor

# What to regress to? Are there purposes in the EU documents? Ex ante? Researches?