---
title: "Closure and Containment Changes in Oxford COVID-19 Government Response Tracker"
author: "Oxford COVID-19 Government Response Tracker (OxCGRT)"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, echo=F,include=F}
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(viridis)
library(knitr)
library(xtable)
library(knitr)
library(data.table)
library(kableExtra)
library(gganimate)
oxcgrt_changes <- fread("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest_allchanges.csv")


ChangesTable <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest_allchanges.csv", 
                         col_types = cols(Date = col_date(format = "%Y%m%d")))
ChangesTable <- ChangesTable %>% filter(Date>Sys.Date()-14)

url_oxcgrt <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest_withnotes.csv"
OxCGRTLatest <- as_tibble(read_csv(url(url_oxcgrt), col_types = cols(RegionName = col_character(), 
                                                                     RegionCode = col_character())))
OxCGRTLatest$Date <- ymd(OxCGRTLatest$Date)
```
```{r Changes, echo=F,include=F}
OxCGRTLastFortnight <- filter(OxCGRTLatest, Date>Sys.Date()-14 & is.na(RegionName))

OxCGRTCountriesChange <- OxCGRTLastFortnight %>% group_by(CountryName) %>% summarise(SI_Changes = n_distinct(StringencyIndex, na.rm=T)) %>%
  filter(SI_Changes>1) %>% select(CountryName) %>% pull()

OxCGRT_SIRanges <- OxCGRTLastFortnight %>% filter(CountryName %in% OxCGRTCountriesChange) %>% group_by(CountryName) %>%
  summarise(SIMaxWeeklyChange = range(StringencyIndex, na.rm=T)[2]-range(StringencyIndex, na.rm=T)[1])

OxCGRT_SIChange <- OxCGRTLastFortnight %>% filter(CountryName %in% OxCGRTCountriesChange) %>% group_by(CountryName, CountryCode) %>% arrange(Date) %>%
  select(CountryName,StringencyIndex) %>%
  distinct() %>% arrange(CountryName) %>% na.omit() %>%
  summarise(NumberChanges = length(StringencyIndex)-1,
    SIChange = last(StringencyIndex)-first(StringencyIndex),
    First=first(StringencyIndex), Last=last(StringencyIndex)) %>%
  left_join(OxCGRT_SIRanges) %>%
  mutate(MapColours = rep("0",length(CountryName)),
    MapColours = case_when(is.na(SIChange) ~ "0",
                                SIChange == 0 ~ "0",
                                SIChange < 0 ~ "1",
                                SIChange > 0 ~ "2"))
    
    
    # MapColours = case_when(is.na(SIChange) ~ "0",
    #                             SIChange <= -60 ~ "1",
    #                             SIChange > -60 & SIChange <= -40 ~ "2",
    #                             SIChange > -40 & SIChange <= -20 ~ "3",
    #                             SIChange > -20 & SIChange <= 0 ~ "4",
    #                             SIChange > 0 & SIChange <= 20 ~ "5",
    #                             SIChange > 20 & SIChange <= 40 ~ "6",
    #                             SIChange > 40 & SIChange <= 60 ~ "7",
    #                             SIChange > 60 ~ "8"))
```

## Global Changes in Stringency Index

The following map details the changes to the stringency index during the last two weeks:

```{r Setup,echo=FALSE,include=T, out.width="200%",fig.width = 7,fig.height=3}
# Can add captions by adding fig.cap="caption" to the {} section above
worldMap <- rnaturalearth::ne_countries(scale=110, returnclass = "sf") %>% filter(sov_a3!="ATA")

SIChangeMapData <- left_join(worldMap, OxCGRT_SIChange, by=c("sov_a3"="CountryCode"))
WorldChangeSIFortnightMap <- ggplot(SIChangeMapData, aes(geometry=geometry)) + geom_sf(aes(fill=SIChange))+
  scale_fill_viridis(na.value="grey90", name="Stringency Index Change") + theme(panel.background = element_blank())


SIChangeMapData$MapColours <- factor(SIChangeMapData$MapColours, levels=c(0:2))
# IndexColours <- c('#762a83', '#9970ab', '#c2a5cf','#e7d4e8',
#                   '#d9f0d3','#a6dba0','#5aae61','#1b7837',
#                   "grey90")
IndexColours <- c("grey90",'#762a83','#1b7837')
names(IndexColours) <- levels(SIChangeMapData$MapColours)
# FillScale <- scale_fill_manual(name = "Stringency Index Change",values = IndexColours, limits = factor(c(1:8,0)),
#                                          labels = c("Less -60","-60 to -40","-40 to -20","-20 to 0",
#                                                     "0 to 20","20 to 40","40 to 60", "More 60","No change"),
                                # na.value="grey90")
FillScale <- scale_fill_manual(name = "Stringency Index Change",values = IndexColours, limits = factor(c(1,0,2)),
                                         labels = c("Decreasing Stringency Index","No Change in Stringency Index","Increasing Stringency Index"),
                                na.value="grey90")

# The plot
ggplot(SIChangeMapData, aes(geometry=geometry)) + geom_sf(aes(fill=MapColours), size=0.1)+ 
  theme(panel.background = element_blank(),axis.ticks=element_blank(),legend.box.just = "center",
        axis.text = element_blank(), legend.key.size = unit(15, units = "pt"), plot.background = element_rect(size = 100),legend.title.align=1,legend.spacing.y = unit(4, units = "pt"), legend.title = element_text(size=10, face = "bold"),legend.text = element_text(size=7)) + FillScale

```

```{r Table, echo=F,include=T, results='asis'}
# SIMoveChangesTable <- ChangesTable %>% filter(CountryName %in% unique(OxCGRT_SIChange$CountryName) & str_detect(Notes, "change")==F) %>% filter(str_detect(Notes, "No policy")==F)

final <- oxcgrt_changes %>%
  ### THIS MIDDLE BLOCK COMBINES THE VALUE AND FLAG, i.e. value=1 and flag=1 becomes 1G, maybe this isn't useful for our purposes?
  .[, FlagCode := ifelse(Flag == 0, "T", ifelse(Flag == 1, "G", NA))] %>%
  .[, PolicyCode := ifelse(!is.na(FlagCode), paste0(as.character(PolicyValue), FlagCode), as.character(PolicyValue))] %>%
  .[order(CountryCode, PolicyType, Date)] %>%
  .[, `:=`(previous_PolicyCode = shift(PolicyCode, n = 1L, type = "lag")), by = .(CountryCode, PolicyType)] %>%
  .[,`:=` (Date = ymd(Date))] %>%
  .[Date > Sys.Date() - 36] %>%
  .[,!c("PolicyValue", "Flag", "FlagCode")] %>%
  filter(PolicyCode != previous_PolicyCode) %>%
  filter(Date >= Sys.Date() - 14) %>%
  select(!CountryCode) %>%
  relocate(CountryName,Date,PolicyType,PolicyCode,previous_PolicyCode,Notes) %>%
  rename("Country/territory/region"=CountryName,"Indicator" = PolicyType, "Current Policy"=PolicyCode, "Previous Policy" = previous_PolicyCode, "Date of change"=Date) %>% mutate(
    `Current Policy` = factor(`Current Policy`,levels = c("0","1","1T","1G","2","2T","2G","3","3T","3G","4","4T","4G"),ordered=TRUE),
    `Previous Policy` = factor(`Previous Policy`,levels = c("0","1","1T","1G","2","2T","2G","3","3T","3G","4","4T","4G"),ordered=TRUE),
    Direction_Change = paste0(day(`Date of change`),"/",month(`Date of change`),"/",year(`Date of change`)," ",
                              ifelse(`Current Policy`>`Previous Policy`,
                                     "<img src=/Users/user/Documents/COVID19%20Project/RA%20Projects/WeeklyChangesShinyApp/upPPT.png>",
                          "<img src=/Users/user/Documents/COVID19%20Project/RA%20Projects/WeeklyChangesShinyApp/downPPT.png>")))
                                     # " \U2191"," \U2193")))

# kable(final[1:5])

```

## Changes in closure and containment measures in the last two weeks

Please see our codebook for a full description of the policies we track.

```{r TableLoop, results='asis', echo = FALSE, message=FALSE,warning = FALSE}
FirstLast <- OxCGRTLastFortnight %>% filter(CountryName %in% OxCGRTCountriesChange) %>% group_by(CountryName, CountryCode) %>% arrange(Date) %>%
  select(CountryName,StringencyIndex) %>%
  distinct() %>% arrange(CountryName) %>% na.omit() %>%
  summarise(First=first(StringencyIndex), Last=last(StringencyIndex)) %>% select(CountryName,First,Last)

CountryFinalList <- final$`Country/territory/region`

finalCCPolicy <- final %>%
  filter(substring(Indicator,first = 0, last=1)=="C") %>%
  filter(`Current Policy` != `Previous Policy`)%>%
  mutate(
  IndicatorFormatted=paste0(substring(Indicator,first =5)," (",substring(Indicator,first = 0, last=2),")"),
  LevelsofClosureCurrentText = 
case_when(
#C1
substring(Indicator, first = 0, last=2) == "C1" & substring(`Current Policy`,first=0,last=1)=="1" ~ "**Recommended closure ** of schools or open with significant operational differences",
substring(Indicator, first = 0, last=2) == "C1" & substring(`Current Policy`,first=0,last=1)=="2" ~ "**Require** closure of **some** levels of schools",
substring(Indicator, first = 0, last=2) == "C1" & substring(`Current Policy`,first=0,last=1)=="3" ~ "**Require** closure of **all** levels of schools",
#C2
substring(Indicator, first = 0, last=2) == "C2" & substring(`Current Policy`,first=0,last=1)=="1" ~ "**Recommended closure ** (or work from home) of businesses or open with significant operational differences",
substring(Indicator, first = 0, last=2) == "C2" & substring(`Current Policy`,first=0,last=1)=="2" ~ "**Require** closure of **some** sectors of business",
substring(Indicator, first = 0, last=2) == "C2" & substring(`Current Policy`,first=0,last=1)=="3" ~ "**Require** closure of **all** sectors of business",
#C3
substring(Indicator, first = 0, last=2) == "C3" & substring(`Current Policy`,first=0,last=1)=="1" ~ "**Recommended** cancellation of public events",
substring(Indicator, first = 0, last=2) == "C3" & substring(`Current Policy`,first=0,last=1)=="2" ~ "**Required** cancellation of public events",
#C4
substring(Indicator, first = 0, last=2) == "C4" & substring(`Current Policy`,first=0,last=1)=="1" ~ "Restrictions on **very large** gatherings (over 1000 people)",
substring(Indicator, first = 0, last=2) == "C4" & substring(`Current Policy`,first=0,last=1)=="2" ~ "Restrictions on **large** gatherings (between 101-1000 people)",
substring(Indicator, first = 0, last=2) == "C4" & substring(`Current Policy`,first=0,last=1)=="3" ~ "Restrictions on **medium** gatherings (11-100 people)",
substring(Indicator, first = 0, last=2) == "C4" & substring(`Current Policy`,first=0,last=1)=="4" ~ "Restrictions on **small** gatherings (10 people or fewer)",
#C5
substring(Indicator, first = 0, last=2) == "C5" & substring(`Current Policy`,first=0,last=1)=="1" ~ "**Recommended closure ** (or significant reduction in volume/routes) of public transport",
substring(Indicator, first = 0, last=2) == "C5" & substring(`Current Policy`,first=0,last=1)=="2" ~ "**Require closure ** (or significant reduction in volume/routes) of public transport",
#C6
substring(Indicator, first = 0, last=2) == "C6" & substring(`Current Policy`,first=0,last=1)=="1" ~ "**Recommend** not leaving the house",
substring(Indicator, first = 0, last=2) == "C6" & substring(`Current Policy`,first=0,last=1)=="2" ~ "**Require** not leaving the house with exceptions for daily exercise, grocery shopping, and 'essential' trips",
substring(Indicator, first = 0, last=2) == "C6" & substring(`Current Policy`,first=0,last=1)=="3" ~ "**Require** not leaving the house with **minimal exceptions** (eg allowed to leave once a week, or only one person can leave at a time, etc)",
#C7
substring(Indicator, first = 0, last=2) == "C7" & substring(`Current Policy`,first=0,last=1)=="1" ~ "**Recommend** to not travel between regions/cities",
substring(Indicator, first = 0, last=2) == "C7" & substring(`Current Policy`,first=0,last=1)=="2" ~ "**Internal travel restrictions** are in place",
#C8
substring(Indicator, first = 0, last=2) == "C8" & substring(`Current Policy`,first=0,last=1)=="1" ~ "**Screening** arrivals coming into",
substring(Indicator, first = 0, last=2) == "C8" & substring(`Current Policy`,first=0,last=1)=="2" ~ "**Quarantine** arrivals from **some** regions coming into",
substring(Indicator, first = 0, last=2) == "C8" & substring(`Current Policy`,first=0,last=1)=="3" ~ "**Ban** arrivals from **some** regions coming into",
substring(Indicator, first = 0, last=2) == "C8" & substring(`Current Policy`,first=0,last=1)=="4" ~  "**Border closure** (ban on all regions) in",

# No measures 
substring(Indicator, first = 0, last=2) == "C1" & substring(`Current Policy`,first=0,last=1)=="0" ~ "**No measures** are present across all of",
substring(Indicator, first = 0, last=2) == "C2" & substring(`Current Policy`,first=0,last=1)=="0" ~ "**No measures** are present across all of",
substring(Indicator, first = 0, last=2) == "C3" & substring(`Current Policy`,first=0,last=1)=="0" ~ "**No measures** are present across all of",
substring(Indicator, first = 0, last=2) == "C4" & substring(`Current Policy`,first=0,last=1)=="0" ~ "**No measures** are present across all of",
substring(Indicator, first = 0, last=2) == "C5" & substring(`Current Policy`,first=0,last=1)=="0" ~ "**No measures** are present across all of",
substring(Indicator, first = 0, last=2) == "C6" & substring(`Current Policy`,first=0,last=1)=="0" ~ "**No measures** are present across all of",
substring(Indicator, first = 0, last=2) == "C7" & substring(`Current Policy`,first=0,last=1)=="0" ~ "**No measures** are present across all of",
substring(Indicator, first = 0, last=2) == "C8" & substring(`Current Policy`,first=0,last=1)=="0" ~ "**No measures** are present across all of"),

TargettingCurrentText = case_when(
substring(`Current Policy`,first=2,last=2)=="T" ~"in **some** parts of ",
substring(`Current Policy`,first=2,last=2)=="G" ~"across **all** of ",
is.na(substring(`Current Policy`,first=2,last=2))~ " "),

LevelsofPreviousClosureText = 
case_when(
#C1
substring(Indicator, first = 0, last=2) == "C1" & substring(`Previous Policy`,first=0,last=1)=="1" ~ "**Recommended closure ** of schools or open with significant operational differences",
substring(Indicator, first = 0, last=2) == "C1" & substring(`Previous Policy`,first=0,last=1)=="2" ~ "**Require** closure of **some** levels of schools",
substring(Indicator, first = 0, last=2) == "C1" & substring(`Previous Policy`,first=0,last=1)=="3" ~ "**Require** closure of **all** levels of schools",
#C2
substring(Indicator, first = 0, last=2) == "C2" & substring(`Previous Policy`,first=0,last=1)=="1" ~ "**Recommended closure ** (or work from home) of businesses or open with significant operational differences",
substring(Indicator, first = 0, last=2) == "C2" & substring(`Previous Policy`,first=0,last=1)=="2" ~ "**Require** closure of **some** sectors of business",
substring(Indicator, first = 0, last=2) == "C2" & substring(`Previous Policy`,first=0,last=1)=="3" ~ "**Require** closure of **all** sectors of business",
#C3
substring(Indicator, first = 0, last=2) == "C3" & substring(`Previous Policy`,first=0,last=1)=="1" ~ "**Recommended** cancellation of public events",
substring(Indicator, first = 0, last=2) == "C3" & substring(`Previous Policy`,first=0,last=1)=="2" ~ "**Required** cancellation of public events",
#C4
substring(Indicator, first = 0, last=2) == "C4" & substring(`Previous Policy`,first=0,last=1)=="1" ~ "Restrictions on **very large** gatherings (over 1000 people)",
substring(Indicator, first = 0, last=2) == "C4" & substring(`Previous Policy`,first=0,last=1)=="2" ~ "Restrictions on **large** gatherings (between 101-1000 people)",
substring(Indicator, first = 0, last=2) == "C4" & substring(`Previous Policy`,first=0,last=1)=="3" ~ "Restrictions on **medium** gatherings (11-100 people)",
substring(Indicator, first = 0, last=2) == "C4" & substring(`Previous Policy`,first=0,last=1)=="4" ~ "Restrictions on **small** gatherings (10 people or fewer)",
#C5
substring(Indicator, first = 0, last=2) == "C5" & substring(`Previous Policy`,first=0,last=1)=="1" ~ "**Recommended closure ** (or significant reduction in volume/routes) of public transport",
substring(Indicator, first = 0, last=2) == "C5" & substring(`Previous Policy`,first=0,last=1)=="2" ~ "**Require closure ** (or significant reduction in volume/routes) of public transport",
#C6
substring(Indicator, first = 0, last=2) == "C6" & substring(`Previous Policy`,first=0,last=1)=="1" ~ "**Recommend** not leaving the house",
substring(Indicator, first = 0, last=2) == "C6" & substring(`Previous Policy`,first=0,last=1)=="2" ~ "**Require** not leaving the house with exceptions for daily exercise, grocery shopping, and 'essential' trips",
substring(Indicator, first = 0, last=2) == "C6" & substring(`Previous Policy`,first=0,last=1)=="3" ~ "**Require** not leaving the house with **minimal exceptions** (eg allowed to leave once a week, or only one person can leave at a time, etc)",
#C7
substring(Indicator, first = 0, last=2) == "C7" & substring(`Previous Policy`,first=0,last=1)=="1" ~ "**Recommend** to not travel between regions/cities",
substring(Indicator, first = 0, last=2) == "C7" & substring(`Previous Policy`,first=0,last=1)=="2" ~ "**Internal travel restrictions** are in place",
#C8
substring(Indicator, first = 0, last=2) == "C8" & substring(`Previous Policy`,first=0,last=1)=="1" ~ "**Screening** arrivals into",
substring(Indicator, first = 0, last=2) == "C8" & substring(`Previous Policy`,first=0,last=1)=="2" ~ "**Quarantine** arrivals from **some** regions coming into",
substring(Indicator, first = 0, last=2) == "C8" & substring(`Previous Policy`,first=0,last=1)=="3" ~ "**Ban** arrivals from **some** regions coming into",
substring(Indicator, first = 0, last=2) == "C8" & substring(`Previous Policy`,first=0,last=1)=="4" ~  "**Border closure** (ban on all regions) coming into",

# No measures 
substring(Indicator, first = 0, last=2) == "C1" & substring(`Previous Policy`,first=0,last=1)=="0" ~ "**No measures** are present across all of",
substring(Indicator, first = 0, last=2) == "C2" & substring(`Previous Policy`,first=0,last=1)=="0" ~ "**No measures** are present across all of",
substring(Indicator, first = 0, last=2) == "C3" & substring(`Previous Policy`,first=0,last=1)=="0" ~ "**No measures** are present across all of",
substring(Indicator, first = 0, last=2) == "C4" & substring(`Previous Policy`,first=0,last=1)=="0" ~ "**No measures** are present across all of",
substring(Indicator, first = 0, last=2) == "C5" & substring(`Previous Policy`,first=0,last=1)=="0" ~ "**No measures** are present across all of",
substring(Indicator, first = 0, last=2) == "C6" & substring(`Previous Policy`,first=0,last=1)=="0" ~ "**No measures**s are present across all of",
substring(Indicator, first = 0, last=2) == "C7" & substring(`Previous Policy`,first=0,last=1)=="0" ~ "**No measures** are present across all of",
substring(Indicator, first = 0, last=2) == "C8" & substring(`Previous Policy`,first=0,last=1)=="0" ~ "**No measures** are present across all of"),

TargettingPreviousText = case_when(
substring(`Previous Policy`,first=2,last=2)=="T" ~"in **some** parts of ",
substring(`Previous Policy`,first=2,last=2)=="G" ~"across **all** of ",
is.na(substring(`Previous Policy`,first=2,last=2))~ ""),

`Current Policy Text` = ifelse(substring(`Current Policy`,first=1,last=1)=="0"|
                                 substring(Indicator, first = 0, last=2) == "C8",
                        paste0("**New policy:** \n\n",LevelsofClosureCurrentText," ",`Country/territory/region`," (",substring(`Current Policy`, first = 0, last=2),")"),
  paste0("**New policy:** \n\n",LevelsofClosureCurrentText,"; ",TargettingCurrentText," ",`Country/territory/region`," (",substring(`Current Policy`, first = 0, last=2),")")),
`Previous Policy Text` = ifelse(substring(`Previous Policy`,first=1,last=1)=="0"|substring(Indicator, first = 0, last=2) == "C8",
                        paste0("**Old policy:**\n \n", LevelsofPreviousClosureText," ",`Country/territory/region`," (",substring(`Previous Policy`, first = 0, last=2),")"),
                        paste0("**Old policy:** \n\n",LevelsofPreviousClosureText,"; ",TargettingPreviousText," ",`Country/territory/region`," (",substring(`Previous Policy`, first = 0, last=2),")")))



TableData <- finalCCPolicy %>% select(`Country/territory/region`,IndicatorFormatted,`Previous Policy Text`,Direction_Change,`Current Policy Text`,Notes)

library(kableExtra)
for (i in unique(FirstLast$CountryName)) {
  CountrySubsetTemp <- TableData %>% filter(`Country/territory/region`==i)
  cat("### ", i, " (",(FirstLast$First[FirstLast$CountryName==i]),"\U2192",FirstLast$Last[FirstLast$CountryName==i],")","\n",sep ="")
  for (j in unique(CountrySubsetTemp$IndicatorFormatted)){
    cat("#### ", j,"\n")
    TD2 <- TableData %>% filter(`Country/territory/region`==i & IndicatorFormatted==j) %>%
      select(!c(IndicatorFormatted,`Country/territory/region`))
    ImageVector <- ifelse(str_detect(TD2$Direction_Change,"\U2191"),
                          "<img src=/Users/user/Documents/COVID19 Project/RA Projects/WeeklyChangesShinyApp/upPPT.png>",
                          "<img src=/Users/user/Documents/COVID19 Project/RA Projects/WeeklyChangesShinyApp/downPPT.png>")
    cat(kableExtra::kbl(TD2,"html", col.names = NULL,escape = F) %>% 
          kable_styling() %>%
          column_spec(column=1, width = "1.5in") %>%
          column_spec(column=2, color = "grey80", width = "1in") %>%
          # , image = spec_image(path=ImageVector, width = 0.5,height = 0.5,res=15)) %>%
          column_spec(column=3, width = "1.5in") %>%
            scroll_box(width = "100%", height = "200px"))}
    cat("  \n")
}
```
