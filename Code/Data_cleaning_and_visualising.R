####Start####
####load libraries####
if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl, RColorBrewer)




rm(list=setdiff(ls(), c("")))


options(scipen=999)


####download datasets####
dfin <-  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/NHS_privatisation_treatments/f44c3d46df8645ce7d853df84646f13b5dbf8f04/Data/Provider_admitted.csv"))
dfout <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/NHS_privatisation_treatments/f44c3d46df8645ce7d853df84646f13b5dbf8f04/Data/Provider_non_admitted.csv"))

dfin <- dfin%>%dplyr::select(-X, -nhs)
dfout <- dfout%>%dplyr::select(-X, -nhs)

names(dfout)[names(dfout)=="Total.All"] <- "Total.nonadmitted"
names(dfin)[names(dfin)=="Total.All"] <- "Total.admitted"

names(dfout)[names(dfout)=="within18"] <- "within18.nonadmitted"
names(dfin)[names(dfin)=="within18"] <- "within18.admitted"

provider_data <- merge(dfin, dfout, by=c("month", "year","Provider.Org.Code", "Provider.Org.Name", "Sector", "Treatment.Function.Name", "Treatment.Function.Code"), all=T)
provider_data[is.na(provider_data)] <- 0

provider_data$Total.All <- as.numeric(provider_data$Total.nonadmitted)+as.numeric(provider_data$Total.admitted)

provider_data$within18_all <- (((as.numeric(provider_data$within18.admitted)*as.numeric(provider_data$Total.admitted))+(as.numeric(provider_data$within18.nonadmitted)*as.numeric(provider_data$Total.nonadmitted)))/(as.numeric(provider_data$Total.All)))*100




provider_data <- provider_data%>%mutate(monthno = ifelse(provider_data$month=="january", 1,
                                                         ifelse(provider_data$month=="february", 2,
                                                                ifelse(provider_data$month=="march", 3,
                                                                       ifelse(provider_data$month=="april", 4,
                                                                              ifelse(provider_data$month=="may", 5,
                                                                                     ifelse(provider_data$month=="june", 6,
                                                                                            ifelse(provider_data$month=="july", 7,
                                                                                                   ifelse(provider_data$month=="august", 8,
                                                                                                          ifelse(provider_data$month=="september", 9,
                                                                                                                 ifelse(provider_data$month=="october", 10,
                                                                                                                        ifelse(provider_data$month=="november", 11,
                                                                                                                               ifelse(provider_data$month=="december", 12,NA)))))))))))))

provider_data$yearno <- as.double(provider_data$year)-2011

provider_data$time <- provider_data$monthno+(12*provider_data$yearno)-3

check <- unique(provider_data[c("Sector", "Provider.Org.Name")])

provider_data$Sector[provider_data$Provider.Org.Name=="TETBURY HOSPITAL TRUST LTD"] <- "Independent"
provider_data$Sector[provider_data$Provider.Org.Name=="FOSCOTE COURT (BANBURY) TRUST LTD"] <- "Independent"




rm(list=setdiff(ls(), c("provider_data")))

####Qual datavis####

col_strip <- brewer.pal(11, "RdBu")

df4 <- aggregate(.~Sector+time, data=provider_data[c("Sector", "time", "Total.All")], sum)
df4 <- df4 %>%  pivot_wider(names_from = Sector, values_from = c(Total.All))
df4$outsourcing <- (df4$Independent/(df4$NHS+df4$Independent))*100

#df4 <-df4[which(df4$time<109),]
theme_strip <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  )

warmingstripes <- ggplot(df4,
                         aes(x = time, y = 1, fill = outsourcing))+
  geom_tile()+
  labs(x="Year", y="Independent treatments (%)", title = "")+
  scale_x_continuous(breaks=c(10,22,34,46,58,70,82,94, 106, 118, 130),
                     labels=c("2012","2013",  "2014", "2015", "2016","2017", "2018", "2019", "2020", "2021", "2022"))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip), name = "Independent Sector\nTreatments (%)")+
  guides(fill = guide_colorbar(barwidth = 1))+
  theme_strip+
  theme(text=element_text(size=10), legend.key.size = unit(0.9, "cm"))



my_breaks <- c(2, 3,4, 5, 6,7, 8, 9)


blog <- ggplot(data=df4, aes(x=time, y=outsourcing)) +
  
  # geom_line() +
  geom_area(fill="navyblue")+
  theme_minimal()+
  geom_vline(xintercept = 108, linetype="dashed")+
  annotate("text", x=105, y=9, label=" National\nCOVID-19 lockdown", angle=90, size=3)+
  theme(legend.position="bottom",legend.key.width = unit(2, 'cm'),legend.key.size = unit(0.5, "cm"))+
  labs(x="Year", y="Private Sector\nTreatments (%)", title = "")+
  scale_x_continuous(breaks=c(10,22,34,46,58,70,82,94, 106, 118, 130),
                     labels=c("2012","2013",  "2014", "2015", "2016","2017", "2018", "2019", "2020", "2021", "2022"))

df <- provider_data

df$Total.All <- as.numeric(df$Total.All)
df$Treatment.Function.Name <- gsub(" Service", "", df$Treatment.Function.Name)

df$Treatment.Function.Name[df$Treatment.Function.Name=="Trauma and Orthopaedic"] <- "Trauma & Orthopaedics"
df$Treatment.Function.Name[df$Treatment.Function.Name=="Neurosurgical"] <- "Neurosurgery"
df$Treatment.Function.Name[df$Treatment.Function.Name=="Elderly Medicine"] <- "Geriatric Medicine"
df$Treatment.Function.Name[df$Treatment.Function.Name=="Thoracic Medicine"] <- "Respiratory Medicine"
df$Treatment.Function.Name[df$Treatment.Function.Name=="General Medicine"] <- "General Internal Medicine"

df$Treatment.Function.Name[df$Treatment.Function.Name=="ENT"] <- "Ear Nose and Throat"
df$Treatment.Function.Name[df$Treatment.Function.Name=="Ear, Nose & Throat (ENT)"] <- "Ear Nose and Throat"

df <- aggregate(.~time+Treatment.Function.Name+Sector,  data=df[c("time","Treatment.Function.Name","Sector", "Total.All")], sum)

df <- df %>%  pivot_wider(names_from = Sector, values_from = c(Total.All))
df$outsourcing <- (df$Independent/(df$NHS+df$Independent))*100

df <- df %>% mutate(Treatment.Function.Name2 = (ifelse(Treatment.Function.Name=="Gastroenterology", "Gastroenterology",
                                                       ifelse(Treatment.Function.Name=="Ear Nose and Throat", "Ear Nose and Throat",
                                                              ifelse(Treatment.Function.Name=="Trauma & Orthopaedics", "Trauma & Orthopaedics",
                                                                     ifelse(Treatment.Function.Name=="General Surgery", "General Surgery",
                                                                            ifelse(Treatment.Function.Name=="Gynaecology", "Gynaecology",
                                                                                   ifelse(Treatment.Function.Name=="Ophthalmology", "Ophthalmology", NA))))))))


df <- df[- grep("Other", df$Treatment.Function.Name),]
df <- df[- grep("Total", df$Treatment.Function.Name),]


my_breaks <- c(0, 0.5, 1,2, 5, 10,20, 30)



plot2 <- ggplot(df[complete.cases(df$Treatment.Function.Name),], aes(x=time, y=reorder(Treatment.Function.Name,outsourcing), fill=outsourcing))+
  geom_tile(color="transparent" )+theme_minimal()+
  scale_fill_gradientn(colors = rev(col_strip), name = "Private Sector Treatments\n(%, logged colour scale)",trans = scales::pseudo_log_trans(sigma = 0.4, base=2), breaks=my_breaks, labels = my_breaks)+
  scale_x_continuous(breaks=c(10,22,34,46,58,70,82,94, 106, 118, 130),
                     labels=c("2012","2013",  "2014", "2015", "2016","2017", "2018", "2019", "2020", "2021", "2022"))+
  labs(x="Year", y="Treatment", fill = "NHS Treatments Outsourced\n(%)")+
  theme(axis.text.y = element_text(size=8) ,legend.key.width = unit(2, 'cm'),legend.key.size = unit(0.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),plot.caption = element_text(hjust=0,  size=8),
        legend.position = "bottom")




yes1 <- cowplot::plot_grid(NULL, blog,  ncol=2, rel_widths = c(0.07, 0.9))

yes <- cowplot::plot_grid(yes1, plot2, ncol=1)

ggsave(plot=yes, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NHS_privatisation_treatments/Plots/outsourcing_by_treatments.png",width=12, height=8, dpi=600)
ggsave(plot=blog, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NHS_privatisation_treatments/Plots/all_outsourcing.png",width=12, height=6, dpi=600)

