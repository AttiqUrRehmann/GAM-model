library(tidyverse)
library(mgcv)
library(readxl)
library(reshape2)
library(data.table)
library(rWBclimate)
library(countrycode)
library(gratia)
library(mgcViz)

# the particulate matter, which pm2.5 and pm10 is not used so you can comment those codes. 
url.pm2.5 <- 'https://edgar.jrc.ec.europa.eu/download.php?edgar_dst=227345'


temp <- tempfile()
td<- tempdir()
download.file(url.pm2.5,temp,
              mode = "wb")

file_names <- unzip(temp, exdir = td)


pm.2.5 <- as.data.frame(read_excel(file_names[2], sheet = 3, col_names = T,
                                   range = cell_cols("D:AX"), skip = 9)) 

reshape.pm2.5 <- melt(pm.2.5, id.vars = "Name", value.name = "PM2.5")

names(reshape.pm2.5)[1:2] <- c("country", "year")

data.pm2.5 <- reshape.pm2.5 %>% arrange(country, year)



####################### Cleaning data for particulate matter 10

url.pm10 <- 'https://edgar.jrc.ec.europa.eu/download.php?edgar_dst=227346'


download.file(url.pm10,temp,
              mode = "wb")

file_names <- unzip(temp, exdir = td)


pm.10 <- as.data.frame(read_excel(file_names[2], sheet = 3, col_names = T,
                                  range = cell_cols("D:AX"), skip = 9)) 

reshape.pm10 <- melt(pm.10, id.vars = "Name", value.name = "PM10")

names(reshape.pm10)[1:2] <- c("country", "year")

data.pm10 <- reshape.pm10 %>% arrange(country, year)

################### Black carbon 

url.bc <- 'https://edgar.jrc.ec.europa.eu/download.php?edgar_dst=227339'


download.file(url.bc,temp,
              mode = "wb")

file_names <- unzip(temp, exdir = td)


bc <- as.data.frame(read_excel(file_names[2], sheet = 3, col_names = T,
                               range = cell_cols("D:AX"), skip = 9)) 

reshape.bc <- melt(bc, id.vars = "Name", value.name = "BC")

names(reshape.bc)[1:2] <- c("country", "year")

data.bc <- reshape.bc %>% arrange(country, year)



####################################################
###########   Global warming potential index
####################################################


################# co2
url.co2 <- 'https://edgar.jrc.ec.europa.eu/download.php?edgar_dst=188376'


download.file(url.co2,temp,
              mode = "wb")

file_names <- unzip(temp, exdir = td)


co2 <- as.data.frame(read_excel(file_names[1], sheet = 3, col_names = T,
                                range = cell_cols("D:AX"), skip = 9, na="")) 

reshape.co2 <- melt(co2, id.vars = "Name", value.name = "co2")

names(reshape.co2)[1:2] <- c("country", "year")

data.co2 <- reshape.co2 %>% arrange(country, year) %>% 
  setDT() %>% 
  mutate(year= as.numeric(as.character(
    year)),
    joint:=year) %>%  
  data.table() %>%
  setkey(country,joint)


################ CH4

url.ch4 <- 'https://edgar.jrc.ec.europa.eu/download.php?edgar_dst=188375'


download.file(url.ch4,temp,
              mode = "wb")

file_names <- unzip(temp, exdir = td)


ch4 <- as.data.frame(read_excel(file_names[1], sheet = 3, col_names = T,
                                range = cell_cols("D:AX"), skip = 9, na="")) 

reshape.ch4 <- melt(ch4, id.vars = "Name", value.name = "ch4")

names(reshape.ch4)[1:2] <- c("country", "year")

data.ch4 <- reshape.ch4 %>% arrange(country, year) %>% 
  setDT() %>% 
  mutate(year= as.numeric(as.character(year)),
         joint:=year) %>% 
  data.table() %>%
  setkey(country,joint)

#data.ch4 <- data.ch4[-c(1:2)]

################ N2O

url.n2o <- 'https://edgar.jrc.ec.europa.eu/download.php?edgar_dst=188378'


download.file(url.n2o,temp,
              mode = "wb")

file_names <- unzip(temp, exdir = td)


n2o <- as.data.frame(read_excel(file_names[1], sheet = 3, col_names = T,
                                range = cell_cols("D:AX"), skip = 9)) 

reshape.n2o <- melt(n2o, id.vars = "Name", value.name = "n2o")

names(reshape.n2o)[1:2] <- c("country", "year")

data.n2o <- reshape.n2o %>% arrange(country, year) %>% 
  setDT() %>% 
  mutate(year= as.numeric(as.character(year)),
         joint:=year) %>% 
  data.table() %>%
  setkey(country,joint)
####################################################

data.pm2.5 <- reshape.pm2.5 %>% arrange(country, year)%>% 
  setDT() %>% 
  mutate(year= as.numeric(as.character(year)),
         joint:=year) %>% 
  data.table() %>%
  setkey(country,joint)


data.pm10 <- reshape.pm10 %>% arrange(country, year)%>% 
  setDT() %>% 
  mutate(year= as.numeric(as.character(year)),
         joint:=year) %>% 
  data.table() %>%
  setkey(country,joint)

data.bc <- reshape.bc %>% arrange(country, year)%>% 
  setDT() %>% 
  mutate(year= as.numeric(as.character(year)),
         joint:=year) %>% 
  data.table() %>%
  setkey(country,joint)


data <- data.co2[data.ch4, roll = "nearest"][data.n2o,
                roll="nearest"][data.pm2.5,
                roll="nearest"][data.pm10,
                roll="nearest"][data.bc, roll="nearest"]%>%
  select(-c("joint", "i.year", "i.year.1", "i.year.2",
            "i.year.3", "i.year.4")) %>%
  drop_na() %>%
  mutate(ch4=as.numeric(ch4), n2o = as.numeric(n2o),gwp = ch4*34+n2o*298+co2)

coord <- read.csv("C:\\Users\\HP\\Desktop\\DATA\\countries.csv", header = T)

coord1 <- coord %>%
  rename(name=country,country=name) %>%
  mutate_if(is.factor, as.character) 

anth_gases <- inner_join(data, coord1, by="country") %>%
  select(-c("co2", "ch4", "n2o", "name")) %>%
  mutate(country = as.factor(country)) %>%
  mutate(Lgwp =log(gwp),
         Lpm2.5=log(as.numeric(PM2.5)),
         Lpm10=log(as.numeric(PM10)), Lbc = log(BC)) %>%
  mutate_at(c("latitude", "longitude"), ~(scale(.) %>% as.vector))


##############################################
####              Temperature
##############################################

all.country <- c(
  Africa_country, 
  Asia_country, 
  Eur_country, 
  NoAm_country, 
  Oceana_country, 
  SoAm_country)

all.country <- all.country[all.country!="UMI"]
#hist.temp.ac <- get_historical_temp(all.country, "year")
load("C:/Users/HP/Desktop/DATA/Climate research/wtemp.RData")

hist_tempf <- hist.temp.ac %>% setDT() %>% 
  mutate(country = countrycode(locator,"iso3c", "country.name")) %>% 
  mutate_if(is.character, as.factor) %>%
  drop_na() %>%
  mutate(syear = scale(year), Temp = scale(data)) %>%
  data.table()

hist_tempff <-  hist_tempf %>% group_by(country) %>% filter(data > -20)

hist.temp <- hist.temp.ac %>% setDT() %>% 
  mutate(year= as.numeric(as.character(year)),
         joint:=year, 
         country = countrycode(locator,"iso3c", "country.name")) %>% 
  filter(year >1969) %>% drop_na() %>%
  data.table() %>%
  setkey(country,joint)


####################################
###   Final Dataset
###################################


#remove Last three values before going to final data


data <- data.co2[data.ch4, roll = "nearest"][data.n2o, roll="nearest"][data.pm2.5,
                                                                       roll="nearest"][data.pm10,
                                                                                       roll="nearest"][data.bc,
                                                                                                       roll="nearest"][hist.temp, roll="nearest"]%>%
  select(-c("joint", "i.year", "i.year.1", "i.year.2","i.year.3","i.year.4", "i.year.5")) %>%
  drop_na() %>%
  mutate(ch4=as.numeric(ch4), n2o = as.numeric(n2o),gwp = ch4*34+n2o*298+co2) %>%
  rename(Temp = data) 

coord <- read.csv("C:\\Users\\HP\\Desktop\\DATA\\countries.csv", header = T)

coord1 <- coord %>%
  rename(name=country,country=name) %>%
  mutate_if(is.factor, as.character) 

final_data <- inner_join(data, coord1, by="country") %>%
  select(-c("co2", "ch4", "n2o", "name")) %>%
  mutate(country = as.factor(country)) %>%
  mutate(Lgwp =log(gwp),
         Lpm2.5=log(as.numeric(PM2.5)), Lpm10=log(as.numeric(PM10)), Lbc = log(BC)) %>%
  mutate_at(c("latitude", "longitude"), ~(scale(.) %>% as.vector))

# Univariate Temperature gam model

Temp_coord <- coord %>%
  rename(name=country,country=name)

Tempdta <- inner_join(hist_tempf, Temp_coord, by="country") %>%
  mutate_if(is.character, as.factor)

gam_modTemp <- bam(Temp~ s(year, k=40) +
                         s(longitude,latitude,k=15,bs="ds",
                           m=c(1,1))+
                         s(country, year, bs="fs"),
                       data = Tempdta, method = "fREML")

# Smooth plots 

load("C:/Users/HP/Desktop/DATA/resultsTemp.Rdata")
Temp <- getViz(gam_modTemp)

plot(sm(Temp,1)) + l_fitLine(colour="red", size=1.2) +
  l_ciLine(mul = 3, linetype = 2)+
  theme(text = element_text(size = 15),
        plot.subtitle = element_text(size = 15),
        axis.text = element_text(size = 15)) +
  labs(x= "Year", subtitle = "Temperature")
ggsave("Temp.png")

indv_Temp <- plot(sm(Temp,3))



indv_Temp$data$fit %>% group_by(id) %>%
  mutate(ustemp= ((y*sd(Tempdta$data))+mean(Tempdta$data))) %>%
  filter(id ==c("South Africa")) %>%
  ggplot(aes(x=x,y=ustemp, group=id, colour=id)) + 
  geom_line(size=1.2) + ylim(11, 18.2) +
  labs(x= "Time", y= "Temperature", subtitle = "South Africa") +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 15),
        text = element_text(size = 15),
        axis.text = element_text(size = 15))
ggsave("South Africa.png")


# Univariate GWP index gam model

gam_modLgwp <- bam(Lgwp~ s(year, k=20) + s(longitude,latitude,k=12,bs="ds",m=c(1,1))+
                     s(country, year, bs="fs"),data = anth_gases, method = "fREML")

(which(resid(gam_modLgwp) > 1 | resid(gam_modLgwp) < -1))
no_outlr <- anth_gases %>% slice(-c(2462, 2463, 3254, 3255))


gam_modLgwp_noutlr <- bam(Lgwp~ s(year, k=20) + s(longitude,latitude,k=12,
                                           bs="ds",m=c(1,1))+
      s(country, year, bs="fs"),data = no_outlr, method = "fREML")

load("C:/Users/HP/Desktop/DATA/resultsGWP.Rdata")

GWP <- getViz(gam_modLgwpno)

plot(sm(GWP,1)) + l_fitLine(colour="red", size=1.2) +
  l_ciLine(mul = 3, linetype = 2)+
  theme(text = element_text(size = 15),
        plot.subtitle = element_text(size = 15),
        axis.text = element_text(size = 15)) +
  labs(x= "Year", subtitle = "Log(GWP Index)")
ggsave("gwp.png")

indv_GWP <- plot(sm(GWP,3))


indv_GWP$data$fit %>% group_by(id) %>%
  filter(id ==c("Japan")) %>%
  ggplot(aes(x=x,y=ty, group=id, colour=id)) + 
  geom_line(size=1.2) + ylim(2.3, 3.7) +
  labs(x= "Time", y= "Log(GWP Index)", subtitle = "Japan") +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 15),
        text = element_text(size = 15),
        axis.text = element_text(size = 15))
ggsave("Japan_gwp.png")


# Univariate Black Carbon gam model

gam_modBC <- bam(Lbc ~ s(year, k=15) +
                   s(longitude,latitude,k=12,bs="ds",m=c(1,1))+
                   s(country, year, bs="fs"),data = anth_gases,
                 method = "fREML")

(which(resid(gam_modBC)>2.5 | resid(gam_modBC)< -3))

no_outlr <- anth_gases %>%
  slice(-c(3254, 3255, 4346))

gam_BC_noutlr <- bam(Lbc ~ s(year, k=15) +
                  s(longitude,latitude,k=8,bs="ds",m=c(1,1))+
                  s(country, year, bs="fs"),data = no_outlr,
                method = "fREML")

load("C:/Users/HP/Desktop/DATA/resultsBC.Rdata")

bc <- getViz(gam_BC_noutlr)

plot(sm(bc,1))+ l_fitLine(colour="red", size=1.2) +
  l_ciLine(mul = 3, linetype = 2)+
  theme(text = element_text(size = 15),
        plot.subtitle = element_text(size = 15),
        axis.text = element_text(size = 15)) +
  labs(x= "Year", subtitle = "Log(BC)")
ggsave("BC.png")


invd_bc <- plot(sm(bc, 3))


invd_bc$data$fit %>% group_by(id) %>%
  filter(id ==c("Brazil")) %>%
  ggplot(aes(x=x,y=ty, group=id, colour=id)) + 
  geom_line(size=1.2) + ylim(4.5, 7.5) +
  labs(x= "Time", y= "Log(BC)", subtitle = "Brazil") +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 15),
        text = element_text(size = 15),
        axis.text = element_text(size = 15))
ggsave("Brazil_uniBC.png")


