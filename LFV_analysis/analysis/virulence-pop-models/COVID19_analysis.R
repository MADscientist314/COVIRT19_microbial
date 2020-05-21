require('ggplot2')
require('nlme')
#covid_us <- read.csv(url("http://covidtracking.com/api/states/daily.csv"))
covid_world <- read.csv(url("https://covid.ourworldindata.org/data/ecdc/full_data.csv"))
covid_us <- read.csv(url("https://github.com/nytimes/covid-19-data/raw/master/us-states.csv"))
covid_county <- read.csv(url("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv"))
us_density <- read.csv(url("https://github.com/camillol/cs424p3/raw/master/data/Population-Density%20By%20County.csv"))
state_population <- read.csv(url("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/state/detail/SCPRC-EST2019-18+POP-RES.csv"))

max_days = 12 # days used for fitting
max_plot = 50 # days used for plotting
w_list = c('China','South Korea','Italy','Spain','France','United States')
c_list = c('New York City','King','Orleans','Wayne','Los Angeles','Cook','Bergen')
s_list = c('New York','New Jersey','Massachusetts','Illinois','Connecticut','Michigan','Washington','Louisiana','California')
fips_list = c('New York County','King County','Orleans Parish','Wayne County','Los Angeles County','Cook County','Bergen County')
fit_days = c(11,10,10,8,8,8,8) # how many days per county to be used for early growth fit
group_CA = covid_us$state=='California'
# death_man = data.frame('2020-03-29',19,132,'US-CA')
# colnames(death_man) = c('date','days','death','location')

death_growth = covid_us[group_CA,c('date','deaths')]
colnames(death_growth) = c('date','death')
death_growth = death_growth[!is.na(death_growth$death) & death_growth$death>3,]
num_days = length(death_growth$date)
death_growth$days = seq(1,num_days)
#death_growth$death = death_growth[seq(num_days,1),]$death # Inverse order. US data are inverted
death_growth$location = 'US-CA'
#death_growth$death = c(4,4,5,5,6,11,13,18,20,24,27,40,53,65,88) # update by hand. Since no true update
#death_growth = rbind(death_growth,death_man)
total_growth = death_growth # Variable keeping all data, not just up to max_days (for plotting)
final_growth = death_growth # variable to get latest growth trends

# World fits
for (i_list in 1:length(w_list)){ 
  sel_group = covid_world$location==w_list[i_list]
  tmp_growth = covid_world[sel_group, c('date','total_deaths')]
  colnames(tmp_growth) = c('date','death')
  tmp_growth = tmp_growth[!is.na(tmp_growth$death)&tmp_growth$death>3,] # early deaths are not reliable
  tmp_growth = tmp_growth[seq(1,min(length(tmp_growth$date),max_plot)),] # only keep a certain # of points
  num_days = min(max_days,length(tmp_growth$date))
  full_days = length(tmp_growth$date)
  tmp_growth$days = seq(1,full_days)
  tmp_growth$location = w_list[i_list]
  death_growth = rbind(death_growth,tmp_growth[seq(1,num_days),]) # only keep same number of time point
  final_growth = rbind(final_growth,tmp_growth[seq(full_days-4,full_days),])
  total_growth = rbind(total_growth,tmp_growth)
}

fits <- nlsList(death ~ c*exp(days/t)  | location, data=death_growth, start =c(t=3,c=5))
tmp_fit = coef(fits)
total_growth$t = 0 #Initialize parameters
total_growth$c=0
w_list = c(w_list,'US-CA')
for (i_list in 1:length(w_list)){ 
  keep_ind = total_growth$location == w_list[i_list]
  total_growth[keep_ind,]$c = tmp_fit[w_list[i_list],]$c
  total_growth[keep_ind,]$t = tmp_fit[w_list[i_list],]$t
}
total_growth$fit = total_growth$c*exp(total_growth$days/total_growth$t)


fits <- nlsList(death ~ c*exp(days/t)  | location, data=final_growth, start =c(t=3,c=5))
tmp_final_fit = coef(fits)

# Lives saved by country
total_growth$lives = total_growth$fit - total_growth$death 
total_growth[total_growth$days==35,]

# County fits
for (i_list in 1:length(c_list)){ 
  sel_group = covid_county$county==c_list[i_list]
  tmp_growth = covid_county[sel_group, c('date','deaths')]
  colnames(tmp_growth) = c('date','death')
  tmp_growth = tmp_growth[!is.na(tmp_growth$death)&tmp_growth$death>5,] # early deaths are not reliable
  tmp_growth = tmp_growth[seq(1,min(length(tmp_growth$date),max_plot)),] # only keep a certain # of points
  num_days = min(fit_days[i_list],length(tmp_growth$date))
  full_days = length(tmp_growth$date)
  tmp_growth$days = seq(1,full_days)
  tmp_growth$location = c_list[i_list]
  tmp_growth$early = c(tmp_growth[seq(1,num_days),]$death,NA*seq(1,full_days-num_days)) # variable for growth fit
    tmp_growth$density = max(us_density[us_density$GCT_STUB.display.label == fips_list[i_list],]$Density.per.square.mile.of.land.area)
  if (i_list==1) {
    death_county_growth = tmp_growth
  } else {
    death_county_growth = rbind(death_county_growth,tmp_growth) # only keep same number of time point
  }
}

fits <- nlsList(early ~ c*exp(days/t)  | location, data=death_county_growth, start =c(t=2,c=6), na.action = na.exclude)
tmp_county_fit = coef(fits)
tmp_county_fit$density = aggregate(death_county_growth$density, list(death_county_growth$location), mean)$x
tmp_county_fit$location  = rownames(tmp_county_fit)
tmp_county_fit$doubling = log(2)*tmp_county_fit$t
tmp_county_fit = merge(tmp_county_fit,death_county_growth[death_county_growth$days==1,c('date','location')],by='location')

death_county_growth$t = 0 #Initialize parameters
death_county_growth$c=0
for (i_list in 1:length(c_list)){ 
  keep_ind = death_county_growth$location == c_list[i_list]
  keep_ind2 = tmp_county_fit$location == c_list[i_list]
  death_county_growth[keep_ind,]$c = tmp_county_fit[keep_ind2,]$c
  death_county_growth[keep_ind,]$t = tmp_county_fit[keep_ind2,]$t
}
death_county_growth$fit = death_county_growth$c*exp(death_county_growth$days/death_county_growth$t)
death_county_growth$life = death_county_growth$fit-death_county_growth$death
death_county_growth[death_county_growth$life<1,]$life = 1

# fit States
for (i_list in 1:length(s_list)){ 
  sel_group = covid_us$state==s_list[i_list]
  tmp_growth = covid_us[sel_group, c('date','deaths')]
  colnames(tmp_growth) = c('date','death')
  tmp_growth = tmp_growth[!is.na(tmp_growth$death)&tmp_growth$death>5,] # early deaths are not reliable
  tmp_growth = tmp_growth[seq(1,min(length(tmp_growth$date),max_plot)),] # only keep a certain # of points
  num_days = min(10,length(tmp_growth$date))
  full_days = length(tmp_growth$date)
  tmp_growth$days = seq(1,full_days)
  tmp_growth$location = s_list[i_list]
  tmp_growth$early = c(tmp_growth[seq(1,num_days),]$death,NA*seq(1,full_days-num_days)) # variable for growth fit
#  tmp_growth$density = max(us_density[us_density$GCT_STUB.display.label == fips_list[i_list],]$Density.per.square.mile.of.land.area)
  if (i_list==1) {
    death_state_growth = tmp_growth
  } else {
    death_state_growth = rbind(death_state_growth,tmp_growth) # only keep same number of time point
  }
}

fits <- nlsList(early ~ c*exp(days/t)  | location, data=death_state_growth, start =c(t=2,c=6), na.action = na.exclude)
tmp_state_fit = coef(fits)
#tmp_state_fit$density = aggregate(death_county_growth$density, list(death_county_growth$location), mean)$x
tmp_state_fit$location  = rownames(tmp_state_fit)
tmp_state_fit$doubling = log(2)*tmp_state_fit$t
tmp_state_fit = merge(tmp_state_fit,death_state_growth[death_state_growth$days==1,c('date','location')],by='location')

death_state_growth$t = 0 #Initialize parameters
death_state_growth$c=0
death_state_growth$ddeath=0

for (i_list in 1:length(s_list)){ 
  keep_ind = death_state_growth$location == s_list[i_list]
  keep_ind2 = tmp_state_fit$location == s_list[i_list]
  death_state_growth[keep_ind,]$c = tmp_state_fit[keep_ind2,]$c
  death_state_growth[keep_ind,]$t = tmp_state_fit[keep_ind2,]$t
  death_state_growth[keep_ind,]$ddeath = c(0,diff(death_state_growth[keep_ind,]$death))
}
death_state_growth$fit = death_state_growth$c*exp(death_state_growth$days/death_state_growth$t)
death_state_growth$life = death_state_growth$fit-death_state_growth$death
death_state_growth[death_state_growth$life<1,]$life = 1

merge(death_state_growth,state_population[,c("NAME","POPESTIMATE2019")],by.x="location",by.y="NAME")

# Print world growth
p<- ggplot(total_growth[total_growth$location!="US-CA",], aes(x=days, y=death, color=location)) +
  geom_point() +
 geom_line(aes(x=days,y=fit,color=location))+
  xlim(0,max_plot) +
  ylim(0,9000) +
  scale_y_continuous(trans='log2') 
print(p)

# Print growth by county
p<- ggplot(death_county_growth, aes(x=days, y=death, color=location)) +
  geom_point() +
  geom_line(aes(x=days,y=fit,color=location, linetype=location))+
  xlim(0,29) +
  scale_y_continuous(trans='log2') +
  xlab('Days starting when death toll > 4') +
  ylab('Number of reported COVID-19 deaths')
print(p)

p<- ggplot(death_county_growth, aes(x=days, y=life, color=location)) +
  geom_point() +
  xlim(10,25) +
  scale_y_continuous(trans='log2') 
print(p)

p <- ggplot(tmp_county_fit, aes(x=density, y=doubling, color=location, size=location)) +
  geom_point() +
  scale_x_continuous(trans='log2')+
  xlab('Density (people/square mile)') +
  ylab('Doubling time (days)')
print(p)

# Print growth by state
p<- ggplot(death_state_growth, aes(x=days, y=death, color=location)) +
  geom_point() +
  geom_line(aes(x=days,y=fit,color=location, linetype=location))+
  xlim(0,40) +
  scale_y_continuous(trans='log2') +
  xlab('Days starting when death toll > 4') +
  ylab('Number of reported COVID-19 deaths')
print(p)

p<- ggplot(death_state_growth, aes(x=days, y=life, color=location)) +
  geom_point() +
  xlim(10,25) +
  scale_y_continuous(trans='log2') 
print(p)


p<- ggplot(death_state_growth, aes(x=days, y=ddeath, color=location)) +
  geom_smooth() +
  # geom_line(aes(x=days,y=fit,color=location, linetype=location))+
  xlim(0,40) +
  ylim(0,100) +
  # scale_y_continuous(trans='log2') +
  xlab('Days starting when death toll > 4') +
  ylab('Number of daily reported COVID-19 deaths')
print(p)

# p <- ggplot(tmp_state_fit, aes(x=density, y=doubling, color=location, size=location)) +
#   geom_point() +
#   scale_x_continuous(trans='log2')+
#   xlab('Density (people/square mile)') +
#   ylab('Doubling time (days)')
# print(p)




