## Mandatory task in Econometrics. Scriot for R

install.packages("haven")
library(haven); library(tidyverse);library(MASS);library(ISLR)
seminar_econometric <- read_dta(file = "seminar_econometric.dta")
View(seminar_econometric)
seminar_econometric$femalef <- as.factor(seminar_econometric$female)

## Lager et enkelt boxplott til å begynne med bare for å få en enkel oversikt over datasettet. 

ggplot(data = seminar_econometric, aes(x = femalef, y = wage4)) +
  geom_boxplot() +
  xlab("Kjønn, Kvinne = 1, Mann = 0") +
  ylab("Timelønn USD") +
  theme_minimal() +
  ylim(0,75)



  
mean(seminar_econometric$female)

mean(seminar_econometric$wage4[seminar_econometric$female == 0], na.rm = TRUE)
sd(seminar_econometric$wage4[seminar_econometric$female == 0], na.rm = TRUE)
mean(seminar_econometric$wage4[seminar_econometric$female == 1], na.rm = TRUE)
sd(seminar_econometric$wage4[seminar_econometric$female == 1], na.rm = TRUE)

wagemale <- seminar_econometric$wage4[seminar_econometric$female == 0]
wagefemale <- seminar_econometric$wage4[seminar_econometric$female == 1]
wagefemale <- wagefemale[!is.na(wagefemale)]
wagemale <- wagemale[!is.na(wagemale)]


ggplot(data = seminar_econometric, aes(x = wage4, fill =femalef)) +
  geom_bar(binwidth = 5) +
  xlim(0,100) +
  xlab("Timelønn") +
  ylab("Antall") +
  theme_minimal()

ggplot(data = seminar_econometric, aes(x = wage4, fill = femalef)) +
  geom_bar(binwidth = 5, position = "fill") +
  xlim(0,150) +
  xlab("Timelønn") +
  ylab("Fordeling i %") +
  theme_minimal() 

## Filtrerer på ønsket variabler og trekker ut lønn som en vektor. For så å fjerne NA
femalewage2kid <- seminar_econometric %>%
  filter(ownchild >= 2) %>%
  filter(female == 1) %>%
  pull(wage4) %>%
  na.omit()
  
malewage2kid <- seminar_econometric %>%
  filter(ownchild >= 2) %>%
  filter(female == 0) %>%
  pull(wage4) %>%
  na.omit()

femalewagekid <- seminar_econometric %>%
  filter(ownchild <2) %>%
  filter(female == 1) %>%
  pull(wage4) %>%
  na.omit()

malewagekid <- seminar_econometric %>%
  filter(ownchild < 2) %>%
  filter(female == 0) %>%
  pull(wage4) %>%
  na.omit()

testmale <- seminar_econometric %>%
  filter(female == 0) %>%
  pull(wage4) %>%
  na.omit()

testfemale <- seminar_econometric %>%
  filter(female == 1) %>%
  pull(wage4) %>%
  na.omit()

## Prøve å lage en rekke boxplot som viser ulike utdanningsnivå via facet wrap

ggplot(data = seminar_econometric, aes(x = femalef, y = wage4)) +
  geom_boxplot() +
  xlab("Kjønn, Kvinne = 1, Mann = 0") +
  ylab("Timelønn USD") +
  theme_minimal() +
  ylim(0,75) +
  facet_wrap(~ educ)
## Hovedtrekk i funn her er at kvinner sin medianlønn er på nivå med menn som ligger et nivå lavere. 

## Lage boxplott hvor mange timer kvinner og menn jobber etter utdanningsnivå
ggplot(data = seminar_econometric, aes(x = femalef, y = hourslw)) +
  geom_boxplot() +
  xlab("Kjønn, Kvinne = 1, Mann = 0") +
  ylab("Timelønn USD") +
  theme_minimal() +
  ylim(30,50) +
  facet_wrap(~ educ)
## Interessant funn. Median er lik. Men fra utdanningsnivå 2 og oppover er menn normalfordelt på median. Mens kvinner ligger
##underkant. På høyeste utdanningsnivå ligger menn i overkant. Kan være noe å gå mer i dybden på.

## Se gjennomsnitt for hvor mange jobber de har. NB: 0 = 1 jobb. 

mean(seminar_econometric %>%
  filter(femalef == 1) %>%
  pull(multjob) %>%
  na.omit())
## Kvinner har i snitt 1.0518 jobber

mean(seminar_econometric %>%
       filter(femalef == 0) %>%
       pull(multjob) %>%
       na.omit())
## Menn har i snitt 1.04456 jobber.


## Ser på ulikheter i antall jobber etter utdanningsnivå og kjønn. Få laget en egen function og en grafisk?
mean(seminar_econometric %>%
       filter(femalef == 1) %>%
       filter(educ == 5) %>%
       pull(multjob) %>%
       na.omit())

## Se på ulikheten i antall arbeidstimer

femalewrkh <- seminar_econometric %>%
  filter(female == 1) %>%
  pull(hourslw) %>%
  na.omit

maleworkh <- seminar_econometric %>%
  filter(female == 0) %>%
  pull(hourslw) %>%
  na.omit

t.test(maleworkh, femalewrkh)

##Ulikheten er også tilstede her. Kvinner har med 95% signifikans 10% mindre arbeidstid. Noe som bidrar til å holde lønnen
## nede ytterligere. Men noe av dette kan være frivillig. 



## Prøve å lage scatterplot. X-aksen alder/erfaring. Y-aksen timelønn (tot lønn?) og dele opp på kjønn.
## Denne ser ikke bra ut. 

ggplot(data = seminar_econometric) +
  geom_point(mapping = aes(x = age, y = wage4, color = femalef)) +
  geom_smooth(mapping = aes(x = age, y = wage4, linetype = femalef)) +
  ylim(0,100)

## Prøver med en heatmap
## Inspirasjon til Density plot istedenfor histogram https://wytham.rbind.io/post/scraping-nih-pis-with-rvest/
## Kanskje lage to tibbles. En for kvinner og en for menn. Enklere å lage overlay?

## Lage regresjonsmodell på alder og lønn.

malematrix <- seminar_econometric %>%
     filter(female == 0)
femalematrix <- seminar_econometric %>%
     filter(female == 1)

femalelm <- lm(wage4 ~ age, data=femalematrix)
print(femalelm)
malelm <- lm(wage4 ~ age, data=malematrix)
print(malelm)


## FUNN: kryssningspunktet er høyere til damer. Men Betta1(koeffisienten) er høyere til menn. Så menn går forbi damer i denne 
## denne modellen før de når arbeidsfør alder. Brattere stigning for menn.
sort(unique(seminar_econometric$age))
table(seminar_econometric$age)

summary(femalelm)

par(mfrow=c(2,2))
plot(femalelm)
mean(femalelm$residuals)

par(mfrow=c(2,2))
plot(malelm)
mean(malelm$residuals)

## Ser fra graf at en kan akseptere forutsetningen om homoskedacity. http://r-statistics.co/Assumptions-of-Linear-Regression.html

## PRøver å plotte med ggplot. Kan kanskje lage flere ablines for ulik alder
ggplot(data = femalematrix) +
  geom_point(mapping = aes(x = age, y = wage4)) +
  abline(femalelm) 
