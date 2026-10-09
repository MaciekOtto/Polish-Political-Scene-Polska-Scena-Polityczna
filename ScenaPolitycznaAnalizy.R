library(readxl)
library(ggparliament)
library(tidyverse)
library(ggplot2)
install.packages("plotrix")
library(plotrix)
dane <- read_excel("cechy.xlsx")
summary(dane1)
dane1 <- dane[,-1]
dane_tusk<-dane1[,1:17]
dane_trz<-dane1[18:34]
dane_

summary(dane_trz)

dane_trz$trza_innowacyjny <- as.numeric(dane_trz$trza_innowacyjny)


osoby <- read_excel("ankietowani.xlsx")
summary(osoby)
osoby$płeć <- factor(osoby$płeć, levels = c(1,2),labels=c("Kobiety","Mężczyźni"))
plec_table <- table(osoby$płeć)

pie(plec_table, 
    main = "Rozkład Płci", 
    col = c("lightcoral","lightblue"), 
    labels = paste(names(plec_table), "\n", plec_table),
    border = "white")

osoby$wykształcenie <- factor(osoby$wykształcenie, levels = c(1,2,3,4),labels=c("Podstawowe","Zawodowe","Średnie","Wyższe"))
ggplot(osoby, aes(x = osoby$wykształcenie, fill = osoby$wykształcenie)) +
  geom_bar() +
  scale_fill_manual(values = c("Podstawowe" = "#4F81BD",
                               "Zawodowe" = "#C0504D",
                               "Średnie"   = "#9BBB59",
                               "Wyższe"    = "#8064A2")) +
  labs(title = "Rozkład Wykształcenia",
       x = "Poziom wykształcenia",
       y = "Liczba osób") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))


hist(osoby$wiek,
     breaks = seq(18, 93, by = 1), 
     col = "#C0504D",
     border = "white",
     main = "Histogram wieku",
     xlab = "Wiek",
     ylab = "Częstość",
     las = 1)



ggplot(osoby, aes(x = wiek)) +
  geom_histogram(binwidth = 1, fill = "#C0504D", color = "white") +  
  scale_x_continuous(breaks = seq(18, 93, by = 1)) +  
  labs(title = "Histogram wieku",
       x = "Wiek",
       y = "Częstość") +
  theme_minimal()

ggplot(osoby, aes(x = wiek)) +
  geom_histogram(binwidth = 30, fill = c("red","blue","green"), color = "white") + 
  labs(title = "Histogram wieku",
       x = "Wiek",
       y = "Częstość") +
  theme_minimal()


install.packages("DescTools")
library(DescTools)
library(psych)
dane0 <- read_excel("tablice.xlsx")
View(dane0)
xtabs(~ wiek + x22, data = dane0)
crosstabs <- xtabs(~ wiek + x22, data = dane0)
prop.table(crosstabs, 1)
wiersze <- 100 * prop.table(crosstabs, 1)
wiersze <- round(100 * prop.table(crosstabs, 1), 2)
wiersze


kolumny <- 100 * prop.table(crosstabs, 2)
kolumny <- round(100 * prop.table(crosstabs, 2), 2)
kolumny

wiersze
kolumny

tab <- table(dane0$wiek, dane0$x22)
tab

plot(wiersze, color = "red")
plot(kolumny, color = "green")

summary(tab) #chi-kwadrat

chisq.test(tab)
chi_wiek<-chisq.test(tab)
tab.expected <- chi_wiek$expected
tab.expected

wiek.table<-addmargins(tab)
wiek.table

total <- sum(tab)
tab.p <- tab/total *100
tab.p
wiek.table.p <- addmargins(tab.p)
kable(wiek.table.p)

tab-tab.expected
smoke.table.e <-addmargins(tab.expected)
kable(smoke.table.e)

install.packages("vcd")
library(vcd)
View(Chi(tab))
chi$obsertabchi$observed
chi$expected
chi$residuals
Phi(płeć, x21)
Phi(tab)
TschuprowT(tab)
ContCoef(tab)
CramerV(tab)
YuleY(tab)
YuleQ(tab)
Yule(tab)

# musimy sami zrobić korektę!!!

C <- ContCoef(tab)
#jeżeli liczba wierszy jest równa liczbie kolumn (w = k)
w = 3
k = 3
Cmax = sqrt((k-1)/k)
Cmax
Ckor = C/Cmax
Ckor

# jeżeli liczba wierszy jest różna od liczby kolumn (w!=k) 
w = 2
k = 3

Cmax = ((sqrt((k-1)/k)) + (sqrt((w-1)/w)))/2
Cmax
Ckor = C/Cmax
Ckor

library(psych)
library(readxl)
library(corrplot)
dane9 <- read_excel('pytania.xlsx')
dane9
dane9$trza_innowacyjny <- as.numeric(dane9$trza_innowacyjny)
dane9 <-data.frame(dane9)
summary(dane9)
View(dane9)
dane9_sums <-rowSums(dane9)
dane9_sums
vartotal <- var(dane9_sums)
vartotal

part1 <- data.frame(dane9[,c("x1","x3","x5","x7","x9","x11","x13","x15","x17","x19","x21","x23")])
part2 <- data.frame(dane9[,c("x2","x4","x6","x8","x10","x12","x14","x16","x18","x20","x22","x24")])

part1 <- data.frame(dane9[,c("x2","x4","x5","x8","x10","x11","x14","x16","x17","x20","x22","x23")])
part2 <- data.frame(dane9[,c("x1","x3","x6","x7","x9","x12","x13","x15","x18","x19","x21","x24")])

part1 <- data.frame(dane9[,c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12")])
part2 <- data.frame(dane9[,c("x13","x14","x15","x16","x17","x18","x19","x20","x21","x22","x23","x24")])

part1_sums <-rowSums(part1)

mean1_sum <- mean(part1_sums)
sum1_value <- sum(part1_sums)
sd1_sum <- sd(part1_sums)
var1_sum <- var(part1_sums)
alpha1_value <- alpha(part1,check.keys = T)

results1 <- data.frame(statystyka = c("średnia","suma","odch std","wariancja","alpha cronbacha"),wynik = c(mean1_sum,sum1_value,sd1_sum,var1_sum,alpha1_value$total$raw_alpha))

#druga polowka
part2_sums <-rowSums(part2)

mean2_sum <- mean(part2_sums)
sum2_value <- sum(part2_sums)
sd2_sum <- sd(part2_sums)
var2_sum <- var(part2_sums)
alpha2_value <- alpha(part2,check.keys = T)

results2 <- data.frame(statystyka =c("średnia","suma","odch std","wariancja","alpha cronbacha"),wynik = c(mean2_sum,sum2_value,sd2_sum,var2_sum,alpha2_value$total$raw_alpha))
#podsumowanie
print(results1)
print(results2)

cor_half <- cor(part1_sums,part2_sums)
cat("korelacja miedzy 1 a 2 polowa: ", cor_half)

#rzetelnosc polowkowa (wspolczynnik Spearmana-Browna)
r_sb<-(2*cor_half)/(1+cor_half)
cat("korelacja spearmana-browna: ",r_sb)
#wartosc bliska 1 wskazuje na wysoka rzetelnosc testu, co oznacza ze pytania w tescie sa spojne i dobrze mierza te sama ceche

#rzetelnosc polowkowa Guttmana
r_guttman <- (2*(vartotal-var1_sum-var2_sum))/vartotal
cat("rzetelnosc polowkowa guttmana: ",r_guttman)

#wykres gestosci dla obu polowek
library(ggplot2)

ggplot() +
  geom_density(aes(x=part1_sums),fill="blue",alpha=0.6)+
  geom_density(aes(x=part2_sums),fill="green",alpha=0.6)+
  labs(title="wyrkes gestosci wynikow z obu polowek testu",
       x="suma wydatkow", y="gestosc")+
  theme_minimal()

cor.plot(dane9)
cor(dane9)


library("readxl")
Politycy <- read_excel("zerojeden.xlsx")
Politycy
Politycy <-data.frame(Politycy)
Politycy
str(Politycy)

jednostki = Politycy[,1]
jednostki

df <- Politycy[,-1]
df
row.names(df)<-jednostki
df

install.packages("BiocManager")
BiocManager::install("qvalue")
library(qvalue)

install.packages("jaccard")
library(jaccard)

install.packages("vegan")
library(vegan)

install.packages("proxy")
library(proxy)

install.packages("tidyverse")
library(tidyverse)

###########################
vegdist(df,method = "jaccard") #odl jaccarda w pracy pokazywac macierz odl
Jaccard_podob <-1-vegdist(df,method="jaccard")
Jaccard_podob #wspolczynnik podobienstwa jaccarda

jaccard_odl <-vegdist(df,method = "jaccard") #odl jaccarda
jaccard_odl <- as.matrix(jaccard_odl)[1:9,1:9]
jaccard_odl
jaccard_odl <- round((jaccard_odl),digits=3)

jaccard_podob <- 1-jaccard_odl # wspolczynnik pdoobienstwa jaccarda
jaccard_podob <- round((1-jaccard_odl),digits=3)
jaccard_podob
#taka macierz w pracy, cala, macierz w postaci zero jedynkowej podstawa do tej analizy
install.packages("factoextra")
library(factoextra)

jaccard_odl_druga <- get_dist(df,method="binary",stand=FALSE) #inna funkcja
jaccard_odl_druga 

fviz_dist(jaccard_odl_druga)
fviz_dist(jaccard_odl_druga, order=FALSE)
fviz_dist(jaccard_odl_druga, order=FALSE,
          gradient=list(low="seashell",mid="pink",high="red"))
fviz_dist(jaccard_odl_druga, order=TRUE,
          gradient=list(low="ivory",mid="lightblue",high="midnightblue"))


library(reshape2)
jaccard_podob
jaccard_podob_long <-melt(jaccard_podob, varnames=c("Politycy.","Politycy"))
ggplot(jaccard_podob_long, aes(Politycy.,Politycy, fill=value))+
  geom_tile()+
  scale_fill_gradient2(low="white",high="blue",mid="pink",midpoint=0.5)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust =1))+
  labs(title = "macierz podobienstwa jaccarda", fill="podobienstwo")+
  coord_fixed()

#do projektu macierz cyfrowa plus jedna macierz wizualna jaccard + macierz sokal
library(proxy)
sm_podob <- simil(df,y=NULL, method=NULL,diag=FALSE,pairwise=FALSE,by_rows=TRUE, convert_distances = TRUE,auto_convert_data_frames = TRUE)

sm_podob
sm_podob <- as.matrix(sm_podob) [1:9,1:9]
diag(sm_podob)=1
sm_podob
##ta macierz tez w pracy
install.packages("plot.matrix")
library(plot.matrix)
install.packages("png")
library(png)

par(mar=c(9,10,3,7))
plot(jaccard_odl, digits=2,text_cell=list(cex=0.8),col=c("green","yellow","orange","red"),breaks = c(0,0.25,0.5,0.75,1),las=2,
     xlab ="",ylab ="",main="",cex.axis=0.8)
plot(jaccard_odl, col=rainbow,las=2,
     xlab = "",ylab = "",main="",cex.axis=0.8)
plot(jaccard_odl, col=c("red","green"),breaks = c(0,0.5,1),las=2,
     xlab = "",ylab = "",main="",cex.axis=0.8)
plot(jaccard_odl, col=c("pink","orange","magenta","red"),breaks = c(0,0.25,0.5,0.75,1),las=2,
     xlab = "",ylab = "",main="",cex.axis=0.8)
plot(jaccard_odl, digits=2,text.cell=list(cex=0.8),col=c("red","green"),breaks = c(0,0.5,1),las=2,
     xlab = "",ylab = "",main="",cex.axis=0.8)
par(mar=c(0,0,0,0))

install.packages("cluster")
library(cluster)
install.packages("dendextend")
library(dendextend)

#metody average complete

odl <- dist(df,method="Jaccard")
dendJ <-hclust(odl,method="average")
View(dendJ)
dendJ$height
plot(dendJ,main="",
     xlab="Politycy",ylab="Poziom uwiązania",col="black",cex=0.8,)
grupy<-cutree(dendJ, k=3)
grupy
rect.hclust(dendJ,k=3,border=c("violetred2","navy","tomato"))

#to w pracy ktory jest najblizej idealnego/najdalej wzgledem analizowanego tematu

#zbior drugi do zrobienia zmienne mieszane ilosciowe i jakosciowe
gower<-read_excel("mieszane.xlsx")
gower <-data.frame(gower)
str(gower)
jednostki = gower[,1]
df1<-gower[,-1]
row.names(df1)<-jednostki

df1$x2 <-factor(x=df1$x2, levels = c("TAK","NIE"))
df1$x4 <-factor(x=df1$x4, levels = c("TAK","NIE"))
df1$x8 <-factor(x=df1$x8, levels = c("TAK","NIE"))
View(df1)
#zakodowac tylko cechy jakosciowe

library(cluster)
daisy(df1,metric=c("gower"))
gower_odl <-daisy(df1,metric=c("gower"))
gower_odl
gower_odl <-as.matrix(gower_odl)[1:9,1:9]
gower_odl<-round((gower_odl),digits=3)
gower_odl

gdm_dist <- dist(df1, method = "euclidean")  # Używamy metody euklidesowej
View(gdm_dist)

# Wyświetlenie macierzy GDM
gdm_matrix <- as.matrix(gdm_dist)
print(gdm_matrix)

library(cluster)
library(dendextend)
library(ggplot2)
library(proxy)

odl<-dist(df1,method="Gower")
dendG<-hclust(odl,method="complete")
dendG$height
plot(dendG,main="",xlab="Politycy",ylab="Poziom wiązania",col="black",cex=0.8,ylim=c(0,max(dendG$height)))
grupy<-cutree(dendG,k=2)
grupy
rect.hclust(dendG,k=2,border=c("navy","tomato"))





library(readxl)
wszystkieherbaty <-read_excel("pytania.xlsx")
dane1<-wszystkieherbaty
#dane1<-dane1[,-1]
str(dane1)
dim(dane1)
View(dane1)

srednie<-colMeans(dane1)
srednie

macierz<-matrix(c(srednie),ncol=24)
macierz

dane<-t(macierz)
dane
colnames(dane)<-c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","x19","X20","X21","X22","x23","x24")
colnames(dane)<-c("srednia")
rownames(dane)<-c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","x19","X20","X21","X22","x23","x24")
#w pracy mozna pokazac tablice cala i tablice co dana zmienna x znaczy, np x1=intensywnosc
dane

install.packages("DescTools")
library(DescTools)

CronbachAlpha(dane1)
#pokazuje spojnosc danych 0.76 to calkiem spojne dane - moga byc tam czynniki, ale na tym etapie niewiadomo co, cechy wykazuja pewne powiazania

library(corrplot)
install.packages("RColorBrewer")
library(RColorBrewer)

spearman = cor(dane1,method=c("spearman")) #method spearman,kendall,pearson
spearman

kor<-round(spearman,2)
kor

corrplot(kor,method='square',diag=F,order='hclust',addrect=3,rect.col='darkblue',rect.lwd = 3,tl.pos='d')
#z tego wiemy ze x2 nie ma mocy, to wizualnie widac - trzeba potwierdzic statystycznie
#pokazac korelacje w postaci macierzy danych i macierzy graficznej + macierz korelacji czastkowej (sprawdzic czy sie wybieli, straci intensywnosc w porownaniu z korelacja calk.)
#jesli straci to znaczy ze wchodzi w korelacje z innymi zmiennymi a o to chyba chodzi
corrplot(kor,method = 'ellipse',order='AOE',type='upper')
pairs(dane1)

library(psych)
#test bartletta
cortest.bartlett(dane1)
KMO(dane1)
dane1
#to dodajemy do raportu, zakladamy ze mamy probe losowa dla testu bartletta
#odzrucamy h0 mowiace o jednostkowej macierzy wspolczunnikow korelacji czyli odrzucajac mowimy ze mamy macierz wspl korelacji posiadajacej zaleznosci miedzy cechami
#potwierdza sie dla x2 ze ma male zaleznosci z innymi - wyrzucamy lub sprawdzamy czy moze przy wyrzuceniu innej msa wzrosnie dla pozostalych, i patrzymy na kmo czy sie polepsza

danebg <- dane1
danebg
df<-danebg

corrplot(kor,method='square',diag=F,order='hclust',addrect=3,rect.col='darkblue',rect.lwd = 3,tl.pos='d')
corrplot(kor,method = 'ellipse',order='AOE',type='upper')

library(factoextra)
#ustalanie liczby czynników 
vss(df)
#os pozioma - czynniki, pionowa - potencjal/ladunki, do momentu jak wykres jest wygiety to sugestia ze w czynnikach jest zasob do budowania zmiennej latentnej, plaski wykres to znaczy ze kolejne czynniki nie maja ladunkow juz

#osypiska
fa.parallel(df)

fa.parallel(df,fa="fa",fm="pa",main="Scree plot") #macierz pearson, metoda principal axis
abline(h=1,col="green",lwd=2,lty=2)
fa.parallel(df,fa="fa",fm="ml",main="Scree plot") #macierz pearson, metoda najwiekszej wiarygodnosci
abline(h=1,col="green",lwd=2,lty=2)
fa.parallel(kor,fa="fa",fm="pa",main="Scree plot",n.obs = 452) #macierz spearmana, metoda principal axis
abline(h=1,col="green",lwd=2,lty=2)
fa.parallel(kor,fa="fa",fm="minres",main="Scree plot",n.obs = 452) #macierz spearmana, metoda minres
abline(h=1,col="green",lwd=2,lty=2)

###na chwile analiza glownych skladowych
pc0 <-principal(r=df,10,rotate = "none",cor=T)
pc0
pc1<-principal(r=df,2,rotate = "none",cor=T)
pc1
pc2<-principal(r=df,2,rotate = "varimax",cor=T)
pc2
fa.diagram(pc2)

pc3<-principal(r=kor,2,rotate = "none")
pc3
fa.diagram(pc3)#pca z macierza spearmana

pc4<-principal(r=kor1,2,rotate = "varimax")
pc4
fa.diagram(pc4)#pca z macierza spearmana i rotacja

pc5<-principal(r=df,cor=T,rotate = "oblimin",nfactors = 2)
pc5
fa.diagram(pc5)#pca z macierza df i rotacja ukosna
###koniec


###trzeba przejrzec w pracy te schematy i rozne metody i wybrac jedna do interpretacji jesli cos znajdziemy

pca<- prcomp(df,scale=T)
fviz_pca_biplot(pca)

###METODA NAJWIEKSZEJ WIARYGODNOSCI
ml0 <-fa(kor,nfactors = 2,rotate="none",fm="ml",residuals = T)
ml0

ml1 <-fa(kor,nfactors = 2,rotate="varimax",fm="ml",residuals = T)
ml1

fa.diagram(ml1)
ml2 <-fa(df,nfactors = 2,rotate="varimax",fm="ml",residuals = T)
ml2
biplot(ml2)
fa.diagram(ml2)

#metoda principal axis
pa0 <-fa(kor,nfactors = 2,rotate="none",fm="pa",residuals = T)
pa0

pa1 <-fa(kor,nfactors = 2,rotate="varimax",fm="pa",residuals = T)
pa1

fa.diagram(pa1)

pa2 <-fa(df,nfactors = 2,rotate="varimax",fm="pa",residuals = T)
pa2
biplot(pa1)
fa.diagram(pa2)


###metoda minres
mm0 <-fa(kor,nfactors = 2,rotate="none",fm="minres")
mm0
fa.diagram(mm0)

mm1 <-fa(kor,nfactors = 2,rotate="varimax",fm="minres")
mm1

fa.diagram(mm1)

mm3 <-fa(kor,nfactors = 2,rotate="quartimax",fm="minres")
mm3
fa.diagram(mm3)

mm4 <-fa(kor,nfactors = 2,rotate="equamax",fm="minres")
mm4

fa.diagram(mm4)


mm3 <-fa(kor,nfactors = 2,rotate="quartimax",fm="minres")
mm3
fa.diagram(mm3)

mm4 <-fa(kor,nfactors = 2,rotate="equamax",fm="minres")
mm4

fa.diagram(mm4)








library(factoextra)
housetasks
dim(housetasks)
srednie <- read_excel("srednie.xlsx")
srednie <- srednie[,-1]
tablica_danych <- as.matrix(srednie)
tablica_danych    # 17x 9 chcemy zwizualizować mape korespondencji
summary(tablica_danych)
View(tablica_danych)

suma <- addmargins(tablica_danych)
suma   

# liczymy udziały względne - macierz P
udziały_względne <- prop.table(tablica_danych)
udziały_względne

sumy_względne <- addmargins(udziały_względne)
sumy_względne     # kolumny sum - przecietny profil kolumnowy i wierszowy

# to jest potrzebne do svd
# dwie cechy - jedna 13 wariantów druga 4

# warto profile narysować

profil_wierszowy_wzg <- prop.table(tablica_danych, margin = 1)
profil_wierszowy_wzg

profil_wierszowy_wzg_suma <- addmargins(profil_wierszowy_wzg,
                                        margin = 2, FUN = sum)

profil_wierszowy_wzg_suma

profil_kolumnowy_wzg <- prop.table(tablica_danych, margin = 2)
profil_kolumnowy_wzg
profil_kolumnnowy_wzg_suma <- addmargins(profil_kolumnowy_wzg,
                                         margin = 1, FUN = sum)
profil_kolumnnowy_wzg_suma

# Wyznaczanie profili polityków
profil_politikow <- profil_wierszowy_wzg - profil_kolumnnowy_wzg_suma[1,]
profil_politikow

#Wykres liniowy profili polityków

library(readxl)
library(ggplot2)

# Wczytanie danych
srednie <- read_excel("srednie.xlsx")
srednie <- srednie[,-1]
tablica_danych <- as.matrix(srednie)
tablica_danych
# Liczenie udziałów względnych
udziały_względne <- prop.table(tablica_danych)

# Wyznaczanie profili polityków
profil_wierszowy_wzg <- prop.table(tablica_danych, margin = 1)
profil_kolumnowy_wzg <- prop.table(tablica_danych, margin = 2)
profil_politikow <- profil_wierszowy_wzg - profil_kolumnowy_wzg

# Konwersja profilu polityków do ramki danych
profil_politikow_df <- as.data.frame(profil_politikow)
profil_politikow_df$Cechy <- rownames(profil_politikow_df)
profil_politikow_df <- tidyr::pivot_longer(profil_politikow_df, -Cechy, names_to = "Polityk", values_to = "Wartość")

# Tworzenie wykresu
ggplot(profil_politikow_df, aes(x = Cechy, y = Wartość, color = Polityk, group = Polityk)) +
  geom_line() +
  geom_point() +
  labs(title = "Profile Polityków", x = "Cechy", y = "Wartość") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = "Polityk")
################################################################################

# przeciętny kontra żona, przeciętny kontra mąż, przeciętny kontra razem
# my mamy zrobić profile polityków, tam gdzie cechy przewyższają profil przeciętny - cecha ważna dla polityka

#wychodzimy od macierzy krzyżowej (ma 9 polityków i 17 cech) - na teams, z każdej kolumny liczymy średnią, w tablicy są średnie wartości
# wyznaczamy profile - politycy podobni do siebie, różni - to będzie widać na wykresie

#porównujemy np żone/męża z sumy_względne - kolumna 'sum'

################################################################################

chi <- chisq.test(tablica_danych) # podać ile wynosi w pracy chi-kwadrat. tu: 1944,5, n = 1744
chi
chi$expected # wartości oczekiwane (teoretyczne) im próba większa tym lepiej, u nas powinno wyjść dobrze

View(chi)


# liczymy współczynniki kontyngencji np. czuprow
library(DescTools)

TschuprowT(tablica_danych)
ContCoef(tablica_danych)
CramerV(tablica_danych)
C <- ContCoef(tablica_danych)

# jeżeli liczba wierszy jest różna od liczby kolumn (w !=k)
w = 17
k = 9
Cmax = ((sqrt((k-1)/k)) + (sqrt((w-1)/w)))/2
Cmax
Ckor = C/Cmax
Ckor

################# ANALIZA KORESPONDENCJI ######################################

install.packages("ca")
library(ca)
library(ggplot2)

danek <- ca(tablica_danych, graph = FALSE)
danek
danek$sv      # wartości osobliwe
plot(danek)

ev <- get_eigenvalue(danek)
ev
inercja <- sum(ev[,1])
inercja  # suma wartości własnych czyli kolumny eigenvalue
# pierwsze dwa wymiary wyjaśniają ok 90% inercji - problem jest dobrze zobrazowany

# mass - kto dominuje w danej tablicy kontyngencji - tu pranie i żony
# w pracy mają być pokazane wartości własne i osobliwe (pierwiastek z wariancji)


# 1944.5 / 1744 = 1.11 <- miara Pearsona (inercja) 


fviz_screeplot(danek, addlabels = TRUE, ylim = c(0,50))
# kobiety gotują i piorą
# mężczyźni jeźdżą i naprawiają
# razem: ...

row <- get_ca_row(danek)
row
View(row)
View(danek)
danek$rowmass            # masy wierszowe, częstości brzegowe wierszy, średni profil kolumnowy
row$coord               # współrzędne wariantów cechy 1 (wiersze)
row$cos2                # cos2 dla wszystkich wymiarów
rowSums(row$cos2[,1:2])# jakość dla dwóch wymiarów     # tu wystarczy 90%, nie trzeba 3 wymiarów
row$contrib             # bezwłądność po wymiarach, suma = 1
row$inertia             # inercja dla wariantów cechy 1 (wiersze), wariancja
sum(row$inertia)
(row$inertia/sum(row$inertia))*100        # względna bezwładność, inercja, suma = 100%

# najniższą jakość ma 'official', 3 wymiar to prawdopodobnie sprawy urzędowe, 1 i 2 też nazwać

#jak dane warianty korelują z wymiarami
# wymiar 2 - przyjemność(wakacje) vs usługi/czynności domowe
# wymiar 3 - sprawy urzędowe


###### TO SAMO ROBIMY PO KOLUMNACH

col <- get_ca_col(danek)
col
View(col)
View(danek)
danek$colmass            # masy wierszowe, częstości brzegowe wierszy, średni profil kolumnowy
col$coord               # współrzędne wariantów cechy 1 (wiersze)
col$cos2                # cos2 dla wszystkich wymiarów
colSums(col$cos2[,1:8]) # jakość dla dwóch wymiarów     # tu wystarczy 90%, nie trzeba 3 wymiarów
col$contrib             # bezwłądność po wymiarach, suma = 1
col$inertia             # inercja dla wariantów cechy 1 (wiersze), wariancja
sum(col$inertia)
(col$inertia/sum(col$inertia))*100 

# na mapie zamazać to co wchodzi w 3 wymiar bo to psuje i nie pasuje

graphics.off()
install.packages("factoextra")
install.packages("ggplot2")
install.packages("ggrepel")

library(factoextra)
library(ggplot2)
library(ggrepel)


row.names(tablica_danych) <- c("UCZCIWY","WIARYGODNY","KOMPETENTNY","KONSEKWENTNY","CHARYZMATYCZNY","KOMUNIKATYWNY","KULTURALNY","LOJALNY",
                     "ODPOWIEDZIALNY","MEDIALNY","INNOWACYJNY","TOLERANCYJNY","DYPLOMATYCZNY","SZANOWANY","ELOKWENTNY","WPŁYWOWY","ATRAKCYJNY")
row.names(tablica_danych)<-NULL
danek
fviz_ca_biplot(danek, repel = TRUE)



fviz_ca_row(danek, col.row = "cos2",
            gradient.cols = c("#00AFBB","#E7B800","#FC4E77"),
            repel = TRUE)


library(vcd)
install.packages("ggpubr")
library(ggpubr)
library(gplots)
tablica_danych

dt <- as.table(tablica_danych)    # dane jako tablica
dt
balloonplot(dt, main = "", xlab = "", ylab = "",
            label = TRUE, show.margins = FALSE)


























