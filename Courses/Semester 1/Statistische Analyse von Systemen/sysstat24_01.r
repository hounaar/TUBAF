################################################################################
# R-Skript sysstat24_r01.r zur Übung "Statistische Analyse von Systemen" 
#####

## Das Symbol '#' ist in R das Kommentarzeichen: Alles, was in einer Zeile
## auf '#' folgt, wird beim Ausführen ignoriert.

## Führen Sie die folgenden Befehle aus, indem Sie in RStudio im Skript immer
## in eine Zeile gehen und dann Steuerung+Enter oder den Run-Button drücken.

## Hilfe in R
help(sum)       # Hilfe für vordefinierte Funktion 'sum'
?sum            # das Gleiche wie 'help(sum)'

## R als Taschenrechner
1+1       # Addieren
2*3       # Multiplizieren
3/2       # Dividieren
3^2       # Potenzieren
(1+2)*3   # Klammern setzen
1.3*2     # Der Punkt fungiert als Dezimalzeichen.
pi        # Die Konstante 3.141592...
pi/4      # Rechnen mit der Konstanten
1/0       # Inf = Infinity (unendlich)
0/0       # NaN = Not a Number: Platzhalter bzw. Fehlermeldung
pi[2]     # NA = Not Available: Platzhalter bzw. Fehlermeldung

## Variablen
a <- 2*3        # eine Variable a anlegen und gleichzeitig einen Wert zuweisen
a               # den Wert der Variablen a anzeigen lassen
a^2             # mit der Variable rechnen ...
b <- a^2        # ... und das Ergebnis einer weiteren Variablen zuweisen
Baum <- "Eiche" # Wert einer Variablen kann auch eine Zeichenkette ... 
Baum                 
w.wert <- TRUE  # ... oder ein Wahrheitswert (oder noch komplexer) sein
w.wert

## Funktionen
log(2.7182818) # der natürliche Logarithmus
log(16,2)      # der Logarithmus von 16 zur Basis 2
objects()      # alle vorhandenen Variablen und Objekte anzeigen

BspFunktion <- function(a,b) { # zwei Argumente 
  w <- 5*a+b                   # das, was diese Funktion immer mit ihren Argumenten macht
  return(w)                    # Rückgabewert
}
BspFunktion(1,0)
BspFunktion(2,2)

BspFunktion <- function(a=0,b=0) {  
  w <- 5*a+b
  return(w)                    
}
BspFunktion()
BspFunktion(1)                 # erstes Argument wird auf 1 gesetzt

## Schleifen
for(i in 1:10) print(i^2)
for( name in c("I", "want", "to", "go", "home!") ) {
  cat(name,"\n")
}
while(rnorm(1) < 1) print("hallo")

## Bedingungen
if(rnorm(1) > 0) {
  print("rnorm erzeugte einen positiven Wert.")
} else {
  print("rnorm erzeugte einen negativen Wert.")
} 

## Vektoren
x <- c(1, 2, 3, 4, 5) # 'c' kommt von concatenate.
x
y <- exp(x)-2         # 'exp' (= "e hoch") wird auf jedes Element von x angewendet.
y
y[2]                  # zweites Element von y
y[2] <- 1             # zweitem Element von y einen Wert zuweisen

rep(1,5)           # Wiederholung
1:5                # das Gleiche wie c(1, 2, 3, 4, 5)
seq(1,20,by=4)     # Folge mit Abstand by=4
seq(1,20,length=5) # Folge mit length=5 Elementen in gleichen Abständen

x <- 1:30/10
length(x)         # Anzahl der Elemente von x
x[1:10]           # die ersten zehn Elemente von x
x[c(1,10,15)]     # die Elemente an Position 1, 10 ,15
x[c(10,1,15)]     # die gleichen Element in anderer Reihenfolge
x[ x > 2.5 ]      # alle Elemente von x, die > 2.5 sind
x > 2.5           # Vektor von Wahrheitswerten
x[-(1:10)];       # alle Elemente außer den Elementen 1 bis 10	

## Matrizen
x <- c(1,2,3,4)
matrix(x,nrow=2,ncol=2) # Elemente spaltenweise angeordnet
matrix(x,2,2,byrow = T) # Elemente zeilenweise angeordnet
A <- matrix(1:15,3)
B <- matrix(1:3,3,5)
B

A + B         # elementweise Summe
A * B         # elementweise Multiplikation
A %*% t(B)    # Matrixprodukt, t(B) ist transponierte Matrix B

dim(B)  # Dimension (Anzahl der Zeilen bzw. Spalten)
NROW(B) # Zeilenanzahl
NCOL(B) # Spaltenanzahl
A[,1]   # erste Spalte
A[1,]   # erste Zeile
A[3,1]  # erstes Element der dritten Zeile

cbind(c(1,2), c(3,4))   # spaltenweise
rbind(c(1,2), c(3,4))   # zeilenweise

## Listen
course.info <- list(students = c("Sandra", "Karl", "Thomas", "Nadine"),
                    nr.of.exercises = 15, 
                    rooms = c("Seminarraum","PC-Pool"))
course.info$rooms        
course.info[[3]]
course.info$students[2]

## Zufallszahlen
rnorm(10)       # Generiert 10 Zufallszahlen zur N(0,1)-Verteilung (Standardnormalverteilung)
qnorm(0.975)    # 97.5 %-Quantil der Standardnormalverteilung
pnorm(0)        # Verteilungsfunktion 
dnorm(0)        # Wahrscheinlichkeitsdichtefunktion
# 'rnorm' ist dabei aus 'r' und 'norm' zusammengesetzt.
# 'r' steht für Zufallszahlenerzeugung, 'norm' ist der Name der Verteilung.
# Entsprechend steht 'q' für Quantil, 'p' für Verteilungsfunktion und 'd' für Dichtefunktion.
# Dieses Prinzip gilt auch für andere Verteilungen, z.B.: 
# 'unif' steht für Gleichverteilung -> 'runif' erzeugt Zufallszahlen zur Gleichverteilung
# 'exp' steht für Exponentialverteilung -> 'rexp' erzeugt Zufallszahlen zur Exponentialverteilung

## Statistiken
x <- rnorm(20)                        
y <- rnorm(20,mean=2,sd=2)            # 20 Zufallszahlen zur N(2,4)-Verteilung
mean(x)                               # empirischer Mittelwert
mean(y)
sd(x)                                 # empirische Standardabweichung
sd(y)
var(x)                                # empirische Varianz
var(y)
z <- 2*(0.5*x + sqrt(0.75)*rnorm(20))
cov(x,z)                              # empirische Kovarianz von x und z
cor(x,z)                              # empirischer Korrelationskoeffizient

## einfache Grafiken
hist(rnorm(100))       # Histogramm
plot(x,z)              # Streudiagramm
xx <- seq(-3,3,by=0.1)
plot(xx,dnorm(xx),type="l",main="Dichtefkt. der N(0,1)-Vert.",xlab="x",ylab="f(x)")

## Dateien
getwd()                              # aktuelles Arbeitsverzeichnis von R
setwd("C:/meinPfad")                 # setzt Arbeitsverzeichnis auf den Ordner "C:/meinPfad"
# wobei 'meinPfad' ein von Ihnen gewähltes Unterverzeichnis der Festplatte "C" ist  

A <- matrix(rnorm(20),4,5)
write.table(A,file="AMatrix.txt",row.names=FALSE,col.names=FALSE) # Speichern
AA <- read.table("AMatrix.txt")      # Einlesen
class(AA)                            # Typ 'data.frame'
AA <- as.matrix(AA)                  # Umwandeln in den Typ 'matrix'
class(AA)

save(course.info,file="ci.RData")    # Abspeichern im .RData-Format
remove(course.info)                  # Löschen des Objekts course.info
course.info                          # Anzeigen
load(file="ci.RData")                # Einlesen
course.info                          # Anzeigen


########################################
# Aufgabe 1
###

# In dieser Aufgabe betrachten wir als Grundgesamtheit eine normalverteilte Zufallsgröße
# mit Erwartungswert mu und Varianz sigma^2.
mu <- 5
sigma <- 2

n <- 100                             # Stichprobenumfang

X <- rnorm(n=n,mean=mu,sd=sigma)     # Erzeugung von n=100 Zufallszahlen zur Normalverteilung N(mu,sigma^2)
# mit Erwartungswert mu und Varianz sigma^2

mean(X)                              # empirischer Mittelwert (i. Allg. ungleich mu)
var(X)                               # empirische Varianz/Stichprobenstreuung (i. Allg. ungleich sigma^2)

# Im Folgenden wird eine Funktion 'getEmp' erklärt, die zunächst n Zufallszahlen zur N(mu,sigma^2)-Verteilung
# (also eine Stichprobe vom Umfang n) erzeugt und dann einen Vektor bestehend aus deren empirischen Mittelwert
# und deren empirischer Varianz zurückgibt.
# 'n', 'mu', 'sigma' und 'i' sind die Eingabeparameter dieser Funktion, 
# dabei ist 'i' eine Art Nummer der Stichprobe, die das Folgende vereinfacht.
getEmp <- function(n,mu,sigma,i=0) { 
  X <- rnorm(n=n,mean=mu,sd=sigma)   
  return(c(mean(X),var(X)))          
} 

# Wir wenden die Funktion 'getEmp' mit den oben gewählten Parametern an. (Da in der Funktionsdefinition für 'i'
# bereits ein Default-Wert vorgegeben wurde, müssen wir 'i' nicht unbedingt einen Wert zuweisen.)
# Im Funktionsaufruf 'getEmp(n=n,...)' steht das erste 'n' für den Eingabeparameter 'n' von 'getEmp',
# während das zweite 'n' die gleichnamige Variable ist, die wir oben mit 'n=100' festgelegt hatten.
getEmp(n=n,mu=mu,sigma=sigma)                   # empirischer Mittelwert und empirische Varianz für eine Stichprobe
getEmp(n=n,mu=mu,sigma=sigma)                   # das Gleiche für eine andere Stichprobe

# Wir wollen im Folgenden 'getEmp' mehrmals anwenden und Schreibarbeit sparen. Dazu könnte man z.B. 
# eine for-Schleife verwenden. Praktischer und auch schneller in R sind aber die 'apply'-Befehle. 
# 'sapply' wendet auf einen Vektor gewünschter Werte für ein Argument (hier Stichprobennummer 'i') die im
# zweiten Argument ('FUN') angegebene Funktion an, alle weiteren Eingabeparameter dieser Funktion werden
# nachfolgend angegeben.
sapply(1:10,FUN=getEmp,n=n,mu=mu,sigma=sigma) # Aufruf von getEmp für i=1:10, Rückgabe enthält in der
# ersten Zeile die empirischen Mittelwerte, in der zweiten die empirischen Varianzen

# Nach diesen Vorbereitungen werden im Folgenden jeweils 100 Stichproben zu den 
# Stichprobenumfängen n=10, n=100, n=1000 erzeugt und Mittelwerte und Varianzen geschätzt:
mv10 <- sapply(1:100,getEmp,n=10,mu=mu,sigma=sigma)
mv100 <- sapply(1:100,getEmp,n=100,mu=mu,sigma=sigma)
mv1000 <- sapply(1:100,getEmp,n=1000,mu=mu,sigma=sigma)

# Darstellung der Ergebnisse für die Mittelwerte
# Boxplot
boxplot(mv10[1,],mv100[1,],mv1000[1,],xlab="Stichprobenumfang",ylab="Empirischer Mittelwert",names=c("n=10","n=100","n=1000"))
abline(h=mu,col=2)
# mittlere quadratische Abweichung vom theoretischen Erwartungswert (mean squared error, MSE)
c(mean((mv10[1,]-mu)^2),mean((mv100[1,]-mu)^2),mean((mv1000[1,]-mu)^2)) 
# -> Um welchen Faktor verbessert sich der MSE jeweils ungefähr mit wachsendem n?

# Darstellung der Ergebnisse für die Varianzen
# Boxplot
boxplot(mv10[2,],mv100[2,],mv1000[2,],xlab="Stichprobenumfang",ylab="Empirische Varianz",names=c("n=10","n=100","n=1000"))
abline(h=sigma^2,col=2)
# mittlere quadratische Abweichung von der theoretischen Varianz (mean squared error, MSE)
c(mean((mv10[2,]-sigma^2)^2),mean((mv100[2,]-sigma^2)^2),mean((mv1000[2,]-sigma^2)^2)) 
# -> Um welchen Faktor verbessert sich der MSE jeweils ungefähr mit wachsendem n?


########################################
# Aufgabe 2
###

# In dieser Aufgabe betrachten wir eine Zeitreihe monatlicher CO2-werte:
co2                                  # die Daten (monatliche CO2-Werte)
?co2                                 # Metadaten zum Datensatz

# Mit Hilfe des 'plot'-Befehls lässt sich eine Darstellung als verbundene Zeitreihe erzeugen:
plot(co2)                            
# -> Welche zwei offensichtliche Beobachtungen lassen sich hier machen?

# Im Folgenden speichern wird den Datensatz in Form eines Vektors in der Variable 'Z':
Z <- c(co2)                     

# Für einen Vektor 'V' zeitlich aufeinander folgender Werte bestimmt die folgende Funktion
# die empirische Korrelation der Werte mit Zeitabstand 'm' (Monate):
getCorr <- function(V,m) {           
  n <- length(V)                     
  return(cor(V[1:(n-m)],V[(1+m):n]))
}

# wir wenden 'getCorr' auf den co2-Datensatz an:
getCorr(Z,1)                         # Korrelation der CO2-Werte im Zeitabstand ein Monat
getCorr(Z,2)                         # Korrelation der CO2-Werte im Zeitabstand zwei Monate
n <- length(Z)
plot(Z[1:(n-1)],Z[2:n],xlab="Werte",ylab="Werte einen Monat später")    # Streudiagramm, Abstand ein Monat
plot(Z[1:(n-2)],Z[3:n],xlab="Werte",ylab="Werte zwei Monate später")    # Streudiagramm, Abstand zwei Monate
# Die jeweils hohe Korrelation (Werte sehr nahe bei 1) ist hier nicht verwunderlich, 
# da es offensichtlich eine starke deterministische Abhängigkeit der Werte im Sinne eines 
# jährlichen Zuwachses (Trend) als auch einer saisonalen Schwankung innerhalb eines Jahres gibt.
# Daher ist es in der Zeitreihenanalyse üblich, zunächst den Trend und die Saison-
# komponente zu schätzen und dann von der Zeitreihe abzuziehen.

# Wir schätzen daher zunächst den Trend und die Saisonkomponente:
fit <- stl(co2, s.window="period")         

# Der folgende Plot zeigt die Daten, die Saisonkomponente, den Trend und die
# Restwerte = Daten - Trend - Saisonkomponente:
plot(fit)

# Die Frage ist nun, ob es nach Abzug von Trend und Saisonkomponente immer noch eine
# Abhängigkeit/Korrelation von Werten benachbarter Monate gibt. Wir betrachten daher
# im Folgenden nur noch die Zeitreihe dieser Restkomponenten:
Zrest <- c(fit$time.series[,"remainder"])
n <- length(Zrest)

# Zunächst betrachten wir direkt benachbarte Monate:
plot(Zrest[1:(n-1)],Zrest[2:n],xlab="Restwerte",ylab="Restwerte einen Monat später")    # Streudiagramm, Abstand ein Monat 
getCorr(Zrest,1)                           # Korrelation der Restwerte im Zeitabstand ein Monat

# Da die Korrelation nun nicht mehr so nahe bei 1 wie zuvor liegt, führen wir einen 
# statistischen Test auf Korrelation 0 durch:
t1 <- cor.test(Zrest[1:(n-1)],Zrest[2:n])  # Test auf Unkorreliertheit
t1                                         # Ausgabe des Tests
t1$p.value                                 # p-Wert
# Da der p-Wert offenbar deutlich kleiner als üblich alpha-Werte wie 0.05 oder 0.01 ist,
# ist der geschätzte Korrelationswert also signifkant von 0 verschieden.
# D.h., dass die Restwerte im Monatsabstand tatsächlich miteinander korreliert 
# (und damit nicht unabhängig) sind.

# Wir betrachten nun Restwerte im Zwei-Monats-Abstand:
plot(Zrest[1:(n-2)],Zrest[3:n],xlab="Restwerte",ylab="Restwerte zwei Monate später")    # Streudiagramm, Abstand zwei Monate 
getCorr(Zrest,2)                           # Korrelation der Restwerte im Zeitabstand zwei Monate
t2 <- cor.test(Zrest[1:(n-2)],Zrest[3:n])  # Test auf Unkorreliertheit
t2                                         # Ausgabe des Tests
t2$p.value                                 # p-Wert
# Für diesen Zeitabstand ist der p-Wert nun größer als 0.05, weswegen man also davon ausgehen
# kann, dass die Restwerte im Abstand von zwei Monaten nicht miteinander korreliert sind.

# Fazit: Auch nach Bereinigung der Trend- und Saisonkomponente gibt es offenbar eine
# signifikante Korrelation zwischen den CO2-Werten im zeitlichen Abstand von einem
# Monat, die bei einer Modellierung der Zeitreihe berücksichtigt werden müsste.

