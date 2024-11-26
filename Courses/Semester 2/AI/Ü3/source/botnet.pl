/* https://en.wikipedia.org/wiki/Botnet 11-2018 */

/* Date created,Date dismantled,Name,Estimated no. of bots,Spam capacity (bn/day)*/
botnet('2003','','MaXiTE',1000,0).
botnet('2004 (Early)','','Bagle',230000,5.7).
botnet('','','Marina Botnet',6215000,92).
botnet('','','Torpig',180000,-1).
botnet('','','Storm',160000,3).
botnet('2006 (around)','2011 (March)','Rustock',150000,30).
botnet('','','Donbot',125000,0.8).
botnet('2007 (around)','','Cutwail',1500000,74).
botnet('2007','','Akbot',1300000,-1).
botnet('2007 (March)','2008 (November)','Srizbi',450000,60).
botnet('','','Lethic',260000,2).
botnet('','','Xarvester',10000,0.15).
botnet('2008 (around)','','Sality',1000000,-1).
botnet('2008 (around)','2009-Dec','Mariposa',12000000,-1).
botnet('2008 (November)','','Conficker',10500000,10).
botnet('2008 (November)','2010 (March)','Waledac',80000,1.5).
botnet('','','Maazben',50000,0.5).
botnet('','','Onewordsub',40000,1.8).
botnet('','','Gheg',30000,0.24).
botnet('','','Nucrypt',20000,5).
botnet('','','Wopla',20000,0.6).
botnet('2008 (around)','','Asprox',15000,-1).
botnet('','','Spamthru',12000,0.35).
botnet('2008 (around)','','Gumblar',-1,-1).
botnet('2009 (May)','November 2010 (not complete)','BredoLab',30000000, -1).
botnet('2009 (Around)','2012-07-19','Grum',560000,39.9).
botnet('','','Mega-D',509000,10).
botnet('','','Kraken',495000,9).
botnet('2009 (August)','','Festi',250000,2.25).
botnet('2010 (March)','','Vulcanbot',-1,-1).
botnet('2010 (January)','','LowSec',11000,0.5).
botnet('2010 (around)','','TDL4',4500000, -1).
botnet('','','Zeus',3600000, -1).
botnet('2010','(Several: 2011 2012)','Kelihos',300000,4).
botnet('2011 or earlier','2015-02','Ramnit',3000000, -1).
botnet('2013 (early)','2013','Zer0n3t',200,4).
botnet('2012 (Around)','','Chameleon',120000, -1).
botnet('2016 (August)','','Mirai',380000, -1).

starteÜberwachung(Botnet) :- write('Überwachung von: '), write(Botnet).

inaktiv(Botnet) :- 
    botnet(_, DeactiveDate, Botnet, _, _),
    DeactiveDate\=''.

/* ignoriere inaktive */
botnetÜberwachung(Botnet) :-
    inaktiv(Botnet),
    !,
    fail.

botnetÜberwachung(Botnet) :-
    starteÜberwachung(Botnet).