/* Quelle: https://de.wikipedia.org/wiki/Kohle/Tabellen_und_Grafiken#cite_ref-BGR_1-0 */
/* (land, [1970, 1980, 1990, 2000, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016]) */
braunkohle_förderung(deutschland, [369.0, 387.9, 356.5, 167.7, 180.4, 175.3, 169.9, 169.4, 176.5, 185.4, 183.0, 178.2, 178.1, 171.5]).
braunkohle_förderung(volksrepublik_china, [15.4, 24.3, 45.5, 47.7, 97.4, 115.0, 115.5, 125.3, 136.3, 145.0, 147.0, 145.0, 140.0, 140.0]).
braunkohle_förderung(russland, [116.2, 141.5, 138.5, 87.8, 71.3, 82.0, 68.2, 76.0, 77.6, 77.9, 73.0, 70.0, 73.2, 73.7]).
braunkohle_förderung(vereinigte_staaten, [5.4, 42.8, 79.9, 77.6, 71.2, 68.6, 65.8, 71.0, 73.6, 71.6, 70.1, 72.1, 64.9, 66.2]).
braunkohle_förderung(polen, [32.8, 36.9, 67.6, 59.5, 57.5, 59.6, 57.1, 56.5, 62.8, 64.3, 65.8, 63.9, 63.1, 60.2]).
braunkohle_förderung(indonesien, [0.0, 0.0, 0.0, 13.8, 28.0, 38.0, 38.2, 40.0, 51.3, 60.0, 65.0, 60.0, 60.0, 60.0]).
braunkohle_förderung(australien, [24.2, 32.9, 46.0, 67.3, 72.3, 72.4, 68.3, 68.8, 66.7, 69.1, 59.9, 58.0, 61.0, 59.7]).
braunkohle_förderung(türkei, [4.0, 14.5, 44.4, 60.9, 70.0, 81.5, 75.6, 70.0, 71.0, 70.0, 57.5, 60.0, 56.1, 56.9]).
braunkohle_förderung(indien, [3.5, 5.0, 14.1, 24.2, 32.8, 32.2, 34.1, 37.7, 42.3, 46.5, 44.3, 47.2, 43.8, 45.3]).
braunkohle_förderung(tschechien, [77.0, 90.1, 76.0, 50.3, 54.5, 46.8, 45.6, 43.9, 46.8, 43.7, 40.6, 38.3, 38.3, 38.6]).
braunkohle_förderung(serbien, [13.5, 22.7, 36.9, 33.9, 29.8, 30.4, 38.3, 37.8, 41.1, 38.2, 40.3, 29.7, 37.7, 38.0]).
braunkohle_förderung(griechenland, [7.9, 23.2, 51.9, 63.9, 64.4, 65.7, 61.8, 53.6, 58.4, 62.4, 54.0, 50.4, 45.6, 32.3]).
braunkohle_förderung(bulgarien, [28.8, 29.9, 31.5, 26.3, 25.4, 26.2, 27.3, 27.1, 34.5, 31.0, 26.5, 31.3, 35.9, 31.2]).
braunkohle_förderung(rumänien, [14.2, 26.5, 33.7, 29.0, 35.5, 32.1, 28.4, 27.7, 32.9, 34.1, 24.7, 23.6, 25.5, 23.0]).
braunkohle_förderung(thailand, [0.4, 1.5, 12.4, 17.8, 18.2, 18.3, 17.6, 18.3, 21.3, 18.1, 18.1, 18.0, 15.2, 17.0]).
braunkohle_förderung(ungarn, [23.7, 22.6, 15.8, 14.0, 9.8, 9.4, 9.0, 9.0, 9.5, 9.3, 9.6, 9.6, 9.3, 9.2]).
braunkohle_förderung(kanada, [3.5, 6.0, 9.4, 11.2, 10.5, 9.9, 10.6, 10.3, 9.7, 9.5, 9.0, 8.5, 8.4, 9.0]).
braunkohle_förderung(kosovo, [3.1, 5.2, 8.5, 3.0, 6.7, 7.0, 7.9, 8.0, 8.2, 8.0, 8.2, 7.2, 8.2, 8.8]).
braunkohle_förderung(bosnien_und_herzegowina, [0, 0, 0, 0, 0, 0, 0, 0, 7.1, 7.0, 6.2, 6.2, 6.0, 7.3]).
braunkohle_förderung(nordkorea, [5.2, 10.0, 10.6, 7.2, 9.0, 9.0, 9.0, 7.0, 7.6, 7.0, 7.0, 7.0, 7.0, 7.0]).

spalten_kopf([1970,1980,1990,2000,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016]).

gleich_oder_höhere_förderung(Niedrig, Hoch) :-
    Niedrig =< Hoch.

manchmal_besser([X|_],[Y|_], [Jahr|_]) :-
    gleich_oder_höhere_förderung(X,Y),
    write(Jahr).

manchmal_besser([_|X], [_|Y], [_|Rumpf]) :-
    manchmal_besser(X, Y, Rumpf).

höhere_Förderung_pro_Jahr(Land1, Land2) :-
    braunkohle_förderung(Land1, Daten1),
    braunkohle_förderung(Land2, Daten2),
    Land1 \= Land2,
    spalten_kopf(Jahre),
    manchmal_besser(Daten1, Daten2, Jahre).
