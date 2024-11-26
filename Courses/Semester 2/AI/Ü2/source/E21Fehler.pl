/* startflughafen, zielflughafen, Fluggesellschaft
flugRoute(Dresden, Mallorca, Eurowings)
flugRoute(Dresden, Zürich, Swiss)
flugRoute(Dresden, Hurghada, Germania)
flugRoute(Dresden, Malaga, Eurowings)
flugRoute(Dresden, Frankfurt, Lufthansa)
flugRoute(Mallorca, Zürich, Vueling)
flugRoute(Mallorca, Frankfurt, Lufthansa)
flugRoute(Mallorca, Dresden, Eurowings)
flugRoute(Mallorca, Ibiza, Iberia)
flugRoute(Mallorca, Malaga, Ryanair)
betriebsdaten(Ryanair, flugzeuge(0, ))
betriebsdaten(Eurowings, flugzeuge(2, [flugzeug(Boeing, treibstoffVerbrauch(1.4))]))
betriebsdaten(Swiss, flugzeuge(2, [flugzeug(Boeing, treibstoffVerbrauch(2.3))]))
betriebsdaten(Germania, flugzeuge(1, [flugzeug(Airbus, treibstoffVerbrauch(0.9))]))
betriebsdaten(Lufthansa, flugzeuge(3, [flugzeug(Airbus, treibstoffVerbrauch(1.3))]))
betriebsdaten(Vueling, flugzeuge(2, [flugzeug(Dornier, treibstoffVerbrauch(1.8))]))
betriebsdaten(Iberia, flugzeuge(1, [flugzeug(Boeing, treibstoffVerbrauch(1.2))]))

return_Flug(X,Y) :- flugRoute(X,Y) flugRoute(Y,X)
/* Der Betrieb ist gefährdet wenn eine Flugroute existiert und keine Flugzeuge verfügbar sind */
betriebGefährdet(X) :- flugRoute(_,_,X), betriebsdaten(X, flugzeuge(Y, _)), istGleich(Y, 0)
/* Der gesamte TreibstoffVerbrauch errechnet sich aus dem Verbrauch aller Flugzeuge */
gesamtTreibstoffVerbrauch(Fluglinie, X) :- betriebsdaten(Fluglinie, flugzeuge(Y, [flugzeug(_, treibstoffVerbrauch(Z))|_])), X = Y * Z

