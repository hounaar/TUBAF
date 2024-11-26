/* https://de.wikipedia.org/wiki/Liste_der_Kurfürsten%2C_Herzöge_und_Könige_von_Sachsen */

geherscht('Rudolf I.','NA', 1356, 1356).
geherscht('Rudolf II.','NA', 1356,1370).
geherscht('Wenzel','NA', 1370,1388).
geherscht('Rudolf III.','NA', 1388,1419).
geherscht('Albrecht III.', '„der Arme“',1419,1423).
geherscht('Friedrich I.', '„der Streitbare“',1423,1428).
geherscht('Friedrich II.', '„der Sanftmütige“',1428,1464).
geherscht('Ernst','NA', 1464,1486).
geherscht('Friedrich III.', '„der Weise“',1486,1525).
geherscht('Johann ', '„der Beständige“',1525,1532).
geherscht('Johann Friedrich ', '„der Großmütige“',1532,1547).
geherscht('Albrecht ', '„der Beherzte“',1485,1500).
geherscht('Georg ', '„der Bärtige“',1500,1539).
geherscht('Heinrich ', 'der Fromme“',1539,1541).
geherscht('Moritz','NA', 1541,1553).
geherscht('Moritz','NA', 1547,1553).
geherscht('August','NA', 1553,1586).
geherscht('Christian I.','NA', 1586,1591).
geherscht('Christian II.','NA', 1591,1611).
geherscht('Johann Georg I.','NA', 1611,1656).
geherscht('Johann Georg II.','NA', 1656,1680).
geherscht('Johann Georg III.','NA', 1680,1691).
geherscht('Johann Georg IV.','NA', 1691,1694).
geherscht('Friedrich August I.', '„der Starke“',1694,1733).
geherscht('Friedrich August II.','NA', 1733,1763).
geherscht('Friedrich Christian','NA', 1763, 1763).
geherscht('Friedrich August III.', '„der Gerechte“',1763,1806).
geherscht('Friedrich August I.', '„der Gerechte“',1806,1827).
geherscht('Anton ', '„der Gütige“',1827,1836).
geherscht('Friedrich August II.','NA', 1836,1854).
geherscht('Johann','NA', 1854,1873).
geherscht('Albert','NA', 1873,1902).
geherscht('Georg','NA', 1902,1904).
geherscht('Friedrich August III.','NA', 1904,1918).

werWarFürst(Fürst, Jahr) :-
    geherscht(Fürst, _, A, B),
    Jahr >= A,
    Jahr =< B.


