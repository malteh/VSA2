VSA2
====

Anpassen der Knotenanzahl:

 - In der Datei config/system.cfg in der Zeile ```{nodecount, 7}.``` den Wert anpassen.
 - Achtung: Im Ordner config muss ein Unterordner mit diesem Namen existieren und es müssen dort Informationen zu den Knoten enthalten sein. (siehe Beispiele)

Um das System auf einem Rechner zu starten:

 - Die Datei config/host.cfg entsprechend des Rechnernamens anpassen.
 - Dann die nodes.sh oder nodes.bat starten, Es öffnet sich eine Eshell.
 - Dort muss ```global:whereis_name(n0) ! wakeup.``` eingegeben werden. 

Um das System auf mehreren Rechnern zu starten:

 - Die Datei config/host.cfg entsprechend der Rechnernamen auf allen Rechnern anpassen/erweitern.
 - Aus dem Ordner config/ANZAHL die jeweiligen "Knoten" löschen, die nicht auf diesem Rechner gestartet werden sollen.
 - Dann die nodes.sh oder nodes.bat starten, Es öffnet sich eine Eshell.
 - Dort muss ```global:whereis_name(n0) ! wakeup.``` eingegeben werden. (n0 kann durch einen beliebigen, vorhandenen Knotennamen ersetzt werden)

