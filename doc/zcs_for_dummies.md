ZCS from Wilson/Holland für Dummies
============================

"step by step" Aufbau und Erklärung eines "Zeorth Level Classifier"- Systems.

Von André Freudenreich und Michael Bittorf.


 * Am Anfang werden zufällig N (400) "classifier" erstellt und in die "classifier" Liste (P) übertragen. Diese Liste bildet den Grundstock für die Population (P). Diese Liste hat immer eine feste Größe über die Dauer aller Versuche.
   Ein "classifier" ist eine "if then -> Aktion" und hat den Charakter einer Verhaltensregel so wie einer gewissen "Nützlichkeit".
   Diese "classifier" setzen sich zusammen aus dem Matching-string, der aus einer Variation von Informationen inklusive einem möglichen "wildcard" Wert von "#" besteht, einer zufällig zu geordneten Aktion und einer Gewichtung genannt "quality" bzw "strenght" (Dieser Wert wird initial auf einem definierten Wert gesetzt. Im Falle von Wilson's WOOD1 ist der Wert 20).


 * Der Agent ist start bereit:
   Die Sensoren des Agenten liefern einen kodierten sensorischen Input. 
   Nun findet ein Vergleich statt: Dabei wird beginnend mit der ersten bis zur letzten Postion des Input Strings mit der jeweiligen Postion des "matching" Strings eines jeden "classifiers" verglichen.
   Als Ausnahme gilt hier das "wildcard" Zeichen "#" das auf jedes beliebe Zeichen passt.
   Stimmen diese beiden Werte vollkommen überein, wird der betreffende "classifier" in die "matching" Liste (M) einsortiert. 


   * Mit einer fest vordefinierten Wahrscheinlichkeit wird ein genetischer Algorithmus (gA) über die "classifier" Liste ausgeführt.
   Wobei Wilson empfielt, nur "classifier", die schon mehrfach im "action set" waren, auszuwählen (damit diese die Möglichkeit erhalten, sich zu verbessern).
   Über die "classifier" in der "matching" Liste (P) findet eine Einordnung in eine Rangliste statt.
   Generell wird derjenige "classifier" ausgewählt, dessen "quality" am höchsten ist. 
   Spezieller wird eine Variation des "roulette wheel" Verfahrens angewandt:
   Dabei werden alle "classifier" in der "matching" Liste (P) mit ihrer jeweiligen "quality" in einer Liste angeordnet und zwar so, dass "classifier" mit einem sehr hohen "quality"  Wert proportional öfters vertreten sind.
   Auf diese Rangliste wird nun das Verfahren angewandt und zwei "classifier" werden ausgewählt
   Im ersten Schritt werden sie komplett geklont. So dass zwei Eltern und zwei "Kopie" "classifier" (Kinder) existieren (gleicher "matching String, Aktion, "quality"  Wert).
   Im zweiten Schritt wird die Hälfte der "quality"  Werte der Eltern subtrahiert und auf ihre "Kopien" übertragen.
   Im letzten Schritt können genetische Operationen wie "Crossover" und/ oder "Mutation" mit einer fest vordefinierten Wahrscheinlichkeit ausgeführt werden.
   Wobei anzumerken ist, dass die Hauptstärke eines "gA"´s in der "Mutation" liegt: Weil während einer Mutation tatsächlich neue Informationen entstehen!
   
      * Sollte ein "Crossover" ausgeführt werden, so werden beide "Kopien" ausgewählt und mit ihnen ein "one point crossover" auf ihrem "matching" String durchgeführt: D.h. die vorhandenen Informationen werden neu kombiniert und/oder durcheinander gewürfelt. Danach wird der "quality"  Wert beider Kopien auf den Mittelwert ihrer "strength/quality"-Werte zurück gesetzt. Die auszuführende Aktion der "classifier"-Kopien bleibt vom Crossover unberührt.
     
      * Sollte der Operator "Mutation" ausgeführt werden, gibt es wiederum eine fest vordefinierte Wahrscheinlichkeit, nach der jede Positon innerhalb des "matching" String ´s die Chance besitzt, zu mutieren.
      Der "quality" -Wert und die Aktion der "classifier"-Kopien bleiben bei der Mutation unberührt!
   
   Als Letztes werden soviele "classifier" aus der Liste gelöscht, dass am Ende ebensoviele "classifier" in derListe sind, wie zu Beginn.
   Diese werden wieder durch ein "roulette wheel" ermittelt wobei dem die wahrscheinlichkeit ausgewählt für die "classifer" mit niedriger "quality höher ist.
   Im vorliegenden Beispiel wären das zwei "classifier".
   In der Literatur gibt es hier zu mehrere Varianten:
   Zum Beispiel, dass nur die "Kinder" (Kopie-"classifier") in die nächste Population übernommen werden.
   Ein Vorteil, wenn die "Eltern" und die "Kinder" gemischt in die nächste Population übernommen werden ist, dass möglicherweise wichtige Informationen besser erhalten werden können.


 * Ein "classfier" wird aus der "matching" Liste M via "roulette wheel" ausgewählt und in die "action set" Liste (A) übernommen.
   In der "action set" Liste (A) werden jetzt all diejenigen "classifier" aus der ursprünglichen "matching" Liste (M) zusätzlich hinzugefügt, die dieselben Aktionen haben, wie der ausgewählte "classifier".


   * Sollte kein "classifier" ausgewählt werden, oder es liegen die gesamt "strength/quality" aller "classifier" in der "matching" Liste (M) unter ''Phi (0.5) * durchschnitt von P'' wird die "covering" Operation ausgeführt: 
   Dabei wird ein neuer "classifier" mit dem Sensor Input als "matching" String erstellt.
   Wobei einzelne Postionen des neuen "matching" Strings durch die Wahrscheinlichkeit P durch das "wildcard" Zeichen "#" ersetzt werden können. 
   Eine Aktion wird zufällig zugeordnet und die "quality" wird mit dem Durchschnittswert aller "classfier" in P gleichgesetzt.
   Im Anschluss wird ein anderer "classifier" gelöscht, dieser wird durch ein "roulette wheel" ermittelt, wobei die wahrscheinlichkeit ausgewählt zu werden für die "classifer" mit niedriger "quality höher ist.


 * Die "action" des ausgewälten "classifiers" wird als "output" an die "Umgebung" übergeben. D.h. die Aktion wird ausgeführt.


 * Ziehe von allen "classifiern" aus der "action set" Liste (A) einen Teil (Konstante ß) ab und summiere die abgezogenen Werte in einer Variabel "B" auf.


 * Sollte die "Umgebung" einen "reward" ( ein "reward" ist eine Art Belohung, die die "Umgebung" vergibt) auf die gegebene Aktion geben, wird die Formel: 
 ''ß * "reward" / (Anzahl der "classifier" im "action set" A)'' angewandt.
 Das Ergebnis wird zu der "quality" in jedem "classfier" in der "action set" Liste (A) hinzuaddiert.


 * Als letztes werden die "classfier" im "Gedächtnis" verändert.
 Dazu wird folgende Formel verwendet:
 ''"B" * Gamma / (Anzahl der Elemente des "action set" A (T-1)) ''
 Im nächsten Schritt wird das Ergebnis zu der "quality" jedes "classfier"´s in A (T-1) hinzu addiert.
 (Anmerkung: die Elemente aus dem "Gedächtnis sind eine referenzi auf die den Elementen aus der aktuellen "classifier" Liste (P))


 * Überschreibe das "Gedächtnis" mit den Elementen des derzeitigen "actions set" (A).


 * lösche die Variabel "B".


 * Der Vorgang wiederholt sich ab Punkt 2, bis das eigentliche Ziel bzw. die "Aufgabe" gelöst /gefunden wurde.