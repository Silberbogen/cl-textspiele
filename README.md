cl-textspiele
=============

Das Paket CL-TEXTSPIELE enthält eine Reihe klassischer Textspiele, wie
sie in den 60er bis frühen 80er Jahren auf Computern gespielt wurden.

Zum Spielen sollte man per _git clone_ das Repository im Verzeichnis
~/quicklisp/local-projects/ anlegen.

Zum Spielen unter Slime empfehlen sich folgende Kommandos:
- (ql:quickload :cl-textspiele)
- (in-package :cl-textspiele)
- (spiele)


Zum Erstellen einer ausführbaren Datei:
$ *sbcl*
* *(ql:quickload :cl-hilfsroutinen)*
* *(ql:quickload :cl-textspiele)*
* *(sb-ext:save-lisp-and-die #p"textspiele" :toplevel #'cl-textspiele:spiele :executable t)*


*Enthaltene Spiele*
-------------------
* **Zahlenraten (2 Varianten, Mensch rät oder Computer rät)**
* **Craps (nach amerikanischen Casinoregeln)**
* **Addiere Zahlen bis 999**
* **Schere, Stein, Papier (die klassische Variante ebenso wie die moderne Variante)**
* **Begriffe raten**


Bildschirmfotos
---------------
![Bildschirmfoto](/bildschirmfoto.png)
![Bildschirmfoto2](/bildschirmfoto2.png)
![Bildschirmfoto3](/bildschirmfoto3.png)

