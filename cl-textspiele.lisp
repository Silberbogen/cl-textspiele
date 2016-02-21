;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: cl-textspiele.lisp
;;;; Beschreibung: Eine Sammlung verschiedenster Textspiele für die Konsole
;;;; ------------------------------------------------------------------------
;;;; Author: Sascha K. Biermanns, <skkd PUNKT h4k1n9 AT yahoo PUNKT de>
;;;; Lizenz: GPL v3
;;;; Copyright (C) 2011-2015 Sascha K. Biermanns
;;;; This program is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the
;;;; Free Software Foundation; either version 3 of the License, or (at your
;;;; option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;;; Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License along
;;;; with this program; if not, see <http://www.gnu.org/licenses/>. 
;;;; ------------------------------------------------------------------------


(in-package #:cl-textspiele)


(defun hole-zahl (string)
  "(hole-zahl string)
HOLE-ZAHL gibt die Zeichenkette String aus und erzwingt die Eingabe einer Zahl."
  (format t "~A " string)
  (let* ((*read-eval* nil)
		 (zahl (read)))
    (if (not (numberp zahl))
		(hole-zahl string)
		zahl)))


;;; -----------------------------------------------------
;;;                   Das Hauptprogramm
;;; -----------------------------------------------------



(defun spiele ()
  (let* ((spiele-liste (list #'rate-die-zahl
							 #'computer-rät-zahl
							 #'craps
							 #'addiere-bis-999
							 #'schere-stein-papier
							 #'begriffe-raten))
		 (anzahl (length spiele-liste)))
	(do (beenden)
		(beenden)
	  (format t "~%Dir stehen folgende Möglichkeiten zur Auswahl:~%~%")
	  (do ((i 0 (1+ i)))
		  ((= i anzahl))
		(format t "   ~2D ~A~%" (1+ i) (documentation (elt spiele-liste i) 'function)))
	  (format t "   99 Beenden~%~%")
	  (let ((eingabe (hole-zahl "Deine Wahl? ")))
		(when (numberp eingabe)
		  (when (= eingabe 99)
			(setf beenden t))
		  (when (and (>= eingabe 1) (<= eingabe anzahl))
			(terpri)
			(funcall (elt spiele-liste (1- eingabe)))))))))


;;; ----------------
;;; Wir raten selber
;;; ----------------


(defun rate-die-zahl (&optional (minimum 1) (maximum 1000))
  "Versuche die Zahl zu erraten, die der Computer sich ausgedacht hat!"
  (format t "Versuche eine Zahl zwischen ~:d und ~:d zu erraten!~%" minimum maximum)
  (do ((anzahl 0 (1+ anzahl))
	   (zahl (+ (random (1+ (- maximum minimum))) minimum))
	   versuch)
	  ((and (numberp versuch) (= versuch zahl))
	   (format t "Du hast die richtige Zahl in ~A Versuchen erraten!~%" anzahl))
	(setf versuch (hole-zahl "Dein Versuch?"))
	(format t "Dein Versuch ist ~[keine Zahl.~;zu klein.~;zu groß.~;richtig!~]~%"
			(cond ((not (numberp versuch)) 0)
				  ((< versuch zahl) 1)
				  ((> versuch zahl) 2)
				  (t 3)))))


;;; -----------------------------
;;; Wir lassen den Computer raten
;;; -----------------------------


(defvar *minimum*
  "Speichert die kleinste Zahl, die der Rechner vermutet.")


(defvar *maximum*
  "Speichert die größte Zahl, die der Rechner vermutet.")


(defvar *beenden*
  "Speichert, wann das Spiel zu Ende ist.")


(defun %rate ()
  "Glatt gelogen. Der Rechner ermittelt die Mitte."
  (ash (+ *minimum* *maximum*) -1)) 


(defun %kleiner (zahl)
  "Der Rechner verringert das *MAXIMUM*."
  (setf *maximum* (1- zahl)))


(defun %größer (zahl)
  "Der Rechner erhöht das *MINIMUM*."
  (setf *minimum* (1+ zahl)))


(defun %fragerunde (zahl)
  "Die interaktive Kommunikation zwischen den Spielern."
  (format t "Ist deine Zahl vielleicht die ~A?~%" zahl) 
  (let ((eingabe (intern (string-upcase (read-line)) :keyword)))
	(case eingabe
	  ((:= :ja :stimmt :korrekt :gleich)
	   (setf *beenden* t))
	  ((:< :kleiner :weniger :niedriger :tiefer :drunter)
	   (%kleiner zahl))
	  ((:> :größer :mehr :höher :drüber)
	   (%größer zahl))
	  (otherwise
	   (princ "Ich verstehe nicht, was du meinst! ")
	   (%fragerunde zahl)))))


(defun computer-rät-zahl (&optional (minimum 1) (maximum 1000))
  "Lasse den Computer erraten, welche Zahl du dir ausgedacht hast!"
  (setf *minimum* minimum
		*maximum* maximum
		*beenden* nil)
  (format t "Ich werde versuchen, eine Zahl zu erraten, die du dir ausgedacht hast und die zwischen ~A und ~A liegen muß.~%" minimum maximum)
  (do ((i 0 (1+ i))
	   (zahl (%rate) (%rate)))
	  (*beenden*
	   (format t "~&Deine Zahl ist die ~A!~%" zahl)
	   (format t "Damit habee ich die Zahl im ~A. Versuch erraten!~%" i))
	(%fragerunde zahl)))


;;; -----------------------
;;; Craps - ein Würfelspiel
;;; -----------------------


(defun %werfe-zwei-würfel ()
  "Gibt eine Liste zurück, in der 2 Würfelwürfe mit 6-seitigen Würfeln enthalten sind."
  (list (hr:würfelwurf) (hr:würfelwurf)))


(defun %schlangenaugen-p (liste)
  "Überprüft, ob eine übergebene Liste den Wurf zweier Einsen enthält."
  (when (and (eql (first liste) 1)
			 (eql (second liste) 1))
	t))


(defun %güterwagen-p (liste)
  "Überprüft,ob eine übergebene Liste den Wurf zweier Sechsen enthält."
  (when (and (eql (first liste) 6)
			 (eql (second liste) 6))
	t))


(defun %sofort-gewinn-p (liste &aux (wurf (apply #'+ liste)))
  "Der Wurf von 7 oder 11 ein Sofortgewinn."
  (when (or (eql wurf 7)
			(eql wurf 11))
	t))


(defun %sofort-verlust-p (liste &aux (wurf (apply #'+ liste)))
  "Der Wurf von 2, 3 oder 12 ein Sofortverlust."
  (when (or (eql wurf 2)
			(eql wurf 3)
			(eql wurf 12))
	t))


(defun %sage-wurf (liste)
  "Liest das Ergebnis des per Liste übergebenen Wurfs, addiert die Werte und gibt entweder SCHLANGENAUGEN, GÜTERWAGEN oder die Summe der Augenpaare zurück."
  (cond ((%schlangenaugen-p liste)
		 'schlangenaugen)
		((%güterwagen-p liste)
		 'güterwagen)
		(t
		 (apply #'+ liste))))


(defun %versuche-zu-punkten (zahl)
  "Ermöglicht es zu versuchen, die vorherige Zahl noch einmal zu würfeln und so zu gewinnen."
  (let* ((wurf (%werfe-zwei-würfel))
		 (liste ())
		 (geworfen (list (first wurf) 'und (second wurf) 'gewürfelt)))
	(cond ((or (%sofort-gewinn-p wurf)
			   (eql zahl (apply #'+ wurf)))
		   (setf liste (list '-- (%sage-wurf wurf) '-- 'du 'gewinnst))
		   (format t "~A~%" (append geworfen liste)))
		  ((%sofort-verlust-p wurf)
		   (setf liste (list '-- (%sage-wurf wurf) '-- 'du 'verlierst))
		   (format t "~A~%" (append geworfen liste)))
		  (t
		   (setf liste (list '-- (apply #'+ wurf) '-- 'würfle 'nochmal))
		   (format t "~A~%" (append geworfen liste))
		   (%versuche-zu-punkten zahl)))))


(defun craps ()
  "Spiele eine Partie Craps nach den amerikanischen Casinoregeln!"
	(let* ((wurf (%werfe-zwei-würfel))
		   liste
		   (geworfen (list (first wurf) 'und (second wurf) 'gewürfelt)))
	  (cond ((%sofort-gewinn-p wurf)
			 (setf liste (list '-- (%sage-wurf wurf) '-- 'du 'gewinnst))
			 (format t "~A~%" (append geworfen liste)))
			((%sofort-verlust-p wurf)
			 (setf liste (list '-- (%sage-wurf wurf) '-- 'du 'verlierst))
			 (format t "~A~%" (append geworfen liste)))
			(t
			 (setf liste (list '-- 'du 'hast (apply #'+ wurf) 'punkte))
			 (format t "~A~%" (append geworfen liste))
			 (%versuche-zu-punkten (apply #'+ wurf))))))


;;; --------------
;;; Additionsspiel
;;; --------------


(defun addiere-bis-999 (&optional (zahl 0) (anzahl 0))
  "Addiere Zahlen, bis du auf 999 kommst!"
  (let ((eingabe (hole-zahl "Bitte gib eine Zahl ein:")))
    (cond ((not (integerp eingabe))
		   (addiere-bis-999 zahl anzahl))
		  ((= eingabe 999)
		   (format t "Momentan bist du bei ~A und hast bisher ~A Eingaben getätigt.~%" zahl anzahl)
		   (addiere-bis-999 zahl anzahl))
		  (t
		   (incf zahl eingabe)
		   (incf anzahl)
		   (when (= zahl 999)
			 (format t "Du hast die Zahl 999 mit ~A Eingaben erreicht.~%" anzahl)
			 (return-from addiere-bis-999 (values zahl (/ zahl anzahl 1.0))))
		   (addiere-bis-999 zahl anzahl)))))


;;; --------------------------------------
;;; Schere, Stein, Papier (, Echse, Spock)
;;; --------------------------------------


(defun %auswahl-spielart ()
  (format t "Möchtest du:~%1. Die klassische Variante~%2. Die moderne Variante~%3. Die Spielregeln beider Fassungen lesen~%4. Doch nicht spielen~%> ")
  (let* ((*read-eval* nil)
		 (auswahl (read)))
	(cond ((not (numberp auswahl))
		   (%auswahl-spielart))
		  ((or (< auswahl 1) (> auswahl 4))
		   (%auswahl-spielart))
		  ((= auswahl 3)
		   (format t "~%Die klassische Variante~%Hier gilt folgendes:~%Schere schneidet Papier (und gewinnt)~%Papier bedeckt Stein (und gewinnt)~%Stein schleift Papier (und gewinnt, klar)~%~%Die moderne Variante~%hat ein paar weitere Objekte, Echse und Spock, und daher auch weitere Regeln:~%Stein zerquetscht Echse~%Echse vergiftet Spock~%Spock zertrümmert Schere~%Schere köpft Echse~%Echse frisst Papier~%Papier widerlegt Spock~%Spock verdampft Stein~%~%")
		   (%auswahl-spielart))
		  (t auswahl))))


(defun %computerwahl (&optional (tbbt nil))
  (case (if (null tbbt)
			(hr:würfelwurf 3)
			(hr:würfelwurf 5))
	((1) 'schere)
	((2) 'stein)
	((3) 'papier)
	((4) 'echse)
	(otherwise 'spock)))


(defun %sieger (s1 s2)
  (cond ((and (equal s1 'schere)
			  (equal s2 'papier))
		 "Schere schneidet Papier")
		((and (equal s1 'papier)
			  (equal s2 'stein))
		 "Papier bedeckt Stein")
		((and (equal s1 'stein)
			  (equal s2 'echse))
		 "Stein zerquetscht Echse")
		((and (equal s1 'echse)
			  (equal s2 'spock))
		 "Echse vergiftet Spock")
		((and (equal s1 'spock)
			  (equal s2 'schere))
		 "Spock zetrümmert Schere")
		((and (equal s1 'schere)
			  (equal s2 'echse))
		 "Schere köpft Echse")
		((and (equal s1 'echse)
			  (equal s2 'papier))
		 "Echse frisst Papier")
		((and (equal s1 'papier)
			  (equal s2 'spock))
		 "Papier widerlegt Spock")
		((and (equal s1 'spock)
			  (equal s2 'stein))
		 "Spock verdampft Stein")
		((and (equal s1 'stein)
			  (equal s2 'schere))
		 "Stein schleift Schere")
		(t nil)))


(defun %spielerwahl (&optional (tbbt nil))
  (format t "Du hast zur Auswahl:~%1. Schere~%2. Stein~%3. Papier~%")
  (unless (null tbbt)
	(format t "4. Echse~%5. Spock~%~%Bitte triff deine Entscheidung: "))
	(case (intern (string-upcase (read-line)) :keyword)
	  ((:1 :schere)
	   'schere)
	  ((:2 :stein)
	   'stein)
	  ((:3 :papier)
	   'papier)
	  ((:4 :echse)
	   'echse)
	  ((:5 :spock)
	   'spock)
	  (otherwise
	   (%spielerwahl tbbt))))


(defun %ssp (spieler &optional (tbbt nil))
  (let* ((computer (%computerwahl tbbt))
		 (spieler-gewinnt (%sieger spieler computer))
		 (computer-gewinnt (%sieger computer spieler)))
	(cond (spieler-gewinnt
		   (format nil "~A. Du gewinnst!" spieler-gewinnt))
		  (computer-gewinnt
		   (format nil "~A. Ich gewinne!" computer-gewinnt))
		  (t (format nil "Unentschieden!")))))


(defun schere-stein-papier ()
  "Spiele Schere-Stein-Papier, wahlweise auch mit Echse und Spock!"
	(let ((tbbt nil)
		  (auswahl nil))
	  (format t "Willkommen zu Schere-Stein-Papier!~%Ich beherrsche beide Spielarten, das gute alte Schere-Stein-Papier, oder die moderne Version aus TBBT. Bei der klassichen Version stehen lediglich Schere, Stein und Papier zur Auswahl, bei der modernen Version kommen noch Echse und Spock hinzu.~%")
	  (setf auswahl (%auswahl-spielart))
	  (if (= auswahl 2)
		  (setf tbbt t))
	  (unless (= auswahl 4)
		(loop
		   (format t "~A~%" (%ssp (%spielerwahl tbbt) tbbt))
		   (unless (hr:j-oder-n-p "Nochmal?") (return))))
	  (format t "Danke für's mitspielen!~%")
	  'ciao!))


;;; ----------------------------------------------------
;;; Wir erraten Begriffe, die sich der Computer ausdenkt
;;; ----------------------------------------------------


(defun %ausgabe (bekannt gesucht)
  (let ((anzahl (length gesucht))
		(kleingeschrieben (string-downcase gesucht)))
	(do ((i 0 (1+ i)))
		((= i anzahl)
		 kleingeschrieben)
	  (unless (subsetp (list (elt kleingeschrieben i)) bekannt)
		(setf (elt kleingeschrieben i) #\_)))))


(defun %noch-ungenutzt (bekannt)
  (let* ((möglich (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜß" 'list))
		 (großgeschrieben (coerce (string-upcase (coerce bekannt 'string)) 'list))
		 (anzahl (length möglich)))
	(do ((i 0 (1+ i)))
		((= i anzahl)
		 möglich)
	  (when (subsetp (list (elt möglich i)) großgeschrieben)
		(setf (elt möglich i) #\_)))))


(defun %versuch ()
  (let ((eingabe (string-trim " " (read-line))))
	(if (= 1 (length eingabe))
		(coerce (string-downcase (elt eingabe 0)) 'character)
		eingabe)))


(defun %spiele (gesucht &optional (bekannt (list #\space #\, #\. #\; #\? #\!))
						  (runde 1))
  "Das eigentliche Spiel. Das gesuchte Wort wird übergeben."
  (format t "~%*** ~A. Runde ***~%" runde)
  (format t "~&~A~%~%" (%noch-ungenutzt bekannt))
  (format t "~&~A~%~%Dein Tip? " (%ausgabe bekannt gesucht))
  (let ((eingabe (%versuch)))
	(typecase eingabe
	  (character (push eingabe bekannt)
				 (when (subsetp (coerce (string-downcase (hr:nur-buchstaben gesucht)) 'list) bekannt)
				   (format t "~%Und das gesuchte Wort lautete: ~A~%" gesucht)
				   (format t "~%Glückwunsch!~%Du hast es geschafft!~%")
				   (return-from %spiele)))
	  (string (when (equalp eingabe gesucht)
				(format t "~%Und das gesuchte Wort lautete: ~A~%" gesucht)
				(format t "~%Glückwunsch!~%Du hast es geschafft!~%")
				(return-from %spiele))
			  (%spiele gesucht bekannt (1+ runde)))
	  (otherwise (format t "Bitte gib einen einzelnen Buchstaben oder die gesamte Lösung ein!~%")))
	(%spiele gesucht bekannt (1+ runde))))


(defun %erstelle-suchliste (stream-name)
  "Suchbegriffe zeilenweise einlesen"
  (let ((liste ()))
	(with-open-file (stream stream-name)
	  (do ((i (read-line stream nil)
			  (read-line stream nil)))
		  ((null i)
		   liste)
		(push (string-trim " " i) liste)))))


(defun begriffe-raten (&optional (dateiname "Dokumente/begriffe.txt"))
  "Errate einen Begriff, den sich der Computer ausgedacht hat!"
  (do* ((begriffe (hr:mischen (%erstelle-suchliste dateiname)))
		beenden)
	   ((or (null begriffe) beenden)
		(format t "Vielen Dank für's Spielen!~%"))
	(%spiele (string-trim " " (pop begriffe)))
	(when begriffe
		(setf beenden (not (hr:j-oder-n-p "Willst du ein weiteres Spiel spielen? "))))))







