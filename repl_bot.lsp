;;
;; Copyright (c) Haiko Schol (http://www.haikoschol.com/)
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(set 'server "irc.server.org")
(set 'port 6667)
(set 'nick "repl_bot")
(set 'channel "#lisp")
(set 'chuck-norris-facts '())
(set 'cheffe '("a" "list" "of" "nicks"))
(set 'ops '())

(define (handle-command command argument)
  (if (= (upper-case command) "EVAL")
    (string (eval-string argument))
    (string "broken")))

(define (parse-command line)
  (if (regex "(\\S+)\\s+(\(.+\))" line)
    (handle-command $1 $2)
    (string "broken")))

(define (parse-privmsg nick msg)
  (if (regex "eval\\s+\\((\\S+)\\s" msg)
    (if (!= nil (check-acl nick $1))
      (parse-command msg))))

(define (join-channel chan)
  (and
    (net-send socket (append "JOIN " chan "\r\n"))
    (set 'oldchannel channel)
    (set 'channel chan)))

(define (part-channel chan)
  (and
    (set 'channel oldchannel)
    (net-send socket (append "PART " chan "\r\n"))))

(define (login server port nick channel)
  (set 'socket (net-connect server port))
  (if socket
    (and
      (net-send socket (append "NICK " nick "\r\n"))
      (net-send socket (append "USER " nick " sputnik " server " :repl\r\n"))
      (join-channel channel)
      (enter-repl))))

(define (logout)
  (net-send socket "QUIT\r\n"))

(define (enter-repl)
  (while (not (net-error))
    (if (net-select socket "read" 10000)
      (and
        (net-receive socket 'buffer 2048)
        (parse-protocol-message buffer)))))

(define (parse-protocol-message message)
  (if (regex ":(\\S+)!\\S+\\s+PRIVMSG\\s\\S+\\s:!(.+)" message)
    (if (find $1 cheffe)
      (net-send socket (append "PRIVMSG " channel " " (parse-command $2) "\r\n")))
    (if (regex "PING (\\S+)" message)
      (let ((pingcode $1))
        (net-send socket (append "PONG " pingcode "\r\n")))))
  (if (regex ":(\\S+)!\\S+\\s+JOIN\\s" message)
    (if (!= nil (find $1 ops))
      (net-send socket (append "MODE " channel " +o " $1 "\r\n")))))

(define (url-encode param)
  (and
    (replace " " param "+")
    (replace "\"" param "%22")
    (replace "(" param "%28")
    (replace ")" param "%29")))

(define (construct-google-url query)
  (append "http://www.google.de/search?q=" (url-encode query) "&start=0&start=0&ie=utf-8&oe=utf-8&client=firefox&rls=org.mozilla:en-US:unofficial"))

(define (get-google-result-count query)
  (and
    (regex "hr <b>(\\S+)</b>" (get-url (construct-google-url query)))
    (integer (replace "." $1 ""))))

(define (google-fight word1 word2)
  (if (> (get-google-result-count word1) (get-google-result-count word2))
    (append word1 " has won")
    (append word2 " has won")))

(define (google-fight2 word1 word2)
  (let ((res1 (get-google-result-count word1)))
        (sleep 3)
        (if (> res1 (get-google-result-count word2))
          (string (append word1 " has won"))
          (string (append word2 " has won")))))

(define (construct-wikipedia-url lang query)
  (string (append "http://" lang ".wikipedia.org/wiki/" query)))

(define (get-wikipedia-not-found-message lang)
  (if (= lang "en")
    (string "We don't have an article called")
    (string "Diese Seite existiert noch nicht")))

(define (wikipedia-article-exists? lang query)
  (nil?
    (find
      (get-wikipedia-not-found-message lang) (get-url (construct-wikipedia-url lang query)))))

(define (wikipedia-query-is-ambiguous? page-source)
  (find "Vorlage_Begriffsklaerung" page-source))

(define (wikipedia-extract-article-links page-source)
  (let ((tmplist (parse page-source "\n"))
        (result '()))
    (dolist (tmp tmplist)
      (and
        (regex "a href=\"/wiki/(?!Wikipedia|Portal|Hauptseite|Diskussion|Spezial|Bild|Kategorie)(\\S+)\"" tmp)
        (push (construct-wikipedia-url "de" $1) result -1))
      result)))

(define (wikipedia query)
  (let ((wikipedia-page (get-url (construct-wikipedia-url "de" query))))
    (if (nil? (find (get-wikipedia-not-found-message "de") wikipedia-page))
      (if (wikipedia-query-is-ambiguous? wikipedia-page)
	(and
          (send-list (wikipedia-extract-article-links wikipedia-page))
          't)
        (construct-wikipedia-url "de" query))
      (if (wikipedia-article-exists? "en" query)
        (construct-wikipedia-url "en" query)
        (string "nix gefunden")))))

(define (send-list alist)
  (and
    (dolist (tmp alist)
      (and
        (net-send socket (append "PRIVMSG " channel " " tmp "\r\n"))
        (sleep 2000)))
    't))

(define (chuckism)
  (nth (rand (length chuck-norris-facts)) chuck-norris-facts))

(define (populate-chuck-norris-facts url)
  (let ((page-source (parse (get-url url) "<BR><BR>" 4)))
    (dolist (tmp page-source)
      (and
        (regex "<li>(.*)" tmp)
        (push $1 chuck-norris-facts)))))

(populate-chuck-norris-facts "http://www.chucknorrisfacts.com/")
(populate-chuck-norris-facts "http://www.chucknorrisfacts.com/additionalfacts.html")
(populate-chuck-norris-facts "http://www.chucknorrisfacts.com/morefacts.html")
(login server port nick channel)
