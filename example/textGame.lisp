;; utils
(defn head (l)
  (unless (empty? l) (car l)))

(defn zip-actions-with-args (actions items)
  (cond
    ((empty? actions) '())
    ((empty? items) '())
    ((eq? (car items) #nil) (zip-actions-with-args (cdr actions) (cdr items)))
    (#t (cons (list (car actions) (car items))
              (zip-actions-with-args (cdr actions) (cdr items))))))

(defn set-flag (flag-name value)
  (fn () (def* flag-name value)))

;; printing
(defn clear-output ()
  (def output-lines '()))

(defn print-list (lines)
  (def output-lines (cat output-lines lines)))

(defn print ()
  (def output-lines (cat output-lines _args)))

(defn flush-output ()
  (def output (unlines output-lines)))

;; description
(defn* description ()
  (list 'description _args))

(defn get-attribute (name element)
  (get name (eval element)))

(defn get-description (element)
  (let description (get 'description (eval element)))
  (if (eq? description #nil) description-dont-exist ;; todo change to exception
      description))

(defn eval-desc-item (item)
  (cond
    ((eq? (type-of item) "string") item)
    ((eq? (type-of item) "list") (eval item))))

(defn eval-description (desc)
  (filter (fn (x) (eq? (type-of x) "string"))
                      (map eval-desc-item desc)))

;; item (name, description, traits)
(defn item (name)
  (def* name (cdr _args)))

(defn name (name-str)
  (list 'name name-str))

(defn* dialog ()
  (list 'dialog _args))

(defn attribute (attr value)
  (list attr value))

(defn event (event-name handler)
  (list event-name handler))

(defn traits ()
  (list 'traits _args))

(defn get-item-name (item)
  (let name (get 'name (eval item)))
  (if (eq? name #nil) name-dont-exist ;; todo change to exception
      name))

(defn get-item-traits (item)
  (let traits (get 'traits (eval item)))
  (if (eq? traits #nil) '() traits))

;; action (verb, type, perform)
(def all-actions (list))

(defn action (name)
  (def all-actions (insert name all-actions))
  (def* name (cdr _args)))

(defn verb (verb-str)
  (list 'verb (words verb-str)))

(defn get-verb (action)
  (let verb (get 'verb (eval action)))
  (if (eq? verb #nil) verb-dont-exist
      verb))

(defn action-type (type-name)
  (list 'action-type type-name))

(defn perform (f)
  (list 'perform f))

;; room (description, items, traits, locations)
(defn room (name)
  (def* name (cdr _args)))

(defn items ()
  (list 'items _args))

(defn location (name room)
  (list name room))

(defn locations ()
  (list 'locations _args))

;; current room
(defn get-curr-room-items ()
  (let items (get 'items (eval current-room)))
  (if (eq? items #nil) '() items))

(defn get-curr-room-locations ()
  (let locations (get 'locations (eval current-room)))
  (if (eq? locations #nil) '() locations))

;; printing game stuff
(defn print-item (item)
  (print-list (cons (cats " - " (get-item-name item))
         (map (fn (line) (cats "     " line)) (eval-description (get-description item))))))

(defn print-curr-room-items-descriptions ()
  (print "" "Widzisz:")
  (map print-item (get-curr-room-items)))

(defn print-equipment-items ()
  (if (eq? (length equipment) 0)
      (print "Masz pusty ekwipunek.")
      (progn
        (print "W twoim ekwipunku znajduje si??:")
        (map print-item equipment))))

(defn print-curr-room-locations ()
  (print "" "Mo??esz i???? do:")
  (map (fn (location-item) (print (cats " - " (car location-item))))
       (get-curr-room-locations)))

(defn print-curr-room-description ()
  (print-list (eval-description (get-description current-room))))

(defn print-curr-item-whole-description ()
  (print-curr-room-description)
  (print-curr-room-items-descriptions)
  (print-curr-room-locations))

;; equipment
(def equipment '())

;; action finding
(defn skip (n list)
  (cond
    ((= n 0) list)
    ((empty? list) list)
    (#t (skip (- n 1) (cdr list)))))

(defn starts-with (input-words prefix-words)
  (cond
    ((empty? prefix-words) #t)
    ((empty? input-words) #f)
    ((not (eq? (car prefix-words) (car input-words))) #f)
    (#t (starts-with (cdr input-words) (cdr prefix-words)))))

(defn split-on (word input)
  (cond
    ((empty? input) #nil)
    ((eq? (car input) word)
     (list '() (cdr input)))
    (#t (progn
          (let result (split-on word (cdr input)))
          (if (eq? result #nil) #nil
              (list
               (cons (car input) (car result))
               (car (cdr result))))))))

(defn find-matching-action (input-words)
  (find
   (fn (x)
       (starts-with input-words (get-verb x)))
   all-actions))

(defn parse-action-arg-pair (input)
  (let input-words (words input))
  (let parsed-action (find-matching-action input-words))
  (unless (eq? parsed-action #nil)
    (progn
      (let action-verb (get-verb parsed-action))
      (list parsed-action (skip (length action-verb) input-words)))))

(defn eval-action-arg-pair (action-arg-pair)
  (let action (eval (car action-arg-pair)))
  (let action-arg (car (cdr action-arg-pair)))
  ((get 'perform action) action-arg))

;; game loop
(defn init ()
  (clear-output)
  (print-curr-item-whole-description)
  (flush-output))

(defn update ()
  (def room-changed #f)
  (clear-output)

  (let action-arg-pair (parse-action-arg-pair input))
  (if (eq? action-arg-pair #nil)
      (print "Nie rozumiem co chcesz zrobi??.")
      (eval-action-arg-pair action-arg-pair))

  (when room-changed (print-curr-item-whole-description))
  (flush-output))

;; action functions
(defn has-trait (trait item)
  (member trait (get-item-traits item)))

(defn is-in-curr-room (item)
  (member item (get 'items (eval current-room))))

(defn is-in-equipment (item)
  (member item equipment))

(defn find-item-in-equipment (item-name)
  (find
   (fn (item) (eq? (get-item-name item) item-name))
   equipment))

(defn find-item-in-curr-room (item-name)
  (let room-items (get-curr-room-items))
  (find
   (fn (item) (eq? (get 'name (eval item)) item-name))
   room-items))

(defn find-near-item (item-name)
  (let item (find-item-in-equipment item-name))
  (cond
    ((eq? item #nil) (find-item-in-curr-room item-name))
    (#t item)))

(defn find-near-location (location-name)
  (let room-locations (get-curr-room-locations))
  (find
   (fn (location) (eq? (car location) location-name))
   room-locations))

(defn take-item-from-room (item)
  (let new-room-items (remove item (get-curr-room-items)))
  (def* current-room (set 'items new-room-items (eval current-room)))
  (def equipment (insert item equipment)))

(defn drop-item-from-equipment (item)
  (def equipment (remove item equipment))
  (let new-room-items (insert item (get-curr-room-items)))
  (def* current-room (set 'items new-room-items (eval current-room))))

(defn remove-item-from-equipment (item)
  (def equipment (remove item equipment)))

(defn add-item-to-equipment (item)
  (def equipment (insert item equipment)))

(defn add-location-to-current-room (location-name location)
  (def new-room-locs (set location-name location (get-curr-room-locations)))
  (def* current-room (set 'locations new-room-locs (eval current-room))))

(defn remove-location-from-current-room (location-name)
  (def new-room-locs (unset location-name (get-curr-room-locations)))
  (def* current-room (set 'locations new-room-locs (eval current-room))))

(defn add-item-to-current-room (item)
  (let new-room-items (insert item (get-curr-room-items)))
  (def* current-room (set 'items new-room-items (eval current-room))))

(defn remove-item-from-current-room (item)
  (let new-room-items (remove item (get-curr-room-items)))
  (def* current-room (set 'items new-room-items (eval current-room))))

(defn change-room (room)
  (def current-room room)
  (def room-changed #t)
  (let event-handler (get-attribute 'on-enter room))
  (unless (eq? event-handler #nil) (event-handler)))

(defn is-near (item)
  (cond ((is-in-equipment item) #t)
        ((is-in-curr-room item) #t)
        (#t #f)))

(defn opening-one-item (arg)
  (let arg (unwords arg))
  (let item (find-item-in-curr-room arg))
  (cond
    ((eq? item #nil)
     (print (cats "W tym miejscu nie ma " arg ".")))
    ((not (has-trait 'openable))
     (print (cats "Nie mo??esz otworzy?? " arg ".")))
    ((eq? (get-attribute 'lock-flag item) 0)
     (print (cats "Nie mo??esz otworzy?? czego?? co ju?? jest otwarte.")))
    (#t (print "Potrzebujesz czego?? do otwarcia"))))

(defn opening-two-items (fst-item snd-item)
    (let fst-item (find-item-in-curr-room (unwords fst-item)))
    (let snd-item (find-item-in-equipment (unwords snd-item)))
    (cond
      ((eq? fst-item #nil)
       (print "Nie rozumiem co chcesz otworzy??."))
      ((eq? snd-item #nil)
       (print "Nie rozumiem czego chcesz u??y??."))
      ((not (has-trait 'openable fst-item))
       (print (cats "Nie mo??esz otworzy?? " (get-item-name fst-item) ".")))
      ((eq? (get-attribute 'key-flag snd-item) #nil)
       (print (cats "Nie uda??o si?? otworzy?? " (get-item-name fst-item) " za pomoc?? " (get-item-name snd-item) ".")))
      ((eq? (get-attribute 'lock-flag fst-item) 0)
       (print (cats "Nie mo??esz otworzy?? czego?? co ju?? jest otwarte.")))
      ((eq? (get-attribute 'key-flag snd-item) (get-attribute 'lock-flag fst-item))
       (progn
         (def* fst-item (set 'lock-flag 0 (eval fst-item)))
         (print (cats "Otw??rzy??e?? " (get-item-name fst-item) "."))
         (let item-func (get-attribute 'on-open fst-item))
         (when (eq? (type-of item-func) "func")
           (item-func item))))))
  
(defn opening (arg)
  (let item-pair (split-on "u??ywaj??c" arg))
  (if (eq? item-pair #nil)
      (opening-one-item arg)
      (opening-two-items (car item-pair) (car (cdr item-pair)))))

(defn attacking-one-item (arg)
  (let arg (unwords arg))
  (let item (find-item-in-curr-room arg))
  (cond
    ((eq? item #nil)
     (print (cats "W tym miejscu nie ma " arg ".")))
    ((not (has-trait 'killable))
     (print (cats "Nie mo??esz zaatakowa?? " arg ".")))
    ((> (get-attribute 'power-level item) 0)
     (progn
       (def is-running #f)
       (print "Niestety przegra??e??, zaatakowa??e?? gro??nego przeciwnika nie u??ywaj??c broni.")))
    (#t
     (progn
       (remove-item-from-current-room item)
        (let item-func (get-attribute 'on-kill fst-item))
        (when (eq? (type-of item-func) "func")
          (item-func item))))))
      

(defn attacking-two-items (fst-item snd-item)
    (let fst-item (find-item-in-curr-room (unwords fst-item)))
    (let snd-item (find-item-in-equipment (unwords snd-item)))
    (cond
      ((eq? fst-item #nil)
       (print "Nie rozumiem co chcesz zaatakowa??."))
      ((eq? snd-item #nil)
       (print "Nie rozumiem jakiej broni chcesz u??y??."))
      (#t
       (progn
         (let weapon-level (get 'power-level snd-item))
         (when (eq? weapon-level #nil) (let weapon-level 0))
         (let enemy-level (get 'power-level fst-item))
         (when (eq? enemy-level #nil) (let enemy-level 0))
         (if (>= weapon-level enemy-level)
             (progn
               (remove-item-from-current-room fst-item)
               (print (cats "Wygra??e?? walk?? z " (get-item-name fst-item) "."))
               (let item-func (get-attribute 'on-kill fst-item))
               (when (eq? (type-of item-func) "func")
                 (item-func item)))
             (progn
                (def is-running #f)
                (print "Niestety przegra??e??.")))))))

(defn attacking (arg)
  (let item-pair (split-on "u??ywaj??c" arg))
  (if (eq? item-pair #nil)
      (attacking-one-item arg)
      (attacking-two-items (car item-pair) (car (cdr item-pair)))))



(defn giving-one-item (arg)
  (let arg (unwords arg))
  (let item (find-item-in-equipment arg))
  (cond
    ((eq? item #nil)
     (print (cats "Nie masz w ekwipunku " arg ".")))
    (#t (print (cats "Komu chcesz zanie???? " arg ".")))))

(defn giving-two-items (fst-item snd-item)
    (let fst-item (find-item-in-equipment (unwords fst-item)))
    (let snd-item (find-item-in-curr-room (unwords snd-item)))
    (cond
      ((eq? fst-item #nil)
       (print "Nie rozumiem co chcesz zanie????."))
      ((eq? snd-item #nil)
       (print (cats "Nie rozumiem komu chcesz zanie???? " (get-item-name fst-item) ".")))
      ((not (has-trait 'talkable snd-item))
       (print (cats "Nie mo??esz nic da?? " (get-item-name snd-item "."))))
      ((not (eq? (get-attribute 'want-item snd-item) fst-item))
       (print "Nie chce tego."))
      (#t
       (progn
         (remove-item-from-equipment fst-item)
         (print (cats "Da??e?? " (get-item-name fst-item) "."))
         (let item-func (get-attribute 'on-give snd-item))
         (when (eq? (type-of item-func) "func")
           (item-func fst-item))))))
 

(defn giving (arg)
  (let item-pair (split-on "do" arg))
  (if (eq? item-pair #nil)
      (giving-one-item arg)
      (giving-two-items (car item-pair) (car (cdr item-pair)))))
      
       
(defn taking (arg)
  (let arg (unwords arg))
  (let item (find-item-in-curr-room arg))
  (if (eq? item #nil)
      (progn
        (if (eq? (find-item-in-equipment arg) #nil)
            (print "Nie rozumiem co chcesz podnie????.")
            (print (cats "Masz " arg " w ekwipunku."))))
      (if (has-trait 'pickable item)
          (progn
            (take-item-from-room item)
            (print (cats "Podnios??e?? " (get-item-name item) "."))
            (let item-func (get 'on-pick (eval item)))
            (when (eq? (type-of item-func) "func")
              (item-func item)))
          (print (cats "Nie mozesz podnie???? " arg ".")))))
            
(defn dropping (arg)
  (let arg (unwords arg))
  (let item (find-item-in-equipment arg))
  (if (eq? item #nil)
      (progn
        (if (eq? (find-item-in-curr-room arg) #nil)
            (print "Nie rozumiem co chcesz upu??ci??")
            (print (cats "Nie masz " arg " w ekwipunku."))))
      (progn
        (drop-item-from-equipment item)
        (print (cats "Upu??ci??e?? " (get-item-name item) "."))
        (let item-func (get 'on-drop (eval item)))
        (when (eq? (type-of item-func) "func")
          (item-func item)))))

(defn eating (arg)
  (let arg (unwords arg))
  (let item (find-item-in-equipment arg))
  (cond
    ((eq? item #nil)
     (print (cats "Nie masz " arg " w ekwipunku.")))
    ((not (has-trait 'eatable item))
     (print (cats "Nie mo??esz zje???? " arg ".")))
    (#t
      (progn
        (remove-item-from-equipment item)
        (print (cats "Zjad??e?? " (get-item-name item) "."))
        (let item-func (get 'on-eat (eval item)))
        (when (eq? (type-of item-func) "func")
          (item-func item))))))

(defn playing (arg)
  (let arg (unwords arg))
  (let item (find-item-in-equipment arg))
  (cond
    ((eq? item #nil)
     (print (cats "Nie masz " arg " w ekwipunku.")))
    ((not (has-trait 'playable item))
     (print (cats "Nie mo??esz zagra?? na " arg ".")))
    (#t
     (progn
        (let item-func (get 'on-play (eval item)))
        (when (eq? (type-of item-func) "func")
          (item-func item))))))

(defn drinking (arg)
  (let arg (unwords arg))
  (let item (find-item-in-equipment arg))
  (cond
    ((eq? item #nil)
     (print (cats "Nie masz " arg " w ekwipunku.")))
    ((not (has-trait 'drinkable item))
     (print (cats "Nie mo??esz wypi?? " arg ".")))
    (#t
      (progn
        (remove-item-from-equipment item)
        (print (cats "Wypi??e?? " (get-item-name item) "."))
        (let item-func (get 'on-drink (eval item)))
        (when (eq? (type-of item-func) "func")
            (item-func item))))))

(defn traveling (arg)
  (let location-name (unwords arg))
  (let location (find-near-location location-name))
  (if (eq? location #nil)
      (print "Nie rozumiem gdzie chcesz p??j????")
      (progn
        (print "")
        (change-room (car (cdr location))))))

(defn talking (arg)
  (let arg (unwords arg))
  (let item (find-item-in-curr-room arg))
  (cond
    ((eq? item #nil)
     (print "Nie rozumiem z kim chcesz rozmawia??."))
     ((not (has-trait 'talkable item))
      (print (cats "Nie mo??esz rozmawia?? z " arg ".")))
     (#t
      (progn
        (let dialog (get 'dialog (eval item)))
        (if (eq? dialog #nil)
            (print (cats arg " nie ma Ci nic do powiedzenia."))
            (progn
                (print-list (eval-description dialog))
                (let item-func (get 'on-talk (eval item)))
                (when (eq? (type-of item-func) "func")
                    (item-func item))))))))

;; actions
(action 'look
         (verb "rozejrzyj si??")
         (perform (fn () (def room-changed #t))))

(action 'play
        (verb "zagraj na")
        (perform playing))

(action 'give
        (verb "zanie??")
        (perform giving))

(action 'take
         (verb "podnie??")
         (perform taking))

(action 'look-at-equipment
        (verb "zajrzyj do ekwipunku")
        (perform print-equipment-items))

(action 'go
         (action-type 'travel)
         (verb "id?? do")
         (perform traveling))

(action 'eat
        (verb "zjedz")
        (perform eating))

(action 'drink
        (verb "wypij")
        (perform drinking))

(action 'drop
        (verb "upu????")
        (perform dropping))

(action 'open
        (verb "otw??rz")
        (perform opening))

(action 'talk
        (verb "rozmawiaj z")
        (perform talking))

(action 'attack
        (verb "zaatakuj")
        (perform attacking))

(item 'apple
      (description "Wygl??daj??ce smakowicie jab??ko")
      (traits 'pickable 'eatable)
      (name "jab??ko")
      (list 'on-eat
            (fn (item)
                (print "Dosta??e?? zatrucia i umar??e??.")
                (def is-running #f))))

(item 'lamp
      (name "lampa")
      (description "??atwa do odpalenia i zgaszenia lampa z krzemieniem i krzesiwem.")
      (traits 'pickable))
;; story

(def beggar-got-apple #f)
(def player-musician-relation 'not-found-brother)
(def rats-in-cave #t)

(item 'beggar
      (name "??ebrak")
      (description
       "Cz??owiek bez jednej nogi, ubrany w podarte, brudne, kr??tkie spodenki"
       "Obok niego le??y koszyk z paroma monetami oraz laska.")
      (dialog
       (if beggar-got-apple
           "Dzi??kuj?? Ci nieznajomy za pomoc. Nie zapomn?? Ci tego."
           "Prosz?? znajd?? mi co?? do jedzenia. G??oduj??."))
      (attribute 'want-item 'apple)
      (traits 'talkable)
      (event 'on-give
             (fn (item)
                 (add-item-to-equipment 'lamp)
                 (def beggar-got-apple #t)
                 (print "Dzi??kuj?? Ci z ca??ego serca. Prosz??, to wszystko co mam.")
                 (print "Dosta??e?? lamp??!"))))

(def alchemist-got-herbs #f)

(item 'alchemist
      (name "alchemiczka")
      (description
       "Starsza pani z siwymi w??osami, ubrana w eleganckie ubranie."
       "Miesza dziwny p??yn w wielkim kotle.")
      (attribute 'want-item 'red-herbs)
      (traits 'talkable)
      (dialog
        (if alchemist-got-herbs
            (print "Pami??taj, wypij w??a??ciw??.")
            (print (unlines
                '("Witaj, zagubiona duszo. Wiem czego potrzebujesz."
                  "Nie nale??ysz do tego ??wiata. Mog?? pom??c Ci si?? wydosta??."
                  "Ale najpierw przynie?? mi troch?? takich ??miesznych czerwonych zi????ek."
                  "Rosn?? w naszym lesie, tutaj obok miasteczka.")))))
      (event 'on-give
             (fn (item)
                 (def alchemist-got-herbs #t)
                 (add-item-to-equipment 'red-potion)
                 (add-item-to-equipment 'blue-potion)
                 (print "Dzi??kuj??, przyjmij te mikstury w dow??d mojej wdzi??czno??ci. Wypij w??a??ciw??."))))

(item 'flute
      (name "flet")
      (description
       "Prosty, drewniany flet.")
      (traits 'playable)
      (event 'on-play
             (fn ()
                 (if (and (eq? current-room 'cave) rats-in-cave (is-in-equipment 'lamp))
                     (progn
                       (print "Zagra??e?? pi??kn?? melodie. Jednak szczury z jakiego?? powodu jej nie")
                       (print "doceni??y i poucieka??y w szczeliny jaskini.")
                       (def rats-in-cave #f)
                       (change-room 'cave))
                     (progn
                       (print "Zagra??e?? sobie melodie na flecie."))))))

(item 'crossbow
      (name "kusza")
      (description
       "Spora kusza z zapasem be??tu.")
      (traits 'pickable)
      (attribute 'power-level 60))

(item 'red-potion
      (name "czerwona mikstura")
      (description
       "Fiolka z czerwonym p??ynem.")
      (traits 'pickable 'drinkable)
      (event 'on-drink
             (fn ()
                 (def is-running #f)
                 (print (unlines
                         '("Wypijasz mikstur?? i czekasz na efekt."
                           "Po chwili robisz si?? senny i zasypiasz na ziemi."
                           "Budzisz si?? we w??asnym ??????ku..."))))))
                           

(item 'blue-potion
      (name "niebieska mikstura")
      (description
       "Fiolka z niebieskim p??ynem.")
      (traits 'pickable 'drinkable)
      (event 'on-drink
             (fn ()
                 (change-room 'town)
                 (remove-location-from-current-room "sklep z miksturami")
                 (def equipment '())
                 (print (unlines
                         '("Wypijasz mikstur?? i czekasz na efekt."
                           "Po chwili robisz si????senny i zasypiasz na ziemi."
                           "Budzisz si????wstajesz i rozgl??dasz."))))))

(item 'musician
      (name "muzyk")
      (description
       "Cz??owiek ubrany w zielon?? tunik??, graj??cy pi??kn?? melodie na lutni.")
      (dialog
       (cond 
           ((eq? player-musician-relation 'found-brother-got-flute) "Dzi??kuj?? Ci za znalezienie mojego brata. Zn??w musia?? wpakowa?? si?? w tarapaty.")
           ((eq? player-musician-relation 'found-brother)
            (unlines '(
                       "W wi??zieniu???? Bo??e w co on si?? zn??w wpakowa??. Dzi??kuj?? Ci za znalezienie tego g????ba."
                       "Masz tu prezent ode mnie."
                       ""
                       "Dosta??e?? flet!!!")))
           ((eq? player-musician-relation 'not-found-brother) "Widzia??e?? gdzie?? mojego brata? Wygl??da tak samo jak ja. Je??eli go zobaczysz to powiedz mi gdzie jest.")))
      (traits 'talkable)
      (event 'on-talk
             (fn () (when (eq? player-musician-relation 'found-brother)
                      (progn
                        (add-item-to-equipment 'flute)
                        (def player-musician-relation 'found-brother-got-flute))))))

(item 'prisoner
      (name "wi??zie??")
      (description
       "Cz??owiek w obdartych ubraniach.")
      (dialog
       "Ej Ty, przeka?? mojemu bratu, ??e jestem w wi??zieniu! Jest sprzedawc?? w sklepie z lutniami.")
      (traits 'talkable)
      (event 'on-talk
             (fn () (def player-musician-relation 'found-brother))))

(item 'cage
      (name "klatka")
      (description
       "Du??a klatka, w kt??rej znajduj?? si?? ludzkie zw??oki.")
      (traits 'openable)
      (attribute 'lock-flag 100)
      (event 'on-open (fn ()
                          (add-item-to-current-room 'crossbow)
                          (print "Pod zw??okami znalaz??e?? kusza."))))

(item 'key
      (name "srebrny klucz")
      (traits 'pickable)
      (description
       "Ci????ki, srebrny klucz".)
      (attribute 'key-flag 100))

(item 'wolves
      (name "wilki")
      (traits 'killable)
      (description
       "Stado gro??nych, szarych wilk??w")
      (attribute 'power-level 50)
      (event 'on-kill
             (fn () (add-location-to-current-room "g????boki las" 'deep-forest))))

(item 'red-herbs
      (name "czerwone zio??a")
      (traits 'pickable)
      (description "Dziwnie wygl??daj??ce, czerwone zio??a."))

(room 'town
      (description
       "Jeste?? na ??rodku ma??ego, ??redniowiecznego miasteczka."
       "Przy wielu pe??nych b??ota uliczkach znajduj?? si?? drewniane domki mieszka??c??w."
       "S??ycha??, zlewaj??ce si?? w ha??as, d??wi??ki codziennego ??ycia ludzi."
       "Ludzie chodz??, rozmawiaj??, dzieci ??miej?? si?? i biegaj??."
       "Czu?? intensywny zapach s??omy, ??ajna oraz gnij??cych odpadk??w."
       "Za miasteczkiem znajduje si?? ????ka, z kt??rej dochodz?? odg??osy kr??w.")
      (locations
       (location "????ka" 'grassland)
       (location "wi??zienie" 'prison)
       (location "sklep z instrumentami" 'music-shop)
       (location "las" 'forest)
       (location "sklep z miksturami" 'alchemist-shop))
      (items 'beggar))

(defn on-enter-cave ()
  (if (and (is-in-equipment 'lamp) (not rats-in-cave))
      (progn
        (let old-locations (get 'locations cave))
        (let new-locations (set "dziwny pok??j" 'cave-room old-locations))
        (def cave (set 'locations new-locations cave)))
      (progn
        (let old-locations (get 'locations cave))
        (let new-locations (unset "dziwny pok??j" old-locations))
        (def cave (set 'locations new-locations cave)))))

(room 'alchemist-shop
      (description
       "Wchodzisz do ma??ego pokoiku, wype??nionego r????nymi zio??ami, grzybami i fiolkami z tajemniczymi p??ynami o wszystkich kolorach t??czy."
       "W pokoiku pachnie korzennie, wr??cz piernikowo."
       "Zza lady spogl??da na Ciebie starsza pani.")
      (items 'alchemist)
      (locations
       (location "miasteczko" 'town)))

(room 'cave
      (description
       (cond
         ((not (is-in-equipment 'lamp)) "Jest bardzo ciemno, nic nie widzisz.")
         (rats-in-cave
          (unlines '("Jeste?? w jaskini. Jest ciemno, wi??c odpalasz lamp??."
                     "Schodz??c coraz ni??ej przechodzisz przez korytarz,"
                     "kt??ry w ca??o??ci wype??niony jest szczurami. Przy pr??bie przej??cia dalej"
                     "szczury rzucaj?? si?? na Ciebie i rani?? Ci nogi.")))
         (#t (unlines '("Jeste?? w jaskini."
                        "Schodz??c coraz ni??ej dochodzisz do wyrze??bionego w skale wej??cia do dziwnego pokoju.")))))
      (locations
       (location "????ka" 'grassland))
      (event 'on-enter (fn () (on-enter-cave))))
          
(room 'music-shop
      (description
       "Jeste?? w ma??ym sklepie, wype??nionym r????nymi instrumentami."
       "Czujesz zapach szlachetnego drewna w powietrzu."
       "Za wysok?? lad??, stoi sprzedawca, kt??ry gra pi??kn?? melodie na lutni.")
      (locations
       (location "miasteczko" 'town))
      (items 'musician))

(room 'prison
      (description
       "Jeste?? w ma??ym pokoju o kamiennych ??cianach."
       "Na ??rodku stoi biurku za kt??rym ??pi stra??nik."
       "Za stra??nikiem znajduj?? si?? trzy cele. W jednej z nich znajduje si?? cz??owiek.")
      (locations
       (location "miasteczko" 'town))
      (items 'prisoner))

(room 'grassland
      (description
       "Jeste?? na ????c??, na kt??rej pas?? si?? krowy."
       "Dooko??a rozci??ga si?? intensywna ziele?? trawy."
       "Szelest ro??lin w wietrze przerywaj?? dono??ne muczenie zwierz??t."
       "Za Tob?? znajduje si?? miasteczko, po Twojej prawej widzisz sad, a po prawej"
       "zej??cie do dziwnej groty.")
      (locations
       (location "miasteczko" 'town)
       (location "grota" 'cave)
       (location "sad" 'orchard)))

(room 'orchard
      (description
       "Widzisz mn??stwo jab??onek, grusz, czere??ni i innych drzew."
       "W powietrzu czu?? pi??kny, owocowy zapach."
       "Za Tob?? znajduj?? si?? ????ka.")
      (locations
       (location "????ka" 'grassland))
      (items
       'apple))

(room 'cave-room
      (description
       "Jeste?? w dziwnym pokoju o kamiennych ??cianach."
       "Na ??cianach wisz?? r????nego rodzaju ma??e ostrza, szczypce i druty"
       "Na ??rodku widzisz wielk?? klatk?? na zwierz??ta, w kt??rej zamkni??te s?? ludzkie zw??oki w stanie g??ebokiego rozk??adu."
       "Niedaleko klatki le??y srebrny klucz")
      (locations
       (location "jaskinia" 'cave))
      (items
       'cage 'key))

(room 'forest
      (description
       "Wchodzisz do ciemnego lasu. Pi??kny zapach drzew iglastych oraz mch??w dosi??ga Twojego nosa."
       "Po kr??tkiej drodze po jedynej ??cie??ce natrafiasz na przepa????."
       "Szybko znajdujesz drewniany most i zmierzasz do niego."
       (when (is-in-curr-room 'wolves) (unlines '("Tu?? przed mostem znajduje si?? stado wilk??w." "Gdyby?? mia?? jak???? bro?? by?? mo??e m??g??by?? je zabi??, ale nie mo??esz teraz przej????."))))
      (locations
       (location "miasteczko" 'town))
      (items 'wolves))

(room 'deep-forest
      (description
       "Wchodzisz do bardzo g????bokiego lasu."
       "Powoli przebijaj??c si?? przez kolejne warstwy krzak??w, drzewek i wysokich traw,"
       "dochodzisz do ma??ej polanki, na kt??rej widzisz r????nego rodzaju dziwne ro??liny.")
      (locations
       (location "las" 'forest))
      (items 'red-herbs))

(def current-room 'town)

