;;;; My video game project
;;;; goal of the game is to kill the tyrant-toad. To do this, you need to feed all the monsters to get ingredients and combine and cook stuff to kill the toad.
;;;;
;;;;
;;;; Name: Lancen Daclison  Date turned in: 4/24/17
;;;; Course: ICS 313 Project
;;;; File:cooking_quest.lisp

(defconstant +ID+ "Lancen Daclison")   ;;; global variable, that identifies author of file.
(defun ID(*course* *homework*)         ;;; prints the name, course, and homework of student.
(princ "Name: ")                       ;;; Prints out "Name" (no space)
(princ +ID+)                           ;;; Prints out name of student
(terpri)                               ;;; makes a line of space
(princ "Course: " )                    ;;; prints out "course"
(write *course*)                       ;;; writes out input from user
(terpri)                               ;;; makes a line of space
(princ "HOMEWORK: ")                   ;;; prints out "homework"
(write *homework*)                     ;;; writes out input from user
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; LOCATION DESCRIPTIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *nodes* '((pubic-forest (you are in the pubic-forest.                ;;; a location called "living room"
                            the trees tickle your body.Smells like sasquage.
                            You see a 10-foot-giant but are not afraid.))           ;;; description of what is in living room
                        (da-fishing-spot (you are in da-fishing-spot.                  ;;; a location called "garden"
                            there is a pond in front of you.))                   ;;; description of what is in garden
                        (town-market(you are in the town-market.                 ;;; a location called "bathroom"
                             the place is filled with people and
                              wonderous things.))  ;;; description of what is in bathroom
                        (wizard-house (you are in the wizard-house.                            ;;; a location called "attic"
                            You see a wizard watching tv and hugging 2 body pillows
                                 of what looks to be a high school anime girl.))
                         (attic (you are in the attic. It's dark
                                  and it smells like ass.))
                          (bathroom (you are in the bathroom. You see a
                                    mistreated toilet. It smells like despair.))
                           (cooking-fortress(you are in the cooking fortress.You
                                     can cook safely here.))
                           (haole-fishing-spot(you are in the haole-fishing-spot
                              The water is murky. The fishes are no where to be seen.))
                           (chin-chin-forest(you are in the chin-chin-forest.
                               You see giant mushrooms protruding through the ground.))
                           (oishii-fishing-spot(you are in the oishii-fishing-spot. You
                            see tasty fat fish in a clear pond.))
                           (gangstah-cave(you are in the gangstah-cave. You see a gangstah-bear
                                        mauling a man on the ground.))
                           (king-palace(you are in the king-palace.You see a tyrant-frog the size of a 5 story house. ))

))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LOCATION AREAS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun describe-location (location nodes)                                        ;;; function that describes location
   (cadr (assoc location nodes)))                                                ;;; prints out nodes for user to read about location

(defparameter *edges* '((town-market (da-fishing-spot west path)                          ;;; edges describe path to location, this one describes paths from living room
                                     (wizard-house north path)
                                     (cooking-fortress east path))
                        (da-fishing-spot(town-market east path)
                                         (pubic-forest north path))
                        (pubic-forest(da-fishing-spot south path))
                        (wizard-house (town-market south path)
                                      (attic upstairs ladder)
                                      (bathroom west path))
                        (attic (wizard-house downstairs ladder))
                        (bathroom (wizard-house east path))
                        (cooking-fortress(town-market west path)
                                         (haole-fishing-spot south path)
                                         (chin-chin-forest east path))
                        (haole-fishing-spot(cooking-fortress north path))
                        (chin-chin-forest(cooking-fortress west path)
                                         (oishii-fishing-spot south path)
                                         (gangstah-cave north path))
                        (oishii-fishing-spot(chin-chin-forest north path)
                                             (king-palace east path))
                        (gangstah-cave(chin-chin-forest south path))
                        (king-palace(oishii-fishing-spot west path))

                                                                    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;                              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;   how to connect places      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;                              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun describe-path (edge)                                                      ;;; function that describes the path to user.
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))                     ;;; the format that will tell the user what paths are available in location

(defun describe-paths (location edges)                                           ;;; function that describes the path to user.
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))        ;;; the format that will tell the user what paths are available in location


;;;;;;;;;;;;;;;;;;;;;;;;;;;;                          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;  OBJECT LOCATIONS        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;                          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *objects* '(poop egg mushroom stick rope fish oil_vial mystery_meat bread fishing_rod poison_mushroom poison_omelette fried_fish steak_sandwich recipe_book toxic_spray))                       ;;; objects that exists in the world

(defparameter *object-locations* '(;(poop bathroom)                         ;;; object location of whiskey
                                   (egg town-market)
                                   (toxic_spray town-market)
                                   ;(poison_omelette cooking-fortress)
                                    (mushroom chin-chin-forest)
                                    (stick da-fishing-spot)
                                    (rope attic)
                                    ;(fish oishii-fishing-spot)
                                    (oil_vial pubic-forest)
                                    ;(mystery_meat gangstah-cave)
                                    (bread cooking-fortress)
                                    (recipe_book attic)
                                  ))                             ;;; object location of poop

(defun objects-at (loc objs obj-loc)                                             ;;; a function  that describes where the object is at to user
   (labels ((is-at (obj)                                                         ;;; creates the label of object
              (eq (cadr (assoc obj obj-loc)) loc)))                              ;;; compares if object is still location or not
       (remove-if-not #'is-at objs)))                                            ;;; if object is out of location,it will take it out of description

(defun describe-objects (loc objs obj-loc)                                       ;;; a function that describes where the object is.
    (labels ((describe-obj (obj)                                                 ;;; describes object
                `(you see a ,obj on the floor.)))                                ;;; format that will print out where an object is to user.
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))   ;;; puts everything together to print out for user. Kind of like a string.


;;;;;;;                ;;;;;;;;;;;;;;;;;
;;;;;;; STARTING PLACE ;;;;;;;;;;;;;;;;;
;;;;;;;                ;;;;;;;;;;;;;;;;;
(defparameter *location* 'town-market)                                           ;;; where you first start off.



;;;;;;;;;;;;;;;;;;;;;;;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;COMMON COMMANDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun look ()                                                                   ;;; function that tells the scenerio of where the person currently is.
  (append (describe-location *location* *nodes*)                                 ;;; combines functions location, nodes
          (describe-paths *location* *edges*)                                    ;;; also combines function to describe location and paths
          (describe-objects *location* *objects* *object-locations*)))           ;;; also combines function to describe objects and their placements.


(defun walk (direction)                                                          ;;; a function that allows user to change location
  (labels ((correct-way (edge)                                                   ;;; outcome where you it is possible to walk a certain path
             (eq (cadr edge) direction)))                                        ;;; checking if direction of edge is possible.
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))       ;;; allowing correct path to happen, so can move to next location or whatever it points to.
      (if next                                                                   ;;; conditional statement on next location
          (progn (setf *location* (car next))                                    ;;; setting the current location to next location
                 (look))                                                         ;;; has to look because new location
          '(you cannot go that way.)))))                                         ;;; if you cannot go to a certain path.

(defun pickup (object)                                                           ;;; a function that allows user to pick up stuff.
  (cond ((member object (objects-at *location* *objects* *object-locations*))    ;;; checking the object in the location
         (push (list object 'body) *object-locations*)                           ;;; taking the object in removing it from the location and into inventory
         `(you are now carrying the ,object))                                    ;;; output to user, so they know they can hold something.
	  (t '(you cannot get that.))))                                                ;;; output to user, they cannot hold something.


(defun inventory ()                                                              ;;; function that holds objects you pick up in world.
  (cons 'items- (objects-at 'body *objects* *object-locations*)))                ;;; adds item to inventory.

(defun have (object)                                                             ;;; function that shows you hold the object
    (member object (cdr (inventory))))                                           ;;; output that shows you have the object in inventory




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;  wizards_game part2  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;USE THIS TO MAKE GAME EASIER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (defun game-repl ()                                                                    ;;;function that allows you to play game without parenthesis
            (let ((cmd (game-read)))                                                           ;;;calls the game reader
                (unless (eq (car cmd) 'quit)                                                   ;;;quits if not the command
                    (game-print (game-eval cmd))                                               ;;;keep running unless game quits
                    (game-repl))))

;;;;;;;;;;;;TO READ GAME;;;;;;;;;;;;;;;;;;;;;;;;;
        (defun game-read ()                                                                    ;;;function that reads game
            (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))          ;;;reads from user input
                 (flet ((quote-it (x)                                                          ;;;reads from user input
                            (list 'quote x)))                                                  ;;;reads from user input
                     (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))                         ;;;reads from user input

        (defparameter *allowed-commands* '(look walk pickup inventory exit help h ?))

        (defun game-eval (sexp)                                                               ;;;function that evaluates if command is valid
            (if (member (car sexp) *allowed-commands*)                                        ;;;if command is valid
                (eval sexp)                                                                   ;;;okay
                '(i do not know that command.)))                                              ;;; if not then send error message to user

        (defun tweak-text (lst caps lit)                                                      ;;;function that tweaks text of user to all CAPS
          (when lst                                                                           ;;;condition statement starting from first
            (let ((item (car lst))                                                            ;;;assign to beginning
                  (rest (cdr lst)))                                                           ;;;assign to end
              (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))               ;;;condition of assigning items
                    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))        ;;;getting members of List
                    ((eql item #\") (tweak-text rest caps (not lit)))                         ;;;checking if they equal
                    (lit (cons item (tweak-text rest nil lit)))                               ;;;checking for characters
                    (caps (cons (char-upcase item) (tweak-text rest nil lit)))                ;;;caps the characters in list
                    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

        (defun game-print (lst)                                                               ;;; a fuction that prints stuff
            (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
            (fresh-line))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                 ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GAME-ACTIONS (FOR COMMANDS THAT WILL BE REUSED) ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                 ;;;;;;;;;;;;;;


(defmacro game-action (command subj obj place &body body)                                  ;;;macro function that emulates a game function
  `(progn (defun ,command (subject object)                                                 ;;;sets up variables neeeded to act as a game-action
            (if (and (eq *location* ',place)                                               ;;; if the location is valid
                     (eq subject ',subj)                                                   ;;; if subject is valid
                     (eq object ',obj)                                                     ;;; if objet is valid
                     (have ',subj))                                                        ;;; if player has the object
                ,@body                                                                     ;;; emulate body of code
            '(i cant ,command like that.)))                                                ;;; emulation of error message
          (pushnew ',command *allowed-commands*)))                                         ;;; push to this whole functtion to allowed commands, so you can use it



          (defmacro game-action2 (command subj place &body body)                                  ;;;macro function that emulates a game function
            `(progn (defun ,command (subject)                                                 ;;;sets up variables neeeded to act as a game-action
                      (if (and (eq *location* ',place)                                               ;;; if the location is valid
                               (eq subject ',subj)                                                   ;;; if subject is valid
                                                           ;;; if objet is valid
                               (have ',subj))                                                        ;;; if player has the object
                          ,@body                                                                     ;;; emulate body of code
                      '(i cant ,command like that.)))                                                ;;; emulation of error message
                    (pushnew ',command *allowed-commands*)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;INTERACTIONS WITH THE MONSTERS (ENEMIES) ;;;;;;;;;;;;;;;;;

                   (game-action throw_food steak_sandwich gangstah-bear gangstah-cave
                                (cond
                                      ((have 'steak_sandwich)
                                      (setq *objects* (remove 'steak_sandwich *objects*))
                                      (pushnew 'poop *objects*)                           ;;;push poison poop monster to objects
                                      (pushnew '(poop gangstah-cave) *object-locations*)
                                      '(the gangstah-bear grabs the steak_sandwich and scarfs it down.
                                                  He thanks you and poops on your feet and goes back to mauling that poor man on the ground.
                                                    He sure is a gangstah for lyfe.))

                                      (t '(gangstah-bear wants a steak_sandwh))))


                   (game-action feed fried_fish 10-foot-giant pubic-forest
                        (cond
                              ((have 'fried_fish)
                              (setq *objects* (remove 'fried_fish *objects*))
                              (pushnew 'mystery_meat *objects*)                           ;;;push poison poop monster to objects
                              (pushnew '(mystery_meat pubic-forest) *object-locations*)

                              '(the 10-foot-giant grabs the oil fish and scarfs it down.
                                      He turns to you and says he loves it. He slathers the oil on his
                                        body. He drops a mystery_meat.))

                         (t '(10-foot-giant licks his lips asking for fried_fish))))



(defparameter *poison_omelette-spike* nil)

                         (game-action serve poison_omelette tyrant-frog king-palace
                                    (cond ((not *poison_omelette-spike*)
                                    '(the frog eats it laughs and eats you. You die. You bring dishonor to your family and you die.))
                                          ((have 'poison_omelette)
                                          (setq *objects* (remove 'poison_omelette *objects*))
                                          '(The toad munches on the poison_omelette. He enjoys it but chokes and dies.
                                              The evil toad is dead. YOU Win the game!))

                                           (t '(tyrant-frog wants some mushroom.))))







            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;part1 (macros);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            (defmacro create-object (name place)
                 `(cond
                   ((and (not-object-member ',name)                                    ;;;check if exists
                    (null (not-node-member ',place)))                                  ;;;check if place exists
                    (pushnew ',name *objects*)                                         ;;;push new object
                    (pushnew '(,name ,place) *object-locations*))                      ;;;push new location
                      (t '(Object exists))
                   ))


            (defmacro create-location (place &body body)
              `(cond
                ((not-node-member ',place)                                  ;;;check whether new place  exist in *nodes*
                 (pushnew '(,place (,@body)) *nodes*)                       ;;;push to nodes
                 (pushnew '(,place) *edges*)                                ;;;push the new location to node
                 '(new location has been created))                          ;;print out success message to user
                (t '(The place has already existed.))))                     ;;;message that says already exist


            (defmacro create-path (place1 place2 path &optional dir1 dir2)
              `(cond ((null ',dir1) '(Place enter direction of the ,place1))          ;;;if the direction for path 1 is not there
                    ((null ',dir2) '(Place enter direction of the ,place2))           ;;;same logic for part 2
                    ((or (not-node-member ',place1)(not-node-member ',place2))        ;;;if not nodes
                     '(location 1 or 2 doesn not exist))
                        (t (cond
                             ((check-direction ',dir1 (cdr (assoc ',place1 *edges*)))                   ;;;check for existing connection for direction1
                              '(,dir1 direction of the ,place1 has been connected to other locations))  ;;;message to user
                             ((check-direction ',dir2 (cdr (assoc ',place2 *edges*)))                   ;;;check for exisitng connection for direction2
                              '(,dir2 direction of the ,place2 has been connected to other locations))  ;;;message to user

                         (t (progn(pushnew '(,place2 ,dir1 ,path) (cdr (assoc ',place1 *edges*)))       ;;;push the second location and path
                                  (pushnew '(,place1 ,dir2 ,path) (cdr (assoc ',place2 *edges*))))      ;;;push first location and path
                            '(the new path between ,place1 and ,place2 has been created!))              ;;;message to user
                       ))
            ))



            ;;check the new object
            (defun not-object-member (name)
              (if (list-member2 name *objects*) nil                  ;;;return nil when list-member1 return t
                t))                                                  ;;;else return t

            ;;check the new place
            (defun not-node-member (place)
              (if (list-member1 place *nodes*) nil                  ;;;return nil when list-member1 return t
                t))                                                 ;;;else return turn

            ;;check the location in *nodes*
            (defun list-member1(value list)
              (if (null list) nil                                   ;;;return nil when list is empty
                (if (eql value (caar list)) t                       ;;;else return t when the value equal to (caar list)
                  (list-member1 value (cdr list))                   ;;else recursion
                  )))

            ;;check the object in *objects*
            (defun list-member2(value list)
              (if (null list) nil                                   ;;return nil when list is empty
                (if (eql value (car list)) t                        ;;else return t when the value in the list
                  (list-member2 value (cdr list))                   ;;else recursion
                  )))


            ;;check the direction of the edge of the location
            (defun check-direction (dir place-edge)
              (if (null place-edge) nil                             ;;return nil when place-edge is empty
                (if (eql dir (cadar place-edge)) t                  ;;else return t when the dir
                  (check-direction dir (cdr place-edge))
                  )))


            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;part2  (spel +help);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     RECIPES LIST        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                ;combine fish + oil_vile  = fried fish
                                (game-action fry fish oil_vial cooking-fortress


                                  (cond ((have 'fish)
                                         (setq *objects* (remove 'fish *objects*))
                                         (setq *objects* (remove 'oil_vial *objects*))
                                         (pushnew 'fried_fish *objects*)
                                         (pushnew '(fried_fish cooking-fortress) *object-locations*)
                                         (pickup 'fried_fish)
                                         '(you cooked a fried_fish.
                                            Wow it is really oilly.))                 ;;;message to user
                                        (t'(you do not have a poop or mushroom.))
                                        ))



            ;combine poop + mushroom  = poison-mushroom
            (game-action stew poop mushroom cooking-fortress


              (cond ((have 'poop)
                     (setq *objects* (remove 'poop *objects*))
                     (setq *objects* (remove 'mushroom *objects*))
                     (pushnew 'poison_mushroom *objects*)
                     (pushnew '(poison_mushroom cooking-fortress) *object-locations*)
                     (pickup 'poison_mushroom)
                     '(you cooked a poison_mushroom.
                        Maybe a certain frog would like this.))
                    (t'(you do not have a poop or mushroom.))
                    ))


                    ;combine poison_mushroom + egg  = poison-omelete
                    (game-action grill poison_mushroom egg cooking-fortress


                      (cond ((have 'poison_mushroom)
                             (setq *objects* (remove 'poison_mushroom *objects*))
                             (setq *objects* (remove 'egg *objects*))
                             (pushnew 'poison_omelette *objects*)
                             (pushnew '(poison_omelette cooking-fortress) *object-locations*)
                             (pickup 'poison_omelette)
                             '(you cooked a poison_omelette.
                                Maybe a certain frog would like this.))
                            (t'(you do not have a poison_mushroom or egg.))
                            ))


                    ;combine mystery_meat + bread  = steak_sandwich
                    (game-action bake mystery_meat bread cooking-fortress


                      (cond ((have 'mystery_meat)
                             (setq *objects* (remove 'mystery_meat *objects*))
                             (setq *objects* (remove 'bread *objects*))
                             (pushnew 'steak_sandwich *objects*)
                             (pushnew '(steak_sandwich cooking-fortress) *object-locations*)
                             (pickup 'steak_sandwich)
                             '(you cooked a steak_sandwich.
                                Maybe a certain bear would like this.))
                            (t'(you do not have a poop or mushroom.))
                            ))



                            ;combine stick + rope  = fishing_rod
                            (game-action craft stick rope cooking-fortress


                              (cond ((have 'stick)
                                     (setq *objects* (remove 'stick *objects*))
                                     (setq *objects* (remove 'rope *objects*))
                                     (pushnew 'fishing_rod *objects*)
                                     (pushnew '(fishing_rod cooking-fortress) *object-locations*)
                                     (pickup 'fishing_rod)
                                     '(you created a fishing rod.
                                        you can now get fish!))                 ;;;message to user
                                    (t'(you don't have a stick and rod no more.))
                                    ))


                                  ;(defparamter *recipe-read* nil)
                                    ;to view recepies
                                    (game-action2 view recipe_book cooking-fortress


                                      (cond((have 'recipe_book)
                                             '(FRY+ fish + oil_vial = fried_fish ~~~~~~~~~~~~~~~~~
                                                bake + mystery_meat + bread= steak_sandwich ~~~~~~~~~~~
                                                stew + poop + mushroom =   poison_mushroom~~~~~~~~~~~~
                                                grill +poison_mushroom + egg = poison_omelette )))

                                             )



                                             ;(defparamter *recipe-read* nil)
                                               ;to view fish for fish
                                               (game-action2 fish fishing_rod oishii-fishing-spot


                                                 (cond((have 'fishing_rod)
                                                 (pushnew 'fish *objects*)                           ;;;push poison poop monster to objects
                                                 (pushnew '(fish oishii-fishing-spot) *object-locations*)    ;;;push poison poop monster to room
                                                 (pickup 'fish)                                                 ;;;check body if you have a poop
                                                        '(you caught a juicy fat fish!)))

                                                        )


;;;;;;;;;;;;;;;;;;;;;;;;; WIN CONDITION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                   (game-action spike toxic_spray poison_omelette cooking-fortress
              (if (and (have 'poison_omelette) (not *poison_omelette-spike*))
              (progn (setf *poison_omelette-spike* 't)
              '(the poison_omelette is drugged and lethal. it smells like flowers))                                ;;; sucess message
              '(you do not have poison_omelette)))



            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;help commands;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; list the allow command
            (defun help() `(please use the following commands-> ,@*allowed-commands*))
            (defun h() (help))                       ;you can call for help by pressing 'h'
            (defun ?() (help))                       ;you can call for help by pressing '?'
