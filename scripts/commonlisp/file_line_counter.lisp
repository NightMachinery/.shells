#!/usr/bin/env -S sbcl_batteriful --script

(defparameter in
  (if *repl-mode*
      "/Users/evar/cellar/notes/bookmarks/play/videogames/2d fighting.org
/Users/evar/cellar/notes/bookmarks/play/videogames/9p.org
/Users/evar/cellar/notes/bookmarks/play/videogames/Amanita Design.org
/Users/evar/cellar/notes/bookmarks/play/videogames/action RPG, ARPG.org
/Users/evar/cellar/notes/bookmarks/play/videogames/adventure/3D.org
/Users/evar/cellar/notes/bookmarks/play/videogames/adventure/timeloop.org
/Users/evar/cellar/notes/bookmarks/play/videogames/arcade, space shooter.org
/Users/evar/cellar/notes/bookmarks/play/videogames/arkanoid.md
/Users/evar/cellar/notes/bookmarks/play/videogames/card-games.org
/Users/evar/cellar/notes/bookmarks/play/videogames/casual.org
/Users/evar/cellar/notes/bookmarks/play/videogames/coop.org
/Users/evar/cellar/notes/bookmarks/play/videogames/curated by friends.org
/Users/evar/cellar/notes/bookmarks/play/videogames/first-person shooter, FPS.org
/Users/evar/cellar/notes/bookmarks/play/videogames/free-to-play/card-games.org
/Users/evar/cellar/notes/bookmarks/play/videogames/genres explore.md
/Users/evar/cellar/notes/bookmarks/play/videogames/ipad.org
/Users/evar/cellar/notes/bookmarks/play/videogames/jrpg.org
/Users/evar/cellar/notes/bookmarks/play/videogames/macOS.org
/Users/evar/cellar/notes/bookmarks/play/videogames/management simulation.org
/Users/evar/cellar/notes/bookmarks/play/videogames/multiplayer.org
/Users/evar/cellar/notes/bookmarks/play/videogames/nostalgia old games.org
/Users/evar/cellar/notes/bookmarks/play/videogames/opensource.org
/Users/evar/cellar/notes/bookmarks/play/videogames/party.md
/Users/evar/cellar/notes/bookmarks/play/videogames/platformer/2D.org
/Users/evar/cellar/notes/bookmarks/play/videogames/platformer/3D.org
/Users/evar/cellar/notes/bookmarks/play/videogames/point and click/gen.org
/Users/evar/cellar/notes/bookmarks/play/videogames/point and click/hidden objects.org
/Users/evar/cellar/notes/bookmarks/play/videogames/puzzle/2D.org
/Users/evar/cellar/notes/bookmarks/play/videogames/puzzle/computer code, programming, assembly, circuits.org
/Users/evar/cellar/notes/bookmarks/play/videogames/puzzle/gen.org
/Users/evar/cellar/notes/bookmarks/play/videogames/rationalish.org
/Users/evar/cellar/notes/bookmarks/play/videogames/rts realtime strategy.md
/Users/evar/cellar/notes/bookmarks/play/videogames/sidescroller.org
/Users/evar/cellar/notes/bookmarks/play/videogames/story-rich.org
/Users/evar/cellar/notes/bookmarks/play/videogames/terminal, tui.org
/Users/evar/cellar/notes/bookmarks/play/videogames/text adventures, interactive fiction.org
/Users/evar/cellar/notes/bookmarks/play/videogames/timeLoop.org
/Users/evar/cellar/notes/bookmarks/play/videogames/timetravel.org
/Users/evar/cellar/notes/bookmarks/play/videogames/tower defense.org
/Users/evar/cellar/notes/bookmarks/play/videogames/turn-based/2D.org
/Users/evar/cellar/notes/bookmarks/play/videogames/turn-based/RPG.org
/Users/evar/cellar/notes/bookmarks/play/videogames/visualnovel, VN.org
"
      (alexandria:read-stream-content-into-string *standard-input*)))

(setf lparallel:*kernel* (lparallel:make-kernel 16 :name "custom-kernel"))

(defun file-line-counter (files)
  (letd ((all-lines 0)
         (files-augmented
          (lparallel:pmap
           'vector
           (lambda (file)
             (letd ((file-path
                     (make-pathname :directory "" :name file)) ;; Needed for escaping the glob (wild) characters
                    (file-str (alexandria::read-file-into-byte-vector file-path))
                    (line-count (alexandria::count
                                 (char-int #\Newline)
                                 file-str)))
               (list file line-count)))
           files
           ))
         (line-count-extractor
          (lambda (x) (cadr x)))
         (all-lines (reduce
                     #'+
                     files-augmented
                     :key line-count-extractor
                     :initial-value all-lines)))
    (sort files-augmented #'>
          :key line-count-extractor)
    (format t "All lines: ~d~%" all-lines)
    (loop for f across files-augmented do
      (letd ((line-count (cadr f))
             (file-name (car f)))
        (format t "~d: ~a~%" line-count file-name)))))

(file-line-counter (ppcre:split #\Newline in))

(lparallel:end-kernel :wait t)
