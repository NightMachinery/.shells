#!/usr/bin/env -S sbcl --script

(defparameter repl-mode nil)
;; (setq repl-mode t)
;;;
(let ((init-file (merge-pathnames ".sbclrc"
                                  (user-homedir-pathname))))
  (when (probe-file init-file)
    (load init-file)))
;;;
(with-output-to-string (*standard-output* nil)
  (ql:quickload "alexandria")
  (ql:quickload "cl-heredoc"))

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)
;; (defparameter in #>eof>Write whatever (you) "want",
;;   no matter how many lines or what characters until
;; the magic end sequence has been reached!eof)
;;;
(if repl-mode
    (defparameter in #>eof666>{
          "ao3categories":"F/M, Gen",
          "author":"IAM_Kneazle (writing_as_tracey), writing_as_tracey",
          "authorHTML":"<a class='authorlink' href='https://archiveofourown.org/users/writing_as_tracey/pseuds/IAM_Kneazle'>IAM_Kneazle (writing_as_tracey)</a>, <a class='authorlink' href='https://archiveofourown.org/users/writing_as_tracey/pseuds/writing_as_tracey'>writing_as_tracey</a>",
          "authorId":"IAM_Kneazle, writing_as_tracey",
          "authorUrl":"https://archiveofourown.org/users/writing_as_tracey/pseuds/IAM_Kneazle, https://archiveofourown.org/users/writing_as_tracey/pseuds/writing_as_tracey",
          "bookmarked":"",
          "bookmarkprivate":"",
          "bookmarkrec":"",
          "bookmarks":"2184",
          "bookmarksummary":"",
          "bookmarktags":"",
          "byline":"IAM_Kneazle (writing_as_tracey),writing_as_tracey",
          "category":"Harry Potter - J. K. Rowling",
          "chapterslashtotal":"25/?",
          "chapterstotal":"?",
          "characters":"Alastor \"Mad-Eye\" Moody, Albus Dumbledore, Alphard Black, Bartemius Crouch Jr., Charlus Potter, Dirk Cresswell, Dorea Black Potter, Evans family OCs, Hermione Granger, Hogwarts Staff, Hogwarts Students, James Potter, Lily Evans Potter, Marauders (Harry Potter), Order of the Phoenix, Peter Pettigrew, Petunia Evans Dursley, Regulus Black, Remus Lupin, Sean Bowes - Author created OMC, Severus Snape, Sirius Black",
          "collections":"AboutTime, Fics I Love, Fics I will sell my Soul for, HP - \u5fc5\u8aad (\u3072\u3064\u3069\u304f), I Found These Masterpieces And Fell In Love, I Read This Instead of Sleeping, Lady's collection of PERFECT fics., Read again: Harry Potter, Restless Wonders, SakurAlpha's Fic Rec of Pure how did you create this you amazing bean, Stories I will reread, alREADyHPfics: Harry Potter fics that I have read, did someone say time travel, i solemnly swear i up to no good",
          "comments":"2757",
          "cover_image":"",
          "dateCreated":"2021-06-19 19:05:19",
          "datePublished":"2018-02-09",
          "dateUpdated":"2021-05-01",
          "description":"<div class=\"userstuff\">\n              <p>James Potter went five years at Hogwarts without realizing Lily had a little sister. Hermione would have preferred if he never realized she existed. Now she's stuck, in Potter's circle of awareness, and maintaining the timeline. Not like he makes it easy, or something.</p>\n<p></p><div class=\"center\">\n  <p>\n    \n  </p>\n</div>\n            </div>",
          "extratags":"FanFiction",
          "fandoms":"Harry Potter - J. K. Rowling",
          "formatext":".epub",
          "formatname":"epub",
          "freeformtags":"(attempting to write) BAMF Hermione Granger, Alternate Universe - Canon Divergence, DC-style magic (John Constantine), First Wizard War (Harry Potter), Fullmetal Alchemist: Brotherhood style magic, Horcrux Hunting, Horcruxes, MCU-style magic (Dr Strange), Pre-War, Ravenclaw Hermione Granger, The Deathly Hallows, Time Travel, Time Travel Fix-It, Transmutation, Tropes, genre: action/adventure, genre: romance, genre: slice of life, no beta we die like men, trigger warning - torture",
          "freefromtags":"(attempting to write) BAMF Hermione Granger, Alternate Universe - Canon Divergence, DC-style magic (John Constantine), First Wizard War (Harry Potter), Fullmetal Alchemist: Brotherhood style magic, Horcrux Hunting, Horcruxes, MCU-style magic (Dr Strange), Pre-War, Ravenclaw Hermione Granger, The Deathly Hallows, Time Travel, Time Travel Fix-It, Transmutation, Tropes, genre: action/adventure, genre: romance, genre: slice of life, no beta we die like men, trigger warning - torture",
          "genre":"(attempting to write) BAMF Hermione Granger, Alternate Universe - Canon Divergence, DC-style magic (John Constantine), F/M, First Wizard War (Harry Potter), Fullmetal Alchemist: Brotherhood style magic, Gen, Horcrux Hunting, Horcruxes, MCU-style magic (Dr Strange), Pre-War, Ravenclaw Hermione Granger, The Deathly Hallows, Time Travel, Time Travel Fix-It, Transmutation, Tropes, genre: action/adventure, genre: romance, genre: slice of life, no beta we die like men, trigger warning - torture",
          "hits":"169393",
          "kudos":"6655",
          "langcode":"en",
          "language":"English",
          "lastupdate":"Last Update Year/Month: 2021/05, Last Update: 2021/05/01",
          "numChapters":"25",
          "numWords":"229,569",
          "output_css":"\nbody { background-color: #ffffff;\ntext-align: justify;\nmargin: 2%;\nadobe-hyphenate: none; }\npre { font-size: x-small; }\nh1 { text-align: center; }\nh2 { text-align: center; }\nh3 { text-align: center; }\nh4 { text-align: center; }\nh5 { text-align: center; }\nh6 { text-align: center; }\n.CI {\ntext-align:center;\nmargin-top:0px;\nmargin-bottom:0px;\npadding:0px;\n}\n.center   {text-align: center;}\n.cover    {text-align: center;}\n.full     {width: 100%; }\n.quarter  {width: 25%; }\n.smcap    {font-variant: small-caps;}\n.u        {text-decoration: underline;}\n.bold     {font-weight: bold;}\n.big { font-size: larger; }\n.small { font-size: smaller; }",
          "output_filename":"Yesterday is Tomorrow (everything is connected)-ao3_13625910.epub",
          "publisher":"archiveofourown.org",
          "rating":"Teen And Up Audiences",
          "restricted":"",
          "sectionUrl":"https://archiveofourown.org/works/13625910",
          "series":"",
          "series00":"",
          "series00HTML":"",
          "series00Url":"",
          "series01":"",
          "series01HTML":"",
          "series01Url":"",
          "series02":"",
          "series02HTML":"",
          "series02Url":"",
          "series03":"",
          "series03HTML":"",
          "series03Url":"",
          "seriesHTML":"",
          "seriesUrl":"",
          "ships":"Hermione Granger/James Potter, Petunia Evans/OC",
          "site":"archiveofourown.org",
          "siteabbrev":"ao3",
          "status":"In-Progress",
          "storyId":"13625910",
          "storyUrl":"https://archiveofourown.org/works/13625910",
          "title":"Yesterday is Tomorrow (everything is connected)",
          "titleHTML":"<a class='titlelink' href='https://archiveofourown.org/works/13625910'>Yesterday is Tomorrow (everything is connected)</a>",
          "version":"4.3.0",
          "warnings":"Creator Chose Not To Use Archive Warnings, No Archive Warnings Apply",
          "words_added":"",
          "zchapters":[
          [
          1,
          {
          "date":"2018-02-09",
          "title":"1. Everything is Connected",
          "url":"https://archiveofourown.org/works/13625910/chapters/31287798"
          }
          ],
          [
          2,
          {
          "date":"2018-02-11",
          "title":"2. It's a Curse",
          "url":"https://archiveofourown.org/works/13625910/chapters/31341882"
          }
          ],
          [
          3,
          {
          "date":"2018-02-18",
          "title":"3. Take Control of Your Life",
          "url":"https://archiveofourown.org/works/13625910/chapters/31537884"
          }
          ],
          [
          4,
          {
          "date":"2018-05-22",
          "title":"4. Choose the Path",
          "url":"https://archiveofourown.org/works/13625910/chapters/34048893"
          }
          ],
          [
          5,
          {
          "date":"2018-07-15",
          "title":"5. Exile and Friendly Smiles",
          "url":"https://archiveofourown.org/works/13625910/chapters/35479902"
          }
          ],
          [
          6,
          {
          "date":"2018-08-14",
          "title":"6. Einstein's General Theory of Relativity",
          "url":"https://archiveofourown.org/works/13625910/chapters/36442824"
          }
          ],
          [
          7,
          {
          "date":"2018-08-20",
          "title":"7. What's the Future Looking like?",
          "url":"https://archiveofourown.org/works/13625910/chapters/36621228"
          }
          ],
          [
          8,
          {
          "date":"2018-09-20",
          "title":"8. Heavy is the Future",
          "url":"https://archiveofourown.org/works/13625910/chapters/37458323"
          }
          ],
          [
          9,
          {
          "date":"2018-11-11",
          "title":"9. Choices",
          "url":"https://archiveofourown.org/works/13625910/chapters/38882072"
          }
          ],
          [
          10,
          {
          "date":"2018-12-27",
          "title":"10. The Hardest Part",
          "url":"https://archiveofourown.org/works/13625910/chapters/40390217"
          }
          ],
          [
          11,
          {
          "date":"2019-06-18",
          "title":"11. Throw Yourself In - Part A",
          "url":"https://archiveofourown.org/works/13625910/chapters/45831193"
          }
          ],
          [
          12,
          {
          "date":"2019-06-22",
          "title":"12. Throw Yourself In (Scar Tissue) - Part B",
          "url":"https://archiveofourown.org/works/13625910/chapters/45955255"
          }
          ],
          [
          13,
          {
          "date":"2019-10-27",
          "title":"13. Hold On",
          "url":"https://archiveofourown.org/works/13625910/chapters/50472584"
          }
          ],
          [
          14,
          {
          "date":"2020-03-12",
          "title":"14. Overcome",
          "url":"https://archiveofourown.org/works/13625910/chapters/55300132"
          }
          ],
          [
          15,
          {
          "date":"2020-04-23",
          "title":"15. The Future Influences the Past",
          "url":"https://archiveofourown.org/works/13625910/chapters/57194809"
          }
          ],
          [
          16,
          {
          "date":"2020-07-07",
          "title":"16. A Beacon in the Darkness",
          "url":"https://archiveofourown.org/works/13625910/chapters/60884656"
          }
          ],
          [
          17,
          {
          "date":"2020-07-30",
          "title":"17. Our Actions Can Change Things",
          "url":"https://archiveofourown.org/works/13625910/chapters/62160454"
          }
          ],
          [
          18,
          {
          "date":"2020-08-13",
          "title":"18. The Past Doesn't Want to be Changed",
          "url":"https://archiveofourown.org/works/13625910/chapters/62882821"
          }
          ],
          [
          19,
          {
          "date":"2020-08-17",
          "title":"19. Looking at the Present",
          "url":"https://archiveofourown.org/works/13625910/chapters/63111838"
          }
          ],
          [
          20,
          {
          "date":"2020-08-25",
          "title":"20. Everything has a Purpose",
          "url":"https://archiveofourown.org/works/13625910/chapters/63497632"
          }
          ],
          [
          21,
          {
          "date":"2020-11-14",
          "title":"21. No Longer Bound By Time",
          "url":"https://archiveofourown.org/works/13625910/chapters/67393150"
          }
          ],
          [
          22,
          {
          "date":"2020-12-24",
          "title":"22. Things Done in Secret",
          "url":"https://archiveofourown.org/works/13625910/chapters/69325647"
          }
          ],
          [
          23,
          {
          "date":"2021-01-14",
          "title":"23. All What Is in Between",
          "url":"https://archiveofourown.org/works/13625910/chapters/70518852"
          }
          ],
          [
          24,
          {
          "date":"2021-04-16",
          "title":"24. Who You Are Tomorrow",
          "url":"https://archiveofourown.org/works/13625910/chapters/75810212"
          }
          ],
          [
          25,
          {
          "date":"2021-05-01",
          "title":"25. Give Them a Secret",
          "url":"https://archiveofourown.org/works/13625910/chapters/76638515"
          }
          ]
          ]
          }
          eof666)

    (defparameter in (alexandria:read-stream-content-into-string *standard-input*)))

(defparameter d (json-parse in))
(defun v (key)
  (json-get d key))
;;;
(progn
  (let ((title (v "title"))
        (url (v "storyUrl")))
    (org-link-write :url url :title title)
    (ec))

  (let ((out_stream *standard-output*)
        (accessor #'v)
        (keys '(;; "threadmarks_title"
                "author" "authorUrl"
                "series" "seriesUrl"
                "status" "threadmarks_status"
                "numWords" "estimatedWords"
                "chapterslashtotal" "numChapters"
                "datePublished" "dateUpdated"
                "stars" "likes" "dislikes" "kudos" "reviews" "favs" "follows"
                "views" "total_views" "hits"
                "ships"
                "fandoms"
                "ao3categories"
                "freeformtags" "forumtags" "genre" "extratags" ;; `freefromtags' is a compat-shim for `freeformtags'
                "rating"
                "characters"
                "short_description" "threadmarks_description"
                "groups"
                "prequel" "sequels"
                ;; "comment_count"
                "authorLastLogin"
                "storyId"
                "cover_image")))
    (org-properties-write :keys keys :accessor accessor :out_stream out_stream))

  (let ((desc (v "description")))
    (org-quote-write :content (html2org desc))
    ))
;;;
