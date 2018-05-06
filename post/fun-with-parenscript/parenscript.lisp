(in-package :fun-with-ps)

;;;; Example 1: Toggle something

(defun normalize-display (display)
  "Convert the empty string to the string 'block', otherwise return
`display' unmodified."
  (if (string= display "") "block" display))

(defmacro swap (a b)
  "Swap the values of variables `a' and `b'.

Note that `a' and `b' are both evaluated twice, and must be valid
JavaScript l-values."
  (with-ps-gensyms (tmp)
    `(let ((,tmp ,a))
       (setf ,a ,b)
       (setf ,b ,tmp))))

(defun toggle-content (id-a id-b id-link)
  "Toggle the display of the elements given by `id-a' and `id-b'."
  (let* ((link (chain document (get-element-by-id id-link)))
	 (element-a (chain document (get-element-by-id id-a)))
	 (element-b (chain document (get-element-by-id id-b)))
	 (a-display (normalize-display (@ element-a style display)))
	 (b-display (normalize-display (@ element-b style display)))
	 (link-text (concatenate 'string "Show "
				 (if (equal a-display "block") id-a id-b)))
	 (state (concatenate 'string a-display "," b-display)))
    (when (not (or (equal state "block,none")
		   (equal state "none,block")))
      (throw (concatenate 'string "Invalid state: " state)))
    (swap (@ element-a style display) (@ element-b style display))
    (setf (@ link inner-h-t-m-l) link-text)))

(chain document
       (get-element-by-id "toggle-link")
       (add-event-listener
	"click"
	(lambda (event)
	  (chain event (prevent-default))
	  (toggle-content "toggle-jelly" "toggle-cat" "toggle-link"))))

;;;; All the remaining examples require the fetch API. Warn the user
;;;; if it's not supported.
(when (undefined fetch)
  (dolist (el (chain document (get-elements-by-class-name "fetch-warning")))
    (setf (@ el style display) "block")
    (setf (@ el text-content)
	  "Your browser does not support the fetch API. This example won't work.")))

;;;; Example 2: Fun with SVG

;;; Include the Parenscript Lisp Library, which includes the following
;;; 7 functions: member, map, mapcar, reduce, map-into,
;;; set-difference, and nconc.
(lisp *ps-lisp-library*)

;;; You can't actually call this function from Parenscript directly,
;;; since if "array" appears in the car of a sexp, Parenscript
;;; compiles it down to an array literal, like so: (array 1 2 3) -->
;;; [1,2,3]. This function is useful, however, to pass as an argument
;;; to higher-order functions, since #'make-array, #'array, and #'list
;;; won't work.
(defun array (&rest args)
  "Return the unmodified arguments array."
  args)

(defun curry (fn &rest curried-args)
  "Bind the first N `args' of `fn'."
  ;; We could just delegate to JavaScript's Function.prototype.bind().
  (lambda (&rest rest-args)
    (apply fn (append curried-args rest-args))))

(defun flip (fn)
  "Reverse the order of arguments for 2-ary function `fn'."
  (lambda (a b) (fn b a)))

(defun numeric-sort-by! (key table)
  "Numerically sort `table' on `key' in descending order."
  (chain table (sort (lambda (a b)
		       (- (+ (getprop b key)) (+ (getprop a key)))))))

(defun take (n array)
  "Take the first `n' items from `array'."
  (chain array (slice 0 n)))

(defun zip (&rest args)
  (apply #'mapcar #'array args))

(defun make-object-from (key-value-pairs)
  "Construct an Object from `key-value-pairs'.

Example:

(make-object-from ([] (a 1) (b 2) (c 3)))
-> {a: 1, b: 2, c: 3}"
  (let ((o (create)))
    (dolist (kv key-value-pairs o)
      (setf (getprop o (@ kv 0)) (@ kv 1)))))

(defun fetch-text (url)
  "Fetch `url' and return the response body as text."
  (chain (fetch url)
	 (then (lambda (response) (chain response (text))))
	 (catch (lambda (error) (throw error)))))

(defun parse-csv (csv-string &optional (numeric-columns (array)))
  "Parse `csv-string' into an array of objects.

The first line of the `csv-string' is expected to contain the
header. Each line of the csv file is then parsed into an object with
properties taken from the header line.

Values for columns in `numeric-colums' are stored as numbers, rather
than strings.

Example:
(parse-csv \"a,b,c\n1,2,3\n4,5,6\" '(\"a\" \"b\"))
-> [{a: 1, b: 2, c: \"3\"}, {a: 4, b: 5, c: \"6\"}]"
  (flet ((convert (alist)
	   ;; Note that alist is not a true a-list, since JavaScript
	   ;; doesn't have conses. It's just a two-element array.
	   (map (lambda (kv)
		  (destructuring-bind (key value) kv
		    (if (member key numeric-columns)
			(list key (+ value))
			kv)))
		alist)))
    (let* ((lines (chain csv-string (split #\newline)))
	   (columns (chain lines (shift) (split #\,))))
      (map (lambda (line)
	     (make-object-from (convert (zip columns (chain line (split #\,))))))
	   lines))))

(defvar +svg-ns+ "http://www.w3.org/2000/svg")

(defmacro create-svg-element (tag-name)
  `(chain document (create-element-n-s +svg-ns+ ,tag-name)))

(defmacro set-attr (element attr value)
  `(chain ,element (set-attribute ,attr ,value)))

(defmacro append-child. (element child)
  `(chain ,element (append-child ,child)))

(defun make-plot-title
    (title &key (fill "grey") (font-size "10pt") (text-anchor "middle") (x 0) (y 0))
  (let ((plot-title (create-svg-element "text")))
    (setf (@ plot-title id) "plot-title")
    (setf (@ plot-title text-content) title)
    (setf (@ plot-title style font-size) font-size)
    (set-attr plot-title "x" x)
    (set-attr plot-title "y" y)
    (set-attr plot-title "fill" fill)
    (set-attr plot-title "text-anchor" text-anchor)
    plot-title))

(defun make-y-label
    (label
     &key (fill "grey") (font-size "8pt") (text-anchor "middle") (x 0) (y 0))
  (let ((y-label (create-svg-element "text")))
    (setf (@ y-label text-content) label)
    (setf (@ y-label style font-size) font-size)
    (set-attr y-label "x" x)
    (set-attr y-label "y" y)
    (set-attr y-label "fill" fill)
    (set-attr y-label "text-anchor" text-anchor)
    (set-attr y-label "transform" "rotate(-90)")
    y-label))

(defun make-y-axis
    (&key (ax-offset 50) (ax-stroke "lightgrey") (ax-stroke-width "1px")
       (grid-stroke ax-stroke) (grid-stroke-width (/ ax-stroke-width 4))
       (label-fill "grey") (label-font-size "4pt")
       (n-ticks 4) (tick-length 4) (tick-scale 1.0)
       (width 500) (height 100))
  (let* ((svg-element (create-svg-element "svg"))
	 (ax-g (create-svg-element "g"))
	 (labels-g (create-svg-element "g"))
	 (grid-g (create-svg-element "g"))
	 (y-axis (create-svg-element "line"))
	 (half-tick-length (/ tick-length 2))
	 (ax-x (- ax-offset half-tick-length)))
    (set-attr svg-element "width" width)
    (set-attr svg-element "height" height)
    (set-attr ax-g "stroke" ax-stroke)
    (set-attr ax-g "stroke-width" ax-stroke-width)
    (set-attr grid-g "stroke" ax-stroke)
    (set-attr grid-g "stroke-width" "0.25px")
    (set-attr labels-g "fill" label-fill)
    (set-attr labels-g "text-anchor" "end")
    (setf (@ labels-g style font-size) label-font-size)

    ;; y-axis
    (set-attr y-axis "x1" ax-x)
    (set-attr y-axis "x2" ax-x)
    (set-attr y-axis "y1" 0)
    (set-attr y-axis "y2" height)
    (append-child. ax-g y-axis)

    ;; grid, ticks, and tick-labels
    (do* ((tick-interval (/ height (1+ n-ticks)))
	  (tt tick-interval (+ tt tick-interval)))
	 ((>= tt height))
      (let ((tick (create-svg-element "line"))
	    (grid-line (create-svg-element "line"))
	    (tick-label (create-svg-element "text")))
	(set-attr tick "x1" (- ax-x half-tick-length))
	(set-attr tick "x2" (+ ax-x half-tick-length))
	(set-attr tick "y1" tt)
	(set-attr tick "y2" tt)
	;; Grid lines start where the tick ends and continue to the
	;; right edge of the graph.
	(set-attr grid-line "x1" (+ ax-x half-tick-length))
        (set-attr grid-line "x2" width)
	(set-attr grid-line "y1" tt)
	(set-attr grid-line "y2" tt)
	(set-attr tick-label "x" (- ax-x (1+ half-tick-length)))
	(set-attr tick-label "y" (+ tt 2))
	;; Convert from svg coords to tick coords.
	(setf (@ tick-label text-content)
	      (chain (* (- height tt) tick-scale) (to-fixed) (to-string)))
	(append-child. ax-g tick)
	(append-child. grid-g grid-line)
	(append-child. labels-g tick-label)))
    (append-child. svg-element ax-g)
    (append-child. svg-element labels-g)
    (append-child. svg-element grid-g)
    svg-element))

(defun make-bars
    (label-key title-key value-key table
     &key (fill "steelblue") (label-font-size "6pt") (value-scale 1.0)
       (width 500) (height 100) (x 0) (y 0))
  (let ((svg-element (create-svg-element "svg"))
	(bar-width (/ width (length table))))
    (set-attr svg-element "x" x)
    (set-attr svg-element "y" y)
    (set-attr svg-element "width" width)
    (set-attr svg-element "height" height)
    (set-attr svg-element "fill" fill)
    (dotimes (i (length table))
      (let* ((value (getprop (elt table i) value-key))
	     (h (* value value-scale))
	     (x (* bar-width i))
	     (y (- height h))
	     (rect (create-svg-element "rect"))
	     (label (create-svg-element "text"))
	     (title (create-svg-element "title"))
	     (label-text (getprop (elt table i) label-key))
	     (title-text (stringify (getprop (elt table i) title-key) ": " value)))
	;; Some user agents will display the title as a tooltip when
	;; the user hovers over the element.
	(setf (@ title text-content) title-text)
	(append-child. rect title)

	(set-attr rect "x" x)
	(set-attr rect "y" y)
	(set-attr rect "width" bar-width)
	(set-attr rect "height" h)
	(append-child. svg-element rect)

	(setf (@ label text-content) label-text)
	(setf (@ label style font-size) label-font-size)
	(set-attr label "x" (+ x (/ bar-width 2)))
	(set-attr label "y" (- y 2))
	(set-attr label "text-anchor" "middle")
	(append-child. svg-element label)))
    svg-element))

(defun draw-svg-bar-chart (svg-id table)
  "Draw a bar chart from the data in `table' on the svg element given
by `svg-id'."
  (let* ((svg-element (chain document (get-element-by-id svg-id)))
	 (svg-height 100)
	 (svg-width 500)
	 (view-box (chain (list 0 0 svg-width svg-height) (join " ")))
	 ;; Reserve h-offset pixels at the top for a title and
	 ;; w-offset pixels on the left for the y-axis and label.
	 (h-offset 20)
	 (w-offset 50)
	 (max-height (- svg-height h-offset))
	 ;; Cannot use 0 as the init value since 0 is falsey in
	 ;; JavaScript! See the implementation of reduce for why. Note
	 ;; that reduce is defined in *ps-lisp-library*.
	 (max-value (reduce (lambda (acc row)
			      (max acc (getprop row "total exports")))
			    table -1)))
    (set-attr svg-element "viewBox" view-box)
    (flet ((svg-append-all (&rest elements)
	     (dolist (element elements)
	       (append-child. svg-element element))))
      (svg-append-all
       ;; Y-axis goes first so grid lines are layered at the bottom.
       (make-y-axis
	:w-offset w-offset
	:width svg-width
	:height svg-height
	:tick-scale (/ max-value max-height))
       (make-plot-title
	"Top 25 US States by Agriculture Exports (2011)"
	;; Title is centered on the x position.
	:x (+ w-offset (/ (- svg-width w-offset) 2))
	:y h-offset)
       (make-y-label
	"Millions USD"
	;; Label is rotated 90 deg, so x and y are "swapped"
	:x (- (/ svg-height 2))
	:y h-offset)
       (make-bars
	"code" "state" "total exports" table
	:width (- svg-width w-offset)
	:height svg-height
	:value-scale (/ max-height max-value)
	:x w-offset)))))

(chain (fetch-text "2011_us_ag_exports.csv")
       (then (curry (flip parse-csv) '("total exports")))
       (then (curry numeric-sort-by! "total exports"))
       (then (curry take 25))
       (then (curry draw-svg-bar-chart "svg-chart")))


;;;; Example 3: Fun with Plotly.js

;;; This macro is analogous to Parenscript's `[]' macro. That is,
;;; `{}.' is to `create' as `[]' is to `array'. Note that Parenscript
;;; already defines `{}' as a symbol-macro that expands to (create);
;;; hence, this macro has a trailing period to avoid the Parenscript
;;; compiler warning about redefining `{}'.
;;;
;;; Actually, because Parenscript's `{}' is a symbol-macro, we *could*
;;; name this macro `{}', and SBCL at least allows both to co-exist
;;; peacefully. If the `{}' appears in the function position, this
;;; macro expansion is used; if it appears anywhere else, the
;;; symbol-macro expansion is used. But the Parenscript compiler
;;; warnings are annoying, so better to just use a unique name here.
(defmacro {}. (&rest args)
  "Create nested object literals."
  `(create ,@(mapcar (lambda (arg)
		       (if (and (consp arg) (not (equal '{}. (car arg))))
			   (cons '{}. arg)
			   arg))
		     args)))

(defun extract-column (column table)
  "Return an array of the values for `column' in `table'."
  (map (lambda (row) (getprop row column)) table))

(defun draw-plotly-bar-chart (chart-id table)
  (let ((layout ({}. :title "Top 25 US States by Agriculture Exports (2011)"
		     :yaxis (:title "Millions USD")))
	(data (list (create :x (extract-column "code" table)
			    :y (extract-column "total exports" table)
			    :text (extract-column "state" table)
			    :type "bar"))))
    (chain -plotly (plot chart-id data layout))))

(chain (fetch-text "2011_us_ag_exports.csv")
       (then (curry (flip parse-csv) '("total exports")))
       (then (curry numeric-sort-by! "total exports"))
       (then (curry take 25))
       (then (curry draw-plotly-bar-chart "plotly-bar")))

(defun draw-plotly-choropleth (chart-id table)
  (let ((layout ({}. :title "US Agriculture Exports by State (2011)"
		     :geo (:scope "usa"
			   :showlakes t
			   :lakecolor "rgb(255,255,255)")))
	(data (list (create :type "choropleth"
			    :locationmode "USA-states"
			    :locations (extract-column "code" table)
			    :z (extract-column "total exports" table)
			    :text (extract-column "state" table)
			    :zmin 0
			    :zmax 17000
			    :colorbar ({}. :title "Millions USD"
					   :thickness 0.2)
			    :colorscale ([] (0 "rgb(242,240,247)")
					    (0.2 "rgb(218,218,235)")
					    (0.4 "rgb(188,189,220)")
					    (0.6 "rgb(158,154,200)")
					    (0.8 "rgb(117,107,177)")
					    (1 "rgb(84,39,143)"))
			    :marker ({}. :line (:color "rgb(255,255,255)"
						:width 2))))))
    (chain -plotly (plot chart-id data layout ({}. :show-link false)))))

(chain (fetch-text "2011_us_ag_exports.csv")
       (then (curry (flip parse-csv) '("total exports")))
       (then (curry draw-plotly-choropleth "plotly-choropleth")))
