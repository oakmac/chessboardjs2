(ns com.oakmac.chessboard2.core
  (:require
    [clojure.string :as str]
    [com.oakmac.chessboard2.api :as api]
    [com.oakmac.chessboard2.config :as config]
    [com.oakmac.chessboard2.css :as css]
    [com.oakmac.chessboard2.dom-ops :as dom-ops]
    [com.oakmac.chessboard2.feature-flags :as flags]
    [com.oakmac.chessboard2.html :as html]
    [com.oakmac.chessboard2.js-api :as js-api]
    [com.oakmac.chessboard2.util.data-transforms :refer [map->js-return-format]]
    [com.oakmac.chessboard2.util.dom :as dom-util :refer [add-class! append-html! remove-class! remove-element!]]
    [com.oakmac.chessboard2.util.ids :refer [random-id]]
    [com.oakmac.chessboard2.util.logging :refer [error-log info-log warn-log]]
    [com.oakmac.chessboard2.util.moves :refer [move->map]]
    [com.oakmac.chessboard2.util.pieces :refer [random-piece-id]]
    [com.oakmac.chessboard2.util.predicates :refer [arrow-item? circle-item? valid-color? valid-move-string? valid-square? valid-piece?]]
    [com.oakmac.chessboard2.util.squares :as square-util]
    [com.oakmac.chessboard2.util.string :refer [safe-lower-case]]
    [goog.array :as garray]
    [goog.dom :as gdom]
    [goog.functions :as gfunctions]
    [goog.object :as gobj]))

(declare percent? size-string->number tshirt-sizes)

;; TODO
;; - need to write a Cypress test for calling .move() while an animation is happening

;; NOTE: the transitionend event fires for every CSS property that is transitioned
;; This function fires twice for most (but not all) piece moves (css props 'left' and 'top')
(defn on-transition-end
  "This function fires on every 'transitionend' event inside the root DOM element."
  [board-state js-evt]
  (let [target-el (gobj/get js-evt "target")
        el-id (gobj/get target-el "id")]
    ;; is there an animation-end callback associated with this element?
    (when-let [callback-fn (get-in @board-state [:animation-end-callbacks el-id])]
      ;; execute the callback
      (callback-fn)
      ;; remove callback from the cache
      (swap! board-state update-in [:animation-end-callbacks] dissoc el-id))))

;; TODO:
;; could we simplify this by using elementsFromPoint?
;; which approach is faster?
;; https://developer.mozilla.org/en-US/docs/Web/API/Document/elementsFromPoint
(defn xy->square
  "Returns the square from the provided X, Y Viewport coordinates.
  Returns nil if no square was found."
  [x y square->square-ids]
  (let [square-els-selector (->> square->square-ids
                              vals
                              (map #(str "#" %))
                              (str/join ", "))
        square-els (dom-util/query-select-all square-els-selector)]
    (reduce
      (fn [_acc square-el]
        (when (dom-util/xy-inside-element? square-el x y)
          (-> square-el
              (gobj/get "dataset")
              (gobj/get "squareCoord")
              reduced)))
      nil
      square-els)))

(defn begin-dragging!
  "initialize dragging a piece"
  [board-state square piece x y]
  (let [{:keys [dragging-piece-id onDragStart orientation piece-square-pct position square->piece-id]} @board-state
        ;; call their onDragStart function if provided
        on-drag-start-result (when (fn? onDragStart)
                               (let [js-board-position (clj->js position)]
                                 ;; could be an arbitrary data-chessboard2-draggable element
                                 ;; TODO: pass in a timestamp of their event
                                 (try
                                   (onDragStart (js-obj "orientation" orientation
                                                        "piece" piece
                                                        "position" js-board-position
                                                        ;; FIXME: need "source" here
                                                        ;; "source" "FIXME"
                                                        "square" square))
                                   (catch js/Error err
                                     (error-log "Runtime error with provided onDragStart function:" err)
                                     nil))))]
    ;; do nothing if they return false from onDragStart
    (when-not (false? on-drag-start-result)
      (let [piece-id (get square->piece-id square)
            _ (when flags/runtime-checks?
                (when-not piece-id
                  (error-log "Unable to find piece-id in begin-dragging")))
            piece-el (dom-util/get-element piece-id)
            ;; NOTE: these two calls could be combined for a quick perf improvement
            piece-height (dom-util/get-height piece-el)
            piece-width (dom-util/get-width piece-el)]
        ;; create dragging piece if necessary
        (when-not (dom-util/get-element dragging-piece-id)
          (dom-util/append-html!
            js/window.document.body
            (html/DraggingPiece {:height piece-height
                                 :id dragging-piece-id
                                 :piece piece
                                 :piece-square-pct piece-square-pct
                                 :width piece-width
                                 :x x
                                 :y y})))
        ;; flag that we are actively dragging
        (swap! board-state assoc :dragging? true
                                 :dragging-el (dom-util/get-element dragging-piece-id)
                                 :dragging-starting-square square
                                 :dragging-starting-piece piece)))

        ;; remove the piece from the source square
        (let [updated-position (dissoc position square)]
          (api/set-position! board-state updated-position {:animate false}))))
        ;; TODO: they could return an object from their onDragStart function to control what happens
        ;;       to the source piece

(defn on-touch-start
  "This function fires on every 'touchstart' event inside the root DOM element"
  [board-state js-evt]
  ;; prevent "double-tap to zoom"
  (dom-util/safe-prevent-default js-evt)
  (let [{:keys [draggable onTouchSquare orientation position
                square->square-ids touchDraggable touchMove]}
        @board-state
        js-first-touch (aget (gobj/get js-evt "touches") 0)
        clientX (gobj/get js-first-touch "clientX")
        clientY (gobj/get js-first-touch "clientY")

        square (xy->square clientX clientY square->square-ids)

        _ (when flags/runtime-checks?
            (when-not (valid-square? square)
              (error-log "Invalid square in on-touch-start:" square)))

        ;; NOTE: piece may be nil if there is no piece on the square they touched
        piece (get position square)

        ;; call their onTouchSquare function if provided
        on-touchsquare-result (when (fn? onTouchSquare)
                                (let [js-board-info (js-obj "orientation" orientation
                                                            "position" (clj->js position))]
                                                            ;; FIXME: add square, piece here
                                                            ;; what else?
                                  (onTouchSquare square piece js-board-info)))]

    ;; begin dragging if configured
    (when (and piece
               (or (true? draggable) (true? touchDraggable)))
      (begin-dragging! board-state square piece clientX clientY))

    ;; highlight the square and queue a move if touchmove is enabled
    (when (and (true? touchMove)
               (not (false? on-touchsquare-result))
               piece))
      ;; FIXME:
      ;; - highlight the square here?
      ;; - or should this be their responsibility?
      ;; queue their touchmove
      ; (swap! board-state assoc :touch-move-queue1 {:piece piece, :square square}))
    ;; return null
    nil))

(defn on-mousedown-root-el
  "This function fires on every 'mousedown' event inside the root DOM element"
  [board-state js-evt]
  (dom-util/safe-prevent-default js-evt)
  (let [{:keys [draggable mouseDraggable onMousedownSquare orientation position square->square-ids touchMove]} @board-state
        clientX (gobj/get js-evt "clientX")
        clientY (gobj/get js-evt "clientY")

        square (xy->square clientX clientY square->square-ids)

        _ (when flags/runtime-checks?
            (when-not (valid-square? square)
              (error-log "Invalid square in on-mouse-down:" square)))

        ;; NOTE: piece may be nil if there is no piece on the square they touched
        piece (get position square)

        ;; call their onMousedownSquare function if provided
        on-mousedown-result (when (fn? onMousedownSquare)
                              (let [js-board-info (js-obj "orientation" orientation
                                                          "piece" piece
                                                          "position" (clj->js position)
                                                          "square" square)]
                                (onMousedownSquare js-board-info js-evt)))]

    ;; begin dragging if configured
    (when (and piece
               (or (true? draggable) (true? mouseDraggable)))
      (begin-dragging! board-state square piece clientX clientY))

    ;; highlight the square and queue a move if touchmove is enabled
    (when (and (true? touchMove)
               (not (false? on-mousedown-result))
               piece))
      ;; FIXME:
      ;; - highlight the square here?
      ;; - or should this be their responsibility?
      ;; queue their touchmove
      ; (swap! board-state assoc :touch-move-queue1 {:piece piece, :square square}))
    ;; return null
    nil))

(defn on-mouseup-root-el
  "This function fires on every 'mouseup' event inside the root DOM element"
  [board-state js-evt]
  (let [{:keys [onMouseupSquare orientation position square->square-ids]} @board-state
        clientX (gobj/get js-evt "clientX")
        clientY (gobj/get js-evt "clientY")
        square (xy->square clientX clientY square->square-ids)

        _ (when flags/runtime-checks?
            (when-not (valid-square? square)
              (error-log "Invalid square in on-mouse-up:" square)))

        ;; NOTE: piece may be nil if there is no piece on the square
        piece (get position square)]

    ;; call their onMouseupSquare function if provided
    (when (fn? onMouseupSquare)
      (let [js-board-info (js-obj "orientation" orientation
                                  "piece" piece
                                  "position" (clj->js position)
                                  "square" square)]
        (onMouseupSquare js-board-info js-evt)))))

(defn update-dragging-piece-position!
  "Update the x, y coordinates of the dragging piece on the next animationFrame"
  [dragging-el x y]
  (.requestAnimationFrame js/window
    (fn []
      (dom-util/set-style-prop! dragging-el "left" (str x "px"))
      (dom-util/set-style-prop! dragging-el "top" (str y "px")))))

(defn on-mousemove-window
  [board-state js-evt]
  (let [{:keys [dragging? dragging-el]} @board-state]
    ;; do nothing if we are not actively dragging
    (when dragging?
      (let [x (gobj/get js-evt "clientX")
            y (gobj/get js-evt "clientY")]
        (update-dragging-piece-position! dragging-el x y)))))

;; NOTE: this function has the potential to be a perf bottleneck
;;       need to benchmark and optimize this function
(defn on-mousemove-root-el
  [board-state js-evt]
  (let [clientX (gobj/get js-evt "clientX")
        clientY (gobj/get js-evt "clientY")
        {:keys [orientation onMouseenterSquare onMouseleaveSquare position
                square->square-ids square-mouse-is-currently-hovering-over]}
        @board-state
        prev-square square-mouse-is-currently-hovering-over
        new-square (xy->square clientX clientY square->square-ids)]
    (when-not (= new-square square-mouse-is-currently-hovering-over)
      ;; update mouse position
      (swap! board-state assoc :square-mouse-is-currently-hovering-over new-square)

      ;; call their onMouseleaveSquare function if provided
      (when (and prev-square (fn? onMouseleaveSquare))
        (let [piece (get position prev-square)
              js-board-info (js-obj "orientation" orientation
                                    "piece" piece
                                    "position" (clj->js position)
                                    "square" prev-square
                                    "toSquare" (if new-square new-square "off-board"))]
          (onMouseleaveSquare js-board-info js-evt)))

      ;; call their onMouseenterSquare function if provided
      (when (and new-square (fn? onMouseenterSquare))
        (let [piece (get position new-square)
              js-board-info (js-obj "orientation" orientation
                                    "piece" piece
                                    "position" (clj->js position)
                                    "square" new-square
                                    "fromSquare" (if prev-square prev-square "off-board"))]
          (onMouseenterSquare js-board-info js-evt))))))

(defn on-mouseleave-root-el
  "Clear the current mouse position when the cursor leaves the board."
  [board-state _js-evt]
  (swap! board-state assoc :square-mouse-is-currently-hovering-over nil))

(defn on-touchmove
  [board-state js-evt]
  (let [{:keys [dragging? dragging-el]} @board-state
        js-first-touch (some-> (gobj/get js-evt "touches")
                               (aget 0))]
    ;; do nothing if we are not actively dragging
    (when (and dragging? js-first-touch)
      (let [x (gobj/get js-first-touch "clientX")
            y (gobj/get js-first-touch "clientY")]
        (update-dragging-piece-position! dragging-el x y)))))

(defn drop-piece!
  [board-state x y]
  (let [{:keys [dragging-el dragging-starting-piece dragging-starting-square dropOffBoard
                onDrop orientation position square->square-ids]}
        @board-state
        dropped-square (xy->square x y square->square-ids)
        target (if dropped-square dropped-square "offboard")
        ;; call their onDrop function if provided
        on-drop-result (when (fn? onDrop)
                         ;; NOTE: they can calculate what element the piece was dropped
                         ;; onto by using elementFromPoint
                         ;; https://developer.mozilla.org/en-US/docs/Web/API/Document/elementFromPoint
                         (try
                           (onDrop (js-obj "orientation" orientation
                                           "piece" dragging-starting-piece
                                           "source" dragging-starting-square
                                           "target" target
                                           "x" x
                                           "y" y))
                           (catch js/Error err
                             (error-log "Runtime error with provided onDrop function:" err)
                             nil)))]
    ;; TODO: refactor to reduce code here
    (cond
      (= on-drop-result "snapback")
      (do ;; destroy the dragging piece
          (dom-util/remove-element! dragging-el)
          ;; TODO: perform snapback animation here
          ;; update board state
          (swap! board-state dissoc :dragging?
                                    :dragging-el
                                    :dragging-starting-piece
                                    :dragging-starting-square))

      (= on-drop-result "remove")
      (let [updated-position (dissoc position dragging-starting-square)]
        (dom-util/remove-element! dragging-el)
        ;; perform an instant position adjustment
        (api/set-position! board-state updated-position {:animate false})
        ;; update board state
        (swap! board-state dissoc :dragging?
                                  :dragging-el
                                  :dragging-starting-piece
                                  :dragging-starting-square))

      ;; TODO: add "donothing" here

      ;; piece was dropped onto a square
      dropped-square
      (do ;; destroy the dragging piece
          (dom-util/remove-element! dragging-el)
          ;; actually place the piece
          (let [updated-position (assoc position dropped-square dragging-starting-piece)]
            (api/set-position! board-state updated-position {:animate false}))
          ;; update board state
          (swap! board-state dissoc :dragging?
                                    :dragging-el
                                    :dragging-starting-piece
                                    :dragging-starting-square))

      ;; piece was dropped outside of the board: snapback
      (and (= target "offboard") (= dropOffBoard "snapback"))
      (do ;; destroy the dragging piece
          (dom-util/remove-element! dragging-el)
          ;; TODO: perform snapback animation here
          ;; update board state
          (swap! board-state dissoc :dragging?
                                    :dragging-el
                                    :dragging-starting-piece
                                    :dragging-starting-square))

      ;; piece was dropped outside of the board: remove
      (and (= target "offboard") (= dropOffBoard "remove"))
      (let [updated-position (dissoc position dragging-starting-square)]
        (dom-util/remove-element! dragging-el)
        ;; perform an instant position adjustment
        (api/set-position! board-state updated-position {:animate false})
        ;; update board state
        (swap! board-state dissoc :dragging?
                                  :dragging-el
                                  :dragging-starting-piece
                                  :dragging-starting-square))

      :else
      (when flags/runtime-checks? (error-log "mouseup case not handled")))))

(defn on-touchend
  [board-state js-evt]
  (let [{:keys [dragging?]} @board-state
        js-touch (some-> (gobj/get js-evt "changedTouches")
                         (aget 0))]
    ;; do nothing if we are not actively dragging
    (when (and dragging? js-touch)
      (let [x (gobj/get js-touch "clientX")
            y (gobj/get js-touch "clientY")]
        (drop-piece! board-state x y)))))

(defn on-mouseup-window
  [board-state js-evt]
  (let [{:keys [dragging?]} @board-state]
    ;; do nothing if we are not actively dragging
    (when dragging?
      (let [x (gobj/get js-evt "clientX")
            y (gobj/get js-evt "clientY")]
        (drop-piece! board-state x y)))))

(defn- add-events!
  "Attach DOM events."
  [root-el board-state]
  ;; global window events
  (.addEventListener js/window "mousemove" (fn [js-evt] (on-mousemove-window board-state js-evt)))
  (.addEventListener js/window "mouseup"   (fn [js-evt] (on-mouseup-window board-state js-evt)))
  (.addEventListener js/window "touchend"  (fn [js-evt] (on-touchend board-state js-evt)))
  (.addEventListener js/window "touchmove" (fn [js-evt] (on-touchmove board-state js-evt)))
  (.addEventListener js/window "resize"    (gfunctions/debounce
                                             (fn [] (api/resize! board-state))
                                             10)) ;; TODO: make this debounce value configurable

  ;; events on the root element
  (.addEventListener root-el "mouseleave" (fn [js-evt] (on-mouseleave-root-el board-state js-evt)))
  (.addEventListener root-el "mousemove"  (fn [js-evt] (on-mousemove-root-el board-state js-evt)))
  (.addEventListener root-el "mousedown"  (fn [js-evt] (on-mousedown-root-el board-state js-evt)))
  (.addEventListener root-el "mouseup"    (fn [js-evt] (on-mouseup-root-el board-state js-evt)))
  (.addEventListener root-el "touchstart" (fn [js-evt] (on-touch-start board-state js-evt)))
  (.addEventListener root-el "transitionend" (fn [js-evt] (on-transition-end board-state js-evt))))

;; TODO: move this to util ns
(defn toggle-orientation [o]
  (if (= o "white") "black" "white"))

(defn get-all-item-elements-from-dom
  "returns an Array of all Item elements currently in the DOM"
  [items-container-id]
  (gobj/get (gdom/getElement items-container-id) "children"))

(defn get-item-el-ids
  "returns a Set of all the Item ids currently in the DOM"
  [items-container-id]
  (let [item-els (get-all-item-elements-from-dom items-container-id)
        ;; TODO: make this transient for perf reasons?
        ids (atom #{})]
    (garray/forEach item-els
      (fn [itm]
        (swap! ids conj (gobj/get itm "id"))))
    @ids))

;; TODO:
;; - [ ] move this to dom-ops
;; - [ ] need to decide if Pieces are considered Items or not
;; - [ ] refactor this function to not use an atom
(defn- draw-items-instant!
  "Update all Items in the DOM instantly (ie: no animation)"
  [board-state]
  (let [{:keys [board-width items items-container-id orientation piece-square-pct position square->piece-id]} @board-state
        html (atom "")]
    ;; remove existing Items from the DOM
    (doseq [item-id (keys items)]
      (remove-element! item-id))
    ;; remove existing Pieces from the DOM
    (doseq [el-id (vals square->piece-id)]
      (remove-element! el-id))
    ;; clear the :square->piece-id map
    (swap! board-state assoc :square->piece-id {})
    (doseq [[square piece] position]
      ;; TODO: can do this without an atom
      (let [piece-id (random-piece-id)]
        (swap! html str (html/Piece {:board-orientation orientation
                                     :board-width board-width
                                     :id piece-id
                                     :hidden? false
                                     :piece piece
                                     :piece-square-pct piece-square-pct
                                     :square square
                                     :width (/ board-width 8)}))
        (swap! board-state assoc-in [:square->piece-id square] piece-id)))
    ;; add Items back
    (doseq [item (vals items)]
      (cond
        (arrow-item? item)
        (swap! html str (html/Arrow (merge
                                      item
                                      {:board-width board-width
                                       :orientation orientation})))

        (circle-item? item)
        (swap! html str (html/Circle (merge
                                       item
                                       {:board-width board-width
                                        :orientation orientation})))

        ;; FIXME: we will need to support Custom Items here

        :else (js/console.warn "draw-items-instant! Unrecognized Item type:" (pr-str item))))
    (append-html! items-container-id @html)))

;; -----------------------------------------------------------------------------
;; API Methods

(defn get-circles-by-square
  "Returns a Map of Circles with their square as the key."
  [board-state]
  (let [circles (->> @board-state
                     :items
                     vals
                     (filter circle-item?))]
    (zipmap (map :square circles) circles)))

(defn js-get-circles
  "Returns the Circle Items on the board as either a JS Array (default), JS Object, or JS Map"
  [board-state return-fmt]
  (map->js-return-format (api/get-items-by-type board-state "CHESSBOARD_CIRCLE")
                         (safe-lower-case return-fmt)))

(defn remove-circle-by-id
  "Removes a Circle from the board using it's item-id"
  [board-state item-id]
  (dom-ops/apply-ops! board-state [{:remove-el item-id}])
  (swap! board-state update-in [:items] dissoc item-id))

(defn remove-circle
  "Remove a Circle from the board"
  [board-state item-id-or-square]
  (if (valid-square? item-id-or-square)
    (let [square->circles-map (get-circles-by-square board-state)]
      (when-let [circle (get square->circles-map item-id-or-square)]
        (remove-circle-by-id board-state (:id circle))))
    (remove-circle-by-id board-state item-id-or-square)))

;; FIXME: this should be variadic as well as accept an array of circle ids
(defn js-remove-circle
  [board-state item-id-or-square]
  ;; TODO:
  ;; - input validation here
  ;; - log.warn if item-id is not valid
  (remove-circle board-state item-id-or-square))

;; TODO: this function could be combined with clear-arrows
(defn clear-circles
  "Removes all Circles from the board"
  [board-state]
  (let [item-ids (->> @board-state
                      :items
                      vals
                      (filter circle-item?)
                      (map :id))
        dom-ops (map
                  (fn [id]
                    {:remove-el id})
                  item-ids)]
    (dom-ops/apply-ops! board-state dom-ops)
    (swap! board-state update-in [:items]
           (fn [items]
             (apply dissoc items item-ids)))
    nil))

(defn add-circle
  "Adds a Circle to the board. Returns the id of the new Circle."
  [board-state {:keys [color id opacity size square] :as _circle-config}]
  (when flags/runtime-checks?
    (assert (valid-square? square) (str "Invalid square passed to add-circle:" (pr-str square))))
  (let [{:keys [board-width orientation]} @board-state
        id (or id (random-id "item"))
        size (size-string->number size)
        circle-item {:color color
                     :id id
                     :opacity opacity
                     :size size
                     :square square
                     :type "CHESSBOARD_CIRCLE"}
        circle-html (html/Circle {:board-width board-width
                                  :color color
                                  :id id
                                  :opacity opacity
                                  :orientation orientation
                                  :size size
                                  :square square})]
    (dom-ops/apply-ops! board-state [{:new-html circle-html}])
    (swap! board-state assoc-in [:items id] circle-item)
    id))

(defn add-or-replace-circle
  "Adds a Circle to the board. Returns the id of the new Circle.
  If there is already a Circle on the board on that square, it will be replaced."
  [board-state {:keys [square] :as circle-config}]
  (let [square->circles-map (get-circles-by-square board-state)]
    (if-let [current-circle (get square->circles-map square)]
      (do
        (remove-circle-by-id board-state (:id current-circle))
        (add-circle board-state (assoc circle-config :id (:id current-circle))))
      (add-circle board-state circle-config))))

(def default-circle-config
  {:color "#777"
   :opacity 0.8
   :size "small"})

(defn- looks-like-a-js-circle-config? [js-cfg]
  (and (object? js-cfg)
       (valid-square? (gobj/get js-cfg "square"))))

;; TODO: can be combined with valid-arrow-size?
(defn valid-circle-size? [s]
  (or (contains? tshirt-sizes s)
      (percent? s)))

;; TODO: move this to js-api ns and add unit tests for the argument parsing
(defn js-add-circle
  [board-state arg1 arg2 arg3]
  (let [cfg (cond-> default-circle-config
              (valid-square? arg1) (merge {:square arg1})
              (and (valid-color? arg2)
                   (not (valid-circle-size? arg2)))
              (merge {:color arg2})
              (valid-circle-size? arg2) (merge {:size arg2})
              (valid-circle-size? arg3) (merge {:size arg3})
              (looks-like-a-js-circle-config? arg1) (merge (js->clj arg1 :keywordize-keys true)))]
    (if (valid-square? (:square cfg))
      (add-or-replace-circle board-state cfg)
      (warn-log "Invalid square passed to .addCircle() method:" (:square cfg)))))

(def default-arrow-config
  {:color "#777"
   :opacity 0.8
   :size "large"})

;; TODO: move to predicates ns
(defn- looks-like-a-js-arrow-config? [js-cfg]
  (and (object? js-cfg)
       (valid-square? (gobj/get js-cfg "start"))
       (valid-square? (gobj/get js-cfg "end"))))

;; TODO: move to util ns
(def tshirt-sizes
  #{"small" "medium" "large"})

;; TODO: move to predicates ns
(defn percent? [n]
  (and (number? n)
       (>= n 0)))

;; TODO: move to predicates ns
(defn valid-arrow-size? [s]
  (or (contains? tshirt-sizes s)
      (percent? s)))

;; TODO: move to util ns
(defn size-string->number
  "Converts a size string to a number. Does nothing if s is already a number."
  [s]
  (let [lc-s (safe-lower-case s)]
    (cond
      (= lc-s "small") 0.4
      (= lc-s "medium") 0.7
      (= lc-s "large") 0.9
      (number? s) s
      ;; NOTE: this should never happen
      :else 0.9)))

(defn add-arrow
  "Adds an analysis arrow to the board. Returns the id of the new arrow."
  [board-state {:keys [color end opacity size start] :as _arrow-config}]
  (let [{:keys [board-width orientation]} @board-state
        id (random-id "item")
        size (size-string->number size)
        arrow-item {:id id
                    :color color
                    :end end
                    :opacity opacity
                    :size size
                    :start start
                    :type "CHESSBOARD_ARROW"}
        arrow-html (html/Arrow {:board-width board-width
                                :color color
                                :end end
                                :id id
                                :opacity opacity
                                :orientation orientation
                                :size size
                                :start start})]
    (dom-ops/apply-ops! board-state [{:new-html arrow-html}])
    (swap! board-state assoc-in [:items id] arrow-item)
    id))

(defn remove-arrow
  "Remove an Analysis Arrow from the board"
  [board-state item-id]
  (dom-ops/apply-ops! board-state [{:remove-el item-id}])
  (swap! board-state update-in [:items] dissoc item-id))

(defn get-arrows-on-squares
  "Returns a map of the Arrows on the given squares"
  [board-state {:keys [start end]}]
  (reduce
    (fn [arrows [item-id itm]]
      (if (and (= (:type itm) "CHESSBOARD_ARROW")
               (= (:start itm) start)
               (= (:end itm) end))
        (assoc arrows item-id itm)
        arrows))
    {}
    (:items @board-state)))

(defn js-remove-arrow
  [board-state item-id-or-squares]
  (cond
    (valid-move-string? item-id-or-squares)
    (let [arrows (get-arrows-on-squares board-state (move->map item-id-or-squares "ARROW_FORMAT"))]
      (doseq [item-id (keys arrows)]
        (remove-arrow board-state item-id)))

    :else
    (if-let [_arrow (get-in @board-state [:items item-id-or-squares])]
      (remove-arrow board-state item-id-or-squares)
      ;; TODO: error code here?
      (warn-log "Invalid argument passed to removeArrow():" item-id-or-squares))))

; (defn move-arrow
;   "Move an Analysis Arrow on the board"
;   [board-state item-id new-move])
;   ;; TODO: apply-dom-ops

; (defn js-move-arrow
;   [board-state item-id new-move]
;   ;; TODO: validation here, warn if item-id is not valid
;   (valid-move-string? new-move)
;   ;; TODO: (move->map new-move) ??
;   (move-arrow board-state item-id new-move))

(defn js-add-arrow
  [board-state arg1 arg2 arg3]
  (let [cfg (cond-> default-arrow-config
              (valid-move-string? arg1) (merge (move->map arg1 "ARROW_FORMAT"))
              (and (valid-color? arg2)
                   (not (valid-arrow-size? arg2)))
              (merge {:color arg2})
              (valid-arrow-size? arg2) (merge {:size arg2})
              (valid-arrow-size? arg3) (merge {:size arg3})
              (looks-like-a-js-arrow-config? arg1) (merge (js->clj arg1 :keywordize-keys true)))]
    (add-arrow board-state cfg)))

(defn get-arrows
  "Returns a map of the Arrow Items on the board"
  [board-state]
  (let [arrows (->> @board-state
                    :items
                    vals
                    (filter arrow-item?))]
    (zipmap (map :id arrows) arrows)))

(defn js-get-arrows
  "Returns the Arrow Items on the board as either a JS Array (default), JS Object, or JS Map"
  [board-state return-fmt]
  (map->js-return-format (api/get-items-by-type board-state "CHESSBOARD_ARROW")
                         (safe-lower-case return-fmt)))

(defn get-items
  "Returns a map of the Items on the board."
  [board-state]
  (:items @board-state))

(defn js-get-items
  "Returns the Items on the board as either a JS Array (default), JS Object, or JS Map"
  [board-state return-fmt]
  (map->js-return-format (get-items board-state) (safe-lower-case return-fmt)))

(defn set-white-orientation!
  [board]
  (let [squares-selector (str "#" (:container-id @board) " ." css/squares)
        squares-el (dom-util/get-element squares-selector)]
    (remove-class! squares-el css/orientation-black)
    (add-class! squares-el css/orientation-white)
    (draw-items-instant! board)))

(defn set-black-orientation!
  [board]
  (let [squares-selector (str "#" (:container-id @board) " ." css/squares)
        squares-el (dom-util/get-element squares-selector)]
    (remove-class! squares-el css/orientation-white)
    (add-class! squares-el css/orientation-black)
    (draw-items-instant! board)))

(defn orientation
 ([board]
  (orientation board nil))
 ([board arg]
  (let [lc-arg (safe-lower-case arg)]
    (cond
      (= lc-arg "white") (do (swap! board assoc :orientation "white")
                             "white")
      (= lc-arg "black") (do (swap! board assoc :orientation "black")
                             "black")
      (= lc-arg "flip") (do (swap! board update :orientation toggle-orientation)
                            (let [new-orientation (:orientation @board)]
                              (if (= new-orientation "white")
                                (swap! board assoc :orientation "white")
                                (swap! board assoc :orientation "black"))
                              new-orientation))
      :else (:orientation @board)))))

(defn array-of-moves? [arg]
  (and (array? arg)
       (every? arg valid-move-string?)))

(defn looks-like-a-move-object? [js-move]
  (and (object? js-move)
       (valid-square? (gobj/get js-move "from"))
       (valid-square? (gobj/get js-move "to"))))

;; TODO: this function should accept an Array of squares
;; TODO: this function should accept an Object with "onFinishAnimation" callback
; (defn js-remove-piece
;   [board-state]
;   (let [js-args (array)]
;     (copy-arguments js-args)
;     (.shift js-args)
;     (let [current-pos (:position @board-state)
;           squares-to-remove (set (js->clj js-args))
;           ;; any argument of 'false' to this function means no animation
;           animate? (not-any? false? squares-to-remove)
;           new-position (apply dissoc current-pos squares-to-remove)]
;       (position board-state new-position animate?))))

(defn js-get-pieces
  "Returns the Pieces on the board as either a JS Array (default), JS Object, or JS Map"
  [board-state return-fmt]
  ;; FIXME: this does not work correctly because pieces are not Items
  (map->js-return-format (api/get-items-by-type board-state "CHESSBOARD_PIECE") (safe-lower-case return-fmt)))

(defn valid-add-piece-config? [cfg]
  (and (valid-piece? (:piece cfg))
       (valid-square? (:square cfg))))

; (defn add-piece
;   [board-state {:keys [animate? piece square] :as add-piece-cfg}]
;   (if-not (valid-add-piece-config? add-piece-cfg)
;     (js/console.warn "FIXME ERROR CODE: Invalid arguments passed to the .addPiece() method")
;     (let [current-pos (:position @board-state)
;           new-pos (assoc current-pos square piece)]
;       (position board-state new-pos animate?))))

; (defn- looks-like-a-js-add-piece-config? [js-cfg]
;   (and (object? js-cfg)
;        (valid-piece? (gobj/get js-cfg "piece"))
;        (valid-square? (gobj/get js-cfg "square"))))

(def default-add-piece-config
  {:animate? true})
   ;; TODO: onAnimationComplete callback here

; (defn js-add-piece
;   [board-state arg1 arg2 arg3]
;   (let [cfg (cond-> default-add-piece-config
;               (valid-square? arg1) (merge {:square arg1})
;               (valid-piece? arg2) (merge {:piece arg2})
;               (false? arg3) (merge {:animate? arg3})
;               (looks-like-a-js-add-piece-config? arg1) (merge (js->clj arg1 :keywordize-keys true)))]
;     (add-piece board-state cfg)))

(defn hide-coordinates!
  [board-state]
  (swap! board-state assoc :show-coords? false))
  ; (let [container-id (:container-id @board-state)]
  ;   (add-class! (dom-util/get-element container-id) "hide-notation-cbe71")
  ;   (swap! board-state assoc :show-notation? false)))

(defn show-coordinates!
  [board-state]
  (swap! board-state assoc :show-coords? true))
  ; (let [container-id (:container-id @board-state)]
  ;   (remove-class! (dom-util/get-element container-id) "hide-notation-cbe71")
  ;   (swap! board-state assoc :show-notation? true)))

(defn toggle-coordinates!
  [board-state]
  (swap! board-state update :show-coords? not))
  ; (if (:show-notation? @board-state)
  ;   (hide-coordinates! board-state)
  ;   (show-coordinates! board-state)))

;; -----------------------------------------------------------------------------
;; Constructor

(defn init-dom!
  [board-state]
  (let [{:keys [root-el] :as board-cfg} @board-state]
    ;; write the container <div>s to the DOM
    (dom-util/set-inner-html! root-el (html/BoardContainer board-cfg))
    ;; calculate width / height
    (api/resize! board-state)))

;; recommend they style the coordinate text using CSS
;; the chessboard API can support the "on/off" and position stuff
(def default-coords
  {:bottom {:position "outside", :show? false, :type "letters"}
   :left   {:position "outside", :show? false, :type "numbers"}
   :right  {:position "outside", :show? false, :type "numbers"}
   :top    {:position "outside", :show? false, :type "letters"}})

(def default-animate-speed-ms 80)
; (def default-animate-speed-ms 2500)

(defn board-state-change
  [_key board-atom old-state new-state]
  (when new-state
    ;; board orientation
    (when-not (= (:orientation old-state) (:orientation new-state))
      (if (= "white" (:orientation new-state))
        (set-white-orientation! board-atom)
        (set-black-orientation! board-atom)))
    ;; FIXME: coordinate config change
    ;; show / hide coordinates
    (when-not (= (:show-coords? old-state) (:show-coords? new-state))
      (if (:show-coords? new-state)
         (remove-class! (dom-util/get-element (:container-id new-state)) "hide-notation-cbe71")
         (add-class! (dom-util/get-element (:container-id new-state)) "hide-notation-cbe71")))
    ;; fire their onChange event
    (let [on-change-fn (:onChange new-state)
          old-position (:position old-state)
          new-position (:position new-state)]
      (when (and (fn? on-change-fn)
                 (not= old-position new-position))
        (on-change-fn (clj->js old-position) (clj->js new-position))))))

(defn constructor2
  [root-el js-opts]
  (let [;; create some random IDs for internal elements
        container-id (random-id "container")
        items-container-id (random-id "items-container")
        squares-container-id (random-id "squares-container")
        dragging-piece-id (random-id "dragging-piece")

        their-config (js-api/parse-constructor-second-arg js-opts)
        starting-config (config/merge-config their-config)

        default-num-cols 8
        square->square-ids (square-util/create-random-square-ids default-num-cols default-num-cols)

        opts3 (assoc starting-config
                :animate-speed-ms default-animate-speed-ms
                :animation-end-callbacks {}
                :container-id container-id
                :coords default-coords
                :dragging-piece-id dragging-piece-id
                :items {}
                :items-container-id items-container-id
                :num-cols default-num-cols
                :num-rows default-num-cols
                ;; the percent that piece images fill in the Item container
                ;; ie: what percent of the square should be filled with the image of the piece
                ;; TODO: make this configurable
                :piece-square-pct 0.95
                :root-el root-el
                :show-coords? true ;; are the Coordinates showing?
                :square->piece-id {}
                :square->square-ids square->square-ids
                :squares-container-id squares-container-id)

        ;; create an atom to track the state of the board
        board-state (atom opts3)]

    ;; add a watch function to the board state
    (add-watch board-state :on-change board-state-change)

    ;; Initial DOM Setup
    (init-dom! board-state)
    (add-events! root-el board-state)
    (draw-items-instant! board-state)

    ;; return a JS object that implements the API
    (js-obj
      "addArrow" (partial js-add-arrow board-state)
      "arrows" (partial js-get-arrows board-state)
      "clearArrows" (partial api/clear-arrows board-state)
      "getArrows" (partial js-get-arrows board-state)
      "removeArrow" (partial js-remove-arrow board-state)

      "addCircle" (partial js-add-circle board-state)
      "circles" (partial js-get-circles board-state)
      "clearCircles" (partial clear-circles board-state)
      "getCircles" (partial js-get-circles board-state)
      "removeCircle" (partial js-remove-circle board-state)

      "config" (partial js-api/config board-state)
      "getConfig" (partial js-api/get-config board-state)
      "setConfig" (partial js-api/set-config board-state)

      ;; FIXME: allow adding custom items
      ;; https://github.com/oakmac/chessboard2/issues/9
      "addItem" (partial js-api/add-item board-state)
      "clearItems" #() ;; FIXME
      "getItems" (partial js-get-items board-state) ;; FIXME: should be able to pass the string type here as an argument
      "items" (partial js-get-items board-state)
      "moveItem" (partial js-api/move-item board-state)
      "removeItem" (partial js-api/remove-item board-state)

      ; "addPiece" (partial js-add-piece board-state) ;; FIXME: write this
      "clearPieces" (partial js-api/clear board-state)
      "getPieces" (partial js-get-pieces board-state)
      "pieces" (partial js-get-pieces board-state)
      ; "removePiece" (partial js-remove-piece board-state) ;; FIXME: write this

      "clear" (partial js-api/clear board-state)
      "move" (partial js-api/move-piece board-state)
      ;; FIXME: moveInstant ???
      "position" (partial js-api/position board-state)
      "getPosition" (partial js-api/get-position board-state)
      "setPosition" (partial js-api/set-position board-state)

      "destroy" (partial api/destroy! board-state)
      "fen" (partial js-api/fen board-state)

      ;; FIXME: implement these
      ;; https://github.com/oakmac/chessboard2/issues/27
      ;; - should this just be "clearSquares()"?
      ;; - would also be nice if this returned x,y coordinates of the squares, for custom integrations
      ; "clearSquareHighlights" #()
      ; "getSquares" #() ;; squares can have colors, ids, properties like "black" and "white"
      ;                  ;; whether they are highlighted or not
      ; "setSquare" #() ;; .setSquare('e2', 'blue')
      ; "squares" #()

      ;; FIXME: implement these
      ;; https://github.com/oakmac/chessboard2/issues/25
      "coordinates" #() ;; FIXME: returns the current state with 0 arg, allows changing with other args
      "getCoordinates" #() ;; FIXME: returns the config object
      "hideCoordinates" (partial hide-coordinates! board-state)
      "setCoordinates" #() ;; FIXME: sets the config object (do we need this? just use .setConfig() ?)
      "showCoordinates" (partial show-coordinates! board-state)
      "toggleCoordinates" (partial toggle-coordinates! board-state)

      "flip" #(orientation board-state "flip")
      "orientation" (partial orientation board-state)
      "getOrientation" (partial orientation board-state nil)
      "setOrientation" (partial orientation board-state)

      ;; FIXME: implement these
      ;; https://github.com/oakmac/chessboard2/issues/28
      ; "animatePiece" #()
      ; "bouncePiece" #()
      ; "flipPiece" #() ;; rotate a piece upside down with animation
      ; "pulsePiece" #()

      "resize" (partial api/resize! board-state)

      "start" (partial js-api/start board-state))))

(defn constructor
  "Called to create a new Chessboard2 object."
  ([]
   (error-log "Please pass a DOM element, element ID, or query selector as the first argument to the Chessboard2() function."))
  ([el]
   (constructor el {}))
  ([el js-opts]
   (let [root-el (dom-util/get-element el)]
     (if-not root-el
       (error-log "Unable to find DOM element:" el)
       (constructor2 root-el js-opts)))))

(when flags/runtime-checks?
  (info-log "runtime-checks? are enabled ✅"))

;; Export / Module
(goog-define ES_MODULE false)

;; put Chessboard2 on the window object
(when (and (not ES_MODULE)
           (exists? js/window)
           (not (fn? (gobj/get js/window "Chessboard2"))))
  (gobj/set js/window "Chessboard2" constructor))

;; common JS export
(when (and (not ES_MODULE)
           (exists? js/exports)
           (not= (type (gobj/get js/exports "nodeName")) "string"))
  (gobj/set js/exports "Chessboard2" constructor))

;; TODO: do we need to support AMD?
