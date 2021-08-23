(ns ca.clojurist.subtitles.ingester.subrip
  "SubRip format subtitle ingester."
  {:author "Robert Medeiros" :email "robert@clojurist.ca"}
  (:require
   [clojure.string :as string])
  (:require
   [ca.clojurist.subtitles.time :as time]))


(defn extract-index
  "Extract the cue index from a sequence of lines representing a
  subtitle. The index is expected to be the only content of the first
  line of the line sequence. If the index is the string
  representation of an integer it will be returned as an Integer
  instance value for key :index in a map, otherwise nil is returned."
  [sub-seq]
  {:pre [(seq? sub-seq) (> (count sub-seq) 0)]
   :post [(map? %)]}
  (let [raw-idx (-> sub-seq first string/trim)
        idx (re-find #"^\d+$" raw-idx)]
    (when idx
      {:index (Integer/parseInt idx)})))

(defn extract-duration
  "Extract the start and end time of a subtitle in milliseconds. A map
  having keys :start-ms and :end-ms is returned. The timestamps are
  assumed to have the format 'hh:mm:ss,µµµ' and be separated by the
  string ' --> ', e.g. '00:00:00,000 --> 00:00:01,000'."
  [sub-seq]
  {:pre [(seq? sub-seq) (> (count sub-seq) 1)]
   :post [(map? %)]}
  (let [raw-duration (-> sub-seq second string/trim)
        [start-time end-time] (string/split raw-duration #"-->")
        ts->ms #(-> % string/trim (time/parse-time :decimal \,) time/time->ms)
        start-ms (ts->ms start-time)
        end-ms (ts->ms end-time)]
    {:start-ms start-ms
     :end-ms end-ms}))

;; NB: the subtitle should contain at least the integer index and
;; the duration string. There may or may not be any subtitles; empty
;; subtitles can be stripped out later on.
(defn extract-text
  "Extract the line(s) of subtitle text from a sequence of lines. A
  map is returned having key :lines whose value is a vector of
  strings representing each line of cue text."
  [sub-seq]
  {:pre [(seq? sub-seq) (>= (count sub-seq) 2)]}
  (let [;; First two entries are index and duration.
        lines-raw (->> sub-seq (drop 2) (map string/trim))
        ;; Keep only lines that contain non-whitespace.
        lines (into [] (filter (comp pos? count) lines-raw))]
    {:lines lines}))

(defn duration?
  "Returns true if input string matches the format used to indicate
caption display time."
  [s]
  (->> s string/trim
       (re-matches #"\d+:\d+:\d+,\d+\s+-->\s+\d+:\d+:\d+,\d+")))

(defn cue-indexes
  "Return the sequence of line numbers on which new cues begin."
  [lines]
  (let [not-nil? (complement nil?)
        ;; If the following string contains a cue duration spec, return
        ;; the current string, otherwise return nil.
        duration-follows? (fn [s t] (if (duration? t) s nil))
        ;; Return a seq of cue start strings or nil; to examine
        ;; successor elements we pass the same sequence of lines with
        ;; the first element truncated.
        cue-starts (map duration-follows? lines (rest lines))]
    ;; Return only the integer indexes of the lines that precede a
    ;; duration spec, i.e. the cue starting lines.
    (keep-indexed
     (fn [idx val] (if (not-nil? val) idx))
     cue-starts)))

(defn cue-intervals
  "Turn a sequence of integers representing strictly increasing line
numbers into a sequence of non-overlapping line number ranges
representing the lines that each cue contains.

  (0 5 10 13 15) => ([0 4] [5 9] [10 12] [13 14] [15 Integer/MAX_VALUE])"
  [indexes]
  {:pre [(seq? indexes) (every? integer? indexes)]}
  (let [pairs (partition 2 1 indexes)
        ;; Decrement the high end of each range so as not to overlap
        ;; with the succeeding interval.
        intervals (map (fn [[a b]] [a (dec b)]) pairs)]
    ;; Append a final interval representing the start of the last cue to
    ;; the end of the input file.
    (concat intervals `([~(last indexes) ~Integer/MAX_VALUE]))))

(defn interval-contains-line?
  ""
  [line-no [low high]]
  {:pre [(every? integer? [line-no low high])]}
  (and (>= line-no low) (<= line-no high)))

(defn line-no->interval
  "Return the interval into which each line falls; used to partition the
input lines."
  [intervals [line-no _]]
  (first (filter (partial interval-contains-line? line-no) intervals)))

(defn split-lines
  ""
  [lines]
  {:pre [(vector? lines) (every? string? lines)]}
  (let [;; Get the set of line numbers on which cues start.
        intervals (-> lines cue-indexes cue-intervals)
        ;; Partitioning function that returns the interval (and thus the
        ;; subtitle) into which any line falls.
        partition-by-interval (partial line-no->interval intervals)
        ;; Turn each line into a pair [line-no line].
        idx-line-pairs (map-indexed (fn [idx val] [idx val]) lines)
        ;; Returns a sequence of sequences of vector pairs:
        ;; (([0 "1"] [1 "00:00:02,133 --> 00:00:02,767"] [2 "* text"] [3 ""])
        ;;  ...)
        ;; where each pair is [line-no line] and each nested sequence is
        ;; a single caption.
        cue-sequences (partition-by partition-by-interval idx-line-pairs)
        ]
    (for [cue-sequence cue-sequences]
      ;; Keep only the line of cue text, dropping the line number.
      (map (fn [[_ line]] line) cue-sequence))))


(defn ->subtitle
  "Convert a sequence of lines representing a subtitle from a SubRip
  file into a map describing the subtitle."
  [sub]
  {:pre [(seq? sub)]}
  (merge (extract-index sub)
         (extract-duration sub)
         (extract-text sub)))

(defn import-subrip
  "Take a string containing the contents of an SubRip subtitle file
  and return a vector of maps representing subtitles."
  [s]
  {:pre [(string? s) (> (count s) 0)]}
  (let [lines (string/split-lines s)
        sub-seqs (split-lines lines)
        ;; Returns true if string contains non-whitespace characters.
        non-whitespace-fn #(not= "" (string/trim %))
        ;; Remove whitespace from each subsequence.
        no-whitespace (map #(filter non-whitespace-fn %) sub-seqs)
        ;; Remove any subsequences made empty by removing whitespace.
        no-whitespace (remove empty? no-whitespace)]
    (into [] (map ->subtitle no-whitespace))))
