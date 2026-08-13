(ns library-test
  (:require [cljs.reader :refer [read-string]]
            [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.library :as library]
            [ogres.app.vec :as vec :refer [Vec2]]))

(deftest test-image->entry-anchor-round-trips-as-plain-vector
  (testing ":image/anchor is a Vec2, whose own printer emits an
            unreadable #vec2[x,y] tag -- image->entry must convert it to
            a plain [x y] *before* anything downstream ever calls
            pr-str, or export! would silently produce a file that throws
            on import."
    (let [entry (library/image->entry {:image/hash "abc" :image/anchor (Vec2. 12 34)})]
      (is (= (:anchor entry) [12 34]))
      (is (vector? (:anchor entry)))
      ;; The actual trap: a raw pr-str/read-string round trip of the
      ;; *entry* (not the Vec2 itself) must not throw, and must come
      ;; back with the same plain vector -- proving export!'s blob and
      ;; import!'s read-string are safe to use as-is.
      (is (= (read-string (pr-str entry)) entry)))))

(deftest test-image->entry-omits-absent-fields
  (testing "only :hash is guaranteed; every other key is omitted (not
            written as nil/false) so a partially-calibrated image
            doesn't round-trip fake values"
    (let [entry (library/image->entry {:image/hash "abc"})]
      (is (= entry {:hash "abc"}))))
  (testing "a locally-uploaded image (bare SHA-1 hash) carries no
            :location -- only a URL/data-uri hash does"
    (let [entry (library/image->entry {:image/hash "deadbeef"})]
      (is (not (contains? entry :location))))
    (let [entry (library/image->entry {:image/hash "https://example.com/a.png"})]
      (is (= (:location entry) "https://example.com/a.png")))))

(deftest test-sanitize-entry-drops-hashless-entries
  (testing "no usable :hash -- the whole entry is meaningless, dropped"
    (is (nil? (library/sanitize-entry {:name "token.png" :cell-px 70})))
    (is (nil? (library/sanitize-entry {:hash ""})))
    (is (nil? (library/sanitize-entry {:hash "   "})))
    (is (nil? (library/sanitize-entry "not-even-a-map")))))

(deftest test-sanitize-entry-coerces-garbage-anchor-to-absent-not-a-failure
  (testing "a malformed :anchor doesn't fail the entry -- it's dropped,
            same reject-don't-coerce contract as the rest of the file"
    (let [entry (library/sanitize-entry {:hash "abc" :anchor "not-a-vector"})]
      (is (= (:hash entry) "abc"))
      (is (not (contains? entry :anchor))))
    (let [entry (library/sanitize-entry {:hash "abc" :anchor [1 2 3]})]
      (is (not (contains? entry :anchor))))
    (let [entry (library/sanitize-entry {:hash "abc" :anchor [1 "two"]})]
      (is (not (contains? entry :anchor))))
    (let [entry (library/sanitize-entry {:hash "abc" :anchor [1 2]})]
      (is (= (:anchor entry) [1 2])))))

(deftest test-sanitize-entry-keeps-only-usable-fields
  (let [entry (library/sanitize-entry
               {:hash "abc" :name "  goblin.png  " :width 64 :height "not-a-number"
                :cell-px 70 :rotation 90 :location "  https://example.com/a.png  "
                :public true :default-label 42 :url ""})]
    (is (= entry {:hash "abc"
                  :name "goblin.png"
                  :width 64
                  :cell-px 70
                  :rotation 90
                  :location "https://example.com/a.png"
                  :public true}))))

(deftest test-sanitize-manifest-rejects-unrecognized-format
  (is (nil? (library/sanitize-manifest {:library/gallery :token :library/images []} :token))
      "missing/wrong :library/format -- not one of ours at all"))

(deftest test-sanitize-manifest-rejects-gallery-mismatch
  (testing "the enforcement point for per-gallery isolation -- a
            .token-library file must not partially succeed when pointed
            at the Props panel's import control"
    (let [data {:library/format library/format-id
                :library/gallery :token
                :library/name "My Tokens"
                :library/images [{:hash "abc"}]}]
      (is (some? (library/sanitize-manifest data :token)))
      (is (nil? (library/sanitize-manifest data :props)))
      (is (nil? (library/sanitize-manifest data :scene)))))
  (testing "an unrecognized gallery keyword is refused outright, not
            just mismatched"
    (let [data {:library/format library/format-id
                :library/gallery :not-a-real-gallery
                :library/images [{:hash "abc"}]}]
      (is (nil? (library/sanitize-manifest data :token))))))

(deftest test-sanitize-manifest-drops-bad-entries-keeps-good-ones
  (let [data {:library/format library/format-id
              :library/gallery :props
              :library/images [{:hash "good"} {:name "no hash here"} "garbage" nil]}
        result (library/sanitize-manifest data :props)]
    (is (= (:gallery result) :props))
    (is (= (count (:entries result)) 1))
    (is (= (:hash (first (:entries result))) "good"))))

(deftest test-sanitize-manifest-falls-back-to-a-default-name
  (let [data {:library/format library/format-id
              :library/gallery :scene
              :library/images []}]
    (is (= (:name (library/sanitize-manifest data :scene)) "Imported library"))))
