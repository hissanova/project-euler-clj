(ns project-euler-clj.core
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st])
  (:gen-class))

(def current-directory (clojure.java.io/file "./src/project_euler_clj"))
(def files (for [file (file-seq current-directory)] (.getName file)))

;; (map (comp #(str "./src/project_euler_clj/" %))
;;      (filter #(re-matches #"problem_(.*).clj" %) files))

;; (for [file-str (filter #(re-matches #"problem_(.*).clj" %) files)]
;;   (load-file (str "./src/project_euler_clj/" file-str)))
(require 'project-euler-clj.problem-041)

(map println (file-seq (clojure.java.io/file "./src/project_euler_clj")))

(defn hello-world
  [x]
  (println x " Hello World!"))
  
