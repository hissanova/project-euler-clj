(ns project-euler-clj.core
  (:require [clojure.string :as st])
  (:gen-class))

(def current-directory (clojure.java.io/file "./src/project_euler_clj"))
(def files (for [file (file-seq current-directory)] (.getName file)))

;; (map (comp #(str "./src/project_euler_clj/" %))
;;      (filter #(re-matches #"problem_(.*).clj" %) files))

;; (for [file-str (filter #(re-matches #"problem_(.*).clj" %) files)]
;;   (load-file (str "./src/project_euler_clj/" file-str)))
(use '[project-euler-clj.common :as common])
(require 'project-euler-clj.problem-13)
(refer 'project-euler-clj.problem-13)

(defn hello-world
  [x]
  (println x " Hello World!"))
  
