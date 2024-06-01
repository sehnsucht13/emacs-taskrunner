;;; taskrunner-clojure-cli.el --- Provide functions to retrieve clojure tasks via Clojure's cli -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Yavor Konstantinov

;;;; Commentary:
;; Provide support for Clojure(deps.edn)

;;;; Code:

;;;; Requirements
(require 'cl-lib)

;;;; Variables

(defconst taskrunner-clojure-cli--query
  "(require '[clojure.tools.deps] '[clojure.string] '[clojure.set])
(let [edn-srcs (clojure.tools.deps/create-edn-maps nil)
      src-aliases (reduce-kv #(assoc %1 %2 (:aliases %3)) {} edn-srcs)]
  (doseq [[prefix target] [[\"-X\" :exec-fn]
                           [\"-M\" :main-opts]]]
    (let [invokable-aliases (reduce-kv
                             (fn [m src aliases]
                               (assoc m
                                      src
                                      (reduce-kv
                                       (fn [a alias alias-defn]
                                         (cond-> a
                                           (pos? (count (clojure.set/intersection #{target} (set (keys alias-defn)))))
                                           (assoc alias alias-defn)))
                                       {} aliases)))
                             {} src-aliases)
          all-aliases (->> invokable-aliases (map val) (mapcat #(-> % keys sort)) distinct)]
      (doseq [alias all-aliases]
        (let [srcs (reduce-kv (fn [srcs src deps-edn]
                                (if (contains? (:aliases deps-edn) alias)
                                  (conj srcs src)
                                  srcs))
                              [] edn-srcs)]
          (println (str \"(\" (clojure.string/join \", \" (map name srcs)) \")\") (str prefix alias)))))))"
  "Clojure snippet used to retrieve scoped aliases")

;;;; Functions

;; These are here just to silence the bytecompiler. They are defined in
;; `taskrunner.el' and will be loaded later on but due to these files being
;; required before the function being loaded, a warning is emitted.

(declare-function taskrunner--make-task-buff-name "ext:taskrunner")

(defun taskrunner-clojure-cli--get-tasks-from-buffer ()
  "Retrieve all clojure-cli tasks from the current buffer."
  (remove nil
          (cl-map 'list (lambda (elem)
                          (when (string-prefix-p "(" elem)
                            (concat "CLOJURE " elem)))
                  (split-string (buffer-string) "\n"))))

(defun taskrunner-get-clojure-cli-tasks (DIR)
  "Retrieve the tasks for the project in directory DIR.
This function returns a list of the form:
\(\"CLOJURE TASK1\" \"CLOJURE TASK2\"...)"
  (let ((default-directory DIR)
        (task-buffer (taskrunner--make-task-buff-name "clojure-cli"))
        (clojure-tasks))

    (call-process "clojure" nil task-buffer nil
                  "-M" "-e" taskrunner-clojure-cli--query)
    (with-temp-buffer
      (set-buffer task-buffer)
      (goto-char (point-min))
      (setq clojure-tasks (taskrunner-clojure-cli--get-tasks-from-buffer))
      (kill-current-buffer)
      clojure-tasks)))

(provide 'taskrunner-clojure-cli)
;;; taskrunner-clojure-cli.el ends here
