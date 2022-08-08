#!/usr/bin/env janet
#(import ./lib/argparse :as ap)
(import ./read-config :as conf)
(import ./lib/fsutils :as fs)
(use ./lib/prelude)

``
`target` is an array of arguments, which can be composed of either path
segments, or script arguments, or both
``

# Step 1. Given a list of segments, we need to determine where the path ends, and the args begin.
# Paths are interpreted from the root of script-dir
# The path ends if either we encounter an executable file, or a dir with an
# executable "main" file

(def default-config
  {:script-dir (string (os/getenv "HOME") "/pilot/scripts")
   :pilot-editor (conf/resolve-default-editor)
   :cat-provider "bat"})

(def config #TODO
  (merge default-config {}))

(defn- dir-with-main? [path]
  (let [has-main? (partial has-equals? "main")]
    (and
      (fs/dir? path)
      (has-main? (os/dir path))
      (fs/executable?
        (fs/pathify path "main")))))

(defn- dir-with-help? [path]
  (let [has-dot-help? (partial has-equals? ".help")]
    (and
      (fs/entity-exists? path)
      (has-dot-help? (os/dir path)))))

(defn- dir-with-template? [path]
  (let [has-template? (partial has-equals? ".template")]
    (and
      (fs/entity-exists? path)
      (has-template? (os/dir path)))))

(defn- mainpath-of
  [path]
  (let [path-append (partial fs/pathify path)]
    (if (dir-with-main? path)
      (path-append "main")
      path)))

# If no flags are specified (meaning command-args/order are also both empty,
# and we actually want to 'run' something), we need to parse the target args
# and break it up into the script path, and the arguments to the script.

# If a --new, --edit, --which, --cat, or --help flag is specified, all the
# target args should be considered the location of the subject only.

(defn split-into-path-and-args
  ``
  Given a sequential array of arguments, determine which arguments are suitable
  to consider as part of a filepath, and which, if any, ought to be interpreted
  as script arguments.

  There are two cases in which we may have a populated array of script
  arguments:

  A. `path` resolves to an executable file
  B. `path` resolves to a directory that contains a file with the name `main`

  All other cases will result in an empty array of script args, and a path that
  may or may not be valid.
  ``
  [args &opt path]
  (let [path-append (partial fs/pathify path)
        [next-arg rem-args] (hd-tl args)]
    (cond
      (nil? path) (split-into-path-and-args rem-args (path-append next-arg))
      (dir-with-main? path) [(path-append "main") args] #run
      (fs/executable? path) [path args] #run
      (empty? args) [path []]

      # Continue building the path.
      (fs/dir? path) (split-into-path-and-args rem-args (path-append next-arg))

      # Not executing (we can no longer make a valid existing path), therefore
      # we don't have args.
      [(path-append ;args) []])))

(comment
  (def home (os/getenv "HOME"))
  (split-into-path-and-args ["hi" "arg1" "arg2"] (string home "/pilot/scripts"))
  (split-into-path-and-args [(string home "/pilot/scripts") "hi" "arg1" "arg2"])
  (split-into-path-and-args
    [(string home "/pilot/scripts") "dir-with-main" "arg1" "arg2"])
  (split-into-path-and-args [(string home "/pilot/scripts") "hi" "arg1" "arg2"])

  # TODO: Should we error in this case?
  (split-into-path-and-args ["/nowhere" "hi" "arg1" "arg2"]))

(defn run [path args]
  (os/execute [path ;args]))

(def invalid-path?
  (partial
    meets-any-criteria? [fs/entity-does-not-exist? fs/not-executable?]))

(defn open-in-editor
  [path]
  (os/execute [(config :pilot-editor) (mainpath-of path)] :p))

(defn build-target-path-from-segment-list
  [target]
  (first (split-into-path-and-args target)))

(defn run-cat
  [path]
  (os/execute [(config :cat-provider) (mainpath-of path)] :p))

(defn run-edit [path] (open-in-editor path))

(defn run-which
  [path]
  (os/execute ["echo" path] :p))

(defn run-script [target]
  (let [[path args] (split-into-path-and-args target)]
    (cond
      #(fs/entity-does-not-exist? path) (run-help target)
      #(fs/dir? path) (run-help target)
      (fs/not-executable? path) (run-cat path)
      (os/execute [path ;args] :p))))

(defn main
  [this-file & args]
  (let [script-dir (config :script-dir)]
    (cond
      #(= (first args) "which") (run-which [script-dir ;(rest args)])
      #(= (first args) "cat") (run-cat [script-dir ;(rest args)])
      :default (run-script [script-dir ;args]))))
