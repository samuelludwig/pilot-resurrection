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

# TODO: I think help should its own separate program, it feels/can be more
# general. I could then call to it here.
#(defn run-help [segments] nil)

(defn run-new
  [segments]
  (let [path (fs/pathify ;segments)
        stock-template "#!/usr/bin/env sh"]
    (fs/create-executable path stock-template)
    (open-in-editor path)))

(defn run-cat
  [segments]
  (let [path (fs/pathify ;segments)]
    (os/execute [(config :cat-provider) (mainpath-of path)] :p)))

(defn run-edit [segments]
  (open-in-editor
    (fs/pathify ;segments)))

(defn run-which
  [segments]
  (let [path (fs/pathify ;segments)]
    (os/execute ["echo" path] :p)))

(defn run-script [args]
  (let [[path args] (split-into-path-and-args args)]
    (cond
      (fs/entity-does-not-exist? path) (do
                                         (print "File not found:" path)
                                         (os/exit 1))
      (fs/dir? path) (os/execute ["ls" path] :p)
      (fs/not-executable? path) (run-cat path)
      (os/execute [path ;args] :p))))

# Think it would be nice to have default behaviors for new, cat, which, help,
# etc. and allow the user to override them with their own implementations. TODO
#
# We should then also provide a facility that will alert whats being
# overridden.

(defn main
  [this-file & args]
  (let [script-dir (config :script-dir)]
    (case (first args)
      "which" (run-which [script-dir ;(rest args)])
      "cat" (run-cat [script-dir ;(rest args)])
      "edit" (run-edit [script-dir ;(rest args)])
      "new" (run-new [script-dir ;(rest args)])
      (run-script [script-dir ;args]))))
