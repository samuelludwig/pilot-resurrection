(import ./lib/fsutils :as fs)
(use ./lib/prelude)

(def basic-script
  ``
  #!/usr/bin/env bash
  echo $1 and $2
  ``)

(defn empty-script-dir
  ``
  Wipe the dir at the given path, creating it if it doesn't already exist.
  ``
  [path]
  (when (fs/entity-exists? path)
    (os/execute ["rm" "-r" path] :p))
  (fs/mkdir-p path))

(defn init-script-dir
  ``
  Wipe the dir at the given path, creating it if it doesn't already exist, and
  add a basic script, and directory with a mainfile, at root-level.
  ``
  [path]
  (empty-script-dir path)
  (let [script-path (string path "/hi")]
    (fs/create-executable script-path basic-script))
  (let [script-path (string path "/dir-with-main/main")]
    (fs/mkdir-p (string path "/dir-with-main"))
    (fs/create-executable script-path basic-script)))

(defn install-to-local-bin
  []
  (let [dest (string (os/getenv "HOME") "/.local/bin/pi")]
    (os/execute ["rm" dest] :p)
    (os/execute ["jpm" "quickbin" "src/init.janet" dest] :p)))

(comment

  # Clean and reinitialize the default script dir
  (init-script-dir (string (os/getenv "HOME") "/pilot/scripts"))

  (install-to-local-bin)

  nil)

