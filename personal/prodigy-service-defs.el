(require 'prodigy)
(require 'pipenv)

(prodigy-define-service
  :name "itin:8000(py)"
  :command "python"
  :args '("manage.py" "runserver")
  :cwd (expand-file-name "Code/itin"
                         (getenv "HOME"))
  :stop-signal 'sigkill
  :init (lambda () (pipenv-activate)))

(prodigy-define-service
  :name "hugo:1313"
  :command "hugo"
  :args '("-w" "-D" "server")
  :cwd (expand-file-name "Code/hittaruki.info/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)

(prodigy-define-service
  :name "cte-authn:3000(lein)"
  :env '(("RELEASE" "prodigy")
         ("APP_CONFIG" "config/config.dev.edn"))
  :command "lein"
  :args '("ring" "server-headless")
  :cwd (expand-file-name "Concur/legacy-authn/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)
