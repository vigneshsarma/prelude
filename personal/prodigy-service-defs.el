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
  :cwd (expand-file-name "Concur/cte-authn/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)

(prodigy-define-service
  :name "oauth2:3000(lein)"
  :env '(("RELEASE" "prodigy")
         ("APP_CONFIG" "config/config.dev.properties"))
  :command "lein"
  :args '("ring" "server-headless")
  :cwd (expand-file-name "Concur/oauth2/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)

(prodigy-define-service
  :name "keystore:3000(lein)"
  :env '(("RELEASE" "prodigy")
         ("APP_CONFIG" "config/config.dev.properties"))
  :command "lein"
  :args '("ring" "do" "clean" "server-headless")
  :cwd (expand-file-name "Concur/keystore/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)

(prodigy-define-service
  :name "tickets4sale:3010(lein)"
  ;; :env '(("RELEASE" "prodigy")
  ;;        ("APP_CONFIG" "config/config.dev.edn"))
  :command "lein"
  :args '("ring" "server-headless")
  :cwd (expand-file-name "Code/tickets4sale/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)
