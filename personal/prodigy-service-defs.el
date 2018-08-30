(require 'prodigy)
(require 'virtualenvwrapper)

(prodigy-define-service
 :name "itin:8000(py)"
 :command "python"
 :args '("manage.py" "runserver")
 :cwd "/Users/vigneshS/code/itin"
 :stop-signal 'sigkill
 :init (lambda () (venv-workon "moneym")))

(prodigy-define-service
  :name "cte-authn:3000(lein)"
  :env '(("RELEASE" "prodigy")
         ("APP_CONFIG" "config/config.dev.edn"))
  :command "lein"
  :args '("ring" "server-headless")
  :cwd "/Users/vigneshS/concur/legacy-authn/"
  :stop-signal 'sigkill)
