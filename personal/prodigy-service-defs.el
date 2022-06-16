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
  :name "bulky-mocks:8924(py)"
  :command "flask"
  :args '("run" "--debug")
  :env '(("FLASK_APP" "mock/users_api.py"))
  :cwd (expand-file-name "Concur/bulky/"
                         (getenv "HOME"))
  :stop-signal 'sigkill
  :init (lambda () (pipenv-activate)))

(prodigy-define-service
  :name "bulky:6666"
  :command "cargo"
  :env '(("VERSION" "1")
         ("BULKY_ENV" "dev")
         ("BULKY_S3_BUCKET" "concur-sap-bulk-password-reset-non-prod-us")
         ("BULKY_WORKER_IMAGE" "quay.cnqr.delivery/core/bulky-worker:d78ae267e916f7fa87a0f6d43d2d13d94b5c6c6c")
         ;; ("http_proxy" "http://ess:HFRrnMBaEHcP7Rnzm276U@127.0.0.1:31288")
         ("RUST_BACKTRACE" "1")
         ("AWS_ACCESS_KEY_ID" "AKIA4VXDXNW5B6K4DFU2")
         ("AWS_SECRET_ACCESS_KEY" "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")
         ("POD_NAMESPACE" "test"))
  :args '("run" "--bin" "bulky-server" "--features=server")
  ;; :ready-message "Server starting at"
  :cwd (expand-file-name "Concur/bulky/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)

(prodigy-define-service
  :name "bulky:5555"
  :command "go"
  :env '(("VERSION" "1")
         ("BULKY_ENV" "dev")
         ("BULKY_S3_BUCKET" "concur-sap-bulk-password-reset-non-prod-us")
         ("BULKY_WORKER_IMAGE" "quay.cnqr.delivery/core/bulky-worker:68f73a4e75dfd574448cf3a287f0d9bfd687582c")
         ;; ("http_proxy" "http://ess:HFRrnMBaEHcP7Rnzm276U@127.0.0.1:31288")
         ("AWS_ACCESS_KEY_ID" "AKIA4VXDXNW5B6K4DFU2")
         ("AWS_SECRET_KEY" "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")
         ("POD_NAMESPACE" "test"))
  :args '("run" "lb.go" "handlers.go" "state.go")
  :ready-message "Server starting at"
  :cwd (expand-file-name "Concur/bulky/lb/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)

(prodigy-define-service
  :name "cte-authn:3000(lein)"
  :env '(("RELEASE" "prodigy")
         ("APP_CONFIG" "config/config.dev.edn")
         ;; ("JVM_OPTS" " -Djavax.net.ssl.trustStore=/Users/i323200/Concur/keys/mitm.jks -Djavax.net.ssl.trustStorePassword=password ")
         )
  :command "lein"
  :args '("ring" "server-headless")
  :cwd (expand-file-name "Concur/cte-authn/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)

(prodigy-define-service
  :name "cte-authn-repl(lein)"
  :env '(("RELEASE" "prodigy")
         ("APP_CONFIG" "config/config.dev.edn"))
  :command "lein"
  :args '("repl")
  :cwd (expand-file-name "Concur/cte-authn/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)


;; (expand-file-name "Concur/oidc/config/config.dev.edn"
;;                   (getenv "HOME"))

(prodigy-define-service
  :name "oidc:3000(lein)"
  :env '(("RELEASE" "prodigy")
         ("APP_CONFIG" "config/config.dev.edn")
         ;; ("APP_VERBOSE" "true")
         )
  :command "lein"
  :args '("with-profile" "+repl" "ring" "server-headless")
  :cwd (expand-file-name "Concur/oidc/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)

(prodigy-define-service
  :name "saml:3000(lein)"
  :env '(("RELEASE" "prodigy")
         ("APP_CONFIG" "config/config.dev.properties")
         ;; ("APP_VERBOSE" "true")
         )
  :ready-message "Started server on port 3000"
  :command "lein"
  :args '("with-profile" "+repl" "ring" "server-headless")
  :cwd (expand-file-name "Concur/saml1/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)

(prodigy-define-service
  :name "oauth2:3000(lein)"
  :env '(("RELEASE" "prodigy")
         ("APP_CONFIG" "config/config.dev.properties"))
  :command "lein"
  :args '("with-profile" "+repl" "ring" "server-headless")
  :cwd (expand-file-name "Concur/oauth2/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)

(prodigy-define-service
  :name "token:3000(lein)"
  :env '(("RELEASE" "prodigy")
         ("APP_CONFIG" "config/config.dev.properties"))
  :command "lein"
  :args '("ring" "server-headless")
  :cwd (expand-file-name "Concur/token/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)

(prodigy-define-service
  :name "keystore:3000(lein)"
  :env '(("RELEASE" "prodigy")
         ("APP_CONFIG" "config/config.dev.properties"))
  :command "lein"
  :args '("ring" "server-headless")
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
