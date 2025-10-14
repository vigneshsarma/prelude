;; -*- lexical-binding: t; -*-

(require 'prodigy)

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
  :name "core-saml:3000(lein)"
  :env '(("RELEASE" "prodigy")
         ("APP_CONFIG" "config/config.dev.properties")
         ;; ("APP_VERBOSE" "true")
         )
  :ready-message "Started server on port 3000"
  :command "lein"
  :args '("with-profile" "+repl" "ring" "server-headless")
  :cwd (expand-file-name "Concur/core-saml/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)

(prodigy-define-service
  :name "oauth2:3000(lein)"
  :env '(("RELEASE" "prodigy")
         ("APP_CONFIG" "config/config.edn"))
  :ready-message "Starting server at host 0.0.0.0"
  :command "lein"
  :args '("do" "clean," "with-profile" "+dev,+repl" "run" "-c" "config/config.edn,test/integration/resources/certs/secrets.edn")
  :cwd (expand-file-name "Concur/oauth2/"
                         (getenv "HOME"))
  :stop-signal 'sigkill)

(prodigy-define-service
  :name "clojure-ai:3002(bb)"
  :command "bb"
  :ready-message "SAP AI Proxy started successfully"
  :args '("start")
  :cwd (expand-file-name "Concur/clojure-ai-core-proxy/"
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
