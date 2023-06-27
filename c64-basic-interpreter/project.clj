(defproject c64-basic-interpreter "0.1.0-SNAPSHOT"
  :description "Intérprete de Commodore 64 en Basic - Autor: Amaru Gabriel Durán"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main c64-basic-interpreter.core
  :aot [c64-basic-interpreter.core]
  :repl-options {:init-ns c64-basic-interpreter.core})
