(ns git
  (:import 
    [java.io File]
    [org.eclipse.jgit.http.server GitServlet]
    [org.eclipse.jgit.transport.resolver FileResolver]
    [org.eclipse.jetty.server Server]
    [org.eclipse.jetty.ee10.servlet ServletContextHandler ServletHolder]))

(def repo-root (File. "/tmp/git"))

(def git-servlet
  (doto (GitServlet.)
    (.setRepositoryResolver
      (FileResolver. repo-root true))))

(def server (Server. 8080))
(def context (ServletContextHandler. "/"))


(defn run [opts]
  (.addServlet context (ServletHolder. git-servlet) "/*")
  (.setHandler server context)
  (.start server))

