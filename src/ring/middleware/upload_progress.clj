(ns ring.middleware.upload-progress
  "Parse multipart upload into params and provide progress information."
  (:use [ring.middleware.params :only (assoc-param)]
        [ring.middleware.session.store :only (read-session write-session)])
  (:import (org.apache.commons.fileupload
             FileUpload RequestContext ProgressListener)
           (org.apache.commons.fileupload.disk
             DiskFileItemFactory
             DiskFileItem)))

(defn- get-session-key
  [request options]
  (let [cookie-name (:cookie-name options "ring-session")]
    (get-in request [:cookies cookie-name :value])))

(defn- make-progress-listener
  [swap-session!]
  (proxy [ProgressListener] []
    (update [bytes-read content-length items]
      (swap-session! assoc :upload-progress
                     {:bytes-read bytes-read
                      :content-length content-length
                      :items items}))))

(defn- multipart-form?
  "Does a request have a multipart form?"
  [request]
  (if-let [^String content-type (:content-type request)]
    (.startsWith content-type "multipart/form-data")))

(defn- ^FileUpload file-upload
  [swap-session!]
  (doto (FileUpload.
    (doto (DiskFileItemFactory.)
      (.setSizeThreshold -1)
      (.setFileCleaningTracker nil)))
    (.setProgressListener (make-progress-listener swap-session!))))

(defn- request-context
  "Create a RequestContext object from a request map."
  {:tag RequestContext}
  [request encoding]
  (reify RequestContext
    (getContentType [this]       (:content-type request))
    (getContentLength [this]     (:content-length request))
    (getCharacterEncoding [this] encoding)
    (getInputStream [this]       (:body request))))

(defn- file-map
  "Create a file map from a DiskFileItem."
  [^DiskFileItem item]
  (with-meta
    {:filename     (.getName item)
     :size         (.getSize item)
     :content-type (.getContentType item)
     :tempfile     (.getStoreLocation item)}
    {:disk-file-item item}))

(defn- parse-multipart-params
  "Parse a map of multipart parameters from the request."
  [request encoding swap-session!]
  (reduce
    (fn [param-map, ^DiskFileItem item]
      (assoc-param param-map
        (.getFieldName item)
        (if (.isFormField item)
          (if (.get item) (.getString item))
          (file-map item))))
    {}
    (.parseRequest
       (file-upload swap-session!)
       (request-context request encoding))))

(defn wrap-upload-progress
  "Middleware to parse multipart parameters from a request. Adds the
  following keys to the request map:
    :multipart-params - a map of multipart parameters
    :params           - a merged map of all types of parameter

    :read-session     - a zero-argument function that returns the current
                        content of the session
    :swap-session!    - like swap! on the current session

  Also adds an :upload-progress key to the session that indicates the
  current state of any uploads.

  Takes an optional configuration map. Recognized keys are:
    :encoding - character encoding to use for multipart parsing. If not
                specified, uses the request character encoding, or \"UTF-8\"
                if no request character encoding is set.
    :cookie-name - the name of the session cookie. defaults to \"ring-session\"
"
  [handler session-store & [opts]]
  (fn [request]
    (let [sess-key (get-session-key request opts)
          swap-session! (fn [f & args]
                          (let [session (read-session session-store sess-key)]
                            (write-session session-store sess-key
                                           (apply f session args))))
          encoding (or (:encoding opts)
                       (:character-encoding request)
                       "UTF-8")
          request (assoc request :swap-session! swap-session!
                         :read-session (fn [] (read-session session-store sess-key)))
          params   (if (multipart-form? request)
                     (parse-multipart-params request encoding swap-session!)
                     {})
          request  (merge-with merge request
                     {:multipart-params params}
                     {:params params})]
      (handler request))))
