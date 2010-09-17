(ns ring.middleware.upload-progress
  "Parse multipart upload into params and provide progress information."
  (:use [clojure.contrib.def :only (defvar-)]
        [ring.middleware.params :only (assoc-param)]
        [ring.middleware.session.store :only (read-session write-session)])
  (:import (org.apache.commons.fileupload
             FileUpload RequestContext ProgressListener)
           (org.apache.commons.fileupload.disk
             DiskFileItemFactory
             DiskFileItem)))

(defn- get-session-key
  [request options]
  (let [cookie-name (or (:cookie-name options) "ring-session")]
    (get-in request [:cookies cookie-name :value])))

(defn- make-progress-listener
  [store key]
  (proxy [ProgressListener] []
    (update [bytes-read content-length items]
            (let [session (read-session store key)]
              (write-session store key (assoc session :upload-progress
                                              {:bytes-read bytes-read
                                               :content-length content-length
                                               :items items}))))))

(defn- multipart-form?
  "Does a request have a multipart form?"
  [request]
  (if-let [^String content-type (:content-type request)]
    (.startsWith content-type "multipart/form-data")))

(defn- ^FileUpload file-upload
  [store key]
  (doto (FileUpload.
    (doto (DiskFileItemFactory.)
      (.setSizeThreshold -1)
      (.setFileCleaningTracker nil)))
    (.setProgressListener (make-progress-listener store key))))

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
  [request encoding store key]
  (reduce
    (fn [param-map, ^DiskFileItem item]
      (assoc-param param-map
        (.getFieldName item)
        (if (.isFormField item)
          (if (.get item) (.getString item))
          (file-map item))))
    {}
    (.parseRequest
       (file-upload store key)
       (request-context request encoding))))

(defn wrap-upload-progress
  "Middleware to parse multipart parameters from a request. Adds the
  following keys to the request map:
    :multipart-params - a map of multipart parameters
    :params           - a merged map of all types of parameter

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
    (let [encoding (or (:encoding opts)
                       (:character-encoding request)
                       "UTF-8")
          params   (if (multipart-form? request)
                     (parse-multipart-params request encoding session-store (get-session-key request opts))
                     {})
          request  (merge-with merge request
                     {:multipart-params params}
                     {:params params})]
      (handler request))))
