
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
 |
 | Dependencies:
 |
 | experiment.lisp - because the log contains the trial number
 |                   maintained by the experiment module
 |
 ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
 |
 | dmGraphR module data structure
 |
 ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
 
(defstruct dmGraphR-module
    #| module parameters for timeline and graph |#
    enabled path sampleDuration autoLog
    #| module parameters for timeline |#
    logSpecs maxGraphDepth eventsParametersToLog
    #| module parameters for graph |#
    buffersToGraph
    #| ----------------------------------------------- |#
    #| internal variables for timeline and graph |#
    logFile logEvent postEventHook
    #| internal variables for timeline |#
    chunksToLog
    #| internal variables for graph |#
    vertices vertexAttributes edges)

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
 |
 | dmGraphR module functionality
 |
 ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
 
(defun dmGraphR-start(dmGR)
    (if (dmGraphR-module-enabled dmGR)
        (progn
            (if (dmGraphR-module-logEvent dmGR)
                ; there is an old event that should be stopped first
                (dmGraphR-stop dmGR))
            ; start logging
            (setf (dmGraphR-module-logEvent dmGR) (schedule-periodic-event (dmGraphR-module-sampleDuration dmGR)
                'dmGraphR-log-graph
                :time-in-ms t
                :module 'dmGraphR
                :destination 'dmGraphR
                :maintenance t))
            (setf (dmGraphR-module-postEventHook dmGR) (add-post-event-hook 'dmGraphR-log-event)))))
        
(defun dmGraphR-isLogging(dmGR)
    (if (dmGraphR-module-logEvent dmGR)
        t nil))
        
(defun dmGraphR-stop(dmGR)
    (if (dmGraphR-module-enabled dmGR)
        (progn
            (if (dmGraphR-module-logEvent dmGR)
                (progn (delete-event (dmGraphR-module-logEvent dmGR))
                       (setf (dmGraphR-module-logEvent dmGR) nil)))
            (if (dmGraphR-module-postEventHook dmGR)
                (progn (delete-event-hook (dmGraphR-module-postEventHook dmGR))
                       (setf (dmGraphR-module-postEventHook dmGR) nil))))))
 
(defun find-vertices (dmGR vertex depth)
    (if (and (<= depth (dmGraphR-module-maxGraphDepth dmGR))
             (not (eq vertex nil)))
        (progn
            ; add this vertex, if not in vertices, yet and if it is not
            ; an (unmodified) copy of an existing chunk/did not result
            ; from a merge with another chunk ...
            ; TODO: Using the true chunk name migth mask minor
            ; differences in activation between retrieved chunks in
            ; the retrieval buffer during progressing/after retrieval
            ; and the original chunk
            (if (and (not (member vertex (dmGraphR-module-vertices dmGR)))
                     (not (member (true-chunk-name-fct (chunk-copied-from-fct vertex)) (dmGraphR-module-vertices dmGR)))
                     (chunk-matches-specs vertex (dmGraphR-module-logSpecs dmGR)))
                (progn
                    (setf (dmGraphR-module-vertices dmGR)
                          (append (dmGraphR-module-vertices dmGR) (list vertex))
                    )
                )
            )
            ; add slot values of vertex
            (let ((cType (chunk-chunk-type-fct vertex)))
                (loop for slotName in (chunk-type-slot-names-fct cType) do
                    (let ((slotValue (chunk-slot-value-fct vertex slotName)))
                        ; add slot value to *vertices* if it is not in it yet
                        (if (and (not (eq slotValue nil))
                                 (chunk-p-fct slotValue))
                            (find-vertices dmGR slotValue (+ depth 1)))))))))

#| TODO: use (numberp (position nil '())) |#
(defun dmGraphR-log-graph (dmGRR)
    (declare (ignore dmGRR)) #| TODO: use dmGRR instead of dmGR |#
    (let ((actOld (no-output (first (sgp-fct '(:act )))))
          (sactOld (no-output (first (sgp-fct '(:sact)))))
          (dmGR (get-module dmGraphR))
          (dm (get-module declarative)))
    (if (current-model)
        (progn
            ; Try to find new reachable chunks
            (let ((bufferedChunks (no-output (buffer-chunk-fct (dmGraphR-module-buffersToGraph dmGR)))))
                (loop for chunk in bufferedChunks do
                    (find-vertices dmGR chunk 0)))
             (sgp-fct '(:act nil :sact nil)) ; do not log activation computation of dmGraphR
             (loop for chunk in (dmGraphR-module-vertices dmGR) do
                ; create vertex attributes, i.e. chunk activation
                (let ((blAct (if (< 0 (chunk-reference-count chunk))
                                 (base-level-activation dm chunk) 0))
                      (sAct (suppress-warnings (spreading-activation dm chunk))))
                    (let ((totalAct (+ blAct sAct (activation-offsets dm chunk))))
                        (setf (dmGraphR-module-vertexAttributes dmGR) (append (dmGraphR-module-vertexAttributes dmGR) (list (list
                          chunk (beautifyNodeName chunk) totalAct blAct sAct))))))
                ; create edges attributes (i.e. Wkj and Sji values)
                (let ((cType (chunk-chunk-type-fct chunk)))
                    (loop for slotName in (chunk-type-slot-names-fct cType) do
                        (let ((slotValue (chunk-slot-value-fct chunk slotName)))
                            (if (and (not (eq slotValue nil))
                                     (chunk-p-fct slotValue)
                                     (chunk-matches-specs slotValue (dmGraphR-module-logSpecs dmGR)))
                                (progn
                                    (if (position slotValue (dmGraphR-module-vertices dmGR))
                                        ; only include edges to known vertices - i.e. when
                                        ; logging is not started automatically, no edges to
                                        ; nodes created before start of logging will be
                                        ; shown
                                        (setf (dmGraphR-module-edges dmGR) (append (dmGraphR-module-edges dmGR) (list (list
                                            (+ 1 (position chunk (dmGraphR-module-vertices dmGR)))
                                            (+ 1 (position slotValue (dmGraphR-module-vertices dmGR)))
                                            slotName
                                            (suppress-warnings (sji-fct slotValue chunk)))))))
                                    ))))))
             (sgp-fct (list :act actOld :sact sactOld))
             ; TODO: keep old version of vertices, vertexAttributes and edges
             ; write only differences to disk to reduce the file size
             (with-open-file (outStream (dmGraphR-module-logFile dmGR) :direction :output :if-exists :append :if-does-not-exist :create)
                 ; dump buffer contents
                 (loop for buffer in (dmGraphR-module-buffersToGraph dmGR) do
                    (let ((chunk (car (no-output (buffer-chunk-fct (list buffer))))))
                        (format outStream "buffer~T~D~T~5,3f~T~S~T~4,2f~T~S~T~S~%"
                            (next-stimulus) (mp-time)
                            buffer (buffer-spread buffer)
                            chunk (beautifyNodeName chunk))))
                 ; dump vertex attributes
                 (loop for vAttr in (dmGraphR-module-vertexAttributes dmGR) do
                    (apply #'format
                        (append
                            (list outStream "chunk~T~D~T~5,3f~T~S~T~S~T~4,2f~T~4,2f~T~4,2f~%"
                                (next-stimulus)
                                (mp-time))
                            vAttr)))
                 ; dump edges
                 (loop for edge in (dmGraphR-module-edges dmGR) do
                    (apply #'format
                        (append
                            (list outStream "slot~T~D~T~5,3f~T~D~T~D~T~S~T~4,2f~%"
                                (next-stimulus)
                                (mp-time))
                            edge)))
             )
             ; reset attributes and edges
             (setf (dmGraphR-module-vertexAttributes dmGR) (list ))
             (setf (dmGraphR-module-edges dmGR) (list ))
             ))))

(defun chunk-matches-specs (chunk specs)
    ; TODO: Check whether logSpecs is really a list and turn it into
    ; one, if not, or just log all chunks, if it is not set
     (loop for spec in specs do
        ; TODO: successfully created chunk specs should actually be
        ; cached and re-used, use (no-output ...) in a way that does
        ; not make debugging more difficult? - maybe apply tests like
        ; (chunk-p spec) and construct a custom error message ...
        (if (and (= (length spec) 1)
                 (chunk-p-fct (first spec)))
            (progn
                ;(format t "~S~T~S~T~S~%" chunk "spec" (match-chunk-spec-p chunk spec))
                (if (string-equal chunk (first spec))
                    (return t)))
                nil)
        (if (and (> (length spec) 1)
                 (chunk-type-p-fct (second spec))
                 (setf spec (define-chunk-spec-fct spec)))
            (progn
                ;(format t "~S~T~S~T~S~%" chunk "spec" (match-chunk-spec-p chunk spec))
                (if (match-chunk-spec-p chunk spec)
                    (return t)))
            nil)))
            
(defun dmGraphR-log-event (event)
    (let ((dmGR (get-module dmGraphR)))
        (with-open-file (outStream (dmGraphR-module-logFile dmGR) :direction :output :if-exists :append :if-does-not-exist :create)
            (loop for eventSpec in (dmGraphR-module-eventsParametersToLog dmGR) do
                (if (and (eq (evt-module event) (first eventSpec))
                         (eq (evt-action event) (second eventSpec)))
                    (format outStream "event~T~D~T~5,3f~T~S~T~S~T~S~%"
                        (next-stimulus) (evt-time event) (first eventSpec) (second eventSpec)
                        (let ((value (nth (third eventSpec) (evt-params event))))
                            ; TODO: make name beautification function and arguments
                            (if (get-chunk value) (beautifyNodeName value) value))) )))))

(defun beautifyNodeName (node)
    ; TODO: remove dependency on TLS / TWM
    ; for entity nodes, return EN<index>-<lexicon entry>
    (if (and (< 10 (length (write-to-string node)))
                 (string-equal "entitynode" (subseq (write-to-string node) 0 10)))
        (concatenate 'string
            (subseq (write-to-string node) 0 2) (subseq (write-to-string node) 10)
            "-" (subseq (write-to-string (chunk-slot-value-fct node 'le)) 3))
        ; for verb nodes, return VE<index>-<lexicon entry>
        (if (and (< 8 (length (write-to-string node)))
                 (string-equal "verbnode" (subseq (write-to-string node) 0 8)))
            (concatenate 'string
                (subseq (write-to-string node) 0 2) (subseq (write-to-string node) 8)
                "-" (subseq (write-to-string (chunk-slot-value-fct node 'le)) 3))
            ; for relation nodes, return RE<index>-<lexicon entry>
            (if (and (< 12 (length (write-to-string node)))
                     (string-equal "relationnode" (subseq (write-to-string node) 0 12)))
                (concatenate 'string
                    (subseq (write-to-string node) 0 2) (subseq (write-to-string node) 12)
                    "-" (subseq (write-to-string (chunk-slot-value-fct node 'le)) 3))
                ; for text chunks, return TXT<index>-<string>
                (if (and (< 4 (length (write-to-string node)))
                         (string-equal "text" (subseq (write-to-string node) 0 4)))
                    (concatenate 'string
                        "TXT" (subseq (write-to-string node) 4)
                        "-" (subseq (write-to-string (chunk-slot-value-fct node 'value)) 1
                                (- (length (write-to-string (chunk-slot-value-fct node 'value))) 1)))
                    ; for visual-location chunks, return VLOC<index>@<screen-x>,<screen-y>
                    (if (and (< 15 (length (write-to-string node)))
                             (string-equal "visual-location" (subseq (write-to-string node) 0 15)))
                        (concatenate 'string
                            "VLOC" (subseq (write-to-string node) 15)
                            "@" (write-to-string (chunk-slot-value-fct node 'screen-x))
                            "," (write-to-string (chunk-slot-value-fct node 'screen-y)))
                        node))))))

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
 |
 | dmGraphR module framework
 |
 ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

(defun dmGraphR-params (dmGraphR param)
  (if (consp param)
    #| set values |#
    (case (car param)
        (:dmGraphR-enabled
            (setf (dmGraphR-module-enabled dmGraphR) (cdr param)))
        (:dmGraphR-path
            (setf (dmGraphR-module-path dmGraphR) (cdr param)))
        (:dmGraphR-sampleDuration
            (setf (dmGraphR-module-sampleDuration dmGraphR) (cdr param)))
        (:dmGraphR-autoLog
            (setf (dmGraphR-module-autoLog dmGraphR) (cdr param)))
        (:dmGraphR-logSpecs
            (setf (dmGraphR-module-logSpecs dmGraphR) (cdr param)))
        (:dmGraphR-maxGraphDepth
            (setf (dmGraphR-module-maxGraphDepth dmGraphR) (cdr param)))
        (:dmGraphR-eventsParametersToLog
            (setf (dmGraphR-module-eventsParametersToLog dmGraphR) (cdr param)))
        (:dmGraphR-buffersToGraph
            (setf (dmGraphR-module-buffersToGraph dmGraphR) (cdr param))))
    #| get values |# 
    (case param
        (:dmGraphR-enabled
            (dmGraphR-module-enabled dmGraphR))
        (:dmGraphR-path
            (dmGraphR-module-path dmGraphR))
        (:dmGraphR-sampleDuration
            (dmGraphR-module-sampleDuration dmGraphR))
        (:dmGraphR-autoLog
            (dmGraphR-module-autoLog dmGraphR))
        (:dmGraphR-logSpecs
            (dmGraphR-module-logSpecs dmGraphR))
        (:dmGraphR-maxGraphDepth
            (dmGraphR-module-maxGraphDepth dmGraphR))
        (:dmGraphR-eventsParametersToLog
            (dmGraphR-module-eventsParametersToLog dmGraphR))
        (:dmGraphR-buffersToGraph
            (dmGraphR-module-buffersToGraph dmGraphR)))))

(defun create-dmGraphR-module (model-name)
    (declare (ignore model-name))
    (make-dmGraphR-module))
        
(defun reset-dmGraphR-module (dmGraphR)
    "(Re-)Initialize internal variables"
    (setf (dmGraphR-module-chunksToLog dmGraphR) (list ))
    (setf (dmGraphR-module-vertices dmGraphR) (list ))
    (setf (dmGraphR-module-vertexAttributes dmGraphR) (list ))
    (setf (dmGraphR-module-edges dmGraphR) (list )))

(defun reset2-dmGraphR-module (dmGraphR)
    "secondary module reset, I'm not sure when this is called"
    (declare (ignore dmGraphR)))

(defun dmGraphR-get-logFile(dmGR)
    (dmGraphR-module-logFile dmGR))

(defun dmGraphR-reset-logFile(dmGR)
    (setf (dmGraphR-module-logFile dmGR) (concatenate 'string (dmGraphR-module-path dmGR) "dmGraph-" (write-to-string (get-universal-time)) ".csv" ))
    #|(format t "Writing dmGraphR log to ~S~%" (dmGraphR-module-logFile dmGR))|#)

(defun reset3-dmGraphR-module (dmGR)
    "Continue reset after parameters have been set: upon first reset, set the logFile path and schedule the periodic logging event"
    (if (dmGraphR-module-enabled dmGR)
        (progn
            (if (not (dmGraphR-module-logFile dmGR))
                (dmGraphR-reset-logFile dmGR))
            (dmGraphR-stop dmGR)
            (if (dmGraphR-module-autoLog dmGR)
                (dmGraphR-start dmGR)))))
      
(defun delete-dmGraphR-module (dmGraphR)
    (if (dmGraphR-module-logEvent dmGraphR)
        (format t "Deleting logEvent ...~S~%" (write-to-string (delete-event (dmGraphR-module-logEvent dmGraphR)))))
    (setf (dmGraphR-module-logEvent dmGraphR) nil)
    (if (dmGraphR-module-postEventHook dmGraphR)
        (format t "Deleting postEventHook ...~S~%" (write-to-string (delete-event-hook (dmGraphR-module-postEventHook dmGraphR)))))
    (setf (dmGraphR-module-postEventHook dmGraphR) nil))

#| TODO: create needs to set timestamp and file name; called upon
model creation i.e. should install events ...|#

(define-module-fct 'dmGraphR
    nil #| the module has no buffers |#
    (list
        (define-parameter :dmGraphR-enabled
            :documentation "to enable/disable the module's operation"
            :default-value t
            :valid-test nil #| TODO: add valid test and warning |#
            :warning ""
            :owner t)
        (define-parameter :dmGraphR-path
            :documentation "the directory to create the dmGraphR log files in"
            :default-value "."
            :valid-test (lambda(x) (stringp x))
            :warning "string"
            :owner t)
        (define-parameter :dmGraphR-sampleDuration
            :documentation "how many milliseconds to pause between logging two samples"
            :default-value 10
            :valid-test (lambda (x) (and (numberp x) (> x 0)))
            :warning "positive number"
            :owner t)
        (define-parameter :dmGraphR-autoLog
            :documentation "t if logging should start automatically, nil if (dmGraphR-start) should be invoked to start logging"
            :default-value t
            :valid-test nil #| TODO: add valid test and warning |#
            :warning ""
            :owner t)
        #| TODO: rename into chunk-names and a list of names and not a
           list of single-element lists |#
        (define-parameter :dmGraphR-logSpecs
            :documentation "a list of chunk specs used to determine which chunks to log for graphing"
            :default-value (list )
            :valid-test nil #| TODO: add valid test and warning |#
            :warning ""
            :owner t)
        (define-parameter :dmGraphR-maxGraphDepth
            :documentation "how many links away from a buffer to log and plot"
            :default-value 3
            :valid-test (lambda (x) (and (numberp x) (>= x 0)))
            :warning "non-negative number"
            :owner t)
        (define-parameter :dmGraphR-eventsParametersToLog
            :documentation "a list of lists of <moduleName> <action> <zeroBasedParameterPosition> as retrieved by (evt-module event), (evt-action event), (nth <zeroBasedParameterPosition> (evt-action event))"
            :default-value (list )
            :valid-test nil #| TODO: add valid test and warning |#
            :warning ""
            :owner t)
        (define-parameter :dmGraphR-buffersToGraph
            :documentation "a list of names of buffers to search for chunks to be included in the chunk graph"
            :default-value (list 'goal 'retrieval 'imaginal)
            :valid-test nil #| TODO: add valid test and warning |#
            :warning ""
            :owner t)
        #| TODO: add non-owned parameters for the hooks used in
           activation calculations? |#
        #| TODO: Right now beautifyNodes is used directly and this function is not used at all
        (defvar *timelineTransformations* (list
            (list "activation" 3 'beautifyNodeName)
            (list "event"      5 'beautifyNodeName))) |#
        )
    :version "0.1"
    :documentation "Logs activation data to a file to be plotted in R"    :creation 'create-dmGraphR-module
    :reset (list 'reset-dmGraphR-module 'reset2-dmGraphR-module 'reset3-dmGraphR-module)
    :delete 'delete-dmGraphR-module
    :params 'dmGraphR-params)