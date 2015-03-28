; Daia Version 4

(load "experiment-preparation.lisp")

(clear-all)

(load "exp-ia-german.lisp")               
(load "experiment-preparation2.lisp")

(define-model daia

(load "experiment-preparation3.lisp")

#| dmGraphR parameters |#
(sgp
    :dmGraphR-path "/Users/Basti/Documents/studium/TUBerlin/SS2013/Extra/ACT-R_SpringSchool/fun/stripped/logs/"
    :dmGraphR-sampleDuration 50
    :dmGraphR-logSpecs ( (isa TWMNode) #| (isa Goal) (isa activateTWMNode) (isa reactivateTWMNode)|#)
    :dmGraphR-maxGraphDepth 3
    :dmGraphR-eventsParametersToLog ( ( declarative SET-BUFFER-CHUNK 1)
                                      ( imaginal    SET-BUFFER-CHUNK 1))
    :dmGraphR-buffersToGraph ( goal retrieval visual imaginal)
    )
    
(schedule-periodic-event 50 'dmGraphR-log-graph
                :time-in-ms t
                :module 'dmGraphR
                :destination 'dmGraphR
                :maintenance t)

(load "experiment-preparation4.lisp")
(load "lexicon-de.lisp")
(load "experiment-preparation5.lisp")

)