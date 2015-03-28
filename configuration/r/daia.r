Sys.setlocale("LC_COLLATE", "C")
# Do not use UTF-8, for the lexicon entries from ACT-R are ISO encoded
# because ACT-R cannot display UTF-8 itself (at least not out of the box)
#Sys.setlocale(category = "LC_ALL", locale="UTF-8")
setwd("C:\\Users\\monochromata\\git\\ecem.git\\MAModel\\runs\\19.03.15\\23.14.40")
require(lattice)
require(RColorBrewer)
require(permute)
require(igraph)
require(colorspace)

dmGraphVShape <- function(g) {
    return(sapply(V(g), function(v) {
        if(is.na(get.vertex.attribute(g, "chunkName", v))) {
            return("none")
        } else {
            return("circle")
        }
    }))
}

dmGraphLabel <- function(g, showActivation=TRUE) {
    return(sapply(V(g), function(v) {
        # print(get.vertex.attribute(g, "chunkName", v))
        if(is.na(get.vertex.attribute(g, "chunkName", v))) {
            return("")
        } else {
            if(showActivation) {
              return(paste(get.vertex.attribute(g, "beautifiedChunkName", v), "\n",
                  get.vertex.attribute(g, "totalActivation", v),
                  get.vertex.attribute(g, "blActivation", v),
                  get.vertex.attribute(g, "spreadingActivation", v)))
            } else {
                return(paste(get.vertex.attribute(g, "beautifiedChunkName", v)))
            }
        }
    }))
}

dmGraphLabelColor <- function(g, colorSpecs, chunkLabelColor) {
    return(sapply(V(g), function(v) {
       if(!is.na(get.vertex.attribute(g, "beautifiedChunkName", v))) {
            if(is.data.frame(colorSpecs)) {
                for(i in 1:nrow(colorSpecs)) {
                    if(0 < length(grep(as.character(colorSpecs$spec[i]), get.vertex.attribute(g, "beautifiedChunkName", v), value=TRUE))) {
                        return(as.character(colorSpecs$col[i]))
                    }
                }
            } 
       }       
       return(chunkLabelColor)
    }))
}

dmColors <- function(act, minActivation, maxActivation) {
    return(sapply(act, function(a) {
        if(is.na(as.double(as.character(a)))) {
            return("gray")
        } else {
            return (rgb(
                max(0, if(a >  maxActivation/2) { 1 } else { 2*a/maxActivation }),
                max(0, if(a <= maxActivation/2) { 1 } else { 2-2*(a/maxActivation) }),
                0))
        }
    }))
}

dmActivationLegend <- function(x, y, lineHeight, iWidth, minActivation, maxActivation, fontSize=0.8) {
    text(x, y, cex=fontSize, adj=c(0,0.5), "Chunk activation")
    ytop=c(
        y-lineHeight,
        y-3*lineHeight,
        y-5*lineHeight,
        y-7*lineHeight,
        y-9*lineHeight)
    distance=maxActivation-minActivation
    act=c(maxActivation,
          (3*distance)/4+minActivation,
          distance/2+minActivation,
          distance/4+minActivation,
          minActivation)
    rect(x, ytop-2*lineHeight, x+lineHeight, ytop, density=NA,
        col=dmColors(act, minActivation, maxActivation))
    # TODO: add labels
    text(x+lineHeight+iWidth, ytop-lineHeight, cex=fontSize, adj=c(0,0.5), round(act, digits=2))
}

dmGraph <- function(graphFile, trial, itemsfile=NULL, outfile=NA, edgeWidth=1, showLinkLabels=TRUE,
	showActivation=TRUE, showItem=TRUE, showBuffers=TRUE, showActivationLegend=TRUE, fontSize=0.8,
	chunkLabelColorSpecs=NA, chunkLabelColor="darkblue") {
    # Read items and trial text
    if(is.null(itemsfile)) {
        trialText = ""
    } else {
    	  items = data.frame(read.csv(itemsfile, sep=";", header=TRUE))
    	  trialText = as.character(items[which(items$no==trial), "item"])
    } 
    
    # Read buffer and graph data
    graphData = readLines(graphFile)
    
    # Buffers
    rawB=grep("^buffer.*$", graphData, value=TRUE)
    allBufferEvents = read.csv(textConnection(
        gsub("#C\\(([0-9\\.]+) ([0-9\\.]+)\\)", "#C(\\1;\\2)", rawB)),
        header=FALSE, sep=" ", col.names=c("eventType", "trial", "time", "buffer", "bufferSpread", "chunkName", "beautifiedChunkName"))
    bufferEvents=allBufferEvents[which(allBufferEvents$trial==trial),]
        
    # Vertices
    rawV=grep("^chunk.*$", graphData, value=TRUE)
    allVertexEvents = read.csv(textConnection(
        # replace whitespace in representation of complex numbers
        gsub("#C\\(([0-9\\.]+) ([0-9\\.]+)\\)", "#C(\\1;\\2)", rawV)),
        header=FALSE, sep=" ", col.names=c("eventType", "trial", "time", "chunkName", "beautifiedChunkName", "totalActivation", "blActivation", "spreadingActivation"))
    vertexEvents=allVertexEvents[which(allVertexEvents$trial==trial),]
    tActDblNa=as.double(as.character(vertexEvents[,"totalActivation"]))
    tActDbl=tActDblNa[which(!is.na(tActDblNa))]
    minActivation=max(0,min(tActDbl))
    maxActivation=max(tActDbl)
        
    # Edges
    rawE=grep("^slot.*$", graphData, value=TRUE)
    allEdgeEvents = read.csv(textConnection(
        # replace whitespace in representation of complex numbers
        gsub("#C\\(([0-9\\.]+) ([0-9\\.]+)\\)", "#C(\\1;\\2)", rawE)),
        header=FALSE, sep=" ", col.names=c("eventType", "trial", "time", "srcVertexIndex", "targetVertexIndex", "srcVertexSlot", "sji"))
    edgeEvents=allEdgeEvents[which(allEdgeEvents$trial==trial),]
        
    # create a stable layout based on the final graph
    set.seed(1)
    finalTime=max(edgeEvents$time)
    g <- graph.empty()
    buffers=bufferEvents[which(bufferEvents$time==finalTime),]
    vertices=vertexEvents[which(vertexEvents$time==finalTime),]
    maxVertices=nrow(vertices)
    g <- add.vertices(g, nrow(vertices), 
        chunkName=as.character(vertices[,"chunkName"]),
        beautifiedChunkName=as.character(vertices[,"beautifiedChunkName"]),
        totalActivation=as.double(as.character(vertices[,"totalActivation"])),
        blActivation=as.double(as.character(vertices[,"blActivation"])),
        spreadingActivation=as.double(as.character(vertices[,"spreadingActivation"])))
    edges=edgeEvents[which(edgeEvents$time==finalTime),]
    g <- add.edges(g, t(edges[,c("srcVertexIndex","targetVertexIndex")]),
        srcVertexSlot=as.character(edges[,"srcVertexSlot"]),
        sji=as.double(as.character(edges[,"sji"])))
    l=layout.fruchterman.reingold(g)
    
    # render the graph
    aspect=0.6
    size=10
    if(!is.na(outfile))
        pdf(outfile, onefile=TRUE, width=size, height=size*aspect)
    for(tPoint in unique(edgeEvents$time)) {
        
        g <- graph.empty()
        
        buffers=bufferEvents[which(bufferEvents$time==tPoint),]
        
        vertices=vertexEvents[which(vertexEvents$time==tPoint),]
        g <- add.vertices(g, nrow(vertices), 
                chunkName=as.character(vertices[,"chunkName"]),
                beautifiedChunkName=as.character(vertices[,"beautifiedChunkName"]),
                totalActivation=as.double(as.character(vertices[,"totalActivation"])),
                blActivation=as.double(as.character(vertices[,"blActivation"])),
                spreadingActivation=as.double(as.character(vertices[,"spreadingActivation"])))
        for(i in 1:(maxVertices-nrow(vertices))) {
            # add as many placebo vertices as required to match the final
            # number of vertices
            g <- g + vertex(chunkName=NA, beautifiedChunkName=NA,
                    totalActivation=NA, blActivation=NA, spreadingActivation=NA)
        }
        
        edges=edgeEvents[which(edgeEvents$time==tPoint),]
        g <- add.edges(g, t(edges[,c("srcVertexIndex","targetVertexIndex")]),
                srcVertexSlot=as.character(edges[,"srcVertexSlot"]),
                sji=as.double(as.character(edges[,"sji"])))
        
        # TODO: edge colors: E(g)[ weight > 0.9 ]$color <- "red"
        
        # Plot graph
        plot(g,
            layout=l,
            asp=aspect,
            vertex.label=dmGraphLabel(g, showActivation),
            vertex.label.family="sans",
            vertex.label.font=2,
            vertex.label.color=dmGraphLabelColor(g, chunkLabelColorSpecs, chunkLabelColor),
            vertex.label.cex=fontSize,
            vertex.size=6*fontSize,
            vertex.shape=dmGraphVShape(g),
            vertex.color=dmColors(V(g)$totalActivation, minActivation, maxActivation),
            edge.color=dmColors(E(g)$sji, minActivation, maxActivation),
            edge.arrow.size=.75,
            edge.width=edgeWidth,
            edge.label=if(showLinkLabels) { paste(E(g)$srcVertexSlot, E(g)$sji) } else { "" },
            edge.label.color="black",
            edge.label.family="sans",
            edge.label.cex=fontSize-0.1)
            
        lineHeight=strheight("I", cex=fontSize*1.2)
        iWidth=strwidth("I", cex=fontSize*1.2)
        offset=0          
            
        # Plot trial text
        if(showItem) {
            text(-1.6, 1.5, cex=fontSize, adj=c(0,0.5), trialText)
            offset=offset+2
        }
            
        # Plot time
        text(-1.6, 1.5-offset*lineHeight, cex=fontSize, adj=c(0,0.5), paste(collapse="", tPoint, "s"))
        offset=offset+2
        
        # Plot buffers
        if(showBuffers) {
            text(-1.6, 1.5-offset*lineHeight, cex=fontSize, adj=c(0,0.5), "Buffer activation and contents")
            offset=offset+1
            for (i in 1:nrow(buffers)) {
                text(-1.6, 1.5-(offset*lineHeight), cex=fontSize, adj=c(0,0.5),
                    paste(collapse=": ", buffers[i,"bufferSpread"], buffers[i,"buffer"], buffers[i,"beautifiedChunkName"]))
                offset=offset+1
            }
            offset=offset+1
        }
        
        # Plot activation legend
        if(showActivationLegend)
            dmActivationLegend(-1.6, 1.5-(offset*lineHeight), lineHeight, iWidth, minActivation, maxActivation, fontSize=fontSize)
        
        if(is.na(outfile) && interactive())
            Sys.sleep(1)
    }
    if(!is.na(outfile))
        dev.off()
}

# TODO: the manual should highlight that LISP/ACT-R is not
# case-sensitive, but R is, i.e. DECLARATIVE, IMAGINAL, ... have to be
# used; column names in R are (currently) transformed from names in
# ACT-R, too (each dash is transformed to a dot)

# TODO: Show two experiments in one row with event data

# TODO: Show two experiments in one row without event data
plotActivation <- function(timelineFile, trial, itemsfile=NULL, outfile=NA, showLegend=TRUE, showEvents=TRUE,
    legendLeftX = -0.35, displayThreshold = 0, xlim=NULL, ylim=NULL, chunksToDisplay=NULL, chunksToIgnore=NULL,
    eventsToDisplay=NULL, horizontalLine=NULL, pdfWidth=12, pdfHeight=5, correctLISPComplexNumbers=TRUE) {
    if(!is.na(outfile))
        pdf(outfile, onefile=TRUE, width=as.integer(pdfWidth), height=as.integer(pdfHeight))
    par(oma=c(0,0,0,0)+0.1, mar=c(2.5,2.5,2.5,1)+0.1, mgp=c(1.5,0.5,0))
    plotActivation0(timelineFile, trial, itemsfile=itemsfile, showLegend, showEvents,
	legendLeftX, displayThreshold, xlim, ylim, chunksToDisplay, chunksToIgnore,
	eventsToDisplay, horizontalLine, correctLISPComplexNumbers)
    if(!is.na(outfile))
        dev.off()
}

plotActivation0 <- function(timelineFile, trial, itemsfile="items.csv", showLegend=TRUE, showEvents=TRUE,
    legendLeftX = NIL, displayThreshold = 0, xlim=NULL, ylim=NULL, chunksToDisplay=NULL, chunksToIgnore=NULL,
    eventsToDisplay=NULL, horizontalLine=NULL, correctLISPComplexNumbers=TRUE) {
    
    if(is.null(itemsfile)) {
        trialText = ""
    } else {
        items = data.frame(read.csv(itemsfile, sep=";", header=TRUE))
        trialText = as.character(items[which(items$no==trial), "item"])
    }    

    # Read log
    graphData = readLines(timelineFile)

    # Chunks
    rawV=grep("^chunk.*$", graphData, value=TRUE)

    if(correctLISPComplexNumbers) {
	  # replace whitespace in representation of complex numbers
        data = gsub("#C\\(([0-9\\.]+) ([0-9\\.]+)\\)", "#C(\\1;\\2)", rawV)
    } else {
        data = rawV
    }

    allActivation = read.csv(textConnection(data),
        header=FALSE, sep=" ", col.names=c("eventType", "trial", "time", "chunkName", "beautifiedChunkName", "totalActivation", "blActivation", "spreadingActivation"))
    activation = allActivation[which(allActivation$trial==trial),]
    activation[,"chunkName"]=chunkName=as.character(activation[,"chunkName"])
    activation[,"beautifiedChunkName"]=as.character(activation[,"beautifiedChunkName"])
    activation[,"totalActivation"]=as.double(as.character(activation[,"totalActivation"]))
    if(length(which(is.na(activation$totalActivation))) > 0) {
        activation[which(is.na(activation$totalActivation)),"totalActivation"] = 0
    }
    activation[,"blActivation"]=as.double(as.character(activation[,"blActivation"]))
    if(length(which(is.na(activation$blActivation))) > 0) {
        activation[which(is.na(activation$blActivation)),"blActivation"] = 0
    }
    activation[,"spreadingActivation"]=as.double(as.character(activation[,"spreadingActivation"]))
    if(length(which(is.na(activation$spreadingActivation))) > 0) {
        activation[which(is.na(activation$spreadingActivation)),"spreadingActivation"] = 0
    }
    
    # Events
    rawE=grep("^event.*$", graphData, value=TRUE)
    allEvents = read.csv(textConnection(
        # replace whitespace in representation of complex numbers
        gsub("#C\\(([0-9\\.]+) ([0-9\\.]+)\\)", "#C(\\1;\\2)", rawE)),
        header=FALSE, sep=" ", col.names=c("eventType", "trial", "time", "module", "action", "parameter"))
    events = allEvents[which(allEvents$trial==trial),]
    
    if(is.null(legendLeftX)) {
        legendLeftX=-0.4
    }
    if(is.null(xlim)) {
        # Add 10+% whitespace on the left for the legend of chunks
        # TODO: compute the space from the width of the labels ...
        minAct = min(activation$time)
        maxAct = max(activation$time)
        xlim=c(minAct-((maxAct-minAct)*0.1)+legendLeftX,maxAct)
        legendLeftX=minAct-((maxAct-minAct)*0.1)+legendLeftX
    }
    if(is.null(ylim)) {
        # Add 25 % whitespace at the top for the events
        ylim=c(min(activation$totalActivation),max(activation$totalActivation)*1.25)
    }
    plot(0,0,col="white",ylab="Activation", xlab="Model time (sec)", lab=c(40,5,7), xlim=xlim, ylim=ylim)
    # mgp=c(2,1,0), oma=c(0,0,0,0)
    title(main=trialText,cex.main=0.9)
    
    # Configure chunks to be displayed
    if(is.null(chunksToDisplay)) {
        originalChunkNames=unique(activation$beautifiedChunkName)
        chunkNames=c()
        for(chunkName in originalChunkNames) {
            if(max(activation[which(activation$beautifiedChunkName==chunkName), "totalActivation"]) > displayThreshold)
                chunkNames = append(chunkNames, chunkName)
        }
    } else {
        chunkNames=chunksToDisplay
    }
    
    if(!is.null(chunksToIgnore)) {
        chunkNames=grep(chunksToIgnore, chunkNames, value=TRUE, invert=TRUE)
    }
    
    # TODO: Make events to display configurable
    #if(is.null(eventsToDisplay)) {
        events0=events
    #} else {
        # TODO: Implement something like
        # events0=events[which(events$value in eventsToDisplay)),]
    #}
    
    numberOfChunks=length(chunkNames)
    # TODO: divide palette in half and merge the two halves one by one
    #colours=rainbow(numberOfChunks,s=0.7,v=0.7)[shuffle(numberOfChunks)]
    colours = c("#B33636FF", "#36B3B3FF", "#B37436FF", "#3674B3FF",
                "#B3B336FF", "#3636B3FF", "#74B336FF", "#7436B3FF",
                "#36B336FF", "#B336B3FF", "#36B374FF", "#B33674FF")
    colours = append(colours, rep("black", each=max(0,numberOfChunks-length(colours))))
    # TODO: maybe allow more than 12 chunks to be displayed (number of
    # colours needs to be increased, then)?
    i=1
    lineHeight=strheight("I")
    #legendTopY=max(max(activation$totalActivation), max(ylim))-(lineHeight*0.75)
    for(chunkName in rev(chunkNames)) {
        lines(totalActivation ~ time, data=activation[which(activation$beautifiedChunkName==chunkName),], col=rev(colours)[i])
        if(showLegend) {
            text(legendLeftX, 0+((i-1)*lineHeight), chunkName, adj=c(0,0), col=rev(colours)[i], pos=4, cex=0.8)
        }
        i<-i+1
    }
    if(showEvents) {
        eventsY=max(max(activation$totalActivation), max(ylim))
        # TODO: make the groups of events and their colours configurable
        declarativeEvents=events0[which(events0$module=="DECLARATIVE"),]
        imaginalEvents=events0[which(events0$module=="TWM1"),]
        abline(v=declarativeEvents$time,col="gray",lty="dotted")
        text(declarativeEvents$time,eventsY,declarativeEvents$parameter,col="gray",bg="white",adj=1,srt=90,cex=0.6)
        abline(v=imaginalEvents$time,col="gray",lty="dotted")
        text(imaginalEvents$time,eventsY,imaginalEvents$parameter,col="blue",bg="white",adj=1,srt=90,cex=0.6)
    }
    if(!is.null(horizontalLine)) {
        abline(h=horizontalLine,col="gray")
    }
}

# Show two experiments in two rows with event data
compare2Items <- function(timelineFile, item0, item1, itemsfile="items.csv", outfile=NA, showLegend=TRUE, showEvents=TRUE, 
    legendLeftX = -0.35, displayThreshold = 0, xlim=NULL, ylim=NULL, chunksToDisplay=NULL, chunksToIgnore=NULL, eventsToDisplay=NULL, horizontalLine=NULL, pdfWidth=40, pdfHeight=5) {
    if(!is.na(outfile))
        pdf(outfile, onefile=TRUE, width=as.integer(pdfWidth), height=as.integer(pdfHeight))
    par(mfrow=c(2,1),oma=c(1,3,0,0)+0.1, mar=c(2,0,2.5,1)+0.1, mgp=c(2,1,0))
    plotActivation0(timelineFile, item0, itemsfile=itemsfile, showLegend=showLegend, showEvents=showEvents, legendLeftX=legendLeftX, displayThreshold=displayThreshold, xlim=xlim, ylim=ylim, chunksToDisplay=chunksToDisplay, chunksToIgnore=chunksToIgnore, eventsToDisplay=eventsToDisplay, horizontalLine=horizontalLine)
    plotActivation0(timelineFile, item1, itemsfile=itemsfile, showLegend=showLegend, showEvents=showEvents, legendLeftX=legendLeftX, displayThreshold=displayThreshold, xlim=xlim, ylim=ylim, chunksToDisplay=chunksToDisplay, chunksToIgnore=chunksToIgnore, eventsToDisplay=eventsToDisplay, horizontalLine=horizontalLine)
    par(mfrow=c(1,1))
    if(!is.na(outfile))
        dev.off()
}

compare2ItemVersions <- function(timelineFile0, timelineFile1, item, 
    itemsfile="items.csv", outfile=NA, showLegend=TRUE, showEvents=TRUE, legendLeftX = -0.35,
    displayThreshold = 0, xlim=NULL, ylim=NULL,
    chunksToDisplay=NULL, chunksToIgnore=NULL, eventsToDisplay=NULL, horizontalLine=NULL, pdfWidth=10, pdfHeight=5) {
    if(!is.na(outfile))
        pdf(outfile, onefile=TRUE, width=as.integer(pdfWidth), height=as.integer(pdfHeight))
    par(mfrow=c(2,1),oma=c(1,3,0,0)+0.1, mar=c(2,0,2.5,1)+0.1, mgp=c(2,1,0))
    plotActivation0(timelineFile0, item, itemsfile=itemsfile, showLegend=showLegend, showEvents=showEvents, legendLeftX=legendLeftX, displayThreshold=displayThreshold, xlim=xlim, ylim=ylim, chunksToDisplay=chunksToDisplay, chunksToIgnore=chunksToIgnore, eventsToDisplay=eventsToDisplay, horizontalLine=horizontalLine)
    plotActivation0(timelineFile1, item, itemsfile=itemsfile, showLegend=showLegend, showEvents=showEvents, legendLeftX=legendLeftX, displayThreshold=displayThreshold, xlim=xlim, ylim=ylim, chunksToDisplay=chunksToDisplay, chunksToIgnore=chunksToIgnore, eventsToDisplay=eventsToDisplay, horizontalLine=horizontalLine)
    par(mfrow=c(1,1))
    if(!is.na(outfile))
        dev.off()
}
