# Hitch

A Clojure(script) library is a mini-language that decouples place from computation.

## Coordinates
[com.breezeehr/hitch "0.1.6-SNAPSHOT"]

# What is it?
It consists of a set of tools to derive data, manage data dependencies and setup state 
machines for a frame of reference. These tools should be used together to give you the 
developer promises about how the system will function. One of the most common promises 
is that all derived data for a frame of reference is consistent, but there are many more 
uses around batching and local transactions. 

##Selector
A selector is a precise name for a piece of data along with the directions to get the data
if it is not available in the graph. Selectors are able to dereference other selectors. 
This allows you to derive data on anything you know the name of its selector selectors.

## Graph
Then main interface that hitch exposes is called the graph. You can think of it as a atom
or a consistent peephole to the world. You can do two things with a graph dereference 
selectors and apply commands. Then the graph will give you promises about your responses.
When a selector is dereferenced there are the graph there is a matrix of options as to how.
 * evaluated here or on a remote server
 * cached or not
 * subscribed or not
 * inlined or not
 * if the selector is a selector interface. It is late bound which implementation to use
 
There are even more choices of adapters to the host language. Most are exposed through the 
graph api and others a delegated, but they are orthogonal to the definition of the selector.

###Service Selectors
Selectors are not pure functions as they apply the effect of dereferencing other selectors,
but service Selectors are full blown state Machines. They tend to most of the details of 
applying commands and keeping consistency promises. 
(should we call them something else?)
   

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
