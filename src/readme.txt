DataObjects library.
Written by Sean Solberg.

This directory of delphi (Object Pascal) source code is for all the core dataObjects source files.
It includes the core objects, some utility helper source files, and the set of source files that provide
encoding/decoding to each of the supported streaming formats such as JSON, BSON, CBOR, etc. 

None of the source files in this directory should touch anything user interface-wise. 

The only required file that must be included into a project is DataObjects2.pas which provides the core
objects and the abstract concept of serialization (Encoding and Decoding).  If you want to serialize to
one of the supported formats, include the source code for that format's serialization such as DataObjects2JSON.pas.

Note that the dataObjects library currently does not support the Geometry (point, polyline, polygon) data type yet.
However, you will see the Well Known Binary source code in there that is the starting point for this functionality
although this code isn't really used yet. 


