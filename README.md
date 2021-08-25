# DynamicDataObjects
This Delphi code library lets you model structured data and serialze to/from a variety of structured data serializations such as:
CBOR, JSON, MessagePack, ION, UBJSON, BSON, Smile, DataObj, CSV, ICS, BinaryJData, etc.  Most of these serializations are complete such as JSON, CBOR, DataObj, but some of the more obscure ones are coded but not yet 100% complete and certainly only minimally tested.

This project is primarily coded and tested with Delphi 10.4.  It has not been tested with other Delphi versions or other pascal compilers, but likely it will work just fine with some minor tweaks. 

A significant amount of attention has been placed on serialization performance, especially with CBOR, DataObj and JSON.  With JSON, it's considerably faster than the Embarcadero's multiple JSON implementations. 

To use this code in its most basic form, you only need to include DataObjects2.pas in your project.  Then, to choose one or more serializers to also be included, simply include those units as well.  
