unit DataObjects2;

{********************************************************************************}
{                                                                                }
{                         Dynamic Data Objects Library                           }
{                                                                                }
{                                                                                }
{ MIT License                                                                    }
{                                                                                }
{ Copyright (c) 2022 Sean Solberg                                                }
{                                                                                }
{ Permission is hereby granted, free of charge, to any person obtaining a copy   }
{ of this software and associated documentation files (the "Software"), to deal  }
{ in the Software without restriction, including without limitation the rights   }
{ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      }
{ copies of the Software, and to permit persons to whom the Software is          }
{ furnished to do so, subject to the following conditions:                       }
{                                                                                }
{ The above copyright notice and this permission notice shall be included in all }
{ copies or substantial portions of the Software.                                }
{                                                                                }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  }
{ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  }
{ SOFTWARE.                                                                      }
{                                                                                }
{********************************************************************************}

{ This unit defines the core classes for using DataObjects2.

  DataObjects2 was written by Sean Solberg starting 2/28/2018 as a somewhat functional replace for the old original DataObjects that I used to use at a previous company.
  Since this was written from scratch, I had a chance to have a completely different internal design and add additional capabilities as well as not have some of the bloat.

  Design principles:
    No geometry - maybe someday I will implement WellKnownBinary geometry but for now there is none as I will probably never have the need.  Maybe model as binary with geometry flags.
    Do not want any extra stuff hacked in there to support COM or the Prism compiler for DotNet, etc.  If someone else needs that, please be my guest to edit the code to add that.
    I do want to support firemonkey and the VCL so that means care in variant data storage.  Need to keep consistent data sizing, etc.
    I do want to be mindful of being able to implement this stuff enrirely in any another language.  I'm sure I won't get this perfect, but I'll try to be mindful.
    I want the searching of slots by name to be faster.  This is a trade-off as some techniques are good for low slot counts (loop) and some are good for high slot counts (TDictionary)
    I want to have an array type that also lets me have sparse slots (each slot has a numerical index like the frame does with a string, but there can be gaps between indexes)
    I want this core file to only support the core data modeling( so I can use it very compactly), and any extra serialization techniques must be implemented in other units to perform the streaming.  IE JSON, CBOR, etc.)
    I want the default built-in serialization to be a bit more compact.  (single byte for dataType and flags, VarInt Streaming if I can make it fast, etc.)
    I want to mostly support (and serialize) all the same data types that are supported by BSON (I say mostly cause some are deprecated or not really that useful)
    I want to support the concept of having "attributes" on a value.  "attributes" will be available for each data type.
      Note that serializing data that has attributes out to a medium that doesn't support attributes natively (such as BSON) will likely loose the content of those attributes cause there's not place to put them.
      The concept of attributes is something borrowed from XML and I think there's some value in supporting that when getting data from an XML source even if BSON, JSON, etc. doesn't support it.
    I will not use BSON serialization directly because there are certain aspects of BSON serialization that are geared for a different purpose.
       Serializing a BSON document starts out with a length in bytes of the document.  that means you have to serialize all document contents into memory in order
       to know the actual length or you need to calculate the length in order to start serializing.  That's a performance hastle when you are in a situation where the purpose
       of the serialization is so you can send data from pointA to PointB and the receiver is likely going to be interpreting and working with all the data it received.
       BSON's technique is to put a "length" at the beginning of a document so that you can "skip" over the current piece of data without having to fully decode it so you can look into the next piece of data.
       I'm thinking that the reason they did this is so that they can query documents from storage without necessarily having to decode a full document to do comparisons in a "table scan".
       Once they've excluded a document in what they've scanned so far, they can then seek to the end of the document to start checking the next document.
       Since we are designing serialization for the purpose of sharing data from point A to Point B and the reader most likely needs to use all content coming from a stream,
       this concept of putting a "length" at the beginning of a block of data doesn't help us and it causes a hastle because the writer needs to figure it out.

     Strings:  All strings transmitted on the wire will be UTF-8 encoded in the stream because we want them serialized compactly. In Memory, they are full unicodeStrings
     There are lots of "\x00" null terminators in strings in BSON.   I'm not going to serialize that way.  I'm going to serialize a length prior to the string content.
     Note that we are limiting the length of a slotname to be 255 characters so that the length defined for the slotname only takes one byte to define it and we don't need to do VarInt serializing.
     NOTE:  UTF-8 can have some multi-byte characters so just cause the character limit is 255 doesn't mean that the byte limit for a slotname is 255.  It could be more.

     We will serialize one byte for the dataType, not 4 bytes like the old dataObjects did.

     BSON makes an array be the same as a frame except that it uses a string representation of the index for each item.  That is absolutely JANK and I refuse to do that.
     Real array's don't need a "slotname" identifier as their identifier is basically their index into the array.  However,  I have found the need to have
     something like a frame but with integers as the "slotname".  Basically, an array with some of the items in the array missing and taking an item out of the
     array doesn't shift latter items down an index.  This is the SparseArray.

     It's tempting to separate WireTypes from DataTypes (see google protocol buffers for what this means) to keep from using up the possible dataTypes ( I have a 5 bit limit) (IE: booleans, the Object type's dataType isn't ever serialized, etc.)
     However, since very little can be saved, I figured it's not worth the effort and it's better to have consistency of constant values for the code implementation and for transport in the byte stream.
     This means there will be DataType codes (cDataTypeObject) that never ends up in the stream of bytes as it's really streamed as a frame with a flag.

     It's also tempting to have some sort of version bits so we can modify the streaming in the future.   However, the probability for lots of change is very slim and really, if there's a new streaming byte order
     for an existing dataType that's really important enough to warrant it, we can just issue a new data type code for it. We just need to be judicious.
     I'm not putting anything in for version-to-version compatibility.  IE) if you have a new version with a new dataType someday, the streaming will not be understood
     by old code anyway.

     Note about Data Conversions.  When calling a getAsXXXXXXXX function, that function will return a value under four distinct situations:
       1.  The dataType of the TDataObj is the same as what is being asked for and so it can be returned as is.
       2.  The dataType of the TDataObj is different than what is being asked for, but the value being asked for can easily be directly converted to, so the TDataObj contents are not changed at all
           and the appropriate conversion is done and the value is returned.  IE).  if the value is an Int32 and you call getAsString, then the string representation of
           this value is returned without changing the dataType of the data object over to a string.
       3.  The dataType of the TDataObj is different and the existing value can be converted to the data type asked for but in order to return the type of data being asked for,
           the TDataObj's contents must be converted to the new dataType.  IE).  If the value is a string and you call getStringList, then the existing string value in the
           TDataObj must be converted to a TStringList and then that new TStringList is returned.
       4.  The dataType of the TDataObj is different and the contents can not be converted to the new dataType being asked for.   In this case, the dataType of the TDataObj
           is changed to the new dataType being asked for and all contents that were in this dataObj are lost.  IE) if a TDataObj has binary data in it and you call getAsFrame,
           then the dataObj is turned into an empty TDataFrame.   So,  play nice and check the datatype of a dataObject before calling a potentially destructive property.
  }


  {Main Objects:

    TDataObj - This is the main object a user can instantiate to use and this one is the only one that should be instantiated by outside code.
               The other objects listed below are data container objects for working with specific types of data owned by TDataObj.

    TDataFrame - contains a collection of child objects indexed by a slotname.  Similar to a document in BSON.

    TDataArray - contains a collection of child objects that are in a collection ordered by an indexed array [0..n]

    TDataSparseArray - contians a collection of child objects that are in a collection indexed by a positive integer value.

    TDataStringList - contains a collection of strings.  Note:  this is not quite the same thing as a string with carriage return/line feeds in it.  However, they are very, very similar.
                      It's basically a TStringList;

    TDataBinary - contains binary data. By default, the implementation holds the data in TMemoryStream.  However, there may be provisions to have a reference to any other TStream descendant as well to avoid a copy of data in some cases.
  }

  { Planned Serialization mechanisms
    DataObj - binary
    CBOR - binary
    JSON - text
    DDO - First will be without geometry, then later add geometry.

    BSON - binary
    UBJSON - binary
    Binary JData (derived from UBJSON)
    Smile - binary
    Messagepack
    ION - binary and text (json superset)

    Planned Internal Improvements
    Finish all the details around the sparse array.
    Internal support for the WKB Geometry data type so that DDO can be fully serialized.
    Internal support for the Half Float (Float16)
    Internal support for the Extended Float
    Internal support for the full unsigned set of integers (UInt16, UInt32, Uint64) and the signed byte.
    Either get the concept of "Attributes" working correctly or remove it entirely.
    Support JSON5 serialization
    Support YAML serialization
    Support the ability for TDataFrame to have an option for case-sensitive fieldnames.  This is needed for more correct support of JSON, although pascal object properties are case insensitive, so....
  }


interface

uses SysUtils, DateUtils, Generics.collections, Classes, VarInt, StreamCache, Rtti, typInfo, System.RTLConsts, System.NetEncoding
{$ifdef MSWINDOWS}
   ,windows
{$endif};

// If you enable cMakeMoreCompatibleWithOldDataObjects then it makes this code more compatible with the old dataObjects library I used to use.
//{$Define cMakeMoreCompatibleWithOldDataObjects}



type
  TDataObjParameterPurpose = (cppDecoding, cppEncoding);
  TDataObjParameterPurposes = set of TDataObjParameterPurpose;
  TOnHandleExceptionProc = procedure(Sender: TObject; aException: Exception);
  TMemberVisibilities = set of TMemberVisibility;      // used for RTTI assigning
  TCaseChangeOptions = (cCaseChangeNone, cCaseChangeUpper, cCaseChangeLower);

  TDataObjAssignContext = class
  private
    fSerializedObjects: TList;    // reference list of objects that have already been serialized.  This is to prevent an infinite object ref-to-object circular serialization.  EG)  Object that has a Parent property that refers to the parent object.
    fOnHandleException: TOnHandleExceptionProc;
  public
    MemberVisibilities: TMemberVisibilities;
    DoNotSerializeDefaultValues: boolean;   // if set to true, then we won't serialize out to a frame those properties that contain a value that is the default value.  EG) an integer that has the value zero.
    SerializeEnumerationsAsIntegers: boolean; // if set to true, then we serialize enumeration values out as a number.  If the enumeration has a small set of values, it will be a byte.  If it has a lot, it will be an integer.
                                              // by default, the enumeration value is serialized as a symbol text.
    IncludeSerializingClassName: boolean;     // by default this is true.  Means that we will add "_Class" field to the dataObject we are serializing to.
    constructor Create;
    destructor Destroy; override;
    function IsAlreadySerialized(aObject: TObject): boolean;
    procedure AddObject(aObject: TObject);
    procedure ReportException(aException: Exception);    // when assignment is happening, if there is an exception, it can be reported to here.  Maybe the caller will want to handle it.

    property OnHandleException: TOnHandleExceptionProc read fOnHandleException write fOnHandleException;
  end;


const
  cPublishedMembers = [TMemberVisibility.mvPublished];
  cPublicMembers = [TMemberVisibility.mvPublished, TMemberVisibility.mvPublic];

 // data Type used for defining the type of data held by a TDataObj
 // The bottom 5 bits in this byte are used to determine the type of data held by a TDataObj as listed below
 // The next two MSB bits in this byte are used as a subclass mechanism for each of the data types and those 4 possible code values are specific to the data type (if used).
 // The top MSB bit is not used and is reserved for either expanding the subClass or expanding the root dataType code.
 // For a string for example.  Subclass code = 0: String.  This is a normal text string that represents data.
 //                                     code = 1: Symbol.  A Symbol is similar to a string, but is considered a unique value in and of itself such as an identifier.
 //                                                        for example, we can have a string that is "visible" which is the literal text of those characters, or we can have a symbol "visible", which really is an identifer that really represents something is TRUE to "BE" visible.
 //                                                        when serializing JSON, technically symbols are not valid in JSON, but we may read Invalid JSON values as symbols
 //                                                        for example:  {"test": "~template1~"}  is a string value
 //                                                                      {"test": ~template1~} is a symbol value, which is technically not JSON compliant, but might be used when a file that builds JSON from a template is used.
 //                                     code = 2,3: future reserved.
 // Note that the SubCodes are intended to give variations to the meaning of the data, not to be used as serialization variation flags.
 cSubCodeGeneric = 0;
 cSubCodeSymbol = 1;

 cFalseStr = 'False';
 cTrueStr = 'True';

 // Future ideas...
 // For an array for example:  subclass code = 0: Generic element array where each element in the array is a TDataObj
 //                                     code = 1: Specific type array where each element in the array is the same data type.  So, the next byte
 //                                               serialized is the dataType and every object serialized then must have the exact same dataType (including flags)
 //                                               This means that as the subsequent array elements read themselves, they don't read their dataType byte individually
 //                                               from the stream because the container tells them what it's going to be.  This saves space and is a common pattern in real data situations.
 //                                               it saves about 25% space on an array of integers, 12.5% space on an array of doubles, 50% space on an array of bytes.
 // for the int64 type:        subclass code = 3: Came in from BSON or a SQL Database as a TimeStamp (64bit timestamp) What the bits mean may depend on where it came from:  MongoDB, SQLServer, etc.
 // for the float types:       subclass code = 0: generic
 //                                     code = 1: currency

type
  _TwoChar = packed record
    case Integer of
      0:(ch: array[1..2] of Char);
      1:(u32: Cardinal);
  end;

const cTwoHexLookup: packed array[0..255] of _TwoChar =
  (
   (ch:'00'),(ch:'01'),(ch:'02'),(ch:'03'),(ch:'04'),(ch:'05'),(ch:'06'),(ch:'07'),(ch:'08'),(ch:'09'),(ch:'0a'),(ch:'0b'),(ch:'0c'),(ch:'0d'),(ch:'0e'),(ch:'0f'),
   (ch:'10'),(ch:'11'),(ch:'12'),(ch:'13'),(ch:'14'),(ch:'15'),(ch:'16'),(ch:'17'),(ch:'18'),(ch:'19'),(ch:'1a'),(ch:'1b'),(ch:'1c'),(ch:'1d'),(ch:'1e'),(ch:'1f'),
   (ch:'20'),(ch:'21'),(ch:'22'),(ch:'23'),(ch:'24'),(ch:'25'),(ch:'26'),(ch:'27'),(ch:'28'),(ch:'29'),(ch:'2a'),(ch:'2b'),(ch:'2c'),(ch:'2d'),(ch:'2e'),(ch:'2f'),
   (ch:'30'),(ch:'31'),(ch:'32'),(ch:'33'),(ch:'34'),(ch:'35'),(ch:'36'),(ch:'37'),(ch:'38'),(ch:'39'),(ch:'3a'),(ch:'3b'),(ch:'3c'),(ch:'3d'),(ch:'3e'),(ch:'3f'),
   (ch:'40'),(ch:'41'),(ch:'42'),(ch:'43'),(ch:'44'),(ch:'45'),(ch:'46'),(ch:'47'),(ch:'48'),(ch:'49'),(ch:'4a'),(ch:'4b'),(ch:'4c'),(ch:'4d'),(ch:'4e'),(ch:'4f'),
   (ch:'50'),(ch:'51'),(ch:'52'),(ch:'53'),(ch:'54'),(ch:'55'),(ch:'56'),(ch:'57'),(ch:'58'),(ch:'59'),(ch:'5a'),(ch:'5b'),(ch:'5c'),(ch:'5d'),(ch:'5e'),(ch:'5f'),
   (ch:'60'),(ch:'61'),(ch:'62'),(ch:'63'),(ch:'64'),(ch:'65'),(ch:'66'),(ch:'67'),(ch:'68'),(ch:'69'),(ch:'6a'),(ch:'6b'),(ch:'6c'),(ch:'6d'),(ch:'6e'),(ch:'6f'),
   (ch:'70'),(ch:'71'),(ch:'72'),(ch:'73'),(ch:'74'),(ch:'75'),(ch:'76'),(ch:'77'),(ch:'78'),(ch:'79'),(ch:'7a'),(ch:'7b'),(ch:'7c'),(ch:'7d'),(ch:'7e'),(ch:'7f'),
   (ch:'80'),(ch:'81'),(ch:'82'),(ch:'83'),(ch:'84'),(ch:'85'),(ch:'86'),(ch:'87'),(ch:'88'),(ch:'89'),(ch:'8a'),(ch:'8b'),(ch:'8c'),(ch:'8d'),(ch:'8e'),(ch:'8f'),
   (ch:'90'),(ch:'91'),(ch:'92'),(ch:'93'),(ch:'94'),(ch:'95'),(ch:'96'),(ch:'97'),(ch:'98'),(ch:'99'),(ch:'9a'),(ch:'9b'),(ch:'9c'),(ch:'9d'),(ch:'9e'),(ch:'9f'),
   (ch:'a0'),(ch:'a1'),(ch:'a2'),(ch:'a3'),(ch:'a4'),(ch:'a5'),(ch:'a6'),(ch:'a7'),(ch:'a8'),(ch:'a9'),(ch:'aa'),(ch:'ab'),(ch:'ac'),(ch:'ad'),(ch:'ae'),(ch:'af'),
   (ch:'b0'),(ch:'b1'),(ch:'b2'),(ch:'b3'),(ch:'b4'),(ch:'b5'),(ch:'b6'),(ch:'b7'),(ch:'b8'),(ch:'b9'),(ch:'ba'),(ch:'bb'),(ch:'bc'),(ch:'bd'),(ch:'be'),(ch:'bf'),
   (ch:'c0'),(ch:'c1'),(ch:'c2'),(ch:'c3'),(ch:'c4'),(ch:'c5'),(ch:'c6'),(ch:'c7'),(ch:'c8'),(ch:'c9'),(ch:'ca'),(ch:'cb'),(ch:'cc'),(ch:'cd'),(ch:'ce'),(ch:'cf'),
   (ch:'d0'),(ch:'d1'),(ch:'d2'),(ch:'d3'),(ch:'d4'),(ch:'d5'),(ch:'d6'),(ch:'d7'),(ch:'d8'),(ch:'d9'),(ch:'da'),(ch:'db'),(ch:'dc'),(ch:'dd'),(ch:'de'),(ch:'df'),
   (ch:'e0'),(ch:'e1'),(ch:'e2'),(ch:'e3'),(ch:'e4'),(ch:'e5'),(ch:'e6'),(ch:'e7'),(ch:'e8'),(ch:'e9'),(ch:'ea'),(ch:'eb'),(ch:'ec'),(ch:'ed'),(ch:'ee'),(ch:'ef'),
   (ch:'f0'),(ch:'f1'),(ch:'f2'),(ch:'f3'),(ch:'f4'),(ch:'f5'),(ch:'f6'),(ch:'f7'),(ch:'f8'),(ch:'f9'),(ch:'fa'),(ch:'fb'),(ch:'fc'),(ch:'fd'),(ch:'fe'),(ch:'ff')
  );


type

  EDataObj = class(Exception);

  // Note that the code needs to fit in a 5 bit number so the limit is 31.
  TDataTypeCode = (
    cDataTypeNull = 0,
    cDataTypeBoolean = 1,
    cDataTypeByte = 2,        // One byte number
    cDataTypeInt32 = 3,       // 4 byte number
    cDataTypeInt64 = 4,       // 8 byte number
    cDataTypeSingle = 5,      // single floating point
    cDataTypeDouble = 6,      // double floating point
    cDataTypeDecimal128 = 7,  // Reserved to be compatible with BSON, but probably not going to implement this data type.
    cDataTypeDateTime = 8,    // Delphi's TDateTime 8 byte value.
    cDataTypeUTCDateTime = 9, //The int64 is UTC milliseconds since the Unix epoch.
    cDataTypeDate = 10,
    cDataTypeTime = 11,
    cDataTypeGUID = 12,       // Stored and streamed as the 16 byte data that makes up a GUID.
    cDataTypeObjectID = 13,   // equivalent to the BSON ObjectID  12 bytes.   https://docs.mongodb.com/manual/reference/method/ObjectId/
    cDataTypeString = 14,     // Unicode encode string.   always 2 bytes per character.
    cDataTypeStringList = 15,
    cDataTypeFrame = 16,           // Each slot in the frame is identified by a case-insensitive UTF-8 string
    cDataTypeArray = 17,           //
    cDataTypeSparseArray = 18,
    cDataTypeBinary = 19,
    cDataTypeObject = 20,          // Note that this means that TDataObj holds an object instance where that object instance is serialized/deserialzed to/from the stream
                                   // directly.  The streaming still complies by using all the other core data types except that by convention, the Frame Object that is
                                   // serialized must have an attribute with the "_className" name.  That value is then used to instantiate the right object owned by TDataObj
                                   // which will then read the rest of the frame.  If that object can't be instantiated, then the TDataObj is a frame as normal with the attribute in place.
                                   // This lets us have a real object that we want to serialize in dataObject form without having to make a full copy of the data in DataObject
                                   // form first before we can serialize to a stream.  It means we are not doubling up the memory when reading from and writing from a stream.

    cDataTypeTag = 21              // a tag is an unsigned number (up to 32 bit).  The tag data type, then holds exactly one child dataObject that can be of any time (including another tag).

  );



  TDataType = packed record
  private
    fCode: TDataTypeCode;
    fSubClass: Byte;
    procedure setCode(const aValue: TDataTypeCode);
    procedure setSubClass(const aValue: byte);
  public
    property Code: TDataTypeCode read fCode write setCode;
    property SubClass: Byte read fSubClass write SetSubClass;
  end;

const
  cDataTypeStrings: array[0..21] of string = (
     'null',
     'Boolean',
     'Byte',
     'Int32',
     'Int64',
     'Single',
     'Double',
     'Decimal128',  // Not implemented yet, but this is the plan to be compatible with BSON
     'DateTime',
     'UTCDateTime',
     'Date',
     'Time',
     'GUID',
     'ObjectID',
     'String',
     'StringList',
     'Frame',
     'Array',
     'SparseArray',
     'Binary',
     'Object',
     'Tag');


type

  // forward declarations;
  TDataGUID = class;
  TDataObjectID = class;
  TDataStringList = class;
  TDataFrame = class;
  TDataArray = class;
  TDataSparseArray = class;
  TDataBinary = class;
  TDataTag = class;
  TDataAttributeStore = class;


  // The TDataStore's job is to be the container that "Holds" the actual data that is contained within the TDataObject.
  TDataStore = packed record
  private
    function getDataGUID: TDataGUID; inline;
    procedure setDataGUID(aValue: TDataGUID); inline;
    function getDataFrame: TDataFrame; inline;
    procedure setDataFrame(const aValue: TDataFrame); inline;
    function getDataArray: TDataArray; inline;
    procedure setDataArray(const Value: TDataArray); inline;
    function getDataSparseArray: TDataSparseArray; inline;
    procedure setDataSparseArray(const Value: TDataSparseArray); inline;
    function getDataBinary: TDataBinary; inline;
    procedure setDataBinary(const Value: TDataBinary); inline;
    function getDataTag: TDataTag; inline;
    procedure setDataTag(const Value: TDataTag); inline;
    function getDataObjectID: TDataObjectID; inline;
    function getDataStringList: TDataStringList; inline;
    procedure setDataObjectID(const Value: TDataObjectID); inline;
    procedure setDataStringList(const Value: TDataStringList); inline;
    function GetObject: TObject; inline;
    procedure SetObject(const Value: TObject); inline;
    function getDataString: string; inline;
    procedure setDataString(const Value: string); inline;
  public
    procedure ClearData; inline;

    property DataGUID: TDataGUID read getDataGUID write setDataGUID;
    property DataFrame: TDataFrame read getDataFrame write setDataFrame;
    property DataArray: TDataArray read getDataArray write setDataArray;
    property DataSparseArray: TDataSparseArray read getDataSparseArray write setDataSparseArray;
    property DataBinary: TDataBinary read getDataBinary write setDataBinary;
    property DataTag: TDataTag read getDataTag write setDataTag;
    property DataObjectID: TDataObjectID read getDataObjectID write setDataObjectID;
    property DataObject: TObject read getObject write setObject;
    property DataStringList: TDataStringList read getDataStringList write setDataStringList;
    property DataString: string read getDataString write setDataString;
  var
    fDataObject: TObject;         // If this data object holds a datatype that requires an object to hold it, that is stored/owned here.  Note, depending on the compiler, this may use arc or may not use arc.
    fDataString: string;          // if the data is going to be a string, then it is stored here so that arc can manage it.

    // Note that the biggest size of any of these storage values should be 8 bytes (pointer on 64 bit systems, or Double).  Anything that is bigger than this will have an object that is intantiated to hold it.
    case byte of
      ord(cDataTypeByte): (fDataByte: byte;);
      ord(cDataTypeBoolean): (fDataBoolean: boolean;);
      ord(cDataTypeInt32), ord(cDataTypeDate): (fDataInt32: Integer;);       // always 32 bit signed on every platform.  for Date, it's the number of days since 1899.
      ord(cDataTypeInt64), {cDataTypeVarInt,} ord(cDataTypeUTCDateTime): (fDataInt64: int64;);
      ord(cDataTypeSingle): (fDataSingle: single;);
      ord(cDataTypeDouble): (fDataDouble: double;);
//      cDataTypeDecimal128:();                                  // Reserved to be compatible with BSON, but probably not going to implement this data type yet.
      ord(cDataTypeDateTime): (fDataDateTime: TDateTime;);             // Delphi's TDateTime 8 byte value.
      ord(cDataTypeTime): (fDataTime: TTime;);
//      ord(cDataTypeGUID): (dataGUID: TDataGUID;);                     // object that's allocated to store the GUID data.
//      ord(cDataTypeObjectID): (dataObjectID: TDataObjectID;);         // equivalent to the BSON ObjectID  12 bytes.   https://docs.mongodb.com/manual/reference/method/ObjectId/
//      cDataTypeUTF8String: (dataUTF8String: PUTF8String;);     // Storing and serializing here as a unicode string and serialized as a unicode string (Unicode with default UTF-16 characters in memory)
//      ord(cDataTypeString): (dataUnicodeString: PString;);            // Storing and serializing as a UTF8String (1-4 bytes per character, but typically going to be 1)
//      ord(cDataTypeStringList): (dataStringList: TDataStringList;);
//      ord(cDataTypeFrame): (dataFrame: TDataFrame;);
//      ord(cDataTypeArray): (dataArray: TDataArray;);
//      ord(cDataTypeSparseArray): (dataSparseArray: TDataSparseArray;);
//      ord(cDataTypeBinary): (dataBinary: TDataBinary;);
//      ord(cDataTypeObject): (dataObject: TObject;);     // Note that this means that TDataObj holds an object instance where that object instance is serialized/deserialzed to/from the stream using RTTI
//      ord(cDataTypeTag): (dataTag: TDataTag;);

  end;

  PTDataStore = ^TDataStore;

  TDataObj = class;

  // This is the base class that all streamers must descend from.  It just defines the core abstract behavior for encoding and decoding to a descendant streamer's format.
  TDataObjStreamerBase = class
  private
    fOwnsStream: boolean;
    procedure setStream(const Value: TStream);
  protected
    fStream: TStream;   // reference only in most situations.   However, if you set OwnsStream to true, then when this object is freed, then fStream will be freed.
  public
    constructor Create(aStream: TStream = nil); virtual;
    destructor Destroy; override;

    function Clone: TDataObjStreamerBase; virtual;

    class function FileExtension: string; virtual; abstract;
    class function Description: string; virtual; abstract;
    class procedure GetParameterInfo(aParameterPurpose: TDataObjParameterPurposes; aStrings: TStrings); virtual;  // Will fill aStrings with information about the optional parameters that the streamer may support.
    class function GetFileFilter: string; virtual; abstract;
    class function IsFileExtension(aStr: string): boolean; virtual;
    class function ClipboardPriority: Cardinal; virtual; abstract;

    class function GetClipboardFormatStr: string; virtual;

    procedure Decode(aDataObj: TDataObj); virtual; abstract;
    procedure Encode(aDataobj: TDataObj); virtual; abstract;
    procedure ApplyOptionalParameters(aParams: TStrings); overload; virtual;
    procedure ApplyOptionalParameters(aParams: String); overload;

    property Stream: TStream read fStream write setStream;
    property OwnsStream: boolean read fOwnsStream write fOwnsStream;
  end;
  TDataObjStreamerClass = class of TDataObjStreamerBase;



  TDataObj = class
  strict private
    fStore: TDataStore;                 // most of the time, you should not access this directly.  use a call to GetStore to get it. This stores the actual data and it can vary what type of data it is holding.
    fDataType: TDataType;               // most of the time, you should not access this directly.
    fAttributes: TDataAttributeStore;   // will be nil unless attributes have been added.
    fFlags: Cardinal;                   // Set of flags that can be used for generic purposes.  DataObject Editor uses it for storing flags that signal how two dataObjects are diffed with each other.
  private
    function getAsArray: TDataArray;
    function getAsBinary: TDataBinary;
    function getAsBoolean: Boolean;
    function getAsByte: Byte;
    function getAsDate: TDate;
    function getAsDateTime: TDateTime;
    function getAsDouble: Double;
{$ifDef cMakeMoreCompatibleWithOldDataObjects}
    function getAsFloat: Double; deprecated 'Use AsDouble instead.';
{$endif}
    function getAsFrame: TDataFrame;
    function getAsGuid: TDataGUID;
    function getAsInt32: integer;
    function getAsInt64: Int64;
    function getAsObject: TObject;
    function getAsObjectID: TDataObjectID;
    function getAsSingle: Single;
    function getAsSparseArray: TDataSparseArray;
    function getAsString: String;
    function getAsStringList: TDataStringList;
    function getAsSymbol: String;
    function getAsTime: TTime;
    function getAsUTCDateTime: int64;
    function getDataType: TDataType;
    function getDataTypeString: String;
    function getAsTag: TDataTag;

    procedure setAsArray(const aValue: TDataArray);
    procedure setAsBinary(const aValue: TDataBinary);
    procedure setAsBoolean(const aValue: Boolean);
    procedure setAsByte(const aValue: Byte);
    procedure setAsDate(const aValue: TDate);
    procedure setAsDateTime(const aValue: TDateTime);
    procedure setAsDouble(const aValue: Double);
{$ifDef cMakeMoreCompatibleWithOldDataObjects}
    procedure setAsFloat(const aValue: Double); deprecated 'Use AsDouble instead.';
{$endif}
    procedure setAsFrame(const aValue: TDataFrame);
    procedure setAsGuid(const aValue: TDataGUID);
    procedure setAsInt32(const aValue: integer);
    procedure setAsInt64(const aValue: Int64);
    procedure setAsObject(const aValue: TObject);
    procedure setAsObjectID(const aValue: TDataObjectID);
    procedure setAsSingle(const aValue: Single);
    procedure setAsSparseArray(const aValue: TDataSparseArray);
    procedure setAsString(const aValue: String);
    procedure setAsStringList(const aValue: TDataStringList);
    procedure setAsSymbol(const aValue: String);
    procedure setAsTime(const aValue: TTime);
    procedure setAsUTCDateTime(const aValue: Int64);
    procedure setDataType(const Value: TDataType);

    function getItem(aKey: variant): TDataObj;  // if aKey is a string, this will treat this dataObj as a Frame and do a newSlot on the frame
                                                // if aKey is an integer, will treat this dataObj as an array and do a newSlot on the array
    function getHasAttributes: boolean;
    procedure setHasAttributes(const Value: boolean);

  public
    destructor Destroy; override;

    function getStore: PTDataStore; inline;   // Provides a way for code outside this unit to get work directly with the fDataStore

    procedure SetDataTypeParts(aCode: TDataTypeCode; aSubType: byte);

    // this will set this dataObj to the NULL value.  Any other data possibly held by this dataObject is freed including all attributes
    procedure Clear;

    // this will set this dataObj to the NULL value.  Any other data possibly held by this dataObject is freed but any attributes on this dataObject will stay intact.
    procedure ClearData;

    // this will clear all attributes from this DataObject.
    procedure ClearAttributes;

    //Prints the contents of this object into a human-readable text form.  This form is for viewing the contents, not for streaming out to share the data somewhere.
    procedure PrintToStringBuilder(aStringBuilder: TStringBuilder; aIndent: integer = 0);

    //Prints the contents of this object into a human-readable text form.
    function PrintToString: string;

    // Write the contents of this data object to a new memory buffer that is allocated and return the size of the buffer written to.
    // If the buffer size that was created is bigger than zero, then the aBuffer PointerPointer (This is a pointer to a pointer that will be updated to the address of the newly allocated buffer)
    // This is a bit goofy, but it is the easiest way to do remote debugging of a TDataObj using a custom visualizer within the Delphi IDE.  We can basically transfer the memory that was put into this buffer to then visualize it.
    // If aStreamerClass is left as undefined, then the default streamer class is used.
    function AllocateAndWriteToMemoryBuffer(aPointerToPointer: PPointer; aStreamerClass: TClass = nil): int64;

    // Write the contents of this data object to aStream using the given aStreamerClass.  If aStreamerClass is left as undefined, then the default streamer class is used.
    procedure WriteToStream(aStream: TStream; aStreamerClass: TClass = nil);

    // Read the contents of this data object from aStream using the given aStreamerClass.  If aStreamerClass is left as undefined, then the default streamer class is used.
    procedure ReadFromStream(aStream: TStream; aStreamerClass: TClass = nil);

    procedure ReadFromMemoryBuffer(aBuffer: Pointer; aSize: NativeInt; aStreamerClass: TClass = nil);

    procedure AssignTo(aObj: TObject; aMemberVisibilities: TMemberVisibilities = [mvPublished]);

    procedure AssignFrom(aObj: TObject; aMemberVisibilities: TMemberVisibilities = [mvPublished];
                         aDoNotSerializeDefaultValues: boolean = false; aSerializeEnumerationsAsIntegers: boolean = false);

    property Items[aKey: variant]: TDataObj read getItem; default;

    property AsBoolean: Boolean read getAsBoolean write setAsBoolean;
    property AsByte: Byte read getAsByte write setAsByte;
    property AsInt32: integer read getAsInt32 write setAsInt32;         // used when you want to explicitly serialize as a 32bit integer
    property AsInt64: Int64 read getAsInt64 write setAsInt64;           // used when you want to explicitly serialize as a 64bit integer
{$ifdef cMakeMoreCompatibleWithOldDataObjects}
    property AsInteger: integer read getAsInt32 write setAsInt32;       // This property is a duplicate of asInt32.  It is here for code backwards compatibility so that I can use this new DataObjects against old code that was making use of the old DataObjects code.
{$endif}
    property AsSingle: Single read getAsSingle write setAsSingle;
    property AsDouble: Double read getAsDouble write setAsDouble;
{$ifdef cMakeMoreCompatibleWithOldDataObjects}
    property AsFloat: Double read getAsFloat write setAsFloat;          // Only here for compatibility with old DDO code.
{$endif}

    property AsDateTime: TDateTime read getAsDateTime write setAsDateTime;
    property AsUTCDateTime: int64 read getAsUTCDateTime write setAsUTCDateTime;
    property AsDate: TDate read getAsDate write setAsDate;
    property AsTime: TTime read getAsTime write setAsTime;

    property AsGUID: TDataGUID read getAsGuid write setAsGuid;
    property AsObjectID: TDataObjectID read getAsObjectID write setAsObjectID;

    property AsString: String read getAsString write setAsString;
//    property AsUTF8String: UTF8String read getAsUTF8String write setAsUTF8String;
    property AsSymbol: String read getAsSymbol write setAsSymbol;
    property AsStringList: TDataStringList read getAsStringList write setAsStringList;

    property AsFrame: TDataFrame read getAsFrame write setAsFrame;
    property AsArray: TDataArray read getAsArray write setAsArray;
    property AsSparseArray: TDataSparseArray read getAsSparseArray write setAsSparseArray;

    property AsBinary: TDataBinary read getAsBinary write setAsBinary;
{$ifDef cMakeMoreCompatibleWithOldDataObjects}
    property AsMemStream: TDataBinary read getAsBinary write setAsBinary;
{$endif}
    property AsObject: TObject read getAsObject write setAsObject;

    property AsTag: TDataTag read getAsTag;   // note that a setAsTag is not necessary

    property DataType: TDataType read getDataType write setDataType;
    property DataTypeString: String read getDataTypeString;

    property HasAttributes: boolean read getHasAttributes write setHasAttributes;

    property Flags: Cardinal read fFlags write fFlags;

    function DataTypeIsAContainer: boolean;     // means that we can put one or more than one child item into this object.
                                                // This returns false for items that contain a fixed number of child objects such as the tag type.
    function DataTypeCanHaveAChildObject: boolean;  // returns true for those data types that are containers and for those other types that could have a child data object such as a tag type.



//    property AsOleVariant: OleVariant read getOleVariant write setOleVariant;   maybe implement this someday.

    procedure CopyFrom(aSrcDataObj: TDataObj);

    procedure ChangeCaseOnStrings(aContentCaseChange: TCaseChangeOptions; aSlotNameCaseChange: TCaseChangeOptions);


    // If you pass in nil for aStreamer then this function will try to find the right streamer class by the filename extension and aStreamer will be populated with that object.
    // NOTE: this function could potentially return a Streamer object, if so then the caller must free it.  It could return nil
    // This function will try to load this TDataObj by reading the file using one of the possible registered streamers.
    // Many different exceptions could be raised here, but if any exception is raised when reading data from a file, the object
    // will be populated with whatever data it was successful loading before the exception.
    // The caller must free the returned streamer object that was created if the caller did not pass in aStreamer
    function LoadFromFile(aFilename: string; aStreamer: TDataObjStreamerBase; aOptionalParameters: string=''): TDataObjStreamerBase; overload;
    procedure LoadFromFile(aFilename: string; aOptionalParams: string=''); overload;
{$ifDef cMakeMoreCompatibleWithOldDataObjects}
    procedure ReadFromFile(aFilename: string);
{$endif}
    //WriteToFile will look at the aFilename extension to choose a streamer class if one is not passed in through the aStreamerClass parameter.
    //by default, it will pick the ".DataObj" default streamer if a streamer cannot be determined by the parameter or the filename extension.
    procedure WriteToFile(aFilename: string; aStreamerClass: TDataObjStreamerClass = nil);  overload;

    // Write to the file identified by aFilename using the instance of aStreamer given the properties that have been set on aStreamer
    procedure WriteToFile(aFilename: string; aStreamer: TDataObjStreamerBase); overload;
  end;

{$ifDef cMakeMoreCompatibleWithOldDataObjects}
  TDataSlot = TDataObj;       // Just an alias for old code backward compatibility.
{$endif}

  TDataGUID = class
    private
        fGUID: TGUID;   // 16 bytes.
    function getAsString: string;
    procedure setAsString(const aValue: string);
  public
    property GUID: TGUID read fGUID write fGUID;
    property AsString: string read getAsString write setAsString;
  end;

  { The 12-byte ObjectId value consists of:
    a 4-byte value representing the seconds since the Unix epoch,
    a 3-byte machine identifier,
    a 2-byte process id, and
    a 3-byte counter, starting with a random value.
  }
  TDataObjectIDData = array[0..11] of byte;
  PTDataObjectIDData = ^TDataObjectIDData;
  TDataObjectID = class
  private
    function getAsString: string;
    procedure setAsString(const Value: string);
    function getSeconds: cardinal;
    function getMachineID: cardinal;
    function getProcessID: cardinal;
    function getCounter: cardinal;
    procedure setCounter(const Value: Cardinal);
    procedure setMachineID(const Value: Cardinal);
    procedure setProcessID(const Value: Cardinal);
    procedure setSeconds(const Value: Cardinal);
  public
    Data: TDataObjectIDData;
    property AsString: string read getAsString write setAsString;
    property Seconds: Cardinal read getSeconds write setSeconds;
    property MachineID: Cardinal read getMachineID write setMachineID;
    property ProcessID: Cardinal read getProcessID write setProcessID;
    property Counter: Cardinal read getCounter write setCounter;
    procedure GenerateNewID;
  end;

  // We are using our own type here just in case we want to add methods to it.
  TDataStringList = class(TStringList)
  public
    function GetAsArrayOfStrings: TArray<string>;
  end;

  TDataFrameEnumeratorRec = record
    DataObj: TDataObj;
    Slotname: string;
  end;

  // Started out using a Dictionary, but found that it is slower for case sensitive lookups when the number of slots is under 15 and
  // slower for case insensitive lookups when the number of slots is under about 50.   So, we just do brute force scanning to find a match.
  TDataFrame = class  //(TStringList)
  type
    TDataFrameEnumerator = class
    private
      FIndex: Integer;
      FDataFrame: TDataFrame;
    public
      constructor Create(ADataFrame: TDataFrame);
      function GetCurrent: TDataFrameEnumeratorRec;
      function MoveNext: Boolean;
      property Current: TDataFrameEnumeratorRec read GetCurrent;
    end;

  private
    fSlotList: TStringList;    //Owns the objects
    function getSlot(aIndex: integer): TDataObj;
    function FindSlotIndex(const aSlotName: string): integer;
    function GetItem(const aKey: string): TDataObj;
  public
    constructor Create;
    destructor Destroy; override;
    function GetEnumerator: TDataFrameEnumerator;
    function FindSlot(const aSlotName: string): TDataObj; overload;
    function FindSlot(const aSlotName: string; var oSlot: TDataObj): boolean; overload;

    // if aRaiseExceptionIfAlreadyExists is true, then an exception is raised if trying to add a new slot with the given aSlotName finds that this slot already exists.
    // the normal operation (when aRaiseExceptionIfAlreadyExists=false) when this occurs is to just return the slot that was found.
    function NewSlot(const aSlotName: string; aRaiseExceptionIfAlreadyExists: boolean = false): TDataObj;
    procedure AppendSlot(const aSlotName: string; aDataObj: TDataObj);
    function SlotByName(const aSlotName: string): TDataObj;
    function DeleteSlot(const aSlotName: string): boolean;   // returns true if the slot was found and deleted.
    function Delete(aIndex: integer): boolean;         // returns true if the slot was found and deleted.
    function RemoveSlot(aSlot: TDataObj): boolean;     // returns true if the slot was found and deleted.
    function IndexOfChildSlot(aSlot: TDataObj): integer;
    property Slots[aIndex: integer]: TDataObj read getSlot;
    function Slotname(aIndex: integer): string;
    procedure SetSlotname(aIndex: integer; aSlotname: string);
    function ChangeSlotName(aOldSlotName, aNewSlotName: string): Boolean;  // returns TRUE if in fact a slot did have it's name changed.  False othewise.  Only applies if the aOldSlotName is found and the aNewSlotName doesn't already exist.

{$ifDef cMakeMoreCompatibleWithOldDataObjects}
    property SlotNames[aIndex: integer]: string read Slotname;
{$endif}
    function Count: integer;
    procedure Clear;
    procedure CopyFrom(aSource: TDataFrame);

    property Items[const aKey: string]: TDataObj read GetItem; default;
  end;

  TForEachProcedure = reference to procedure(aArray: TDataArray; aCurrentValue: TDataObj; aIndex: integer);
  TForEachFunction = reference to function(aArray: TDataArray; aCurrentValue: TDataObj; aIndex: integer): boolean;
  TReduceProcedure = reference to procedure(aTotal: TDataObj; aArray: TDataArray; aCurrentValue: TDataObj; aIndex: integer);
  TMapProcedure = reference to procedure(aTargetObj: TDataObj; aCurrentArray: TDataArray; aCurrentValue: TDataObj; aIndex: integer);

  TDataArray = class
  type
    TDataArrayEnumerator = class
    private
      FIndex: Integer;
      FDataArray: TDataArray;
    public
      constructor Create(ADataArray: TDataArray);
      function GetCurrent: TDataObj;
      function MoveNext: Boolean;
      property Current: TDataObj read GetCurrent;
    end;

  const
    cInitialCapacity = 16;
  private
    fCount: integer;
    fCapacity: integer;
    fItems: array of TDataObj;
    function getSlot(aIndex: integer): TDataObj;
    function getItem(aIndex: Cardinal): TDataObj;
    procedure setItem(aIndex: Cardinal; const Value: TDataObj);
    procedure CheckIndexInRange(aIndex: integer); inline;    // will raise exception if aIndex is invalid
    procedure setCapacity(const aCapacity: Integer);
  public
    constructor Create(aInitialCapacity: Integer = 0);
    destructor Destroy; override;
    function GetEnumerator: TDataArrayEnumerator;
    function NewSlot: TDataObj;
    function Add(aDataObj: TDataObj): Integer;
    property Items[aIndex: Cardinal]: TDataObj read getItem write setItem; default;
    procedure Clear;

    // The following set of methods are very similar to the set of methods you can call on a javaScript array
    function Find(aFindFunction: TForEachFunction): TDataObj;   // can return nil if not found.
    procedure ForEach(aForEachFunction: TForEachProcedure; aReverseOrder: boolean = false);
    function Every(aEveryFunction: TForEachFunction): boolean;
    function IndexOf(aString: string; aCaseInsensitive: boolean = false): integer; overload;   // returns -1 if nothing found.
    function IndexOf(aInteger: Integer; aCaseInsensitive: boolean = false): integer; overload;   // returns -1 if nothing found.
    function IndexOf(aInt64: Int64; aCaseInsensitive: boolean = false): integer; overload;   // returns -1 if nothing found.
    function LastIndexOf(aString: string; aCaseInsensitive: boolean = false): integer;   // returns -1 if nothing found.

    function RemoveForEach(aEveryFunction: TForEachFunction): integer;    // returns the number of items removed.
    function Reduce(aReduceProcedure: TReduceProcedure): TDataObj;        // returns a newly created TDataObj that should contain the aggregated value according to how the aReduceProcedure calculates it.   It's up to the receiver to free it.
    function Map(aMapProcedure: TMapProcedure): TDataObj;                 // returns a newly created TDataObj that will be an array with the same number of slots as the source (self).  The values in each element are calculated by the map function.  It's up to the receiver to free it.
    function Concat(aArray: TDataArray): TDataObj;                        // returns a newly created TDataObj that will be a clone of self array and it will clone and append the slots from aArray.
    procedure AppendFrom(aArray: TDataArray);                             // clones the slots in aArray and adds them to self.  same as CopyFrom.
    property Slots[aIndex: integer]: TDataObj read getSlot;
    procedure DeleteSlot(aIndex: integer);
    function IndexOfChildSlot(aSlot: TDataObj): integer;
{$ifdef cMakeMoreCompatibleWithOldDataObjects}
    procedure CopyFrom(aArray: TDataArray);  deprecated 'Use AppendFrom instead';  // clones the slots in aArray and adds them to self.  same as AppendFrom.
{$endif}

    property Count: Integer read fCount;
    property Capacity: Integer read fCapacity write setCapacity;
  end;



  // TDataSparseArray = class(TDictionary<integer, TDataObj>);
  // Started out using a Dictionary, but found that it is slower for when the number of slots is under 15
  // Since the purpose of a sparse array is likely to not have a lot of items in it, we'll just use a simple list.
  // This implementation just adds the indexKeys to the normal array, so really, it's pretty much like a frame but with numbers instead of strings.
  // NOTE:  parameters that are "aIndex" are 0..n indexes.  parameters that are "aSlotIndex" are the numerical identifier for the slot.
  TDataSparseArray = class(TDataArray)
  private
    fIndexKeyList: TList<int64>;         // each "key" in the list is going to be an integer
  public
    constructor Create;
    destructor Destroy; override;
    function FindSlot(aSlotIndex: integer): TDataObj; overload;        // will return nil if aSlotIndex is not found.
    function FindSlot(aSlotIndex: integer; var oSlot: TDataObj): boolean; overload;
    function NewSlot(aSlotIndex: integer; aRaiseExceptionIfAlreadyExists: boolean = false): TDataObj;
    procedure AppendSlot(aSlotIndex: integer; aDataObj: TDataObj);
    function SlotByIndex(aSlotIndex: integer): TDataObj;     // generates an exception if the aSlotIndex is not found
    function DeleteSlot(aSlotIndex: integer): boolean;       // returns true if the slot was found and delted.
    function SlotIndex(aIndex: integer): integer;            // pass in the index into the collection (0..count-1) and get back the slot's indentifer index.
    procedure Clear;
    procedure CopyFrom(aSource: TDataSparseArray);
    function FindLargestSlotIndex: integer;                 // will scan through all slots in this sparseArray and return the slotIndex value that is the largest.  will return 0 if there are no slots in the collection.
  end;


  // We are using our own type here just in case we want to add methods to it in the future.
  TDataBinary = class(TMemoryStream)
  private
    fSubTypeCode: byte;
  public
    property SubTypeCode: byte read fSubTypeCode write fSubTypeCode;   // Matches the BSON SubType code for the Binary data type.  Default: 0.
  end;

  TDataTag = class
  private
    fTagValue: Cardinal;
    fDataObj: TDataObj;

    function getDataObj: TDataObj;
    procedure setDataObj(const Value: TDataObj);
  public
    constructor Create;
    destructor Destroy; override;

    property TagValue: Cardinal read fTagValue write fTagValue;
    property DataObj: TDataObj read getDataObj write setDataObj;
  end;

  TDataAttributeStore = class(TDataFrame)  // this is here to hold the attributes that might be applied to a DataObj.
  end;


  // Here are some useful utility functions
  procedure DataObjConvertStringListToArrayOfStrings(aDataObj: TDataObj);
  procedure DataObjConvertArrayOfStringsToStringList(aDataObj: TDataObj);
  procedure DataObjConvertBase64StringToBinary(aDataObj: TDataObj);
  procedure DataObjConvertBinaryToBase64String(aDataObj: TDataObj);

  procedure AssignObjectToDataObj(aDataObj: TDataObj; aObj: TObject; aAssignContext: TDataObjAssignContext);
  procedure AssignDataObjToObject(aDataObj: TDataObj; aObj: TObject; aAssignContext: TDataObjAssignContext);



var
  gNewObjectID_MachineID: integer;   // These two global variables are used for generating new ObjectID values
  gNewObjectID_Counter: Cardinal;

implementation

uses DataObjects2Streamers, Variants;

type
  // Helper class to be used in the TDataObj.AllocateAndWriteToMemoryBuffer
  TDetachableMemoryStream = class(TMemoryStream)
  public
    destructor Destroy; override;
  end;

type
  TGetObjProc = reference to function: TDataObj;

var
  gRttiContext: TRttiContext;  // Used for the RTTI assignment to/from DataObjects.  Is created in initialization.

function GetRttiContext: TRttiContext;
begin
  Result := gRttiContext;
end;

{ Unix date conversion support.  Note that the default Delphi code in System.DateUtils has these functions but they are WRONG.
  They are doing SecondIncrements instead of Millisecond Increments }
function UnixToDateTime(const AValue: Int64; AReturnUTC: Boolean = True): TDateTime;
begin
  if AReturnUTC then
    Result := IncMilliSecond(UnixDateDelta, AValue)
  else
    Result := TTimeZone.Local.ToLocalTime(IncMilliSecond(UnixDateDelta, AValue));
end;

function DateTimeToUnix(const AValue: TDateTime): Int64;
begin
  Result := MilliSecondsBetween(UnixDateDelta, AValue);
  if AValue < UnixDateDelta then
     Result := -Result;
 end;





function TDataStore.getDataArray: TDataArray;
begin
  result := TDataArray(fDataObject);
end;

function TDataStore.getDataBinary: TDataBinary;
begin
  result := TDataBinary(fDataObject);
end;

function TDataStore.getDataFrame: TDataFrame;
begin
  result := TDataFrame(fDataObject);
end;

function TDataStore.getDataGUID: TDataGUID;
begin
  result := TDataGUID(fDataObject);
end;

function TDataStore.getDataObjectID: TDataObjectID;
begin
  result := TDataObjectID(fDataObject);
end;

function TDataStore.getDataSparseArray: TDataSparseArray;
begin
  result := TDataSparseArray(fDataObject);
end;

function TDataStore.getDataStringList: TDataStringList;
begin
  result := TDataStringList(fDataObject);
end;

function TDataStore.getDataTag: TDataTag;
begin
  result := TDataTag(fDataObject);
end;

function TDataStore.GetObject: TObject;
begin
  result := fDataObject;
end;

procedure TDataStore.setDataArray(const Value: TDataArray);
begin
  fDataObject := Value;
end;

procedure TDataStore.setDataBinary(const Value: TDataBinary);
begin
 fDataObject := Value;
end;

procedure TDataStore.setDataFrame(const aValue: TDataFrame);
begin
  fDataObject := aValue;
end;

procedure TDataStore.setDataGUID(aValue: TDataGUID);
begin
  fDataObject := aValue;
end;

procedure TDataStore.setDataObjectID(const Value: TDataObjectID);
begin
  fDataObject := Value;
end;

procedure TDataStore.setDataSparseArray(const Value: TDataSparseArray);
begin
  fDataObject := Value;
end;

procedure TDataStore.setDataStringList(const Value: TDataStringList);
begin
  fDataObject := Value;
end;

procedure TDataStore.setDataTag(const Value: TDataTag);
begin
  fDataObject := Value;
end;

procedure TDataStore.SetObject(const Value: TObject);
begin
  fDataObject := Value;
end;

//*********************************************************************************************************
//*********************************************************************************************************
// procedures used to assign an Object to a DataObject
procedure AssignVariantToDataObj(aDataObj: TDataObj; aVariant: Variant; aAssignContext: TDataObjAssignContext);
begin
  //FINISH
end;

Procedure AssignValueToDataObj(aValue: TValue; aDefault: integer; aAssignContext: TDataObjAssignContext; aGetObjProc: TGetObjProc);
var
  i: integer;
  lObject: TObject;
  lArray: TDataArray;
  lByte: Byte;
  lInteger: Integer;
  lInt64: int64;
  lLength: integer;
  lTypeInfo: PTypeInfo;
  lRecord: TRttiRecordType;
  lField: TRttiField;
  lFrame: TDataFrame;
type
  PTGuid = ^TGuid;
begin
  try
    case aValue.Kind of
      tkInteger: begin
        if aValue.TypeInfo = System.TypeInfo(Boolean) then
        begin
          aGetObjProc.AsBoolean := aValue.AsInteger <> 0;
        end
        else
        begin
          lInteger := aValue.AsInteger;
          if (lInteger <> aDefault) or (aAssignContext.DoNotSerializeDefaultValues = false) then  // note that Default values can only be up to 32 bit.
            aGetObjProc.AsInt32 := lInteger;
        end;
      end;

      tkChar: begin
        lInteger := aValue.AsOrdinal;
        if (lInteger <> aDefault) or (aAssignContext.DoNotSerializeDefaultValues = false) then  // note that Default values can only be up to 32 bit.
          aGetObjProc.AsByte := lInteger;
      end;

      tkEnumeration: begin
        //  We can either serialize emumerations as text symbols or as ordinal values?   Default option is to do symbols(text)
        if aValue.TypeInfo = System.TypeInfo(Boolean) then
        begin
          aGetObjProc.AsBoolean := aValue.AsOrdinal <> 0;  // right now we are not skipping this if DoNotSerializeDefaultValues is true no matter what the value is.
        end
        else
        begin
          lInt64 := aValue.AsOrdinal;
          if aAssignContext.SerializeEnumerationsAsIntegers then
          begin
            if (lInt64 <> aDefault) or (aAssignContext.DoNotSerializeDefaultValues = false) then  // note that Default values can only be up to 32 bit.
            begin
              case aValue.DataSize of
                1: aGetObjProc.AsByte:=lInt64;
                2, 4: aGetObjProc.AsInt32:=lInt64;
                else
                  aGetObjProc.AsInt64:=lInt64;
              end;
            end;
          end
          else
          begin
            // We serialize an enumeration as a symbol (text) which is the text representation of the enumerated value.
            lTypeInfo := aValue.TypeInfo;
            aGetObjProc.AsString := GetEnumName(lTypeInfo, lInt64)
          end;
        end;
      end;

      tkFloat:
      begin
        if aValue.Typeinfo = System.TypeInfo(TDateTime) then
          aGetObjProc.AsDateTime:=aValue.AsExtended
        else
          aGetObjProc.AsDouble:=aValue.AsExtended;
      end;

      tkString, tkLString, tkWString, tkUString:
      begin
        if (aValue.AsString <> '') or (aAssignContext.DoNotSerializeDefaultValues = false) then
        begin
          aGetObjProc.AsString:=aValue.AsString;
        end;
      end;

      tkSet:
      begin
        case aValue.DataSize of
          1: begin
            lByte := PByte(aValue.GetReferenceToRawData)^;
            if (lByte <> aDefault) or (aAssignContext.DoNotSerializeDefaultValues=false) then
              aGetObjProc.AsByte:=lByte;
          end;
          2, 4: begin
            lInteger := PInteger(aValue.GetReferenceToRawData)^;
            if (lInteger <> aDefault) or (aAssignContext.DoNotSerializeDefaultValues=false) then
              aGetObjProc.AsInt32:=lInteger;
          end;
          // HMMM.  can a set be bigger than 32bit?  The RTTI suggests no, and if you try, you get a compiler error.
        end;
      end;

      tkClass:
      begin
        lObject:=aValue.AsObject;
        if Assigned(lObject) then
        begin
          if (lObject is TCollection) then   // Collections are the way that Delphi serializes published child items (TPersistent descendants)
          begin
            lArray := aGetObjProc().AsArray;   // make this before the loop so we can actually put out an empty array if the collection is empty.
            for i := 0 to TCollection(lObject).Count - 1 do
            begin
              // collections are saved as an array of items
              if not aAssignContext.IsAlreadySerialized(TCollection(lObject).Items[i]) then  // as long as this object hasn't been serialized already by a parent hierarachially
              begin
                AssignObjectToDataObj(lArray.newSlot,TCollection(lObject).Items[i], aAssignContext);
              end;
            end;
          end
          // FINISH - Are we going to automatically serialize other types of collection items?  TList, TObjectList, TDictionary, etc?
          else
          begin
            // General Object.
            if not aAssignContext.IsAlreadySerialized(lObject) then  // as long as this object hasn't been serialized already by a parent hierarachially
            begin
              AssignObjectToDataObj(aGetObjProc(), lObject, aAssignContext);    // two-function recursion happening here.
            end;
          end;
        end;
      end;

//      tkMethod: ;

      tkWChar: begin
        lInteger := aValue.AsOrdinal;
        if (lInteger <> aDefault) or (aAssignContext.DoNotSerializeDefaultValues = false) then  // note that Default values can only be up to 32 bit.
          aGetObjProc.AsInt32 := lInteger;
      end;

      tkVariant: begin
        AssignVariantToDataObj(aGetObjProc, aValue.AsVariant, aAssignContext);
      end;

      tkArray,tkDynArray: begin
        lLength := aValue.GetArrayLength;
        lArray := aGetObjProc.AsArray;
        for i := 0 to lLength-1 do
        begin
          AssignValueToDataObj(aValue.GetArrayElement(i), 0, aAssignContext,
            function: TDataObj
            begin
              result := lArray.NewSlot;
            end
          );
        end;
      end;

      tkRecord: begin
        lTypeInfo := aValue.TypeInfo;

        if lTypeInfo = TypeInfo(TGuid) then
        begin
          // We need to handle GUID's specifically.
          aGetObjProc.AsGUID.GUID := PTGUID(aValue.GetReferenceToRawData)^;
        end
        else
        begin
          lRecord := gRttiContext.GetType(lTypeInfo).AsRecord ;
          lFrame := aGetObjProc.AsFrame;
          for lField in lRecord.GetFields do
          begin
            // NOTE:  Delphi has a bug where if a record contains a property and that property is an array ( such as an array[0..7] of byte for example )
            // The array property is not given a TRTTI type by the compiler.  And thus, if you ask for it, it will be nil.
            // We have no choice but to skip these types of properties.
            if assigned(lField.FieldType) then
            begin
              // I don't think there is any such thing as a defined default value for a record field like there is with object fields cause it really only is applicable to published properties.  However, we still consider zero as our serialization default.
              AssignValueToDataObj(lField.GetValue(aValue.GetReferenceToRawData), 0, aAssignContext,
                function: TDataObj
                begin
                  result := lFrame.NewSlot(lField.name);
                end
              );
            end;
          end;
        end;
      end;

//      tkInterface: ;  // Is it possible to maybe serialize an interface reference?   Hmmm.

      tkInt64: begin
        if aValue.TypeInfo = System.TypeInfo(Boolean) then    // can we have a 64 bit Boolean?  Not sure if it is actually possible.
        begin
          aGetObjProc.AsBoolean := aValue.AsInt64 <> 0;
        end
        else
        begin
          lInt64 := aValue.AsInt64;
          if (lInt64 <> aDefault) or (aAssignContext.DoNotSerializeDefaultValues = false) then  // note that Default values can only be up to 32 bit.
            aGetObjProc.AsInt64 := lInt64;
        end;
      end;

//      tkClassRef: ;
//      tkPointer: ;
//      tkProcedure: ;
    end;
  except
    //Just going to trap all errors and move on to the next property.  However, report it to our AssignContext and maybe the caller wants to do something with it.
    on e: exception do
    begin
      aAssignContext.ReportException(e);
    end;
  end;

end;

Procedure AssignObjectPropertyToFrame(aFrame: TDataFrame; aObj: TObject; aRttiProp: TRttiProperty; aAssignContext: TDataObjAssignContext);
var
  lDefault: integer;
begin
  // Note that a Default is only possible on 32 ordinal values in the RTTI.
  if aRttiProp is TRttiInstanceProperty then
    lDefault := TRttiInstanceProperty(aRttiProp).Default     // we need this in order to lookup the default value of the property.
  else
    lDefault := 0;

  AssignValueToDataObj(aRttiProp.GetValue(aObj), lDefault, aAssignContext,
    function: TDataObj
    begin
      result := aFrame.NewSlot(aRttiProp.Name);
    end
  );
end;

procedure AssignObjectToDataObj(aDataObj: TDataObj; aObj: TObject; aAssignContext: TDataObjAssignContext);
var
  lRttiType: TRttiType;
  lRttiProp: TRttiProperty;
  lFrame: TDataFrame;
  lProperties: TArray<TRttiProperty>;
begin
  // Now go though the members of the instance and serialize those
  if aObj = nil then
  begin
    aDataObj.Clear;   // makes the dataObj contain nil because aObj is nil.
  end
  else
  begin
    lRttiType := GetRttiContext.GetType(aObj.ClassType);

    if lRttiType.IsInstance then
    begin
      lFrame := aDataObj.AsFrame;
      aAssignContext.AddObject(aObj);   // important to be here so that child properties can't ultimately refer back to aObj and cause infinite recursion.

      if aAssignContext.IncludeSerializingClassName then
        lFrame.NewSlot('_Class').AsSymbol := aObj.ClassName;

      lProperties := lRttiType.GetProperties;
      for lRttiProp in lProperties do
      begin
        if (lRTTIProp.IsReadable) then
        begin
          if (lRTTIProp.Visibility in aAssignContext.MemberVisibilities) then
          begin
            AssignObjectPropertyToFrame(lFrame, aObj, lRttiProp, aAssignContext);
          end;
        end;
      end;
    end;
  end;
end;

//*********************************************************************************************************
//*********************************************************************************************************
// procedures used to assign a DataObject to an Object


// aValue should really have been made prior to calling this procedure according to the property that this value is for so it has the right Kind set.
// This method will update the value that this aValue holds from associated data in the dataObject.
function AssignDataObjToValue(aDataObj: TDataObj; var aValue: TValue; aAssignContext: TDataObjAssignContext): boolean;   // returns true if a value was loaded
var
  i: Integer;
  lObject: TObject;
//  lValueArray: TArray<TValue>;
  lArray: TDataArray;
  lTypeInfo: PTypeInfo;
  lRecord: TRttiRecordType;
  lFrame: TDataFrame;
  lField: TRttiField;
  lValue: TValue;   // for nested records.
type
  PTGuid = ^TGUID;
begin
  result := false;

  if not assigned(aDataObj) then exit;    // Handle the case where we don't have a dataObject for this value.
  
  try
    case aValue.Kind of
      tkSet:
      begin
        case aValue.DataSize of
          1: begin
            PByte(aValue.GetReferenceToRawData)^ := aDataObj.AsByte;
            result := true;
          end;
          2, 4: begin
            PInteger(aValue.GetReferenceToRawData)^ := aDataObj.AsInt32;
            result := true;
          end;
          // HMMM.  can a set be bigger than 32bit?  The RTTI suggests no, and if you try, you get a compiler error.
        end;
      end;

      tkEnumeration: begin
        // Emumerations can either be serialized as text symbols or as ordinal values?   Default option is to do symbols(text).  We need to detect type and do either.
        case aDataObj.DataType.Code of
          cDataTypeNull: begin
            result := false;
          end;
          cDataTypeBoolean: begin
            aValue := aDataObj.AsBoolean;
            result := true;
          end;
          cDataTypeByte: begin
            aValue.FromOrdinal(aValue.TypeInfo, aDataObj.AsByte);
            result := true;
          end;
          cDataTypeInt32: begin
            aValue.FromOrdinal(aValue.TypeInfo, aDataObj.AsInt32);
            result := true;
          end;
          cDataTypeInt64: begin
            aValue.FromOrdinal(aValue.TypeInfo, aDataObj.AsInt64);
            result := true;
          end;
          cDataTypeString: begin
            aValue.FromOrdinal(aValue.TypeInfo, GetEnumValue(aValue.TypeInfo, aDataObj.AsString));
//            aValue := GetEnumValue(aValue.TypeInfo, aDataObj.AsString);
//            aRttiProp.SetValue(aObj, TValue.FromOrdinal(lValue.TypeInfo, GetEnumValue(lValue.TypeInfo, aDataObj.AsString)));
            result := true;
          end;
          cDataTypeTag: begin
            // Look inside the tag to pickup the nested item and get back to this method recursively.
            AssignDataObjToValue(aDataObj.AsTag.getDataObj, aValue, aAssignContext);
            result := true;
          end;
        end;
      end;

      tkInteger:
      begin
        aValue := aDataObj.AsInt32;
        result := true;
      end;

      tkChar, tkWChar:
      begin
        aValue := TValue.FromOrdinal(aValue.TypeInfo, aDataObj.AsInt64);
      end;

      tkInt64:
      begin
        aValue := aDataObj.AsInt64;
        result := true;
      end;

      tkFloat:
      begin
        case aDataObj.DataType.Code of
          cDataTypeSingle: begin aValue := aDataObj.AsSingle; result := true; end;
          cDataTypeDouble: begin aValue := aDataObj.AsDouble; result := true; end;
          cDataTypeDatetime: begin aValue := aDataObj.AsDateTime; result := true; end;
          cDataTypeTime: begin aValue := aDataObj.AsTime; result := true; end;
          cDatatypeDate: begin aValue := aDataObj.AsDate; result := true; end;
        end;
      end;

      tkString, tkLString, tkWString, tkUString:
      begin
        aValue := aDataObj.AsString;
        result := true;
      end;

      tkClass:
      begin
        // if the property is a class, then it is expected that the aDataObj is a frame

        // aValue should contain an object, but is that object created yet or not?  If it is created, then we nest an assignment.
        // FINISH - If it's not yet created, then we need to create it.  How do we know which object to create?
        lObject := aValue.AsObject;

        if aDataObj.DataType.Code = cDataTypeFrame then
        begin
          if assigned(lObject) then
          begin
            AssignDataObjToObject(aDataObj, aValue.AsObject, aAssignContext);
            result := true;
          end;
        end
        else if aDataObj.DataType.Code = cDataTypeArray then
        begin
          // an array can be serialized into a Class only if the class is a TCollection.
          // Future - maybe we support other collection types of classes in the future.
          if aValue.AsObject is TCollection then
          begin
            // Note:  the TCollection instance knows what class of items (only one) it can contain so
            lArray := aDataObj.AsArray;
            for i := 0 to lArray.Count-1 do
            begin
              AssignDataObjToObject(lArray.Slots[i], TCollection(aValue.AsObject).Add, aAssignContext);
            end;
            result := true;
          end;
        end;
      end;

      tkVariant: begin
        case aDataObj.DataType.code of
          cDataTypeNull: aValue := nil;
          cDataTypeBoolean: begin aValue := aDataObj.AsBoolean; result := true; end;
          cDataTypeByte: begin aValue := aDataObj.AsByte; result := true; end;
          cDataTypeInt32: begin aValue := aDataObj.AsInt32; result := true; end;
          cDataTypeInt64: begin aValue := aDataObj.AsInt64; result := true; end;
          cDataTypeSingle: begin aValue := aDataObj.AsSingle; result := true; end;
          cDataTypeDouble: begin aValue := aDataObj.AsDouble; result := true; end;
//          cDataTypeDecimal128: ;
          cDataTypeDateTime: begin aValue := aDataObj.AsDateTime; result := true; end;
          cDataTypeUTCDateTime: begin aValue := aDataObj.AsUTCDateTime; result := true; end;
          cDataTypeDate: begin aValue := aDataObj.AsDate; result := true; end;
          cDataTypeTime: begin aValue := aDataObj.AsTime; result := true; end;
          cDataTypeGUID: begin aValue := aDataObj.AsGUID.AsString; result := true; end;
          cDataTypeObjectID: begin aValue := aDataObj.AsObjectID.AsString; result := true; end;
          cDataTypeString: begin aValue := aDataObj.AsString; result := true; end;
          cDataTypeStringList: begin {FINISH} end;
          cDataTypeFrame: begin {FINISH} end;
          cDataTypeArray: begin {FINISH} end;
          cDataTypeSparseArray: begin {FINISH} end;
          cDataTypeBinary: begin {FINISH} end;
          cDataTypeObject: begin {FINISH} end;
          cDataTypeTag: result := AssignDataObjToValue(aDataObj.AsTag.getDataObj, aValue, aAssignContext);    // recursion here.
        end;
      end;

      tkArray,tkDynArray: begin
(*   FINISH some day
        if aDataObj.DataType.code = cDataTypeArray then
        begin
          lArray := aDataObj.AsArray;
          SetLength(lValueArray, lArray.Count);
          for i := 0 to lArray.Count-1 do
          begin
            aValue.TypeInfo.Kind

            AssignDataObjToObject(lArray.Slots[i], ??, aAssignContext);
          end;

          aValue.FromArray(aValue.TypeInfo, lValueArray);
        end;
        *)
      end;

      tkRecord, tkMRecord: begin            // tkMRecord is for a managed record.  It is new.  Not really sure what the implications are.
        lTypeInfo := aValue.TypeInfo;

        if lTypeInfo = TypeInfo(TGuid) then
        begin
          // We need to handle GUID's specifically as a specifically known type of record.
          case aDataObj.DataType.Code of
            cDataTypeGUID: PTGUID(aValue.GetReferenceToRawData)^ := aDataObj.AsGUID.GUID;
            cDataTypeString: begin
              PTGUID(aValue.GetReferenceToRawData)^ := aDataObj.AsGUID.GUID;
            end;
//            cDataTypeFrame:       // possibly could do 4 known slots in a frame;
//            cDataTypeArray:       // possibly could do 4 numbers in an array;
//            cDataTypeSparseArray: // possibly could do 4 numbers in a sparsearray;
            cDataTypeBinary: begin
              if aDataObj.AsBinary.Size = sizeof(TGUID) then
              begin
                PTGUID(aValue.GetReferenceToRawData)^ := PTGUID(aDataObj.AsBinary.Memory)^;   // copy the 16 bytes
              end;
            end;
          end;
        end
        else
        begin
          lRecord := gRttiContext.GetType(lTypeInfo).AsRecord ;
          lFrame := aDataObj.AsFrame;
          for lField in lRecord.GetFields do
          begin
            // NOTE:  Delphi has a bug where if a record contains a property and that property is an array ( such as an array[0..7] of byte for example )
            // The array property is not given a TRTTI type by the compiler.  And thus, if you ask for it, it will be nil.
            // We have no choice but to skip these types of properties.
            if assigned(lField.FieldType) then
            begin
              // I don't think there is any such thing as a defined default value for a record field like there is with object fields cause it really only is applicable to published properties.  However, we still consider zero as our serialization default.
              lValue := lField.GetValue(aValue.GetReferenceToRawData);
              AssignDataObjToValue(lFrame.FindSlot(lField.Name), lValue, aAssignContext);
            end;
          end;
        end;
      end;

    end;
  except
    //Just going to trap all errors and move on to the next property.
    on e: exception do
    begin
      aAssignContext.ReportException(e);
    end;
  end;

end;


procedure AssignDataObjToObjectProperty(aDataObj: TDataObj; aObj: TObject; aRttiProp: TRttiProperty; aAssignContext: TDataObjAssignContext);
var
  lValue: TValue;
begin
  if not assigned(aDataObj) then exit;     // aDataObj could be nil which means we have nothing to load.

  if aRttiProp.IsWritable then
  begin
    lValue := aRttiProp.GetValue(aObj);
    AssignDataObjToValue(aDataObj, lValue, aAssignContext);
    aRttiProp.SetValue(aObj, lValue);
  end;
end;

procedure AssignDataObjToObject(aDataObj: TDataObj; aObj: TObject; aAssignContext: TDataObjAssignContext);
var
  lRttiType: TRttiType;
  lRttiProp: TRttiProperty;
  lFrame: TDataFrame;
  lProperties: TArray<TRttiProperty>;
begin
  // Now go though the members of the instance and try to serialize values into them from the incoming aDataObj if that aDataObj actually has data for each property
  lRttiType := GetRttiContext.GetType(aObj.ClassType);

  if not (aDataObj.DataType.Code = cDataTypeFrame) then exit;

  if lRttiType.IsInstance then
  begin
    lFrame := aDataObj.AsFrame;

    lProperties := lRttiType.GetProperties;
    for lRttiProp in lProperties do
    begin
      if (lRTTIProp.IsReadable) then
      begin
        if (lRTTIProp.Visibility in aAssignContext.MemberVisibilities) then
        begin
          AssignDataObjToObjectProperty(lFrame.FindSlot(lRTTIProp.Name), aObj, lRttiProp, aAssignContext);
        end;
      end;
    end;
  end;
end;



//*********************************************************************************************************
//*********************************************************************************************************


{ TDataObj }

// This call is here to facilitate remote debugging of TDataObject contents to the Delphi IDE through the debugger.
function TDataObj.AllocateAndWriteToMemoryBuffer(aPointerToPointer: PPointer; aStreamerClass: TClass = nil): int64;
var
  LStream: TDetachableMemoryStream;
begin
  LStream:=TDetachableMemoryStream.Create;
  try
    WriteToStream(LStream, aStreamerClass);
    result := lStream.Size;    // return how many bytes we wrote into the memory buffer, which is likely less than the actual size of the memory buffer.

    if lStream.Size > 0 then
    begin
      // now that lStream has a memory buffer, we need to steal it.  The pointer we are given is the location of a pointer that we need to update with the address of the memory we are going to steal.
      aPointerToPointer^ := lStream.Memory;
      // Caller needs to use FreeMem(Memory) to free the memory we are transferring ownership of.
    end
    else
      aPointerToPointer^ := nil;   // we are not giving the caller any memory so tell them so.

  finally
    lStream.Free;   // will free the stream class, but it will not free the memory allocated within the stream object if it has any data in it because we stole it above.
  end;
end;

procedure TDataObj.AssignFrom(aObj: TObject; aMemberVisibilities: TMemberVisibilities = [mvPublished]; aDoNotSerializeDefaultValues: boolean = false; aSerializeEnumerationsAsIntegers: boolean = false);
var
  lAssignContext: TDataObjAssignContext;
begin
  lAssignContext:=TDataObjAssignContext.Create;
  try
    lAssignContext.MemberVisibilities := aMemberVisibilities;
    lAssignContext.DoNotSerializeDefaultValues := aDoNotSerializeDefaultValues;
    lAssignContext.SerializeEnumerationsAsIntegers := aSerializeEnumerationsAsIntegers;
    AssignObjectToDataObj(self, aObj, lAssignContext);
  finally
    lAssignContext.Free;
  end;
end;

procedure TDataObj.AssignTo(aObj: TObject; aMemberVisibilities: TMemberVisibilities = [mvPublished]);
var
  lAssignContext: TDataObjAssignContext;
begin
  lAssignContext:=TDataObjAssignContext.Create;
  try
    lAssignContext.MemberVisibilities := aMemberVisibilities;
    AssignDataObjToObject(self, aObj, lAssignContext);
  finally
    lAssignContext.Free;
  end;
end;

procedure TDataObj.ChangeCaseOnStrings(aContentCaseChange, aSlotNameCaseChange: TCaseChangeOptions);
var
  i: integer;
  lStringList: TDataStringList;
  lFrame: TDataFrame;
  lArray: TDataArray;
  lSparseArray: TDataSparseArray;
begin
  case DataType.Code of
    cDataTypeString: begin
      if aContentCaseChange = TCaseChangeOptions.cCaseChangeUpper then
        AsString := UpperCase(AsString)
      else if aContentCaseChange = TCaseChangeOptions.cCaseChangeLower then
        AsString := LowerCase(AsString);
    end;

    cDataTypeStringList: begin
      if (aContentCaseChange <> TCaseChangeOptions.cCaseChangeNone) then
      begin
        lStringList := AsStringList;
        for i := 0 to lStringList.Count-1 do
        begin
          if aContentCaseChange = TCaseChangeOptions.cCaseChangeUpper then
            lStringList.Strings[i] := UpperCase(lStringList.Strings[i])
          else if aContentCaseChange = TCaseChangeOptions.cCaseChangeLower then
            lStringList.Strings[i] := LowerCase(lStringList.Strings[i]);
        end;
      end;
    end;

    cDataTypeFrame: begin
      lFrame := AsFrame;
      for i := 0 to lFrame.Count-1 do
      begin
        if aSlotNameCaseChange = TCaseChangeOptions.cCaseChangeUpper then
          lFrame.SetSlotname(i, UpperCase(lFrame.Slotname(i)))
        else if aSlotNameCaseChange = TCaseChangeOptions.cCaseChangeLower then
          lFrame.SetSlotname(i, LowerCase(lFrame.Slotname(i)));

        lFrame.Slots[i].ChangeCaseOnStrings(aContentCaseChange, aSlotNameCaseChange);    // recursion happening here.
      end;
    end;

    cDataTypeArray: begin
      lArray := AsArray;
      for i := 0 to lArray.Count-1 do
      begin
        lArray.Slots[i].ChangeCaseOnStrings(aContentCaseChange, aSlotNameCaseChange);    // recursion happening here.
      end;
    end;

    cDataTypeSparseArray: begin
      lSparseArray := AsSparseArray;
      for i := 0 to lSparseArray.Count-1 do
      begin
        lSparseArray.Slots[i].ChangeCaseOnStrings(aContentCaseChange, aSlotNameCaseChange);    // recursion happening here.
      end;
    end;

    cDataTypeObject: begin
      //FINISH
    end;
  end;
end;

procedure TDataObj.Clear;
begin
  ClearData;
  ClearAttributes;
end;

procedure TDataObj.ClearAttributes;
begin
  FreeAndNil(fAttributes);
end;

procedure TDataObj.ClearData;
begin
  fStore.ClearData;
  fDataType.Code:= cDataTypeNULL;
end;

procedure TDataObj.CopyFrom(aSrcDataObj: TDataObj);
var
  lPosition: Int64;
begin
  if not assigned(aSrcDataObj) then exit;
  
  case aSrcDataObj.DataType.Code of
    cDataTypeNull: self.Clear; // makes sure we are a null object

    cDataTypeBoolean: self.AsBoolean := aSrcDataObj.AsBoolean;

    cDataTypeByte: self.AsByte := aSrcDataObj.AsByte;

    cDataTypeInt32: self.AsInt32 := aSrcDataObj.AsInt32;

    cDataTypeInt64: self.AsInt64 := aSrcDataObj.AsInt64;

    cDataTypeSingle: self.AsSingle := aSrcDataObj.AsSingle;

    cDataTypeDouble: self.AsDouble := aSrcDataObj.AsDouble;

//    cDataTypeDecimal128: self.AsDecimal128 := aSrcDataObj.AsDecimal128;

    cDataTypeDateTime: self.AsDateTime := aSrcDataObj.AsDateTime;

    cDataTypeUTCDateTime: self.AsUTCDateTime := aSrcDataObj.AsUTCDateTime;

    cDataTypeDate: self.AsDate := aSrcDataObj.AsDate;

    cDataTypeTime: self.AsTime := aSrcDataObj.AsTime;

    cDataTypeGUID: Self.AsGUID.GUID := aSrcDataObj.AsGUID.GUID;

    cDataTypeObjectID: self.AsObjectID.Data := aSrcDataObj.AsObjectID.Data;

    cDataTypeString: self.AsString := aSrcDataObj.AsString;

    cDataTypeStringList: self.AsStringList.AddStrings(aSrcDataObj.AsStringList);      // note, we could be adding additional strings to self.

    cDataTypeFrame: self.AsFrame.CopyFrom(aSrcDataObj.AsFrame);                       // note, we could be adding additional slots to self or we could be assigning new values to slots that are coming from the source but are also in self.

    cDataTypeArray: self.AsArray.AppendFrom(aSrcDataObj.AsArray);                     // note, we could be adding additional slots to self

    cDataTypeSparseArray: self.AsSparseArray.copyFrom(aSrcDataObj.AsSparseArray);     // note, we could be adding additional slots to self or we could be assigning new values to slots that are coming from the source but are also in self.

    cDataTypeBinary: begin
      lPosition := aSrcDataObj.AsBinary.Position;                                     // Note.  This does not clear out self's binary stream first.  It appends more data to the end of the stream.
      aSrcDataObj.AsBinary.Seek(0,soFromBeginning);
      self.AsBinary.CopyFrom(aSrcDataObj.AsBinary, aSrcDataObj.AsBinary.Size);
      aSrcDataObj.AsBinary.Position := lPosition;   // reset the stream back to where it was when we started here.
    end;

    cDataTypeObject: begin
      // FINISH.  Not sure how to do this yet.  Do we have the new copy point to the same object instance or are we cloning the object instance.

    end;

    cDataTypeTag: begin
      self.AsTag.TagValue := aSrcDataObj.AsTag.TagValue;
      self.AsTag.DataObj.CopyFrom(aSrcDataObj.AsTag.DataObj);
    end;
  end;
end;




destructor TDataObj.Destroy;
begin
  Clear;
  inherited;
end;

function TDataObj.getAsArray: TDataArray;
var
  i: Integer;

  // used for temp moving
  lDataString: string;
  lDataObject: TObject;
  lDataInt64: Int64;
  lDataType: TDataType;
  lNewSlot: TDataObj;
begin
  case fDataType.Code of
    cDataTypeArray: begin
      result:=fStore.dataArray;
    end;
    cDataTypeFrame: begin
      // we can convert a frame to an array by simply using the natural order of the frame slots.
      result := TDataArray.Create;
      for i := 0 to fStore.dataFrame.Count-1 do
      begin
        // instead of doing copy operations from the original collection of TDataObjs, we can leave them in place and take those objects off the original collection and put them on the new collection.
        result.Add(fStore.dataFrame.Slots[i]);           // add the old TDataObj item to the new collection
        fStore.dataFrame.fSlotList.Objects[i] := nil;    // set it to nil so that when we free the TDataFrame, it doesn't free the TDataObj item.
      end;
      setAsArray(result);                                // this will free the old TDataFrame and get this object to now be holding the new TDataArray.
    end;
    cDataTypeSparseArray: begin
      // we can convert a sparse array to a regular array by simply taking the items in their natural order and applying them to the array, of course we loose the original's Index value.
      result := TDataArray.Create;
      for i := 0 to fStore.dataSparseArray.Count-1 do
      begin
        // instead of doing copy operations from the original collection of TDataObjs, we can leave them in place and take those objects off the original collection and put them on the new collection.
        result.Add(fStore.dataSparseArray.Items[i]);     // add the old TDataObj item to the new collection
        fStore.dataSparseArray.Items[i] := nil;          // set it to nil so that when we free the TDataArray, it doesn't free the TDataObj item.
      end;
      setAsArray(result);            // this will free the old TDataSparseArray and get this object to now be holding the new TDataArray.
    end;
    cDataTypeStringList: begin
      // we can convert a stringList into an array of strings.
      result := TDataArray.Create;
      for i := 0 to fStore.DataStringList.Count-1 do
      begin
        // instead of doing copy operations from the original collection of TDataObjs, we can leave them in place and take those objects off the original collection and put them on the new collection.
        result.NewSlot.AsString := fStore.DataStringList[i];
      end;
      setAsArray(result);            // this will free the old TDataStringList and get this object to now be holding the new TDataArray.
    end;
  else
    // there's no data that can possibly be converted so set as a new empty TDataArray
    if fDataType.Code = cDataTypeNull then
    begin
      result := TDataArray.Create;
      setAsArray(result);
    end
    else
    begin
      // there is some kind of data in this object now, so we are going to wrap this data in an array so that the original data is the first item in the array.
      // we are going to "detach" the data in the store from the current object and move it over to the first item that is being placed in the array.
      lDataString := fStore.fDataString;
      lDataObject := fStore.fDataObject;
      lDataInt64 := fStore.fDataInt64;
      lDataType := self.DataType;
      fStore.fDataObject := nil;   // done so that the .clear call that happens inside the setAsArray will not free the object that we now put into lDataObject as a temporary reference.

      // now switch this object over to being an array which will do a clear on it and make it an empty array
      result := TDataArray.Create;
      setAsArray(result);

      // Create the first item in the new array and give it the data its supposed to hold.
      lNewSlot := Result.NewSlot;
      lNewSlot.fStore.fDataString := lDataString;
      lNewSlot.fStore.fDataObject := lDataObject;
      lNewSlot.fStore.fDataInt64 := lDataInt64;
      lNewSlot.DataType := lDataType;
    end;
  end;
end;

function TDataObj.getAsBinary: TDataBinary;
begin
  case fDataType.Code of
    cDataTypeBinary: result:=fStore.dataBinary;
    // FINISH - could we possibly convert strings and stringList to a binary stream here?  What about encoding?  UTF-8?
  else
    // there's no data that can possibly be converted so set as a new empty TDataBinary
    result := TDataBinary.Create;
    setAsBinary(result);
  end;
end;

function TDataObj.getAsBoolean: Boolean;

  function LocalStrToBool(aStr: string): boolean;     // default is false.
  const
    cTrueValues: array[0..4] of char = ('T', 't', 'Y', 'y', '1');
  var
    i: integer;
  begin
    result := false;
    for i := 0 to high(cTrueValues) do
    begin
      if aStr[1] = cTrueValues[i] then
      begin
        result := true;
        break;
      end;
    end;
  end;

begin
  case fDataType.Code of
    cDataTypeNull: result := false;
    cDataTypeBoolean: result := fDataType.SubClass <> 0;
    cDataTypeByte: result := fStore.fDataByte <> 0;
    cDataTypeInt32: result := fStore.fDataInt32 <> 0;
    cDataTypeInt64, cDataTypeUTCDateTime: result := fStore.fDataInt64 <> 0;
    cDataTypeSingle: result := fStore.fDataSingle <> 0;
    cDataTypeDouble: result := fStore.fDataDouble <> 0;
//    cDataTypeDecimal128 = 9;
    cDataTypeDateTime, cDataTypeDate, cDataTypeTime: result := fStore.fDataDateTime <> 0;
    cDataTypeString: result := LocalStrToBool(fStore.DataString);
    cDataTypeStringList: result := LocalStrToBool(fStore.dataStringList.text);
  else
    result := false;
  end;
end;

function TDataObj.getAsByte: Byte;
begin
  case fDataType.Code of
    cDataTypeByte: Result:=fStore.fDataByte;
    cDataTypeInt32: Result := fStore.fDataInt32;
    cDataTypeInt64: Result := fStore.fDataInt64;
    cDataTypeBoolean: Result := fDataType.SubClass;
//    cDataTypeBooleanTrue: Result := 1;
    cDataTypeString: Result:=Byte(getAsInt32);  // possible truncating
  else
    Result:=0;    // all other types are not convertable to a Byte
  end;
end;

function TDataObj.getAsDate: TDate;
begin
  result := Trunc(getAsDateTime);
end;

function TDataObj.getAsDateTime: TDateTime;

  function TryConvertingStrToDateTime(aStr: string): TDatetime;   // this local function will try to convert string representations of a date into a TDatetime use a variety of possibilities.
  begin
    result := StrToDateTimeDef(fStore.DataString, TDateTime(0));    // first, try a normal system local orientated dateTime string
    if result = 0 then
    begin
      try
        result := ISO8601ToDate(fStore.DataString);      // Now try an ISODate string format   Example:  "2021-01-27T00:19:08.862Z"
      except
        result := 0;
      end;

      if result = 0 then
      begin
        try
          result := HttpToDate(fStore.DataString);      // now try a HTTP Date: Example: "Apr 9 00:00:00 2015 GMT"
        except
          result := 0;
        end;
      end;
    end;
  end;
begin
  case fDataType.Code of
    cDataTypeInt32: result := fStore.fDataInt32;
    cDataTypeInt64: result := fStore.fDataInt64;
    cDataTypeDouble: result:=fStore.fDataDouble;    // convert double to tdateTime which is essentially a double.
    cDataTypeSingle: result:=fStore.fDataSingle;    // convert single to tdateTime which is essentially a double.
    cDataTypeDateTime, cDataTypeDate, cDataTypeTime: result:=fStore.fDataDateTime;
    cDataTypeUTCDateTime: result := UnixToDateTime(fStore.fDataInt64);
    cDataTypeString: result := TryConvertingStrToDateTime(fStore.DataString);
    cDataTypeStringList: result := TryConvertingStrToDateTime(fStore.DataStringList.text);
  else
    result:=0;
  end;
end;

function TDataObj.getAsDouble: Double;
begin
  case fDataType.Code of
    cDataTypeByte: Result := fStore.fDataByte;
    cDataTypeInt32: Result := fStore.fDataInt32;
    cDataTypeInt64: Result := fStore.fDataInt64;
    cDataTypeBoolean: Result := fDataType.SubClass;
    cDataTypeString:
    begin //check for a normal floating point number in the string
      if not TryStrToFloat(getAsString, result) then
        result := 0;
    end;
    cDataTypeSingle: Result:=fStore.fDataSingle;
    cDataTypeDouble: Result:=fStore.fDataDouble;
  else
    Result:=0;    // all other types are not convertable to a Byte
  end;
end;


{$ifDef cMakeMoreCompatibleWithOldDataObjects}
function TDataObj.getAsFloat: Double;
begin
  result := getAsDouble;
end;
{$endif}

function TDataObj.getAsFrame: TDataFrame;
var
  i: Integer;
begin
  case fDataType.Code of
    cDataTypeFrame: begin
      result:=fStore.DataFrame;
    end;
    cDataTypeArray: begin
      // we can convert an array to a frame by simply making each slotname be a string representation of the index values
      result := TDataFrame.Create;
      for i := 0 to fStore.DataArray.Count-1 do
      begin
        // instead of doing copy operations from the original collection of TDataObjs, we can leave them in place and take those objects off the original collection and put them on the new collection.
        result.AppendSlot(intToStr(i), fStore.DataArray.Items[i]);     // add the old TDataObj item to the new collection
        fStore.DataArray.Items[i] := nil;                              // set it to nil so that when we free the TDataArray, it doesn't free the TDataObj item.
      end;
      setAsFrame(result);            // this will free the old TDataArray and get this object to now be holding the new TDataFrame.
    end;
    cDataTypeSparseArray: begin
      // we can convert a sparse array to a frame by simply making each slotname be a string representation of the index values
      result := TDataFrame.Create;
      for i := 0 to fStore.DataSparseArray.Count-1 do
      begin
        // instead of doing copy operations from the original collection of TDataObjs, we can leave them in place and take those objects off the original collection and put them on the new collection.
        result.AppendSlot(IntToStr(fStore.DataSparseArray.SlotIndex(i)), fStore.DataSparseArray.Items[i]);     // add the old TDataObj item to the new collection
        fStore.DataSparseArray.Items[i] := nil;                                                               // set it to nil so that when we free the TDataSparseArray, it doesn't free the TDataObj item.
      end;
      setAsFrame(result);            // this will free the old TDataArray and get this object to now be holding the new TDataFrame.
    end;
  else
    // there's no data that can possibly be done so covert to a new empty TDataFrame
    result := TDataFrame.Create;
    setAsframe(result);
  end; // case
end;

function TDataObj.getAsGuid: TDataGUID;
begin
  case fDataType.Code of
    cDataTypeGUID: Result:=fStore.DataGUID;
    cDataTypeString: begin
      // try to convert a string of the format:  '{41E3188A-B2EE-4FDA-9D4C-4929CFFE3B6D}' to a TDataGUID.
      result := TDataGUID.Create;
      result.AsString := getAsString;   // this will internally handle bad formatting strings and put it as an empty GUID if it's bad.
      setAsGUID(result);
    end;
//    cDataTypeBinary:  FINISH - maybe someday, we can convert from binary to a TDataGUID if it's a perfect 16 bytes in length.
  else
    result := TDataGUID.Create;
    setAsGUID(result);
  end;
end;

function TDataObj.getAsInt32: integer;
begin
  case fDataType.Code of
    cDataTypeByte: Result:=fStore.fDataByte;
    cDataTypeInt32: Result := fStore.fDataInt32;
    cDataTypeInt64: Result := fStore.fDataInt64;
    cDataTypeBoolean: Result := fDataType.SubClass;
    cDataTypeString: Result:=StrToIntDef(fStore.DataString,0);
    cDataTypeSingle: Result:=Round(fStore.fDataSingle);
    cDataTypeDouble: Result:=Round(fStore.fDataDouble);
  else
    Result:=0;    // all other types are not convertable to a Byte
  end;
end;

function TDataObj.getAsInt64: Int64;
begin
  case fDataType.Code of
    cDataTypeByte: Result:=fStore.fDataByte;
    cDataTypeInt32: Result := fStore.fDataInt32;
    cDataTypeInt64: Result := fStore.fDataInt64;
    cDataTypeBoolean: Result := fDataType.SubClass;
    cDataTypeString: Result:=StrToIntDef(fStore.DataString,0);
    cDataTypeSingle: Result:=Round(fStore.fDataSingle);
    cDataTypeDouble: Result:=Round(fStore.fDataDouble);
  else
    Result:=0;    // all other types are not convertable to a Byte
  end;
end;


function TDataObj.getAsObject: TObject;
begin
  case fDataType.Code of
    cDataTypeObject: result := fStore.fDataObject;     // could also possibly return nil.
  else
    result := nil;                                    // no possible conversions
  end;
end;

function TDataObj.getAsObjectID: TDataObjectID;
begin
  case fDataType.Code of
    cDataTypeObjectID: result := fStore.DataObjectID
  else
    result := TDataObjectID.Create;
    setAsObjectID(result);
  end;
end;

function TDataObj.getAsSingle: Single;
begin
  case fDataType.Code of
    cDataTypeByte: Result:=fStore.fDataByte;
    cDataTypeInt32: Result := fStore.fDataInt32;
    cDataTypeInt64: Result := fStore.fDataInt64;
    cDataTypeBoolean: Result := fDataType.SubClass;
    cDataTypeString:
    begin //check for a normal floating point number in the string
      if not TryStrToFloat(getAsString, result) then
         result := 0;
    end;
    cDataTypeSingle: Result:=fStore.fDataSingle;
    cDataTypeDouble: Result:=fStore.fDataDouble;
  else
    Result:=0;    // all other types are not convertable to a Byte
  end;
end;

function TDataObj.getAsSparseArray: TDataSparseArray;
var
  i: integer;
begin
  case fDataType.Code of
    cDataTypeSparseArray: begin
      result:=fStore.DataSparseArray;
    end;
    cDataTypeFrame: begin
      // we can convert a frame to a sparse array, but the slotnames will be lost and replaced with array indexes.  Will start with zero index.
      result := TDataSparseArray.Create;
      for i := 0 to fStore.DataFrame.fSlotList.Count-1 do
      begin
        // instead of doing copy operations from the original collection of TDataObjs, we can leave them in place and take those objects off the original collection and put them on the new collection.
        // NOTE:  should we try to convert the frame's slotnames to an integer or just use the natural order.
        // For now, decided to use the natural order cause it's the reliable way.
        result.AppendSlot(i, fStore.DataFrame.Slots[i]);        // add the old TDataObj item to the new collection
        fStore.DataFrame.fSlotList.Objects[i] := nil;           // set it to nil so that when we free the TDataFrame, it doesn't free the TDataObj item.
      end;
      setAsSparseArray(result);                                 // this will free the old TDataFrame and get this object to now be holding the new TDataFrame.
    end;
    cDataTypeArray: begin
      // we can convert an array to a sparse array by simply taking the items in their natural order and applying them to the array
      result := TDataSparseArray.Create;
      for i := 0 to fStore.DataArray.Count-1 do
      begin
        // instead of doing copy operations from the original collection of TDataObjs, we can leave them in place and take those objects off the original collection and put them on the new collection.
        result.AppendSlot(i, fStore.DataArray.Items[i]);             // add the old TDataObj item to the new collection
        fStore.DataArray.Items[i] := nil;  // set it to nil so that when we free the TDataArray, it doesn't free the TDataObj item.
      end;
      setAsSparseArray(result);            // this will free the old TDataArray and get this object to now be holding the new TDataFrame.
    end;
  else
    // there's no data that can possibly be converted so set as a new empty TDataSparseArray
    result := TDataSparseArray.Create;
    setAsSparseArray(result);
  end;
end;

function TDataObj.getAsString: String;
var
  lDateTime: TDateTime;
begin

  case fDataType.Code of
    cDataTypeNull: Result := '';
    cDataTypeBoolean: if fDataType.SubClass<>0 then Result := cTrueStr else Result := cFalseStr;
    cDataTypeByte: Result := IntToStr(fStore.fDataByte);
    cDataTypeInt32: result := IntToStr(fStore.fDataInt32);
    cDataTypeInt64: result := IntToStr(fStore.fDataInt64);
    cDataTypeSingle: Result := FloatToStr(fStore.fDataSingle);
    cDataTypeDouble: Result := FloatToStr(fStore.fDataDouble);
//    cDataTypeDecimal128 = 9;  // Reserved to be compatible with BSON, but probably not going to implement this data type.
    cDataTypeDateTime: begin
      lDateTime:=fStore.fDataDateTime;
      if lDateTime=0 then
        Result:=''         // is this really what we should do?  technically, this is a valid dateTime
      else
        Result:=DateTimeToStr(lDateTime);
    end;
    cDataTypeUTCDateTime: begin
      result := DateTimetoStr(UnixToDateTime(fStore.fDataInt64));
    end;
    cDataTypeDate: begin
      lDateTime:=fStore.fDataDateTime;
      if lDateTime=0 then
        Result:=''
      else
        Result:=DateToStr(lDateTime);
    end;
    cDataTypeTime: begin
      lDateTime:=fStore.fDataTime;
      if lDateTime=0 then
        Result:=''
      else
        Result:=TimeToStr(lDateTime);
    end;
    cDataTypeGUID: result := fStore.DataGUID.GUID.ToString;
    cDataTypeObjectID: result := fStore.DataObjectID.AsString;
    cDataTypeString: Result := fStore.DataString;
    cDataTypeStringList: result := fStore.DataStringList.Text;
  else
    Result:='';
  end;
end;

function TDataObj.getAsStringList: TDataStringList;
var
  lStringList: TDataStringList;
begin
  case fDataType.Code of
    cDataTypeStringList: Result:=fStore.DataStringList;    // we are a stringList now so just return it.
  else
    // Any other type, try to convert the existing string string representation of it to a stringList
    lStringList := TDataStringList.Create;
    lStringList.Text := getAsString;
    setAsStringList(lStringList);
    result := lStringList;
  end;
end;

function TDataObj.getAsSymbol: String;
begin
  result := getAsString;
end;

function TDataObj.getAsTag: TDataTag;
begin
  // the getAsTag call will return the TDataTag if this dataobject is holding a TDataTag ALREADY.  If this data object is not holding this type, but is instead
  // holding a different type, then this call will create a tag in this dataObject and whatever this dataObject used to hold will be put into the TagObject that this dataObject now holds.
  if fDataType.Code = cDataTypeTag then
  begin
    result := fStore.DataTag;
  end
  else
  begin
    result := TDataTag.Create;             // NOTE:  THIS CODE IS NOT TESTED YET.
    result.DataObj.fStore := fStore;      // copying the stored contents of whatever was in self DataObj to the new DataObj that is owned by the new tag we just created.   Basically, transferring that data from self to the new TDataTag.
    fStore.DataTag := result;              // our store now hold our new TDataTag when then contains the contents of what this objects store used to have.
    fDataType.Code := cDataTypeTag;       // converting this dataObject to now hold a TDataTag and it also doesn't hold any attributes either, that's why we are calling value instead of code.
  end;
end;

function TDataObj.getAsTime: TTime;  // TTime is a fraction of a day.   should be 0.38487294, etc.
var
  lDateTime: TDateTime;
begin
  lDateTime := getAsDateTime;
  result := lDateTime - Trunc(lDateTime);   // just return the time portion.
end;

function TDataObj.getAsUTCDateTime: int64;
begin
  case fDataType.Code of
    cDataTypeInt64, cDataTypeUTCDateTime: result := fStore.fDataInt64;
    cDataTypeDateTime, cDataTypeDate, cDataTypeTime: begin
      result := DateTimeToUnix(fStore.fDataDateTime);
    end;
  else
    Result:=0;
  end;
end;

function TDataObj.getDataType: TDataType;
begin
  result := fDataType;
end;

function TDataObj.getDataTypeString: String;
begin
  if fDataType.Code = cDataTypeString then
  begin
    if fDataType.SubClass = cSubCodeSymbol then
      result := 'Symbol'
    else
      result := cDataTypeStrings[ord(fDataType.Code)];   // 'String'
  end
  else if fDataType.Code = cDataTypeObject then
  begin
    if assigned(fStore.fDataObject) then
      result := cDataTypeStrings[ord(fDataType.Code)] + ':'+fStore.fDataObject.ClassName
    else
      result := cDataTypeStrings[ord(fDataType.Code)] + ':nil';
  end
  else
  begin
    if fDataType.Code <= high(TDataTypeCode) then
    begin
      result := cDataTypeStrings[ord(fDataType.Code)];
    end
    else
    begin
      result := 'OutOfRange: '+intToStr(ord(fDataType.Code));
    end;

  end;
end;

function TDataObj.getHasAttributes: boolean;
begin
  result := assigned(fAttributes);
end;

function TDataObj.getItem(aKey: variant): TDataObj;
var
  lVT: word;
begin
  lVT := VarType(akey) and varTypeMask;
  case lVT of
    varSmallint, //  16-bit signed integer (type Smallint in Delphi, short in C++).
    varInteger,  //  32-bit signed integer (type Integer in Delphi, int in C++).
    varShortInt, //  8-bit signed integer (type ShortInt in Delphi or signed char in C++).
    varByte, //  A Byte.
    varWord, //    Unsigned 16-bit value (Word).
    varLongWord, //  Unsigned 32-bit value (type LongWord in Delphi or unsigned long in C++).
    varInt64: //  64-bit signed integer (Int64 in Delphi or __int64 in C++).
    begin
      // we are given a number so treat this data object as an array or a frame or a sparseArray.
      if self.DataType.Code = cDataTypeFrame then
      begin
        // this object is already a frame so try to access the "nth" item in the frame.
        result := self.AsFrame.getSlot(aKey);
      end
      else if self.DataType.Code = cDataTypeSparseArray then
      begin
        result := self.AsSparseArray.getSlot(aKey);
      end
      else
      begin
        // we are given a number so treat this data object as an array and return the Nth item.
        // Note that we could be converting from a different data type here to the Array data type.
        // f the array doesn't have the Nth item then we raise an invalid index exception
        result := self.AsArray.Slots[aKey];
      end;
    end;

    varOleStr, //  Reference to a dynamically allocated UNICODE string.
    varStrArg, //  COM-compatible string.
    varString, //  Reference to a dynamically allocated string (not COM-compatible).
    varUString: // Unicode string
    begin
      // we are given a string for the key so treat this data object as a frame and return the slot or make a new slot with this key name.
      result := Self.AsFrame.NewSlot(aKey);
    end;

    else begin
      // aKey was given to us with a data type that we can't make use of.  so generate an exception.
      raise Exception.Create('Invalid Key data type on call to TDataObj.Item');
    end;
  end;
end;

function TDataObj.DataTypeCanHaveAChildObject: boolean;
begin
  result := DataTypeIsAContainer or (fDataType.Code = cDataTypeTag);
end;

function TDataObj.DataTypeIsAContainer: boolean;
begin
  result := (fDataType.code=cDataTypeFrame) or (fDataType.code=cDataTypeArray) or
            (fDataType.code=cDataTypeSparseArray);
end;

function TDataObj.getStore: PTDataStore;
begin
  //NOTE:  This should be the ONLY method that can ever access fStore.   Everywhere else should call this function to get it.
  result := @fStore;
end;

// If you pass in nil for aStreamer then this function will try to find the right streamer class by the filename extension and aStreamer will be populated with that object.
// NOTE: this function could potentially return a Streamer object, if so then the caller must free it.  It could return nil
// This function will try to load this TDataObj by reading the file using one of the possible registered streamers.
// Many different exceptions could be raised here, but if any exception is raised when reading data from a file, the object
// will be populated with whatever data it was successful loading before the exception.
// The caller must free the returned streamer object that was created if the caller did not pass in aStreamer
function TDataObj.LoadFromFile(aFilename: string; aStreamer: TDataObjStreamerBase; aOptionalParameters: string=''): TDataObjStreamerBase;
var
  lStreamerClass: TDataObjStreamerClass;
//  lStreamer: TDataObjStreamerBase;
  lFS: TFileStream;
  lRS: TStreamReadCache;
begin
  result := nil;
  lFS := TFileStream.Create(aFilename, fmOpenRead + fmShareDenyNone);
  try
    lRS := TStreamReadCache.Create(lFS);
    try
      if assigned(aStreamer) then
      begin
        aStreamer.Stream := lRS;
        result := aStreamer;
      end
      else
      begin
        // Find the right streamer based on the filename extension if one was not given to us.
        lStreamerClass := gStreamerRegistry.FindStreamerClassByFilename(aFilename);      // It is possible that this returns nil if one can't be found.
        if assigned(lStreamerClass) then
        begin
          result := lStreamerClass.Create(lRS);
        end
        else
        begin
         // FINISH - If nothing concrete was found, then attempt to load from each of the streamers and if one of them doesn't except out, then maybe it was successful loading
         // For now, we are returning nil to signal that we could not get a streamer to do the load.
          //raise(exception.Create(format('%s is not a format that can be loaded into a dataObject.',[aFilename])));
        end;
      end;
      if assigned(result) then
      begin
        result.ApplyOptionalParameters(aOptionalParameters);
        result.Decode(self);
      end;
    finally
      lRS.free;
    end;
  finally
    lFS.Free;
  end;
end;


procedure TDataObj.LoadFromFile(aFilename: string; aOptionalParams: string='');
var
  lStreamer: TDataObjStreamerBase;
begin
  lStreamer := nil;
  try
    lStreamer := LoadFromFile(aFileName, nil, aOptionalParams);
  finally
    lStreamer.Free;
  end;
end;

function TDataObj.PrintToString: string;
var
  lSB: TStringBuilder;
begin
  lSB:=TStringBuilder.Create;
  try
    PrintToStringBuilder(lSB);
    result := lSB.ToString;
  finally
    lSB.Free;
  end;
end;

// At first glance, this looks like it produces JSON, but it is NOT JSON, it's just a way to easily show a string representation of the data to a human.  Don't use it for transmitting data.
procedure TDataObj.PrintToStringBuilder(aStringBuilder: TStringBuilder; aIndent: integer = 0);
var
  i: Integer;
  lSpaces: string;
begin
  case fDataType.Code of
    cDataTypeNull: aStringBuilder.AppendLine('nil');
    cDataTypeBoolean: if fDataType.SubClass <> 0 then
                        aStringBuilder.AppendLine(cTrueStr)
                      else
                        aStringBuilder.AppendLine(cFalseStr);
    cDataTypeByte: aStringBuilder.Append(fStore.fDataByte).AppendLine;
    cDataTypeInt32: aStringBuilder.Append(fStore.fDataInt32).AppendLine;
    cDataTypeInt64: aStringBuilder.Append(fStore.fDataInt64).AppendLine;
    cDataTypeSingle: aStringBuilder.Append(fStore.fDataSingle).AppendLine;
    cDataTypeDouble: aStringBuilder.Append(fStore.fDataDouble).AppendLine;
//    cDataTypeDecimal128: aStringBuilder.Append(fStore.fDataInt64);
    cDataTypeDateTime: aStringBuilder.AppendLine(DateTimeToStr(fStore.fDataDateTime));
    cDataTypeUTCDateTime: aStringBuilder.AppendLine(DateTimeToStr(UnixToDateTime(fStore.fDataInt64)));
    cDataTypeDate: aStringBuilder.AppendLine(DateToStr(fStore.fDataDateTime));
    cDataTypeTime: aStringBuilder.AppendLine(TimeToStr(fStore.fDataDateTime));
    cDataTypeGUID: aStringBuilder.AppendLine(fStore.DataGUID.AsString);
    cDataTypeObjectID: aStringBuilder.AppendLine(fStore.DataObjectID.AsString);
    cDataTypeString: aStringBuilder.AppendLine('"'+fStore.DataString+'"');

    cDataTypeStringList: begin
      aStringBuilder.AppendLine('[');
      aIndent := aIndent+2;
      lSpaces := StringOfChar(' ',aIndent);
      for i := 0 to fStore.DataStringList.Count-1 do
      begin
        if i<fStore.DataStringList.Count-1 then
          aStringBuilder.AppendLine(lSpaces+'`'+fStore.DataStringList.Strings[i]+'`,')
        else
          aStringBuilder.AppendLine(lSpaces+'`'+fStore.DataStringList.Strings[i]+'`');
      end;
      lSpaces := StringOfChar(' ',aIndent-2);
      aStringBuilder.AppendLine(lSpaces+']');
    end;

    cDataTypeFrame: begin
      aStringBuilder.Append('{ ');
      lSpaces := '';
      aIndent := aIndent+2;
      for i := 0 to fStore.DataFrame.Count-1 do
      begin
        if i=1 then
          lSpaces := StringOfChar(' ',aIndent);
        aStringBuilder.Append(lSpaces+fStore.DataFrame.slotName(i)+': ');
        fStore.DataFrame.Slots[i].PrintToStringBuilder(aStringBuilder, aIndent);
      end;
      lSpaces := StringOfChar(' ',aIndent-2);
      aStringBuilder.AppendLine(lSpaces+'}');
    end;

    cDataTypeArray: begin
      aStringBuilder.Append('[ ');
      lSpaces := '';
      aIndent := aIndent+2;
      for i := 0 to fStore.DataArray.Count-1 do
      begin
        if i=1 then
          lSpaces := StringOfChar(' ',aIndent);
        aStringBuilder.Append(lSpaces+IntToStr(i)+': ');
        fStore.DataArray.items[i].PrintToStringBuilder(aStringBuilder, aIndent);
      end;
      lSpaces := StringOfChar(' ',aIndent-2);
      aStringBuilder.AppendLine(lSpaces+']');
    end;

    cDataTypeSparseArray: begin
      aStringBuilder.Append('[ ');
      lSpaces := '';
      aIndent := aIndent+2;
      for i := 0 to fStore.DataSparseArray.Count-1 do
      begin
        if i=1 then
          lSpaces := StringOfChar(' ',aIndent);
        aStringBuilder.Append(lSpaces+'0:');
        fStore.DataSparseArray.items[i].PrintToStringBuilder(aStringBuilder, aIndent);
      end;
      lSpaces := StringOfChar(' ',aIndent-2);
      aStringBuilder.AppendLine(lSpaces+']');
    end;

    cDataTypeBinary: begin
      aStringBuilder.AppendFormat('<binary size=%d>',[fStore.DataBinary.Size]).AppendLine;
    end;

    cDataTypeObject: begin
      aStringBuilder.AppendFormat('<TObject classname=%s>',[fStore.fDataObject.ClassName]).AppendLine;
    end;

    cDataTypeTag: begin
      aStringBuilder.Append(IntToStr(fStore.DataTag.TagValue)+'(');
      lSpaces := '';
      aIndent := aIndent+2;
      fStore.DataTag.DataObj.PrintToStringBuilder(aStringBuilder, aIndent);
      lSpaces := StringOfChar(' ',aIndent-2);
      aStringBuilder.AppendLine(lSpaces+')');
    end;
  end;
end;

{$ifDef cMakeMoreCompatibleWithOldDataObjects}
procedure TDataObj.ReadFromFile(aFilename: string);
begin
  LoadFromFile(aFilename);
end;
{$endif}

procedure TDataObj.ReadFromMemoryBuffer(aBuffer: Pointer; aSize: NativeInt; aStreamerClass: TClass);
var
  lStream: TPointerStream;
begin
  lStream:=TPointerStream.create(aBuffer, aSize, true);
  try
    ReadFromStream(lStream, aStreamerClass);
  finally
    lStream.Free;
  end;
end;

procedure TDataObj.ReadFromStream(aStream: TStream; aStreamerClass: TClass = nil);
var
  lStreamer: TDataObjStreamerBase;
  lClass: TDataObjStreamerClass;
begin
  lClass := nil;
  if assigned(aStreamerClass) then
  begin
    if not aStreamerClass.InheritsFrom(TDataObjStreamerBase) then    // just a safety check that should never happen.
    begin
      raise Exception.Create('Attempted to read a dataObject from a stream using class '+aStreamerClass.ClassName+' but this class is not a TDataObjStreamerBase descendant');
    end;
    lClass := TDataObjStreamerClass(aStreamerClass);
  end;

  if not assigned(lClass) then
    lClass := TDataObjStreamer;

  lStreamer := lClass.Create(aStream);
  try
    lStreamer.Decode(self);
  finally
    lStreamer.free;
  end;
end;


procedure TDataObj.setAsArray(const aValue: TDataArray);
begin
  ClearData;    // Note that this clears any existing data but it leaves attributes intact.
  fDataType.code:=cDataTypeArray;
  FStore.dataArray:=aValue;
end;

procedure TDataObj.setAsBinary(const aValue: TDataBinary);
begin
  ClearData;    // Note that this clears any existing data but it leaves attributes intact.
  fDataType.code:=cDataTypeBinary;
  FStore.dataBinary:=aValue;
end;

procedure TDataObj.setAsBoolean(const aValue: Boolean);
begin
  ClearData;
  fDataType.Code := cDataTypeBoolean;
  if aValue then fDataType.SubClass := 1 else fDataType.SubClass := 0;
end;

procedure TDataObj.setAsByte(const aValue: Byte);
begin
  ClearData;    // does not clear attributes if there are any.
  fDataType.Code := cDataTypeByte;
  FStore.fDataByte := aValue;
end;

procedure TDataObj.setAsDate(const aValue: TDate);
begin
  ClearData;
  fDataType.Code:=cDataTypeDate;
  FStore.fDataDateTime:=aValue;
end;

procedure TDataObj.setAsDateTime(const aValue: TDateTime);
begin
  ClearData;
  fDataType.Code:=cDataTypeDateTime;
  FStore.fDataDateTime:=aValue;
end;

procedure TDataObj.setAsDouble(const aValue: Double);
begin
  ClearData;
  fDataType.Code:=cDataTypeDouble;
  FStore.fDataDouble:=aValue;
end;

{$ifDef cMakeMoreCompatibleWithOldDataObjects}
procedure TDataObj.setAsFloat(const aValue: Double);
begin
  SetAsDouble(aValue);
end;
{$endif}

procedure TDataObj.setAsFrame(const aValue: TDataFrame);
begin
  ClearData;    // Note that this clears any existing data but it leaves attributes intact.
  fDataType.code:=cDataTypeFrame;
  FStore.dataFrame:=aValue;
end;

procedure TDataObj.setAsGuid(const aValue: TDataGUID);
begin
  ClearData;    // Note that this clears any existing data but it leaves attributes intact.
  fDataType.code:=cDataTypeGUID;
  FStore.dataGUID:=aValue;
end;

procedure TDataObj.setAsInt32(const aValue: integer);
begin
  ClearData;
  fDataType.Code:=cDataTypeInt32;
  FStore.fDataInt32:=aValue;
end;

procedure TDataObj.setAsInt64(const aValue: Int64);
begin
  ClearData;
  fDataType.Code:=cDataTypeInt64;
  FStore.fDataInt64:=aValue;
end;

procedure TDataObj.setAsObject(const aValue: TObject);
begin
  ClearData;    // Note that this clears any existing data but it leaves attributes intact.
  fDataType.code:=cDataTypeObject;
  FStore.dataObject:=aValue;               // note that this is a reference to the TObject passed in aValue.  We do not own it.
end;

procedure TDataObj.setAsObjectID(const aValue: TDataObjectID);
begin
  ClearData;    // Note that this clears any existing data but it leaves attributes intact.
  fDataType.code:=cDataTypeObjectID;
  FStore.dataObjectID:=aValue;
end;

procedure TDataObj.setAsSingle(const aValue: Single);
begin
  ClearData;    // does not clear attributes if there are any.
  fDataType.Code := cDataTypeSingle;
  FStore.fDataSingle := aValue;
end;

procedure TDataObj.setAsSparseArray(const aValue: TDataSparseArray);
begin
  ClearData;    // Note that this clears any existing data but it leaves attributes intact.
  fDataType.code:=cDataTypeSparseArray;
  FStore.dataSparseArray:=aValue;
end;

procedure TDataObj.setAsString(const aValue: String);
begin
  ClearData;    // does not clear attributes if there are any.
  fDataType.Code := cDataTypeString;
  fDataType.SubClass := cSubCodeGeneric;
  FStore.DataString := aValue;
end;

procedure TDataObj.setAsStringList(const aValue: TDataStringList);    // Note, this takes over ownership of aValue;
begin
  ClearData;    // Note that this clears any existing data but it leaves attributes intact.
  fDataType.code:=cDataTypeStringList;
  FStore.dataStringList:=aValue;
end;

procedure TDataObj.setAsSymbol(const aValue: String);
begin
  ClearData;    // does not clear attributes if there are any.
  fDataType.Code:= cDataTypeString;
  fDataType.SubClass := cSubCodeSymbol;
  FStore.DataString := aValue;
end;

procedure TDataObj.setAsTime(const aValue: TTime);
begin
  ClearData;    // does not clear attributes if there are any.
  fDataType.Code:= cDataTypeTime;
  fDataType.SubClass := cSubCodeGeneric;
  FStore.fDataDateTime := aValue;
end;

procedure TDataObj.setAsUTCDateTime(const aValue: Int64);
begin
  ClearData;
  fDataType.Code := cDataTypeUTCDateTime;
  fDataType.SubClass := cSubCodeGeneric;
  fStore.fDataInt64 := aValue;    //The int64 is UTC milliseconds since the Unix epoch. January 1, 1970.  See DateTimeToUnix(dt);
end;

procedure TDataObj.SetDataTypeParts(aCode: TDataTypeCode; aSubType: byte);
begin
  fDataType.Code := aCode;
  fDataType.SubClass := aSubType;
end;

procedure TDataObj.setHasAttributes(const Value: boolean);
begin
  if Value then
  begin
    if not assigned(fAttributes) then
    begin
      fAttributes := TDataAttributeStore.Create;
    end;
  end
  else
  begin
    FreeAndNil(fAttributes);          // if we had attributes, they are gone now.
  end;
end;

procedure TDataObj.setDataType(const Value: TDataType);
begin
  fDataType := Value;
end;


procedure TDataObj.WriteToFile(aFilename: string; aStreamerClass: TDataObjStreamerClass = nil);
var
  lStreamer: TDataObjStreamerBase;
begin
  // First, see if the streamer passed in is usable
  lStreamer := nil;
  if assigned(aStreamerClass) then
  begin
    if not aStreamerClass.InheritsFrom(TDataObjStreamerBase) then    // just a safety check that should never happen.
    begin
      raise Exception.Create('Attempted to write a dataObject to a stream using class '+aStreamerClass.ClassName+' but this class is not a TDataObjStreamerBase descendant');
    end;
    lStreamer := aStreamerClass.Create(nil);
  end;

  if not assigned(lStreamer) then
    lStreamer := gStreamerRegistry.CreateStreamerByFilename(aFilename);

  if assigned(lStreamer) then
  begin
    try
      WriteToFile(aFilename, lStreamer);
    finally
      lStreamer.Free;
    end;
  end
  else
  begin
    // If nothing concrete was found, then raise an exception.
    raise Exception.Create('Unable to create a streamer for file '+aFilename);
  end;
end;

procedure TDataObj.WriteToFile(aFilename: string; aStreamer: TDataObjStreamerBase);
var
  lFS: TFileStream;
  lWS: TStreamWriteCache;
begin
  if aStreamer=nil then
  begin
    WriteToFile(aFilename); // go choose a streamer that will then call back to WriteToFile.
  end
  else
  begin
    lFS := TFileStream.Create(aFilename, fmCreate);
    try
      lWS := TStreamWriteCache.Create(lFS);
      try
        aStreamer.Stream := lWS;
        aStreamer.Encode(self);
      finally
        lWS.free;
      end;
    finally
      lFS.Free;
    end;
  end;
end;

procedure TDataObj.WriteToStream(aStream: TStream; aStreamerClass: TClass = nil);
var
  lStreamer: TDataObjStreamerBase;
  lClass: TDataObjStreamerClass;
begin
  lClass := nil;
  if assigned(aStreamerClass) then
  begin
    if not aStreamerClass.InheritsFrom(TDataObjStreamerBase) then    // just a safety check that should never happen.
    begin
      raise Exception.Create('Attempted to write a dataObject to a stream using class '+aStreamerClass.ClassName+' but this class is not a TDataObjStreamerBase descendant');
    end;
    lClass := TDataObjStreamerClass(aStreamerClass);
  end;

  if not assigned(lClass) then
    lClass := TDataObjStreamer;    // This is our default.

  lStreamer := lClass.Create(aStream);
  try
    lStreamer.Encode(self);
  finally
    lStreamer.Free;
  end;
end;

{ TDataType }

procedure TDataType.setCode(const aValue: TDataTypeCode);
begin
  fCode := aValue;
end;

procedure TDataType.setSubClass(const aValue: byte);
begin
  fSubClass := aValue;
end;

{ TDataGUID }

function TDataGUID.getAsString: string;
begin
  result := GUIDToString(fGUID);
end;

procedure TDataGUID.setAsString(const aValue: string);
begin
  try
    fGUID := StringToGUID(aValue);
  except
    fGUID := TGUID.Empty;
  end;
end;

{ TDataFrame }

procedure TDataFrame.AppendSlot(const aSlotName: string; aDataObj: TDataObj);
begin
  // this will add a new slot as long as aSlotName is not already in this frame.  If it is, then the previous slot wil be removed first.
  DeleteSlot(aSlotName);
  fSlotList.AddObject(aSlotname, aDataObj);
end;

function TDataFrame.ChangeSlotName(aOldSlotName, aNewSlotName: string): Boolean;
var
  lOldSlotIndex, lNewSlotIndex: Integer;
begin
  lOldSlotIndex := FindSlotIndex(aOldSlotName);
  lNewSlotIndex := FindSlotIndex(aNewSlotName);
  if (lOldSlotIndex>=0) and (lNewSlotIndex<0) then
  begin
    SetSlotname(lOldSlotIndex, aNewSlotName);
    result := true;
  end
  else
    result := false;
end;

procedure TDataFrame.Clear;
begin
  fSlotList.Clear;   // will free all owned child objects.
end;

procedure TDataFrame.CopyFrom(aSource: TDataFrame);
var
  i: Integer;
begin
  for i := 0 to aSource.Count-1 do
  begin
    newSlot(aSource.Slotname(i)).CopyFrom(aSource.Slots[i]);
  end;
end;

function TDataFrame.Count: integer;
begin
  result := fSlotList.Count;
end;

constructor TDataFrame.Create;
begin
  inherited Create;
  fSlotList := TStringList.Create;
  fSlotList.OwnsObjects := true;
  fSlotList.Duplicates := dupError;     // duplicates should never happen cause we are controlling it.
end;

function TDataFrame.DeleteSlot(const aSlotName: string): boolean;   // returns true if the slot was found and deleted.
var
  lIndex: integer;
begin
  lIndex := fSlotList.IndexOf(aSlotname);
  result := Delete(lIndex);
end;

function TDataFrame.Delete(aIndex: integer): boolean;         // returns true if the slot was found and deleted.
begin
  if aIndex >= 0 then
  begin
    fSlotList.Delete(aIndex);
    result := true;
  end
  else
    result := false;
end;


destructor TDataFrame.Destroy;
begin
  Clear;
  fSlotList.Free;
  inherited Destroy;
end;

function TDataFrame.FindSlot(const aSlotName: string; var oSlot: TDataObj): boolean;
var
  lIndex: integer;
begin
  result := false;
  lIndex := FindSlotIndex(aSlotName);
  if lIndex >= 0 then
  begin
    oSlot := Slots[lIndex];
    result := true;
  end;
end;

function TDataFrame.FindSlot(const aSlotName: string): TDataObj;
var
  lIndex: integer;
begin
  result := nil;
  lIndex := FindSlotIndex(aSlotName);
  if lIndex >= 0 then
  begin
    result := Slots[lIndex];
  end;
end;

function TDataFrame.FindSlotIndex(const aSlotName: string): integer;    // returns -1 if not found
var
  I: Integer;
begin
  // Doing brute force scanning to find the slotname.  Turns out to be faster than something more sophistocated such as using a dictionary.
  // The reason is that there is overhead using a dictionary and in DataObjects, we treat slot names as case insensitive, so using a dictionary
  // anyway would force a bunch of case shifting. This is faster for around 50 and under slotnames in the frame.  Which, most likely, we will always
  // be below this level.  Maybe in the future, we will have some kind a slot count threshold that will shift to something other than a simple scan.
  result := -1;
  for I := 0 to fSlotList.Count-1 do
  begin
    if CompareText(aSlotName, fSlotList.Strings[i])=0 then
    begin
      result := i;
      break;
    end;
  end;
end;

function TDataFrame.GetEnumerator: TDataFrameEnumerator;
begin
  result := TDataFrameEnumerator.Create(self);
end;

function TDataFrame.GetItem(const aKey: string): TDataObj;
begin
  result := NewSlot(aKey);
end;

function TDataFrame.getSlot(aIndex: integer): TDataObj;
begin
  result := TDataobj(fSlotList.Objects[aIndex]);
end;

function TDataFrame.IndexOfChildSlot(aSlot: TDataObj): integer;
begin
  result := fSlotList.IndexOfObject(aSlot);
end;

function TDataFrame.NewSlot(const aSlotName: string; aRaiseExceptionIfAlreadyExists: boolean = false): TDataObj;
begin
  if not FindSlot(aSlotname, result) then
  begin
    result := TDataObj.Create;
    fSlotList.AddObject(aSlotName, result);
  end
  else if aRaiseExceptionIfAlreadyExists then
  begin
    raise EDataObj.Create('NewSlot called with slotname="'+aSlotname+'" but this slot already existed in the DataFrame.');
  end;
end;



function TDataFrame.RemoveSlot(aSlot: TDataObj): boolean;
var
  lIndex: integer;
begin
  lIndex := fSlotList.IndexOfObject(aSlot);
  result := Self.Delete(lIndex);               // will handle whether aSlot was found or not.   Also does the freeing of aSlot internally.
end;

procedure TDataFrame.SetSlotname(aIndex: integer; aSlotname: string);
begin
  fSlotList.Strings[aIndex] := aSlotname;
end;

function TDataFrame.SlotByName(const aSlotName: string): TDataObj;
begin
  result := FindSlot(aSlotName);
  if not assigned(result) then
  begin
    raise EDataObj.Create('Slot "'+aSlotname+'" not found.');
  end;
end;

function TDataFrame.Slotname(aIndex: integer): string;
begin
  result := fSlotList.Strings[aIndex];
end;

{ TDataObjectID }

// Fills self object with a newly generated ObjectID.
procedure TDataObjectID.GenerateNewID;
var
  lNow: TDatetime;
begin
  //just one way of generating mongoDB objectID's
  lNow := Now;
  self.MachineID := gNewObjectID_MachineID;  //see initialization
  Self.ProcessID := System.MainThreadID;  //  GetCurrentProcessId;  //GetCurrentThreadId;      //GetCurrentProcessId;
  Self.Counter := AtomicIncrement(integer(gNewObjectID_Counter));
  Self.Seconds := (((Round(EncodeDate(lNow.Year,lNow.Month,lNow.Day))-UnixDateDelta)*24+lNow.Hour)*60+lNow.Minute)*60+lNow.Second;
end;


function TDataObjectID.getAsString: string;
var
  i: Cardinal;
  lP: PCardinal;
begin
  SetLength(result, 24);
  lP := Pointer(result);
  for i := low(Data) to high(Data) do
  begin
    PCardinal(lP)^ := cTwoHexLookup[Data[i]].u32;    //lCharPair is 4 bytes.  Each pair is a unicode Character from the cTwoHexLookup Table.
    inc(lP);
  end;
end;

function TDataObjectID.getCounter: cardinal;
begin
  result := (Data[9] shl 16) + (Data[10] shl 8) + Data[11];
end;

function TDataObjectID.getMachineID: cardinal;
begin
  result := (Data[4] shl 16) + (Data[5] shl 8) + Data[6];
end;

function TDataObjectID.getProcessID: cardinal;
begin
  result := (Data[7] shl 8) + Data[8];
end;

function TDataObjectID.getSeconds: cardinal;
begin
 result := (Data[0] shl 24) + (Data[1] shl 16) + (Data[2] shl 8) + Data[3];
end;

procedure TDataObjectID.setAsString(const Value: string);
begin
  if Length(Value) <> 24 then
    raise EDataObj.Create('Invalid ObjectId Length');

  HexToBin(PChar(Value), @Data[0], 12);   // 12 for the amount of byes in a 24 hex string div 2
end;

procedure TDataObjectID.setCounter(const Value: Cardinal);
begin
  {$R-}
  Data[9] := Value shr 16;
  Data[10] := Value shr 8;
  Data[11] := Value;
  {$R+}
end;

procedure TDataObjectID.setMachineID(const Value: Cardinal);
begin
  {$R-}
  Data[4] := Value shr 16;
  Data[5] := Value shr 8;
  Data[6] := Value;
  {$R+}
end;

procedure TDataObjectID.setProcessID(const Value: Cardinal);
begin
  {$R-}
  Data[7] := Value shr 8;
  Data[8] := Value;
  {$R+}
end;

procedure TDataObjectID.setSeconds(const Value: Cardinal);
begin
  {$R-}
  Data[0] := Value shr 24;
  Data[1] := Value shr 16;
  Data[2] := Value shr 8;
  Data[3] := Value;
  {$R+}
end;

{ TDataSparseArray }

procedure TDataSparseArray.AppendSlot(aSlotIndex: integer; aDataObj: TDataObj);
begin
  //FINISH - what does append mean?  make a copy?  Take Over Ownership?
end;

procedure TDataSparseArray.Clear;
begin
  inherited Clear;
  fIndexKeyList.Clear;
end;

procedure TDataSparseArray.CopyFrom(aSource: TDataSparseArray);
var
  i: Integer;
begin
  for i := 0 to aSource.Count-1 do
  begin
    newSlot(aSource.SlotIndex(i)).CopyFrom(aSource.Slots[i]);
  end;
end;

constructor TDataSparseArray.Create;
begin
  inherited Create;
  fIndexKeyList := TList<Int64>.Create;
end;

function TDataSparseArray.DeleteSlot(aSlotIndex: integer): boolean;
begin
  //Finish
  result := false;
end;

destructor TDataSparseArray.Destroy;
begin
  Clear;
  fIndexKeyList.Free;
  inherited Destroy;
end;

function TDataSparseArray.FindLargestSlotIndex: integer;
var
  i: Integer;
  lSlotIndex: integer;
begin
  //NOTE:  could there be SlotIndexes that are negative?   Yes, possibly so, that's why this code is written the way it is.   IE) it's possible that "-10" is the largest slotIndex value.
  result := 0;
  if count > 0 then
  begin
    result := slotIndex(0);
    for i := 1 to count-1 do
    begin
      lSlotIndex := SlotIndex(i);
      if lSlotIndex > result then
        result := lSlotIndex;
    end;
  end;
end;

function TDataSparseArray.FindSlot(aSlotIndex: integer; var oSlot: TDataObj): boolean;
begin
  //Finish
  result := false;
end;

function TDataSparseArray.FindSlot(aSlotIndex: integer): TDataObj;
begin
  result := nil;
  //Finish
end;

function TDataSparseArray.NewSlot(aSlotIndex: integer; aRaiseExceptionIfAlreadyExists: boolean = false): TDataObj;
begin
  // not tested
  //FINISH - need to do a findSlot First.
  result := FindSlot(aSlotIndex);
  if not assigned(result) then
  begin
    result := TDataObj.Create;
    add(result);
  end
  else if aRaiseExceptionIfAlreadyExists then
  begin
    raise EDataObj.Create('NewSlot called with slotIndex='+IntToStr(aSlotIndex)+'" but this slot already existed in the SparseArray.');
  end;
end;

function TDataSparseArray.SlotByIndex(aSlotIndex: integer): TDataObj;
begin
  //Finish
  result := nil;
end;

function TDataSparseArray.SlotIndex(aIndex: integer): integer;
begin
  //Finish
  result := 0;
end;

{ TDataArray }

function TDataArray.Add(aDataObj: TDataObj): Integer;
var
  lNewCapacity: Cardinal;
begin
  if fCount >= fCapacity then
  begin
    // Expand.  While our array is relatively small, we will start at 16 (arbitrary) and double each time we need more.
    // Once we grow over 8192, we will increase each time by 50%
    if fCapacity<cInitialCapacity then
    begin
      lNewCapacity := cInitialCapacity;
    end
    else if fCapacity>=8192 then
    begin
      lNewCapacity := (Capacity * 3) shr 1;  // shr is a divide by 2
    end
    else
    begin
      lNewCapacity := fCapacity * 2;
    end;
    SetLength(fItems, lNewCapacity);
    fCapacity := lNewCapacity;
  end;
  fItems[fCount] := aDataObj;
  result := fCount;
  inc(fCount);
end;

procedure TDataArray.AppendFrom(aArray: TDataArray);
var
  i: Integer;
begin
  for i := 0 to aArray.Count-1 do
  begin
    NewSlot.CopyFrom(aArray.Items[i]);
  end;
end;

procedure TDataArray.CheckIndexInRange(aIndex: integer);
begin
  if (aIndex<0) or (aIndex>=fCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange) at ReturnAddress;
end;

procedure TDataArray.Clear;
var
  i: Integer;
  lItem: TDataObj;
begin
  for i := 0 to Count-1 do
  begin
    lItem := fItems[i];
    if assigned(lItem) then   // It is possible that sometimes an item could be in the array as nil.
    begin
      lItem.Destroy;
      fItems[i] := nil;   // don't really need this, but it's a tad helpful to be able to see these nils in the debugger.
    end;
  end;
  fCount := 0;   // Not reducing the capacity

end;

function TDataArray.Concat(aArray: TDataArray): TDataObj;
begin
  result := TDataObj.Create;
  with result.AsArray do
  begin
    AppendFrom(self);
    AppendFrom(aArray);
  end;
end;

{$ifDef cMakeMoreCompatibleWithOldDataObjects}
procedure TDataArray.CopyFrom(aArray: TDataArray);
begin
  AppendFrom(aArray);
end;
{$endif}

constructor TDataArray.Create(aInitialCapacity: Integer);
begin
  inherited Create;
  if aInitialCapacity > 0 then
    SetLength(fItems, aInitialCapacity);
end;

procedure TDataArray.DeleteSlot(aIndex: integer);
var
  lObj: TDataObj;
  lSize: NativeInt;
begin
  CheckIndexInRange(aIndex);
{$R-}   // can turn off range checking because it was already checked.
  lObj := fItems[aIndex];
  lObj.Free;
  lSize := SizeOf(TDataObj)*(fCount-aIndex-1);
  if lSize>0 then
    move(fItems[aIndex+1], fItems[aIndex], lSize);
  dec(fCount);
{$R+}

  {Note about DeleteSlot...
   If we are in a situation where a caller is going to do a lot of DeleteSlot calls on an array, then we have a performance problem because the
   move call above is going to get called a lot and it's possible that it's called a lot on a large amount of items and thus larger amounts of memory to do the move on.
   It's not efficient to delete a slot and move the back portion of the array after the slot that was deleted (to pack out the nil item) over and over again.
   So, it would be nice to have a way to DeleteSlots without packing, and then followup with a .Pack() call that will intelligently do the set of moves that are
   needed to get all the nils packed out.  Something like procedure TDataArray.DeleteSlot(aIndex: integer; aDoPack: boolean=true);
                                                          lDataObj.DeleteSlot(10, false);
                                                          lDataObj.DeleteSlot(11, false);
                                                          lDataObj.DeleteSlot(12, false);
                                                          lDataObj.DeleteSlot(20, false);
                                                          lDataObj.DeleteSlot(21, false);
                                                          lDataObj.Pack(); }
end;

destructor TDataArray.Destroy;
begin
  Clear;
  SetLength(fItems,0);
  inherited;
end;

function TDataArray.Every(aEveryFunction: TForEachFunction): boolean;
var
  i: Integer;
begin
  result := true;   // assume true until one of the callbacks return false.
  for i := 0 to count-1 do
  begin
    if aEveryFunction(self, items[i], i) = false then
    begin
      result := false;
      break;
    end;
  end;
end;

function TDataArray.Find(aFindFunction: TForEachFunction): TDataObj;
var
  i: Integer;
begin
  result := nil;   // assume true until one of the callbacks return true.
  for i := 0 to count-1 do
  begin
    if aFindFunction(self, items[i], i) then
    begin
      result := items[i];
      break;
    end;
  end;
end;

procedure TDataArray.ForEach(aForEachFunction: TForEachProcedure; aReverseOrder: boolean = false);
var
  i: Integer;
begin
  if aReverseOrder then
  begin
    for i := count downto 0 do
    begin
      aForEachFunction(self, Items[i], i);
    end;
  end
  else
  begin
    for i := 0 to count-1 do
    begin
      aForEachFunction(self, Items[i], i);
    end;
  end;
end;

function TDataArray.GetEnumerator: TDataArrayEnumerator;
begin
  Result := TDataArrayEnumerator.Create(Self);
end;

function TDataArray.IndexOf(aString: string; aCaseInsensitive: boolean = false): integer;
var
  i: Integer;
begin
  result := -1;
  if aCaseInsensitive then
  begin
    for i := 0 to count-1 do
    begin
      if SameText(items[i].AsString, aString) then
      begin
        result := i;
        break;
      end;
    end;
  end
  else
  begin
    for i := 0 to count-1 do
    begin
      if items[i].AsString = aString then
      begin
        result := i;
        break;
      end;
    end;
  end;
end;

function TDataArray.LastIndexOf(aString: string; aCaseInsensitive: boolean): integer;
var
  i: Integer;
begin
  result := -1;
  if aCaseInsensitive then
  begin
    for i := count-1 downto 0 do
    begin
      if SameText(items[i].AsString, aString) then
      begin
        result := i;
        break;
      end;
    end;
  end
  else
  begin
    for i := count-1 downto 0 do
    begin
      if items[i].AsString = aString then
      begin
        result := i;
        break;
      end;
    end;
  end;
end;

function TDataArray.Map(aMapProcedure: TMapProcedure): TDataObj;
var
  i: Integer;
begin
  result := TDataObj.Create;
  for i := 0 to count-1 do
  begin
    aMapProcedure(result.AsArray.NewSlot, self, items[i], i);
  end;
end;

function TDataArray.NewSlot: TDataObj;
begin
  result := TDataObj.Create;
  add(result);
end;

function TDataArray.getItem(aIndex: Cardinal): TDataObj;
begin
  result := fItems[aIndex];
end;

function TDataArray.getSlot(aIndex: integer): TDataObj;
begin
  result := items[aIndex];
end;


function TDataArray.IndexOf(aInteger: Integer; aCaseInsensitive: boolean): integer;
var
  i: Integer;
  lItem: TDataObj;
begin
  result := -1;
  for i := 0 to count-1 do
  begin
    lItem := Items[i];
    if lItem.DataType.Code = cDataTypeInt32 then
    begin
      if lItem.AsInt32 = aInteger then
      begin
        result := i;
        break;
      end;
    end;
  end;
end;

function TDataArray.IndexOf(aInt64: Int64; aCaseInsensitive: boolean): integer;
var
  i: Integer;
  lItem: TDataObj;
begin
  result := -1;
  for i := 0 to count-1 do
  begin
    lItem := Items[i];
    if lItem.DataType.Code = cDataTypeInt64 then
    begin
      if lItem.AsInt64 = aInt64 then
      begin
        result := i;
        break;
      end;
    end;
  end;
end;

function TDataArray.IndexOfChildSlot(aSlot: TDataObj): integer;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
  begin
    if fItems[i]=aSlot then
    begin
      Exit(i);
    end;
  end;
  Result := -1;
end;

function TDataArray.Reduce(aReduceProcedure: TReduceProcedure): TDataObj;
var
  i: Integer;
begin
  result := TDataObj.Create;
  for i := 0 to count-1 do
  begin
    aReduceProcedure(result, self, items[i], i);
  end;
end;

function TDataArray.RemoveForEach(aEveryFunction: TForEachFunction): integer;
var
  i: Integer;
begin
  result := 0;
  for i := count-1 downto 0 do
  begin
    if aEveryFunction(self, items[i], i) then
    begin
      self.DeleteSlot(i);
      inc(result);
    end;
  end;
end;



procedure TDataArray.setCapacity(const aCapacity: Integer);
begin
  if aCapacity > fCount then               // only accept a new capacity if it is larger than our current count.
  begin
    SetLength(fItems, aCapacity);
    fCapacity := aCapacity;
  end;
end;

procedure TDataArray.setItem(aIndex: Cardinal; const Value: TDataObj);
begin
  fItems[aIndex] := Value;
end;

{ TDataTag }

constructor TDataTag.Create;
begin
  inherited Create;
  fDataObj := TDataObj.Create;   //this tag always owns one contained dataObject.
end;

destructor TDataTag.Destroy;
begin
  fDataObj.Free;
  inherited;
end;

function TDataTag.getDataObj: TDataObj;
begin
  result := fDataObj;
end;

procedure TDataTag.setDataObj(const Value: TDataObj);
begin
  if assigned(fDataObj) then
    FreeAndNil(fDataObj);
  fDataObj := Value;
end;

{ TDataStore }

procedure TDataStore.ClearData;
begin
  if pointer(fDataString)<>nil then
    fDataString := '';

  if assigned(fDataObject) then   // This code block is a bit faster than calling FreeAndNil()
  begin
    fDataObject.Destroy;
    fDataObject := nil;
  end;

  fDataInt64 := 0;
end;


function TDataStore.getDataString: string;
begin
  result := fDataString;
end;


procedure TDataStore.setDataString(const Value: string);
begin
  fDataString := Value;
end;


{ TDataObjStreamerBase }
procedure TDataObjStreamerBase.ApplyOptionalParameters(aParams: TStrings);
begin
  // by default, nothing to implement.  descendants will override if they need to do this.
end;

procedure TDataObjStreamerBase.ApplyOptionalParameters(aParams: String);
var
  lStringList: TStringList;
  lParams: string;
begin
  lParams := trim(aParams);
  if length(lParams) > 0 then
  begin
    lStringList:=TStringList.Create;
    try
      lStringList.Delimiter := ' ';
      lStringList.DelimitedText := lParams;
      ApplyOptionalParameters(lStringList);
    finally
      lStringList.Free;
    end;
  end;
end;

function TDataObjStreamerBase.Clone: TDataObjStreamerBase;
begin
  if assigned(self) then
  begin
    Result := TDataObjStreamerBase(self.ClassType.NewInstance);   // instantiate an object of the same class that this is NOT bound to the same stream.
//  Result := TDataObjStreamerBase(self.ClassType).Create(fStream);   // instantiate an object of the same class that this is and bound to the same fStream.
  end
  else
  begin
    result := nil;
  end;
end;

constructor TDataObjStreamerBase.Create(aStream: TStream = nil);
begin
  inherited Create;
  fStream := aStream;
end;

destructor TDataObjStreamerBase.Destroy;
begin
  if fOwnsStream then
    fStream.Free;
  inherited;
end;

class function TDataObjStreamerBase.GetClipboardFormatStr: string;
begin
  result := 'CF_'+self.ClassName;    // base class implements this pattern for all descendants, but each descendant is free to override and do something else.
end;

class function TDataObjStreamerBase.IsFileExtension(aStr: string): boolean;
begin
  result := SameText(aStr, FileExtension) or SameText(aStr, '.'+FileExtension);
end;

procedure TDataObjStreamerBase.setStream(const Value: TStream);
begin
  if assigned(fStream) then
  begin
    if (fStream<>Value) and (fOwnsStream) then   // we are being given a new stream to bind to and since we own the current one, we must free it or the reference will forever be lost.
    begin
      freeAndNil(fStream);
    end;
  end;
  fStream := Value;
end;

class procedure TDataObjStreamerBase.GetParameterInfo(aParameterPurpose: TDataObjParameterPurposes; aStrings: TStrings);
begin
  // Most descendant classes will not have optional streaming parameters so we implement a base here to do nothing
end;





procedure DataObjConvertStringListToArrayOfStrings(aDataObj: TDataObj);
var
  i: integer;
  lDataArray: TDataArray;
begin
  if aDataObj.DataType.Code = cDataTypeStringList then
  begin
    lDataArray:=TDataArray.create;
    for i := 0 to aDataObj.AsStringList.Count-1 do
    begin
      lDataArray.NewSlot.AsString := aDataObj.AsStringList.Strings[i];
    end;
    aDataObj.setAsArray(lDataArray);     // This gets the aDataObj to take over ownership.
  end;
end;

procedure DataObjConvertArrayOfStringsToStringList(aDataObj: TDataObj);
var
  i: integer;
  lSL: TDataStringList;
  lDataArray: TDataArray;
begin
  if aDataObj.DataType.Code = cDataTypeArray then
  begin
    lDataArray := aDataObj.AsArray;
    lSL:=TDataStringList.create;
    try
      for i := 0 to lDataArray.Count-1 do
      begin
        lSL.Add(lDataArray.Slots[i].AsString);
      end;
    finally
      aDataObj.setAsStringList(lSL);     // This gets the aDataObj to take over ownership.
    end;
  end;
end;

procedure DataObjConvertBase64StringToBinary(aDataObj: TDataObj);
var
  lEncoder : TBase64StringEncoding;
  lSS: TStringStream;
  lMS: TMemoryStream;
begin
  if aDataObj.DataType.Code = cDataTypeString then
  begin
    lSS:=TStringStream.create(aDataObj.AsString);    // get the source string accessible as a Stream.
    lMS:=TMemoryStream.Create;
    try
      lEncoder := TBase64StringEncoding.create;
      lEncoder.decode(lSS, lMS);

      // If the above call did not except out, then we can use what was produced.
      aDataObj.AsBinary.LoadFromStream(lMS);    // FINISH - Bettery way to ASSIGN the memory stream just so ownership can be taken over?

    finally
      lSS.Free;
      lMS.Free;
    end;
  end;
end;

procedure DataObjConvertBinaryToBase64String(aDataObj: TDataObj);
var
  lEncoder : TBase64StringEncoding;
  lSS: TStringStream;
begin
  if aDataObj.DataType.Code = cDataTypeBinary then
  begin
    lSS:=TStringStream.create();
    try
      lEncoder := TBase64StringEncoding.create;
      lEncoder.encode(aDataObj.AsBinary, lSS);

      // If the above call did not except out, then we can use what was produced.
      aDataObj.AsString := lSS.DataString;
    finally
      lSS.Free;
    end;
  end;

end;

{ TDataObjAssignContext }

procedure TDataObjAssignContext.AddObject(aObject: TObject);
begin
  fSerializedObjects.Add(aObject);
end;

constructor TDataObjAssignContext.Create;
begin
  inherited Create;
  fSerializedObjects := TList.Create;
  IncludeSerializingClassName := true;
end;

destructor TDataObjAssignContext.Destroy;
begin
  fSerializedObjects.Free;
  inherited;
end;

function TDataObjAssignContext.IsAlreadySerialized(aObject: TObject): boolean;
begin
  result := fSerializedObjects.IndexOf(aObject) >= 0;
end;

procedure TDataObjAssignContext.ReportException(aException: Exception);
begin
  if assigned(fOnHandleException) then
    fOnHandleException(self, aException);
end;

{ TDataStringList }

function TDataStringList.GetAsArrayOfStrings: TArray<string>;
var
  i: Integer;
begin
  SetLength(result, count);
  for i := 0 to count-1 do
    result[i] := self.Strings[i];
end;

{ TDetachableMemoryStream }

destructor TDetachableMemoryStream.Destroy;
begin
  if Size=0 then    // if the size is over zero, we are not calling the Clear method here because something else is taking ownership of the memory we allocated.
    clear;          // if the size is zero.  Which means we didn't put anything into it, we have to do a clear because the capacity could have been set and it could be bigger than zero.
//  inherited;     //DO NOT call inherited destroy as this will then go to TMemoryStream.Destroy which will free the allocated memory which we do NOT want to do.
end;





procedure InitObjectIDGenerator;
var
  lComputerName: string;
  i,l: integer;
begin
  {$ifdef MSWINDOWS}
  // We need to setup the number that represents this Machine and the random starting value for the sequence number whenever we ask TDataObjectID to generate a new ID.
    l:=MAX_COMPUTERNAME_LENGTH;
    SetLength(lComputerName,l);
    if GetComputerName(PChar(lComputerName),cardinal(l)) then
      SetLength(lComputerName,l)
    else
      lComputerName:=GetEnvironmentVariable('COMPUTERNAME');
  {$else}
    // How do we get a computer name from Android, IOS, Mac, Linux, etc?  for now, generate something random.  This needs to be fixed someday I think.
    setLength(lComputerName,10);
    for i := 1 to 10 do
    begin
      lComputerName[i] := chr(random(ord('Z')-ord('A'))+ord('A'));
    end;
  {$endif}

  gNewObjectID_MachineID:=$10101;
  for i:=1 to Length(lComputerName) do
  begin
    case lComputerName[i] of
      '0'..'9':
        gNewObjectID_MachineID:=(gNewObjectID_MachineID*36+(byte(lComputerName[i]) and $0F)) and $FFFFFF;
      'A'..'Z','a'..'z':
        gNewObjectID_MachineID:=(gNewObjectID_MachineID*36+(byte(lComputerName[i]) and $1F)+9) and $FFFFFF;
      //else ignore
    end;
  end;

  //GetTickCount returns a cardinal and the gNewObjectID_Counter is an integer
  gNewObjectID_Counter:=TThread.GetTickCount;  // Generate a random starting point.
end;


{ TDataArray.TDataArrayEnumerator }

constructor TDataArray.TDataArrayEnumerator.Create(ADataArray: TDataArray);
begin
  inherited Create;
  FIndex := -1;
  FDataArray := ADataArray;
end;


function TDataArray.TDataArrayEnumerator.GetCurrent: TDataObj;
begin
  Result := FDataArray.FItems[FIndex];
end;

function TDataArray.TDataArrayEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < fDataArray.count;
end;

{ TDataFrame.TDataFrameEnumerator }

constructor TDataFrame.TDataFrameEnumerator.Create(ADataFrame: TDataFrame);
begin
  inherited Create;
  fDataFrame := aDataFrame;
  FIndex := -1;
end;

function TDataFrame.TDataFrameEnumerator.GetCurrent: TDataFrameEnumeratorRec;
begin
  result.DataObj := FDataFrame.getSlot(FIndex);
  result.Slotname := FDataFrame.Slotname(FIndex);
end;

function TDataFrame.TDataFrameEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < fDataFrame.count;
end;

initialization
  gRttiContext := TRttiContext.Create;   // we make our own RttiContext to use for RTTI assignment.
  InitObjectIDGenerator;

end.
