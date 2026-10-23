unit DataObjects2CBOR;

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

interface

uses classes, DataObjects2, DataObjects2Streamers, SysUtils, RTTI, TypInfo, DataObjects2Utils;

type

(*
  TCborTag = record
    TagNo: integer;
    OnType: string;
    Desc: string;
  end;

const
  cCborTagDefs: array[0..1] of TCborTag = (
    (Tagno: 0; OnType: 'UTF-8 string'; Desc: 'Standard date/time string'),
    (Tagno: 1; OnType: 'multiple'; Desc: 'Epoch-based date/time')
    //FINISH this table.
  );
  *)

(* CBOR registered tag definitions
https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml

Tag	Data Item	Semantics	Reference
0	UTF-8 string	Standard date/time string; see Section 2.4.1	[RFC7049]
1	multiple	Epoch-based date/time; see Section 2.4.1	[RFC7049]
2	byte string	Positive bignum; see Section 2.4.2	[RFC7049]
3	byte string	Negative bignum; see Section 2.4.2	[RFC7049]
4	array	Decimal fraction; see Section 2.4.3	[RFC7049]
5	array	Bigfloat; see Section 2.4.3	[RFC7049]
15-Jun	Unassigned
16	COSE_Encrypt0	COSE Single Recipient Encrypted Data Object	[RFC8152]
17	COSE_Mac0	COSE Mac w/o Recipients Object	[RFC8152]
18	COSE_Sign1	COSE Single Signer Data Object	[RFC8152]
19-20	Unassigned
21	multiple	Expected conversion to base64url encoding; see Section 2.4.4.2	[RFC7049]
22	multiple	Expected conversion to base64 encoding; see Section 2.4.4.2	[RFC7049]
23	multiple	Expected conversion to base16 encoding; see Section 2.4.4.2	[RFC7049]
24	byte string	Encoded CBOR data item; see Section 2.4.4.1	[RFC7049]
25	unsigned integer	reference the nth previously seen string	[http://cbor.schmorp.de/stringref][Marc_A._Lehmann]
26	array	Serialised Perl object with classname and constructor arguments	[http://cbor.schmorp.de/perl-object][Marc_A._Lehmann]
27	array	Serialised language-independent object with type name and constructor arguments	[http://cbor.schmorp.de/generic-object][Marc_A._Lehmann]
28	multiple	mark value as (potentially) shared	[http://cbor.schmorp.de/value-sharing][Marc_A._Lehmann]
29	unsigned integer	reference nth marked value	[http://cbor.schmorp.de/value-sharing][Marc_A._Lehmann]
30	array	Rational number	[http://peteroupc.github.io/CBOR/rational.html][Peter_Occil]
31	Unassigned
32	UTF-8 string	URI; see Section 2.4.4.3	[RFC7049]
33	UTF-8 string	base64url; see Section 2.4.4.3	[RFC7049]
34	UTF-8 string	base64; see Section 2.4.4.3	[RFC7049]
35	UTF-8 string	Regular expression; see Section 2.4.4.3	[RFC7049]
36	UTF-8 string	MIME message; see Section 2.4.4.3	[RFC7049]
37	byte string	Binary UUID ([RFC4122] section 4.1.2)	[https://github.com/lucas-clemente/cbor-specs/blob/master/uuid.md][Lucas_Clemente]
38	array	Language-tagged string	[http://peteroupc.github.io/CBOR/langtags.html][Peter_Occil]
39	multiple	Identifier	[https://github.com/lucas-clemente/cbor-specs/blob/master/id.md][Lucas_Clemente]
40-60	Unassigned
61	CBOR Web Token (CWT)	CBOR Web Token (CWT)	[RFC8392][Michael_B._Jones]
62-95	Unassigned
96	COSE_Encrypt	COSE Encrypted Data Object	[RFC8152]
97	COSE_Mac	COSE MACed Data Object	[RFC8152]
98	COSE_Sign	COSE Signed Data Object	[RFC8152]
99-102	Unassigned
103	array	Geographic Coordinates	[https://github.com/allthingstalk/cbor/blob/master/CBOR-Tag103-Geographic-Coordinates.md][Danilo_Vidovic]
104-119	Unassigned
120	multiple	Internet of Things Data Point	[https://github.com/allthingstalk/cbor/blob/master/CBOR-Tag120-Internet-of-Things-Data-Points.md][Danilo_Vidovic]
121-255	Unassigned
256	multiple	mark value as having string references	[http://cbor.schmorp.de/stringref][Marc_A._Lehmann]
257	byte string	Binary MIME message	[http://peteroupc.github.io/CBOR/binarymime.html][Peter_Occil]
258	array	Mathematical finite set	[https://github.com/input-output-hk/cbor-sets-spec/blob/master/CBOR_SETS.md][Alfredo_Di_Napoli]
259	Unassigned
260	byte string	Network Address (IPv4 or IPv6 or MAC Address)	[http://www.employees.org/~ravir/cbor-network.txt][Ravi_Raju]
261	map (IPAddress + Mask Length)	Network Address Prefix (IPv4 or IPv6 Address + Mask Length)	[https://github.com/toravir/CBOR-Tag-Specs/blob/master/networkPrefix.md][Ravi_Raju]
262	byte string	Embedded JSON Object	[https://github.com/toravir/CBOR-Tag-Specs/blob/master/embeddedJSON.md][Ravi_Raju]
263	byte string	Hexadecimal string	[https://github.com/toravir/CBOR-Tag-Specs/blob/master/hexString.md][Ravi_Raju]
264	array	Decimal fraction with arbitrary exponent	[http://peteroupc.github.io/CBOR/bigfrac.html][Peter_Occil]
265	array	Bigfloat with arbitrary exponent	[http://peteroupc.github.io/CBOR/bigfrac.html][Peter_Occil]
266-1000	Unassigned
1001	map	extended time	[draft-bormann-cbor-time-tag-01]
1002	map	duration	[draft-bormann-cbor-time-tag-01]
1003	map	period	[draft-bormann-cbor-time-tag-01]
1004-22097	Unassigned
22098	multiple	hint that indicates an additional level of indirection	[http://cbor.schmorp.de/indirection][Marc_A._Lehmann]
22099-55798	Unassigned
55799	multiple	Self-describe CBOR; see Section 2.4.5	[RFC7049]
55800-15309735	Unassigned
15309736	map (major type 5)	RAINS Message	[https://britram.github.io/rains-prototype][Brian_Trammell]
15309737-18446744073709551615	Unassigned

  *)

  ECBORStreamerExceptionCantReadStream = class(Exception);
  ECBORStreamerExceptionInvalidFormat = class(Exception);


  TCBORStreamer = class(TDataObjStreamerBase)
  private
    fCurrentTagValue: UInt64;        // used to keep track of state when decoding.

    // Settings for Encoding/Decoding options.
    fSupportSimples: boolean;       // If true, then we allow encoding and decoding of the Simple values under type 7 generically for those that are undefined

    procedure DoRead(var Buffer; Count: Longint);
    function DecodeInternal(aDataObj: TDataObj): boolean;

  public
    constructor Create(aStream: TStream = nil); override;
    class function FileExtension: string; override;
    class function Name: string; override;
    class function Description: string; override;
    class function GetFileFilter: string; override;
    class function IsFileExtension(aStr: string): boolean; override;
    class function Priority: cardinal; override;

    procedure Decode(aDataObj: TDataObj); override;
    procedure Encode(aDataObj: TDataObj); override;

    property SupportSimples: boolean read fSupportSimples write fSupportSimples;
  end;

  procedure WriteObjectToCBORStream(aStream: TStream; aObject: TObject);
  procedure DoReadStream(aStream: TStream; var Buffer; Count: Longint);

implementation

uses Math;

var
  gRttiContext: TRttiContext;

resourceString
  cExceptInvalidTypeCodeAndSubType = 'Received TypeCode=%d, Invalid Subtype=%d';
  cExceptInvalidSimpleTypeForTypeCode7 = 'Received TypeCode=7,Subtype=24 with invalid subsequent byte SimpleType value of %d';
  cExceptInvalidSubTypeForTypeCode7 = 'Received TypeCode=7 with invalid subType of %d';
  cExceptInvalidByteStringChunkSubTypeBad = 'Error while reading chunks of an indefinite-length ByteString, chunk header read had invalid SubType of %d';
  cExceptInvalidByteStringChunkMajorTypeBad = 'Error while reading chunks of an indefinite-length ByteString, chunk header read had a MajorType of %d.  Only MajorTypes of 2 (ByteString) are allowed.';
  cExceptNotEnoughBytesByteString = 'Error while reading a definite-length ByteString.  Tried to read %d bytes, but could only read %d bytes from the source stream.';
  cExceptInvalidTextStringChunkSubTypeBad = 'Error while reading chunks of an indefinite-length TextString, chunk header read had invalid SubType of %d';
  cExceptInvalidTextStringChunkMajorTypeBad = 'Error while reading chunks of an indefinite-length TextString, chunk header read had a MajorType of %d.  Only MajorTypes of 3 (TextString) are allowed.';
  cExceptNotEnoughBytesTextString = 'Error while reading a definite-length TextString.  Tried to read %d bytes, but could only read %d bytes from the source stream.';
  cExceptNegativeNumberTooBig = 'Negative number too large to be supported. ';
  cExceptPositiveNumberTooBig = 'Positive number too large to be supported. ';




procedure WriteTypeAndNumber(aStream: TStream; aMajorType: byte; aCount: UInt32); overload;
var
  lBuffer: array[0..4] of byte;
begin
  //Deals with positive numbers
  if aCount <= 23 then
  begin
    lBuffer[0] := byte(aCount or (aMajorType shl 5));
    aStream.Write(lBuffer[0], 1);     // Major=0, value is 0 - 23
  end
  else if aCount <= 255 then
  begin
    lBuffer[0] := byte(24 or (aMajorType shl 5));  // major = 0, additional=24 which means one byte follows. value is 24-255
    lBuffer[1] := byte(aCount);
    aStream.Write(lBuffer[0], 2);
  end
  else if aCount <= 65535 then
  begin
    lBuffer[0] := byte(25 or (aMajorType shl 5));  // major = 0, additional=25 which means two bytes follows.  value = 256-65535
    lBuffer[2] := byte(aCount);         // truncate to one byte
    lBuffer[1] := byte(aCount shr 8);   // 2nd MSB
    aStream.Write(lBuffer[0],3);
  end
  else
  begin
    lBuffer[0] := byte(26 or (aMajorType shl 5));   // major = 0, additional=26 which means four bytes follows. value = 65536 - 4,294,967,295
    lBuffer[4] := byte(aCount);         // Shift in each of the bytes to the buffer
    lBuffer[3] := byte(aCount shr 8);   //
    lBuffer[2] := byte(aCount shr 16);  //
    lBuffer[1] := byte(aCount shr 24);  //
    aStream.Write(lBuffer[0],5);
  end;

end;


//writes out the generic pattern of a MajorType number 0-7 along with the aCount which is the value portion of this MajorType.
procedure WriteTypeAndNumber(aStream: TStream; aMajorType: byte; aValue: UInt64); overload;
var
  lBytes: TNumBytes;
begin
  //Deals with positive numbers
  if aValue <= 23 then
  begin
    lBytes.Code := byte(aValue or (aMajorType shl 5));
    aStream.Write(lBytes, 1);     // Major=0, value is 0 - 23
  end
  else if aValue <= 255 then
  begin
    lBytes.Code := byte(24 or (aMajorType shl 5));  // major = 0, additional=24 which means one byte follows. value is 24-255
    lBytes.SetByteValue(byte(aValue));
    aStream.Write(lBytes, 2);
  end
  else if aValue <= 65535 then
  begin
    lBytes.Code := byte(25 or (aMajorType shl 5));  // major = 0, additional=25 which means two bytes follows.  value = 256-65535
    lBytes.SetUnsignedInt16Value(word(aValue));
    aStream.Write(lBytes,3);
  end
  else if aValue <= $FFFFFFFF then
  begin
    lBytes.Code := byte(26 or (aMajorType shl 5));   // major = 0, additional=26 which means four bytes follows. value = 65536 - 4,294,967,295
    lBytes.SetUnsignedIntValue(Cardinal(aValue));
    aStream.Write(lBytes,5);
  end
  else
  begin
    lBytes.Code := byte(27 or (aMajorType shl 5));    // major = 0, additional=27 which means eight bytes follows. value = 4,294,967,296 -  max int64
    lBytes.SetUInt64Value(aValue);
    aStream.Write(lBytes,9);
  end;
end;

procedure WriteUInt64(aStream: TStream; aValue: UInt64);
begin
  WriteTypeAndNumber(aStream, 0, aValue);
end;

procedure WriteNegInt64(aStream: TStream; aValue: int64);    // expect that aValue is negative
var
  lInt: Int64;
begin
  lInt := -1-aValue;
  WriteTypeAndNumber(aStream, 1, UInt64(lInt));
end;

procedure WriteInt64(aStream: TStream; aValue: Int64);
begin
  if aValue >= 0 then
    WriteUInt64(aStream, UInt64(aValue))
  else
    WriteNegInt64(aStream, aValue);
end;

procedure WriteSingle(aStream: TStream; aValue: single);
type
  TCodeWithSingle = packed record
    lCode: byte;
    lSingle: Cardinal;    // 4-byte cardinal representation of a byte-swapped 4-byte single.
  end;
var
  lCodeWithSingle: TCodeWithSingle;
begin
  lCodeWithSingle.lCode := $FA;                      // Major 7, value=26;     // means a 4-byte single follows
  lCodeWithSingle.lSingle := SwapBytesSingle(aValue);
  aStream.Write(lCodeWithSingle, 5);
end;

procedure WriteDouble(aStream: TStream; aValue: Double);
type
  TCodeWithDouble = packed record
    lCode: byte;
    lDouble: UInt64;   // 8 byte UInt64 representation of a byte-swapped 8-byte double.
  end;
var
  lCodeWithDouble: TCodeWithDouble;
begin
  lCodeWithDouble.lCode := $FB;                      // Major 7, value=27;     // means a 8-byte double follows
  lCodeWithDouble.lDouble := SwapBytesFromDouble(aValue);                // need to reverse the order of the bytes.
  aStream.Write(lCodeWithDouble, 9);
end;


(*
procedure WriteUTF8String(aStream: TStream; aStr: UTF8String);
var
  lLen: UInt32;
begin
  lLen := length(aStr);
  WriteTypeAndNumber(aStream, 3, lLen);
  aStream.Write(aStr[1], lLen);
end;   *)

procedure WriteUnicodeString(aStream: TStream; aStr: String);
var
  lLen: UInt32;
  lStr: UTF8String;
begin
  lStr := UTF8String(aStr);                // converting to the string type we are putting on the wire.
  lLen := UInt32(length(lStr));
  WriteTypeAndNumber(aStream, 3, lLen);
  aStream.Write(lStr[1], Integer(lLen));
end;

procedure WriteBooleanFalse(aStream: TStream);
var
  lCode: byte;
begin
  lCode := $F4;
  aStream.Write(lCode, 1);          //Major 7, value=20;
end;

procedure WriteBooleanTrue(aStream: TStream);
var
  lCode: byte;
begin
  lCode := $F5;
  aStream.Write(lCode, 1);          //Major 7, value=20;
end;

procedure WriteByteValue(aStream: TStream; aByte: byte);
var
  lBuffer: array[0..1] of byte;
begin
  if aByte <= 23 then              // we inherently treat a byte as unsigned.
    aStream.Write(aByte, 1)        // Major=0, value is 0 - 23
  else
  begin
    lBuffer[0] := 24;
    lBuffer[1] := aByte;
    aStream.Write(lBuffer[0], 2);       // major = 0, additional=24 which means one byte follows. value is 24-255
  end;
end;

procedure WriteTag(aStream: TStream; aTagNo: UInt64);
begin
  WriteTypeAndNumber(aStream, 6, aTagNo);
end;



procedure RttiValueToStream(aStream: TStream; aInstance: TObject; aMember: TRttiMember);
var
  lObject: TObject;
  lValue: TValue;
begin
  { The available kinds are:                                                   }
  { tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,                      }
  { tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,         }
  { tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray, tkUString, }
  { tkClassRef, tkPointer, tkProcedure                                         }
  if AMember is TRttiProperty then
    lValue:=TRttiProperty(AMember).GetValue(AInstance)
  else
    Exit;

  case lValue.Kind of
    //tkSet, tkEnumeration, tkWChar:
    tkSet:
    begin
      case lValue.DataSize of
        1: begin
          WriteUnicodeString(aStream, aMember.Name);
          WriteByteValue(aStream, PByte(lValue.GetReferenceToRawData)^);
        end;
        2,4: begin
          WriteUnicodeString(aStream, aMember.Name);
          WriteInt64(aStream, PInteger(lValue.GetReferenceToRawData)^);  // converting to 64bit in for writing.
        end;
      end;
    end;
    tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64:  //Ordinal data types
    begin
      // We need to handle booleans separately.
      if lValue.TypeInfo = System.TypeInfo(Boolean) then
      begin
        WriteUnicodeString(aStream, aMember.Name);
        if lValue.AsOrdinal<>0 then
          WriteBooleanTrue(aStream)
        else
          WriteBooleanFalse(aStream);
      end
      else
      begin
        case lValue.DataSize of
          1: begin
            WriteUnicodeString(aStream, aMember.Name);
            WriteByteValue(aStream, Byte(lValue.AsOrdinal));
          end;
          2,4: begin
            WriteUnicodeString(aStream, aMember.Name);
            WriteInt64(aStream, lValue.AsOrdinal);                 // converting to 64bit in for writing.
          end;
          //FINSH - is there a case..else?
        end;
       end;
    end;
    tkFloat:
    begin
      WriteUnicodeString(aStream, aMember.Name);

      // We need to handle TDateTimes separately
      if lValue.Typeinfo = System.TypeInfo(TDateTime) then
      begin
        WriteUnicodeString(aStream, '<DATE>');          //stub for now.
        // finish - write dateTime value. AObj.AsFrame.NewSlot(AMember.Name).AsDateTime:=lValue.AsExtended
      end
      else
      begin
        WriteDouble(aStream, lValue.AsExtended);
      end;
    end;
    tkString, tkLString, tkWString, tkUString:
    begin
      WriteUnicodeString(aStream, aMember.Name);
      WriteUnicodeString(aStream, lValue.AsString);
    end;
    tkClass:
    begin
      lObject:=lValue.AsObject;
      if  Assigned(lObject) then
      begin
        WriteObjectToCBORStream(aStream, lObject);            // two-function recursion happening here.
      end;
    end;
  end;
end;

procedure WriteObjectToCBORStream(aStream: TStream; aObject: TObject);
var
  lRttiType : TRttiType;
  lRttiProp : TRttiProperty;

  lCount: UInt32;

 function PropShouldBeSerialized: boolean;
// var
//   lCustomAtt : TCustomAttribute;
//   i: Integer;
 begin
   result := false;
   if (lRTTIProp.IsReadable) then
   begin
     if (lRTTIProp.Visibility = TMemberVisibility.mvPublished) then
       result := true
     else
     begin
       // scan to see if we have a defined custom attribute on this property that flags it as serializable.
       // Put this back in someday?
       (*
       for lCustomAtt in lRttiProp.GetAttributes do
       begin
         if lCustomAtt is DataObjectAssignableAttribute then
         begin
           result := true;
           break;
         end;
       end;
       *)
     end;
   end;
 end;


begin
  lCount := 0;
  // if the aObject is a collection, then we will be producing an array.
  if (aObject is TCollection) then
  begin
    // the instance is a collection of objects so save it into the data object (self) as an array of frames.
    //FINISH
{      for i := 0 to TCollection(AInfo.Instance).Count - 1 do
    begin
      AInfo.ParentObj.AsArray.newSlot.AssignFromCallback(TCollection(AInfo.Instance).Items[i],AInfo.Callback);
    end;}
  end
  else
  begin
    // this object isn't a collection, so we are producing a frame (CBOR map)
    // for this object with published attributes, we are going to generate a frame (CBOR map)
    // Now go though the members of the instance and serialize those properties that we should serialize
    lRttiType := gRttiContext.GetType(aObject.ClassType);

    // first we have to figure out how many of these properties we are going to serialize.
    // CBOR Map is capable of having a serialized collection of items without knowing the count first.  maybe we should switch to that?
    for lRttiProp in lRttiType.GetProperties do
    begin
      if PropShouldBeSerialized then
        inc(lCount);
    end;

    // First, write out the special tag that identifies the following map as an Object
// DON"T do this here as it's done by the caller.      WriteTypeAndNumber(aStream, 6, 27);         // 27 is the tag for "Serialised language-independent object with type name and constructor arguments"
    WriteTypeAndNumber(aStream, 5, lCount+1);   // write out the map dataType with the number of properties we are going to serialize.   The +1 is for the '_Type'

    // The first property we are writing is going to be the classname and by convention, it has a hard-coded slotname of '_type'
    WriteUnicodeString(aStream, '_type');
    WriteUnicodeString(aStream, aObject.ClassName);

    // Now go and write out the published properties.
    for lRttiProp in lRttiType.GetProperties do
    begin
      if PropShouldBeSerialized then
      begin
        // write out the name - value pair.
        RttiValueToStream(aStream, aObject, lRttiProp);
      end;
    end;
  end;
end;

procedure RaiseParsingException(aStream: TStream; aMessage: string);
begin
  raise ECBORStreamerExceptionInvalidFormat.Create(aMessage+' when reading a CBOR Stream at position='+intToStr(aStream.Position));
end;


// note, this takes the currently already-read byte that defines the start of a new chunk of data and it decodes the SubType to possibly
// read follow-up bytes that give the Content Length that will be read next
function ReadLength(aStream: TStream; aMajorType, aSubType: byte): UInt64;
var
  lSimpleValue: Byte;
  lShort: UInt16;
  lCardinal: Cardinal;
  lUInt64: UInt64;
begin
  result := 0;
  case aSubType of
    0..23: begin
      result := aSubType;    // subtype just contains the negative int directly
    end;
    24: begin
      DoReadStream(aStream, lSimpleValue, 1);
      result := lSimplevalue;  // numbers 24-255 are covered by reading one more byte.
    end;
    25: begin
      DoReadStream(aStream, lShort, 2);
      lShort := SwapBytes(lShort);
      result := lShort;     // numbers 256-65535 are covered by reading two bytes
    end;
    26: begin
      DoReadStream(aStream, lCardinal, 4);
      lCardinal := SwapBytes(lCardinal);
      result := lCardinal;  // numbers 65536 -  2 billion are covered by reading four bytes
    end;
    27: begin
      DoReadStream(aStream, lUInt64, 8);  // 8 byte unsigned int64.
      lUInt64 := SwapBytes(lUInt64);
      result := lUInt64;
    end
    else
    begin
      // Error condition. 28-31 these are not defined.
      RaiseParsingException(aStream, Format(cExceptInvalidTypeCodeAndSubType,[aMajorType, aSubType]));
    end;
  end;
end;


function ReadTextString(aStream: TStream; aSubType: byte): string;
var
  lMajorType: byte;
  lSS: TStringStream;
  lToReadCount: UInt64;
  lCount: int64;
  lSubType: byte;
begin
  // reading a Text String;
  if aSubType = 31 then
  begin
    // This is an indefinite length array of UTF8 byte.  To read this, we are then reading an indefinite number of definite length text strings.
    // SO, what follows should be one or more definite length textStrings.  When it's done, we will receive an "end-Of-stream" marker which is major=7,sub=31.  $FF.
    lSS:=TStringStream.Create('',TEncoding.UTF8);
    try
      while true do
      begin
        aStream.Read(lMajorType, 1);
        if lMajorType=$FF then
        begin
          break;                          // we successfully hit the "break" stop code, so we are done reading chunks.
        end
        else
        begin
          // we should be reading another chunk which should be a definite-length textString.  Anything else is an error condition.
          lSubType := lMajorType and $1F;   // first 5 bits
          lMajorType := Byte(lMajorType shr 5);   // get it to a 0-7 range

          if (lMajorType=3) and (lSubType <= 27) then
          begin
            // we are now given a definiteLength textString that we need to append to string of data we may have already loaded for this string.
            lToReadCount := ReadLength(aStream, lMajorType, lSubType);
            if lToReadCount>0 then
            begin
              lCount := lSS.CopyFrom(aStream, Int64(lToReadCount));
              if lCount <> lToReadCount then
                RaiseParsingException(aStream, Format(cExceptNotEnoughBytesTextString, [lToReadCount, lCount]));
            end
            else
            begin
              //Technically, receiving a chunk that is zero in length is probably considered malformed.  Not really a big deal here, so no exception.  maybe we will raise exception in the future.
            end;
          end
          else
          begin
            // we either received a majorType that wasn't a byteString or it had a subType that was invalid (28-30 are undefined and 31=indefiniteLength is not valid cause all chunks must be a definite length chunk)
            if lMajorType=3 then
              RaiseParsingException(aStream, Format(cExceptInvalidTextStringChunkSubTypeBad,[lSubType]))
            else
              RaiseParsingException(aStream, Format(cExceptInvalidTextStringChunkMajorTypeBad,[lMajorType]));
          end;
        end;
      end;  //while

      // Now that we have broken out of the loop without an exception cause we received a $FF, we need to take the UTFString we received and convert it to a normal string.
      result := lSS.DataString;  // converting from UTF8 to wideChar string
    finally
      lSS.Free;
    end;
  end
  else
  begin
    // reading a defined-length text string, so red the expected length
    lSS:=TStringStream.Create('',TEncoding.UTF8);
    try
      lToReadCount := ReadLength(aStream, 3, aSubType);
      if lToReadCount>0 then
      begin
        lCount := lSS.CopyFrom(aStream, Int64(lToReadCount));
        result := lSS.DataString;  // converting from UTF8 to wideChar string

        // Note, we have read whatever number of UTF8 bytes we could above.  Here we will check to see if we did read what we were expected to read.
        // we have already accepted as much as we could into our object model, so generating this exception below will still leave what we did read intact.
        if lCount <> lToReadCount then
          RaiseParsingException(aStream, format(cExceptNotEnoughBytesTextString, [lToReadCount, lCount]));
      end;
    finally
      lSS.Free;
    end;
  end;
end;


function ReadObjectFromCBORStream(aStream: TStream; aCount: UInt64): TObject;
var
  i: UInt64;
  lMajorType: byte;
  lSubType: byte;
  lSlotName: string;
  lClassname: string;
  lInstance : TRttiInstanceType;
  lContext : TRttiContext;
  lType: TRttiType;
  mClass : TValue;

  function ReadClassName: string;
  var
    lMT: byte;
    lST: byte;
  begin
    // try to read a string (the classname) from the stream.  Except out if the cbor data being read is anything else but a string
    aStream.Read(lMT,1);
    lST := lMT and $1F;     // first 5 bits
    lMT := byte(lMT shr 5);     // get it to a 0-7 range
    if lMT = 3 then
    begin
      result := ReadTextString(aStream, lST);
    end
    else
      RaiseParsingException(aStream, format('Error when reading an object from a CBOR map.  Expected to read a string for the ClassName, but the data''s major data type was %d',[lMT]));
  end;

begin
  result := nil;
  // We are now reading a normal CBOR Map, but we are going to be trying to instantiate the correct object that it tells us by looking at the _Type slot and then
  // RTTIing the rest of the slots into the properties of the object
  for i := 0 to aCount-1 do
  begin
    // Need to read a string only for the key since the key is the property name in this case.
    // The CBOR spec allows for reading any type of CBOR data type as a key, but we only support strings here.
    aStream.Read(lMajorType,1);                // FINISH - If we ran out of stream bytes to read then we should have a different exception produced that what we get below which gives an incorrect exception message
    lSubType := lMajorType and $1F;     // first 5 bits
    lMajorType := byte(lMajorType shr 5);     // get it to a 0-7 range

    if lMajorType = 3 then
    begin
      // Read the Property name as we have a string.
      lSlotName := ReadTextString(aStream, lSubType);

      if assigned(result) then
      begin
        // apply the property to the object
      end
      else
      begin
        // if the slotname is "_Type", then we will be able to get the classname to instantiate.
        if CompareText(lSlotName, '_Type')=0 then
        begin
          lClassname := ReadClassname;

          // we have a classname successfully read from the _Type slot in the cbor map, so use it to instantiate an object.
          // First, find it and instantiate it using the RTTI.
          lContext := TRttiContext.Create;
          try
            lType := lContext.FindType(lClassname); //ClassName is something like  'Classes.TStringList';
            if assigned(lType) then
            begin
              lInstance := lType.AsInstance;
              mClass := lInstance.GetMethod('Create').Invoke(lInstance.MetaclassType,[]);
              result := mClass.AsObject;
            end;
          finally
            lContext.free;
          end;
        end;
      end;

      //FINISH - need to read the value and apply it to the Object.

     // self.AsFrame.NewSlot(lSlotName).ReadCborFromStream(aStream);     //NOTE:  recursion happening here.  maybe someday in the future we will put some kind of a nesting limit in to prevent stack overflow.
    end
    else
    begin
      // We can only accept strings as map (frame) keys, so generate an exception.
      RaiseParsingException(aStream, format('Can only read text strings as map keys when reading a map into an object instance.  Error, encountered datatype %d',[lMajorType]));
    end;
  end;
  //
end;






class function TCBORStreamer.GetFileFilter: string;
begin
  result := 'CBOR Files (*.cbor)|*.cbor';
end;

class function TCBORStreamer.IsFileExtension(aStr: string): boolean;
begin
  result := SameText(aStr, '.cbor') or SameText(aStr, 'cbor');
end;

class function TCBORStreamer.Name: string;
begin
  result := 'Compact Binary Object Representation';
end;

procedure TCBORStreamer.Encode(aDataObj: TDataObj);
var
  i: Integer;
  lCode: byte;
  lByte: byte;
  lStringList: TDataStringList;
  lFrame: TDataFrame;
  lArray: TDataArray;
  lSparseArray: TDataSparseArray;
  lBinary: TDataBinary;


begin
  case aDataObj.DataType.Code of
    cDataTypeNull: begin
      lCode := $F6;
      fStream.Write(lCode,1);          //Major 7, value=22;
    end;

    cDataTypeBoolean: begin
      if aDataObj.AsBoolean then
        WriteBooleanTrue(fStream)
      else
        WriteBooleanFalse(fStream);
    end;

    cDataTypeByte: begin
      lByte := aDataObj.AsByte;
      if lByte <= 23 then              // we inherently treat a byte as unsigned.
        fStream.Write(lByte, 1)        // Major=0, value is 0 - 23
      else
      begin
        lCode := 24;
        fStream.Write(lCode, 1);       // major = 0, additional=24 which means one byte follows. value is 24-255
        fStream.Write(lByte, 1);
      end;
    end;

    cDataTypeInt32: begin                     // we are treating the int32 as signed.
      WriteInt64(fStream, aDataObj.AsInt32);  // convert to 64 bit and write for easy code sharing
    end;

    cDataTypeInt64: begin
      WriteInt64(fStream, aDataObj.AsInt64);
    end;

    cDataTypeSingle: begin
      WriteSingle(fStream, aDataObj.AsSingle);
    end;

    cDataTypeDouble: begin
      WriteDouble(fStream, aDataObj.AsDouble);
    end;

//    cDataTypeDecimal128: aStringBuilder.Append(lStore.dataInt64);

    cDataTypeDateTime: begin
      WriteTag(fStream, 1); // Tag #1 means it's a dateTime.
      WriteInt64(fStream, aDataObj.AsUTCDateTime);
    end;

    // for the unix UTC time do we publish in string notation or in the int64 notation.    I say in64 notation since that's the purpose of this data type
    cDataTypeUTCDateTime: begin
      WriteTag(fStream, 1); // Tag #1 means it's a dateTime.
      WriteInt64(fStream, aDataObj.AsUTCDateTime);
    end;

    cDataTypeDate: begin
      // FINISH - hopefully, there would be a better Tag for this someday
      WriteTag(fStream, 1); // Tag #1 means it's a dateTime.
      WriteInt64(fStream, aDataObj.AsUTCDateTime);
    end;

    cDataTypeTime: begin
      // FINISH - hopefully, there would be a better Tag for this someday
      WriteTag(fStream, 1); // Tag #1 means it's a dateTime.
      WriteInt64(fStream, aDataObj.AsUTCDateTime);
    end;

    cDataTypeGUID: begin
      WriteTag(fStream, 37);                  // Tag 37 = UUID for binary
      WriteTypeAndNumber(fStream, 2, 16);    // Means writing binary that is 16 bytes long.
      fStream.Write(aDataObj.AsGUID.GUID, 16);    //Need to test.
    end;

    cDataTypeObjectID: begin
//      WriteTag(fStream, ???);     //FINISH - get a registered IANA tag for ObjectID
      WriteTypeAndNumber(fStream, 2, 12);    // Means writing binary that is 12 bytes long.
      fStream.Write(aDataObj.AsObjectID.Data[0], 12);         //need to test.
    end;

    cDataTypeString: begin
      WriteUnicodeString(fStream, aDataObj.AsString);
    end;

    cDataTypeStringList: begin
      // We are going to code the StringList data type as an array of Text Strings.
      // WriteTag(fStream, ???); // FINISH - let's get a registered tag reserved to define an array of strings.
      lStringList := aDataObj.AsStringList;
      WriteTypeAndNumber(fStream, 4, UInt64(lStringList.Count));
      for i := 0 to lStringList.Count-1 do
      begin
        WriteUnicodeString(fStream, lStringList.Strings[i]);
      end;
    end;

    cDataTypeFrame: begin
      lFrame := aDataObj.AsFrame;
      WriteTypeAndNumber(fStream, 5, UInt64(lFrame.Count));
      for i := 0 to lFrame.Count-1 do
      begin
        // write out the name - value pair.
        WriteUnicodeString(fStream, lFrame.slotName(i));
        Encode(lFrame.Slots[i]);        //recursion happening here
      end;
    end;

    cDataTypeArray: begin
      lArray := aDataObj.AsArray;
      WriteTypeAndNumber(fStream, 4, UInt64(lArray.Count));
      for i := 0 to lArray.Count-1 do
      begin
        Encode(lArray.Items[i]);        //recursion happening here
      end;
    end;

    cDataTypeSparseArray: begin
      lSparseArray := aDataObj.AsSparseArray;
      WriteTypeAndNumber(fStream, 5, UInt64(lSparseArray.Count));  // A Sparse Array and a Map are the same thing so they have the same type, but the difference is that maps use strings for keys and sparseArrays use integers for keys
      for i := 0 to lSparseArray.Count-1 do
      begin
        WriteInt64(fStream, lSparseArray.SlotIndex(i));    // FINISH - maybe we should only have positive number keys?  maybe they should be int64 instead of int32?
        Encode(lSparseArray.Items[i]);    //recursion happening here
      end;
    end;

    cDataTypeBinary: begin
      lBinary := aDataObj.AsBinary;
      WriteTypeAndNumber(fStream, 2, UInt64(lBinary.Size));        //byte string
      lBinary.seek(0, soBeginning);
      fStream.CopyFrom(lBinary, lBinary.Size);
    end;

    cDataTypeObject: begin
      // FINISH - Objects are basically just serialized the same as a frame.  However, we should have a Tag that defines this is a core Object.
      // also, we somehow need to force the classname in there in a consistent way so that the receiver can instantiate the right object.
      WriteTag(fStream, 27);
      WriteObjectToCBORStream(fStream, aDataObj.AsObject);
    end;

    cDataTypeTag: begin
      WriteTag(fStream, aDataObj.AsTag.TagValue);
      Encode(aDataobj.AsTag.DataObj);              // recursion happening here.
    end;
  end;
end;


class function TCBORStreamer.FileExtension: string;
begin
  result := 'cbor';
end;

class function TCBORStreamer.Description: string;
begin
  result := 'Compact Binary Object Representation. https://cbor.io/ and https://en.wikipedia.org/wiki/CBOR';
end;

// This is a helper routine that we use for reading some bytes from the stream where it will generate an exception for us
// if we could not read the number of bytes that were needed to be read.
procedure DoReadStream(aStream: TStream; var Buffer; Count: Longint);
var
  lDidReadCount: integer;
begin
  lDidReadCount := aStream.Read(Buffer, Count);
  if lDidReadCount <> Count then
  begin
    raise ECBORStreamerExceptionCantReadStream.Create('Unable to read '+IntToStr(Count)+' Bytes from stream when reading a CBOR Stream at position='+intToStr(aStream.Position));
  end;
end;


// This is a helper routine that we use for reading some bytes from the stream where it will generate an exception for us
// if we could not read the number of bytes that were needed to be read.
procedure TCBORStreamer.DoRead(var Buffer; Count: Longint);
begin
  DoReadStream(fStream, Buffer, Count);
end;


class function TCBORStreamer.Priority: cardinal;
begin
  result := 20;
end;


constructor TCBORStreamer.Create(aStream: TStream);
begin
  inherited;
  fSupportSimples := true;
end;

procedure TCBORStreamer.Decode(aDataObj: TDataObj);
begin
  DecodeInternal(aDataObj);
end;





// returns true if something was loaded.
// Returns false if Nothing was loaded which is the case when we decode a "break" stop code while reading items in an indefinite length array or frame.
function TCBORStreamer.DecodeInternal(aDataObj: TDataObj): boolean;
var
  lMajorType: byte;
  lSubType: byte;
  lSimpleValue: Byte;
  lShort: UInt16;
  lSingle: single;
  lDouble: double;
  lCardinal: Cardinal;
  lUInt64: UInt64;
  lInt64: Int64;
  lToReadCount: UInt64;
  lCount: Int64;
  i: UInt64;
  lSlotName: string;
  lObject: TObject;
  lBinary: TDataBinary;
  lExp: integer;
  lMant: integer;
  lDecodeResult: boolean;


  function Ldexp(aMantissa: Integer; aExponent: Integer): Double;
  begin
    Result := aMantissa * Power(2.0, aExponent);
  end;

  //This read Integer function is ONLY for reading the Integer value of a Key value
  // Note, in our implementation of a SparseArray, we support up to 64 bit integer keys, so we need to handle up to that level here.
  function ReadPositiveIntegerKey: Int64;
  var
    lUInt64: UInt64;
  begin
    lUInt64 := ReadLength(fStream, lMajorType, lSubType);
    if (lUInt64 > $7FFFFFFFFFFFFFFF) then
    begin
      RaiseParsingException(fStream, cExceptPositiveNumberTooBig);
    end;
    result := Int64(lUInt64);
  end;

  function ReadNegativeIntegerKey: Int64;
  var
    lUInt64: UInt64;
  begin
    lUInt64 := ReadLength(fStream, lMajorType, lSubType);
    if (lUInt64 > $7FFFFFFFFFFFFFFF) then
    begin
      RaiseParsingException(fStream, cExceptNegativeNumberTooBig);
    end;
    result := -1 - Int64(lUInt64);   // Now make it negative.
  end;

begin
  DoRead(lMajorType, 1);
  lSubType := lMajorType and $1F;     // first 5 bits
  lMajorType := byte(lMajorType shr 5);  // get it to a 0-7 range
  result := true;                  // assume that something will be loaded below.  Code below can change that back to false if otherwise.

  case lMajorType of
    0: begin
      // Reading an unsigned integer.  Number of bytes defined by the MajorTypes SubTypeCode
      case lSubType of
        0..23: begin
          aDataObj.AsByte := lSubtype;    // subtype just contains the unsigned int directly
        end;
        24: begin
          DoRead(lSimpleValue, 1);
          aDataObj.AsByte := lSimplevalue;  // numbers 24-255 are covered by reading one more byte
        end;
        25: begin
          DoRead(lShort, 2);
          aDataObj.AsInt32 := SwapBytes(lShort);     // numbers 256-65535 are covered by reading two bytes
        end;
        26: begin
          DoRead(lCardinal, 4);
          lCardinal := SwapBytes(lCardinal);
          if lCardinal > $7FFFFFFF then
            aDataObj.AsInt64 := lCardinal   // since the 32bit unsigned number is over the largest signed number possible, we must put into a 64bit integer.
          else
            aDataObj.AsInt32 := Integer(lCardinal);  // numbers 65536 -  2 billion are covered by reading four bytes
        end;
        27: begin
          DoRead(lUInt64, 8);
          lUInt64 := SwapBytes(lUInt64);
          if (lUInt64 > $7FFFFFFFFFFFFFFF) then
          begin
            aDataObj.AsUInt64 := lUInt64;  //  We now have full UInt64 support in DataObjects, so that's how we must store this since it's too big to store in Int64.
          end
          else
          begin
            // even though we are bringing in a clearly positive number, we are going to default to storing it as int64 and not UInt64 since we have room to do so.
            aDataObj.AsInt64 := Int64(lUInt64);
          end;
        end;
      end;
    end;

    1: begin
      // Reading a negative integer. Number of bytes defined by the MajorTypes SubTypeCode.  The real value is given by: (-1 - IncomingValue)
      case lSubType of
        0..23: begin
          aDataObj.AsInt32 := -1-lSubtype;    // subtype just contains the negative int directly
        end;
        24: begin
          DoRead(lSimpleValue, 1);
          aDataObj.AsInt32 := -1-lSimplevalue;  // numbers 24-255 are covered by reading one more byte.
        end;
        25: begin
          DoRead(lShort, 2);
          aDataObj.AsInt32 := -1-SwapBytes(lShort);     // numbers 256-65535 are covered by reading two bytes
        end;
        26: begin
          DoRead(lCardinal, 4);
          if lCardinal > $7FFFFFFF then
            aDataObj.AsInt64 := -1-Integer(lCardinal)  // numbers 65536 -  2 billion are covered by reading four bytes
          else
            aDataObj.AsInt32 := -1-Integer(lCardinal);  // numbers 65536 -  2 billion are covered by reading four bytes
        end;
        27: begin
          DoRead(lUInt64, 8);
          if lUInt64 > $7FFFFFFFFFFFFFFF then
          begin
            RaiseParsingException(fStream, cExceptNegativeNumberTooBig);
          end;
          aDataObj.AsInt64 := -1-Int64(lUInt64);     // 8 byte unsigned int64.  Problem we have here is that we natively model signed 64biters, so it's possible we get an incoming unsigned number that's too big here and an exception will be raised.
        end;
      end;
    end;

    2: begin
      // reading a byteString;
      if lSubType = 31 then
      begin
        // This is an indefinite length binary blob of bytes.  To read this, we are then reading and indefinite number of definite length chunks of binary data.  SO, what follows should be one or more
        // definite length byteStrings.  When it's done, we will receive an "end-Of-stream" marker which is major=7,sub=31.
        while true do
        begin
          DoRead(lMajorType, 1);
          if lMajorType=$FF then
          begin
            break;                          // we successfully hit the "break" stop code, so we are done reading chunks.
          end
          else
          begin
            // we should be reading another chunk which is a definite-length byteString.  Anything else is an error condition.
            lSubType := lMajorType and $1F;   // first 5 bits
            lMajorType := byte(lMajorType shr 5);   // get it to a 0-7 range

            if (lMajorType=2) and (lSubType <= 27) then
            begin
              // we are now given a definiteLength byteString that we need to append to the binary Stream.
              lToReadCount := ReadLength(fStream, lMajorType, lSubType);
              if lToReadCount>0 then
              begin
                lCount := aDataObj.AsBinary.CopyFrom(fStream, Int64(lToReadCount));
                if lCount <> lToReadCount then
                  RaiseParsingException(fStream, Format(cExceptNotEnoughBytesByteString, [lToReadCount, lCount]));
              end
              else
              begin
                //Technically, receiving a chunk that is zero in length is probably considered malformed.  Not really a big deal here, so no exception.  maybe we will raise exception in the future.
              end;
            end
            else
            begin
              // we either received a majorType that wasn't a byteString or it had a subType that was invalid (28-30 are undefined and 31=indefiniteLength is not valid cause all chunks must be a definite length chunk)
              if lMajorType=2 then
                RaiseParsingException(fStream, Format(cExceptInvalidByteStringChunkSubTypeBad,[lSubType]))
              else
                RaiseParsingException(fStream, Format(cExceptInvalidByteStringChunkMajorTypeBad,[lMajorType]));
            end;
          end;
        end;
      end
      else
      begin
        // reading a defined-length string of bytes.
        lToReadCount := ReadLength(fStream, 2, lSubType);
        lBinary := aDataObj.AsBinary;    // make it a binary slot even if no bytes are following to put into it.
        if lToReadCount > 0 then
        begin
          lCount := lBinary.CopyFrom(fStream, Int64(lToReadCount));
          if lCount <> lToReadCount then
            RaiseParsingException(fStream, format(cExceptNotEnoughBytesByteString, [lToReadCount, lCount]));
        end;
      end;
    end;

    3: begin
      aDataObj.AsString := ReadTextString(fStream, lSubType);
    end;

    4: begin
      // Reading an Array
      if lSubType = 31 then
      begin
        //Indefinite length array - keep reading objects until DecodeInternal returns false
        repeat
          lDecodeResult := DecodeInternal(aDataObj.AsArray.NewSlot);
        until lDecodeResult=false;
        // The last item that was added to the array to give to DecodeInternal was not filled with any data because lDecodeResult returned false, so we must free that last one which was never filled.
        aDataObj.AsArray.DeleteSlot(aDataObj.AsArray.count-1);
      end
      else
      begin
        lToReadCount := ReadLength(fStream, 4, lSubType);
        aDataObj.AsArray;     // Must have this here in order to get the dataObject assigned as an array even if we have no values to import
        for i := 1 to lToReadCount do
        begin
          Decode(aDataObj.AsArray.NewSlot);                 //NOTE:  recusion happening here.  maybe someday in the future we will put some kind of a nesting limit in to prevent stack overflow.
        end;
      end;
    end;

    5: begin
      //Reading a Map (Frame)
      if lSubType = 31 then
      begin
        //Indefinite length map - keep reading objects until DecodeInternal returns false
        repeat
          aDataObj.AsFrame;     // Must have this here in order to get the dataObject assigned as a Frame even if we have no values to import
          lDecodeResult := true;
          while lDecodeResult do
          begin
            // Need to read a string only for the key.  The spec allows for reading any type of CBOR data type as a key, but we only support strings here.
            // maybe we will also support numbers for the sparse array which uses numbers as the keys.
            DoRead(lMajorType,1);
            if lMajorType = $FF then
            begin
              lDecodeResult := false;
              break;    // we read a "break" stop code so get out of our indefinite loop.
            end;

            lSubType := lMajorType and $1F;     // first 5 bits
            lMajorType := byte(lMajorType shr 5);  // get it to a 0-7 range

            if lMajorType = 3 then
            begin
              // We are seeing a string, so read a string key value.
              lSlotName := ReadTextString(fStream, lSubType);
              lDecodeResult := DecodeInternal(aDataObj.AsFrame.NewSlot(lSlotName));   //NOTE:  recursion happening here.  maybe someday in the future we will put some kind of a nesting limit in to prevent stack overflow.
            end
            else if (lMajorType = 0) or (lMajorType = 1) then
            begin
              if lMajorType=0 then
                lInt64 := ReadPositiveIntegerKey
              else
                lInt64 := ReadNegativeIntegerKey;

              if aDataObj.Datatype.code = cDataTypeFrame then
              begin
                // If our container is already a frame, then we put this key in as a string representation of the Int64
                lDecodeResult := DecodeInternal(aDataObj.AsFrame.NewSlot(IntToStr(lInt64)));   //NOTE:  recursion happening here.  maybe someday in the future we will put some kind of a nesting limit in to prevent stack overflow.
              end
              else
              begin
                // If we are not a frame, then reading an integer as a key value makes a sparse Array.
                lDecodeResult := DecodeInternal(aDataObj.AsSparseArray.NewSlot(lInt64));   //NOTE:  recursion happening here.  maybe someday in the future we will put some kind of a nesting limit in to prevent stack overflow.
              end;
            end
            else
            begin
              // We can only accept strings as map (frame) keys, so generate an exception.
              RaiseParsingException(fStream, format('Can only read text strings or integers as map keys.  Error, encountered datatype %d',[lMajorType]));
            end;
          end;

        until lDecodeResult=false;

      end
      else
      begin
        lToReadCount := ReadLength(fStream, 5, lSubType);

        if fCurrentTagValue = 27 then
        begin
          // we are under a current "Object" tag which means we need to instantiate an object and use the RTTI to read the serialized frame(map) directly into the object's published properties.
          lObject := ReadObjectFromCBORStream(fStream, lToReadCount);
          if assigned(lObject) then
          begin
            // if the reading from the CBOR stream resulted in instantiating an actual TObject, then we will get it called here so we can add it to this TDataObj
            aDataObj.AsObject := lObject;
          end;
        end
        else
        begin
          //We are now reading a normal TDataFrame
          aDataObj.AsFrame;     // Must have this here in order to get the dataObject assigned as a Frame even if we have no values to import
          for i := 1 to lToReadCount do
          begin
            // Need to read a string or integer only for the key.  The spec allows for reading any type of CBOR data type as a key, but we only support strings and integers here.
            // maybe we will also support numbers for the sparse array which uses numbers as the keys.
            DoRead(lMajorType,1);
            lSubType := lMajorType and $1F;     // first 5 bits
            lMajorType := byte(lMajorType shr 5);  // get it to a 0-7 range

            if lMajorType = 3 then
            begin
              lSlotName := ReadTextString(fStream, lSubType);
              Decode(aDataObj.AsFrame.NewSlot(lSlotName));   //NOTE:  recursion happening here.  maybe someday in the future we will put some kind of a nesting limit in to prevent stack overflow.
            end
            else if (lMajorType = 0) or (lMajorType = 1) then
            begin
              if lMajorType=0 then
                lInt64 := ReadPositiveIntegerKey
              else
                lInt64 := ReadNegativeIntegerKey;

              if aDataObj.Datatype.code = cDataTypeFrame then
              begin
                // If our container is already a frame, then we put this key in as a string representation of the Int64
                DecodeInternal(aDataObj.AsFrame.NewSlot(IntToStr(lInt64)));   //NOTE:  recursion happening here.  maybe someday in the future we will put some kind of a nesting limit in to prevent stack overflow.
              end
              else
              begin
                // If we are not a frame, then reading an integer as a key value makes a sparse Array.
                DecodeInternal(aDataObj.AsSparseArray.NewSlot(lInt64));   //NOTE:  recursion happening here.  maybe someday in the future we will put some kind of a nesting limit in to prevent stack overflow.
              end;
            end
            else
            begin
              // We can only accept strings as map (frame) keys, so generate an exception.
              RaiseParsingException(fStream, format('Can only read text strings as map keys.  Error, encountered datatype %d',[lMajorType]));
            end;
          end;
        end;
      end;

    end;

    6: begin
      // Reading a Tag
      fCurrentTagValue := ReadLength(fStream, 6, lSubType);   // this returns the tag number.
      aDataObj.AsTag.TagValue := fCurrentTagValue;
      Decode(aDataObj.AsTag.DataObj);                         // recursion happening here.

      // It is possible that subsequent data loadings could be treated differently because they are following this tag
      // So, we will get this value passed down to subsequent decodes by saving it to a current tag value.
    end;

    7: begin
      // floating-point numbers and simple data types that need no content, as well as the "break" stop code
      case lSubType of
        0..19: begin
          if self.fSupportSimples then
          begin
            aDataObj.AsByte := lSubType;
            aDataObj.DataType.SubClass := cSubCodeSimple;   // This code means we have a byte stored in the DataObject, but it's classified as a "Simple", which means we can re-encode this and again produce a "simple" value.
          end
          else
          begin
            RaiseParsingException(fStream, format(cExceptInvalidSubTypeForTypeCode7,[lSubtype]));
          end;
        end;
        20: begin
          aDataObj.AsBoolean := false;
        end;
        21: begin
          aDataObj.AsBoolean := true;
        end;
        22: begin
          aDataObj.Clear;   // makes it null
        end;
        23: begin
          if self.fSupportSimples then
          begin
            aDataObj.AsByte := lSubType;
            aDataObj.DataType.SubClass := cSubCodeSimple;   // This code means we have a byte stored in the DataObject, but it's classified as a "Simple", which means we can re-encode this and again produce a "simple" value.
          end
          else
          begin
             // this simple value code is considered "undefined" and thus, if we see it, we should get an exception.
            RaiseParsingException(fStream, format(cExceptInvalidSubTypeForTypeCode7,[lSubtype]));
          end;
        end;
        24: begin
          // Simple value in range of 32-255 so we need to read another byte from the stream.
          DoRead(lSimpleValue, 1);
          if self.fSupportSimples then
          begin
            aDataObj.AsByte := lSimpleValue;
            aDataObj.DataType.SubClass := cSubCodeSimple;   // This code means we have a byte stored in the DataObject, but it's classified as a "Simple", which means we can re-encode this and again produce a "simple" value.
          end
          else
          begin
            case lSimpleValue of
              0..31: begin
                //Invalid because these values should have come with the prior MajorType byte.
                RaiseParsingException(fStream, Format(cExceptInvalidSimpleTypeForTypeCode7,[lSimpleValue]));
              end;
              32..255: begin
                // read a simpleValue but these aren't really defined yet
                RaiseParsingException(fStream, Format(cExceptInvalidSimpleTypeForTypeCode7,[lSimpleValue]));
              end;
            end;
          end;
        end;
        25: begin
          // read two more bytes for a half-precision 16 bit float.  Delphi doesn't have this data type.  FINISH converting this someday in the future.
          DoRead(lShort, 2);
          lShort := SwapBytes(lShort);

          lExp := (lShort shr 10) and $1f;
          lMant := lShort and $3ff;
          if lExp = 0 then
            lSingle :=   lDexp(lMant, -24)
          else if lExp <> 31 then
            lSingle := lDexp(lMant+1024, lExp-25)
          else if lMant = 0 then
            lSingle := Single.PositiveInfinity
          else
            lSingle := Single.NaN;
          if (lShort and $8000)<>0 then   // if MSB is set, then we are negative.
            lSingle := -lSingle;

          aDataObj.AsSingle := lSingle;

          (* The following C code will decode the lShort 16 bit unsigned int into a double float.  FINISH implementing this into a delphi single or double float
           double decode_half(unsigned char *halfp) {
             int half = (halfp[0] << 8) + halfp[1];
             int exp = (half >> 10) & 0x1f;
             int mant = half & 0x3ff;
             double val;
             if (exp == 0) val = ldexp(mant, -24);
             else if (exp != 31) val = ldexp(mant + 1024, exp - 25);
             else val = mant == 0 ? INFINITY : NAN;
             return half & 0x8000 ? -val : val;
           } *)
        end;
        26: begin
          // read four more bytes for a 32 bit float
          DoRead(lCardinal, 4);
          lSingle := SwapBytesSingle(lCardinal);
          aDataObj.AsSingle := lSingle;
        end;
        27: begin
          // read 8 more bytes for a 64 bit float
          DoRead(lUInt64, 8);
          lDouble := SwapBytesToDouble(lUInt64);
          aDataObj.AsDouble := lDouble;
        end;
        28,29,30: begin  //unassigned
          if self.fSupportSimples then
          begin
            aDataObj.AsByte := lSubType;
            aDataObj.DataType.SubClass := cSubCodeSimple;   // This code means we have a byte stored in the DataObject, but it's classified as a "Simple", which means we can re-encode this and again produce a "simple" value.
          end
          else
          begin
            RaiseParsingException(fStream, format(cExceptInvalidSubTypeForTypeCode7,[lSubtype]));          //31=break for indefinite-length items.   should never see it here.
          end;
        end;
        31: begin
          // This is a special code that is the "Break" stop code when inside an indefinite length array or map.  So, in this case, return false to signal that nothing was loaded. ;
          result := false;   // nothing loaded, return false to signal a break.
        end
      else
        RaiseParsingException(fStream, format(cExceptInvalidSubTypeForTypeCode7, [lSubType]));
      end;
    end;

  end;
end;


initialization
  // Once we call this, it creates a context and keeps it around forever to re-use.
  gRttiContext:=TRttiContext.Create;

  RegisterDataObjStreamer(TCBORStreamer);

finalization
  gRttiContext.Free;

end.
