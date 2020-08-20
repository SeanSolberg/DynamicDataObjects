unit DataObjectsUtils;

interface

uses DateUtils, SysUtils;

{$define cPurePascal}

type
  TBigTimestampValue = packed record
    N: byte;
    ExtType: shortInt;
    NanoSeconds: Cardinal;
    Seconds: int64;
  end;

  // This record gives us the ability to write a buffer of data where we have a 1-byte code followed by either a 1 byte value, 2 byte value,
  // 4 byte value, 8 byte value, or a TBigTimeStamp value.   When using this, we call the right GetFunctions or SetProcedures and internally
  // the value is stored in BigEndian form and not LittleEndian form.   That way, we can put data into this structure and then follow it up with
  // a stream write call.  Or, we can do a stream read call and then read the data from this structure and this record handles the proper byte swapping.
  // Note that this mechanism can be used by multiple different serializers.
  TNumBytes = packed record
    Code: byte;

    procedure SetByteValue(aValue: byte);
    procedure SetIntValue(aValue: integer);
    procedure SetSingleValue(aValue: Single);
    procedure SetInt64Value(aValue: Int64);
    procedure SetDoubleValue(aValue: double);
    procedure SetUnsignedIntValue(aValue: Cardinal);
    procedure SetUnsignedInt16Value(aValue: Word);
    procedure SetBigTimestampValue(aSeconds: int64; aNanoSeconds: Cardinal);
  strict private
    //    fBytes: array[0..7] of byte;
    case byte of
      0: (fByteValue: byte;);
      1: (fIntValue: Integer;);  //signed
      2: (fSingleValue: Single;);
      3: (fInt64Value: Int64;);  //signed
      4: (fDoubleValue: Double;);
      5: (fUnsignedIntValue: Cardinal;);  // unsigned 32 bit
      6: (fUInt16Value: Word);
      7: (fBigTimestampValue: TBigTimestampValue);
    end;



function SwapBytes(aDouble: double): double; overload; register;

function SwapBytes(aSingle: single): single; overload; register;

function SwapBytes(aInt64: UInt64): UInt64; overload; register;

function SwapBytes(aInt64: Int64): Int64; overload; register;

function SwapBytes(aValue: Int32): Int32; overload; register;

function SwapBytes(aValue: UInt32): UInt32; overload; register;

function SwapBytes(aValue: UInt16): UInt16; overload; register;

function SwapBytes(aValue: Int16): Int16; overload; register;

function DateTimeToISO8601Str(aDateTime: TDateTime): String;

function DateToISO8601Str(aDateTime: TDateTime): String;

function TimeToISO8601Str(aDateTime: TDateTime): String;


implementation


function SwapBytes(aDouble: double): double; overload; register;
type
  //enumeration used in variant record
  BytePos = (EndVal, ByteVal);

  PEndianCnvRec = ^EndianCnvRec;

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      EndVal: (EndianVal: double);
       //Overlapping bytes of the double
      ByteVal: (Bytes: array[0..SizeOf(Double)-1] of byte);
  end;
var
  a,b: EndianCnvRec;
begin
  a.EndianVal := aDouble;
  B.Bytes[0] := A.Bytes[7];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[6];
  B.Bytes[2] := A.Bytes[5];
  B.Bytes[3] := A.Bytes[4];
  B.Bytes[4] := A.Bytes[3];
  B.Bytes[5] := A.Bytes[2];
  B.Bytes[6] := A.Bytes[1];
  B.Bytes[7] := A.Bytes[0];
  result := b.EndianVal;
end;

function SwapBytes(aSingle: single): single; overload; register;
type
  //enumeration used in variant record
  BytePos = (EndVal, ByteVal);

  PEndianCnvRec = ^EndianCnvRec;

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      EndVal: (EndianVal: single);
       //Overlapping bytes of the double
      ByteVal: (Bytes: array[0..SizeOf(single)-1] of byte);
  end;
var
  a,b: EndianCnvRec;
begin
  a.EndianVal := aSingle;
  B.Bytes[0] := A.Bytes[3];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[2];
  B.Bytes[2] := A.Bytes[1];
  B.Bytes[3] := A.Bytes[0];
  result := b.EndianVal;
end;


function SwapBytes(aInt64: UInt64): UInt64; overload; register;
type
  //enumeration used in variant record
  BytePos = (EndVal, ByteVal);

  PEndianCnvRec = ^EndianCnvRec;

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      EndVal: (EndianVal: UInt64);
       //Overlapping bytes of the double
      ByteVal: (Bytes: array[0..SizeOf(UInt64)-1] of byte);
  end;
var
  a,b: EndianCnvRec;
begin
  a.EndianVal := aInt64;
  B.Bytes[0] := A.Bytes[7];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[6];
  B.Bytes[2] := A.Bytes[5];
  B.Bytes[3] := A.Bytes[4];
  B.Bytes[4] := A.Bytes[3];
  B.Bytes[5] := A.Bytes[2];
  B.Bytes[6] := A.Bytes[1];
  B.Bytes[7] := A.Bytes[0];
  result := b.EndianVal;
end;

function SwapBytes(aInt64: Int64): Int64; overload; register;
type
  //enumeration used in variant record
  BytePos = (EndVal, ByteVal);

  PEndianCnvRec = ^EndianCnvRec;

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      EndVal: (EndianVal: Int64);
       //Overlapping bytes of the int64
      ByteVal: (Bytes: array[0..SizeOf(Int64)-1] of byte);
  end;
var
  a,b: EndianCnvRec;
begin
  a.EndianVal := aInt64;
  B.Bytes[0] := A.Bytes[7];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[6];
  B.Bytes[2] := A.Bytes[5];
  B.Bytes[3] := A.Bytes[4];
  B.Bytes[4] := A.Bytes[3];
  B.Bytes[5] := A.Bytes[2];
  B.Bytes[6] := A.Bytes[1];
  B.Bytes[7] := A.Bytes[0];
  result := b.EndianVal;
end;

{$ifdef cPurePascal}
function SwapBytes(aValue: UInt16): UInt16; overload; register;
begin
  result := (aValue shr 8) or (aValue shl 8);
end;

function SwapBytes(aValue: Int32): Int32; overload; register;
type
  //enumeration used in variant record
  BytePos = (EndVal, ByteVal);

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      EndVal: (EndianVal: Int32);
       //Overlapping bytes of the Int32
      ByteVal: (Bytes: array[0..SizeOf(Int32)-1] of byte);
  end;
var
  a,b: EndianCnvRec;
begin
  a.EndianVal := aValue;
  B.Bytes[0] := A.Bytes[3];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[2];
  B.Bytes[2] := A.Bytes[1];
  B.Bytes[3] := A.Bytes[0];
  result := b.EndianVal;
end;

function SwapBytes(aValue: UInt32): UInt32; overload; register;
type
  //enumeration used in variant record
  BytePos = (EndVal, ByteVal);

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      EndVal: (EndianVal: UInt32);
       //Overlapping bytes of the UInt32
      ByteVal: (Bytes: array[0..SizeOf(UInt32)-1] of byte);
  end;
var
  a,b: EndianCnvRec;
begin
  a.EndianVal := aValue;
  B.Bytes[0] := A.Bytes[3];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[2];
  B.Bytes[2] := A.Bytes[1];
  B.Bytes[3] := A.Bytes[0];
  result := b.EndianVal;
end;

{$else}
  {$ifdef WIN64}
    function SwapBytes(aValue: Int32): Int32; overload; register;
    asm
      bswap ecx       // param comes in in ecx
      mov eax, ecx;   // result goes out in eax
    end;

    function SwapBytes(aValue: UInt32): UInt32; overload; register;
    asm
      bswap ecx       // param comes in in ecx
      mov eax, ecx;   // result goes out in eax
    end;

    function SwapBytes(aValue: UInt16): UInt16; overload; register;
    asm
      rol cx, 8       // param comes in in ecx
      mov eax, ecx;   // result goes out in eax
    end;
  {$else}
    // 32 bit versions of these swap bytes
    function SwapBytes(aValue: Int32): Int32; overload; register;
    asm
      bswap eax      // param comes in in eax, result goes out in eax.
    end;

    function SwapBytes(aValue: UInt32): UInt32; overload; register;
    asm
      bswap eax     // param comes in in eax, result goes out in eax.
    end;

    function SwapBytes(aValue: UInt16): UInt16; overload; register;
    asm
      rol ax, 8      // param comes in in eax, result goes out in eax.
    end;
  {$endif}
{$endif}

function SwapBytes(aValue: Int16): Int16; overload; register;
type
  //enumeration used in variant record
  BytePos = (EndVal, ByteVal);

  PEndianCnvRec = ^EndianCnvRec;

  EndianCnvRec = packed record
    case pos: BytePos of
       //The value we are trying to convert
      EndVal: (EndianVal: smallInt);
       //Overlapping bytes of the double
      ByteVal: (Bytes: array[0..SizeOf(smallInt)-1] of byte);
  end;
var
  a,b: EndianCnvRec;
begin
  a.EndianVal := aValue;
  B.Bytes[0] := A.Bytes[1];       // turns out this technique is faster than the others.
  B.Bytes[1] := A.Bytes[0];
  result := b.EndianVal;
end;

function DateTimeToISO8601Str(aDateTime: TDateTime): String;
var
  lYears: Word;
  lMonths: Word;
  lDays: Word;
  lHours: Word;
  lMinutes: Word;
  lSeconds: Word;
  lMilliSeconds: Word;
begin
  DecodeDateTime(aDateTime, lYears, lMonths, lDays, lHours, lMinutes, lSeconds, lMilliSeconds);
  result := Format('%.4d-%.2d-%.2dT%.2d%s%.2d%s%.2d%s%.3dZ', [lYears, lMonths, lDays, lHours, ':', lMinutes, ':', lSeconds, '.', lMilliSeconds]);
end;

function DateToISO8601Str(aDateTime: TDateTime): String;
var
  lYears: Word;
  lMonths: Word;
  lDays: Word;
  lHours: Word;
  lMinutes: Word;
  lSeconds: Word;
  lMilliSeconds: Word;
begin
  DecodeDateTime(aDateTime, lYears, lMonths, lDays, lHours, lMinutes, lSeconds, lMilliSeconds);
  result := Format('%.4d-%.2d-%.2d', [lYears, lMonths, lDays]);
end;

function TimeToISO8601Str(aDateTime: TDateTime): String;
var
  lYears: Word;
  lMonths: Word;
  lDays: Word;
  lHours: Word;
  lMinutes: Word;
  lSeconds: Word;
  lMilliSeconds: Word;
begin
  DecodeDateTime(aDateTime, lYears, lMonths, lDays, lHours, lMinutes, lSeconds, lMilliSeconds);
  result := Format('%.2d%s%.2d%s%.2d%s%.3dZ', [lHours, ':', lMinutes, ':', lSeconds, '.', lMilliSeconds]);
end;

{ TNumBytes }
procedure TNumBytes.SetByteValue(aValue: byte);
begin
  fByteValue := aValue;
end;

procedure TNumBytes.SetIntValue(aValue: Integer);
begin
  fIntValue := SwapBytes(aValue);
end;

procedure TNumBytes.SetSingleValue(aValue: Single);
begin
  fSingleValue := SwapBytes(aValue);
end;

procedure TNumBytes.SetInt64Value(aValue: Int64);
begin
  fInt64Value := SwapBytes(aValue);
end;

procedure TNumBytes.SetDoubleValue(aValue: double);
begin
  fDoubleValue := SwapBytes(aValue);
end;

procedure TNumBytes.SetUnsignedIntValue(aValue: Cardinal);
begin
  fUnsignedIntValue := SwapBytes(aValue);
end;

procedure TNumBytes.SetUnsignedInt16Value(aValue: Word);
begin
  fUInt16Value := SwapBytes(aValue);
end;

procedure TNumBytes.SetBigTimestampValue(aSeconds: int64; aNanoSeconds: Cardinal);
begin
  fBigTimestampValue.N := 12;
  fBigTimeStampValue.ExtType := -1;
  fBigTimestampValue.NanoSeconds := SwapBytes(aNanoSeconds);
  fBigTimestampValue.Seconds := SwapBytes(aSeconds);
end;


end.
