unit VarInt;

interface

uses sysUtils, classes, math;

(* This code implements a 64 bit VarInt as defined by Google's protocol buffers
   as well as a 64 bit Unsigned VarInt that is the same except it doesn't do the zigZag Encoding part.

   When the number is serialized, it will produce 1 to 10 bytes to represent the number.
   The MSB in each serialized byte defines if the byte is an intermediate byte or the last byte.
   1 means there are more bytes that follow
   0 means that this byte is the last byte in the stream for this VarInt value.

   for a full 64 bit integer, the MSB is the negative bit.  This bit is rotated to the LSB before serializing
   so that small negative numbers only serialize with one byte.  I believe they call this zig-zag encoding.
   So for the 7 bits that make up the number, we can only go from
   -64 to 63 within the 1 byte serialization.
   -8192 to 8191 within the 2 byte serialization, etc.
*)

type
  // signed VarInt with zig-zag encoding
  TVarInt64 = record
  private
    Value: int64;
  public
    class function ZigZagEncode64(aValue: int64): Uint64; static;
    class function ZigZagDecode64(aValue: Uint64): int64; static;

    procedure WriteToStream(aStream: TStream);
    procedure ReadFromStream(aStream: TStream);

    class operator Implicit(aValue: Int64): TVarInt64;
    class operator Implicit(aValue: TVarInt64): Int64;
    class operator Implicit(aValue: LongInt): TVarInt64;
    class operator Implicit(aValue: TVarInt64): LongInt;
  end;

  // Unsigned VarInt without zig-zag encoding
  TUVarInt64 = record
  private
    Value: UInt64;
  public
    procedure WriteToStream(aStream: TStream);
    procedure ReadFromStream(aStream: TStream);

    class operator Implicit(aValue: UInt64): TUVarInt64;
    class operator Implicit(aValue: TUVarInt64): UInt64;
    class operator Implicit(aValue: Cardinal): TUVarInt64;
    class operator Implicit(aValue: TUVarInt64): Cardinal;
  end;


  EVarIntException = class(Exception);

ResourceString
  strUnableToReadByte = 'Unable to read a byte from a stream when reading a VarInt.';
  strUnableToReadNumTooBig = 'Error reading VarInt from a stream.  Number is too big for Int64';


implementation

{ TVarInt64 }

//Encode the signed int64 bit number into a zig-zag encoded unsigned 64 number.
//NOTE:  This method can not handle one number and that is the largest negative number. (also considered negative zero)
//       It's hexadecimal representation  $8000000000000000.  It can't be handled because there is no equivalent to a positive number possible.
class function TVarInt64.ZigZagEncode64(aValue: int64): Uint64;
begin
(* Both of these methods produce the same results.
  if aValue < 0 then
  begin
    result := ((not Uint64(aValue)+1) shl 1) or $1;    // flip the bits first, then shift and set the lsb "negative" bit
  end
  else
  begin
    result := aValue shl 1;                  // negative bit doesn't apply to this positive number so just shift.
  end;    *)

  //This method can not handle one number and that is the largest negative number. (also considered negative zero)
  result := abs(aValue) shl 1;
  if aValue < 0 then
    result := result or $1;
end;

class function TVarInt64.ZigZagDecode64(aValue: Uint64): int64;
begin
  result := (aValue shr 1);
  if (aValue and $1) = 1 then
    result := -result;
end;

class operator TVarInt64.Implicit(aValue: Int64): TVarInt64;
begin
  result.Value := aValue;
end;

class operator TVarInt64.Implicit(aValue: TVarInt64): Int64;
begin
  result := aValue.Value;
end;

class operator TVarInt64.Implicit(aValue: Integer): TVarInt64;
begin
  result.Value := aValue;
end;

class operator TVarInt64.Implicit(aValue: TVarInt64): Integer;
begin
  result := aValue.Value;
end;

procedure TVarInt64.ReadFromStream(aStream: TStream);
var
  lCount: byte;  //number of bytes read so far.
  lByte: byte;
  lValue: Uint64;
  lShifter: byte;
begin
  lCount := 0;
  lValue := 0;
  lShifter := 0;
  repeat
    if aStream.Read(lByte, 1) = 0 then
      raise EVarIntException.Create(strUnableToReadByte);

    if lCount<9 then
    begin
      lValue := lValue or (Uint64(lByte and $7F) shl lShifter);
      lShifter := lShifter + 7;
    end
    else
    begin
      // read the final (10th) byte, but we can only use one bit from it.  9*7=63 bits, so we only need one more bit from the 10th byte.
      if lByte > 1 then
        raise Exception.Create(strUnableToReadNumTooBig)
      else if lByte = 1 then
        lValue := lValue + $8000000000000000;
    end;
    inc(lCount);
  until (lByte and $80) = 0;

  Value := ZigZagDecode64(lValue);
end;

procedure TVarInt64.WriteToStream(aStream: TStream);
var
  lBuffer: array[0..9] of byte;
  lValue: Uint64;
  i: byte;
begin
  i:=0;
  lValue := ZigZagEncode64(Value);
  while true do
  begin
    lBuffer[i] := lValue and $7F;    // take only 7 bits
    if lValue < $80 then
      break
    else
    begin
      lBuffer[i] := lbuffer[i] or $80;  // set the msb of this byte to signal that there are more bytes to follow.
      inc(i);
      lValue := lValue shr 7;           // more bytes to follow so prepare to pickup the next 7 bits.
    end;
  end;
  aStream.write(lBuffer,i+1);
end;



{ TUVarInt64 }

class operator TUVarInt64.Implicit(aValue: UInt64): TUVarInt64;
begin
  result.Value := aValue;
end;

class operator TUVarInt64.Implicit(aValue: TUVarInt64): UInt64;
begin
  result := aValue.Value;
end;

class operator TUVarInt64.Implicit(aValue: Cardinal): TUVarInt64;
begin
  result.Value := aValue;
end;

class operator TUVarInt64.Implicit(aValue: TUVarInt64): Cardinal;
begin
  result := aValue.Value;
end;

procedure TUVarInt64.ReadFromStream(aStream: TStream);
var
  lCount: byte;  //number of bytes read so far.
  lByte: byte;
  lShifter: byte;
begin
  lCount := 0;
  Value := 0;
  lShifter := 0;
  repeat
    if aStream.Read(lByte, 1) = 0 then
      raise EVarIntException.Create(strUnableToReadByte);

    if lCount<9 then
    begin
      Value := Value or (Uint64(lByte and $7F) shl lShifter);
      lShifter := lShifter + 7;
    end
    else
    begin
      // read the final (10th) byte, but we can only use one bit from it.  9*7=63 bits, so we only need one more bit from the 10th byte.
      if lByte > 1 then
        raise Exception.Create(strUnableToReadNumTooBig)
      else if lByte = 1 then
        Value := Value + $8000000000000000;
    end;
    inc(lCount);
  until (lByte and $80) = 0;

end;

procedure TUVarInt64.WriteToStream(aStream: TStream);
var
  lBuffer: array[0..9] of byte;
  lValue: Uint64;
  i: byte;
begin
  i:=0;
  lValue := Value;
  while true do
  begin
    lBuffer[i] := lValue and $7F;    // take only 7 bits
    if lValue < $80 then
      break
    else
    begin
      lBuffer[i] := lbuffer[i] or $80;  // set the msb of this byte to signal that there are more bytes to follow.
      inc(i);
      lValue := lValue shr 7;           // more bytes to follow so prepare to pickup the next 7 bits.
    end;
  end;
  aStream.write(lBuffer,i+1);
end;

end.
