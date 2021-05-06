unit DataObjects2JSON;

(* This unit provides the ability to serialize dataObjects to/from JSON *)


// TODO:  check that the Dates, DateTimes, Times are put to JSON in the ISO format?  Or are there options for the format encoding/decoding

interface

uses DataObjects2, DataObjects2Streamers, SysUtils, Classes, DataObjects2Utils;

type
  TObjectBase = TObject;


  TJsonStyle = (cJsonTight, cJsonHumanReadable);
  TJsonFormat = record
    Style: TJsonStyle;
    Indent: integer;
    class operator Implicit(aValue: TJsonStyle): TJsonFormat;
    class operator Implicit(aValue: TJsonFormat): TJsonStyle;
    function IncInd: string;   // increment the indent
    function DecInd: string;   // decrement the indent
  end;

  TDataObjectsJSON = class helper for DataObjects2.TDataObj
  private
    procedure GetAsJSONInternal(aStringBuilder: TStringBuilder; var aFormat: TJsonFormat);
  public
    function GetAsJSON: string;
  end;


  TJsonStreamer = class(TDataObjStreamerBase)
  private
    fStyle: TJsonStyle;
    fIndention: byte;
    fIndent: integer;                 // only used during streaming.
    fStringBuilder: TStringBuilder;
    fFormatSettings: TFormatSettings;

    fJSON: string;
    fEncodeNonAsciiCharacters: boolean;    // only used when streaming out.

    fEncoding: TEncoding;

    procedure SetEncoding(aEncoding: TEncoding);

    procedure ReadFromDataObjInternal(aDataObj: TDataObj);
    function IncInd: string;   // increment the indent
    function DecInd: string;
    procedure parseFromJson(aObj: TDataObj);


  public
    function Clone: TDataObjStreamerBase; override;
    constructor Create(aStream: TStream); overload; override;
    constructor Create(aStyle: TJsonStyle = cJSONTight; aIndention: byte = 2); overload;
    constructor Create(aStream: TStream; aEncoding: TEncoding; aStyle: TJsonStyle = cJSONTight; aIndention: byte = 2); overload;
    destructor Destroy; override;

    class function FileExtension: string; override;
    class function GetFileFilter: string; override;
    class function IsFileExtension(aStr: string): boolean; override;
    class function ClipboardPriority: cardinal; override;

    procedure Decode(aDataObj: TDataObj); override;
    procedure Encode(aDataObj: TDataObj); override;
    procedure ApplyOptionalParameters(aParams: TStrings); override;

    property Style: TJsonStyle read fStyle write fStyle;

    // defines how many spaces are inserted per indention level when Style is cJsonHumanReadable
    property Indention: byte read fIndention write fIndention;

    property EncodeNonAsciiCharacters: boolean read fEncodeNonAsciiCharacters write fEncodeNonAsciiCharacters;
    property Encoding: TEncoding read fEncoding write setEncoding;

    property JSON: string read fJSON write fJSON;

    class function DataObjToJson(aDataObj: TDataObj): string;
    class procedure JsonToDataObj(aJson: string; aDataObj: TDataObj);
  end;

  // Will escape aStr to be compatible with the escaping requirements of JSON.
  // If aEscapeAscii is true, then characters above characer code 127 will be escaped too.  The only reason to do this is if
  // you are going to be putting the resulting string into something that can't fully represent the unicode characters (ie ascii string).
  // NOTE:  There are some other places where using JSON strings that contain characters above 127 don't work so good.  I've seen
  //        comments on the web and u2028 and u2029 aren't useable to feed into javascript, but I played with that a bit and it worked for me in chrome so really I don't know.
  //        I see no reason to escape non-Ascii characters but I left this parameter in the code in case someone out there finds a reason to use this function with it set to true.
  function EncodeSpecialJSONCharacters(aStr: string; aEscapeNonAscii: boolean = false): string;


implementation

uses DateUtils, IdCoderMIME;

function EncodeSpecialJSONCharacters(aStr: string; aEscapeNonAscii: boolean): string;
var
  i: Integer;
  lChar: char;
  lStart: Integer;
  lUnicodeValue: integer;
  lLen: integer;

  type TInt15 = 0..15;

  const cHexChars = '0123456789ABCDEF';

  function Hex(const aDigit: TInt15): char;
  begin
    Result := char(cHexChars.Chars[aDigit]);
  end;

begin
  result := '';
  lStart := 1;
  lLen := length(aStr);
  for i := 1 to lLen do
  begin
    lChar := aStr[i];
    case lChar of
      '"':
        begin
          result := result + copy(aStr, lStart, i-lStart) + '\"';
          lStart := i+1;
        end;
      '\':
        begin
          result := result + copy(aStr, lStart, i-lStart) + '\\';
          lStart := i+1;
        end;
{     NOTE THAT WE DO NOT NEED TO ESCAPE FORWARD SLASHES.
      '/':
        begin
          result := result + copy(aStr, lStart, i-lStart) + '\/';
          lStart := i+1;
        end;}
      #$8:
        begin
          result := result + copy(aStr, lStart, i-lStart) + '\b';
          lStart := i+1;
        end;
      #$c:
        begin
          result := result + copy(aStr, lStart, i-lStart) + '\f';
          lStart := i+1;
        end;
      #$a:
        begin
          result := result + copy(aStr, lStart, i-lStart) + '\n';
          lStart := i+1;
        end;
      #$d:
        begin
          result := result + copy(aStr, lStart, i-lStart) + '\r';
          lStart := i+1;
        end;
      #$9:
        begin
          result := result + copy(aStr, lStart, i-lStart) + '\t';
          lStart := i+1;
        end;
      else
      begin
        if (lChar < WideChar(32)) then
        begin
          lUnicodeValue := Ord(lChar);
          result := result + copy(aStr, lStart, i-lStart) + '\u00' + Hex((lUnicodeValue and $F0) shr 4) + Hex(lUnicodeValue and $F);
          lStart := i+1;
        end

        else if (aEscapeNonAscii and (lChar > WideChar(127))) then
        begin
          // I've seen some parses always escape the non-ascii characters, but there's no reason to do so as long as what is produces is transferred using an encoding
          // that is fully compatible with unicode such as 2-byte unicode or variable byte UTF-8.   But, this code is here if someone out there wants to encode non-ascii characters.
          lUnicodeValue := Ord(lChar);
          result := result + copy(aStr, lStart, i-lStart) + '\u' + Hex((lUnicodeValue and $F000) shr 12) + Hex((lUnicodeValue and $F00) shr 8) + Hex((lUnicodeValue and $F0) shr 4) + Hex(lUnicodeValue and $F);
          lStart := i+1;
        end;
      end;
    end;

  end;
  result := result + copy(aStr, lStart, lLen-lStart+1);
end;



function Base64toJSONText(aStream: TStream): string;
var
  lEncodedStr: string;
  Encoder : TIdEncoderMime;
begin
  if assigned(aStream) then
  begin
    Encoder := TIdEncoderMime.Create(nil);
    try
      aStream.seek(0, soBeginning);
      lEncodedStr := Encoder.Encode(aStream, aStream.size);
    finally
      Encoder.Free;
    end;
  end
  else
    lEncodedStr := '';

  Result := '"\/Base64('+lEncodedStr+')\/"';
end;



{ TDataObjectsJSON }

function TDataObjectsJSON.GetAsJSON: string;
var
  lSB: TStringBuilder;
  lFormat: TJsonFormat;
begin
  lSB := TStringBuilder.Create;
  try
    lFormat := cJSONTight;
    GetAsJSONInternal(lSB, lFormat);
    result := lSB.ToString;
  finally
    lSB.Free;
  end;
end;

procedure TDataObjectsJSON.GetAsJSONInternal(aStringBuilder: TStringBuilder; var aFormat: TJsonFormat);
var
  i: Integer;
  lSpaces: string;
  lTempDDO: TDataObj;
begin
  case self.DataType.Code of
    cDataTypeNull: aStringBuilder.Append('null');
    cDataTypeBoolean: begin
      if self.AsBoolean then
        aStringBuilder.Append('true')
      else
        aStringBuilder.Append('false');
    end;
    cDataTypeByte,cDataTypeInt32,cDataTypeInt64,cDataTypeSingle,cDataTypeDouble: aStringBuilder.Append(self.AsString);
//    cDataTypeDecimal128: aStringBuilder.Append(lStore.dataInt64);
    cDataTypeDateTime: aStringBuilder.Append(DateTimeToISO8601Str(self.AsDateTime));
    // for the unix UTC time do we publish in string notation or in the int64 notation.    I say in64 notation since that's the purpose of this data type
    cDataTypeUTCDateTime: aStringBuilder.Append(inttoStr(self.AsUTCDateTime));
    cDataTypeDate: aStringBuilder.Append(DateToISO8601Str(self.AsDateTime));
    cDataTypeTime: aStringBuilder.Append(TimeToISO8601Str(self.AsDateTime));
    cDataTypeGUID, cDataTypeObjectID, cDataTypeString: aStringBuilder.Append('"'+self.AsString+'"');
    cDataTypeStringList: begin
       if aFormat.Style = cJsonHumanReadable then
         aStringBuilder.AppendLine('[')
       else
         aStringBuilder.Append('[');
      lSpaces := aFormat.IncInd;
      for i := 0 to asStringList.Count-1 do
      begin
        if i<asStringList.Count-1 then
          aStringBuilder.Append(lSpaces+'"'+asStringList.Strings[i]+'",')
        else
          aStringBuilder.Append(lSpaces+'"'+asStringList.Strings[i]+'"');
        if aFormat.Style = cJsonHumanReadable then
          aStringBuilder.AppendLine;
      end;
      lSpaces := aFormat.DecInd;
      aStringBuilder.Append(lSpaces+']');
    end;
    cDataTypeFrame: begin
      aStringBuilder.Append('{');
      lSpaces := aFormat.IncInd;
      for i := 0 to asFrame.Count-1 do
      begin
        if i>0 then
          aStringBuilder.Append(lSpaces);
        aStringBuilder.Append('"'+asFrame.slotName(i)+'":');
        asFrame.Slots[i].GetAsJSONInternal(aStringBuilder, aFormat);   // recursion happening here.
        if i<asFrame.Count-1 then
          aStringBuilder.Append(',');
        if aFormat.Style = cJsonHumanReadable then
          aStringBuilder.AppendLine;
      end;
      lSpaces := aFormat.DecInd;
      aStringBuilder.Append(lSpaces+'}');
    end;
    cDataTypeArray: begin
      aStringBuilder.Append('[');
      lSpaces := aFormat.IncInd;
      for i := 0 to asArray.Count-1 do
      begin
        if i>0 then
          aStringBuilder.Append(lSpaces);
        aStringBuilder.Append(IntToStr(i)+':');
        asArray.items[i].GetAsJSONInternal(aStringBuilder, aFormat);
        if i<asFrame.Count-1 then
          aStringBuilder.Append(',');
        if aFormat.Style = cJsonHumanReadable then
          aStringBuilder.AppendLine;
      end;
      lSpaces := aFormat.DecInd;
      aStringBuilder.Append(lSpaces+']');
    end;
    cDataTypeSparseArray: begin
      aStringBuilder.Append('[');
      lSpaces := aFormat.IncInd;
      for i := 0 to asSparseArray.Count-1 do
      begin
        if i>0 then
          aStringBuilder.Append(lSpaces);
        aStringBuilder.Append(IntToStr(asSparseArray.SlotIndex(i))+':');
        asSparseArray.items[i].GetAsJSONInternal(aStringBuilder, aFormat);
        if i<asFrame.Count-1 then
          aStringBuilder.Append(',');
        if aFormat.Style = cJsonHumanReadable then
          aStringBuilder.AppendLine;
      end;
      lSpaces := aFormat.DecInd;
      aStringBuilder.Append(lSpaces+']');
    end;
    cDataTypeBinary: begin
      aStringBuilder.Append(Base64ToJSONText(asBinary));
    end;
    cDataTypeObject: begin
      // FINISH - Objects are basically just serialized the same as a frame, so maybe we can generate the frame and then JSON that frame.
      lTempDDO:=TDataObj.create;
      try
//        FINISH
//        lTempDDO.AsObject := AsObject;   // Hmmm, what about freeing.  Is this just a ref?
//        aStringBuilder.Append(lTempDDO.GetAsJSON);
      finally
        lTempDDO.Free;
      end;
    end;
    cDataTypeTag: begin
      // FINISH - found that other implementations have an option to put tags into JSON by containing the tag using a Json Object (frame) with a certain slotName naming convention.
      //          maybe we should support this concept too.
      // see https://github.com/intel/tinycbor/commit/782f2545a07e707464c6e9b417768e8b980c8e13

      // For now, we are skipping over the tagging portion and just streaming out the contained DataObject to JSON
      self.AsTag.DataObj.GetAsJSONInternal(aStringBuilder, aFormat);
    end;
  end;
end;

{ TJsonFormat }

function TJsonFormat.DecInd: string;
begin
  if Style = cJsonHumanReadable then
    Indent := Indent - 2;
  result := StringOfChar(' ',Indent);
end;

class operator TJsonFormat.Implicit(aValue: TJsonFormat): TJsonStyle;
begin
  result := aValue.Style;
end;

function TJsonFormat.IncInd: string;
begin
  if Style = cJsonHumanReadable then
    Indent := Indent + 2;
  result := StringOfChar(' ',Indent);
end;

class operator TJsonFormat.Implicit(aValue: TJsonStyle): TJsonFormat;
begin
  result.Style := aValue;
  result.Indent := 0;   // initializing this.
end;

{ TJsonStreamContext }
constructor TJsonStreamer.Create(aStream: TStream);
begin
  inherited Create(aStream);
  fStyle := cJSONTight;
  fIndention := 2;   // irrelevant for the tight streaming but we set it anyway in case it is changed.
  fFormatSettings:=TFormatSettings.Create;
  fFormatSettings.DecimalSeparator := '.';     // used when parsing JSON cause json only allows '.' for decimal separators no matter what locale you are in.
  fEncoding := TEncoding.UTF8;
end;

constructor TJsonStreamer.Create(aStyle: TJsonStyle = cJSONTight; aIndention: byte = 2);
begin
  Create(nil);
  fStyle := aStyle;
  fIndention := aIndention;
end;

function TJsonStreamer.DecInd: string;
begin
  if Style = cJsonHumanReadable then
    fIndent := fIndent - fIndention;
  result := StringOfChar(' ',fIndent);
end;

destructor TJsonStreamer.Destroy;
begin
  //NOTE:  do not free the fEncoding becuase that is management globally.  we just have a reference to it.
  if assigned(fStringBuilder) then
    FreeAndNil(fStringBuilder);
  inherited;
end;



function TJsonStreamer.IncInd: string;
begin
  if Style = cJsonHumanReadable then
    fIndent := fIndent + fIndention;
  result := StringOfChar(' ',fIndent);
end;


procedure TJsonStreamer.ReadFromDataObjInternal(aDataObj: TDataObj);
var
  i: Integer;
  lSpaces: string;
  lTempDDO: TDataObj;
  lStringList: TStringList;
  lFrame: TDataFrame;
  lArray: TDataArray;
  lSparseArray: TDataSparseArray;
begin
  case aDataobj.DataType.Code of
    cDataTypeNull: fStringBuilder.Append('null');
    cDataTypeBoolean: begin
      if aDataObj.AsBoolean then
        fStringBuilder.Append('true')
      else
        fStringBuilder.Append('false');
    end;
    cDataTypeByte,cDataTypeInt32,cDataTypeInt64,cDataTypeSingle,cDataTypeDouble:
       fStringBuilder.Append(aDataobj.AsString);
//    cDataTypeDecimal128: aStringBuilder.Append(lStore.dataInt64);
    cDataTypeDateTime: fStringBuilder.Append('"'+DateTimeToISO8601Str(aDataobj.AsDateTime)+'"');
    // for the unix UTC time do we publish in string notation or in the int64 notation.    I say int64 notation since that's the purpose of this data type
    cDataTypeUTCDateTime: fStringBuilder.Append(inttoStr(aDataobj.AsUTCDateTime));
    cDataTypeDate: fStringBuilder.Append('"'+DateToISO8601Str(aDataobj.AsDateTime)+'"');
    cDataTypeTime: fStringBuilder.Append('"'+TimeToISO8601Str(aDataobj.AsDateTime)+'"');
    cDataTypeGUID, cDataTypeObjectID: fStringBuilder.Append('"'+aDataobj.AsString+'"');
    cDataTypeString: fStringBuilder.Append('"'+EncodeSpecialJSONCharacters(aDataobj.AsString, fEncodeNonAsciiCharacters)+'"');
    cDataTypeStringList: begin
       if fStyle = cJsonHumanReadable then
         fStringBuilder.AppendLine('[')
       else
         fStringBuilder.Append('[');
      lSpaces := IncInd;
      lStringList := aDataobj.asStringList;
      for i := 0 to lStringList.Count-1 do
      begin
        if i<lStringList.Count-1 then
          fStringBuilder.Append(lSpaces+'"'+lStringList.Strings[i]+'",')
        else
          fStringBuilder.Append(lSpaces+'"'+lStringList.Strings[i]+'"');
        if fStyle = cJsonHumanReadable then
          fStringBuilder.AppendLine;
      end;
      lSpaces := DecInd;
      fStringBuilder.Append(lSpaces+']');
    end;
    cDataTypeFrame: begin
      fStringBuilder.Append('{');
      lSpaces := IncInd;
      lFrame := aDataObj.AsFrame;
      for i := 0 to lFrame.Count-1 do
      begin
        if i>0 then
          fStringBuilder.Append(lSpaces);
        fStringBuilder.Append('"'+lFrame.slotName(i)+'":');
        ReadFromDataObjInternal(lFrame.Slots[i]);     // recursion happening here.
        if i<lFrame.Count-1 then
          fStringBuilder.Append(',');
        if fStyle = cJsonHumanReadable then
          fStringBuilder.AppendLine;
      end;
      lSpaces := DecInd;
      fStringBuilder.Append(lSpaces+'}');
    end;
    cDataTypeArray: begin
      fStringBuilder.Append('[');
      lSpaces := IncInd;
      lArray := aDataObj.asArray;
      for i := 0 to lArray.Count-1 do
      begin
        if i>0 then
          fStringBuilder.Append(lSpaces);
        ReadFromDataObjInternal(lArray.items[i]);  // Recursion happening here.
        if i<lArray.Count-1 then
          fStringBuilder.Append(',');
        if fStyle = cJsonHumanReadable then
          fStringBuilder.AppendLine;
      end;
      lSpaces := DecInd;
      fStringBuilder.Append(lSpaces+']');
    end;
    cDataTypeSparseArray: begin
      fStringBuilder.Append('[');
      lSpaces := IncInd;
      lSparseArray:=aDataObj.asSparseArray;
      for i := 0 to lSparseArray.Count-1 do
      begin
        if i>0 then
          fStringBuilder.Append(lSpaces);
        fStringBuilder.Append(IntToStr(lSparseArray.SlotIndex(i))+':');
        ReadFromDataObjInternal(lSparseArray.items[i]);
        if i<lSparseArray.Count-1 then
          fStringBuilder.Append(',');
        if fStyle = cJsonHumanReadable then
          fStringBuilder.AppendLine;
      end;
      lSpaces := DecInd;
      fStringBuilder.Append(lSpaces+']');
    end;
    cDataTypeBinary: begin
      fStringBuilder.Append(Base64ToJSONText(aDataObj.asBinary));
    end;
    cDataTypeObject: begin
      // FINISH - Objects are basically just serialized the same as a frame, so maybe we can generate the frame and then JSON that frame.
      lTempDDO:=TDataObj.create;
      try
//        FINISH
//        lTempDDO.AsObject := AsObject;   // Hmmm, what about freeing.  Is this just a ref?
//        aStringBuilder.Append(lTempDDO.GetAsJSON);
      finally
        lTempDDO.Free;
      end;
    end;
    cDataTypeTag: begin
      // FINISH - found that other implementations have an option to put tags into JSON by containing the tag using a Json Object (frame) with a certain slotName naming convention.
      //          maybe we should support this concept too.
      // see https://github.com/intel/tinycbor/commit/782f2545a07e707464c6e9b417768e8b980c8e13

      // For now, we are skipping over the tagging portion and just streaming out the contained DataObject to JSON
      ReadFromDataObjInternal(aDataObj.AsTag.DataObj);
    end;

  end;
end;







// Read a dataObject from the stream that this JSONStreamContext is linked to.
procedure TJsonStreamer.parseFromJson(aObj: TDataObj);
{ NOTE:  Notes from me (Sean Solberg) about this method.

         It's really unfortunate that I had to write a JSON parser because really that task should be perfected by now.  However, I wasn't
         impressed with the parsing performace that I saw in the different JSON libraries out there and since working with JSON is super important these days, I
         felt that taking some time to try and get the best performance I can (without getting too crazy into assembly) is worth the effort.   Delphi's system.JSON did pretty darn good
         compared to most other libraries but I couldn't use it directly without adding another layer of memory allocations as a middle man load.
         I searched for JSON parsers out there and did some basic performance tests with a real-world sample JSON file that is 909kb as a UTF8 encoded text file,
         I get these results:

             TECHNOLOGY                READ TIME
           - Delphi's system.JSON:     0.162 seconds
           - Old DataObj Code:         0.224 seconds
           - JsonDelphiLibrary:        0.134 seconds
           - Json DelphiWebTools:      15.03 seconds      // wow, this one is out of the ballpark;  I'm not using this one.

         So, the JsonDelphiLibrary from https://sourceforge.net/projects/lkjson/ seemed to be the fastest.   I could not use that code directly either because
         it also creates its own object structure from the JSON it is parsing.  I did however, follow a similar pattern, but changed it significantly enough to
         yield better performance.  About 4 times faster for this test case  ... YEAH!

           - This code:                0.033 seconds    I'm super happy about this.  About 5 times faster than embarcadero's code and 4 times faster than the
                                                        JsonDelphiLibrary code that I loosely patterned this code after.

         When looking at the embarcadero source code and looking at the JsonDelphiLibrary source code, I saw some use cases dealing with their character escape
         handling that didn't look like the performance would be the best.  Since the test case mentioned above didn't really have any data in it that would hit this situation,
         I made a sample JSON text file from a non-copyrighted book (first three chapters) found on the internet and hand edited in some usage of the escaped character sequences.
         Then, I did the tests again parsing the sample store.json test file 100 times.  These results are even more impressive because the other libraries just aren't as efficient as they could be.
             TECHNOLOGY                READ TIME
           - Delphi's system.JSON:     0.395 seconds
           - Old DataObj Code:         0.363 seconds
           - JsonDelphiLibrary:        0.088 seconds
           - Json DelphiWebTools:      I didn't even bother with this test
           - This code:                0.013 seconds.    // wow! This code is 30 times faster than Delphi's code and 6.7 times faster than the JsonDelphiLibrary code.  Worth the effort.

         SO, I then took this test case and decided to compare it against the JSON.parse() call in chrome.  I figured the guys at google have nailed this one down better than anyone
         since it's probably hit by just about every web app these days.  To bring the numbers up a bit, I put the text into a string variable A and ran obj=JSON.parse(A) 100,000 times.
         I really didn't think this would happen but this code can parse JSON a bit faster than Chrome's V8 Javascript can.  About 20% faster.
           - This Code:                13.088 seconds
           - Chrome JavaScript:        15.776 seconds

         Bottom line here is that I took some time (a few days) to study different code patterns and try to get this library parsing JSON as fast as possible.  I'm definitely
         happy with the results.  I'm sure it can be improved with a good eye, but it's definitely good enough for now.

         There are two downsides to this code that I hope to improve upon in the future:
           1.  With TJsonStreamContext, The code is written to expect that the entire source stream is going to be parsed into aDataObj.  This might not be the case in some people's implementations.
               I would rather read and parse the JSON from the stream and once a full JSON payload is loaded, leave the stream untouched and at the right position.  However,
               we are supporting the different possible text encodings (really Unicode and UTF8) and that decoding happens before the JSON parsing actually begins.  I would
               need to be able to pull one "character" at a time from any type of encoding and that's just too much work for me to do right now.   Maybe delphi has a way to
               do that, maybe it doesn't.  Anyway... future issue.
           2.  The error handling isn't very good.  If there's malformed JSON fed in, the resulting object structure will be fine and it will get the data up to the point
               of encountering invalid JSON, but there aren't any good exception messages or line number/character number error messages telling someone at a higher level what
               went wrong and where.  Again, future improvement that should be done but it needs to not slow things down much.

         ALSO.  I've done quite a bit of testing with a handful of use-cases, but I don't feel like I have done exhaustive testing for this yet.  So, if anyone
         finds a JSON source use case that fails on this parser, please please send it to me and I'll fix it ASAP.  Or, if you fix the bugs on your own, please send me the code.
}

  {This call will parse out whatever the next token that's encountered is}
  function ParseAnyType(aIndex: Integer; var oRetIndex: Integer; aDataObj: TDataObj): Boolean; forward;

  {Returns true if we are not at the end of the string that we are parsing.}
  function IsNotEnd(aIndex: Integer): Boolean;
  begin
    result := aIndex <= length(fJSON);
  end;

  {Will bring the aIndex forward until it no longer encounters whitespace.   For us here, we are taking a simplified approach in that we are treating all characters less
   than #33 to be considered whitespace (tabs, carriage returns, linefeeds, space, etc).
   Note also, that we are not including high-up unicode characters that are special kinds of white space such as  00A0:No break space,
   200B:Zero width Space, 3000:ideographic space, 202F: narrow no-break space, 205F: Medium mathematical space, FEFF:Zero width no-break space, 2000-200B: assorted space characters, etc.
   This approach should probably be just fine since these special space characters are valid within JSON strings, but probably are not valid as white-space between JSON elements. }
  procedure SkipSpaces(var aIndex: Integer);
  begin
    while (IsNotEnd(aIndex)) and (ord(fJSON[aIndex]) < 33) do
      inc(aIndex);
  end;

  // NOTE:  the following code blocks are coded a bit wierdly considering the individual character comparisons.  Turns out, when looking for specific
  //        strings like this, this logic is faster than doing calls like "sameText", "Pos", etc.  So, this code looks a little goofy but it's all done this way for performance.

  // Try to parse the true identifier from fJSON
  function ParseTrue(aIndex: Integer; var oRetIndex: Integer; aDataObj: TDataObj): Boolean;
  begin
    result := false;
    if ((fJSON[aIndex] = 't') or (fJSON[aIndex] = 'T')) and               //  Looking for any case of TRUE.  It's a bit wierd to do it this way but it's actually quite a fast way to do it.
       ((fJSON[aIndex+1] = 'r') or (fJSON[aIndex+1] = 'R')) and
       ((fJSON[aIndex+2] = 'u') or (fJSON[aIndex+2] = 'U')) and
       ((fJSON[aIndex+3] = 'e') or (fJSON[aIndex+3] = 'E')) then
    begin
      result := true;
      oRetIndex := aIndex + 4;
      aDataObj.AsBoolean := true;
    end
  end;

  // Try to parse the false identifier from fJSON
  function ParseFalse(aIndex: Integer; var oRetIndex: Integer; aDataObj: TDataObj): Boolean;
  begin
    result := false;
    if ((fJSON[aIndex] = 'f') or (fJSON[aIndex] = 'F')) and
       ((fJSON[aIndex+1] = 'a') or (fJSON[aIndex+1] = 'A')) and
       ((fJSON[aIndex+2] = 'l') or (fJSON[aIndex+2] = 'L')) and
       ((fJSON[aIndex+3] = 's') or (fJSON[aIndex+3] = 'S')) and
       ((fJSON[aIndex+4] = 'e') or (fJSON[aIndex+4] = 'E')) then
    begin
      result := true;
      oRetIndex := aIndex + 5;
      aDataObj.AsBoolean := false;
    end;
  end;

  // Try to parse the null identifier from fJSON
  function ParseNull(aIndex: Integer; var oRetIndex: Integer; aDataObj: TDataObj): Boolean;
  begin
    result := false;
    if ((fJSON[aIndex] = 'n') or (fJSON[aIndex] = 'N')) and
       ((fJSON[aIndex+1] = 'u') or (fJSON[aIndex+1] = 'U')) and
       ((fJSON[aIndex+2] = 'l') or (fJSON[aIndex+2] = 'L')) and
       ((fJSON[aIndex+3] = 'l') or (fJSON[aIndex+3] = 'L')) then
    begin
      result := true;
      oRetIndex := aIndex + 4;
      aDataObj.Clear;   // should already be clear, but let's just be explicit.
    end;
  end;

  function ParseObjectID(aIndex: Integer; var oRetIndex: Integer; aDataObj: TDataObj): Boolean;
  begin
    result := false;
    if ((fJSON[aIndex] = 'o') or (fJSON[aIndex] = 'O')) and
       ((fJSON[aIndex+1] = 'b') or (fJSON[aIndex+1] = 'B')) and
       ((fJSON[aIndex+2] = 'j') or (fJSON[aIndex+2] = 'J')) and
       ((fJSON[aIndex+3] = 'e') or (fJSON[aIndex+3] = 'E')) and
       ((fJSON[aIndex+4] = 'c') or (fJSON[aIndex+4] = 'C')) and
       ((fJSON[aIndex+5] = 't') or (fJSON[aIndex+5] = 'T')) and
       ((fJSON[aIndex+6] = 'i') or (fJSON[aIndex+6] = 'I')) and
       ((fJSON[aIndex+7] = 'd') or (fJSON[aIndex+7] = 'D')) and
       (fJSON[aIndex+8] = '(') then
    begin
      // we have the start of an objectID so now try and parse out the string representation of the objectID followed by a ")"




      result := true;
      oRetIndex := aIndex + 4;
      aDataObj.Clear;   // should already be clear, but let's just be explicit.
    end;
  end;

function ParseISODate(aIndex: Integer; var oRetIndex: Integer; aDataObj: TDataObj): Boolean;
begin
  //FINISH
  result := false;
end;

function NumberInt(aIndex: Integer; var oRetIndex: Integer; aDataObj: TDataObj): Boolean;
begin
  //FINISH
  result := false;
end;




  // Try to parse a number from fJSON
  // This could read and populate an integer(int64, int32, or byte) or a double float.
  // We are not having it read in single floats cause it's not obvious to decide when to choose a single and when to choose a double.  Could be a future improvement maybe.
  function ParseNumber(aIndex: Integer; var aRetIndex: Integer; aDataobj: TDataObj): Boolean;
  var
    lWs: string;
    lContinue: boolean;
    lCanBePlusOrMinus: boolean;
    lWholeCount: integer;
    lIsFloat: boolean;
    lInt64: int64;
  begin
    result := false;
    if not IsNotEnd(aIndex) then exit;

    aRetIndex := aIndex;

    lContinue := true;
    lCanBePlusOrMinus := true;                // + or minus character can only come in as the first character or directly after an "e"
    lWholeCount := 0;
    lIsFloat := false;

    while (lContinue) do
    begin
      if (fJSON[aIndex] >= '0') and (fJSON[aIndex] <= '9') then
      begin
        if not lIsFloat then
          inc(lWholeCount);
        lCanBePlusOrMinus := false;
        lContinue := true;
        inc(aIndex);
        result := true;
      end
      else if lCanBePlusOrMinus and ((fJSON[aIndex] = '+') or (fJSON[aIndex] = '-')) then
      begin
        lContinue := true;
        inc(aIndex);
      end
      else if (fJSON[aIndex] = '.') and (lWholeCount > 0) then
      begin
        lContinue := true;
        lIsFloat := true;
        inc(aIndex);
      end
      else if (fJSON[aIndex] = 'e') or (fJSON[aIndex] = 'E') then
      begin
        lContinue := true;
        lIsFloat := true;
        lCanBePlusOrMinus := true;
        inc(aIndex);
      end
      else
      begin
        // None of the possible conditions above absorbed this character so we are done trying to read this number.
        // to be a valid JSON number that makes sense here, this character should be empty space, or a ',' to end this slot, or a '}' to end this frame, or a ']' to end this array.
        if result then
        begin
          // we either found an integer number or a floating point number
          lWs := copy(fJSON, aRetIndex, aIndex - aRetIndex);
          aRetIndex := aIndex;

          if lIsFloat then
          begin
            aDataObj.AsDouble := StrToFloat(lWs, fFormatSettings);  // Note:  assuming all floats from JSON are double.  not trying singles.
          end
          else
          begin
            // choose the smallest integer data type to hold this value.
            lInt64 := StrToInt64(lWS);
            if (lInt64 > 2147483647) or (lInt64 < -2147483648) then
            begin
              aDataObj.AsInt64 := lInt64;
            end
            else if (lInt64 >= 0) and (lInt64<=255)  then
            begin
              aDataObj.AsByte := lInt64;
            end
            else
            begin
              aDataObj.AsInt32 := lInt64;
            end;
          end;
        end;

        lContinue := false;
      end;
    end;
  end;



  // Try to parse a string from fJSON and along the way, do any special escaping handling such as \uxxxxx, etc.
  function ParseString(aIndex: Integer; var aRetIndex: Integer; var oString: string): Boolean;

    const cHexDecimalConvert: array[Byte] of Byte = (
       $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {00-$0F}
       $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {10-$1F}
       $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {20-2F}
       0,    1,    2,    3,    4,    5,    6,    7,    8,    9,    $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {30-3F}
       $ff,  10,   11,   12,   13,   14,   15,   $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {40-4F}
       $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {50-5F}
       $ff,  10,   11,   12,   13,   14,   15,   $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {60-6F}
       $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {70-7F}
       $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {80-8F}
       $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {90-9F}
       $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {A0-AF}
       $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {B0-BF}
       $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {C-CF}
       $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {D0-DF}
       $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {E0-EF}
       $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff); {F0-FF}

  var
    lWorkString: String;
    i: integer;
    lStartIdx: Integer;
    lEndIdx: integer;
    lStartsWithSingleQuote: boolean;
    lJsonLength: integer;
    lUnicodeCH1: cardinal;   // should this be a word?
    lUnicodeCH2: cardinal;   // should this be a word?
    lUnicodeCH3: cardinal;   // should this be a word?
    lUnicodeCH4: cardinal;   // should this be a word?
    lChar: char;


    procedure AppendWorkStringWithChar(aNewChar: Char);
    begin
      // will move the bytes from the source buffer we are looking through to the WorkString and append aNewChar which came from the escaping.
      lWorkString := lWorkString + copy(fJSON, lStartIdx, lEndIdx-lStartIdx) + aNewChar;        //FINISH - this subtraction may not be right
      lStartIdx := i;
    end;

    procedure FinishWorkString;
    begin
      // will move the bytes from the source buffer we are looking through to the WorkString and append aNewChar which came from the escaping.
      lWorkString := lWorkString + copy(fJSON, lStartIdx, lEndIdx-lStartIdx);
      lEndIdx := i;
    end;

  begin
    lWorkString := '';
    SkipSpaces(aIndex);

    result := IsNotEnd(aIndex);
    if not result then exit;

    lStartsWithSingleQuote := false;
    if (fJSON[aIndex] = '''') then
    begin
      //technically speaking, json doesn't really support single quotes for string definitions.  however, we are going to support it.   maybe someday we will have an option to restrict to just explicitly valid json
      lStartsWithSingleQuote := true;
    end
    else if (fJSON[aIndex] <> '"') then
    begin
      // didn't begin with a double quote or a single quote so can't be a valid string.
      result := false;
      exit;
    end;

    inc(aIndex);
    i := aIndex;

    // Go through all the characters looking for the end character (either a double quote or single quote depending on how we started).
    // along the way, we need to decode any escaping.
    lStartIdx := aIndex;  // this is the index for the begining character that we need to include.  We are starting off with what has been passed into this procedure.
    lEndIdx := aIndex;    // this is the index for the last character that we will include from the source string.
    lJsonLength := length(fJSON);

    while (i<=lJsonLength) do
    begin
      if (fJSON[i]='\') then
      begin
        // we have the starting of an escaper character so start an escape read.
        if i < lJsonLength then
        begin
          inc(i);
          case fJSON[i] of
            '\': begin AppendWorkStringWithChar('\'); inc(i); end;
            '"': begin AppendWorkStringWithChar('"'); inc(i); end;
            '/': begin AppendWorkStringWithChar('/'); inc(i); end;
            'b': begin AppendWorkStringWithChar(#8); inc(i); end;   //backspace
            'f': begin AppendWorkStringWithChar(#12); inc(i); end;  //Form Feed
            'n': begin AppendWorkStringWithChar(#10); inc(i); end;  //New Line
            'r': begin AppendWorkStringWithChar(#13); inc(i); end;  //carriage return
            't': begin AppendWorkStringWithChar(#9); inc(i); end;   //tab
            'u':
              begin
                inc(i);
                if i+3 <= lJsonLength then
                begin
                  lUnicodeCH1 := cHexDecimalConvert[ord(fJSON[i])];
                  inc(i);
                  lUnicodeCH2 := cHexDecimalConvert[ord(fJSON[i])];
                  inc(i);
                  lUnicodeCH3 := cHexDecimalConvert[ord(fJSON[i])];
                  inc(i);
                  lUnicodeCH4 := cHexDecimalConvert[ord(fJSON[i])];
                  inc(i);
                  if (lUnicodeCH1 <= 15) and (lUnicodeCH2 <= 15) and (lUnicodeCH3 <= 15) and (lUnicodeCH4 <= 15)then
                  begin
                    lChar := WideChar((lUnicodeCH1 shl 12) or (lUnicodeCH2 shl 8) or (lUnicodeCH3 shl 4) or lUnicodeCH4);  // we have four valid hex characters.
                    AppendWorkStringWithChar(lChar);
                  end
                  else
                  begin
                    raise Exception.Create('Invalid character parsing an escape "\uxxxx" sequence.');
                  end;
                end
                else
                begin
                  // we started with an /u sequence but there aren't enough characters to pick up 4 more.
                  raise Exception.Create('Invalid characters parsing an escape "\uxxxx" sequence.');
                end;
              end;
            else
            begin
              // This must be an error condition because an invalid character is following the excape '\' character.
              raise Exception.Create('Invalid character after encountering an escape "\" character.');
            end;
          end;
        end
        else
        begin
          // we are out of characters to pickup in an escape sequence.
          raise Exception.Create('Invalid character after encountering an escape "\" character.');
        end;
      end

      else if fJSON[i] = '"' then   // this should be an end of string marker unless we started with a single quote.
      begin
        if lStartsWithSingleQuote then
        begin
          // just take this character as a normal character by setting our ending index to accept it.
          inc(i);
          lEndIdx := i;
        end
        else
        begin
           // Finish our string.
          FinishWorkString;
          result := true;
          break;
        end;
      end

      else if fJSON[i] = '''' then
      begin
        if lStartsWithSingleQuote = false then
        begin
          // just take this character as a normal character by setting our ending index to accept it.
          inc(i);
          lEndIdx := i;
        end
        else
        begin
           // Finish our string.
          FinishWorkString;
          result := true;
          break;
        end;
      end

      else
      begin
        // This is just a normal character so take it by setting our ending index to accept it.
        inc(i);
        lEndIdx := i;
      end;
    end;

    aRetIndex := lEndIdx+1;
    // What happens if we get out of this loop without receiving the closing quote.  That's an error condition.
    if result then
    begin
      oString := lWorkString;
    end;
  end;



(*
  // Try to parse a string from fJSON
  function ParseString(aIndex: Integer; var aRetIndex: Integer; var oString: string): Boolean;

    const cHexDecimalConvert: array[Byte] of Byte = (
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {00-0F}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {10 0F}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {20-2F}
       0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  0,  0,  0,  0,  0, {30-3F}
       0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0, {40-4F}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {50-5F}
       0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0, {60-6F}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {70-7F}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {80-8F}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {90-9F}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {A0-AF}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {B0-BF}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {C0-CF}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {D0-DF}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {E0-EF}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0); {F0-FF}

    // This routine will go through s and replace the special character sequences with their decoded real characters.
    function strSpecialChars(const s: string): string;
    var
      lPos : integer;
      lPrevPos: integer;
      lUnicodeCH: integer;
      lLen: integer;
    begin
      lPos := Pos('\', s);
      if (lPos = 0) then
        Result := s
      else
      begin
        lLen := length(s);
        Result := Copy(s, 1, lPos-1);    // return the beginning up to the '\' character.

        while lPos > 0 do
        begin
          // lEnd here is pointing to the first '\' character found.
          inc(lPos);   // skip over the '\' character.
          case s[lPos] of
            '\': begin Result := Result + '\'; inc(lPos); end;
            '"': begin Result := Result + '"'; inc(lPos); end;
            '/': begin Result := Result + '/'; inc(lPos); end;
            'b': begin Result := Result + #8; inc(lPos); end;   //backspace
            'f': begin Result := Result + #12; inc(lPos); end;  //Form Feed
            'n': begin Result := Result + #10; inc(lPos); end;  //New Line
            'r': begin Result := Result + #13; inc(lPos); end;  //carriage return
            't': begin Result := Result + #9; inc(lPos); end;   //tab
            'u':
              begin
                inc(lPos);
                lUnicodeCH := (cHexDecimalConvert[ord(s[lPos])] shl 12);
                inc(lPos);
                lUnicodeCH := lUnicodeCH or (cHexDecimalConvert[ord(s[lPos])] shl 8);
                inc(lPos);
                lUnicodeCH := lUnicodeCH or (cHexDecimalConvert[ord(s[lPos])] shl 4);
                inc(lPos);
                lUnicodeCH := lUnicodeCH or (cHexDecimalConvert[ord(s[lPos])]);
                inc(lPos);
                Result := Result + WideChar(lUnicodeCH);
              end;
            else
            begin
              // This must be an error condition because an invalid character is following the excape '\' character.
            end;
          end;

          lPrevPos := lPos;        // here, lPos should point to the next character past the escape characters we dealt with just above.
          lPos := Pos('\', s, lPrevPos);     // look for the next escape character if there is one.
          if lPos = 0 then
          begin
            // there is no further escape character so bring in the rest of the original string.
            Result := Result + copy(s, lPrevPos, lLen);
          end
          else
          begin
            // there is an escape character up ahead so copy the next block of characters up to the next '\'
            Result := Result + copy(s, lPrevPos, lPos-lPrevPos);
          end;
        end;
      end;
    end;

  var
    lFinished: Boolean;
    lWorkString: String;
    i,j: integer;
    lWidx: Integer;
    lStartsWithSingleQuote: boolean;

  begin
    SkipSpaces(aIndex);

    result := IsNotEnd(aIndex);
    if not result then exit;

    if (fJSON[aIndex] = '"') then
    begin
      lStartsWithSingleQuote := false;
    end
    else if (fJSON[aIndex] = '''') then
    begin
      //technically speaking, json doesn't really support single quotes for string definitions.  however, we are going to support it.   maybe someday we will have an option to restrict to just explicitly valid json
      lStartsWithSingleQuote := true;
    end
    else
    begin
      // didn't begin with a double quote or a single quote so can't be a valid string.
      result := false;
      exit;
    end;

    inc(aIndex);
    lWidx := aIndex;


    //NOTE:  THIS REPEAT BLOCK HAS THE POTENTIAL TO BE VERY, VERY, VERY INNEFFICIENT FOR A LONG SEQQUENCE OF ENCODED \Uxxxx SEQUENCES AS IT'S LOOKING TO THE END OF THE STRING AND DOWN BELOW
    //       IT RESETS UP WHEN IT DOES lWidx:=i+2;



    lFinished:=false;
    repeat
      i := 0;
      j := 0;
      while (lWidx<=length(fJSON)) and (j=0) do
      begin
        if (i=0) and (fJSON[lWidx]='\') then i:=lWidx
        else if (j=0) and (fJSON[lWidx]='"') and (lStartsWithSingleQuote = false) then j:=lWidx
        else if (j=0) and (fJSON[lWidx]='''') and lStartsWithSingleQuote then j:=lWidx;           //technically speaking, json doesn't really support single quotes for string definitions.  however, we are going to support it.

        inc(lWidx);
      end;

      if j=0 then   // check that we didn't have a closing quotes.
      begin
        result := false;
        exit;
      end;

      if (i=0) or (j<i) then    // if we don't have any slashed characters in the string, then pull out the string contents.
      begin
        lWorkString := copy(fJSON, aIndex, j-aIndex);
        aIndex := j;
        lFinished := true;
      end

      else
      begin
        lWidx:=i+2;    // skip the slashed character
      end;
    until lFinished;

    lWorkString := strSpecialChars(lWorkString);
    inc(aIndex);

    if result then
      oString := lWorkString;

    aRetIndex := aIndex;
  end;


*)

  //Try to parse an array from fJSON.  Needs to start with "["
  function ParseArray(aIndex: Integer; var oRetIndex: Integer; aDataObj: TDataObj): Boolean;
  var
    lArray: TDataArray;
  begin
    try
      result := IsNotEnd(aIndex);
      if not result then exit;
      result := fJSON[aIndex] = '[';
      if not result then exit;

      inc(aIndex);

      SkipSpaces(aIndex);
      lArray := aDataObj.AsArray;
      while IsNotEnd(aIndex) and (fJSON[aIndex] <> ']') do
      begin
        if ParseAnyType(aIndex, aIndex, lArray.NewSlot) then
        begin
          SkipSpaces(aIndex);
          if IsNotEnd(aIndex) and (fJSON[aIndex] = ',') then
          begin
            inc(aIndex);
          end
          else
          begin
            break;
          end;
        end
        else
        begin
//          break;   // we hit the end of the array
        end;
      end;

      result := IsNotEnd(aIndex) and (fJSON[aIndex] = ']');
      if not result then exit;

      inc(aIndex);
    finally
      oRetIndex := aIndex;
    end;
  end;

  // Try to parse a slotname from fJSON string.  We are expecting a slotname, so we should be seeing "somename" and a : should be following it.
  function ParseSlotname(aIndex: integer; var oRetIndex: integer; var oSlotname: string): boolean;
  begin
    // Slotnames must be surrounded by " so really it's the same as string data. The slotname must also end with a :
    // Also, any amount of white space can be after the string and before the colon or after the colon and before the next data content.
    // Examples:  "Name" : null, "Name":"Sean", "First Name" : "billy", "Name": 1

    result := ParseString(aIndex, aIndex, oSlotName);
    if result then
    begin
      // consume up to the ":"
      SkipSpaces(aIndex);
      result := fJSON[aIndex] = ':';
      if result then
      begin
        inc(aIndex);
        oRetIndex := aIndex;
      end;
    end;
  end;

  //Try to parse a frame (in JSON terminoloty, it's called an object) from fJSON.  Needs to start with "{"
  function ParseFrame(aIndex: Integer; var oRetIndex: Integer; aDataObj: TDataObj): Boolean;
  var
    lFrame: TDataFrame;
    lSlotName: string;
  begin
    try
      result := IsNotEnd(aIndex);
      if not result then exit;
      result := fJSON[aIndex] = '{';
      if not result then exit;

      inc(aIndex);

      SkipSpaces(aIndex);
      lFrame := aDataObj.AsFrame;
      while IsNotEnd(aIndex) and (fJSON[aIndex] <> '}') do
      begin
        if ParseSlotName(aIndex, aIndex, lSlotName) then
        begin
          if ParseAnyType(aIndex, aIndex, lFrame.NewSlot(lSlotName)) then
          begin
            SkipSpaces(aIndex);
            if IsNotEnd(aIndex) and (fJSON[aIndex] = ',') then
            begin
              inc(aIndex);
            end
            else
            begin
              break;
            end;
          end;
        end
        else
        begin
          raise Exception.Create('Unable to parse JSON at '+InttoStr(aIndex));
          // Error reading the slotname.
        end;
      end;

      result := IsNotEnd(aIndex) and (fJSON[aIndex] = '}');
      if not result then exit;

      inc(aIndex);
    finally
      oRetIndex := aIndex;
    end;
  end;

  // This function will parse whatever JSON data type it can find next and will put that data into aDataObj
  function ParseAnyType(aIndex: Integer; var oRetIndex: Integer; aDataObj: TDataObj): Boolean;
  var
    lString: string;
  begin
    SkipSpaces(aIndex);
    result := ParseFalse(aIndex, oRetIndex, aDataObj);
    if not result then result := ParseTrue(aIndex, oRetIndex, aDataObj);
    if not result then result := ParseNull(aIndex, oRetIndex, aDataObj);
    if not result then result := ParseNumber(aIndex, oRetIndex, aDataObj);
    if not result then
    begin
      result := ParseString(aIndex, oRetIndex, lString);
      if result then
      begin
        aDataObj.AsString := lString;
      end;
    end;
    if not result then result := ParseArray(aIndex, oRetIndex, aDataObj);
    if not result then result := ParseFrame(aIndex, oRetIndex, aDataObj);

    //Extended JSON parsing options such as MongoDB Extended JSON.
    //Note that parsing these are not following standard JSON, but I've found that there ara other "json" like files such as BSON text format that we can support here.
    if not result then result := ParseObjectID(aIndex, oRetIndex, aDataObj);
    if not result then result := ParseISODate(aIndex, oRetIndex, aDataObj);
    if not result then result := NumberInt(aIndex, oRetIndex, aDataObj);
  end;

var
  lIndex: Integer;
begin
  if fJSON = '' then exit;

  lIndex := 1;

(*  // skip a BOM utf8 marker.  Really, we shouldn't have to do this here since it's expected that any encodings have already been handled and converted to straight unicode into the fJSON variable.
  if copy(fJSON, lIndex, 3) = #$EF#$BB#$BF then
  begin
    inc(lIndex, 3);
    if lIndex > length(fJSON) then exit;
  end; *)

  if not ParseAnyType(lIndex, lIndex, aObj) then
  begin
    //Getting here means we were not able to parse the JSON appropriately.
    raise exception.Create('Error parsing JSON');
  end;
end;

class function TJsonStreamer.DataObjToJson(aDataObj: TDataObj): string;      // finish - need to pass in the context properties.
var
  lContext: TJsonStreamer;
begin
  result := '';
  // Notice that we are creating a TJsonStringContext and leaving it with the default serialization options which is cTightJSON and EncodeNonAsciiCharacters=false.
  lContext:=TJsonStreamer.Create(cJsonHumanReadable, 2);
  try
    lContext.Encode(aDataObj);
    result := lContext.fJSON;
  finally
    lContext.Free;
  end;
end;

class procedure TJsonStreamer.JsonToDataObj(aJson: string; aDataObj: TDataObj);
var
  lContext: TJsonStreamer;
begin
  lContext:=TJsonStreamer.Create;
  try
    lContext.fJSON := aJson;
    lContext.Decode(aDataObj);
  finally
    lContext.Free;
  end;
end;


class function TJsonStreamer.GetFileFilter: string;
begin
  result := 'JSON Files (*.json, *.txt)|*.json;*.txt';
end;

procedure TJsonStreamer.ApplyOptionalParameters(aParams: TStrings);
var
  i: Integer;
  lIndention: integer;
begin
  inherited;

  i := 0;
  while i < aParams.Count do
  begin
    if SameText(aParams[i], '-Human') then
      Style := cJsonHumanReadable
    else if SameText(aParams[i], '-Indent') and (i < aParams.Count-1) then
    begin
      lIndention := StrToIntDef(aParams[i+1],-1);
      if (lIndention >= 0) and (lIndention <= 20) {resonable limit?} then
        Indention := lIndention;
      inc(i);
    end
    else if SameText(aParams[i], '-Encoding') and (i < aParams.Count-1) then
    begin
      if SameText(aParams[i+1], 'UTF-8') then
        Encoding := TEncoding.UTF8
      else if SameText(aParams[i+1], 'UTF-7') then
        Encoding := TEncoding.UTF7
      else if SameText(aParams[i+1], 'ASCII') then
        Encoding := TEncoding.ASCII
      else if SameText(aParams[i+1], 'UNICODE') then
        Encoding := TEncoding.Unicode
      else if SameText(aParams[i+1], 'BIGENDIANUNICODE') then
        Encoding := TEncoding.BigEndianUnicode;
      // ANYTHING ELSE IS IGNORED.

      inc(i);
    end;

    inc(i);
  end;

end;

class function TJsonStreamer.ClipboardPriority: cardinal;
begin
  result := 100;
end;

function TJsonStreamer.Clone: TDataObjStreamerBase;
var
  lS: TJsonStreamer;
begin
  lS := TJsonStreamer.create;
  lS.fStyle:=self.fStyle;
  lS.fIndention:=self.fIndention;
  lS.fIndent:=self.fIndent;
  lS.fFormatSettings:=self.fFormatSettings;
  lS.fEncodeNonAsciiCharacters:=self.fEncodeNonAsciiCharacters;
  lS.fEncoding:=self.fEncoding;
  result := lS;
end;

constructor TJsonStreamer.Create(aStream: TStream; aEncoding: TEncoding; aStyle: TJsonStyle = cJSONTight; aIndention: byte = 2);
begin
  inherited Create(aStream);
  fStyle := aStyle;
  fIndention := aIndention;
  fStream := aStream;
  fEncoding := aEncoding;
end;


class function TJsonStreamer.IsFileExtension(aStr: string): boolean;
begin
  result := SameText(aStr, 'json') or SameText(aStr, '.json') or SameText(aStr, 'txt') or SameText(aStr, '.txt');
end;

procedure TJsonStreamer.Decode(aDataObj: TDataObj);
var
  lBytes: TBytes;
  lSize: int64;
  lEncoding: TEncoding;
  lPreambleSize: integer;
begin
  // If fStream is assigned, then we are going to need to pull out the data from that stream.  It could be ascii encoded, UTF-8 encoded, etc.  so use TEncoding to deal with that.
  // if fStream is not assigned, then we are assuming that fJSON has already been filled with JSON text.
  if assigned(fStream) then
  begin
    // heres the problem with this. How do we know how much data to read from this stream through the fEncoding?
    // right now, we are going to assume that all data in the stream is destined to aDataObj.
    // but, there may be more data in the stream after the aDataObj is read out.    Hmmmmm.  How do we fix this someday?

    lSize := fStream.Size;
    SetLength(lBytes, lSize);
    fStream.Read(lBytes[0], lSize);

    lEncoding := nil;
    lPreambleSize := TEncoding.GetBufferEncoding(lBytes, lEncoding);
    if assigned(lEncoding) then
    begin
      SetEncoding(lEncoding);
    end;

    fJSON := fEncoding.GetString(lBytes, lPreambleSize, lSize-lPreambleSize);
  end;

  ParseFromJSON(aDataObj);     // take the fJSON string and parse it into aDataObj.
end;


procedure TJsonStreamer.Encode(aDataObj: TDataObj);
var
  lBytes: TBytes;
begin
  // First, get the aDataObj serialized into a JSON string (Full Unicode Delphi string)
  fStringBuilder:=TStringBuilder.Create;                    // This will hold the first copy of the produced json
  try
    ReadFromDataObjInternal(aDataObj);                    //FINISH - If we are encoding to ASCII, then we should have the JSON produced escaped characters above $80.
    fJSON := fStringBuilder.ToString;                   // This string now holds the JSON text as a delphi String (unicode string)
  finally
    FreeAndNil(fStringBuilder);
  end;

  if assigned(fStream) then
  begin
    // If we have an assigned stream, then move the string to a stream using the chosen text encoding.
    // If it's not assigned, then most likely the caller is just interested in getting the result by reading the fJSON string.
    lBytes := fEncoding.GetPreamble;
    fStream.Write(lBytes[0], length(lBytes));

    lBytes:=fEncoding.GetBytes(fJSON);                  // perform the encoding step to return the actual bytes that the string should be encoded to.  For example, convert from unicode to UTF8
    fStream.Write(lBytes[0], length(lBytes));
  end;
end;

class function TJsonStreamer.FileExtension: string;
begin
  result := 'json';
end;

procedure TJsonStreamer.SetEncoding(aEncoding: TEncoding);
begin
  fEncoding := aEncoding;     // note that we could be assigning nil to fEncoding here, but otherwise, fEncoding should not be nil.
end;


initialization
  RegisterDataObjStreamer(TJsonStreamer);

end.
