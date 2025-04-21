unit DataObjects2JSON;

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

(* This unit provides the ability to serialize dataObjects to/from JSON *)


// TODO:  check that the Dates, DateTimes, Times are put to JSON in the ISO format?  Or are there options for the format encoding/decoding
//        Improve the performance of the TDataArray.DeleteSlot method so that packing can be deferred.

interface

uses DataObjects2, DataObjects2Streamers, SysUtils, Classes, DataObjects2Utils, windows;

type
  TJsonStyle = (cJsonTight, cJsonHumanReadable);

  // This class helper Gives us the ability to do the following
  //   lSomeString := lDataObj.JSON;  When producing JSON, it uses the Tight JSON formatting.
  //     or
  //   lSomeString := lDataObj.JSONFormatted;  // if you want formatted JSON text, then call this
  //     or
  //   lDataObj.JSON := lSomeString;
  TDataObjectsJSONHelper = class helper for DataObjects2.TDataObj
  public
    procedure setJSON(const Value: string);
    function GetAsJSON: string;
    property JSON: string read getAsJSON write setJSON;
    function JSONFormatted(aIndention: byte = 2): string;
  end;

  EJSONParsingException = class(Exception)
  public
    CharacterPosition: int64;
  end;


  TJsonStreamer = class(TDataObjStreamerBase)
  private
    // Settings for Encoding options.
    fStyle: TJsonStyle;
    fIndention: byte;
    fEncodeNonAsciiCharacters: boolean;    // only used when streaming out.
    fIncludeEncodingPreamble: boolean;     // only used when streaming out.

    // Settings for Decoding options
    fAllowParsingExtendedTypes: boolean;
    fAllowParsingSymbols: boolean;        // Means we will allow a text "identifer" to be streamed in after a slotname that is not valid JSON, but we consider it a symbol.  IE)  {"Slotname": ~TemplateName~, "SlotName2": ALWAYSRUN}

    // Settings for both Encoding and Decoding options.
    fEncoding: TEncoding;

    // Variables used during serialization.
    fFormatSettings: TFormatSettings;
    fStringBuilder: TStringBuilder;
    fJSON: string;             //NEVER Set this directly.  ALWAYS go through SetJSON or the property JSON which redirects to SetJSON
    fIndent: integer;
    fInitialBufferCapacity: Cardinal;
    fSupportJSON5: boolean;                      // only used during streaming.

    // working variables used during the parsing.
    fCurrentCharPtr: PChar;
    fEndPtr: PChar;           // Pointer to the last allowed character in the JSON string.

    // working helper functions used during the parsing.
    procedure SkipSpaces; inline;
    function eof: boolean; inline;
    function CurrentChar: Char; inline;
    procedure IncIndex; inline;
    function HasNumberOfChars(aCount: integer): boolean; inline;
    procedure RaiseParsingException(aMsg: string; aCharacterPos: PChar);


    // the following set of methods are used in the parsing, most of which participate in the JumpTable.
    function ParseString(var oString: string): boolean;
    function ParseSlotname(var oSlotname: string): boolean;
    function ParseAnyType(aDataObj: TDataObj): boolean;
    function ParseInvalidChar(aDataObj: TDataObj): boolean;
    function ParseStringValue(aDataObj: TDataObj): boolean;
    function ParseNumber(aDataObj: TDataObj): boolean;
    function ParseArray(aDataObj: TDataObj): boolean;
    function ParseInfinity(aDataObj: TDataObj): boolean;
    function ParseINF(aDataObj: TDataObj): boolean;
    function ParseNegativeInfinity(aDataObj: TDataObj): boolean;
    function ParseNegativeInf(aDataObj: TDataObj): boolean;
    function ParseNegative(aDataObj: TDataObj): boolean;
    function ParseTrue(aDataObj: TDataObj): Boolean;
    function ParseFalse(aDataObj: TDataObj): boolean;
    function ParseNull(aDataObj: TDataObj): boolean;
    function ParseNan(aDataObj: TDataObj): boolean;
    function ParseN(aDataObj: TDataObj): boolean;
    function ParseObjectID(aDataObj: TDataObj): boolean;
    function ParseISODate(aDataObj: TDataObj): boolean;
    function ParseFrame(aDataObj: TDataObj): boolean;
    function ParseI(aDataObj: TDataObj): boolean;
    function TryParsingSymbol(aDataObj: TDataObj): integer;  //0=nothing applicable, 1=successful parse, 2=Applicable, but error in parsing.

    procedure SetEncoding(aEncoding: TEncoding);
    procedure ReadFromDataObjInternal(aDataObj: TDataObj);
    function IncInd: string;   // increment the indent
    function DecInd: string;
    procedure WriteStringEncodingSpecialJSONCharacters(const aString: string);
    procedure ParseFromJson(aObj: TDataObj);
    procedure SetJSON(const Value: string);
  public
    function Clone: TDataObjStreamerBase; override;
    constructor Create(aStream: TStream); overload; override;
    constructor Create(aStyle: TJsonStyle = cJSONTight; aIndention: byte = 2); overload;    // Will generate a warning because we are using overload and other versions are overridden virtual.
    constructor Create(aStream: TStream; aEncoding: TEncoding; aStyle: TJsonStyle = cJSONTight; aIndention: byte = 2); overload;    // Will generate a warning because we are using overload and other versions are overridden virtual.
    destructor Destroy; override;

    class function FileExtension: string; override;
    class function Name: string; override;
    class function Description: string; override;
    class procedure GetParameterInfo(aParameterPurpose: TDataObjParameterPurposes; aStrings: TStrings); override;
    class function GetFileFilter: string; override;
    class function IsFileExtension(aStr: string): boolean; override;
    class function Priority: cardinal; override;
    class procedure GetClipboardPublishingFormats(aCallback: TGetClipboardPublishingCallbackProc); override;
    procedure SetPreferencesByClipboardVersion(aClipboardVersion: integer); override;
//    function GetClipboardFormat: word; override;


    // Parse the JSON from the attached Stream or from the JSON string if there is not Stream attached and put the results into aDataObj
    procedure Decode(aDataObj: TDataObj); override;

    // Produce JSON from the aDataObj and put the results into the JSON property using the formatting options defined by Style, Indention, etc.
    // Also put the results into the attached Stream if it is not nil according to the encoding defined in the Encoding property and the IncludeEncodingPreamble property.
    procedure Encode(aDataObj: TDataObj); override;
    procedure ClipboardEncode(aDataObj: TDataObj); override;
    procedure ApplyOptionalParameters(aParams: TStrings); override;

    property Style: TJsonStyle read fStyle write fStyle;
    property IncludeEncodingPreamble: boolean read fIncludeEncodingPreamble write fIncludeEncodingPreamble;
    property SupportJSON5: boolean read fSupportJSON5 write fSupportJSON5;

    // defines how many spaces are inserted per indention level when Style is cJsonHumanReadable
    property Indention: byte read fIndention write fIndention;

    property EncodeNonAsciiCharacters: boolean read fEncodeNonAsciiCharacters write fEncodeNonAsciiCharacters;
    property Encoding: TEncoding read fEncoding write setEncoding;
    property AllowParsingExtendedTypes: boolean read fAllowParsingExtendedTypes write fAllowParsingExtendedTypes;
    property AllowParsingSymbols: boolean read fAllowParsingSymbols write fAllowParsingSymbols;
    property InitialBufferCapacity: Cardinal read fInitialBufferCapacity write fInitialBufferCapacity;

    property JSON: string read fJSON write SetJSON;

    // Produce a JSON string from the data in aDataObj according to the properties passed into this class function.
    class function DataObjToJson(aDataObj: TDataObj; aStyle: TJSONStyle=cJsonTight; aIndent: byte = 2; aEncodeNonAsciiCharacters: boolean=false): string;

    // Parse a JSON string and put the results into aDataObj.
    class procedure JsonToDataObj(const aJson: string; aDataObj: TDataObj);

    class function CreateDataObjFromJSON(const aJson: string): TDataObj;
  end;

var
  gJSONFormatSettings: TFormatSettings;

implementation

uses DateUtils, IdCoderMIME;

resourcestring
  SJsonParseMessageAt = '%s at %d';
  cExceptInvalidCharacterParsingEscape = 'Invalid character parsing an escape "\uxxxx" sequence.';
  cExceptInvalidCharacterAfterEscape = 'Invalid character after encountering an escape "\" character.';
  cExceptUnableToConvertText = 'Unable to convert text to floating point number';
  cExceptNumberTooBigWhen = 'Number too big when parsing a JSON number.  Library is limited to 64 bit numbers.';
  cExceptErrorParsingAnArray = 'Error parsing an array.  Expected ]';
  cExceptErrorParsingObject = 'Error Parsing ObjectID';
  cExceptErrorParsingISODate = 'Error Parsing ISODate';
  cExceptInvalidTokenParsing = 'Invalid token parsing JSON';
  cErrorErrorParsingJSON = 'Error Parsing JSON. Missing closing }';
  cExceptErrorParsingJSON = 'Error parsing JSON';


const cHexDecimalConvert: array[0..102] of Byte = (
   $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {00-$0F}
   $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {10-$1F}
   $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {20-2F}
   0,    1,    2,    3,    4,    5,    6,    7,    8,    9,    $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {30-3F}
   $ff,  10,   11,   12,   13,   14,   15,   $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {40-4F}    //A-F
   $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff,  $ff, {50-5F}
   $ff,  10,   11,   12,   13,   14,   15);                                                       {60-66}    //a-f

type
  TJumpFunction = function(aDataObj: TDataObj): boolean of object;

// This is the magic method jump table that helps us with improving performance in parsing JSON text.
const cJumpTable: array[#0..#127] of pointer =
 (@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,  //#0..#7
  @TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,  //#8..#15
  @TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,  //#16..#23
  @TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,  //#24..#31
  @TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseStringValue,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseStringValue,  //#32..#39:   #34=", #39='
  @TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseNegative,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,     //#40..#47:   #45=-
  @TJsonStreamer.ParseNumber,@TJsonStreamer.ParseNumber,@TJsonStreamer.ParseNumber,@TJsonStreamer.ParseNumber,@TJsonStreamer.ParseNumber,@TJsonStreamer.ParseNumber,@TJsonStreamer.ParseNumber,@TJsonStreamer.ParseNumber,                                          //#48..#55:   '0' - '7'
  @TJsonStreamer.ParseNumber,@TJsonStreamer.ParseNumber,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,            //#56..#63    #56='8', #57='9'
  @TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseFalse,@TJsonStreamer.ParseInvalidChar,        //#64..#71:   #65='I', #70='F'
  @TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseI,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseN,@TJsonStreamer.ParseObjectID,                         //#72..#79:   #78='N', #79='O'
  @TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseTrue,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,         //#80..#87:   #84='T'
  @TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseArray,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,        //#88..#95:   #91='['
  @TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseFalse,@TJsonStreamer.ParseInvalidChar,        //#96..#103:  #102='f'
  @TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseI,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseN,@TJsonStreamer.ParseObjectID,                         //#104..#111: #105='i', #110='n', #111='o'
  @TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseTrue,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,         //#112..#119: #116='t'
  @TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseFrame,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar,@TJsonStreamer.ParseInvalidChar         //#120..#127: #123='{'
);


  {Will bring the CurrentCharPtr forward until it no longer encounters whitespace.   For us here, we are taking a simplified approach in that we are treating all characters less
   than #33 to be considered whitespace (tabs, carriage returns, linefeeds, space, etc).
   Note also, that we are not including high-up unicode characters that are special kinds of white space such as  00A0:No break space,
   200B:Zero width Space, 3000:ideographic space, 202F: narrow no-break space, 205F: Medium mathematical space, FEFF:Zero width no-break space, 2000-200B: assorted space characters, etc.
   This approach should probably be just fine since these special space characters are valid within JSON strings, but probably are not valid as white-space between JSON elements. }
procedure TJsonStreamer.SkipSpaces;
begin
  while (fCurrentCharPtr <= fEndPtr) and (fCurrentCharPtr^ < #33) do
    inc(fCurrentCharPtr);
end;

function TJsonStreamer.CurrentChar: Char;
begin
  result := fCurrentCharPtr^;
end;

function TJsonStreamer.eof: Boolean;
begin
  result := fCurrentCharPtr > fEndPtr;
end;

function TJsonStreamer.HasNumberOfChars(aCount: integer): boolean;
var
  lPointer: PChar;
begin
  lPointer := fCurrentCharPtr;
  inc(lPointer, aCount);
  result := lPointer <= fEndPtr;
end;

procedure TJsonStreamer.IncIndex;
begin
  inc(fCurrentCharPtr);
end;

//************************************************************************************
// The following block of functions are called to by the cJumpTable during decoding.
//************************************************************************************

function TJsonStreamer.ParseInvalidChar(aDataObj: TDataObj): boolean;
begin
  result := false;   // This is a handler routine for the jumpTable so we can handle characters in the jumpTable that we should never encounter unless we have invalid JSON or we are allowing the parsing of Symbols.
end;

// Try to parse a string from fJSON and along the way, do any special escaping handling such as \uxxxxx, etc.
function TJsonStreamer.ParseString(var oString: string): boolean;
var
  lSB: TStringBuilder;     // This may be nil if it is not needed.  Only needed IF/When we need to do string concatenations due to escape processing.
  lStartsWithSingleQuote: boolean;
  lUnicodeCH1: cardinal;   // should this be a word?
  lUnicodeCH2: cardinal;   // should this be a word?
  lUnicodeCH3: cardinal;   // should this be a word?
  lUnicodeCH4: cardinal;   // should this be a word?
  lChar: char;
  lChPtr: PChar;
  lStartPtr: PChar;
  lCharCount: integer;

  procedure AppendWorkStringWithChar(aNewChar: Char);
  var
    lSize: integer;
  begin
    if not assigned(lSB) then
    begin
      lSize := 1024;               // lets start with a decent starting buffer size, much bigger than the normal default
      if lCharCount > lSize then
      begin
        lSize := lCharCount * 3 div 2;   // 50% more than we need so we have some initial room to add more.
      end;
      lSB:=TStringBuilder.Create(lSize);
    end;

    // will move the characters from the source buffer we are looking through to the WorkString and append aNewChar which came from the escaping.
    lSB.Append(lStartPtr, 0, lCharCount);
    lSB.Append(aNewChar);
    lStartPtr := lChPtr;    // make our new starting Pointer be where our current working pointer is now.
    lCharCount := 0;
  end;

  procedure FinishWorkString;
  begin
    // will move the bytes from the source buffer we are looking through to the WorkString
    if assigned(lSB) then
    begin
      lSB.Append(lStartPtr, 0, lCharCount);    // Only add our last string section if we have a string Builder already made to add it to. It's OK if lCharCount is zero.
      oString := lSB.ToString(True);
    end
    else
    begin
      SetString(oString, lStartPtr, lCharCount);    // everything from the original start position to the end of what we want will be extracted out in one swoop as no escaping needed to be handled.
    end;
    fCurrentCharPtr := lChPtr+1;          // Tell our parsing context that we finished parsing the string all the way up through the ending quote by positioning it to one past that character.
  end;

  function LocalHexConvert(aChr: Char): Byte; inline;
  begin
    if aChr <= 'f' then
    begin
      {$R-}   // don't need range checking because the range was just limited in the above IF.
      result := cHexDecimalConvert[ord(aChr)];
      {$R+}
    end
    else
    begin
      result := $FF; // code for "invalid"
    end;
  end;

begin
  SkipSpaces;
  result := false;
  lSB := nil;   // Only need a StringBuilder if we need to handle escaping.

  if eof then
    exit;

  try
    lStartsWithSingleQuote := false;
    lChPtr := fCurrentCharPtr;
    lChar := lChPtr^;
    if (lChar = '''') then
    begin
      //technically speaking, json doesn't really support single quotes for string definitions.  however, we are going to support it.  JSON5 allows it.  maybe someday we will have an option to restrict to just explicitly valid json
      lStartsWithSingleQuote := true;
    end
    else if (lChar <> '"') then
    begin
      // didn't begin with a double quote or a single quote so can't be a valid string.
      exit;
    end;

    inc(lChPtr);  // Move up to the first actual character in our string

    // Go through all the characters looking for the end character (either a double quote or single quote depending on how we started).
    // along the way, we need to decode any escaping.
    lStartPtr := lChPtr;
    lCharCount := 0;

    while (lChPtr <= fEndPtr) do
    begin
      lChar := lChPtr^;

      if (lChar='\') then
      begin
        // we have the starting of an escaper character so start an escape read.
        inc(lChPtr);
        if (lChPtr <= fEndPtr) then
        begin
          lChar := lChPtr^;
          case lChar of
            '\': begin inc(lChPtr); AppendWorkStringWithChar('\'); end;
            '"': begin inc(lChPtr); AppendWorkStringWithChar('"'); end;
            '/': begin inc(lChPtr); AppendWorkStringWithChar('/'); end;
            'b': begin inc(lChPtr); AppendWorkStringWithChar(#8); end;   //backspace
            'f': begin inc(lChPtr); AppendWorkStringWithChar(#12); end;  //Form Feed
            'n': begin inc(lChPtr); AppendWorkStringWithChar(#10); end;  //New Line
            'r': begin inc(lChPtr); AppendWorkStringWithChar(#13); end;  //carriage return
            't': begin inc(lChPtr); AppendWorkStringWithChar(#9); end;   //tab
            'u':
              begin
                inc(lChPtr);   // Get past the 'u' character
                if lChPtr+3*sizeof(Char) <= fEndPtr then
                begin
                  lUnicodeCH1 := LocalHexConvert(lChPtr^);
                  inc(lChPtr);
                  lUnicodeCH2 := LocalHexConvert(lChPtr^);
                  inc(lChPtr);
                  lUnicodeCH3 := LocalHexConvert(lChPtr^);
                  inc(lChPtr);
                  lUnicodeCH4 := LocalHexConvert(lChPtr^);
                  inc(lChPtr);    // Now points past the 4 escaping characters
                  if (lUnicodeCH1 <= 15) and (lUnicodeCH2 <= 15) and (lUnicodeCH3 <= 15) and (lUnicodeCH4 <= 15)then
                  begin
                    lChar := WideChar((lUnicodeCH1 shl 12) or (lUnicodeCH2 shl 8) or (lUnicodeCH3 shl 4) or lUnicodeCH4);  // we have four valid hex characters so stitch them together into a unicode Character.
                    AppendWorkStringWithChar(lChar);
                  end
                  else
                  begin
                    RaiseParsingException(cExceptInvalidCharacterParsingEscape, lChPtr);
                  end;
                end
                else
                begin
                  // we started with an /u sequence but there aren't enough characters to pick up 4 more.
                  RaiseParsingException(cExceptInvalidCharacterParsingEscape, lChPtr);
                end;
              end;
            else
            begin
              // This must be an error condition because an invalid character is following the excape '\' character.
              RaiseParsingException(cExceptInvalidCharacterAfterEscape, lChPtr);
            end;
          end;
        end
        else
        begin
          // we are out of characters to pickup in an escape sequence.
          RaiseParsingException(cExceptInvalidCharacterAfterEscape, lChPtr);
        end;
      end

      else if lChar = '"' then   // this should be an end of string marker unless we started with a single quote.
      begin
        if lStartsWithSingleQuote then
        begin
          // just take this character as a normal character by counting up to include it.
          inc(lChPtr);
          inc(lCharCount);
        end
        else
        begin
           // Finish our string.
          FinishWorkString;
          result := true;
          break;
        end;
      end

      else if lChar = '''' then
      begin
        if lStartsWithSingleQuote = false then
        begin
          // just take this character as a normal character by counting up to include it.
          inc(lChPtr);
          inc(lCharCount);
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
        // just take this character as a normal character by counting up to include it.
        inc(lChPtr);
        inc(lCharCount);
      end;
    end;

    // If we get out of this loop above without receiving the closing quote.  That's an error condition as we ran out of characters to process.
    // If we got out of the loop above by calling the FinishWorkString, then we know that our result was set to true and the output string was created and returned.
  finally
    lSB.Free;
  end;
end;

function TJsonStreamer.ParseStringValue(aDataObj: TDataObj): boolean;
var
  lString: string;
begin
  result := ParseString(lString);
  if result then
    aDataObj.AsString := lString;
end;

// Try to parse a number from fJSON
// This could read and populate an integer(int64, int32, or byte) or a double float.
// We are not having it read in single floats cause it's not obvious to decide when to choose a single and when to choose a double.  Could be a future improvement maybe.
// Note that technically the JSON spec doesn't allow a number to start out with the '+' character such as "+10", but we are going to allow it here.
// I do believe that JSON5 does technically allow it, though.
// Note that a number like ".123" is not valid JSON without a leading zero, but I believe it is valid JSON5, so we will allow it as well.

function TJsonStreamer.ParseNumber(aDataObj: TDataObj): boolean;
var
  lContinue: boolean;
  lCanBePlusOrMinus: boolean;
  lWholeCount: integer;
  lIsFloat: boolean;
  lWholeInt64: int64;
  lExpInt64: int64;
  lIsExponent: boolean;
  lChPtr: PChar;
  lChar: Char;
  lExtended: Extended;
  lSavedChar: Char;
  lExpCount: integer;
  lBase: int64;
  lDouble: Double;
  lIsNegative: boolean;

  procedure PerformLocalTextToFloat;
  begin
    // use the Delphi way of converting our string characters to a floaing point number.  should handle exponents, etc.
    lSavedChar := lChPtr^;
    lChPtr^ := #0;            // need to temporarily put in a null for the TextToFloat call next
    if TextToFloat(fCurrentCharPtr, lExtended, TFloatValue.fvExtended) then
    begin
      aDataObj.AsDouble := lExtended;     // assuming all floats are doubles.  not trying singles.
    end
    else
    begin
      // Error Condition of some sort?
      lChPtr^ := lSavedChar;   // restore this before we raise the exception and leave this method without restoring it below.
      RaiseParsingException(cExceptUnableToConvertText, fCurrentCharPtr);
    end;
    lChPtr^ := lSavedChar;   // restore.
  end;

begin
  result := false;
  if eof then exit;

  lContinue := true;
  lCanBePlusOrMinus := true;                // + or - characters can only come in as the first character or directly after an "e"
  lWholeCount := 0;
  lWholeInt64 := 0;                    // build this along the way
  lExpInt64 := 0;                      // build this along the way
  lExpCount := 0;
  lIsFloat := false;
  lIsExponent := false;
  lIsNegative := false;


  lChPtr := fCurrentCharPtr;

  while (lContinue and not eof) do
  begin
    lChar := lChPtr^;
    if (lChar >= '0') and (lChar <= '9') then
    begin
      if lIsFloat then
      begin
        if lIsExponent=false then  // Once we hit an exponent, then I will no longer be doing this conversion and will fall back to a delphi call
        begin
          // FINISH - need to do overflow checking here
          lExpInt64 := (lExpInt64 * 10) + ord(lChar) - ord('0');
          inc(lExpCount);
        end;
      end
      else
      begin
        // Need to do overflow checking here via exception
        try
          lWholeInt64 := (lWholeInt64 * 10) + ord(lChar) - ord('0');
          inc(lWholeCount);
        except
          on e: exception do
          begin
            RaiseParsingException(cExceptNumberTooBigWhen,lChPtr)
          end;
        end;
      end;
      lCanBePlusOrMinus := false;
      inc(lChPtr);
      result := true;       // now that we have received at least one digit, we can possibly have a true result.
    end
    else if lCanBePlusOrMinus and ((lChar = '+') or (lChar = '-')) then
    begin
      lCanBePlusOrMinus := false;
      inc(lChPtr);
      if (lIsExponent=false) and (lChar = '-') then
        lIsNegative := true;
    end
    else if (lIsFloat=false) and (lChar = '.') and (lWholeCount > 0) then
    begin
      lIsFloat := true;
      lCanBePlusOrMinus := false;
      inc(lChPtr);
    end
    else if (lWholeCount=1) and (lIsExponent = false) and ((lChar = 'e') or (lChar = 'E')) then
    begin
      lIsFloat := true;
      lCanBePlusOrMinus := true;
      lIsExponent := true;
      inc(lChPtr);
    end
    else
    begin
      // None of the possible conditions above absorbed this character so we are done trying to read this number.
      if result then
      begin
        // we either found an integer number or a floating point number
        if lIsFloat then
        begin
          if lIsExponent then
          begin
            // we have an exponent in our number text, so we fall back to using Delphi's TextToFloat, but we need to setup a null ending character first.
            PerformLocalTextToFloat;
          end
          else
          begin
            // here we have a lWholeInt64 filled in and we have a lExpInt64 filled in.  Smash the two together to make a double
            if lExpCount<19 then   // check on the character limit of the fractional part which we have to limit to under the character count of the biggest int64.
            begin
              lBase := 10;
              dec(lExpCount);
              while (lExpCount>0) do
              begin
                lBase := lBase * 10;
                dec(lExpCount);
              end;
              lDouble := double(lWholeInt64) + (Double(lExpInt64) / lBase);
              if lIsNegative then
                aDataObj.AsDouble := lDouble * -1
              else
                aDataObj.AsDouble := lDouble;
            end
            else
            begin
              //The fraction part is just too many digits, so just fall back back to Delphi's TextToFloat and let it go how it goes.
              PerformLocalTextToFloat;
            end;
          end;
        end
        else
        begin
          // NOTE: someday, we may need to be able to support unsigned Int64 sized numbers.

          // choose the smallest integer data type to hold this value.
          if (lWholeInt64 > $7FFFFFFF) then
          begin
            if lIsNegative then
              aDataObj.AsInt64 := -lWholeInt64
            else
              aDataObj.AsInt64 := lWholeInt64;
          end
          else if (lIsNegative=false) and (lWholeInt64<=255) then
          begin
            aDataObj.AsByte := lWholeInt64;
          end
          else
          begin
            if lIsNegative then
              aDataObj.AsInt32 := -lWholeInt64
            else
              aDataObj.AsInt32 := lWholeInt64;
          end;
        end;

        fCurrentCharPtr := lChPtr;    // move up our parsing context to the new current character that is after processing number string
      end;

      lContinue := false;
    end;
  end;
end;


//Try to parse an array from fJSON.  Needs to start with "["
// return 0 if this is not the start of an array
// return 1 if this is an array and all the items in the array serialized and completed the array correctly
// return 2 if there is an error where we could not reach the end of the array.
function TJsonStreamer.ParseArray(aDataObj: TDataObj): boolean;
var
  lArray: TDataArray;
begin
  result := false;
  if eof then exit;
  if not (CurrentChar = '[') then exit;   // not the start of an array so get out.

  result := true;
  IncIndex;

  SkipSpaces;
  lArray := aDataObj.AsArray;
  while not (eof) and (CurrentChar <> ']') do
  begin
    if ParseAnyType(lArray.NewSlot) then
    begin
      SkipSpaces;
      if (not eof) and (CurrentChar = ',') then
      begin
        IncIndex;
      end
      else
      begin
        break; // we are done reading the elements in this array.  The current byte could be the ending ] character or it could be invalid data that we error out on next.
      end;
    end
    else
    begin
      // We were not able to parse this item in the array. Maybe nothing was parsed, or maybe an item was paritally parsed.
      // However, there was something there that could not be parsed.  So, the code below will handle that.
    end;
  end;

  if not (eof) and (CurrentChar <> ']') then
  begin
    RaiseParsingException(cExceptErrorParsingAnArray, fCurrentCharPtr);    // We parsed some stuff, but errored out here because we are not getting the end of array marker.
  end;

  IncIndex;   // get moved past the ] chracter.
end;

// Try to parse the INF identifier from fJSON to represent Infinity.
// NOTE that parsing this is not really supported by the core JSON spec.  But we support it along with Nan
// This seems like a lot of code, but it is writting in such a way to be fast and efficient to execute.
// In order to call this function, our parsing context must already have been determined to point to a 'I' or 'i' character
function TJsonStreamer.ParseInfinity(aDataObj: TDataObj): boolean;
const
  cHighCases: array[0..7] of char = ('I','N','F','I','N','I','T','Y');
  cLowCases: array[0..7] of char = ('i','n','f','i','n','i','t','y');
var
  lPtr: PChar;
  lPtrHigh: PChar;
  lPtrLow: PChar;
  i: Cardinal;
begin
  result := false;
  if HasNumberOfChars(8) then
  begin
    lPtr := fCurrentCharPtr;
    lPtrHigh := cHighCases;
    lPtrLow := cLowCases;

    for i := low(cHighCases) to High(cHighCases) do
    begin
      if not ((lPtr^=lPtrLow^) or (lPtr^=lPtrHigh^)) then
      begin
        exit; // did not have a character to character match
      end;
      inc(lPtr);
      inc(lPtrLow);
      inc(lPtrHigh);
    end;

    result := true;   // if we did not exit above then we had a match to our constant string.
    inc(fCurrentCharPtr, 8);
    aDataObj.AsDouble := Double.PositiveInfinity;    // Setting as INF         // Note that we do not support a Single version of Nan
  end;
end;

// Try to parse the -INF identifier from fJSON to represent Negative Infinity.
// In order to call this function, our parsing context must already have been determined to point to a '-' character
function TJsonStreamer.ParseNegativeInfinity(aDataObj: TDataObj): boolean;
const
  cHighCases: array[0..8] of char = ('-','I','N','F','I','N','I','T','Y');
  cLowCases: array[0..8] of char = ('-','i','n','f','i','n','i','t','y');
var
  lPtr: PChar;
  lPtrHigh: PChar;
  lPtrLow: PChar;
  i: Cardinal;
begin
  result := false;
  if HasNumberOfChars(9) then
  begin
    lPtr := fCurrentCharPtr;
    lPtrHigh := cHighCases;
    lPtrLow := cLowCases;

    for i := low(cHighCases) to High(cHighCases) do
    begin
      if not ((lPtr^=lPtrLow^) or (lPtr^=lPtrHigh^)) then
      begin
        exit; // did not have a character to character match
      end;
      inc(lPtr);
      inc(lPtrLow);
      inc(lPtrHigh);
    end;

    result := true;   // if we did not exit above then we had a match to our constant string.
    inc(fCurrentCharPtr, 9);
    aDataObj.AsDouble := Double.NegativeInfinity;    // Setting as -INF         // Note that we do not support a Single version of Nan
  end;
end;

// Try to parse the INF identifier from fJSON to represent Infinity.
// NOTE that parsing this is not really supported by the core JSON spec.  But we support it along with Nan
// This seems like a lot of code, but it is writting in such a way to be fast and efficient to execute.
// In order to call this function, our parsing context must already have been determined to point to a 'I' or 'i' character
function TJsonStreamer.ParseInf(aDataObj: TDataObj): boolean;
const
  cHighCases: array[0..2] of char = ('I','N','F');
  cLowCases: array[0..2] of char = ('i','n','f');
var
  lPtr: PChar;
  lPtrHigh: PChar;
  lPtrLow: PChar;
  i: Cardinal;
begin
  result := false;
  if HasNumberOfChars(3) then
  begin
    lPtr := fCurrentCharPtr;
    lPtrHigh := cHighCases;
    lPtrLow := cLowCases;

    for i := low(cHighCases) to High(cHighCases) do
    begin
      if not ((lPtr^=lPtrLow^) or (lPtr^=lPtrHigh^)) then
      begin
        exit; // did not have a character to character match
      end;
      inc(lPtr);
      inc(lPtrLow);
      inc(lPtrHigh);
    end;

    result := true;   // if we did not exit above then we had a match to our constant string.
    inc(fCurrentCharPtr, 3);
    aDataObj.AsDouble := Double.PositiveInfinity;    // Setting as INF         // Note that we do not support a Single version of Nan
  end;
end;

// Try to parse the -INF identifier from fJSON to represent Negative Infinity.
// In order to call this function, our parsing context must already have been determined to point to a '-' character
function TJsonStreamer.ParseNegativeInf(aDataObj: TDataObj): boolean;
const
  cHighCases: array[0..3] of char = ('-','I','N','F');
  cLowCases: array[0..3] of char = ('-','i','n','f');
var
  lPtr: PChar;
  lPtrHigh: PChar;
  lPtrLow: PChar;
  i: Cardinal;
begin
  result := false;
  if HasNumberOfChars(4) then
  begin
    lPtr := fCurrentCharPtr;
    lPtrHigh := cHighCases;
    lPtrLow := cLowCases;

    for i := low(cHighCases) to High(cHighCases) do
    begin
      if not ((lPtr^=lPtrLow^) or (lPtr^=lPtrHigh^)) then
      begin
        exit; // did not have a character to character match
      end;
      inc(lPtr);
      inc(lPtrLow);
      inc(lPtrHigh);
    end;

    result := true;   // if we did not exit above then we had a match to our constant string.
    inc(fCurrentCharPtr, 4);
    aDataObj.AsDouble := Double.NegativeInfinity;    // Setting as -INF         // Note that we do not support a Single version of Nan
  end;
end;




function TJsonStreamer.ParseNegative(aDataObj: TDataObj): boolean;
begin
  result := ParseNumber(aDataObj);              // Most likely, a '-' sign will give us a number so try that first.
  if not result then
    result := ParseNegativeInfinity(aDataObj);
  if not result then
    result := ParseNegativeInf(aDataObj);
end;

// NOTE:  the following code blocks are coded a bit wierdly considering the individual character comparisons.  Turns out, when looking for specific
//        strings like this, this logic is faster than doing calls like "sameText", "Pos", etc.  So, this code looks a little goofy but it's all done this way for performance.

// Try to parse the true identifier from fJSON
// Prerequisite to calling this function is that the current Character we are processing must have already been determined to be a 't' or 'T', which is what the caller is doing.  So, we skip that check in here because it is redundant.
function TJsonStreamer.ParseTrue(aDataObj: TDataObj): Boolean;
const
  cHighCases: array[0..2] of char = ('R','U','E');
  cLowCases: array[0..2] of char = ('r','u','e');
var
  lPtr: PChar;
  lPtrHigh: PChar;
  lPtrLow: PChar;
  i: Cardinal;
begin
  result := false;
  if HasNumberOfChars(4) then
  begin
    lPtr := fCurrentCharPtr;
    inc(lPtr);                           // we can skip the first character that we are not checking.
    lPtrHigh := cHighCases;
    lPtrLow := cLowCases;

    for i := low(cHighCases) to High(cHighCases) do
    begin
      if not ((lPtr^=lPtrLow^) or (lPtr^=lPtrHigh^)) then
      begin
        exit;    // did not have a character to character match so we must break out with a fail.
      end;
      inc(lPtr);
      inc(lPtrLow);
      inc(lPtrHigh);
    end;

    result := true;   // if we did not exit above then we had a match to our constant string.
    inc(fCurrentCharPtr, 4);
    aDataObj.AsBoolean := true;
  end;
end;


// Try to parse the false identifier from fJSON
// Prerequisite to calling this function is that our parsing context fCurrentCharPtr must have already been determined to be a 'f' or 'F', which is what the caller is doing.  So, we skip that check in here because it is redundant.
function TJsonStreamer.ParseFalse(aDataObj: TDataObj): boolean;
const
  cHighCases: array[0..3] of char = ('A','L','S', 'E');
  cLowCases: array[0..3] of char = ('a','l','s', 'e');
var
  lPtr: PChar;
  lPtrHigh: PChar;
  lPtrLow: PChar;
  i: Cardinal;
begin
  result := false;
  if HasNumberOfChars(5) then
  begin
    lPtr := fCurrentCharPtr;
    inc(lPtr);                           // we can skip the first character that we are not checking.
    lPtrHigh := cHighCases;
    lPtrLow := cLowCases;

    for i := low(cHighCases) to High(cHighCases) do
    begin
      if not ((lPtr^=lPtrLow^) or (lPtr^=lPtrHigh^)) then
      begin
        exit;    // did not have a character to character match so we must break out with a fail.
      end;
      inc(lPtr);
      inc(lPtrLow);
      inc(lPtrHigh);
    end;

    result := true;   // if we did not exit above then we had a match to our constant string.
    inc(fCurrentCharPtr, 5);
    aDataObj.AsBoolean := false;
  end;
end;

// Try to parse the null identifier from fJSON
// Prerequisite to calling this function is that fJSON[aIndex] must have already been determined to be a 'n' or 'N', which is what the caller is doing.  So, we skip that check in here because it is redundant.
// This seems like a lot of code, but it is writting in such a way to be fast and efficient to execute.
function TJsonStreamer.ParseNull(aDataObj: TDataObj): boolean;
const
  cHighCases: array[0..2] of char = ('U','L','L');
  cLowCases: array[0..2] of char = ('u','l','l');
var
  lPtr: PChar;
  lPtrHigh: PChar;
  lPtrLow: PChar;
  i: Cardinal;
begin
  result := false;
  if HasNumberOfChars(4) then
  begin
    lPtr := fCurrentCharPtr;
    inc(lPtr);                           // we can skip the first character that we are not checking.
    lPtrHigh := cHighCases;
    lPtrLow := cLowCases;

    for i := low(cHighCases) to High(cHighCases) do
    begin
      if not ((lPtr^=lPtrLow^) or (lPtr^=lPtrHigh^)) then
      begin
        exit;    // did not have a character to character match so we must break out with a fail.
      end;
      inc(lPtr);
      inc(lPtrLow);
      inc(lPtrHigh);
    end;

    result := true;   // if we did not exit above then we had a match to our constant string.
    inc(fCurrentCharPtr, 4);
    aDataObj.Clear;   // should already be clear, but let's just be explicit.
  end;
end;

// Try to parse the NAN identifier from fJSON
// NOTE that parsing NaN as an identifier for a floating point number value is not really supported by the core JSON spec.  But we support it along with -INF and +INF
// Prerequisite to calling this function is that our parsing context fCurrentCharPtr must have already been determined to be a 'n' or 'N', which is what the caller is doing.  So, we skip that check in here because it is redundant.
function TJsonStreamer.ParseNan(aDataObj: TDataObj): boolean;
const
  cHighCases: array[0..1] of char = ('A','N');
  cLowCases: array[0..1] of char = ('a','n');
var
  lPtr: PChar;
  lPtrHigh: PChar;
  lPtrLow: PChar;
  i: Cardinal;
begin
  result := false;
  if HasNumberOfChars(3) then
  begin
    lPtr := fCurrentCharPtr;
    inc(lPtr);                           // we can skip the first character that we are not checking.
    lPtrHigh := cHighCases;
    lPtrLow := cLowCases;

    for i := low(cHighCases) to High(cHighCases) do
    begin
      if not ((lPtr^=lPtrLow^) or (lPtr^=lPtrHigh^)) then
      begin
        exit;    // did not have a character to character match so we must break out with a fail.
      end;
      inc(lPtr);
      inc(lPtrLow);
      inc(lPtrHigh);
    end;

    result := true;   // if we did not exit above then we had a match to our constant string.
    inc(fCurrentCharPtr, 3);
    aDataObj.AsDouble := 0/0;    // Setting as Nan         // Note that we do not support a Single version of Nan
  end;
end;

function TJsonStreamer.ParseN(aDataObj: TDataObj): boolean;
begin
  result := ParseNull(aDataObj);
  if not result then
    // We have one other situation that may start with a "N" character and that is when sometimes someone produces NaN for a Not-A-Number floating point number.  We check for that extremely rare ( and technically non-JSON compliant ) case here.
    result := ParseNan(aDataObj);
end;





//0=nothing applicable so no parsing done, 1=successful parse, 2=Applicable, but error in parsing.
function TJsonStreamer.ParseObjectID(aDataObj: TDataObj): boolean;
const
  cHighCases: array[0..9] of char = ('O','B','J','E','C','T','I','D','(','"');
  cLowCases: array[0..9] of char = ('o','b','j','e','c','t','i','d','(','"');
var
  lPtr: PChar;
  lPtrHigh: PChar;
  lPtrLow: PChar;
  i: Cardinal;
  lBuffer: array[0..11] of BYTE;
  lPtr2: PChar;
begin
  // Example:  ObjectId("51e80190324277f418000008")
  result := false;
  if AllowParsingExtendedTypes and HasNumberOfChars(36) then
  begin
    lPtr := fCurrentCharPtr;
    lPtrHigh := cHighCases;
    lPtrLow := cLowCases;

    for i := low(cHighCases) to High(cHighCases) do
    begin
      if not ((lPtr^=lPtrLow^) or (lPtr^=lPtrHigh^)) then
      begin
        exit;    // did not have a character to character match so we must break out with a fail.
      end;
      inc(lPtr);
      inc(lPtrLow);
      inc(lPtrHigh);
    end;

    // check that our finalizing " and ) are there correctly.
    lPtr2 := lPtr;   // should be pointing at the first character of the hex objectID value.

    inc(lPtr2, 24);  // move our 2nd pointer to point to the finalizing " character
    if (lPtr2^='"') then
    begin
      inc(lPtr2);
      if (lPtr2^=')') then
      begin
        // we have the structure of an objectID so now try and parse out the string representation of the objectID
        if HexToBin(lPtr, lBuffer, sizeof(lBuffer)) = 0 then
        begin
          inc(fCurrentCharPtr,36);
          result := true;
          Move(lBuffer, aDataObj.AsObjectID.Data, sizeof(lBuffer));
        end
        else
        begin
          RaiseParsingException(cExceptErrorParsingObject, fCurrentCharPtr);
        end;
      end;
    end;
  end;
end;

function TJsonStreamer.ParseISODate(aDataObj: TDataObj): boolean;
const
  cHighCases: array[0..8] of char = ('I','S','O','D','A','T','E','(','"');
  cLowCases: array[0..8] of char = ('i','s','o','d','a','t','e','(','"');
var
  lPtr: PChar;
  lPtrHigh: PChar;
  lPtrLow: PChar;
  i: Cardinal;
  lPtr2: PChar;
  lDateTime: TDateTime;

  function ParseISODateString: boolean;
  begin
    //FINISH
    lDateTime := 0;
    result := false;
  end;
begin
  // Example:  ISODate("2021-01-27T00:19:08.862Z")
  result := false;
  if AllowParsingExtendedTypes and HasNumberOfChars(35) then
  begin
    lPtr := fCurrentCharPtr;
    lPtrHigh := cHighCases;
    lPtrLow := cLowCases;

    for i := low(cHighCases) to High(cHighCases) do
    begin
      if not ((lPtr^=lPtrLow^) or (lPtr^=lPtrHigh^)) then
      begin
        exit;    // did not have a character to character match so we must break out with a fail.
      end;
      inc(lPtr);
      inc(lPtrLow);
      inc(lPtrHigh);
    end;

    // check that our finalizing " and ) are there correctly.
    lPtr2 := lPtr;   // should be pointing at the first character of the hex objectID value.

    inc(lPtr2, 24);  // move our 2nd pointer to point to the finalizing " character
    if (lPtr2^='"') then
    begin
      inc(lPtr2);
      if (lPtr2^=')') then
      begin
        // we have the structure of an ISODate() so now try and parse out the string representation of it
        if ParseISODateString then
        begin
          inc(fCurrentCharPtr,35);
          result := true;
          aDataObj.AsDateTime := lDateTime;
        end
        else
        begin
          RaiseParsingException(cExceptErrorParsingISODate, fCurrentCharPtr);  // The ISODate("") structure was correct, but the contents had characters that were invalid hex.
        end;
      end;
    end;
  end;
end;

function TJsonStreamer.ParseI(aDataObj: TDataObj): boolean;
begin
  result := ParseInfinity(aDataObj);      // formally JSON5 compliant
  if not result then
    result := ParseINF(aDataObj);         // shortcut INF
  if not result then
    result := ParseISODate(aDataObj);     // will only do an ISO date if the feature is turned on.
end;



// Try to parse a slotname from fJSON string.  We are expecting a slotname, so we should be seeing "somename" and a : should be following it.
function TJsonStreamer.ParseSlotname(var oSlotname: string): boolean;
begin
  // Slotnames must be surrounded by " so really it's the same as string data. The slotname must also end with a :
  // Also, any amount of white space can be after the string and before the colon or after the colon and before the next data content.
  // Examples:  "Name" : null, "Name":"Sean", "First Name" : "billy", "Name": 1

  result := ParseString(oSlotName);
  if result then
  begin
    // consume up to the ":"
    SkipSpaces;
    if CurrentChar = ':' then
    begin
      IncIndex;
    end
    else
    begin
      result := false;  // We did parse the slotname, but the ":" is not next and thus the slotname is not really valid.
    end;
  end;
end;

//Try to parse a frame (in JSON terminology, it's called an object) from fJSON.  Needs to start with "{"
//It is already expected that before making a call here that the currentChar is = "}"
function TJsonStreamer.ParseFrame(aDataObj: TDataObj): boolean;
var
  lFrame: TDataFrame;
  lSlotName: string;
begin
//  result := false;
//  if eof then exit;
//  if not (CurrentChar = '{') then exit;

  result := true;  // we are a frame and although we didn't load much yet, we at least already know that we have started a frame.
  IncIndex;   // move past the }
  SkipSpaces;
  lFrame := aDataObj.AsFrame;

  while (not eof) and (CurrentChar <> '}') do
  begin
    if ParseSlotName(lSlotName) then
    begin
      if ParseAnyType(lFrame.NewSlot(lSlotName)) then
      begin
        SkipSpaces;
        if (not eof) and (CurrentChar = ',') then
        begin
          IncIndex;
        end
        else
        begin
          break;
        end;
      end
      else
      begin
        RaiseParsingException(cExceptInvalidTokenParsing, fCurrentCharPtr);
      end;
    end
    else
    begin
        RaiseParsingException(cExceptInvalidTokenParsing, fCurrentCharPtr);
      // Error reading the slotname.
    end;
  end;

  // check for proper frame ending.
  if not(eof) and (CurrentChar <> '}') then
  begin
    RaiseParsingException(cErrorErrorParsingJSON, fCurrentCharPtr);
  end;

  IncIndex; // consume closing '}'
end;

function TJsonStreamer.TryParsingSymbol(aDataObj: TDataObj): integer;  //0=nothing applicable, 1=successful parse, 2=Applicable, but error in parsing.
var
  lChPtr: PChar;
  lchar: Char;
  lString: String;
  lCharCount: integer;
begin
  result := 0;   // FINISH this with new implementation.

  // If we are trying to parse a symbol, then the data that was tried so far by other code did not pick up a valid json value like a number or a true, false, etc.
  // it is also not a string with valid starting quotes.   So, we can simply take all characters that come in up until an expected ending identifier.  These would
  // be the , character or the } character or the ] character. There is no escaping possible with a symbol so really these characters cannot be in a symbol.

  lChPtr := fCurrentCharPtr;
  lCharCount := 0;
  while (not eof) do
  begin
    lChar := lChPtr^;
    if (lChar = ',') or (lChar = '}') or (lChar=']') then
    begin
      // We can end this symbol and take this string into aDataObj
      SetString(lString, fCurrentCharPtr, lCharCount);    // everything from the original start position to the end of what we want will be extracted out in one swoop as no escaping needed to be handled.
      aDataObj.AsSymbol := lString;
      result := 1;       // now that we have received at least one digit, we can possibly have a true result.
      break;
    end
    else
    begin
      inc(lChPtr);    // move up to the next character to test.
      inc(lCharCount);
    end;
  end;

  fCurrentCharPtr := lChPtr;    // move up our parsing context to the new current character that is after what we just processed out into a symbol leaving at a , } or ] or possibly the end of our input buffer
end;

// This function will parse whatever JSON data type it can find next and will put that data into aDataObj
function TJsonStreamer.ParseAnyType( aDataObj: TDataObj): boolean;  // result 0=Not Parsed, 1=Successfully Parsed, 2=Partially Successfully parsed (example: an array was successfully parsing, but errored out on the nth item in the array)
var
  lChar: Char;
  lJump: TJumpFunction;
begin
  SkipSpaces;

  // We want to look at the next character to quickly determine which direction we could "possibly" go to see if we are trying to load a particular "known" JSON keyword.
  // By looking at this character, we can decide what possibility to check.
  lChar := CurrentChar;
  if CurrentChar>#127 then
    RaiseParsingException(cExceptInvalidTokenParsing, fCurrentCharPtr);

{$R-}  // can turn off range checking because we just checked it above.  NOTE:  turning this off only saves us two CPU instructions.
  TMethod(lJump).Code := cJumpTable[lChar];
  TMethod(lJump).Data := self;
  result := lJump(aDataObj);                         // THIS IS THE MAGIC.  DO A JUMP TO THE PARSING CODE THAT WILL MOST LIKELY BE THE RIGHT ONE TO HANDLE THE NEXT SEQUENCE OF CHARACTERS.
{$R+}

  if not result then
  begin
    // Was not able to parse any of the regular JSON data types at this point from either the above jump table to trying any of the options, or from those attempts not being successful.
    // SO, see if we are accepting Symbols to be parsed.
    if AllowParsingSymbols then
    begin
      // We treat symbols as anything up to a possible ending case such as a "," or "]" or "}"
      if TryParsingSymbol(aDataObj) = 1 then
      begin
        // successfully parsed a symbol
        result := true;
      end;
    end;
  end;

  if not result then
  begin
    RaiseParsingException(cExceptInvalidTokenParsing, fCurrentCharPtr);
  end;
end;


{$R-}  // turn off range checking so we can maximize speed.  the bit-shifting and binary operations below will keep our indexes in range already.

// This will write out a string to the fStringBuilder and it will escape any characters in the aString that need to be escaped for proper JSON.
// It will also behave according to the fEncodeNonAsciiCharacters property to also escape characters above 127 if it is set to true.
procedure TJsonStreamer.WriteStringEncodingSpecialJSONCharacters(const aString: string);
var
  lChar: char;
  lUnicodeValue: Cardinal;
  lLen: integer;
  lCharArray: array[0..6] of char;
  lPChar: PChar;
  lPEndChar: PChar;
  lPStartChar: PChar;
  lCharCount: integer;

  const cHexChars: array[0..15] of char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');

  function Hex(const aDigit: Cardinal): char; inline;     // we have the range-checking turned off with a compiler switch above so we must make sure that calls to this function do not allow aDigit to be incoming above 15.
  begin
    Result := cHexChars[aDigit];
  end;

  procedure WriteWhatWasScannedSoFar;
  begin
    fStringBuilder.Append(lPStartChar, 0, lCharCount);
    inc(lPChar);               // Move up to the next character
    lPStartChar := lPChar;     // and make it the new starting point
    lCharCount := 0;
  end;

begin
  lLen := length(aString);

  if lLen>0 then
  begin
    lPChar := @aString[1];
    lPEndChar := @aString[lLen];
    lPStartChar := lPChar;
    lCharCount := 0;

    while (lPChar <= lPEndChar) do
    begin
      lChar := lPChar^;
      if (lChar < #32) then
      begin
        case lchar of
          #$8: begin
            WriteWhatWasScannedSoFar;
            fStringBuilder.Append('\b');
          end;
          #$9: begin
            WriteWhatWasScannedSoFar;
            fStringBuilder.Append('\t');
          end;
          #$a: begin
            WriteWhatWasScannedSoFar;
            fStringBuilder.Append('\n');
          end;
          #$c: begin
            WriteWhatWasScannedSoFar;
            fStringBuilder.Append('\f');
          end;
          #$d: begin
            WriteWhatWasScannedSoFar;
            fStringBuilder.Append('\r');
          end
          else
          begin
            lUnicodeValue := Ord(lChar);
            WriteWhatWasScannedSoFar;

            lCharArray[0] := '\';
            lCharArray[1] := 'u';
            lCharArray[2] := '0';
            lCharArray[3] := '0';
            lCharArray[4] := Hex((lUnicodeValue shr 4) and $F);
            lCharArray[5] := Hex(lUnicodeValue and $F);
            fStringBuilder.Append(Pchar(@lCharArray),0,6);     // we are casting to PChar because if we don't then the compiler will automatically turn our array of chars into a string and call the string version which is more expensive.
          end
        end;  // case
      end

      else if (lChar='"') then
      begin
        WriteWhatWasScannedSoFar;
        fStringBuilder.Append('\"');
      end
      else if (lchar='\') then
      begin
        WriteWhatWasScannedSoFar;
        fStringBuilder.Append('\\');
      end

      else if (fEncodeNonAsciiCharacters and (lChar > #127)) then
      begin
        // I've seen some parsers always escape the non-ascii characters, but there's no reason to do so as long as what is produced is transferred using an encoding
        // that is fully compatible with unicode such as 2-byte unicode or UTF-8.
        // But, this code is here if someone out there wants to encode non-ascii characters such as being in a situation where you are writing to an ASCII encoded file.
        lUnicodeValue := Ord(lChar);
        WriteWhatWasScannedSoFar;
        lCharArray[0] := '\';
        lCharArray[1] := 'u';
        lCharArray[2] := Hex(lUnicodeValue shr 12);
        lCharArray[3] := Hex((lUnicodeValue shr 8) and $F);
        lCharArray[4] := Hex((lUnicodeValue shr 4) and $F);
        lCharArray[5] := Hex(lUnicodeValue and $F);
        fStringBuilder.Append(Pchar(@lCharArray),0,6);     // we are casting to PChar because if we don't then the compiler will automatically turn our array of chars into a string and call the string version which is more expensive.
      end
      else
      begin
        // Normal character so move on to the next one.
        inc(lPChar);
        inc(lCharCount);
      end;
    end;

    WriteWhatWasScannedSoFar;
  end;
end;
{$R+}  // restore range checking




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

function TDataObjectsJSONHelper.GetAsJSON: string;
begin
  result := TJsonStreamer.DataObjToJson(self, cJSONTight);
end;

function TDataObjectsJSONHelper.JSONFormatted(aIndention: byte = 2): string;
begin
  result := TJsonStreamer.DataObjToJson(self, cJsonHumanReadable, aIndention);
end;

procedure TDataObjectsJSONHelper.setJSON(const Value: string);
begin
  TJSONStreamer.JsonToDataObj(Value, self);
end;

{ TJsonStreamer }
constructor TJsonStreamer.Create(aStream: TStream);
begin
  inherited Create(aStream);
  fStyle := cJSONTight;
  fIndention := 2;   // irrelevant for the tight streaming but we set it anyway in case it is changed.
  fFormatSettings:=TFormatSettings.Create;
  fFormatSettings.DecimalSeparator := '.';     // used when parsing JSON cause json only allows '.' for decimal separators no matter what locale you are in.
  fEncoding := TEncoding.UTF8;
  fAllowParsingSymbols := true;
  fInitialBufferCapacity := 1024;
end;

constructor TJsonStreamer.Create(aStyle: TJsonStyle = cJSONTight; aIndention: byte = 2);
begin
  Create(nil);
  fStyle := aStyle;
  fIndention := aIndention;
end;

constructor TJsonStreamer.Create(aStream: TStream; aEncoding: TEncoding; aStyle: TJsonStyle = cJSONTight; aIndention: byte = 2);
begin
  Create(aStream);
  fStyle := aStyle;
  fIndention := aIndention;
  fStream := aStream;
  fEncoding := aEncoding;
end;



class function TJsonStreamer.CreateDataObjFromJSON(const aJson: string): TDataObj;
begin
  result := TDataObj.Create;
  try
    JsonToDataObj(aJSON, result);
  except
    FreeAndNil(result);
    raise;
  end;
end;

function TJsonStreamer.DecInd: string;
begin
  if Style = cJsonHumanReadable then
    fIndent := fIndent - fIndention;
  result := StringOfChar(' ',fIndent);
end;

class function TJsonStreamer.Description: string;
begin
  result := 'JavaScript Object Notation.  https://www.json.org and https://en.wikipedia.org/wiki/JSON';
end;

destructor TJsonStreamer.Destroy;
begin
  //NOTE:  do not free the fEncoding because that is management globally.  we just have a reference to it.
  FreeAndNil(fStringBuilder);
  inherited;
end;

function TJsonStreamer.IncInd: string;
begin
  if Style = cJsonHumanReadable then
    fIndent := fIndent + fIndention;
  result := StringOfChar(' ',fIndent);
end;

procedure TJsonStreamer.RaiseParsingException(aMsg: string; aCharacterPos: PChar);
var
  lInt: NativeInt;
begin
  lInt := NativeInt(aCharacterPos) - NativeInt(@JSON[1]);
  raise EJSONParsingException.CreateFmt(SJsonParseMessageAt,[aMsg, lInt]);
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
  lDataObjectID: TDataObjectID;
  lStr: string;
  lP: PCardinal;

  function FloatToJson(const Value: Extended): string;
  var
    Buffer: array[0..63] of Char;
    L: Integer;
  begin
    L := FloatToText(Buffer, Value, fvExtended, ffGeneral, 18, 0, gJSONFormatSettings);
    Buffer[L] := #0;
    if (StrScan(Buffer, '.') = nil) and (StrScan(Buffer, 'E') = nil) then
    begin
      Buffer[L] := '.';
      Buffer[L + 1] := '0';
      Inc(L, 2);
    end;
    SetString(Result, Buffer, L);
  end;

begin
  case aDataobj.DataType.Code of
    cDataTypeNull:
    begin
      fStringBuilder.Append('null');
    end;

    cDataTypeBoolean:
    begin
      if aDataObj.AsBoolean then
        fStringBuilder.Append('true')
      else
        fStringBuilder.Append('false');
    end;

    cDataTypeByte:
    begin
      fStringBuilder.Append(aDataobj.getStore^.fDataByte);
    end;

    cDataTypeInt32:
    begin
      fStringBuilder.Append(aDataobj.getStore^.fDataInt32);
    end;

    cDataTypeInt64:
    begin
      fStringBuilder.Append(aDataobj.getStore^.fDataInt64);
    end;

    cDataTypeSingle: begin
      if fSupportJSON5 then
      begin
        case aDataObj.AsSingle.SpecialType of
          fsZero: fStringBuilder.Append('0.0');
          fsNZero: fStringBuilder.Append('-0.0');
  //        fsDenormal
  //        fsNDenormal
  //        fsPositive
  //        fsNegative
          fsInf: fStringBuilder.Append('Infinity');
          fsNInf: fStringBuilder.Append('-Infinity');
          fsNaN:  fStringBuilder.Append('NaN');
        else
          fStringBuilder.Append(FloatToJSON(aDataobj.getStore^.fDataSingle));
        end;
      end
      else
      begin
        case aDataObj.AsSingle.SpecialType of
          fsZero: fStringBuilder.Append('0.0');       // Doubles are often zero and this is faster than doing a FloatToStr.
          fsNZero: fStringBuilder.Append('-0.0');
  //        fsDenormal
  //        fsNDenormal
  //        fsPositive
  //        fsNegative
          fsInf: fStringBuilder.Append('null');
          fsNInf: fStringBuilder.Append('null');
          fsNaN: fStringBuilder.Append('null');
        else
          fStringBuilder.Append(FloatToJSON(aDataobj.getStore^.fDataSingle));
        end;
      end;
    end;

    cDataTypeDouble: begin
      // Singles and Doubles are capable of being -INF, INF or NaN.  But, these are not valid strings for JSON.  They are, however, valid for JSON5  See https://spec.json5.org/
      if fSupportJSON5 then
      begin
        case aDataObj.AsDouble.SpecialType of
          fsZero: fStringBuilder.Append('0.0');
          fsNZero: fStringBuilder.Append('-0.0');
  //        fsDenormal
  //        fsNDenormal
  //        fsPositive
  //        fsNegative
          fsInf: fStringBuilder.Append('Infinity');
          fsNInf: fStringBuilder.Append('-Infinity');
          fsNaN:  fStringBuilder.Append('NaN');
        else
          fStringBuilder.Append(FloatToJSON(aDataobj.getStore^.fDataDouble));
        end;
      end
      else
      begin
        case aDataObj.AsDouble.SpecialType of
          fsZero: fStringBuilder.Append('0.0');       // Doubles are often zero and this is faster than doing a FloatToStr.
          fsNZero: fStringBuilder.Append('-0.0');
  //        fsDenormal
  //        fsNDenormal
  //        fsPositive
  //        fsNegative
          fsInf: fStringBuilder.Append('null');
          fsNInf: fStringBuilder.Append('null');
          fsNaN: fStringBuilder.Append('null');
        else
          fStringBuilder.Append(FloatToJSON(aDataobj.getStore^.fDataDouble));
        end;
      end;
    end;

//    cDataTypeDecimal128: aStringBuilder.Append(lStore.dataInt64);

    cDataTypeDateTime:
    begin
      fStringBuilder.Append('"');
      fStringBuilder.Append(DateTimeToISO8601Str(aDataobj.AsDateTime));
      fStringBuilder.Append('"');
    end;

    // for the unix UTC time do we publish in string notation or in the int64 notation.    I say int64 notation since that's the purpose of this data type
    cDataTypeUTCDateTime:
    begin
      fStringBuilder.Append(aDataobj.getStore^.fDataInt64);
    end;

    cDataTypeDate:
    begin
      fStringBuilder.Append('"');
      fStringBuilder.Append(DateToISO8601Str(aDataobj.AsDateTime));
      fStringBuilder.Append('"');
    end;

    cDataTypeTime:
    begin
      fStringBuilder.Append('"');
      fStringBuilder.Append(TimeToISO8601Str(aDataobj.AsDateTime));
      fStringBuilder.Append('"');
    end;

   cDataTypeObjectID:
    begin
      fStringBuilder.Append('"');
      lDataObjectID := aDataobj.AsObjectID;

      SetLength(lStr, 24);
      lP := Pointer(lStr);
      for i := low(lDataObjectID.Data) to high(lDataObjectID.Data) do
      begin
        PCardinal(lP)^ := cTwoHexLookup[lDataObjectID.Data[i]].u32;    //lCharPair is 4 bytes.  Each pair is a unicode Character from the TwoHexLookup Table.
        inc(lP);
      end;
      fStringBuilder.Append(lStr);
      fStringBuilder.Append('"');
    end;

    cDataTypeGUID:
    begin
      fStringBuilder.Append('"');
      fStringBuilder.Append(aDataobj.AsString);
      fStringBuilder.Append('"');
    end;

    cDataTypeString:
    begin
      fStringBuilder.Append('"');
      WriteStringEncodingSpecialJSONCharacters(aDataobj.getStore^.fDataString);                                             //  new way.  a bit faster
      fStringBuilder.Append('"');
    end;

    cDataTypeStringList:
    begin
       if fStyle = cJsonHumanReadable then
         fStringBuilder.AppendLine('[')
       else
         fStringBuilder.Append('[');
      lSpaces := IncInd;
      lStringList := aDataobj.asStringList;
      for i := 0 to lStringList.Count-1 do
      begin
        fStringBuilder.Append(lSpaces);
        fStringBuilder.Append('"');
        WriteStringEncodingSpecialJSONCharacters(lStringList.Strings[i]);
        if i<lStringList.Count-1 then
        begin
          fStringBuilder.Append('",');
        end
        else
        begin
          fStringBuilder.Append('"');
        end;
        if fStyle = cJsonHumanReadable then
          fStringBuilder.AppendLine;
      end;
      lSpaces := DecInd;
      fStringBuilder.Append(lSpaces);
      fStringBuilder.Append(']');
    end;

    cDataTypeFrame:
    begin
      fStringBuilder.Append('{');
      lSpaces := IncInd;
      lFrame := aDataObj.AsFrame;
      for i := 0 to lFrame.Count-1 do
      begin
        if i>0 then
          fStringBuilder.Append(lSpaces);
        fStringBuilder.Append('"');
        WriteStringEncodingSpecialJSONCharacters(lFrame.slotName(i));
//        fStringBuilder.Append(lFrame.slotName(i));
        fStringBuilder.Append('":');
        ReadFromDataObjInternal(lFrame.Slots[i]);     // recursion happening here.
        if i<lFrame.Count-1 then
          fStringBuilder.Append(',');
        if (lFrame.Count>1) and (fStyle = cJsonHumanReadable) then
          fStringBuilder.AppendLine;
      end;
      lSpaces := DecInd;
      if lFrame.Count>1 then
        fStringBuilder.Append(lSpaces);
      fStringBuilder.Append('}');
    end;

    cDataTypeArray:
    begin
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
      if lArray.Count>0 then
        fStringBuilder.Append(lSpaces);
      fStringBuilder.Append(']');
    end;

    cDataTypeSparseArray:
    begin
      fStringBuilder.Append('[');
      lSpaces := IncInd;
      lSparseArray:=aDataObj.asSparseArray;
      for i := 0 to lSparseArray.Count-1 do
      begin
        if i>0 then
          fStringBuilder.Append(lSpaces);
        fStringBuilder.Append(IntToStr(lSparseArray.SlotIndex(i)));
        fStringBuilder.Append(':');
        ReadFromDataObjInternal(lSparseArray.items[i]);
        if i<lSparseArray.Count-1 then
          fStringBuilder.Append(',');
        if fStyle = cJsonHumanReadable then
          fStringBuilder.AppendLine;
      end;
      lSpaces := DecInd;
      fStringBuilder.Append(lSpaces);
      fStringBuilder.Append(']');
    end;

    cDataTypeBinary:
    begin
      fStringBuilder.Append(Base64ToJSONText(aDataObj.asBinary));
    end;

    cDataTypeObject:
    begin
      // FINISH - Objects are basically just serialized the same as a frame, so maybe we can generate the frame and then JSON that frame.
      lTempDDO:=TDataObj.Create;
      try
//        FINISH
//        lTempDDO.AsObject := AsObject;   // Hmmm, what about freeing.  Is this just a ref?
//        aStringBuilder.Append(lTempDDO.GetAsJSON);
      finally
        lTempDDO.Free;
      end;
    end;

    cDataTypeTag:
    begin
      // FINISH - found that other implementations have an option to put tags into JSON by containing the tag using a Json Object (frame) with a certain slotName naming convention.
      //          maybe we should support this concept too.
      // see https://github.com/intel/tinycbor/commit/782f2545a07e707464c6e9b417768e8b980c8e13

      // For now, we are skipping over the tagging portion and just streaming out the contained DataObject to JSON
      ReadFromDataObjInternal(aDataObj.AsTag.DataObj);
    end;

  end;
end;







// Read a dataObject from the stream that this TJsonStreamer is linked to.
procedure TJsonStreamer.ParseFromJson(aObj: TDataObj);
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
           1.  With TJsonStreamer, The code is written to expect that the entire source stream is going to be parsed into aDataObj.  This might not be the case in some people's implementations.
               I would rather read and parse the JSON from the stream and once a full JSON payload is loaded, leave the stream untouched and at the right position.  However,
               we are supporting the different possible text encodings (really Unicode, ASCII and UTF8) and that decoding happens before the JSON parsing actually begins.  I would
               need to be able to pull one "character" at a time from any type of encoding and that's just too much work for me to do right now.   Maybe delphi has a way to
               do that, maybe it doesn't.  Anyway... future issue.
           2.  The error handling isn't very good.  If there's malformed JSON fed in, the resulting object structure will be fine and it will get the data up to the point
               of encountering invalid JSON, but there aren't any good exception messages or line number/character number error messages telling someone at a higher level what
               went wrong and where.  Again, future improvement that should be done but it needs to not slow things down much.

         ALSO.  I've done quite a bit of testing with a handful of use-cases, but I don't feel like I have done exhaustive testing for this yet.  So, if anyone
         finds a JSON source use case that fails on this parser, please please send it to me and I'll fix it ASAP.  Or, if you fix the bugs on your own, please send me the code.
}



(* NOTE:  This is one of the types of JSON produced from MongoDB for ObjectID fields
  "user": {
    "$oid": "543d60369ed7fea94a0e6089"
  }

  "user": ObjectID("543d60369ed7fea94a0e6089")     is another variation.
*)


(* NOTE:  This is how binary is represented in JSON from MongoDB
  {
    "$binary": {
      "base64": "8RNdyah ... "
    }
  }
*)

begin
  if fJSON = '' then exit;

  // Setup our parsing context with the character pointers we will need.
  fCurrentCharPtr:=pchar(JSON);
  fEndPtr := @pchar(JSON)[length(JSON)];    // Address of the last character in the JSON string.

  if not ParseAnyType(aObj) then
  begin
    //Getting here means we were not able to parse the JSON completely.
    RaiseParsingException(cExceptErrorParsingJSON, fCurrentCharPtr);
  end;

  // NOTE that there could be more characters to be processed in fJSON.  The only way to know would be to look at the lContext.CurrentCharPtr and see where it is in relation to the EndPtr.
  // Do we need to do something if there are more chars available?
end;

class function TJsonStreamer.DataObjToJson(aDataObj: TDataObj; aStyle: TJSONStyle=cJsonTight; aIndent: byte = 2; aEncodeNonAsciiCharacters: boolean=false): string;      // finish - need to pass in the context properties.
var
  lContext: TJsonStreamer;
begin
  result := '';
  // Note that we are creating a TJsonStreamer and leaving it with the default serialization options which is cTightJSON and EncodeNonAsciiCharacters=false.
  lContext:=TJsonStreamer.Create(aStyle, aIndent);
  try
    lContext.EncodeNonAsciiCharacters := aEncodeNonAsciiCharacters;
    lContext.Encode(aDataObj);
    result := lContext.fJSON;
  finally
    lContext.Free;
  end;
end;

class procedure TJsonStreamer.JsonToDataObj(const aJson: string; aDataObj: TDataObj);
var
  lContext: TJsonStreamer;
begin
  lContext:=TJsonStreamer.Create;
  try
    lContext.JSON := aJson;
    lContext.Decode(aDataObj);
  finally
    lContext.Free;
  end;
end;

class function TJsonStreamer.Name: string;
begin
  result := 'JSON';
end;

(*function TJsonStreamer.GetClipboardFormat: word;
begin
  // for JSON, we can use it as the "Text" format.
  if self.Encoding.ClassType = TUnicodeEncoding then
  begin
    result := CF_UNICODETEXT;
  end
  else
  begin
    result := CF_TEXT;
  end;
end; *)

procedure TJsonStreamer.SetPreferencesByClipboardVersion(aClipboardVersion: integer);
begin
  case aClipboardVersion of
    0: begin
      self.fStyle := TJsonStyle.cJsonTight;
      SetEncoding(TEncoding.Unicode);
      ClipboardFormat := CF_UNICODETEXT;
    end;
    1: begin
      self.fStyle := TJsonStyle.cJsonTight;
      SetEncoding(TEncoding.ASCII);
      ClipboardFormat := CF_TEXT;
    end;
    2: begin
      self.fStyle := TJsonStyle.cJsonHumanReadable;
      SetEncoding(TEncoding.Unicode);
      ClipboardFormat := CF_UNICODETEXT;
    end;
    3: begin
      self.fStyle := TJsonStyle.cJsonHumanReadable;
      SetEncoding(TEncoding.ASCII);
      ClipboardFormat := CF_TEXT;
    end;
  end;
end;

class procedure TJsonStreamer.GetClipboardPublishingFormats(aCallback: TGetClipboardPublishingCallbackProc);
begin
  // we can call back with multiple versions on how we want to serialize to the clipboard.
  // Each version corresponds to the setup of a serializer in the SetPreferencesByClipboardVersion method
  aCallback(self, 'JSON - Tight Format, UNICODE', 0);
  aCallback(self, 'JSON - Tight Format, ASCII', 1);
  aCallback(self, 'JSON - Human Readable Format, UNICODE', 2);
  aCallback(self, 'JSON - Human Readable Format, ASCII', 3);
end;

class function TJsonStreamer.GetFileFilter: string;
begin
  result := 'JSON Files (*.json, *.txt)|*.json;*.txt';
end;

class procedure TJsonStreamer.GetParameterInfo(aParameterPurpose: TDataObjParameterPurposes; aStrings: TStrings);
begin
  if cppEncoding in aParameterPurpose then
  begin
    aStrings.AddPair('-Human', 'Will produce output with indentions and new lines for better human readability.');
    aStrings.AddPair('-Indent <number>', 'Defines how many characters to indent when using the -Human option. Default = 2');
    aStrings.AddPair('-Encoding <value>', 'Defines the text encoding of the output.  value=[UTF-8, UTF-7, ASCII, UNICODE, BIGENDIANUNICODE] default=UTF-8');
  end;
  if cppDecoding in aParameterPurpose then
  begin
    aStrings.AddPair('-AllowExtendedTypes', 'Allow decoding some non-json compliant extended types like BSON objectIds and IsoDates, etc.');
    aStrings.AddPair('-AllowSymbols', 'Allow decoding non-json compliant values into symbols such as strings without being enclosed in quotes.');
  end;
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
    // Parameters used for Writing out JSON
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
    end

    // Parameters used for logic in Reading JSON
    else if SameText(aParams[i], '-AllowExtendedTypes') then
    begin
      AllowParsingExtendedTypes := true;
    end
    else if SameText(aParams[i], '-AllowSymbols') then
    begin
      AllowParsingSymbols := true;
    end;

    inc(i);
  end;

end;

class function TJsonStreamer.Priority: cardinal;
begin
  result := 100;
end;

procedure TJsonStreamer.ClipboardEncode(aDataObj: TDataObj);
var
  lBytes: TBytes;
begin
  inherited;  // start with the default encoding
  // depending on the encoding, we are either writing a carriage return-line feed as 2 bytes or 4 bytes.
  lBytes := fEncoding.GetBytes(#13+#10);
  fStream.write(lBytes, length(lBytes));
end;

function TJsonStreamer.Clone: TDataObjStreamerBase;
begin
  result := inherited clone;
  TJsonStreamer(result).fStyle:=self.fStyle;
  TJsonStreamer(result).fIndention:=self.fIndention;
  TJsonStreamer(result).fIndent:=self.fIndent;
  TJsonStreamer(result).fEncodeNonAsciiCharacters:=self.fEncodeNonAsciiCharacters;
  TJsonStreamer(result).fEncoding:=self.fEncoding;
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

    lPreambleSize := TEncoding.GetBufferEncoding(lBytes, lEncoding, nil); // this call only chooses an encoding by inspecting the preamble.  If it can't find a preamble, then nil is returned in lEncoding.
    if lEncoding <> nil then
    begin
      if lPreambleSize=0 then   // If a preamble was not found, then we can assume the default was chosen.
      begin
        // Note that if the lBytes didn't have a preamble, the the GetBufferEncoding may choose the wrong one because it's just picking the system default.
        // We should do a little more checking and one little test we can do to see if we have full Unicode in the stream (not UTF-8 or UTF-7), is to think that most
        // likely we are starting out with a "{" or "[" character (or white space leading up to that character).  With or without whitespace, we "should" see
        // "{ #0" or "[ #0" or "<space> #0" or "<tab> #0", etc. as the first two bytes of a starting sequence if the incoming bytes are truely Unicode encoded.
        // so, as a simple check, we will inspect that second byte to see if it is in fact a #0.  If so, we will instantiate the UniCode Encoding.
        if ((lBytes[0] <> 0) and (lBytes[1] = 0)) then
          lEncoding := TEncoding.Unicode;
      end;
      SetEncoding(lEncoding);
    end;

    if fEncoding=nil then
      SetEncoding(TEncoding.Default);

    JSON := fEncoding.GetString(lBytes, lPreambleSize, lSize-lPreambleSize);
  end;

  ParseFromJSON(aDataObj);     // take the fJSON string and parse it into aDataObj.
end;


procedure TJsonStreamer.Encode(aDataObj: TDataObj);
var
  lBytes: TBytes;
begin
  if (fEncoding = fEncoding.ASCII) or (fEncoding = fEncoding.ANSI) then
  begin
    // If we have ascii or ansi encoding, then we MUST have all unicode characters above $7F be escaped.  Hmmm, Is it above $7F or above $FF?
    fEncodeNonAsciiCharacters := true;
  end;

  // First, get the aDataObj serialized into a JSON string (Full Unicode Delphi string)
  fStringBuilder:=TStringBuilder.Create(fInitialBufferCapacity);         // This will hold the produced json as it is being created.
  try
    ReadFromDataObjInternal(aDataObj);
    JSON := fStringBuilder.ToString(True);                   // This string now holds the JSON text as a delphi String (unicode string)
  finally
    FreeAndNil(fStringBuilder);
  end;

  if assigned(fStream) then
  begin
    // If we have an assigned stream, then move the string to a stream using the chosen text encoding.
    // If it's not assigned, then most likely the caller is just interested in getting the result by reading the fJSON string.
    if fIncludeEncodingPreamble then
    begin
      lBytes := fEncoding.GetPreamble;
      if length(lBytes)>0 then
        fStream.Write(lBytes[0], length(lBytes));
    end;

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

procedure TJsonStreamer.SetJSON(const Value: string);
begin
  fJSON := Value;
end;

initialization
  gJSONFormatSettings := TFormatSettings.Invariant;
  RegisterDataObjStreamer(TJsonStreamer);

end.
