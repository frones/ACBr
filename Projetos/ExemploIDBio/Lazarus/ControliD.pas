unit ControliD;

{$MODE Delphi}

interface

  {libcidbio.dll functions}
  Function CIDBIO_Init: Int32 StdCall; External 'libcidbio.dll';
  Function CIDBIO_Terminate: Int32 StdCall; External 'libcidbio.dll';

  Function CIDBIO_CaptureImage(Out imageBuf : PByte; Out width : Int32; Out height : Int32): Int32; cdecl;
    External 'libcidbio.dll';
  Function CIDBIO_CaptureImageAndTemplate(Out t : PAnsiChar; Out imageBuf : PByte; Out width : Int32;
    Out height : Int32; Out quality : Int32): Int32; cdecl; External 'libcidbio.dll';
  Function CIDBIO_CaptureAndEnroll(id : Int64): Int32; cdecl; External 'libcidbio.dll';
  Function CIDBIO_CaptureAndIdentify(Out id : Int64; Out score : Int32; Out quality : Int32): Int32; cdecl;
    External 'libcidbio.dll';
  Function CIDBIO_CaptureAndMatch(id : Int64; Out score : Int32; Out quality : Int32): Int32; cdecl;
    External 'libcidbio.dll';
  Function CIDBIO_CheckFingerprint(Out imageBuf : PByte; Out width : Int32; Out height : Int32): Int32; cdecl;
    External 'libcidbio.dll';
  Function CIDBIO_CancelCapture: Int32; cdecl;
    External 'libcidbio.dll';

  Function CIDBIO_ExtractTemplateFromImage(width : Int32; height : Int32; imageBuf : PByte; Out t : PAnsiChar;
    Out quality : Int32): Int32; cdecl; External 'libcidbio.dll';
  Function CIDBIO_MergeTemplates(t1 : PAnsiChar; t2 : PAnsiChar; t3 : PAnsiChar; Out tFinal : PAnsiChar): Int32;
    cdecl; External 'libcidbio.dll';
  Function CIDBIO_MatchTemplates(t1 : PAnsiChar; t2 : PAnsiChar; Out score : Int32): Int32; cdecl;
    External 'libcidbio.dll';
  Function CIDBIO_MatchTemplateByID(id : Int64; t : PAnsiChar; Out score : Int32): Int32; cdecl;
    External 'libcidbio.dll';

  Function CIDBIO_GetTemplateIDs(Out ids : PInt64; Out len : Int32): Int32; cdecl; External 'libcidbio.dll';
  Function CIDBIO_GetTemplate(id : Int64; Out t : PAnsiChar): Int32; cdecl; External 'libcidbio.dll';
  Function CIDBIO_SaveTemplate(id : Int64; t : PAnsiChar): Int32; cdecl; External 'libcidbio.dll';
  Function CIDBIO_DeleteTemplate(id : Int64): Int32; cdecl; External 'libcidbio.dll';
  Function CIDBIO_DeleteAllTemplates: Int32; cdecl; External 'libcidbio.dll';

  Function CIDBIO_SetParameter(config : Int32; value : PAnsiChar): Int32; cdecl; External 'libcidbio.dll';
  Function CIDBIO_GetParameter(config : Int32; Out value : PAnsiChar): Int32; cdecl; External 'libcidbio.dll';
  Function CIDBIO_GetDeviceInfo(Out version : PAnsiChar; Out serialNumber : PAnsiChar; Out model : PAnsiChar):
    Int32; cdecl; External 'libcidbio.dll';
  Function CIDBIO_UpdateFirmware(filePath : PAnsiChar): Int32; cdecl; External 'libcidbio.dll';

  Function CIDBIO_GetErrorMessage(error : Int32; Out msg : PAnsiChar): Int32; cdecl; External 'libcidbio.dll';

  Function CIDBIO_FreeByteArray(ptr : PByte): Int32; cdecl; External 'libcidbio.dll';
  Function CIDBIO_FreeString(ptr : PAnsiChar): Int32; cdecl; External 'libcidbio.dll';
  Function CIDBIO_FreeIDArray(ptr : PInt64): Int32; cdecl; External 'libcidbio.dll';

Type
  ConfigParam =
  (
    MIN_VAR = 1,
    SIMILIARITY_THRESHOLD = 2,
    BUZZER_ON = 4,
    TEMPLATE_FORMAT = 5,
    ROTATION = 6,
    DETECT_TIMEOUT = 7
  );

  RetCode =
  (
    WARNING_OVERWRITING_TEMPLATE = 3,
    WARNING_NO_IDS_ON_DEVICE = 2,
    WARNING_ALREADY_INIT = 1,
    SUCCESS = 0,
    ERROR_UNKNOWN = -1,
    ERROR_NO_DEVICE = -2,
    ERROR_NULL_ARGUMENT = -3,
    ERROR_INVALID_ARGUMENT = -4,
    ERROR_CAPTURE = -5,
    ERROR_CAPTURE_TIMEOUT = -6,
    ERROR_COMM_USB = -7,
    ERROR_IO_ON_HOST = -8,
    ERROR_TEMPLATE_ALREADY_ENROLLED = -9,
    ERROR_MERGING = -10,
    ERROR_MATCHING = -11,
    ERROR_INVALID_FW_FILE = -12,
    ERROR_NO_SPACE_LEFT_ON_DEVICE = -13,
    ERROR_NO_TEMPLATE_WITH_ID = -14,
    ERROR_INVALID_ERRNO = -15,
    ERROR_UNAVAILABLE_FEATURE = -16,
    ERROR_PREVIOUS_FW_VERSION = -17,
    ERROR_NOT_IDENTIFIED = -18,
    ERROR_BUSY = -19,
    ERROR_CAPTURE_CANCELED = -20,
    ERROR_NO_FINGER_DETECTED = -21
  );

  type
    ByteArray = array of byte;
    Int64Array = array of Int64;

  CIDBio = Class(TObject)
    Public
      Class Function Init: RetCode; Static;
      Class Function Terminate: RetCode; Static;

      Function CaptureImage(Out imageBuf : ByteArray; out width, height : Integer): RetCode;
      Function CaptureImageAndTemplate(Out t : String ; Out imageBuf : ByteArray;
        out width, height, quality : Integer): RetCode;
      Function CaptureAndEnroll(id : Int64): RetCode;
      Function CaptureAndIdentify(Out id : Int64; Out score, quality : Integer): RetCode;
      Function CaptureAndMatch(id : Int64; Out score, quality : Integer): RetCode;
      Function CheckFingerprint(Out imageBuf : ByteArray; out width, height : Integer): RetCode;
      Function CancelCapture: RetCode;

      Function ExtractTemplateFromImage(width, height : Integer; image : ByteArray;
        Out t : String; Out quality : Integer): RetCode;
      Function MergeTemplates(t1, t2, t3 : String; Out tFinal : String): RetCode;
      Function MatchTemplates(t1, t2 : String; Out score : Integer): RetCode;
      Function MatchTemplateByID(id : Int64; t : String; Out score : Integer): RetCode;

      Function GetTemplateIDs(Out ids : Int64Array): RetCode;
      Function GetTemplate(id : Int64; Out t : String): RetCode;
      Function SaveTemplate(id : Int64; t : String): RetCode;
      Function DeleteTemplate(id : Int64): RetCode;
      Function DeleteAllTemplates: RetCode;

      Function SetParameter(config : ConfigParam; value : String): RetCode;
      Function GetParameter(config : ConfigParam; Out value : String): RetCode;
      Function GetDeviceInfo(Out version, serialNumber, model : String): RetCode;
      Function UpdateFirmware(filePath : String): RetCode;

      Class Function GetErrorMessage(error : RetCode): String; Static;

    Private
      Class Function CopyAndDeleteString(ptr : PAnsiChar): String; Static;
      Class Function CopyAndDeleteBytes(ptr : PByte; length : Integer): ByteArray; Static;
      Class Function CopyAndDeleteIds(ptr : PInt64; length : Integer): Int64Array; Static;

  end;

implementation

  Class Function CIDBio.Init: RetCode;
  begin
    Result := RetCode(CIDBIO_Init);
  end;

  Class Function CIDBio.Terminate: RetCode;
  begin
    Result := RetCode(CIDBIO_Terminate);
  end;

  Function CIDBio.CaptureImage(Out imageBuf : ByteArray; out width, height : Integer): RetCode;
  var
    pImage : PByte;
    ret : RetCode;
  begin
    ret := RetCode(CIDBIO_CaptureImage(pImage, width, height));
    imageBuf := CopyAndDeleteBytes(pImage, width*height);
    Result := ret;
  end;

  Function CIDBio.CaptureImageAndTemplate(Out t : String ; Out imageBuf : ByteArray;
    out width, height, quality : Integer): RetCode;
  var
    pImage : PByte;
    pTemplate : PAnsiChar;
    ret : RetCode;
  begin
    ret := RetCode(CIDBIO_CaptureImageAndTemplate(pTemplate, pImage, width, height, quality));
    t := CopyAndDeleteString(pTemplate);
    imageBuf := CopyAndDeleteBytes(pImage, width*height);
    Result := ret;
  end;

  Function CIDBio.CaptureAndEnroll(id : Int64): RetCode;
  begin
    Result := RetCode(CIDBIO_CaptureAndEnroll(id));
  end;

  Function CIDBio.CaptureAndIdentify(Out id : Int64; Out score, quality : Integer) : RetCode;
  begin
    Result := RetCode(CIDBIO_CaptureAndIdentify(id, score, quality));
  end;

  Function CIDBio.CaptureAndMatch(id : Int64; Out score, quality : Integer) : RetCode;
  begin
    Result := RetCode(CIDBIO_CaptureAndMatch(id, score, quality));
  end;

  Function CIDBio.CheckFingerprint(Out imageBuf : ByteArray; out width, height : Integer): RetCode;
  var
    pImage : PByte;
    ret : RetCode;
  begin
    ret := RetCode(CIDBIO_CheckFingerprint(pImage, width, height));
    imageBuf := CopyAndDeleteBytes(pImage, width*height);
    Result := ret;
  end;

  Function CIDBio.CancelCapture: RetCode;
  begin
    Result := RetCode(CIDBIO_CancelCapture);
  end;

  Function CIDBio.ExtractTemplateFromImage(width, height : Integer; image : ByteArray;
        Out t : String; Out quality : Integer) : RetCode;
  var
    pTemplate : PAnsiChar;
    ret : RetCode;
  begin
    ret := RetCode(CIDBIO_ExtractTemplateFromImage(width, height, PByte(image), pTemplate, quality));
    t := CopyAndDeleteString(pTemplate);
    Result := ret;
  end;

  Function CIDBio.MergeTemplates(t1, t2, t3 : String; Out tFinal : String) : RetCode;
  var
    pTemplate : PAnsiChar;
    ret : RetCode;
  begin
    ret := RetCode(CIDBIO_MergeTemplates(PAnsiChar(AnsiString(t1)), PAnsiChar(AnsiString(t2)), PAnsiChar(AnsiString(t3)), pTemplate));
    tFinal := CopyAndDeleteString(pTemplate);
    Result := ret;
  end;

  Function CIDBio.MatchTemplates(t1, t2 : String; Out score : Integer): RetCode;
  begin
    Result := RetCode(CIDBIO_MatchTemplates(PAnsiChar(AnsiString(t1)), PAnsiChar(AnsiString(t2)), score));
  end;

  Function CIDBio.MatchTemplateByID(id : Int64; t : String; Out score : Integer): RetCode;
  begin
    Result := RetCode(CIDBIO_MatchTemplateByID(id, PAnsiChar(AnsiString(t)), score));
  end;

  Function CIDBio.GetTemplateIDs(Out ids : Int64Array): RetCode;
  var
    pIds : PInt64;
    len : Int32;
    ret : RetCode;
  begin
    ret := RetCode(CIDBIO_GetTemplateIDs(pIds, len));
    ids := CopyAndDeleteIds(pIds, len);
    Result := ret;
  end;

  Function CIDBio.GetTemplate(id : Int64; Out t : String): RetCode;
  var
    pTemplate : PAnsiChar;
    ret : RetCode;
  begin
    ret := RetCode(CIDBIO_GetTemplate(id, pTemplate));
    t := CopyAndDeleteString(pTemplate);
    Result := ret;
  end;

  Function CIDBio.SaveTemplate(id : Int64; t : String): RetCode;
  begin
    Result := RetCode(CIDBIO_SaveTemplate(id, PAnsiChar(AnsiString(t))));
  end;

  Function CIDBio.DeleteTemplate(id : Int64): RetCode;
  begin
    Result := RetCode(CIDBIO_DeleteTemplate(id));
  end;

  Function CIDBio.DeleteAllTemplates(): RetCode;
  begin
    Result := RetCode(CIDBIO_DeleteAllTemplates());
  end;

  Function CIDBio.SetParameter(config : ConfigParam; value : String): RetCode;
  begin
    Result := RetCode(CIDBIO_SetParameter(Ord(config), PAnsiChar(AnsiString(value))));
  end;

  Function CIDBio.GetParameter(config : ConfigParam; out value : String): RetCode;
  var
    pValue : PAnsiChar;
    ret : RetCode;
  begin
    ret := RetCode(CIDBIO_GetParameter(Ord(config), pValue));
    value := CopyAndDeleteString(pValue);
    Result := ret;
  end;

  Function CIDBio.GetDeviceInfo(out version, serialNumber, model : String): RetCode;
  var
    pVersion, pSerialNumber, pModel : PAnsiChar;
    ret : RetCode;
  begin
    ret := RetCode(CIDBIO_GetDeviceInfo(pVersion, pSerialNumber, pModel));
    version := CopyAndDeleteString(pVersion);
    serialNumber := CopyAndDeleteString(pSerialNumber);
    model := CopyAndDeleteString(pModel);
    Result := ret;
  end;

  Function CIDBio.UpdateFirmware(filePath : String): RetCode;
  begin
    Result := RetCode(CIDBIO_UpdateFirmware(PAnsiChar(AnsiString(filePath))));
  end;

  Class Function CIDBio.GetErrorMessage(error : RetCode): String;
  var
    pMessage: PAnsiChar;
    ret : RetCode;
  begin
    ret := RetCode(CIDBIO_GetErrorMessage(Ord(error), pMessage));
    if ret < RetCode.SUCCESS then
      Result := ''
    else
      Result := CopyAndDeleteString(pMessage)
    end;


  Class Function CIDBio.CopyAndDeleteString(ptr : PAnsiChar): String;
  var
    str : string;
  begin
    if ptr = Nil then
      Result := ''
    else begin
      str := Copy(String(AnsiString(ptr)), 1, MaxInt);
      CIDBIO_FreeString(ptr);
      Result := str;
    end;
  end;

  Class Function CIDBio.CopyAndDeleteBytes(ptr : PByte; length : Integer): ByteArray;
  var
    vec : ByteArray;
  begin
    if ptr = Nil then
      Result := nil
    else begin
      SetLength(vec, length);
      Move(ptr^, vec[0], SizeOf(Byte) * length);
      CIDBIO_FreeByteArray(ptr);
      Result := vec;
    end;
  end;

  Class Function CIDBio.CopyAndDeleteIds(ptr : PInt64; length : Integer): Int64Array;
  var
    vec : Int64Array;
  begin
    if ptr = Nil then
      Result := nil
    else begin
      SetLength(vec, length);
      Move(ptr^, vec[0], SizeOf(Int64) * length);
      CIDBIO_FreeIDArray(ptr);
      Result := vec;
    end;
  end;

end.
