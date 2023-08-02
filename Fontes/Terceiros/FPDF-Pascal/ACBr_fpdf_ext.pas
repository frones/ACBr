{

 FPDF Pascal Extensions
 https://github.com/Projeto-ACBr-Oficial/FPDF-Pascal

 Copyright (C) 2023 Projeto ACBr - Daniel Simões de Almeida

 Permission is hereby granted, free of charge, to any person obtaining a copy of
 this software and associated documentation files (the "Software"), to deal in
 the Software without restriction, including without limitation the rights to
 use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 of the Software, and to permit persons to whom the Software is furnished to do
 so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.

 Except as contained in this notice, the name of <Projeto ACBr> shall not be
 used in advertising or otherwise to promote the sale, use or other dealings in
 this Software without prior written authorization from <Projeto ACBr>.

 Based on:
 - The FPDF Scripts       http://www.fpdf.org/en/script/index.php
   TFPDFScriptCodeEAN     http://www.fpdf.org/en/script/script5.php  - Olivier
   TFPDFScriptCode39      http://www.fpdf.org/en/script/script46.php - The-eh
   TFPDFScriptCodeI25     http://www.fpdf.org/en/script/script67.php - Matthias Lau
   TFPDFScriptCode128     http://www.fpdf.org/en/script/script88.php - Roland Gautier
   TFPDFExt.Rotate        http://www.fpdf.org/en/script/script2.php  - Olivier
   TFPDFExt.RoundedRect   http://www.fpdf.org/en/script/script35.php - Christophe Prugnaud
   TFPDFExt.AddLayer      http://www.fpdf.org/en/script/script97.php - Oliver
   TFPDFExt.SetProtection http://www.fpdf.org/en/script/script37.php - Klemen Vodopivec

- Free JPDF Pascal from Jean Patrick e Gilson Nunes
   https://github.com/jepafi/Free-JPDF-Pascal
}

unit ACBr_fpdf_ext;

// Define USE_SYNAPSE if you want to force use of Units from synapse
//   Used from HTTPS downlaod em PDF Protection Script  (password)
//   http://www.ararat.cz/synapse
{$DEFINE USE_SYNAPSE}

// If you don't want the AnsiString vs String warnings to bother you
{$DEFINE REMOVE_CAST_WARN}

// If you have DelphiZXingQRCode Unit on you LibPath
// https://github.com/foxitsoftware/DelphiZXingQRCode
{$DEFINE DelphiZXingQRCode}

{$IfDef USE_SYNAPSE}
  {$DEFINE HAS_PROTECTION}
{$EndIf}

{$IfNDef FPC}
  {$IFDEF REMOVE_CAST_WARN}
   {$IF CompilerVersion >= 20}
    {$WARN IMPLICIT_STRING_CAST OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
   {$IfEnd}
  {$EndIf}
{$EndIf}

{$IfDef FPC}
  {$Mode objfpc}{$H+}
  {$Define USE_UTF8}
  {$Define HAS_HTTP}
{$EndIf}

{$IfDef USE_SYNAPSE}
  {$Define HAS_HTTP}
{$EndIf}

{$IfDef POSIX}
  {$IfDef LINUX}
    {$Define USE_UTF8}
  {$EndIf}
  {$Define FMX}
{$EndIf}

{$IfDef NEXTGEN}
  {$ZEROBASEDSTRINGS OFF}
  {$Define USE_UTF8}
{$EndIf}

interface

uses
  Classes, SysUtils,
  ACBr_fpdf
  {$IfDef DelphiZXingQRCode}
   ,ACBrDelphiZXingQRCode
  {$EndIf}
  {$IFDEF HAS_HTTP}
   {$IFDEF USE_SYNAPSE}
    ,httpsend, ssl_openssl, synacode
   {$ELSE}
    ,fphttpclient, opensslsockets
   {$ENDIF}
  {$ENDIF};

{$IfDef NEXTGEN}
type
  AnsiString = RawByteString;
  AnsiChar = UTF8Char;
  PAnsiChar = MarshaledAString;
  PPAnsiChar = ^MarshaledAString;
  WideString = String;
{$EndIf}

const
  cDefBarHeight = 8;
  cDefBarWidth = 0.5; //0.35;

type
  TByteArray = array of Byte;
  TFPDF2DMatrix = array of TByteArray;

  TFPDFTypePermissions = (canCopy, canPrint, canModify, canAannotForms);
  TFPDFPermissions = set of TFPDFTypePermissions;

const
    canAllPermissions = [canCopy, canPrint, canModify, canAannotForms];

type

  TFPDFExt = class;

  { TFPDFScripts }

  TFPDFScripts = class
  protected
    fpFPDF: TFPDFExt;
  public
    constructor Create(AFPDF: TFPDFExt); virtual;
  end;


  { TFPDFScriptCodeEAN }
  { http://www.fpdf.org/en/script/script5.php - Olivier }

  TFPDFScriptCodeEAN = class(TFPDFScripts)
  private
    procedure CheckBarCode(var ABarCode: string; BarCodeLen: integer;
      const BarCodeName: string);
    function AdjustBarCodeSize(const ABarCode: string; BarCodeLen: integer): string;
    function CalcBinCode(const ABarCode: string): String;
    procedure DrawBarcode(const BinCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    function GetCheckDigit(const ABarCode: string; const BarCodeName: string): integer;
    function TestCheckDigit(const ABarCode: string; const BarCodeName: string): boolean;
  public
    procedure CodeEAN13(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    procedure CodeEAN8(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
  end;

  { TFPDFScriptCode39 }
  { http://www.fpdf.org/en/script/script46.php - The-eh }

  TFPDFScriptCode39 = class(TFPDFScripts)
  private
  public
    procedure Code39(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
  end;

  { TFPDFScriptCodeI25 }
  { http://www.fpdf.org/en/script/script67.php - Matthias Lau }

  TFPDFScriptCodeI25 = class(TFPDFScripts)
  private
  public
    procedure CodeI25(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
  end;

  { TFPDFScriptCode128 }
  { http://www.fpdf.org/en/script/script88.php - Roland Gautier }

  TCode128 = (code128A, code128B, code128C);
  TFPDFScriptCode128 = class(TFPDFScripts)
  private
    fABCSet: String;                                  // C128 eligible character set
    fASet: String;                                    // Set A eligible Character
    fBSet: String;                                    // Set B eligible Character
    fCSet: String;                                    // Set C eligible character
    fSetTo: array [TCode128] of string;
    fSetFrom: array [TCode128] of string;
  public
    constructor Create(AFPDF: TFPDFExt); override;
    procedure Code128(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
  end;

  TFPDFLayer =  record
    Name: String;
    Visible: Boolean;
    n: Integer;
  end;

  TFPDFEvent = procedure (APDF: TFPDF) of object;

  { TFPDFExt }

  TFPDFExt = class(TFPDF)
  private
    fOnFooter: TFPDFEvent;
    fOnHeader: TFPDFEvent;
    fProxyHost: string;
    fProxyPass: string;
    fProxyPort: string;
    fProxyUser: string;

    fHREF: String;
    fFontStyle: String;

    {$IfDef HAS_HTTP}
     procedure GetImageFromURL(const aURL: string; const aResponse: TStream);
    {$EndIf}

    {$IfDef HAS_PROTECTION}
    function CreateUniqID: AnsiString;
    procedure _generateencryptionkey(const UserPass: AnsiString;
      const OwnerPass: AnsiString; protection: Integer);
    function _md5_16(AStr: AnsiString): AnsiString;
    function _objectkey(vn: Integer): AnsiString;
    function _Ovalue(const UserPass: AnsiString; const OwnerPass: AnsiString
      ): AnsiString;
    function _Uvalue: AnsiString;
    function RC4(const AKey: AnsiString; const AData: AnsiString): AnsiString;
    {$EndIf}

    function FindNextTagPos(const AHtml: String; out ATag: String; OffSet: Integer): Integer;
    procedure OpenTag(const ATag: String);
    procedure CloseTag(const ATag: String);
    procedure SetStyle(const ATag: String; Enable: Boolean);
    procedure PutLink(const AURL, AText: String);
  protected
    angle: Double;
    layers: array of TFPDFLayer;
    current_layer: Integer;
    open_layer_pane: Boolean;

    encrypted: Boolean;
    padding: String;
    encryption_key: AnsiString;
    Uvalue: AnsiString;             //U entry in pdf document
    Ovalue: AnsiString;             //O entry in pdf document
    Pvalue: Integer;                //P entry in pdf document
    enc_obj_id: Integer;            //encryption object id
    last_key: AnsiString;
    last_state: array[0..255] of Byte;


    procedure _endpage; override;
    procedure _putstream(const Adata: AnsiString); override;
    function _textstring(const AString: String): String; override;
    procedure _putencryption;

    procedure _putresourcedict; override;
    procedure _putresources; override;
    procedure _putlayers;
    procedure _putcatalog; override;
    procedure _puttrailer; override;
    procedure _enddoc; override;

    procedure _Arc(vX1, vY1, vX2, vY2, vX3, vY3: Double);
  public
    procedure Header; override;
    procedure Footer; override;

    procedure InternalCreate; override;

    procedure Rotate(NewAngle: Double = 0; vX: Double = -1; vY: Double = -1);

    procedure RoundedRect(vX, vY, vWidth, vHeight: Double;
      vRadius: Double = 5; vCorners: String = '1234'; vStyle: String = '');

    function AddLayer(const LayerName: String; IsVisible: Boolean = true): Integer;
    procedure BeginLayer(LayerId: Integer); overload;
    procedure BeginLayer(const LayerName: String); overload;
    procedure EndLayer;
    procedure OpenLayerPane;

    procedure WriteHTML(const AHtml: String);

    {$IfDef HAS_PROTECTION}
    procedure SetProtection(Permissions: TFPDFPermissions;
      const UserPass: AnsiString = ''; const OwnerPass: AnsiString = '');
    {$EndIf}

    {$IfDef HAS_HTTP}
    procedure Image(const vFileOrURL: string; vX: double = -9999;
      vY: double = -9999; vWidth: double = 0; vHeight: double = 0;
      const vLink: string = ''); overload; override;
    {$EndIf}

    procedure CodeEAN13(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    procedure CodeEAN8(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    procedure CodeI25(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    procedure Code39(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);
    procedure Code128(const ABarCode: string; vX: double; vY: double;
      BarHeight: double = 0; BarWidth: double = 0);

    {$IfDef DelphiZXingQRCode}
    function QRCode(vX: double; vY: double; const QRCodeData: String;
      DotSize: Double = 0; AEncoding: TQRCodeEncoding = qrAuto): double; overload;
    procedure QRCode(vX: double; vY: double; vQRCodeSize: double;
      const QRCodeData: String; AEncoding: TQRCodeEncoding = qrAuto); overload;
    {$EndIf}
    procedure Draw2DMatrix(AMatrix: TFPDF2DMatrix; vX: double; vY: double;
      DotSize: Double = 0);

    procedure SetDash(ABlack, AWhite: double); overload;
    procedure SetDash(AWidth: double); overload;
    procedure DashedLine(vX1, vY1, vX2, vY2: Double; ADashWidth: double = 1);
    procedure DashedRect(vX, vY, vWidht, vHeight: Double; const vStyle: String = ''; ADashWidth: double = 1);

    function WordWrap(var AText: string; AMaxWidth: Double; AIndent: double = 0): integer;
    function GetNumLines(const AText: string; AWidth: Double; AIndent: double = 0): integer;
    function GetStringHeight(const AText: string; AWidth: double;
      ALineSpacing: double = 0; AIndent: double = 0): double;

    function TextBox(vX, vY, vWidht, vHeight: double; const AText: string;
      const vAlign: char = 'T'; const hAlign: char = 'L';
      ABorder: boolean = True; AWordWrap: boolean = True;
      AScale: boolean = False; ALineSpacing: double = 0): double;

    property OnHeader: TFPDFEvent read fOnHeader write fOnHeader;
    property OnFooter: TFPDFEvent read fOnFooter write fOnFooter;

    property ProxyHost: string read fProxyHost write fProxyHost;
    property ProxyPort: string read fProxyPort write fProxyPort;
    property ProxyUser: string read fProxyUser write fProxyUser;
    property ProxyPass: string read fProxyPass write fProxyPass;
  end;


implementation

uses
  Math, StrUtils;


{ TFPDFScripts }

constructor TFPDFScripts.Create(AFPDF: TFPDFExt);
begin
  inherited Create;
  fpFPDF := AFPDF;
end;

{ TFPDFScriptCodeEAN }

procedure TFPDFScriptCodeEAN.CodeEAN13(const ABarCode: string; vX: double;
  vY: double; BarHeight: double; BarWidth: double);
var
  s, BinCode: string;
begin
  s := Trim(ABarCode);
  CheckBarCode(s, 13, 'EAN13');
  BinCode := CalcBinCode(s);
  DrawBarcode(BinCode, vx, vY, BarHeight, BarWidth);
end;

procedure TFPDFScriptCodeEAN.CodeEAN8(const ABarCode: string; vX: double;
  vY: double; BarHeight: double; BarWidth: double);
var
  s, BinCode: string;
begin
  s := Trim(ABarCode);
  CheckBarCode(s, 8, 'EAN8');
  BinCode := CalcBinCode(s);
  DrawBarcode(BinCode, vx, vY, BarHeight, BarWidth);
end;

procedure TFPDFScriptCodeEAN.CheckBarCode(var ABarCode: string;
  BarCodeLen: integer; const BarCodeName: string);
var
  l: integer;
begin
  ABarCode := Trim(ABarCode);
  l := Length(ABarCode);

  if (l < BarCodeLen - 1) then
    ABarCode := AdjustBarCodeSize(ABarCode, BarCodeLen - 1);

  if (l = BarCodeLen - 1) then
    ABarCode := ABarCode + IntToStr(GetCheckDigit(ABarCode, BarCodeName))
  else if (l = BarCodeLen) then
  begin
    if not TestCheckDigit(ABarCode, BarCodeName) then
      fpFPDF.Error(Format('Invalid %s Check Digit: %s', [BarCodeName, ABarCode]));
  end
  else
    fpFPDF.Error(Format('Invalid %s Code Len: %s', [BarCodeName, ABarCode]));
end;

function TFPDFScriptCodeEAN.AdjustBarCodeSize(const ABarCode: string;
  BarCodeLen: integer): string;
begin
  Result := Trim(copy(ABarCode, 1, BarCodeLen));
  if (BarCodeLen > Length(ABarCode)) then
    Result := StringOfChar('0', BarCodeLen - Length(Result)) + Result;
end;

function TFPDFScriptCodeEAN.GetCheckDigit(const ABarCode: string;
  const BarCodeName: string): integer;
var
  t, l, i: integer;
  v: integer;
begin
  //Compute the check digit
  t := 0;
  l := Length(ABarCode);
  for i := l downto 1 do
  begin
    v := StrToIntDef(ABarCode[i], -1);
    if (v < 0) then
      fpFPDF.Error(Format('Invalid Digits in %s: %s', [BarCodeName, ABarCode]));
    t := t + (v * IfThen(odd(i), 1, 3));
  end;

  Result := (10 - (t mod 10)) mod 10;
end;

function TFPDFScriptCodeEAN.TestCheckDigit(const ABarCode: string;
  const BarCodeName: string): boolean;
var
  l: integer;
begin
  l := Length(ABarCode);
  Result := (l > 0) and (StrToIntDef(ABarCode[l], -1) = GetCheckDigit(copy(ABarCode, 1, l-1), BarCodeName));
end;

function TFPDFScriptCodeEAN.CalcBinCode(const ABarCode: string): String;
type
 TParity = array[0..5] of Byte;
 
const
  codes: array[0..2] of array[0..9] of string =
    (('0001101', '0011001', '0010011', '0111101', '0100011', '0110001',
      '0101111', '0111011', '0110111', '0001011'),
     ('0100111', '0110011', '0011011', '0100001', '0011101', '0111001',
      '0000101', '0010001', '0001001', '0010111'),
     ('1110010', '1100110', '1101100', '1000010', '1011100', '1001110',
      '1010000', '1000100', '1001000', '1110100'));
  parities: array[0..9] of TParity =
    ((0, 0, 0, 0, 0, 0),
     (0, 0, 1, 0, 1, 1),
     (0, 0, 1, 1, 0, 1),
     (0, 0, 1, 1, 1, 0),
     (0, 1, 0, 0, 1, 1),
     (0, 1, 1, 0, 0, 1),
     (0, 1, 1, 1, 0, 0),
     (0, 1, 0, 1, 0, 1),
     (0, 1, 0, 1, 1, 0),
     (0, 1, 1, 0, 1, 0));
var
  v, i, l, p, m: integer;
  pa: TParity;

begin
  l := Length(ABarCode);
  Result := '101';
  if odd(l) then
  begin
    p := 1;
    v := StrToInt(ABarCode[1]);
    pa := parities[v];
  end
  else
  begin
    p := 0;
    pa := parities[0];
  end;

  m := (l - p) div 2;
  for i := p+1 to m+p do
  begin
    v := StrToInt(ABarCode[i]);
    Result := Result + codes[pa[i-1-p], v];
  end;

  Result := Result + '01010';

  for i := m+1+p to l do
  begin
    v := StrToInt(ABarCode[i]);
    Result := Result + codes[2, v];
  end;

  Result := Result + '101';
end;

procedure TFPDFScriptCodeEAN.DrawBarcode(const BinCode: string; vX: double;
  vY: double; BarHeight: double; BarWidth: double);
var
  l, i: Integer;
begin
  if (BarHeight = 0) then
    BarHeight := cDefBarHeight;

  if (BarWidth = 0) then
    BarWidth := cDefBarWidth;

  //Draw bars
  l := Length(BinCode);
  for i := 1 to l do
    if (BinCode[i] = '1') then
      fpFPDF.Rect(vX+i*BarWidth, vY, BarWidth, BarHeight, 'F');
end;

{ TFPDFScriptCode39 }

procedure TFPDFScriptCode39.Code39(const ABarCode: string; vX: double;
  vY: double; BarHeight: double; BarWidth: double);
type
  TBarSeq = array[0..8] of Byte;
const
  Chars: String = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. *$/+%';
  Bars: array[0..43] of TBarSeq =
    ( (0,0,0,1,1,0,1,0,0), (1,0,0,1,0,0,0,0,1), (0,0,1,1,0,0,0,0,1), (1,0,1,1,0,0,0,0,0),
      (0,0,0,1,1,0,0,0,1), (1,0,0,1,1,0,0,0,0), (0,0,1,1,1,0,0,0,0), (0,0,0,1,0,0,1,0,1),
      (1,0,0,1,0,0,1,0,0), (0,0,1,1,0,0,1,0,0), (1,0,0,0,0,1,0,0,1), (0,0,1,0,0,1,0,0,1),
      (1,0,1,0,0,1,0,0,0), (0,0,0,0,1,1,0,0,1), (1,0,0,0,1,1,0,0,0), (0,0,1,0,1,1,0,0,0),
      (0,0,0,0,0,1,1,0,1), (1,0,0,0,0,1,1,0,0), (0,0,1,0,0,1,1,0,0), (0,0,0,0,1,1,1,0,0),
      (1,0,0,0,0,0,0,1,1), (0,0,1,0,0,0,0,1,1), (1,0,1,0,0,0,0,1,0), (0,0,0,0,1,0,0,1,1),
      (1,0,0,0,1,0,0,1,0), (0,0,1,0,1,0,0,1,0), (0,0,0,0,0,0,1,1,1), (1,0,0,0,0,0,1,1,0),
      (0,0,1,0,0,0,1,1,0), (0,0,0,0,1,0,1,1,0), (1,1,0,0,0,0,0,0,1), (0,1,1,0,0,0,0,0,1),
      (1,1,1,0,0,0,0,0,0), (0,1,0,0,1,0,0,0,1), (1,1,0,0,1,0,0,0,0), (0,1,1,0,1,0,0,0,0),
      (0,1,0,0,0,0,1,0,1), (1,1,0,0,0,0,1,0,0), (0,1,1,0,0,0,1,0,0), (0,1,0,0,1,0,1,0,0),
      (0,1,0,1,0,1,0,0,0), (0,1,0,1,0,0,0,1,0), (0,1,0,0,0,1,0,1,0), (0,0,0,1,0,1,0,1,0) );
var
  wide, narrow, gap, lineWidth: Double;
  s: String;
  l, i, j, p: Integer;
  c: Char;
  seq: TBarSeq;
begin
  if (BarHeight = 0) then
    BarHeight := cDefBarHeight;

  if (BarWidth = 0) then
    BarWidth := cDefBarWidth;

  s := UpperCase(ABarCode);
  l := Length(s);
  if (l = 0) then
    Exit;

  wide := BarWidth;
  narrow := wide / 3 ;
  gap := narrow;

  if (s[l] <> '*') then
    s := s+'*';
  if (s[1] <> '*') then
    s := '*'+s;

  l := Length(s);
  for i := 1 to l do
  begin
    c := s[i];
    p := pos(c, Chars);
    if (p = 0) then
      fpFPDF.Error('Code39, invalid Char: '+c);

    seq := Bars[p-1];
    for j := 0 to High(seq) do
    begin
      lineWidth := IfThen(seq[j] = 0, narrow, wide);
      if ((j mod 2) = 0) then
        fpFPDF.Rect(vX, vY, lineWidth, BarHeight, 'F');

      vX := vX + lineWidth;
    end;

    vX := vX + gap;
  end;
end;

{ TFPDFScriptCodeI25 }

procedure TFPDFScriptCodeI25.CodeI25(const ABarCode: string; vX: double;
  vY: double; BarHeight: double; BarWidth: double);
const
  Chars: String = '0123456789AZ';
  Bars: array[1..12] of String =
    (  'nnwwn', 'wnnnw', 'nwnnw', 'wwnnn', 'nnwnw', 'wnwnn', 'nwwnn',
       'nnnww', 'wnnwn', 'nwnwn', 'nn', 'wn');
var
  wide, narrow, lineWidth: Double;
  s, seq: String;
  l, i, j, pcb, pcs: Integer;
  charBar, charSpace: Char;
begin
  s := LowerCase(trim(ABarCode));
  if (s = '') then
    Exit;

  if (BarHeight = 0) then
    BarHeight := cDefBarHeight;

  if (BarWidth = 0) then
    BarWidth := cDefBarWidth;

  wide := BarWidth;
  narrow := wide / 3;

  // add leading zero if code-length is odd
  l := Length(s);
  if ((l mod 2) <> 0) then
    s := '0' + s;

  // add start and stop codes
  s := 'AA' + s + 'ZA';
  l := Length(s);

  i := 1;
  while (i <= l) do
  begin
    // choose next pair of digits
    charBar := s[i];
    charSpace := s[i+1];

    // check whether it is a valid digit
    pcb := pos(charBar, Chars);
    if (pcb = 0) then
      fpFPDF.Error('CodeI25, Invalid character: '+charBar);

    pcs := pos(charSpace, Chars);
    if (pcs = 0) then
      fpFPDF.Error('CodeI25, Invalid character: '+charSpace);

    // create a wide/narrow-sequence (first digit=bars, second digit=spaces)
    seq := '';
    for j := 1 to Length(Bars[pcb]) do
      seq := seq + Bars[pcb][j] + Bars[pcs][j];

    for j := 1 to Length(seq) do
    begin
      // set lineWidth depending on value
      lineWidth := IfThen(seq[j] = 'n', narrow, wide);

      // draw every second value, because the second digit of the pair is represented by the spaces
      if (((j-1) mod 2) = 0) then
        fpFPDF.Rect(vX, vY, lineWidth, BarHeight, 'F');

      vX :=  vX + lineWidth;
    end;

    Inc(i, 2);
  end;
end;

{ TFPDFScriptCode128 }

constructor TFPDFScriptCode128.Create(AFPDF: TFPDFExt);
var
  i: Integer;
  set128: TCode128;
begin
  inherited Create(AFPDF);

  fABCSet := '';
  for i := 32 to 95 do                     // character sets
    fABCSet := fABCSet + chr(i);

  fASet := fABCSet;
  fBSet := fABCSet;

  for i := 0 to 31 do
  begin
    fABCSet := fABCSet + chr(i);
    fASet := fASet + chr(i);
  end;

  for i := 96 to 127 do
  begin
   fABCSet := fABCSet + chr(i);
   fBSet := fBSet + chr(i);
  end;

  for i := 200 to 210 do                   // control 128
  begin
    fABCSet := fABCSet + chr(i);
    fASet := fASet + chr(i);
    fBSet := fBSet + chr(i);
  end;

  fCSet := '0123456789'+chr(206);

  for set128 := Low(TCode128) to High(TCode128) do
  begin
    fSetFrom[set128] := '';
    fSetTo[set128] := '';
  end;

  for i := 0 to 95 do                      // converters for sets A & B
  begin
    fSetFrom[code128A] := fSetFrom[code128A] + chr(i);
    fSetFrom[code128B] := fSetFrom[code128B] + chr(i + 32);
    fSetTo[code128A] := fSetTo[code128A] + chr( IfThen(i < 32, i+64, i-32) );
    fSetTo[code128B] := fSetTo[code128B] + chr(i);
  end;

  for i := 96 to 106 do                    // control of sets A & B
  begin
    fSetFrom[code128A] := fSetFrom[code128A] + chr(i + 104);
    fSetFrom[code128B] := fSetFrom[code128B] + chr(i + 104);
    fSetTo[code128A] := fSetTo[code128A] + chr(i);
    fSetTo[code128B] := fSetTo[code128B] + chr(i);
  end;
end;

procedure TFPDFScriptCode128.Code128(const ABarCode: string; vX: double;
  vY: double; BarHeight: double; BarWidth: double);
type
  T128Parity = array [0..5] of Byte;
const
  C128Start: array[TCode128] of Byte = (103, 104, 105);  // Set selection characters at the start of C128
  C128Swap: array[TCode128] of Byte  = (101, 100, 99);   // Set change characters

  C128Table: array [0..107] of T128Parity =             // Code table 128
       (
         (2, 1, 2, 2, 2, 2),           //0 : [ ]
         (2, 2, 2, 1, 2, 2),           //1 : [!]
         (2, 2, 2, 2, 2, 1),           //2 : ["]
         (1, 2, 1, 2, 2, 3),           //3 : [#]
         (1, 2, 1, 3, 2, 2),           //4 : [$]
         (1, 3, 1, 2, 2, 2),           //5 : [%]
         (1, 2, 2, 2, 1, 3),           //6 : [&]
         (1, 2, 2, 3, 1, 2),           //7 : [']
         (1, 3, 2, 2, 1, 2),           //8 : [(]
         (2, 2, 1, 2, 1, 3),           //9 : [)]
         (2, 2, 1, 3, 1, 2),           //10 : [*]
         (2, 3, 1, 2, 1, 2),           //11 : [+]
         (1, 1, 2, 2, 3, 2),           //12 : [,]
         (1, 2, 2, 1, 3, 2),           //13 : [-]
         (1, 2, 2, 2, 3, 1),           //14 : [.]
         (1, 1, 3, 2, 2, 2),           //15 : [/]
         (1, 2, 3, 1, 2, 2),           //16 : [0]
         (1, 2, 3, 2, 2, 1),           //17 : [1]
         (2, 2, 3, 2, 1, 1),           //18 : [2]
         (2, 2, 1, 1, 3, 2),           //19 : [3]
         (2, 2, 1, 2, 3, 1),           //20 : [4]
         (2, 1, 3, 2, 1, 2),           //21 : [5]
         (2, 2, 3, 1, 1, 2),           //22 : [6]
         (3, 1, 2, 1, 3, 1),           //23 : [7]
         (3, 1, 1, 2, 2, 2),           //24 : [8]
         (3, 2, 1, 1, 2, 2),           //25 : [9]
         (3, 2, 1, 2, 2, 1),           //26 : [:]
         (3, 1, 2, 2, 1, 2),           //27 : [,]
         (3, 2, 2, 1, 1, 2),           //28 : [<]
         (3, 2, 2, 2, 1, 1),           //29 : [=]
         (2, 1, 2, 1, 2, 3),           //30 : [>]
         (2, 1, 2, 3, 2, 1),           //31 : [?]
         (2, 3, 2, 1, 2, 1),           //32 : [@]
         (1, 1, 1, 3, 2, 3),           //33 : [A]
         (1, 3, 1, 1, 2, 3),           //34 : [B]
         (1, 3, 1, 3, 2, 1),           //35 : [C]
         (1, 1, 2, 3, 1, 3),           //36 : [D]
         (1, 3, 2, 1, 1, 3),           //37 : [E]
         (1, 3, 2, 3, 1, 1),           //38 : [F]
         (2, 1, 1, 3, 1, 3),           //39 : [G]
         (2, 3, 1, 1, 1, 3),           //40 : [H]
         (2, 3, 1, 3, 1, 1),           //41 : [I]
         (1, 1, 2, 1, 3, 3),           //42 : [J]
         (1, 1, 2, 3, 3, 1),           //43 : [K]
         (1, 3, 2, 1, 3, 1),           //44 : [L]
         (1, 1, 3, 1, 2, 3),           //45 : [M]
         (1, 1, 3, 3, 2, 1),           //46 : [N]
         (1, 3, 3, 1, 2, 1),           //47 : [O]
         (3, 1, 3, 1, 2, 1),           //48 : [P]
         (2, 1, 1, 3, 3, 1),           //49 : [Q]
         (2, 3, 1, 1, 3, 1),           //50 : [R]
         (2, 1, 3, 1, 1, 3),           //51 : [S]
         (2, 1, 3, 3, 1, 1),           //52 : [T]
         (2, 1, 3, 1, 3, 1),           //53 : [U]
         (3, 1, 1, 1, 2, 3),           //54 : [V]
         (3, 1, 1, 3, 2, 1),           //55 : [W]
         (3, 3, 1, 1, 2, 1),           //56 : [X]
         (3, 1, 2, 1, 1, 3),           //57 : [Y]
         (3, 1, 2, 3, 1, 1),           //58 : [Z]
         (3, 3, 2, 1, 1, 1),           //59 : [[]
         (3, 1, 4, 1, 1, 1),           //60 : [\]
         (2, 2, 1, 4, 1, 1),           //61 : []]
         (4, 3, 1, 1, 1, 1),           //62 : [^]
         (1, 1, 1, 2, 2, 4),           //63 : [_]
         (1, 1, 1, 4, 2, 2),           //64 : [`]
         (1, 2, 1, 1, 2, 4),           //65 : [a]
         (1, 2, 1, 4, 2, 1),           //66 : [b]
         (1, 4, 1, 1, 2, 2),           //67 : [c]
         (1, 4, 1, 2, 2, 1),           //68 : [d]
         (1, 1, 2, 2, 1, 4),           //69 : [e]
         (1, 1, 2, 4, 1, 2),           //70 : [f]
         (1, 2, 2, 1, 1, 4),           //71 : [g]
         (1, 2, 2, 4, 1, 1),           //72 : [h]
         (1, 4, 2, 1, 1, 2),           //73 : [i]
         (1, 4, 2, 2, 1, 1),           //74 : [j]
         (2, 4, 1, 2, 1, 1),           //75 : [k]
         (2, 2, 1, 1, 1, 4),           //76 : [l]
         (4, 1, 3, 1, 1, 1),           //77 : [m]
         (2, 4, 1, 1, 1, 2),           //78 : [n]
         (1, 3, 4, 1, 1, 1),           //79 : [o]
         (1, 1, 1, 2, 4, 2),           //80 : [p]
         (1, 2, 1, 1, 4, 2),           //81 : [q]
         (1, 2, 1, 2, 4, 1),           //82 : [r]
         (1, 1, 4, 2, 1, 2),           //83 : [s]
         (1, 2, 4, 1, 1, 2),           //84 : [t]
         (1, 2, 4, 2, 1, 1),           //85 : [u]
         (4, 1, 1, 2, 1, 2),           //86 : [v]
         (4, 2, 1, 1, 1, 2),           //87 : [w]
         (4, 2, 1, 2, 1, 1),           //88 : [x]
         (2, 1, 2, 1, 4, 1),           //89 : [y]
         (2, 1, 4, 1, 2, 1),           //90 : [z]
         (4, 1, 2, 1, 2, 1),           //91 : [{]
         (1, 1, 1, 1, 4, 3),           //92 : [|]
         (1, 1, 1, 3, 4, 1),           //93 : [}]
         (1, 3, 1, 1, 4, 1),           //94 : [~]
         (1, 1, 4, 1, 1, 3),           //95 : [DEL]
         (1, 1, 4, 3, 1, 1),           //96 : [FNC3]
         (4, 1, 1, 1, 1, 3),           //97 : [FNC2]
         (4, 1, 1, 3, 1, 1),           //98 : [SHIFT]
         (1, 1, 3, 1, 4, 1),           //99 : [Cswap]
         (1, 1, 4, 1, 3, 1),           //100 : [Bswap]
         (3, 1, 1, 1, 4, 1),           //101 : [Aswap]
         (4, 1, 1, 1, 3, 1),           //102 : [FNC1]
         (2, 1, 1, 4, 1, 2),           //103 : [Astart]
         (2, 1, 1, 2, 1, 4),           //104 : [Bstart]
         (2, 1, 1, 2, 3, 2),           //105 : [Cstart]
         (2, 3, 3, 1, 1, 1),           //106 : [STOP]
         (2, 1, 0, 0, 0, 0)            //107 : [END BAR]
       );
var
  s, Aguid, Bguid, Cguid, needle, SminiC: String;
  crypt, cryptb: AnsiString;
  l, i, j, p, IminiC, made, madeA, madeB, check: Integer;
  set128: TCode128;
  modul: Double;
  c: T128Parity;
begin
  s := ABarCode;
  if (s = '') then
    Exit;

  if (BarHeight = 0) then
    BarHeight := cDefBarHeight;

  if (BarWidth = 0) then
    BarWidth := cDefBarWidth;

  if (BarWidth < 1) then
    BarWidth := BarWidth * 100;

  Aguid := '';                                                                      // Creation of ABC choice guides
  Bguid := '';
  Cguid := '';
  l := Length(s);
  for i := 1 to l do
  begin
    needle := copy(s, i, 1);
    Aguid := Aguid + ifthen(pos(needle, fASet) > 0, 'O', 'N');
    Bguid := Bguid + ifthen(pos(needle, fBSet) > 0, 'O', 'N');
    Cguid := Cguid + ifthen(pos(needle, fCSet) > 0, 'O', 'N');
  end;

  SminiC := 'OOOO';
  IminiC := 4;
  crypt := '';
  while (s <> '') do                                                           // MAIN CODING LOOP
  begin
    p := pos(SminiC, Cguid);                                                    // forcing of set C, if possible
    if (p > 0) then
    begin
      Aguid[p] := 'N';
      Bguid[p] := 'N';
    end;

     if (copy(Cguid, 1, IminiC) = SminiC) then                                  // set C
     begin
       crypt := crypt + chr( IfThen(crypt <> '', C128Swap[code128C],  C128Start[code128C]) );  // start Cstart, otherwise Cswap
       made := pos('N', Cguid);                                                 // extended set C
       if (made = 0) then
         made := Length(Cguid)
       else
         Dec(made);

       if ((made mod 2) = 1) then
         Dec(made);                                                             // only an even number

       i := 1;
       while (i <= made) do
       begin
         crypt := crypt + chr(StrToInt(Copy(s, i, 2)));                         // 2 by 2 conversion
         Inc(i, 2);
       end;
     end
     else
     begin
       madeA := pos('N', Aguid);                                                // set A range
       if (madeA = 0) then
         madeA := Length(Aguid)
       else
         Dec(madeA);

       madeB := pos('N', Bguid);                                                // set B range
       if (madeB = 0 ) then
         madeB := Length(Bguid)
       else
         Dec(madeB);

       if (madeA < madeB) then                                                  // treated area and Set in progress
       begin
         made := madeB;
         set128 := code128B;
       end
       else
       begin
         made := madeA;
         set128 := code128A;
       end;

       crypt := crypt + chr(IfThen(crypt <> '', C128Swap[set128], C128Start[set128])); // start, otherwise swap
       cryptb := Copy(s, 1, made);
       for j := 1 to Length(fSetFrom[set128]) do
          cryptb := StringReplace(cryptb, fSetFrom[set128][j], fSetTo[set128][j], [rfReplaceAll]); // conversion according to Set
       crypt := crypt + cryptb;
     end;

     s := Copy(s, made+1, l);                                                   // shorten legend and guides of the treated area
     Aguid := Copy(Aguid, made+1, Length(Aguid));
     Bguid := Copy(Bguid, made+1, Length(Bguid));
     Cguid := Copy(Cguid, made+1, Length(Cguid));
  end;                                                                          // END OF MAIN LOOP

  check := ord(crypt[1]);                                                       // calculation of the checksum
  l := Length(crypt);
  for i := 1 to l do
    check := check + (ord(crypt[i]) * (i-1));
  check := check mod 103;

  crypt := crypt + chr(check) + chr(106) + chr(107);                            // Complete encrypted channel
  l := Length(crypt);
  i := (l * 11) - 8;                                                            // calculation of the module width
  modul := BarWidth/i;

  for i := 1 to l do                                                            // PRINTING LOOP
  begin
    c := C128Table[ord(crypt[i])];
    j := 0;
    while (j <= 5) do
    begin
      if (c[j] > 0) then
      begin
        fpFPDF.Rect(vX, vY, c[j] * modul, BarHeight, 'F');
        Inc(j);
        vX := vX + (c[j-1]+c[j]) * modul;
      end;

      Inc(j);
    end;
  end;
end;


{ TFPDFExt }

procedure TFPDFExt.InternalCreate;
begin
  inherited;
  fProxyHost := '';
  fProxyPass := '';
  fProxyPort := '';
  fProxyUser := '';
  fOnFooter := Nil;
  fOnHeader := Nil;

  angle := 0;
  SetLength(layers,0);
  current_layer := -1;
  open_layer_pane := False;

  encrypted := false;
  padding := #$28#$BF#$4E#$5E#$4E#$75#$8A#$41#$64#$00#$4E#$56#$FF#$FA#$01#$08+
             #$2E#$2E#$00#$B6#$D0#$68#$3E#$80#$2F#$0C#$A9#$FE#$64#$53#$69#$7A;
  encryption_key := '';
  Uvalue := '';
  Ovalue := '';
  Pvalue := 0;
  enc_obj_id := 0;
  last_key := '';
  FillChar(last_state, 256, 0);
end;

{ http://www.fpdf.org/en/script/script2.php - Olivier }
{ See also: http://www.fpdf.org/en/script/script9.php - Watermark }
procedure TFPDFExt.Rotate(NewAngle: Double; vX: Double; vY: Double);
var
  c, s: Extended;
  cx, cy: Double;
begin
  if (vX=-1) then
    vX := Self.x;

  if (vY=-1) then
    vY := Self.y;

  if (Self.angle <> 0) then
    _out('Q');

  Self.angle := NewAngle;
  if (NewAngle <> 0) then
  begin
    NewAngle := NewAngle * Pi/180;
    c := cos(NewAngle);
    s := sin(NewAngle);
    cx := vX * Self.k;
    cy := (Self.h-vY) * Self.k;
    _out(Format('q %.5f %.5f %.5f %.5f %.2f %.2f cm 1 0 0 1 %.2f %.2f cm', [c, s, -s, c, cx, cy, -cx, -cy], FPDFFormatSetings));
  end;
end;

{ http://www.fpdf.org/en/script/script35.php - Christophe Prugnaud }
procedure TFPDFExt.RoundedRect(vX, vY, vWidth, vHeight: Double;
  vRadius: Double = 5; vCorners: String = '1234'; vStyle: String = '');
var
  vK, hp, xc, yc: Double;
  op: Char;
  MyArc: Extended;
begin
  vK := Self.k;
  hp := Self.h;
  if (vStyle='F') then
    op := 'f'
  else if (vStyle='FD') or (vStyle='DF') then
    op := 'B'
  else
    op := 'S';

  MyArc := 4/3 * (sqrt(2) - 1);
  _out(Format('%.2f %.2f m', [(vX+vRadius)*vk, (hp-vY)*k], FPDFFormatSetings));

  xc := vx+vWidth-vRadius;
  yc := vY+vRadius;
  _out(Format('%.2f %.2f l', [xc*vK, (hp-vY)*vK], FPDFFormatSetings));
  if (pos('2', vCorners) = 0) then
    _out(Format('%.2f %.2f l', [(vX+vWidth)*vK,(hp-vY)*vK], FPDFFormatSetings))
  else
    _Arc(xc+vRadius*MyArc, yc-vRadius, xc+vRadius, yc-vRadius*MyArc, xc+vRadius, yc);

  xc := vX+vWidth-vRadius;
  yc := vY+vHeight-vRadius;
  _out(Format('%.2f %.2f l', [(vX+vWidth)*vK,(hp-yc)*vK], FPDFFormatSetings));
  if (pos('3', vCorners) = 0) then
    _out(Format('%.2f %.2f l', [(vX+vWidth)*vk,(hp-(vY+vHeight))*vK], FPDFFormatSetings))
  else
    _Arc(xc+vRadius, yc+vRadius*MyArc, xc+vRadius*MyArc, yc+vRadius, xc, yc+vRadius);

  xc := vX+vRadius;
  yc := vY+vHeight-vRadius;
  _out(Format('%.2f %.2f l', [xc*vK,(hp-(vY+vHeight))*vK] , FPDFFormatSetings));
  if (pos('4', vCorners) = 0) then
    _out(Format('%.2f %.2f l', [(vX)*vK,(hp-(vY+vHeight))*vK], FPDFFormatSetings))
  else
    _Arc(xc-vRadius*MyArc, yc+vRadius, xc-vRadius, yc+vRadius*MyArc, xc-vRadius, yc);

  xc := vx+vRadius;
  yc := vY+vRadius;
  _out(Format('%.2f %.2f l', [(vX)*vK, (hp-yc)*vK], FPDFFormatSetings));
  if (pos('1', vCorners) = 0) then
  begin
    _out(Format('%.2f %.2f l', [(vX)*vK, (hp-vY)*vK], FPDFFormatSetings));
    _out(Format('%.2f %.2f l', [(vx+vRadius)*vK,(hp-vY)*vK], FPDFFormatSetings));
  end
  else
    _Arc(xc-vRadius, yc-vRadius*MyArc, xc-vRadius*MyArc, yc-vRadius, xc, yc-vRadius);

  _out(op);
end;

procedure TFPDFExt._Arc(vX1, vY1, vX2, vY2, vX3, vY3: Double);
var
  vh: Double;
begin
  vh := Self.h;
  _out(Format('%.2f %.2f %.2f %.2f %.2f %.2f c ',
          [vX1*Self.k, (vh-vY1)*Self.k, vX2*Self.k, (vh-vY2)*Self.k, vX3*Self.k, (vh-vY3)*Self.k],
          FPDFFormatSetings));
end;


{ http://www.fpdf.org/en/script/script97.php - Oliver }
function TFPDFExt.AddLayer(const LayerName: String; IsVisible: Boolean = true): Integer;
var
  s: String;
  id: Integer;
begin
  s := ConvertTextToAnsi(LayerName);
  id := Length(layers);
  SetLength(layers, id+1);
  layers[id].Name := LayerName;
  layers[id].Visible := IsVisible;
  layers[id].n := -1;
  Result := id;
end;

procedure TFPDFExt.BeginLayer(LayerId: Integer);
begin
  if (LayerId >= Length(Self.layers)) then
    Error('Invalid Layer Id: '+IntToStr(LayerId));

  EndLayer();
  _out('/OC /OC'+IntToStr(LayerId)+' BDC');
  Self.current_layer := LayerId;
end;

procedure TFPDFExt.BeginLayer(const LayerName: String);
var
  i, Id: Integer;
begin
  Id := -1;
  for i := 0 to Length(Self.layers)-1 do
  begin
    if (LayerName = Self.layers[i].Name) then
    begin
      Id := i;
      Break;
    end;
  end;

  if (Id < 0) then
    Error('Layer Name not found: '+LayerName);

  BeginLayer(Id);
end;

procedure TFPDFExt.EndLayer();
begin
  if (Self.current_layer >= 0) then
  begin
    _out('EMC');
    Self.current_layer := -1;
  end;
end;

procedure TFPDFExt.OpenLayerPane();
begin
  Self.open_layer_pane := true;
end;

function TFPDFExt.WordWrap(var AText: string; AMaxWidth,
  AIndent: double): integer;
{ http://www.fpdf.org/en/script/script49.php - Ron Korving }
var
  Space, Width, WordWidth: Double;
  Lines, Words: TStringArray;
  ALine, Word: string;
  i, j, L: integer;
begin
  AText := Trim(AText);
  Result := 0;
  if AText = '' then
    Exit;
  
  Space := Self.GetStringWidth(' ');
  Lines := Split(AText, sLineBreak);
  AText := '';
  AMaxWidth := AMaxWidth - AIndent;
  Result := 0;
  for i := 0 to Length(Lines) - 1 do
  begin
    ALine := Lines[i];
    Words := Split(ALine, ' ');
    Width := 0;
    for j := 0 to Length(Words) - 1 do
    begin
      Word := Words[j];
      if Trim(Word) = '' then
        Continue;
      WordWidth := Self.GetStringWidth(Word);
      if WordWidth > AMaxWidth then
      begin
        // Word is too long, we cut it
        for L := 1 to Length(Word) do
        begin
          WordWidth := Self.GetStringWidth(Copy(Word, L, 1));
          if (Width + WordWidth <= AMaxWidth) then
          begin
            Width := Width + WordWidth;
            AText := AText + Copy(Word, L, 1);
          end
          else
          begin
            Width := WordWidth;
            AText := TrimRight(AText) + sLineBreak + Copy(Word, L, 1);
            Inc(Result);
            AMaxWidth := AMaxWidth + AIndent;
          end;
        end;
      end
      else
        if (Width + WordWidth <= AMaxWidth) then
        begin
          Width := Width + WordWidth + Space;
          AText := AText + Word + ' ';
        end
        else
        begin
          Width := WordWidth + Space;
          AText := TrimRight(AText) + sLineBreak + Word + ' ';
          Inc(Result);
          AMaxWidth := AMaxWidth + AIndent;
        end;
    end;
    AText := TrimRight(AText) + sLineBreak;
    Inc(Result);
    AMaxWidth := AMaxWidth + AIndent;
  end;
  AText := TrimRight(AText);
end;

procedure TFPDFExt.WriteHTML(const AHtml: String);
var
  s, ATag, AText: String;
  l, lt, p1, p2: Integer;
begin
  // HTML parser
  s := StringReplace(AHtml, #13+#10, ' ', [rfReplaceAll]);
  s := StringReplace(s, #10, ' ', [rfReplaceAll]);
  l := Length(s);
  p1 := 1;
  while p1 <= l do
  begin
    p2 := FindNextTagPos(AHtml, ATag, p1);
    if (p2 = 0) then
      p2 := l+1;

    AText := copy(AHtml, p1, p2-p1);
    if (AText <> '') then
    begin
      if (Self.fHREF <> '') then
        PutLink(Self.fHREF, AText)
      else
        Write(Self.FontSize + 0.5, AText);
    end;

    if (ATag <> '') then
    begin
      lt := Length(ATag);
      if ATag[1] = '/' then
      begin
        Delete(ATag, 1, 1);
        CloseTag(ATag);
      end
      else
        OpenTag(ATag);

      Inc(p2, lt+2);
    end;

    p1 := p2;
  end;
end;

{$IfDef HAS_PROTECTION}
procedure TFPDFExt.SetProtection(Permissions: TFPDFPermissions;
  const UserPass: AnsiString; const OwnerPass: AnsiString);
var
  protection: Integer;
  op: AnsiString;
begin
  protection := 192;
  if (canPrint in Permissions) then
    Inc(protection, 4);
  if (canModify in Permissions) then
    Inc(protection, 8);
  if (canCopy in Permissions) then
    Inc(protection, 16);
  if (canAannotForms in Permissions) then
    Inc(protection, 32);

  if (OwnerPass = '') then
    op := CreateUniqID
  else
    op := OwnerPass;

  Self.encrypted := true;
  _generateencryptionkey(UserPass, op, protection);
end;

function TFPDFExt.CreateUniqID: AnsiString;
var
  guid: TGUID;
begin
  if (CreateGUID(guid) = 0) then
  begin
    Result := GUIDToString(guid);
    Result := StringReplace(Result, '-', '', [rfReplaceAll]);
    Result := copy(Result, 2, Length(Result)-2);
  end
  else
    Result := '';
end;

procedure TFPDFExt._generateencryptionkey(const UserPass: AnsiString;
  const OwnerPass: AnsiString; protection: Integer);
var
  up, op, tmp: AnsiString;
begin
  // Pad passwords
  up := Copy(UserPass+Self.padding, 1, 32);
  op := Copy(OwnerPass+Self.padding, 1, 32);
  // Compute O value
  Self.Ovalue := _Ovalue(up, op);
  // Compute encyption key
  tmp := _md5_16(up + Self.Ovalue + chr(protection) + chr(255)+chr(255)+chr(255));
  Self.encryption_key := Copy(tmp,1,5);
  // Compute U value
  Self.Uvalue := _Uvalue();
  // Compute P value
  Self.Pvalue := -((protection xor 255)+1);
end;

function TFPDFExt._md5_16(AStr: AnsiString): AnsiString;
begin
  result := md5(AStr);
end;

// Compute key depending on object number where the encrypted data is stored
function TFPDFExt._objectkey(vn: Integer): AnsiString;
  function packVXxx(vn: Integer): AnsiString;
  begin
    // https://gist.github.com/pifantastic/290042/548987bc88501ad48cf3efdc71ef65e77b9d85eb
    Result := chr(vn and $000000FF);
    Result := Result + chr((vn shr 8) and $000000ff);
    Result := Result + chr((vn shr 16) and $000000ff);
    Result := Result + chr(0) + chr(0);
  end;
begin
  Result := Copy(_md5_16( Self.encryption_key + packVXxx(vn)), 1, 10);
end;

function TFPDFExt._Ovalue(const UserPass: AnsiString; const OwnerPass: AnsiString): AnsiString;
var
  tmp, owner_RC4_key: AnsiString;
begin
  tmp := _md5_16(OwnerPass);
  owner_RC4_key := copy(tmp, 1, 5);
  Result := RC4(owner_RC4_key, UserPass);
end;

function TFPDFExt._Uvalue(): AnsiString;
begin
  Result := RC4(Self.encryption_key, Self.padding);
end;

function TFPDFExt.RC4(const AKey: AnsiString; const AData: AnsiString): AnsiString;
var
  kd: AnsiString;
  st: array [0..255] of Byte;
  l, j, i: Integer;
  t, a, b, kp: Byte;
  ch: Char;

  Function DupeString(const AText: AnsiString; ACount: Integer): AnsiString;
  var
    i, l: Integer;
  begin
    Result:='';
    if (ACount < 1) then
      Exit;

    l := length(AText);
    SetLength(Result ,ACount*l);
    for i := 0 to ACount-1 do
      move(AText[1], Result[l*i+1], l);
  end;

begin
  if (AKey <> Self.last_key) then
  begin
    l := Ceil(256/Length(AKey));
    kd := DupeString(AKey, l);
    for i := 0 to 255 do
      st[i] := i;

    j := 0;
    for i := 0 to 255 do
    begin
      t := st[i];
      j := (j + t + ord(kd[i+1])) mod 256;
      st[i] := st[j];
      st[j] := t;
    end;
    Self.last_key := AKey;
    move(st[0], Self.last_state[0], 256);
  end
  else
    move(Self.last_state[0], st[0], 256);

  l := Length(AData);
  a := 0;
  b := 0;
  Result :='';
  SetLength(Result, l);
  for i := 1 to l do
  begin
    a := (a+1) mod 256;
    t := st[a];
    b := (b+t) mod 256;
    st[a] := st[b];
    st[b] := t;
    kp := st[(st[a]+st[b]) mod 256];
    ch := chr(ord(AData[i]) xor kp);
    move(ch, Result[i], 1)
  end;
end;
{$EndIf}

function TFPDFExt.FindNextTagPos(const AHtml: String; out ATag: String;
  OffSet: Integer): Integer;
var
  p1, p2: Integer;
begin
  ATag := '';
  p1 := PosEx('<', AHtml, OffSet);
  if (p1 > 0) then
  begin
     p2 := PosEx('>', AHtml, p1);
     if (p2 > 0) then
       ATag := copy(AHtml, p1+1, p2-p1-1) ;
  end;

  Result := p1;
end;

procedure TFPDFExt.OpenTag(const ATag: String);
var
  s: String;
  p1, p2: Integer;
begin
  // Opening tag
  s := UpperCase(ATag);
  if ((s = 'B') or (s = 'I') or (s = 'U')) then
    SetStyle(s, true)
  else if (s = 'BR') then
    Ln(5)
  else if (s[1] = 'A') then
  begin
    p1 := Pos('href="', ATag);
    if (p1 > 0) then
    begin
      Inc(p1, 6);
      p2 := PosEx('"', ATag+'"', p1+1);
      Self.fHREF := copy(ATag, p1, p2-p1);
    end;
  end;
end;

procedure TFPDFExt.CloseTag(const ATag: String);
var
  s: String;
begin
  // Closing tag
  s := UpperCase(ATag);
  if ((s = 'B') or (s = 'I') or (s = 'U')) then
    SetStyle(s, False)
  else if (s = 'A') then
    Self.fHREF := '';
end;

procedure TFPDFExt.SetDash(ABlack, AWhite: double);
{ http://www.fpdf.org/en/script/script33.php - yukihiro_o }
begin
  if (ABlack > 0) and (AWhite > 0) then
    _out(Format('[%.3f %.3f] 0 d', [ABlack * Self.k, AWhite * Self.k], FPDFFormatSetings))
  else
    _out('[] 0 d');
end;

procedure TFPDFExt.SetDash(AWidth: double);
begin
  SetDash(AWidth, AWidth);
end;

procedure TFPDFExt.SetStyle(const ATag: String; Enable: Boolean);
var
  p: Integer;
begin
  p := pos(ATag, Self.fFontStyle);
  if Enable and (p = 0) then
    Self.fFontStyle := Self.fFontStyle + ATag
  else if (not Enable) and (p > 0) then
    Delete(Self.fFontStyle, P, Length(ATag));

  SetFont('',Self.fFontStyle);
end;

function TFPDFExt.TextBox(vX, vY, vWidht, vHeight: double;
  const AText: string; const vAlign,  hAlign: char;
  ABorder, AWordWrap, AScale: boolean; ALineSpacing: double): double;
var
  wText, wLine: string;
  IncY, OldFontSize, AltText, x1, y1, Comp, MaxHeight, wIndent: double;
  wN, i: integer;
  wLines: TStringArray;
begin
  MaxHeight := vHeight;
  wText := AText;
  OldFontSize := Self.FontSizePt;
  Result := vY;
  wIndent := 0;
  if vWidht < 0 then
    Exit;
  wText := Trim(AText);
  if ABorder then
    Self.RoundedRect(vX, vY, vWidht, vHeight, 0.8, '', 'D');
  IncY := Self.FontSize;
  if AWordWrap and (wText <> '') then
  begin
    while AScale and (GetStringHeight(wText, vWidht, ALineSpacing, wIndent) > MaxHeight) do
    begin
      if Self.FontSizePt > 8 then
        Self.SetFont(Self.FontFamily, Self.FontStyle, Self.FontSizePt - 0.5)
      else
        Self.SetFont(Self.FontFamily, Self.FontStyle, Self.FontSizePt - 0.1);
      IncY := Self.FontSize;
    end;
    wN := Self.WordWrap(wText, vWidht, wIndent);
  end
  else
    wN := Length(Split(wText, sLineBreak));

  AltText := (IncY * wN) + ((wN - 1) * ALineSpacing);

  wLines := Split(wText, sLineBreak);

  case vAlign of
    'C': y1 := vY + IncY + ((vHeight - AltText) / 2) - 1;
    'B': y1 := (vY + vHeight + IncY) - AltText - 1;
  else
    // Default: 'T' (top)
    y1 := vY + IncY;
  end;

  for i := 0 to Length(wLines) - 1 do
  begin
    wLine := wLines[i];
    wText := Trim(wLine);
    Comp  := Self.GetStringWidth(wText);
    if Comp > vWidht then
    begin
      if AScale then
      begin
        while Comp > vWidht do
        begin
          if Self.FontSizePt > 8 then
            Self.SetFont(Self.FontFamily, Self.FontStyle, Self.FontSizePt - 0.5)
          else
            Self.SetFont(Self.FontFamily, Self.FontStyle, Self.FontSizePt - 0.1);
          Comp := Self.GetStringWidth(wText);
        end;
      end
      else
        repeat
          wText := Copy(wText, 1, Length(wText) - 1);
          Comp := Self.GetStringWidth(wText);
        until Comp <= vWidht;
    end;

    case hAlign of
      'C': x1 := vX + ((vWidht - Comp) / 2);
      'R': x1 := vX + vWidht - (Comp + 0.5);
    else
      // Default: 'L' (left)
      x1 := vX + 0.5;
    end;

    x1 := x1 + wIndent;
    Self.Text(x1, y1, wText);

    if not AWordWrap and (Self.FontSizePt <> OldFontSize) then
      Self.SetFont(Self.FontFamily, Self.FontStyle, OldFontSize);

//    if Indent > 0 then
//    begin
//      x1 := x1 - Indent;
//      Indent := 0;
//    end;

    y1 := y1 + IncY + ALineSpacing;
    if ((MaxHeight > 0) and (y1 > (vY + (MaxHeight)))) then
      break;
  end;
  Self.SetFont(Self.FontFamily, Self.FontStyle, OldFontSize);
  Result := (y1 - vY) - IncY - ALineSpacing;
end;

procedure TFPDFExt.PutLink(const AURL, AText: String);
begin
  // Put a hyperlink
  SetTextColor(0, 0, 255);
  SetStyle('U', true);
  Write(5, AText, AURL);
  SetStyle('U', false);
  SetTextColor(0);
end;

{$IfDef HAS_HTTP}
procedure TFPDFExt.Image(const vFileOrURL: string; vX: double; vY: double;
  vWidth: double; vHeight: double; const vLink: string);
var
  s, ext: string;
  ms: TMemoryStream;
  img: TFPDFImageInfo;
  i: integer;
begin
  //Put an image From Web on the page
  s := LowerCase(vFileOrURL);
  if ((Pos('http://', s) > 0) or (Pos('https://', s) > 0)) then
  begin
    img := FindUsedImage(vFileOrURL);
    if (img.Data = '') then
    begin
      ext := '';
      if (Pos('.jpg', s) > 0) or (Pos('.jpeg', s) > 0) then
        ext := 'JPG'
      else if (Pos('.png', s) > 0) then
        ext := 'PNG'
      else
        Error('Supported image not found in URL: ' + vFileOrURL);

      ms := TMemoryStream.Create;
      try
        GetImageFromURL(vFileOrURL, ms);
        inherited Image(ms, ext, vX, vY, vWidth, vHeight, vLink);
        i := Length(Self.images) - 1;
        Self.images[i].ImageName := vFileOrURL;
      finally
        ms.Free;
      end;
    end
    else
      inherited Image(img, vX, vY, vWidth, vHeight, vLink);
  end
  else
    inherited Image(vFileOrURL, vX, vY, vWidth, vHeight, vLink);
end;
{$EndIf}

procedure TFPDFExt.CodeEAN13(const ABarCode: string; vX: double; vY: double;
  BarHeight: double; BarWidth: double);
var
  script: TFPDFScriptCodeEAN;
begin
  script := TFPDFScriptCodeEAN.Create(Self);
  try
    script.CodeEAN13(ABarCode, vX, vY, BarHeight, BarWidth);
  finally
    script.Free;
  end;
end;

procedure TFPDFExt.CodeEAN8(const ABarCode: string; vX: double; vY: double;
  BarHeight: double; BarWidth: double);
var
  script: TFPDFScriptCodeEAN;
begin
  script := TFPDFScriptCodeEAN.Create(Self);
  try
    script.CodeEAN8(ABarCode, vX, vY, BarHeight, BarWidth);
  finally
    script.Free;
  end;
end;

procedure TFPDFExt.CodeI25(const ABarCode: string; vX: double; vY: double;
  BarHeight: double; BarWidth: double);
var
  script: TFPDFScriptCodeI25;
begin
  script := TFPDFScriptCodeI25.Create(Self);
  try
    script.CodeI25(ABarCode, vX, vY, BarHeight, BarWidth);
  finally
    script.Free;
  end;
end;

procedure TFPDFExt.Code39(const ABarCode: string; vX: double; vY: double;
  BarHeight: double; BarWidth: double);
var
  script: TFPDFScriptCode39;
begin
  script := TFPDFScriptCode39.Create(Self);
  try
    script.Code39(ABarCode, vX, vY, BarHeight, BarWidth);
  finally
    script.Free;
  end;
end;

procedure TFPDFExt.Code128(const ABarCode: string; vX: double; vY: double;
  BarHeight: double; BarWidth: double);
var
  script: TFPDFScriptCode128;
begin
  script := TFPDFScriptCode128.Create(Self);
  try
    script.Code128(ABarCode, vX, vY, BarHeight, BarWidth);
  finally
    script.Free;
  end;
end;

{$IfDef DelphiZXingQRCode}
procedure TFPDFExt.QRCode(vX, vY, vQRCodeSize: double; const QRCodeData: String;
  AEncoding: TQRCodeEncoding);
var
  qr: TDelphiZXingQRCode;
  DotSize: double;
begin
  qr := TDelphiZXingQRCode.Create;
  try
    qr.Encoding  := AEncoding;
    qr.QuietZone := 1;
    qr.Data := widestring(QRCodeData);
    DotSize := vQRCodeSize / qr.Rows;
  finally
    qr.Free;
  end;

  QRCode(vX, vY, QRCodeData, DotSize, AEncoding);
end;

function TFPDFExt.QRCode(vX: double; vY: double; const QRCodeData: String;
  DotSize: Double; AEncoding: TQRCodeEncoding): double;
var
  qr: TDelphiZXingQRCode;
  PDF2DMatrix: TFPDF2DMatrix;
  r, c: Integer;
begin
  qr := TDelphiZXingQRCode.Create;
  try
    qr.Encoding  := AEncoding;
    qr.QuietZone := 1;
    qr.Data := widestring(QRCodeData);

    SetLength(PDF2DMatrix, qr.Rows);
    for r := 0 to qr.Rows-1 do
    begin
      SetLength(PDF2DMatrix[r], qr.Columns);
      for c := 0 to qr.Columns-1 do
      begin
        if (qr.IsBlack[r, c]) then
          PDF2DMatrix[r][c] := 1
        else
          PDF2DMatrix[r][c] := 0;
      end;
    end;

    Result := qr.Rows * DotSize;
  finally
    qr.Free;
  end;

  Draw2DMatrix(PDF2DMatrix, vX, vY, DotSize);
end;
{$EndIf}

procedure TFPDFExt.DashedLine(vX1, vY1, vX2, vY2, ADashWidth: double);
begin
  SetDash(ADashWidth);
  try
    Line(vX1, vY1, vX2, vY2);
  finally
    SetDash(0);
  end;
end;

procedure TFPDFExt.DashedRect(vX, vY, vWidht, vHeight: Double;
  const vStyle: String; ADashWidth: double);
begin
  SetDash(ADashWidth);
  try
    Rect(vX, vY, vWidht, vHeight, vStyle);
  finally
    SetDash(0);
  end;
end;

procedure TFPDFExt.Draw2DMatrix(AMatrix: TFPDF2DMatrix; vX: double; vY: double;
  DotSize: Double);
var
  rows, cols, r, c: Integer;
  wX: Double;
begin
  rows := Length(AMatrix);
  if (rows < 1) then
    Exit;

  cols := Length(AMatrix[0]);
  if (cols < 1) then
    Exit;

  if DotSize = 0 then
    DotSize := cDefBarWidth;

  wX := vX;
  for r := 0 to rows-1 do
  begin
    vX := wX;
    for c := 0 to cols-1 do
    begin
      if (AMatrix[r][c] <> 0) then
        Rect(vX, vY, DotSize, DotSize, 'F');
      vX := vX + DotSize;
    end;
    vY := vY + DotSize;
  end;
end;

procedure TFPDFExt.Header;
begin
  if Assigned(fOnHeader) then
    fOnHeader(Self);
end;

procedure TFPDFExt.Footer;
begin
  if Assigned(fOnFooter) then
    fOnFooter(Self);
end;

function TFPDFExt.GetNumLines(const AText: string; AWidth,
  AIndent: double): integer;
var
  LocalText: string;
begin
  LocalText := Trim(AText);
  Result := WordWrap(LocalText, AWidth - 0.2, AIndent);
end;

function TFPDFExt.GetStringHeight(const AText: string; AWidth, ALineSpacing,
  AIndent: double): double;
var
  NumLines: integer;
begin
  NumLines := GetNumLines(AText, AWidth, AIndent);
  Result := RoundTo((NumLines * FontSize) + IfThen(NumLines > 1, (NumLines - 1) * ALineSpacing), -2);
  Result := Result + 0.5;
end;

{$IfDef HAS_HTTP}
{$IfDef USE_SYNAPSE}
procedure TFPDFExt.GetImageFromURL(const aURL: string; const aResponse: TStream);
var
  vHTTP: THTTPSend;
  Ok: boolean;
begin
  vHTTP := THTTPSend.Create;
  try
    if (ProxyHost <> '') then
    begin
      vHTTP.ProxyHost := ProxyHost;
      vHTTP.ProxyPort := ProxyPort;
      vHTTP.ProxyUser := ProxyUser;
      vHTTP.ProxyPass := ProxyPass;
    end;
    Ok := vHTTP.HTTPMethod('GET', aURL);
    if Ok then
    begin
      aResponse.Seek(0, soBeginning);
      aResponse.CopyFrom(vHTTP.Document, 0);
    end
    else
      Error('Http Error: ' + IntToStr(vHTTP.ResultCode) +
        ' when downloading from: ' + aURL);
  finally
    vHTTP.Free;
  end;
end;
{$Else}
procedure TFPDFExt.GetImageFromURL(const aURL: string; const aResponse: TStream);
var
  vHTTP: TFPHTTPClient;
begin
  vHTTP := TFPHTTPClient.Create(nil);
  try
    if (ProxyHost <> '') then
    begin
      vHTTP.Proxy.Host := ProxyHost;
      vHTTP.Proxy.Port := StrToIntDef(ProxyPort, 0);
      vHTTP.Proxy.UserName := ProxyUser;
      vHTTP.Proxy.Password := ProxyPass;
    end;

    vHTTP.Get(aURL, aResponse);
  finally
    vHTTP.Free;
  end;
end;
{$EndIf}
{$EndIf}

procedure TFPDFExt._endpage;
begin
  if (Self.angle <> 0) then
  begin
    Self.angle := 0;
    _out('Q');
  end;

  EndLayer();

  inherited _endpage;
end;

procedure TFPDFExt._putstream(const Adata: AnsiString);
{$IfDef HAS_PROTECTION}
var
  s: AnsiString;
{$EndIf}
begin
  {$IfDef HAS_PROTECTION}
   if Self.encrypted then
     s := RC4(_objectkey(Self.n), Adata)
   else
     s := Adata;

   inherited _putstream(s);
  {$Else}
   inherited _putstream(Adata);
  {$EndIf}
end;

function TFPDFExt._textstring(const AString: String): String;
{$IfDef HAS_PROTECTION}
var
  s: AnsiString;
{$EndIf}
begin
  {$IfDef HAS_PROTECTION}
   // Format a text string
   if (not _isascii(AString)) then
     s := AnsiString(_UTF8toUTF16(AString))
   else
     s := AString;

   if Self.encrypted then
     s := RC4( _objectkey(Self.n), s);

   Result := '('+_escape(s)+')';
  {$Else}
   Result := inherited _textstring(AString);
  {$EndIf}
end;

procedure TFPDFExt._putresourcedict;
var
  i, l: Integer;
begin
  inherited _putresourcedict;

  l := Length(Self.layers);
  if (l > 0) then
  begin
    _put('/Properties <<');
    for i := 0 to l-1 do
      _put('/OC'+IntToStr(i)+' '+IntToStr(Self.layers[i].n)+' 0 R');
    _put('>>');
  end;
end;

procedure TFPDFExt._putresources;
begin
  _putlayers;
  inherited _putresources;

  if (Self.encrypted) then
  begin
    _newobj();
    Self.enc_obj_id := self.n;
    _put('<<');
    _putencryption();
    _put('>>');
    _put('endobj');
  end;
end;

procedure TFPDFExt._putencryption;
begin
  _put('/Filter /Standard');
  _put('/V 1');
  _put('/R 2');
  _put('/O ('+_escape(Self.Ovalue)+')');
  _put('/U ('+_escape(Self.Uvalue)+')');
  _put('/P '+FloatToStr(Self.Pvalue));
end;

procedure TFPDFExt._putlayers;
var
  i, l: Integer;
begin
  l := Length(Self.layers)-1;
  for i := 0 to l do
  begin
    _newobj();
    Self.layers[i].n := Self.n;
    _put('<</Type /OCG /Name '+_textstring(Self.layers[i].Name)+'>>');
    _put('endobj');
  end;
end;

procedure TFPDFExt._putcatalog;
var
  l_off, s: String;
  l, i: Integer;
begin
  inherited _putcatalog;

  s := '';
  l_off := '';
  l := Length(Self.layers)-1;
  for i := 0 to l do
  begin
    s := s + IntToStr(Self.layers[i].n)+' 0 R ';
    if (not Self.layers[i].Visible) then
      l_off := l_off + IntToStr(Self.layers[i].n)+' 0 R ';
  end;

  if (s <> '') then
  begin
    _put('/OCProperties <</OCGs ['+s+'] /D <</OFF ['+l_off+'] /Order ['+s+']>>>>');
    if (Self.open_layer_pane) then
      _put('/PageMode /UseOC');
  end;
end;

procedure TFPDFExt._puttrailer;
begin
  inherited _puttrailer;

  if Self.encrypted then
  begin
    _put('/Encrypt '+IntToStr(Self.enc_obj_id)+' 0 R');
    _put('/ID [()()]');
  end;
end;

procedure TFPDFExt._enddoc;
begin
  if (Self.PDFVersion < 1.5) and (Length(Self.layers) > 0) then
    Self.PDFVersion := 1.5;

  inherited _enddoc;
end;

end.

