{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{*******************************************************************************
|* Historico
|*
|* 16/12/2008: Wemerson Souto
|*  - Doação do componente para o Projeto ACBr
|* 20/08/2009: Caique Rodrigues
|*  - Doação units para geração do Danfe via QuickReport
*******************************************************************************}

{$I ACBr.inc}

unit ACBrDFeQRCodeBar;

{*********************************************************
 Código de barras Code128C com DV baseado na :
 Turbo Power SysTools 4.03 Unit : StBarC.pas

 ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** }

interface

uses
  Windows,
  Classes, ClipBrd, Controls, Graphics, Messages, SysUtils, ACBrDFeException;

const
  bcMaxBarCodeLen = 255;
  bcGuardBarAbove = True;
  bcGuardBarBelow = True;
  bcDefNarrowToWideRatio = 2;

type
  TBarKind = (bkSpace, bkBar, bkThreeQuarterBar, bkHalfBar, bkGuard, bkSupplement, bkBlankSpace);

  TBarKindSet = set of TBarKind;
  TStDigitArray = array[1..bcMaxBarCodeLen] of Byte;

  TBarData = class
    FKind    : TBarKindSet;
    FModules : Integer;
  public
    property Kind : TBarKindSet
      read FKind
      write FKind;
    property Modules : Integer
      read FModules
      write FModules;
  end;

  TBarCode128cInfo = class
  private
    FBars       : TList;
    function GetBars(Index : Integer) : TBarData;
    function GetCount : Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(ModuleCount : Integer; BarKind : TBarKindSet);
    procedure Clear;
    property Bars[Index : Integer] : TBarData read GetBars; default;
    property Count : Integer read GetCount;
  end;

  TBarCode128c = class ( TCanvas ) //class(TGraphicControl)
  protected {private}
    {property variables}
    FAddCheckChar     : Boolean;
    FBarColor         : TColor;
    FBarToSpaceRatio  : Double;
    FBarNarrowToWideRatio : Integer;
    FBarWidth         : Double;         {in mils}
    FBearerBars       : Boolean;

    {internal variables}
    bcBarInfo        : TBarCode128cInfo;
    bcBarModWidth    : Integer; {width of single bar}
    bcCheckK         : Integer; {"K" check character for use by Code11}
    bcDigits         : TStDigitArray;
    bcDigitCount     : Integer;
    bcSpaceModWidth  : Integer; {width of empty space between bars}
    bcNormalWidth    : Integer;
    bcSpaceWidth     : Integer;
    bcSupplementWidth: Integer;
    FCode : string ;

    {property methods}
    function GetCode : string;
    procedure SetBarNarrowToWideRatio(Value: Integer);
    procedure SetBarWidth(Value : Double);
    procedure SetBearerBars(Value : Boolean);
    procedure SetCode(const Value : string);

    {internal methods}
    procedure CalcBarCode;
    procedure CalcBarCodeWidth;
    procedure DrawBarCode(const R : TRect);
    function GetDigits(Characters : string) : Integer;
    procedure PaintPrim(const R : TRect);
    function SmallestLineWidth(PixelsPerInch : Integer) : Double;

  public
    constructor Create ; reintroduce ;
    destructor Destroy; override;

    procedure GetCheckCharacters(const S : string; var C, K : Integer);

  published
    {properties}
    property BarNarrowToWideRatio : Integer read FBarNarrowToWideRatio write SetBarNarrowToWideRatio default bcDefNarrowToWideRatio;
    property BarWidth : Double read FBarWidth write SetBarWidth;
    property BearerBars : Boolean read FBearerBars write SetBearerBars ;
    property Code : string read GetCode write SetCode;

    class procedure PaintCodeToCanvas( ACode : string ; ACanvas : TCanvas; ARect : TRect);

  end;

implementation

const

  Code128 : array[0..106] of string[7] =
     {BSBSBS}   {Value  CodeA  CodeB   CodeC}
    ('212222',  {0	SPACE	SPACE	00}
     '222122',  {1	!	!	01}
     '222221',  {2	"	"	02}
     '121223',  {3	#	#	03}
     '121322',  {4	$	$	04}
     '131222',  {5	%	%	05}
     '122213',  {6	&	&	06}
     '122312',  {7	'	'	07}
     '132212',  {8	(	(	08}
     '221213',  {9	)	)	09}
     '221312',  {10	* 	*	10}
     '231212',  {11	+	+	11}
     '112232',  {12	,	,	12}
     '122132',  {13	-	-	13}
     '122231',  {14	.	.	14}
     '113222',  {15	/	/	15}
     '123122',  {16	0	0	16}
     '123221',  {17	1	1	17}
     '223211',  {18	2	2	18}
     '221132',  {19	3	3	19}
     '221231',  {20	4	4	20}
     '213212',  {21	5	5	21}
     '223112',  {22	6	6	22}
     '312131',  {23	7	7	23}
     '311222',  {24	8	8	24}
     '321122',  {25	9	9	25}
     '321221',  {26	:	:	26}
     '312212',  {27	;	;	27}
     '322112',  {28	<	<	28}
     '322211',  {29	= 	= 	29}
     '212123',  {30	>	>	30}
     '212321',  {31	?	?	31}
     '232121',  {32	@	@	32}
     '111323',  {33	A	A	33}
     '131123',  {34	B	B	34}
     '131321',  {35	C	C	35}
     '112313',  {36	D	D	36}
     '132113',  {37	E	E	37}
     '132311',  {38	F	F	38}
     '211313',  {39	G	G	39}
     '231113',  {40	H	H	40}
     '231311',  {41	I	I	41}
     '112133',  {42	J	J	42}
     '112331',  {43	K	K	43}
     '132131',  {44	L	L	44}
     '113123',  {45	M	M	45}
     '113321',  {46	N	N	46}
     '133121',  {47	O	O	47}
     '313121',  {48	P	P	48}
     '211331',  {49	Q	Q	49}
     '231131',  {50	R	R	50}
     '213113',  {51	S	S	51}
     '213311',  {52	T	T	52}
     '213131',  {53	U	U	53}
     '311123',  {54	V	V	54}
     '311321',  {55	W	W	55}
     '331121',  {56	X	X	56}
     '312113',  {57	Y	Y	57}
     '312311',  {58	Z	Z	58}
     '332111',  {59	[	[	59}
     '314111',  {60	\	\	60}
     '221411',  {61	]	]	61}
     '431111',  {62	^	^	62}
     '111224',  {63	_ 	_ 	63}
     '111422',  {64	NU	`	64}
     '121124',  {65	SH	a	65}
     '121421',  {66	SX	b	66}
     '141122',  {67	EX	c	67}
     '141221',  {68	ET	d	68}
     '112214',  {69	EQ	e	69}
     '112412',  {70	AK	f	70}
     '122114',  {71	BL	g	71}
     '122411',  {72	BS	h	72}
     '142112',  {73	HT	i	73}
     '142211',  {74	LF	j	74}
     '241211',  {75	VT	k	75}
     '221114',  {76	FF	l	76}
     '413111',  {77	CR	m	77}
     '241112',  {78	SO	n	78}
     '134111',  {79	SI	o	79}
     '111242',  {80	DL	p	80}
     '121142',  {81	D1	q	81}
     '121241',  {82	D2	r	82}
     '114212',  {83	D3	s	83}
     '124112',  {84	D4	t	84}
     '124211',  {85	NK	u	85}
     '411212',  {86	SY	v	86}
     '421112',  {87	EB	w	87}
     '421211',  {88	CN	x	88}
     '212141',  {89	EM	y	89}
     '214121',  {90	SB	z	90}
     '412121',  (*91	EC	{	91*)
     '111143',  {92	FS		92}
     '111341',  (*93	GS	}	93*)
     '131141',  {94	RS	~	94}
     '114113',  {95	US	DEL	95}
     '114311',  {96	FNC 3	FNC 3	96}      {use #132}
     '411113',  {97	FNC 2	FNC 2	97}      {use #131}
     '411311',  {98	SHIFT	SHIFT	98}      {use #130}
     '113141',  {99	CODE C	CODE C	99}      {use #135}
     '114131',  {100	CODE B	FNC 4	CODE B}  {use #134}
     '311141',  {101	FNC 4	CODE A	CODE A}  {use #133}
     '411131',  {102	FNC 1	FNC 1	FNC 1 }  {use #130}
     '211412',  {103	CODE A}                  {use #136}
     '211214',  {104	CODE B}                  {use #137}
     '211232',  {105	CODE C}                  {use #138}
     '2331112');{106    STOP}                    {use #139}


{*** helper routines ***}

function RectWidth(const R : TRect) : Integer;
begin
  Result := R.Right-R.Left;
end;

function RectHeight(const R : TRect) : Integer;
begin
  Result := R.Bottom-R.Top;
end;


{*** TBarCode128cInfo ***}

procedure TBarCode128cInfo.Add(ModuleCount : Integer; BarKind : TBarKindSet);
var
  Bar : TBarData;
begin
  Bar := TBarData.Create;
  Bar.Modules := ModuleCount;
  Bar.Kind := BarKind;
  FBars.Add(Bar);
end;

procedure TBarCode128cInfo.Clear;
var
  I : Integer;
begin
  for I := 0 to FBars.Count-1 do
    TBarData(FBars[I]).Free;
  FBars.Clear;
end;

constructor TBarCode128cInfo.Create;
begin
  inherited Create;

  FBars := TList.Create;
end;

destructor TBarCode128cInfo.Destroy;
begin
  Clear;
  FBars.Free;
  FBars := nil;

  inherited Destroy;
end;

function TBarCode128cInfo.GetBars(Index : Integer) : TBarData;
begin
  Result := FBars[Index];
end;

function TBarCode128cInfo.GetCount : Integer;
begin
  Result := FBars.Count;
end;


{*** TBarCode128c ***}

procedure TBarCode128c.CalcBarCode;
var
  I  : Integer;
  CheckC  : Integer;
  CheckK  : Integer;
  C       : string;

  procedure AddCode(const S : string; AKind : TBarKindSet);
  var
    I : Integer;
  begin
    for I := 1 to Length(S) do
      if S[I] = '0' then
        bcBarInfo.Add(1, AKind - [bkBar, bkThreeQuarterBar, bkHalfBar] + [bkSpace])
      else
        bcBarInfo.Add(StrToInt(S[I]), AKind);
  end;


  procedure AddCodeModules(const S : string);
  var
    K : Integer;
  begin
    for K := 1 to Length(S) do begin
      if Odd(K) then
        bcBarInfo.Add(StrToInt(S[K]), [bkBar])
      else
        bcBarInfo.Add(StrToInt(S[K]), [bkSpace]);
    end;
  end;

  procedure AddCodeWideNarrow(const S : string);
  var
    K : Integer;
  begin
    for K := 1 to Length(S) do begin
      case S[K] of
        '0' : if Odd(K) then
                bcBarInfo.Add(1, [bkBar])
              else
                bcBarInfo.Add(1, [bkSpace]);
        '1' : if Odd(K) then
                bcBarInfo.Add(FBarNarrowToWideRatio, [bkBar])
              else
                bcBarInfo.Add(FBarNarrowToWideRatio, [bkSpace]);
      end;
    end;
  end;

begin

  bcBarInfo.Clear;
  if Code = '' then
    Exit;

  {get copy of code}
  C := Code;

  {get digits}
  {add start code}
  if ( C[1] <> #138 ) then
     C := #138 + C;

  bcDigitCount := GetDigits(C);

  {add check character}
  GetCheckCharacters(C, CheckC, CheckK);
  Inc(bcDigitCount);
  bcDigits[bcDigitCount] := CheckC;

  {add stop code}
  Inc(bcDigitCount);
  bcDigits[bcDigitCount] := 106;

  for I  := 1 to bcDigitCount do
    AddCodeModules(Code128[bcDigits[I]]);

end;

procedure TBarCode128c.CalcBarCodeWidth;
var
  I : Integer;
begin
  bcNormalWidth := 0;
  bcSpaceWidth := 0;
  bcSupplementWidth := 0;
  for I := 0 to bcBarInfo.Count-1 do begin
    if bkSpace in bcBarInfo[I].Kind then
    begin
      if bkBlankSpace in bcBarInfo[I].Kind then
        Inc(bcSpaceWidth, bcSpaceModWidth*bcBarInfo[I].Modules)
      else if bkSupplement in bcBarInfo[I].Kind then
        Inc(bcSupplementWidth, bcSpaceModWidth*bcBarInfo[I].Modules)
      else
        Inc(bcNormalWidth, bcSpaceModWidth*bcBarInfo[I].Modules)
    end else begin
      if bkBlankSpace in bcBarInfo[I].Kind then
        Inc(bcSpaceWidth, bcBarModWidth*bcBarInfo[I].Modules)
      else if bkSupplement in bcBarInfo[I].Kind then
        Inc(bcSupplementWidth, bcBarModWidth*bcBarInfo[I].Modules)
      else
        Inc(bcNormalWidth, bcBarModWidth*bcBarInfo[I].Modules)
    end;
  end;
end;

constructor TBarCode128c.Create ;
begin
  inherited Create ;

  bcBarInfo := TBarCode128cInfo.Create;

  {defaults}
  FBarColor := clBlack;
  FBarToSpaceRatio := 1;
  FBarNarrowToWideRatio := bcDefNarrowToWideRatio;
  FBarWidth := 12;

end;

destructor TBarCode128c.Destroy;
begin
  bcBarInfo.Free;
  bcBarInfo := nil;

  inherited Destroy;
end;


procedure TBarCode128c.DrawBarCode(const R : TRect);
var
  I, X, Y, TQ    : Integer;
  BarCodeHeight  : Integer;
  BarCodeWidth   : Integer;
  PixelsPerInchX : Integer;
  SmallestWidth  : Double;

  function DrawBar(XPos, YPos, AWidth, AHeight : Integer) : Integer;
  begin
    Rectangle(XPos, YPos, XPos+AWidth, YPos+AHeight);
    Result := XPos + AWidth;
  end;

begin
  Brush.Color := FBarColor;
  Brush.Style := bsSolid;

  PixelsPerInchX := GetDeviceCaps( Handle, LOGPIXELSX ) ;

  {determine narrowest line width}
  SmallestWidth := SmallestLineWidth(PixelsPerInchX);

  {find sizes for the BarCode elements}
  bcBarModWidth := Round(FBarWidth/1000 * PixelsPerInchX);
  if bcBarModWidth < FBarToSpaceRatio then
    bcBarModWidth := Round(FBarToSpaceRatio);
  if bcBarModWidth < SmallestWidth then
    bcBarModWidth := Round(SmallestWidth);
  bcSpaceModWidth := Round(bcBarModWidth / FBarToSpaceRatio);

  {total width of BarCode and position within rect}
  CalcBarCodeWidth;
  BarCodeWidth := bcNormalWidth + bcSpaceWidth + bcSupplementWidth;
  BarCodeHeight := RectHeight(R);
  if BarCodeWidth < RectWidth(R) then
    X := R.Left + (RectWidth(R)-BarCodeWidth) div 2
  else
    X := R.Left;
  Y := R.Top;

  {three quarter height bar adjustment}
  TQ := BarCodeHeight div 4;

  {draw the bar code}
  for I := 0 to bcBarInfo.Count-1 do begin
    if bkSpace in bcBarInfo[I].Kind then
      Inc(X, bcSpaceModWidth*bcBarInfo[I].Modules)
    else if (bkBar in bcBarInfo[I].Kind) or (bkGuard in bcBarInfo[I].Kind) then
      X := DrawBar(X, Y, bcBarModWidth*bcBarInfo[I].Modules, BarCodeHeight)
    else if (bkThreeQuarterBar in bcBarInfo[I].Kind) then
      X := DrawBar(X, Y+TQ, bcBarModWidth*bcBarInfo[I].Modules, BarCodeHeight-TQ);
  end;
end;


procedure TBarCode128c.GetCheckCharacters(const S : string; var C, K : Integer);
var
  I  : Integer;
  C1 : Integer;
  St : string;
begin
  C := -1;
  K := -1;
  St := S;

  {get digits}
  bcDigitCount := GetDigits(St);

  C1 := bcDigits[1];
  for I := 2 to bcDigitCount do
    C1 := C1 + bcDigits[I]*(I-1);

  C := C1 mod 103;
  if C = 103 then
    C := 0;
end;

function TBarCode128c.GetCode : string;
begin
  Result := FCode ;
end;

function TBarCode128c.GetDigits(Characters : string) : Integer;
var
 I             : Integer;
 RLen          : Integer;
 NeedCharCount : Boolean;

  procedure GetACode128CDigit ;
  var
    J : Integer;

  begin
    case ( Characters [ I ] ) of
      #130     : bcDigits[RLen + 1] := 98;  {rest are manufactured characters}
      #131     : bcDigits[RLen + 1] := 97;
      #132     : bcDigits[RLen + 1] := 96;
      #133     : bcDigits[RLen + 1] := 98;
      #134     : bcDigits[RLen + 1] := 100;
      #135     : bcDigits[RLen + 1] := 99;
      #136     : bcDigits[RLen + 1] := 103;
      #137     : bcDigits[RLen + 1] := 104;
      #138     : bcDigits[RLen + 1] := 105;
      #139     : bcDigits[RLen + 1] := 106;
    else
      try
        J := StrToInt (Copy (Characters, I, 2));
        bcDigits[RLen + 1] := J;
        Inc (I);
      except
        raise EACBrDFeException.Create( 'Caracteres inválidos' );
      end;
    end;
    Inc (I);
    Inc (RLen);
  end;


  function CountCode128Digits (Index : Integer) : Integer;
  begin
    Result := 0;
    while (Index <= Length (Characters)) and
          (Characters[Index] >= '0') and (Characters[Index] <= '9') do begin
      Inc (Result);
      Inc (Index);
    end;
  end;

  function CheckCode128Digits (Index : Integer; CharsLen : Integer) : Boolean;
  var
    NumDigits : Integer;
  begin
    Result := False;
    NumDigits := CountCode128Digits (Index);
    if NumDigits mod 2 <> 0 then
    begin
      Characters := Copy (Characters, 1, Index - 1) +
                    '0' + Copy (Characters, Index, CharsLen - Index + 1);
      Result := True;
    end;
  end;


begin
  FillChar(bcDigits, SizeOf(bcDigits), #0);

  I := 1;
  Result := Length (Characters);
  RLen := 0;
  NeedCharCount := True ;

  while I <= Result do
    begin
      if (NeedCharCount) and (Characters[I] >= '0') and (Characters[I] <= '9') then
         begin
            NeedCharCount := False;
            if CheckCode128Digits (I, RLen) then
               Inc (RLen);
         end;

      GetACode128CDigit ;

    end;
  Result := RLen;

end;

procedure TBarCode128c.PaintPrim(const R : TRect);
begin
  Brush.Style := bsClear;
  Brush.Color := FBarColor;
  Pen.Color := FBarColor;
  DrawBarCode(R);
end;

class procedure TBarCode128c.PaintCodeToCanvas(ACode : string ; ACanvas : TCanvas; ARect : TRect);
var
  Margin  : Integer;
  SavedDC : LongInt;
  R       : TRect;
begin
  with Create do
     try
        Code := ACode ;
        CalcBarCode ;

        Handle := ACanvas.Handle;
        SavedDC := SaveDC(ACanvas.Handle);
        try
          {clear the specified area of the canvas}
          Brush.Color := clWhite ;
          Brush.Style := bsSolid;
          FillRect(ARect);

          {adjust height of rect to provide top and bottom margin}
          R := ARect;
          Margin := RectHeight(R)*10 div 100;
          InflateRect(R, 0, -Margin);
          PaintPrim(R);
        finally
          Handle := 0;
          RestoreDC(ACanvas.Handle, SavedDC);
        end;
     finally
        Free ;
     end ;
end;


procedure TBarCode128c.SetBarNarrowToWideRatio(Value : Integer);
begin
  if Value <> FBarNarrowToWideRatio then
     FBarNarrowToWideRatio := Value ;
end;

procedure TBarCode128c.SetBarWidth(Value : Double);
begin
  if Value <> FBarWidth then
    FBarWidth := Value;
end;

procedure TBarCode128c.SetBearerBars(Value : Boolean);
begin
  if Value <> FBearerBars then
    FBearerBars := Value;
end;

procedure TBarCode128c.SetCode(const Value : string);
begin
    FCode := Value;
end;


function TBarCode128c.SmallestLineWidth(PixelsPerInch : Integer) : Double;
begin
  Result := PixelsPerInch * 0.010; {10 mils}
  if Result < 1 then
    Result := 1;
end;

end.
