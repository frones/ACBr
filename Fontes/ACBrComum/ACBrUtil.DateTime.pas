{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}
{                                                                              }
{  Algumas funçoes dessa Unit foram extraidas de outras Bibliotecas, veja no   }
{ cabeçalho das Funçoes no código abaixo a origem das informaçoes, e autores...}
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

{$IFDEF FPC}
 {$IFNDEF NOGUI}
  {$DEFINE USE_LCLIntf}
 {$ENDIF}
{$ENDIF}

unit ACBrUtil.DateTime;

interface

Uses
  SysUtils, Math, Classes,
  ACBrBase, ACBrConsts, IniFiles,
  {$IfDef COMPILER6_UP} StrUtils, DateUtils {$Else} ACBrD5, FileCtrl {$EndIf}
  {$IfDef FPC}
    ,dynlibs, LazUTF8, LConvEncoding, LCLType
    {$IfDef USE_LCLIntf} ,LCLIntf {$EndIf}
  {$EndIf}
  {$IfDef MSWINDOWS}
    ,Windows, ShellAPI
  {$Else}
    {$IfNDef FPC}
      {$IfDef ANDROID}
      ,System.IOUtils
      {$EndIf}
      {$IfDef  POSIX}
      ,Posix.Stdlib
      ,Posix.Unistd
      ,Posix.Fcntl
      {$Else}
      ,Libc
      {$EndIf}
    {$Else}
      ,unix, BaseUnix
    {$EndIf}
    {$IfNDef NOGUI}
      {$IfDef FMX}
        ,FMX.Forms
      {$Else}
        ,Forms
      {$EndIf}
    {$EndIf}
  {$EndIf} ;

function FormatDateBr(const ADateTime: TDateTime; AFormat: String = ''): String;
function FormatDateTimeBr(const ADate: TDateTime; AFormat: String = ''): String;
function StringToDateTime( const DateTimeString : String; const Format : String = '') : TDateTime;
function StringToDateTimeDef( const DateTimeString : String; const DefaultValue : TDateTime;
    const Format : String = '') : TDateTime;
function StoD( YYYYMMDDhhnnss: String) : TDateTime;
function DtoS( ADate : TDateTime) : String;
function DTtoS( ADateTime : TDateTime) : String;

function Iso8601ToDateTime(const AISODate: string): TDateTime;
function DateTimeToIso8601(ADate: TDateTime; const ATimeZone: string = ''): string;

{ Bias = Diferença em minutos do horário atual com o UTC }
function BiasToTimeZone(const aBias: Integer): String;
function TimeZoneToBias(const aTimeZone: String): Integer;

function StrIsTimeZone(const aStr: String): Boolean;
function StrHasTimeZone(const aStr: String): Boolean;

function DateTimeUniversal(const AUTC: String = ''; const ADateTime: TDateTime = 0 ): TDateTime;

function IsWorkingDay(ADate: TDateTime): Boolean;
function WorkingDaysBetween(StartDate, EndDate: TDateTime): Integer;
function IncWorkingDay(ADate: TDateTime; WorkingDays: Integer): TDatetime;

function EncodeDataHora(const DataStr: string;
  const FormatoData: string = 'YYYY/MM/DD'): TDateTime;
function ParseDataHora(const DataStr: string): string;
function AjustarData(const DataStr: string): string;

implementation

uses
  MaskUtils, synautil,
  ACBrUtil.Compatibilidade, ACBrUtil.Strings, ACBrUtil.Base;

{-----------------------------------------------------------------------------
  Converte uma <ADateTime> para String, semelhante ao FormatDateTime,
  porém garante que o separador de Data SEMPRE será a '/'.
  Usa o padrão Brasileiro DD/MM/YYYY.
  <AFormat> pode ser especificado, para mudar a apresentação.
 ---------------------------------------------------------------------------- }
function FormatDateBr(const ADateTime: TDateTime; AFormat: String): String;
begin
  if AFormat = '' then
     AFormat := 'DD/MM/YYYY';

  Result := FormatDateTimeBr( DateOf(ADateTime), AFormat);
end;

{-----------------------------------------------------------------------------
  Converte uma <ADateTime> para String, semelhante ao FormatDateTime,
  porém garante que o separador de Data SEMPRE será a '/', e o de Hora ':'.
  Usa o padrão Brasileiro DD/MM/YYYY hh:nn:ss.
  <AFormat> pode ser especificado, para mudar a apresentação.
 ---------------------------------------------------------------------------- }
function FormatDateTimeBr(const ADate: TDateTime; AFormat: String): String;
Var
  {$IFDEF HAS_FORMATSETTINGS}
  FS: TFormatSettings;
  {$ELSE}
  OldDateSeparator: Char ;
  OldTimeSeparator: Char ;
  {$ENDIF}
begin
  if AFormat = '' then
     AFormat := 'DD/MM/YYYY hh:nn:ss';

  {$IFDEF HAS_FORMATSETTINGS}
  FS := CreateFormatSettings;
  FS.DateSeparator := '/';
  FS.TimeSeparator := ':';
  Result := FormatDateTime(AFormat, ADate, FS);
  {$ELSE}
  OldDateSeparator := DateSeparator;
  OldTimeSeparator := TimeSeparator;
  try
    DateSeparator := '/';
    TimeSeparator := ':';
    Result := FormatDateTime(AFormat, ADate);
  finally
    DateSeparator := OldDateSeparator;
    TimeSeparator := OldTimeSeparator;
  end;
  {$ENDIF}
end;

{-----------------------------------------------------------------------------
  Converte uma <DateTimeString> para TDateTime, semelhante ao StrToDateTime,
  mas verifica se o seprador da Data é compativo com o S.O., efetuando a
  conversão se necessário. Se não for possivel converter, dispara Exception
 ---------------------------------------------------------------------------- }
function StringToDateTime(const DateTimeString: String; const Format: String): TDateTime;
Var
  AStr : String;
  DS, TS: Char;
  {$IFDEF HAS_FORMATSETTINGS}
  FS: TFormatSettings;
  DateFormat, TimeFormat: String;
  p: Integer;
  {$ELSE}
  OldShortDateFormat: String ;
  {$ENDIF}

  // Remove qualquer TimeZone da String. Exemplos:
  // - '2022-02-20 02:02:55Z'      Result: '2022-02-20 02:02:55'
  // - '2022-11-11 11:11:11-03:00' Result: '2022-11-11 11:11:11'
  function RemoverTimeZone(const aDateTimeString: String): String;
  var
    wTMZ: String;
  begin
    Result := Trim(aDateTimeString);
    wTMZ := UpperCase(Result);

    if (not StrHasTimeZone(aDateTimeString)) then
      Exit;

    if (RightStr(wTMZ, 1) = 'Z') then
      Result := LeftStr(aDateTimeString, Length(wTMZ)-1)
    else
    begin
      wTMZ := RightStr(wTMZ, 6);
      Result := StringReplace(aDateTimeString, wTMZ, EmptyStr, [rfReplaceAll]);
    end;
  end;

  function AjustarDateTimeString(const DateTimeString: String; DS, TS: Char): String;
  var
    AStr: String;
  begin
    AStr := RemoverTimeZone(DateTimeString);

    if (DS <> '.') then
      AStr := StringReplace(AStr, '.', DS, [rfReplaceAll]);

    if (DS <> '-') then
      AStr := StringReplace(AStr, '-', DS, [rfReplaceAll]);

    if (DS <> '/') then
      AStr := StringReplace(AStr, '/', DS, [rfReplaceAll]);

    if (TS <> ':') then
      AStr := StringReplace(AStr, ':', TS, [rfReplaceAll]);

    Result := AStr;
  end;

begin
  Result := 0;
  if (DateTimeString = '0') or (DateTimeString = '') then
    exit;

  {$IFDEF HAS_FORMATSETTINGS}
  FS := CreateFormatSettings;
  if (Format <> '') then
  begin
    DateFormat := Format;
    TimeFormat := '';
    p := pos(' ',Format);
    if (p > 0) then
    begin
      TimeFormat := Trim(Copy(Format, p, Length(Format)));
      DateFormat := Trim(copy(Format, 1, p));
    end;
    FS.ShortDateFormat := DateFormat;
    if (TimeFormat <> '') then
      FS.ShortTimeFormat := TimeFormat;
  end;

  DS := FS.DateSeparator;
  TS := FS.TimeSeparator;

  if (Format <> '') then
  begin
    if (DS <> '/') and (pos('/', Format) > 0) then
      DS := '/'
    else if (DS <> '-') and (pos('-', Format) > 0) then
      DS := '-'
    else if (DS <> '.') and (pos('.', Format) > 0) then
      DS := '.';

    if (DS <> FS.DateSeparator) then
      FS.DateSeparator := DS;
  end;

  AStr := AjustarDateTimeString(DateTimeString, DS, TS);
  Result := StrToDateTime(AStr, FS);
  {$ELSE}
  OldShortDateFormat := ShortDateFormat ;
  try
    if Format <> '' then
      ShortDateFormat := Format ;

    DS := DateSeparator;
    TS := TimeSeparator;

    AStr := AjustarDateTimeString(DateTimeString, DS, TS);
    Result := StrToDateTime( AStr ) ;
  finally
    ShortDateFormat := OldShortDateFormat ;
  end ;
  {$ENDIF}
end ;

{-----------------------------------------------------------------------------
  Converte uma <DateTimeString> para TDateTime, semelhante ao StrToDateTimeDef,
  mas verifica se o seprador da Data é compativo com o S.O., efetuando a
  conversão se necessário. Se não for possivel converter, retorna <DefaultValue>
 ---------------------------------------------------------------------------- }
function StringToDateTimeDef(const DateTimeString : String ;
   const DefaultValue : TDateTime ; const Format : String) : TDateTime ;
begin
  if EstaVazio(DateTimeString) then
     Result := DefaultValue
  else
   begin
     try
        Result := StringToDateTime( DateTimeString, Format ) ;
     except
        Result := DefaultValue ;
     end ;
   end;
end ;

{-----------------------------------------------------------------------------
  Converte uma String no formato YYYYMMDDhhnnss  para TDateTime
 ---------------------------------------------------------------------------- }
function StoD( YYYYMMDDhhnnss: String) : TDateTime;
begin
  YYYYMMDDhhnnss := trim( YYYYMMDDhhnnss ) ;

  try
    Result := EncodeDateTime( StrToIntDef(copy(YYYYMMDDhhnnss, 1,4),0),  // YYYY
                              StrToIntDef(copy(YYYYMMDDhhnnss, 5,2),0),  // MM
                              StrToIntDef(copy(YYYYMMDDhhnnss, 7,2),0),  // DD
                              StrToIntDef(copy(YYYYMMDDhhnnss, 9,2),0),  // hh
                              StrToIntDef(copy(YYYYMMDDhhnnss,11,2),0),  // nn
                              StrToIntDef(copy(YYYYMMDDhhnnss,13,2),0),  // ss
                              0 );
  except
    Result := 0;
  end;
end;

{-----------------------------------------------------------------------------
  Converte um TDateTime para uma String no formato YYYYMMDD
 ---------------------------------------------------------------------------- }
function DtoS( ADate : TDateTime) : String;
begin
  Result := FormatDateTime('yyyymmdd', ADate ) ;
end ;

{-----------------------------------------------------------------------------
  Converte um TDateTime para uma String no formato YYYYMMDDhhnnss
 ---------------------------------------------------------------------------- }
function DTtoS( ADateTime : TDateTime) : String;
begin
  Result := FormatDateTime('yyyymmddhhnnss', ADateTime ) ;
end ;


function Iso8601ToDateTime(const AISODate: string): TDateTime;
var
  y, m, d, h, n, s, z: word;
begin
  y := StrToInt(Copy(AISODate, 1, 4));
  m := StrToInt(Copy(AISODate, 6, 2));
  d := StrToInt(Copy(AISODate, 9, 2));
  h := StrToIntDef(Copy(AISODate, 12, 2), 0);
  n := StrToIntDef(Copy(AISODate, 15, 2), 0);
  s := StrToIntDef(Copy(AISODate, 18, 2), 0);
  z := StrToIntDef(OnlyNumber(Copy(AISODate, 21, 3)), 0);

  Result := EncodeDateTime(y,m,d, h,n,s,z);
end;

function DateTimeToIso8601(ADate: TDateTime; const ATimeZone: string = ''): string;
const
  SDateFormat: string = 'yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z''';
begin
  Result := FormatDateTime(SDateFormat, ADate);
  if ATimeZone <> '' then
  begin ;
    // Remove the Z, in order to add the UTC_Offset to the string.
    SetLength(Result, Length(Result) - 1);
    Result := Result + ATimeZone;
  end;
end;

function BiasToTimeZone(const aBias: Integer): String;
const
  cFmt: String = '%.2d:%.2d';
var
  wOp: Char;
begin
  if (aBias = 0) then
  begin
    Result := 'Z';
    Exit;
  end;

  if (aBias > 0) then
    wOp := '-'
  else
    wOp := '+';

  Result := wOp + Format(cFmt, [Abs(aBias) div 60, Abs(aBias) mod 60]);
end;

function TimeZoneToBias(const aTimeZone: String): Integer;
var
  TMZ: String;
  M, H, Tam: Integer;
begin
  Result := -1;
  if (not StrHasTimeZone(aTimeZone)) then
    Exit;

  TMZ := UpperCase(Trim(aTimeZone));
  Tam := Length(TMZ);

  if (TMZ[Tam] = 'Z') then
  begin
    Result := 0;
    Exit;
  end;

  if (Tam > 6) then
    TMZ := RightStr(TMZ, 6);

  H  := StrToIntDef(Copy(TMZ, 2, 2), 0);
  M  := StrToIntDef(Copy(TMZ, 5, 2), 0);
  Result := ((H*60) + M);

  if (TMZ[1] = '+') then
    Result := Result*(-1);
end;

function StrIsTimeZone(const aStr: String): Boolean;
var
  wS: String;
  Tam: Integer;
begin
  wS := UpperCase(aStr);
  Tam := Length(aStr);
  Result := (Tam = 1) and (wS[1] = 'Z');

  if (not Result) and (Tam = 6) and CharInSet(wS[1], ['-', '+']) and (wS[4] = ':') then
  begin
    Delete(wS, 4, 1);
    Delete(wS, 1, 1);
    Result := StrIsNumber(wS);
  end;
end;

function StrHasTimeZone(const aStr: String): Boolean;
begin
  Result := NaoEstaVazio(aStr) and
    (StrIsTimeZone(aStr[Length(aStr)]) or
     StrIsTimeZone(RightStr(aStr, 6)));
end;

{-----------------------------------------------------------------------------
  Retorna ADateTime com hora convertida para TimeZone Universal, baseado no
  TimeZone passado por parâmetro ou no TimeZone Local. ex UTC: -03:00
 -----------------------------------------------------------------------------}
function DateTimeUniversal(const AUTC: String; const ADateTime: TDateTime ): TDateTime;
var
  TZ: String;
  DT: TDateTime;
  Bias, H, M: Integer;
begin
  DT := ADateTime;
  TZ := AUTC;

  if (DT = 0) then
    DT := Now;

  if (TZ = '') then
  begin
    TZ := synautil.TimeZone;
    Insert(':', TZ, 4);
  end
  else
  begin
    if (Length(TZ) <> 6)
        or ( not CharInSet(TZ[1], ['-','+']) )
        or ( not (TZ[4] = ':') ) then
    begin
      Result := 0;
      exit;
    end;

    H := StrToIntDef(copy(TZ,2,2), -99);
    M := StrToIntDef(copy(TZ,5,2), -99);
    if ( (H < -11) or (H > 14) )
        or ( (M < 0) or (M > 60) ) then
    begin
      Result := 0;
      exit;
    end;

  end;

  Bias := TimeZoneToBias(DateTimeToStr(DT) + TZ);
  Result := IncMinute(DT, Bias);
end;

{-----------------------------------------------------------------------------
  Retornar True, se a Data for de Segunda a Sexta-feira. Falso para Sábado e Domingo
 -----------------------------------------------------------------------------}
function IsWorkingDay(ADate: TDateTime): Boolean;
begin
  Result := (DayOfWeek(ADate) in [2..6]);
end;

{-----------------------------------------------------------------------------
  Retornar o total de dias úteis em um período de datas, exceto feriados.
 -----------------------------------------------------------------------------}
function WorkingDaysBetween(StartDate, EndDate: TDateTime): Integer;
var
  ADate: TDateTime;
begin
  Result := 0;
  if (StartDate <= 0) then
    exit;

  ADate  := IncDay(StartDate, 1);
  while (ADate <= EndDate) do
  begin
    if IsWorkingDay(ADate) then
      Inc(Result);

    ADate := IncDay(ADate, 1)
  end;
end;

{-----------------------------------------------------------------------------
  Retornar uma data calculando apenas dias úteis, a partir de uma data inicial,
  exceto feriados.
 -----------------------------------------------------------------------------}
function IncWorkingDay(ADate: TDateTime; WorkingDays: Integer): TDatetime;
var
  DaysToIncrement, WorkingDaysAdded: Integer;

  function GetNextWorkingDay(ADate: TDateTime): TDateTime;
  begin
    Result := ADate;
    while not IsWorkingDay(Result) do
      Result := IncDay(Result, DaysToIncrement);
  end;

begin
  DaysToIncrement := ifthen(WorkingDays < 0,-1,1);

  if (WorkingDays = 0) then
    Result := GetNextWorkingDay(ADate)
  else
  begin
    Result := ADate;
    WorkingDaysAdded := 0;

    while (WorkingDaysAdded <> WorkingDays) do
    begin
      Result := GetNextWorkingDay( IncDay(Result, DaysToIncrement) );
      WorkingDaysAdded := WorkingDaysAdded + DaysToIncrement;
    end;
  end;
end;

function AjustarData(const DataStr: string): string;
var
  Ano, Mes, Dia, i: Integer;
  xData: string;
begin
  xData := DataStr;

  i := Pos('/', xData);

  if i = 0 then
  begin
    Result := xData;
  end
  else
  begin
    if Length(xData) = 7 then
    begin
      if i = 5 then
        xData := xData + '/01'
      else
        xData := '01/' + xData;
    end;

    if i = 5 then
    begin
      Ano := StrToInt(Copy(xData, 1, 4));
      xData := Copy(xData, 6, Length(xData));
      i := Pos('/', xData);

      if i= 0 then
      begin
        Mes := StrToInt(xData);
        Dia := 1;
      end
      else
      begin
        Mes := StrToInt(Copy(xData, 1, i-1));
        Dia := StrToInt(Copy(xData, i+1, Length(xData)));
      end;

      Result := FormatFloat('0000', Ano) + '/' +
                FormatFloat('00', Mes) + '/' +
                FormatFloat('00', Dia);
    end
    else
    begin
      if i = 3 then
      begin
        Dia := StrToInt(Copy(xData, 1, 2));
        xData := Copy(xData, 4, Length(xData));
        i := Pos('/', xData);
        Mes := StrToInt(Copy(xData, 1, i-1));
        Ano := StrToInt(Copy(xData, i+1, Length(xData)));
      end
      else
      begin
        Dia := StrToInt(Copy(xData, 1, 1));
        xData := Copy(xData, 3, Length(xData));
        i := Pos('/', xData);
        Mes := StrToInt(Copy(xData, 1, i-1));
        Ano := StrToInt(Copy(xData, i+1, Length(xData)));
      end;

      if Ano <= 99 then
        Ano := 2000 + Ano;

      Result := FormatFloat('00', Dia) + '/' +
                FormatFloat('00', Mes) + '/' +
                FormatFloat('0000', Ano);
    end;
  end;
end;

function ParseDataHora(const DataStr: string): string;
var
  xDataHora, xData, xHora, xTZD: string;
  p: Integer;

  function ConverteNomeMes(str: string): string;
  var
    iMes: Integer;
    mesNum: string;
  const
    meses: array[1..12] of string = ('JAN','FEV','MAR','ABR','MAI','JUN','JUL','AGO','SET','OUT','NOV','DEZ');
    months: array[1..12] of string = ('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC');
  begin
    Result := str;

    if OnlyNumber(Result) = OnlyAlphaNum(Result) then
      exit;

    for iMes:=1 to 12 do
    begin
      mesNum := Format('%2.2d',[iMes]);

      Result := StringReplace(Result, meses[iMes], mesNum, [rfReplaceAll]);

      case iMes of
        2,4,5,8,9,10,12: Result := StringReplace(Result, months[iMes], mesNum, [rfReplaceAll]);
      end;
    end;
  end;

begin
  xDataHora := Trim(UpperCase(DataStr));

  xDataHora := ConverteNomeMes(xDataHora);
  xDataHora := StringReplace(xDataHora, 'Z', '', [rfReplaceAll]);

  p := Pos('T', xDataHora);

  if p = 0 then
    p := Pos(' ', xDataHora);

  if p > 0 then
    xData := Copy(xDataHora, 1, p-1)
  else
    xData := xDataHora;

  if Length(xData) > 10 then
    xData := copy(xData, 1, 10);

  xData := AjustarData(StringReplace(xData, '-', '/', [rfReplaceAll]));
  xHora := '';
  xTZD := '';

  if p > 0 then
  begin
    xDataHora := Copy(xDataHora, p+1, Length(xDataHora) - p);

    p := Pos('-', xDataHora);

    if (p = 0) then
      p := Pos('+', xDataHora);

    if (p = 0) then
       p := Pos(' ', xDataHora);

    if (p > 0) then
    begin
      xHora := Copy(xDataHora, 1, p-1);
      xTZD := Copy(xDataHora, p, Length(xDataHora));
    end
    else
      xHora := xDataHora;
  end;

  p := Pos('.', xHora);

  if (p > 0) then
    xHora := Copy(xHora, 1, p-1);

  Result := Trim(xData + ' ' + xHora + xTZD);
end;

function EncodeDataHora(const DataStr: string;
  const FormatoData: string = 'YYYY/MM/DD'): TDateTime;
var
  xData, xFormatoData: string;
begin
  xData := ParseDataHora(DataStr);

  if xData = '' then
    Result := 0
  else
  begin
    xFormatoData := FormatoData;

    if xFormatoData = '' then
      xFormatoData := 'YYYY/MM/DD';

    case Length(xData) of
      6: xData := FormatMaskText('!0000\/00;0;_', xData) + '/01';
      7: if Pos('/',xData) = 3 then
           xData := '01/' + xData
         else
           xData := xData + '/01';
      8: xData := FormatMaskText('!0000\/00\/00;0;_', xData);
    end;

    if (Pos('0000/', xData) > 0) or (Pos('/0000', xData) > 0) then
      Result := 0
    else
      Result := StringToDateTime(xData, xFormatoData);
  end;
end;

end.
