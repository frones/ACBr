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

type
  TTimeZoneModoDeteccao = (tzSistema, tzPCN, tzManual);

  { TTimeZoneConf }

  TTimeZoneConf = class(TPersistent)
  private
    FModoDeteccao: TTimeZoneModoDeteccao;
    FTimeZoneStr: string;
    procedure SetModoDeteccao(AValue: TTimeZoneModoDeteccao);
    procedure SetTimeZoneStr(const AValue: string);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property ModoDeteccao: TTimeZoneModoDeteccao read FModoDeteccao
      write SetModoDeteccao default tzSistema;
    property TimeZoneStr: string read FTimeZoneStr write SetTimeZoneStr;
  end;

  TAjustarDataHoraParaUfFunc = function(DataHora: TDateTime; const UF: string; out TimeZoneStr: string): TDateTime;

function DateTimeWithTimeZone(DataHora: TDateTime; cUF: integer): string; overload;
function DateTimeWithTimeZone(DataHora: TDateTime; const UF: string): string; overload;
function AjustarDataHoraParaUf(DataHora: TDateTime; cUF: integer): TDateTime; overload;
function AjustarDataHoraParaUf(DataHora: TDateTime; const UF: string): TDateTime; overload;
function AjustarDataHoraParaUf(DataHora: TDateTime; cUF: integer; out TimeZoneStr: string): TDateTime; overload;
function AjustarDataHoraParaUf(DataHora: TDateTime; const UF: string; out TimeZoneStr: string): TDateTime; overload;

function DateTimeTodh(DataHora: TDateTime): string;
function TimeToDecimal(const ATime: TDateTime): Double;
function DateTimeToDataHora(DataHora: TDateTime): string;

function DateTimeTodhUTC(DataHora: TDateTime; const TZD: string): string;
function GetUTC(UF: string; const dataHora: TDateTime): string;
function GetUTCSistema: string;
function GetUTCUF(UF: string; const dataHora: TDateTime): string;
function IsHorarioDeVerao(const UF: string; const dataHora: TDateTime): Boolean;
function GetPrimeiroDomingoDoMes(const ano, mes: Integer): TDateTime;
function GetTerceiroDomingoDoMes(const ano, mes: Integer): TDateTime;
function GetInicioDoHorarioDeVerao(const ano: Integer): TDateTime;
function GetFimDoHorarioDeVerao(const ano: Integer): TDateTime;
function GetDataDoCarnaval(const ano: Integer): TDateTime;
function GetDataDaPascoa(const ano: Integer): TDateTime;

function TimeZoneConf: TTimeZoneConf;

function FormatDateBr(const ADateTime: TDateTime; AFormat: string = ''): string;
function FormatDateTimeBr(const ADate: TDateTime; AFormat: string = ''): string;
function StringToDateTime( const DateTimeString : string; const Format : string = '') : TDateTime;
function StringToDateTimeDef( const DateTimeString : string; const DefaultValue : TDateTime;
    const Format : string = '') : TDateTime;
function StoD( YYYYMMDDhhnnss: string) : TDateTime;
function DtoS( ADate : TDateTime) : string;
function DTtoS( ADateTime : TDateTime) : string;

function Iso8601ToDateTime(const AISODate: string): TDateTime;
function DateTimeToIso8601(ADate: TDateTime; const ATimeZone: string = ''): string;

{ Bias = Diferença em minutos do horário atual com o UTC }
function BiasToTimeZone(const aBias: Integer): string;
function TimeZoneToBias(const aTimeZone: string): Integer;

function StrIsTimeZone(const aStr: string): Boolean;
function StrHasTimeZone(const aStr: string): Boolean;

function DateTimeUniversal(const AUTC: string = ''; const ADateTime: TDateTime = 0 ): TDateTime;

function IsWorkingDay(ADate: TDateTime): Boolean;
function WorkingDaysBetween(StartDate, EndDate: TDateTime): Integer;
function IncWorkingDay(ADate: TDateTime; WorkingDays: Integer): TDatetime;

function EncodeDataHora(const DataStr: string;
  const FormatoData: string = 'YYYY/MM/DD'): TDateTime;
function ParseDataHora(const DataStr: string): string;
function AjustarData(const DataStr: string): string;

var
  TimeZoneConfInstance: TTimeZoneConf;
  OnAjustarDataHoraParaUf: TAjustarDataHoraParaUfFunc;

implementation

uses
  MaskUtils, synautil,
  ACBrUtil.Compatibilidade, ACBrUtil.Strings, ACBrUtil.Base;

function DateTimeWithTimeZone(DataHora: TDateTime; cUF: integer): string; overload;
begin
  Result := DateTimeWithTimeZone(DataHora, CodigoUFparaUF(cUF));
end;

function DateTimeWithTimeZone(DataHora: TDateTime; const UF: string): string; overload;
var
  TimeZoneStr: string;
begin
  DataHora := AjustarDataHoraParaUf(DataHora, UF, TimeZoneStr);
  Result := DateTimeTodhUTC(DataHora, TimeZoneStr);
end;

function AjustarDataHoraParaUf(DataHora: TDateTime; cUF: integer): TDateTime; overload;
begin
  Result := AjustarDataHoraParaUf(DataHora, CodigoUFparaUF(cUF));
end;

function AjustarDataHoraParaUf(DataHora: TDateTime; const UF: string): TDateTime; overload;
var
  TimeZoneStr: string;
begin
  Result := AjustarDataHoraParaUf(DataHora, UF, TimeZoneStr);
end;

function AjustarDataHoraParaUf(DataHora: TDateTime; cUF: integer; out TimeZoneStr: string): TDateTime; overload;
begin
  Result := AjustarDataHoraParaUf(DataHora, CodigoUFparaUF(cUF), TimeZoneStr);
end;

function AjustarDataHoraParaUf(DataHora: TDateTime; const UF: string; out TimeZoneStr: string): TDateTime; overload;
begin
  if Assigned(OnAjustarDataHoraParaUf) then
    Result := OnAjustarDataHoraParaUf(DataHora, UF, TimeZoneStr)
  else
  begin
    // Sem conversão para não quebrar o comportamento atual do ACBr
    Result := DataHora;
    TimeZoneStr := GetUTC(UF, DataHora);
  end;
end;

function DateTimeTodh(DataHora: TDateTime): string;
var
  wAno, wMes, wDia, wHor, wMin, wSeg, wMil: word;
begin
  DecodeDate(DataHora, wAno, wMes, wDia);
  DecodeTime(DataHora, wHor, wMin, wSeg, wMil);
  Result := IntToStrZero(wAno, 4) + '-' +
            IntToStrZero(wMes, 2) + '-' +
            IntToStrZero(wDia, 2) + 'T' +
            IntToStrZero(wHor, 2) + ':' +
            IntToStrZero(wMin, 2) + ':' +
            IntToStrZero(wSeg, 2);
end;

function TimeToDecimal(const ATime: TDateTime): Double;
var
  H, N, S, MS: word;
  MDec: Double;
begin
  DecodeTime(ATime, H,N,S,MS);

  MDec := N/60;
  Result := H + MDec;
end;

function DateTimeToDataHora(DataHora: TDateTime): string;
var
  wAno, wMes, wDia, wHor, wMin, wSeg, wMil: word;
begin
  DecodeDate(DataHora, wAno, wMes, wDia);
  DecodeTime(DataHora, wHor, wMin, wSeg, wMil);
  Result := IntToStrZero(wDia, 2) +
            IntToStrZero(wMes, 2) +
            IntToStrZero(wAno, 4) +
            IntToStrZero(wHor, 2) +
            IntToStrZero(wMin, 2) +
            IntToStrZero(wSeg, 2);
end;

function DateTimeTodhUTC(DataHora: TDateTime; const TZD: string): string;
var
  wAno, wMes, wDia, wHor, wMin, wSeg, wMil: word;
begin
  DecodeDate(DataHora, wAno, wMes, wDia);
  DecodeTime(DataHora, wHor, wMin, wSeg, wMil);
  Result := IntToStrZero(wAno, 4) + '-' +
            IntToStrZero(wMes, 2) + '-' +
            IntToStrZero(wDia, 2) + 'T' +
            IntToStrZero(wHor, 2) + ':' +
            IntToStrZero(wMin, 2) + ':' +
            IntToStrZero(wSeg, 2) +
            TZD;
end;

function GetUTC(UF: string; const dataHora: TDateTime): string;
begin
  case TimeZoneConf.ModoDeteccao of
    tzSistema:
      Result := GetUTCSistema;

    tzPCN:
      Result := GetUTCUF(UF, dataHora);

    tzManual:
      Result := TimeZoneConf.TimeZoneStr;
  end;
end;

function GetUTCUF(UF: string; const dataHora: TDateTime): string;
const
  UTC5 = '.AC.';
  UTC4 = '.AM.RR.RO.MT.MS.';
  UTC3 = '.AP.PA.MA.PI.TO.GO.CE.RN.PB.PE.AL.SE.BA.MG.ES.RJ.SP.PR.SC.RS.DF.';
var
  HorarioDeVerao: Boolean;
begin
  if (UF = '90') or (UF = '91') or (UF = '') then
    UF := 'DF';

  HorarioDeVerao := IsHorarioDeVerao(UF, dataHora);
  Result := '-03:00';  // TimeZone de Brasília

  if AnsiPos('.' + UF + '.', UTC4) > 0 then
  begin
    Result := '-04:00';
    if HorarioDeVerao then
      Result := '-03:00';
  end
  else
  if AnsiPos('.' + UF + '.', UTC3) > 0 then
  begin
    Result := '-03:00';
    if HorarioDeVerao then
      Result := '-02:00';
  end
  else
  if AnsiPos('.' + UF + '.', UTC5) > 0 then
  begin
    Result := '-05:00';
  end;
end;

function GetUTCSistema: string;
begin
  Result := synautil.TimeZone;
  Insert(':', Result, 4);
end;

function IsHorarioDeVerao(const UF: string; const dataHora: TDateTime): Boolean;
const
  UFHV = '.MT.MS.GO.MG.ES.RJ.SP.PR.SC.RS.DF.';
var
  dia: word;
  mes: word;
  ano: word;
  anoInicio: integer;
  anoFim: integer;
begin
  DecodeDate(dataHora, ano, mes, dia);

  { Mês de inicio do horário de verão: Outubro;
    Mês de fim do horário de verão: Fevereiro;

   * Se a data verificada for de um mês menor que outubro: Ex: 10/08/2010 (Agosto)
       O inicio do horário de verão será OUTUBRO do ano anterior (10/2009);
       O fim do horário de verão será FEVEREIRO do mesmo ano (02/2010);

   * Se a data verificada for de um mês maior ou igual outubro: Ex: 10/11/2010 (Novembro)
       O inicio do horário de verão será OUTUBRO do mesmo ano (10/2010);
       O fim do horário de verão será FEVEREIRO do ano seguinte (02/2011);      }

  anoInicio := ano;
  anoFim := ano;
  if mes < 10 then
    anoInicio := ano - 1
  else
    anoFim := ano + 1;

  Result := False;
  if (GetInicioDoHorarioDeVerao(anoInicio) <= dataHora) and
     (GetFimDoHorarioDeVerao(anoFim) >= dataHora) and
     (AnsiPos(UF, UFHV) > 0) then
    Result := True;

end;

function GetInicioDoHorarioDeVerao(const ano: Integer): TDateTime;
begin

// http://www.planalto.gov.br/ccivil_03/_Ato2019-2022/2019/Decreto/D9772.htm
// http://www.planalto.gov.br/ccivil_03/_ato2015-2018/2017/decreto/D9242.htm

  if Ano >= 2019 then
    Result := 0
  else if Ano >= 2018 then
    Result := GetPrimeiroDomingoDoMes(ano, 11)
  else
  begin
    {Até 2017, o inicio do horário de verão era no terceiro domingo do mes de outubro}
    Result := GetTerceiroDomingoDoMes(ano, 10);
  end;
end;

function GetPrimeiroDomingoDoMes(const ano, mes: Integer): TDateTime;
var
  i: integer;
begin
  {O laço vai até 7 pois até o dia 7 tem que ter passado pelo menos um domingo.}
  result := 0;
  for i := 1 to 7 do begin
    if DayOfWeek(EncodeDate(ano, mes, i)) = 1 then
     begin
       result := EncodeDate(ano, mes, i);
       break;
     end;
  end;

end;

function GetTerceiroDomingoDoMes(const ano, mes: Integer): TDateTime;
begin
  Result := GetPrimeiroDomingoDoMes(ano, mes) + 14;
end;

function GetFimDoHorarioDeVerao(const ano: Integer): TDateTime;
var
  domingoCarnaval: TDateTime;
  terceiroDomingoFevereiro: TDateTime;
begin
  // http://www.planalto.gov.br/ccivil_03/_Ato2019-2022/2019/Decreto/D9772.htm
  Result := 0;
  if ano > 2019 then
    Exit;

  domingoCarnaval := getDataDoCarnaval(ano) - 2; //Carnaval é na terça - 2 = Domingo
  terceiroDomingoFevereiro := getTerceiroDomingoDoMes(ano, 2);
  if domingoCarnaval <> terceiroDomingoFevereiro then
    result := terceiroDomingoFevereiro
  else
    result := IncDay(terceiroDomingoFevereiro, 7);
end;

function GetDataDoCarnaval(const ano: Integer): TDateTime;
var
  pascoa: TDateTime;
begin
  pascoa := getDataDaPascoa(ano);
  result := IncDay(pascoa, -47);
end;

function GetDataDaPascoa(const ano: Integer): TDateTime;
var
  x: integer;
  y: integer;
  a: integer;
  b: integer;
  c: integer;
  d: integer;
  e: integer;
  dia: word;
  mes: word;
begin
  x := 24;
  y := 5;
  a := ano MOD 19;
  b := ano MOD 4;
  c := ano MOD 7;
  d := (19 * a + x) MOD 30;
  e := (2 * b + 4 * c + 6 * d + y) MOD 7;
  if (d + e) > 9 then
   begin
    dia := (d + e - 9);
    mes := 4;
   end
  else
   begin
    dia := (d + e + 22);
    mes := 3;
   end;
  result :=  EncodeDate(ano, mes, dia);
end;

{ TTimeZoneConf }

function TimeZoneConf: TTimeZoneConf;
begin
  if not Assigned(TimeZoneConfInstance) then
    TimeZoneConfInstance := TTimeZoneConf.Create;

  Result := TimeZoneConfInstance;
end;

constructor TTimeZoneConf.Create;
begin
  inherited;

  FTimeZoneStr := '';
  FModoDeteccao := tzSistema;
end;

procedure TTimeZoneConf.Assign(Source: TPersistent);
begin
 if Source is TTimeZoneConf then
 begin
   FModoDeteccao := TTimeZoneConf(Source).ModoDeteccao;
   FTimeZoneStr  := TTimeZoneConf(Source).TimeZoneStr;
 end;
end;

procedure TTimeZoneConf.SetTimeZoneStr(const AValue: string);
var
  Hora, Minuto: Integer;
begin
  if FTimeZoneStr = AValue then Exit;

  if (FModoDeteccao <> tzManual) then
  begin
    FTimeZoneStr := '';
    Exit;
  end;

  if (Trim(AValue) = '') then
  begin
    FTimeZoneStr := GetUTCSistema;
    Exit;
  end;

  if (Length(AValue) <> 6) then
    raise Exception.Create('Tamanho de TimeZone deve ser 6. Ex: -03:00');

  if not CharInSet(AValue[1], ['-','+']) then
    raise Exception.Create('Primeiro caractere deve ser "+,-". Ex: -03:00');

  if not (AValue[4] = ':') then
    raise Exception.Create('Quarto caractere deve ser ":". Ex: -03:00');

  Hora := StrToIntDef(copy(AValue,2,2), -99);
  if ((Hora < -11) or (Hora > 14)) then
    raise Exception.Create('Hora deve estar entre -11 a +14. Ex: -03:00');

  Minuto := StrToIntDef(copy(AValue,5,2), -99);
  if ((Minuto < 0) or (Minuto > 60)) then
    raise Exception.Create('Minuto deve estar entre 0 a 59. Ex: -03:00');

  FTimeZoneStr := AValue;
end;

procedure TTimeZoneConf.SetModoDeteccao(AValue: TTimeZoneModoDeteccao);
begin
  if FModoDeteccao = AValue then Exit;
  FModoDeteccao := AValue;

  if FModoDeteccao <> tzManual then
    FTimeZoneStr := ''
  else
  begin
    if FTimeZoneStr = '' then
      FTimeZoneStr := GetUTCSistema;
  end;
end;

{-----------------------------------------------------------------------------
  Converte uma <ADateTime> para String, semelhante ao FormatDateTime,
  porém garante que o separador de Data SEMPRE será a '/'.
  Usa o padrão Brasileiro DD/MM/YYYY.
  <AFormat> pode ser especificado, para mudar a apresentação.
 ---------------------------------------------------------------------------- }
function FormatDateBr(const ADateTime: TDateTime; AFormat: string): string;
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
function FormatDateTimeBr(const ADate: TDateTime; AFormat: string): string;
Var
  {$IFDEF HAS_FORMATSETTINGS}
  FS: TFormatSettings;
  {$ELSE}
  OldDateSeparator: Char;
  OldTimeSeparator: Char;
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
function StringToDateTime(const DateTimeString: string; const Format: string): TDateTime;
Var
  AStr : string;
  DS, TS: Char;
  {$IFDEF HAS_FORMATSETTINGS}
  FS: TFormatSettings;
  DateFormat, TimeFormat: string;
  p: Integer;
  {$ELSE}
  OldShortDateFormat: string;
  {$ENDIF}

  // Remove qualquer TimeZone da String. Exemplos:
  // - '2022-02-20 02:02:55Z'      Result: '2022-02-20 02:02:55'
  // - '2022-11-11 11:11:11-03:00' Result: '2022-11-11 11:11:11'
  function RemoverTimeZone(const aDateTimeString: string): string;
  var
    wTMZ: string;
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

  function AjustarDateTimeString(const DateTimeString: string; DS, TS: Char): string;
  var
    AStr: string;
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
  OldShortDateFormat := ShortDateFormat;
  try
    if Format <> '' then
      ShortDateFormat := Format;

    DS := DateSeparator;
    TS := TimeSeparator;

    AStr := AjustarDateTimeString(DateTimeString, DS, TS);
    Result := StrToDateTime( AStr );
  finally
    ShortDateFormat := OldShortDateFormat;
  end;
  {$ENDIF}
end;

{-----------------------------------------------------------------------------
  Converte uma <DateTimeString> para TDateTime, semelhante ao StrToDateTimeDef,
  mas verifica se o seprador da Data é compativo com o S.O., efetuando a
  conversão se necessário. Se não for possivel converter, retorna <DefaultValue>
 ---------------------------------------------------------------------------- }
function StringToDateTimeDef(const DateTimeString : string;
   const DefaultValue : TDateTime; const Format : string) : TDateTime;
begin
  if EstaVazio(DateTimeString) then
     Result := DefaultValue
  else
   begin
     try
        Result := StringToDateTime( DateTimeString, Format );
     except
        Result := DefaultValue;
     end;
   end;
end;

{-----------------------------------------------------------------------------
  Converte uma String no formato YYYYMMDDhhnnss  para TDateTime
 ---------------------------------------------------------------------------- }
function StoD( YYYYMMDDhhnnss: string) : TDateTime;
begin
  YYYYMMDDhhnnss := trim( YYYYMMDDhhnnss );

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
function DtoS( ADate : TDateTime) : string;
begin
  Result := FormatDateTime('yyyymmdd', ADate );
end;

{-----------------------------------------------------------------------------
  Converte um TDateTime para uma String no formato YYYYMMDDhhnnss
 ---------------------------------------------------------------------------- }
function DTtoS( ADateTime : TDateTime) : string;
begin
  Result := FormatDateTime('yyyymmddhhnnss', ADateTime );
end;

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
  begin;
    // Remove the Z, in order to add the UTC_Offset to the string.
    SetLength(Result, Length(Result) - 1);
    Result := Result + ATimeZone;
  end;
end;

function BiasToTimeZone(const aBias: Integer): string;
const
  cFmt: string = '%.2d:%.2d';
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

function TimeZoneToBias(const aTimeZone: string): Integer;
var
  TMZ: string;
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

function StrIsTimeZone(const aStr: string): Boolean;
var
  wS: string;
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

function StrHasTimeZone(const aStr: string): Boolean;
begin
  Result := NaoEstaVazio(aStr) and
    (StrIsTimeZone(aStr[Length(aStr)]) or
     StrIsTimeZone(RightStr(aStr, 6)));
end;

{-----------------------------------------------------------------------------
  Retorna ADateTime com hora convertida para TimeZone Universal, baseado no
  TimeZone passado por parâmetro ou no TimeZone Local. ex UTC: -03:00
 -----------------------------------------------------------------------------}
function DateTimeUniversal(const AUTC: string; const ADateTime: TDateTime ): TDateTime;
var
  TZ: string;
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
