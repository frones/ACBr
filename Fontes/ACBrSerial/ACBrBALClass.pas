{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Fabio Farias                           }
{                                       Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 04/10/2005: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrBAL
******************************************************************************}

{$I ACBr.inc}

unit ACBrBALClass;

interface

uses
  ACBrDevice,      {Units da ACBr}
  {$IFDEF COMPILER6_UP} Types {$ELSE} Windows {$ENDIF},
  SysUtils, Classes;

type

{ Classe generica de BALANCA, implementa um Modelo Genérico de SolicitaPeso
  e InterpretarResposta. }

{ TACBrBALClass }

TACBrBALClass = class
  private
    procedure SetAtivo(const Value: Boolean);
  protected
    fpDevice: TACBrDevice;
    fpAtivo: Boolean;
    fpModeloStr: String;
    fpUltimoPesoLido: Double;
    fpUltimaResposta: AnsiString;
    fpArqLOG: String;
    fpPosIni: Integer;
    fpPosFim: Integer;

    procedure GravaLog(const AString: AnsiString; Traduz: Boolean = True);

    function AguardarRespostaPeso(aMillisecTimeOut: Integer = 3000;
      aReenviarSolicitarPeso: Boolean = False): Double; virtual;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure Ativar; virtual;
    procedure Desativar; virtual;
    procedure SolicitarPeso; virtual;
    procedure LeSerial(MillisecTimeOut: Integer = 500); virtual;

    function LePeso(MillisecTimeOut: Integer = 3000): Double; virtual;
    function InterpretarRepostaPeso(const aResposta: AnsiString): Double; virtual;

    property ModeloStr: String  read fpModeloStr;
    property Ativo    : Boolean read fpAtivo  write SetAtivo;
    property ArqLOG   : String  read fpArqLOG write fpArqLOG;
    property PosIni   : Integer read fpPosIni write fpPosIni;
    property PosFim   : Integer read fpPosFim write fpPosFim;

    property UltimaResposta: AnsiString read fpUltimaResposta;
    property UltimoPesoLido: Double     read fpUltimoPesoLido;
end;

implementation

uses
  math, dateutils, strutils,
  ACBrBAL, ACBrUtil, ACBrConsts;

{ TACBrBALClass }

constructor TACBrBALClass.Create(AOwner: TComponent);
begin
  if not (AOwner is TACBrBAL) then
    raise Exception.create(ACBrStr('Essa Classe deve ser instanciada por TACBrBAL'));

  { Criando ponteiro interno para as Propriedade SERIAL de ACBrBAL,
    para permitir as Classes Filhas o acesso a essas propriedades do Componente}

  fpDevice := (AOwner as TACBrBAL).Device;
  fpDevice.SetDefaultValues;

  fpAtivo     := False;
  fpArqLOG    := '';
  fpModeloStr := 'Generica';
end;

destructor TACBrBALClass.Destroy;
begin
  fpDevice := Nil; { Apenas remove referencia (ponteiros internos) }

  inherited Destroy;
end;

procedure TACBrBALClass.SetAtivo(const Value: Boolean);
begin
  if Value then
    Ativar
  else
    Desativar;
end;

procedure TACBrBALClass.GravaLog(const AString: AnsiString; Traduz: Boolean);
begin
  WriteLog(fpArqLOG, AString, Traduz);
end;

function TACBrBALClass.AguardarRespostaPeso(aMillisecTimeOut: Integer;
  aReenviarSolicitarPeso: Boolean): Double;
var
  wFinal: TDateTime;
begin
  Result := -1;
  wFinal := IncMilliSecond(Now, aMillisecTimeOut);

  { Aguarda Resposta da Balança. Classes filhas podem reescrever se necessário }
  while (Result = -1) and (wFinal > Now) do
  begin
    if aReenviarSolicitarPeso then
    begin
      SolicitarPeso;
      Sleep(200);
    end;

    aMillisecTimeOut := Max(MilliSecondsBetween(Now, wFinal), 1000);

    LeSerial(aMillisecTimeOut);
    Result := fpUltimoPesoLido;
  end;
end;

procedure TACBrBALClass.Ativar;
begin
  if fpAtivo then
    Exit;

  GravaLog( sLineBreak   + StringOfChar('-',80)+ sLineBreak +
            'ATIVAR - '  + FormatDateTime('dd/mm/yy hh:nn:ss:zzz', Now) +
            ' - Modelo: '+ ModeloStr +
            ' - Porta: ' + fpDevice.Porta + '         Device: ' +
            fpDevice.DeviceToString(False) + sLineBreak +
            StringOfChar('-', 80) + sLineBreak, False);

  if (fpDevice.Porta <> '') then
    fpDevice.Ativar;

  fpAtivo          := True;
  fpUltimaResposta := '';
  fpUltimoPesoLido := 0;
end;

procedure TACBrBALClass.Desativar;
begin
  if (not fpAtivo) then
    Exit;

  if (fpDevice.Porta <> '') then
    fpDevice.Desativar;

  fpAtivo := False;
end;

procedure TACBrBALClass.SolicitarPeso;
begin
  { Envia comando Padrão para solicitar Peso }
  { As classes filhas podem reescrever se necessário }
  GravaLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' TX -> ' + #05);
  fpDevice.Limpar;
  fpDevice.EnviaString(#05);
end;

procedure TACBrBALClass.LeSerial(MillisecTimeOut: Integer);
begin
  fpUltimoPesoLido := 0;
  fpUltimaResposta := '';

  try
    fpUltimaResposta := fpDevice.LeString(MillisecTimeOut);
    GravaLog(' - ' + FormatDateTime('hh:nn:ss:zzz', Now) + ' RX <- ' + fpUltimaResposta);

    fpUltimoPesoLido := InterpretarRepostaPeso(fpUltimaResposta);
  except
    { Peso não foi recebido (TimeOut) }
    fpUltimoPesoLido := -9;
  end;

  GravaLog('              UltimoPesoLido: ' + FloatToStr(fpUltimoPesoLido) +
           ' - Resposta: ' + fpUltimaResposta);
end;

function TACBrBALClass.LePeso(MillisecTimeOut: Integer): Double;
begin
{$IFNDEF COMPILER23_UP}
  Result := -1;
{$ENDIF}

  if (fpModeloStr = 'Generica') then
  begin
    Result := AguardarRespostaPeso(MillisecTimeOut, True);
    Exit;
  end;

  SolicitarPeso;
  Sleep(200);

  LeSerial(MillisecTimeOut);
  Result := fpUltimoPesoLido;
end;

function TACBrBALClass.InterpretarRepostaPeso(const aResposta: AnsiString): Double;
var
  wResposta: AnsiString;
  wDecimais: Integer;
begin
  Result := 0;

  if (aResposta = EmptyStr) or ((fpPosIni = 0) and (fpPosFim = 0)) then
    Exit;

  wDecimais := 1000;
  wResposta := Trim(Copy(aResposta, fpPosIni, fpPosFim));

  { Ajustando o separador de Decimal corretamente }
  wResposta := StringReplace(wResposta, '.', DecimalSeparator, [rfReplaceAll]);
  wResposta := StringReplace(wResposta, ',', DecimalSeparator, [rfReplaceAll]);

  try
    { Já existe ponto decimal ? }
    if (Pos(DecimalSeparator, wResposta) > 0) then
      Result := StrToFloat(wResposta)
    else
      Result := (StrToInt(wResposta) / wDecimais);
  except
    case wResposta[1] of
      'I': Result := -1;   { Instavel }
      'N': Result := -2;   { Peso Negativo }
      'S': Result := -10;  { Sobrecarga de Peso }
    else
      Result := 0;
    end;
  end;
end;

end.
