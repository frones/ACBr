{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrDeviceClass;

interface

uses
  Classes, SysUtils,
  ACBrBase;

type

  { TACBrDeviceClass }

  TACBrDeviceClass = class
  private
  protected
    fpOwner: TComponent;
    fpPorta: String;

    function GetMaxSendBandwidth: Integer; virtual;
    function GetTimeOutMilissegundos: Integer; virtual;
    procedure SetMaxSendBandwidth(AValue: Integer); virtual;
    procedure SetTimeOutMilissegundos(AValue: Integer); virtual;
  public
    constructor Create(AOwner: TComponent);

    procedure Conectar(const APorta: String; const ATimeOutMilissegundos: Integer); virtual;
    procedure Desconectar(IgnorarErros: Boolean = True); virtual;

    function EmLinha(const ATimeOutMilissegundos: Integer): Boolean; virtual;
    function BytesParaLer: Integer; virtual;

    procedure EnviaString(const AString: AnsiString); virtual;
    procedure EnviaByte(const AByte: byte); virtual;
    function LeString(ATimeOutMilissegundos: Integer = 0; NumBytes: Integer = 0;
      const Terminador: AnsiString = ''): AnsiString; virtual;
    function LeByte(ATimeOutMilissegundos: Integer = 0): byte; virtual;
    procedure Limpar; virtual;

    procedure GravaLog(AString: AnsiString; Traduz: Boolean = False);
    procedure DoException(E: Exception);

    property TimeOutMilissegundos: Integer read GetTimeOutMilissegundos write SetTimeOutMilissegundos;

    property MaxSendBandwidth: Integer read GetMaxSendBandwidth write SetMaxSendBandwidth;
  end;

implementation

uses
  ACBrDevice;

{ TACBrDeviceClass }

constructor TACBrDeviceClass.Create(AOwner: TComponent);
begin
  if not (AOwner is TACBrDevice) then
    DoException(Exception.Create('Dono de TACBrDeviceClass deve ser TACBrDevice'));

  inherited Create;
  fpOwner := AOwner;
end;

procedure TACBrDeviceClass.Conectar(const APorta: String; const ATimeOutMilissegundos: Integer);
begin
  GravaLog('   ' + ClassName + '.Conectar(' + APorta + ', ' + IntToStr(ATimeOutMilissegundos) + ')');
  fpPorta := APorta;
end;

procedure TACBrDeviceClass.Desconectar(IgnorarErros: Boolean);
begin
  GravaLog('   ' + ClassName + '.Desconectar(' + BoolToStr(IgnorarErros, True) + ')');
end;

function TACBrDeviceClass.EmLinha(const ATimeOutMilissegundos: Integer): Boolean;
begin
  Result := True;
end;

function TACBrDeviceClass.BytesParaLer: Integer;
begin
  Result := 0;
end;

function TACBrDeviceClass.GetMaxSendBandwidth: Integer;
begin
  Result := 0;
end;

procedure TACBrDeviceClass.SetMaxSendBandwidth(AValue: Integer);
begin
  {}
end;

function TACBrDeviceClass.GetTimeOutMilissegundos: Integer;
begin
  Result := 100;
end;

procedure TACBrDeviceClass.SetTimeOutMilissegundos(AValue: Integer);
begin
  {}
end;

procedure TACBrDeviceClass.EnviaString(const AString: AnsiString);
begin
  {}
end;

procedure TACBrDeviceClass.EnviaByte(const AByte: byte);
begin
  EnviaString(chr(AByte));
end;

function TACBrDeviceClass.LeString(ATimeOutMilissegundos: Integer; NumBytes: Integer;
  const Terminador: AnsiString): AnsiString;
begin
  Result := '';
end;

function TACBrDeviceClass.LeByte(ATimeOutMilissegundos: Integer): byte;
var
  AStr: AnsiString;
begin
  AStr := LeString(ATimeOutMilissegundos, 1);
  if (Length(AStr) > 0) then
    Result := Ord(AStr[1])
  else
    Result := 0;
end;

procedure TACBrDeviceClass.Limpar;
var
  Buffer: AnsiString;
begin
  repeat
    Buffer := LeString(100);
  until (Length(Buffer) = 0)
end;

procedure TACBrDeviceClass.GravaLog(AString: AnsiString; Traduz: Boolean);
begin
  TACBrDevice(fpOwner).GravaLog(AString, Traduz);
end;

procedure TACBrDeviceClass.DoException(E: Exception);
begin
  if Assigned(E) then
    GravaLog(E.ClassName + ': ' + E.Message);

  raise E;
end;

end.


