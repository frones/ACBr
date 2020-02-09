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

unit ACBrDeviceHook;

interface


uses
  Classes, SysUtils,
  ACBrDeviceClass, ACBrBase;

type
  TACBrDeviceHookAtivar = procedure(const APort: String; Params: String) of object;
  TACBrDeviceHookDesativar = procedure(const APort: String) of object;
  TACBrDeviceHookEnviaString = procedure(const cmd: AnsiString) of object;
  TACBrDeviceHookLeString = procedure(const NumBytes, ATimeOut: Integer; var Retorno: AnsiString) of object;

  { TACBrDeviceHook }

  TACBrDeviceHook = class(TACBrDeviceClass)
  private
    fsHookAtivar: TACBrDeviceHookAtivar;
    fsHookDesativar: TACBrDeviceHookDesativar;
    fsHookEnviaString: TACBrDeviceHookEnviaString;
    fsHookLeString: TACBrDeviceHookLeString;
  protected
  public
    constructor Create(AOwner: TComponent);

    procedure Conectar(const APorta: String; const ATimeOutMilissegundos: Integer); override;
    procedure Desconectar(IgnorarErros: Boolean = True); override;

    procedure EnviaString(const AString: AnsiString); override;
    function LeString(ATimeOutMilissegundos: Integer = 0; NumBytes: Integer = 0;
      const Terminador: AnsiString = ''): AnsiString; override;
    procedure Limpar; override;

    property HookAtivar: TACBrDeviceHookAtivar read fsHookAtivar write fsHookAtivar;
    property HookDesativar: TACBrDeviceHookDesativar read fsHookDesativar write fsHookDesativar;
    property HookEnviaString: TACBrDeviceHookEnviaString read fsHookEnviaString write fsHookEnviaString;
    property HookLeString: TACBrDeviceHookLeString read fsHookLeString write fsHookLeString;
  end;

implementation

uses
  DateUtils, Math, StrUtils,
  ACBrDevice;

{ TACBrDeviceHook }

constructor TACBrDeviceHook.Create(AOwner: TComponent);
begin
  inherited;
  fsHookAtivar := nil;
  fsHookDesativar := nil;
  fsHookEnviaString := nil;
  fsHookLeString := nil;
end;

procedure TACBrDeviceHook.Conectar(const APorta: String; const ATimeOutMilissegundos: Integer);
begin
  inherited;
  if Assigned(fsHookAtivar) then
    fsHookAtivar(APorta, TACBrDevice(fpOwner).DeviceToString(False));
end;

procedure TACBrDeviceHook.Desconectar(IgnorarErros: Boolean);
begin
  inherited;
  if Assigned(fsHookDesativar) then
    fsHookDesativar(fpPorta);
end;

procedure TACBrDeviceHook.EnviaString(const AString: AnsiString);
begin
  if Assigned(fsHookEnviaString) then
  begin
    GravaLog('  TACBrDeviceHook.EnviaString');
    fsHookEnviaString(AString);
  end;
end;

function TACBrDeviceHook.LeString(ATimeOutMilissegundos: Integer; NumBytes: Integer;
  const Terminador: AnsiString): AnsiString;
var
  Buffer: AnsiString;
  Fim: TDateTime;
  LenTer, LenBuf: Integer;
begin
  Result := '';
  with TACBrDevice(fpOwner) do
  begin
    if Assigned( fsHookLeString ) then
    begin
      LenTer := Length(Terminador);
      NumBytes := max(NumBytes,1);
      Fim := IncMilliSecond(Now, ATimeOutMilissegundos);
      repeat
        Buffer := '';
        fsHookLeString(NumBytes, ATimeOutMilissegundos, Buffer);
        LenBuf := Length(Buffer);
        if (LenTer > 0) and (LenBuf > 0) then
        begin
          if (RightStr(Buffer, LenTer) = Terminador) then
          begin
            SetLength(Buffer, (LenBuf-LenTer));
            NumBytes := 0; // força saida
          end;
        end;

        Result := Result + Buffer;
      until (Length(Result) >= NumBytes) or (now > Fim) ;
    end;
  end;
end;

procedure TACBrDeviceHook.Limpar;
begin
  inherited Limpar;
end;

end.



