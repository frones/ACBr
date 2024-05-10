{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

unit ACBrLibAbecsPinpadConfig;

interface

uses
  Classes, SysUtils, IniFiles, synachar,
  ACBrLibConfig, ACBrDeviceConfig, ACBrAbecsPinPad;

type

  {TAbecsPinpadConfig}
  TAbecsPinpadConfig = class
    private
      FPort: String;
      FTimeOut: Integer;
      FLogFile: String;
      FMsgAlign: TACBrAbecsMsgAlign;
      FMsgWordWrap: Boolean;
    public
      constructor Create;
      destructor Destroy; override;
      procedure LerIni(const AIni: TCustomIniFile);
      procedure GravarIni(const AIni: TCustomIniFile);

      property Port: String read FPort write FPort;
      property TimeOut: Integer read FTimeOut write FTimeOut;
      property LogFile: String read FLogFile write FLogFile;
      property MsgAlign: TACBrAbecsMsgAlign read FMsgAlign write FMsgAlign;
      property MsgWordWrap: Boolean read FMsgWordWrap write FMsgWordWrap;
  end;

  { TLibAbecsPinpadConfig }
  TLibAbecsPinpadConfig = class(TLibConfig)
    private
      FAbecsPinpadConfig: TAbecsPinpadConfig;
      FDeviceConfig: TDeviceConfig;

    protected
      procedure INIParaClasse; override;
      procedure ClasseParaINI; override;
      procedure ClasseParaComponentes; override;

      procedure Travar; override;
      procedure Destravar; override;

    public
      constructor Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = ''); override;
      destructor Destroy; override;

      property AbecsPinpadConfig: TAbecsPinpadConfig read FAbecsPinpadConfig;
      property AbecsPinpadDeviceConfig: TDeviceConfig read FDeviceConfig;
  end;

implementation

uses
  ACBrLibConsts, ACBrLibAbecsPinpadConsts,
  ACBrLibAbecsPinpadBase, ACBrUtil.FilesIO, ACBrUtil.Strings;

{ TAbecsPinpadConfig }

constructor TAbecsPinpadConfig.Create;
begin
  FPort := '';
  FTimeOut := 10000;
  FLogFile := '';
  FMsgAlign := alCenter;
  FMsgWordWrap := True;
end;

destructor TAbecsPinpadConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TAbecsPinpadConfig.LerIni(const AIni: TCustomIniFile);
begin
  FPort := AIni.ReadString(CSessaoAbecsPinpad, CChavePortaPinpad, FPort);
  FTimeOut :=  AIni.ReadInteger(CSessaoAbecsPinpad, CChaveTimeOutPinpad, FTimeOut);
  FLogFile := AIni.ReadString(CSessaoAbecsPinpad, CChaveLogFile, FLogFile);
  FMsgAlign := TACBrAbecsMsgAlign(AIni.ReadInteger(CSessaoAbecsPinpad, CChaveMsgAlign, Integer(FMsgAlign)));
  FMsgWordWrap := AIni.ReadBool(CSessaoAbecsPinpad, CChaveMsgWordWrap, FMsgWordWrap);
end;

procedure TAbecsPinpadConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteString(CSessaoAbecsPinpad, CChavePortaPinpad, FPort);
  AIni.WriteInteger(CSessaoAbecsPinpad, CChaveTimeOutPinpad, FTimeOut);
  AIni.WriteString(CSessaoAbecsPinpad, CChaveLogFile, FLogFile);
  AIni.WriteInteger(CSessaoAbecsPinpad, CChaveMsgAlign, Integer(FMsgAlign));
  AIni.WriteBool(CSessaoAbecsPinpad, CChaveMsgWordWrap, FMsgWordWrap);
end;

{ TLibAbecsPinpadConfig }
constructor TLibAbecsPinpadConfig.Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = '');
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FAbecsPinpadConfig := TAbecsPinpadConfig.Create;
  FDeviceConfig := TDeviceConfig.Create(CSessaoAbecsPinpadDevice);
end;

destructor TLibAbecsPinpadConfig.Destroy;
begin
  FAbecsPinpadConfig.Free;
  FDeviceConfig.Free;

  inherited Destroy;
end;

procedure TLibAbecsPinpadConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FAbecsPinpadConfig.LerIni(Ini);
  FDeviceConfig.LerIni(Ini);
end;

procedure TLibAbecsPinpadConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  FAbecsPinpadConfig.GravarIni(Ini);
  FDeviceConfig.GravarIni(Ini);
end;

procedure TLibAbecsPinpadConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
  TACBrLibAbecsPinpad(Owner).AbecsPinpadDM.AplicarConfiguracoes;
end;

procedure TLibAbecsPinpadConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibAbecsPinpad(Owner) do
         AbecsPinpadDM.Travar;
  end;
end;

procedure TLibAbecsPinpadConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibAbecsPinpad(Owner) do
         AbecsPinpadDM.Destravar;
  end;
end;

end.

