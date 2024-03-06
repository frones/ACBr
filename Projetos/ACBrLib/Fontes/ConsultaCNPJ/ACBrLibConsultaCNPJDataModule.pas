{******************************************************************************}
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

unit ACBrLibConsultaCNPJDataModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ACBrLibComum, ACBrLibConfig, syncobjs, ACBrConsultaCNPJ;

type

  { TLibConsultaCNPJDM }

  TLibConsultaCNPJDM = class(TDataModule)
    ACBrConsultaCNPJ1: TACBrConsultaCNPJ;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

  private
    FLock: TCriticalSection;
    fpLib: TACBrLib;

  public
    procedure AplicarConfiguracoes;
    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);
    procedure Travar;
    procedure Destravar;

    property Lib: TACBrLib read fpLib write fpLib;
  end;

implementation

uses
  ACBrLibConsultaCNPJConfig, ACBrLibConsultaCNPJBase;

{$R *.lfm}

 { TLibConsultaCNPJDM }

procedure TLibConsultaCNPJDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
end;

procedure TLibConsultaCNPJDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;
end;

procedure TLibConsultaCNPJDM.AplicarConfiguracoes;
var
  pLibConfig: TLibConsultaCNPJConfig;
begin
  pLibConfig := TLibConsultaCNPJConfig(TACBrLibConsultaCNPJ(Lib).Config);

  with ACBrConsultaCNPJ1 do
  begin
    Provedor      := pLibConfig.ConsultaCNPJConfig.Provedor;
    Usuario       := pLibConfig.ConsultaCNPJConfig.Usuario;
    Senha         := pLibConfig.ConsultaCNPJConfig.Senha;
    ProxyHost     := pLibConfig.ConsultaCNPJConfig.proxyHost;
    ProxyPort     := pLibConfig.ConsultaCNPJConfig.ProxyPort;
    ProxyUser     := pLibConfig.ConsultaCNPJConfig.ProxyUser;
    ProxyPass     := pLibConfig.ConsultaCNPJConfig.ProxyPass;
  end;


end;

procedure TLibConsultaCNPJDM.GravarLog(AMsg: String; NivelLog: TNivelLog;
  Traduzir: Boolean);
begin
  if Assigned(Lib) then
    TACBrLibConsultaCNPJ(Lib).GravarLog(AMsg, NivelLog, Traduzir);
end;

procedure TLibConsultaCNPJDM.Travar;
begin
  GravarLog('Travar', logParanoico);
end;

procedure TLibConsultaCNPJDM.Destravar;
begin
  GravarLog('Destravar', logParanoico);
end;

end.

