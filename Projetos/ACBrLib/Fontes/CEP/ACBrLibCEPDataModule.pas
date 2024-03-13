{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrLibCEPDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, ACBrLibComum, ACBrLibConfig, syncobjs, ACBrCEP, ACBrUtil.FilesIO;

type

  { TLibCEPDM }

  TLibCEPDM = class(TDataModule)
    ACBrCEP1: TACBrCEP;

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
  ACBrLibCEPConfig, ACBrLibCEPBase;

{$R *.lfm}

{ TLibCEPDM }

procedure TLibCEPDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
end;

procedure TLibCEPDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;
end;

procedure TLibCEPDM.AplicarConfiguracoes;
var
  pLibConfig: TLibCEPConfig;
begin
  pLibConfig := TLibCEPConfig(TACBrLibCEP(Lib).Config);

  with ACBrCEP1 do
  begin
    WebService    := pLibConfig.CEPConfig.WebService;
    ChaveAcesso   := pLibConfig.CEPConfig.ChaveAcesso;
    Usuario       := pLibConfig.CEPConfig.Usuario;
    Senha         := pLibConfig.CEPConfig.Senha;
    PesquisarIBGE := pLibConfig.CEPConfig.PesquisarIBGE;
  end;
end;

procedure TLibCEPDM.GravarLog(AMsg: String; NivelLog: TNivelLog;
  Traduzir: Boolean);
begin
  if Assigned(Lib) then
    TACBrLibCEP(Lib).GravarLog(AMsg, NivelLog, Traduzir);
end;

procedure TLibCEPDM.Travar;
begin
  GravarLog('Travar', logParanoico);
  FLock.Acquire;
end;

procedure TLibCEPDM.Destravar;
begin
  GravarLog('Destravar', logParanoico);
  FLock.Release;
end;

end.

