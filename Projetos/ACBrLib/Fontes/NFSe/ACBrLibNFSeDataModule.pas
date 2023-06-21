{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
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

unit ACBrLibNFSeDataModule;

interface

uses
  Classes, SysUtils, ACBrNFSeX, ACBrMail, ACBrNFSeXDANFSeRLClass,
  ACBrLibDataModule;

type

  { TLibNFSeDM }

  TLibNFSeDM = class(TLibDataModule)
    ACBrMail1: TACBrMail;
    ACBrNFSeX1: TACBrNFSeX;
    ACBrNFSeXDANFSeRL1: TACBrNFSeXDANFSeRL;
  private
    procedure AplicarConfigMail;

  public
    procedure AplicarConfiguracoes; override;
    procedure ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False;
                                  MostrarPreview: String = ''; Cancelada: String = '');

    procedure FinalizarImpressao;
  end;

implementation

uses
  pcnConversao, ACBrLibConfig, ACBrNFSeLibConfig, ACBrUtil.Base, ACBrUtil.FilesIO;

{$R *.lfm}

{ TLibNFSeDM }
procedure TLibNFSeDM.AplicarConfiguracoes;
var
  pLibConfig: TLibNFSeConfig;
begin
  ACBrNFSeX1.SSL.DescarregarCertificado;
  pLibConfig := TLibNFSeConfig(Lib.Config);
  ACBrNFSeX1.Configuracoes.Assign(pLibConfig.NFSe);
  ACBrNFSeX1.Configuracoes.WebServices.LerParams;

{$IFDEF Demo}
  ACBrNFSeX1.Configuracoes.WebServices.Ambiente := taHomologacao;
{$ENDIF}

  AplicarConfigMail;
end;

procedure TLibNFSeDM.AplicarConfigMail;
begin
  with ACBrMail1 do
  begin
    Attempts := Lib.Config.Email.Tentativas;
    SetTLS := Lib.Config.Email.TLS;
    DefaultCharset := Lib.Config.Email.Codificacao;
    From := Lib.Config.Email.Conta;
    FromName := Lib.Config.Email.Nome;
    SetSSL := Lib.Config.Email.SSL;
    Host := Lib.Config.Email.Servidor;
    IDECharset := Lib.Config.Email.Codificacao;
    IsHTML := Lib.Config.Email.IsHTML;
    Password := Lib.Config.Email.Senha;
    Port := IntToStr(Lib.Config.Email.Porta);
    Priority := Lib.Config.Email.Priority;
    ReadingConfirmation := Lib.Config.Email.Confirmacao;
    DeliveryConfirmation := Lib.Config.Email.ConfirmacaoEntrega;
    TimeOut := Lib.Config.Email.TimeOut;
    Username := Lib.Config.Email.Usuario;
    UseThread := Lib.Config.Email.SegundoPlano;
  end;
end;

procedure TLibNFSeDM.ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False;
                                         MostrarPreview: String = ''; Cancelada: String = '');
var
  LibConfig: TLibNFSeConfig;
begin
  LibConfig := TLibNFSeConfig(Lib.Config);

  GravarLog('ConfigurarImpressao - Iniciado', logNormal);

  ACBrNFSeXDANFSeRL1 := TACBrNFSeXDANFSeRL.Create(nil);
  ACBrNFSeX1.DANFSE := ACBrNFSeXDANFSeRL1;

   if GerarPDF then
  begin
    if (LibConfig.DANFSe.PathPDF <> '') then
      if not DirectoryExists(PathWithDelim(LibConfig.DANFSe.PathPDF))then
        ForceDirectories(PathWithDelim(LibConfig.DANFSe.PathPDF));
  end;

  LibConfig.DANFSe.Apply(ACBrNFSeXDANFSeRL1, Lib);

  if NaoEstaVazio(NomeImpressora) then
    ACBrNFSeXDANFSeRL1.Impressora := NomeImpressora;

  if NaoEstaVazio(MostrarPreview) then
    ACBrNFSeXDANFSeRL1.MostraPreview := StrToBoolDef(MostrarPreview, False);

  if NaoEstaVazio(Cancelada) then
    ACBrNFSeXDANFSeRL1.Cancelada := StrToBoolDef(Cancelada, False);

  GravarLog('ConfigurarImpressao - Feito', logNormal);
end;

procedure TLibNFSeDM.FinalizarImpressao();
begin
   GravarLog('FinalizarImpressao - Iniciado', logNormal);

   ACBrNFSeX1.DANFSE := nil;
   if Assigned(ACBrNFSeXDANFSeRL1) then FreeAndNil(ACBrNFSeXDANFSeRL1);

   GravarLog('FinalizarImpressao - Feito', logNormal);
end;

end.

