{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
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

unit ACBrLibNFComDataModule;

{$IfDef FPC}
{$mode delphi}
{$EndIf}

interface

uses
  Classes, SysUtils, FileUtil, ACBrLibComum, ACBrLibDataModule,
  ACBrNFCom.DANFComRLClass,
  ACBrNFCom,
  ACBrMail;

type

  { TLibNFComDM }

  TLibNFComDM = class(TLibDataModule)
    ACBrMail1: TACBrMail;
    ACBrNFCom1: TACBrNFCom;
    FDANFComFortes: TACBrNFComDANFComRL;

  protected
    procedure FreeReports;

  public
    procedure AplicarConfiguracoes; override;
    procedure AplicarConfigMail;
    procedure ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False;
                                  MostrarPreview: String = '');
    procedure FinalizarImpressao;
  end;

var
  LibNFComDM: TLibNFComDM;

implementation

uses
  pcnConversao, ACBrLibConfig, ACBrLibNFComConfig, ACBrUtil.Base, ACBrUtil.FilesIO;

{$R *.lfm}

{ TLibNFComDM }

procedure TLibNFComDM.FreeReports;
begin
  ACBrNFCom1.DANFCom := nil;
  if Assigned(FDANFComFortes) then FreeAndNil(FDANFComFortes);
end;

procedure TLibNFComDM.AplicarConfiguracoes;
var
  pLibNFComConfig: TLibNFComConfig;
begin
  ACBrNFCom1.SSL.DescarregarCertificado;
  pLibNFComConfig := TLibNFComConfig(Lib.Config);
  ACBrNFCom1.Configuracoes.Assign(pLibNFComConfig.NFComConfig);
  ACBrNFCom1.DANFCom := FDANFComFortes;

  {$IFDEF Demo}
  GravarLog('Modo DEMO - Forçando ambiente para Homologação', logNormal);
  ACBrNFCom1.Configuracoes.WebServices.Ambiente := taHomologacao;
  {$ENDIF}

  AplicarConfigMail;
end;

procedure TLibNFComDM.AplicarConfigMail;
begin
  with ACBrMail1 do
  begin
    Attempts             := Lib.Config.Email.Tentativas;
    SetTLS               := Lib.Config.Email.TLS;
    DefaultCharset       := Lib.Config.Email.Codificacao;
    From                 := Lib.Config.Email.Conta;
    FromName             := Lib.Config.Email.Nome;
    SetSSL               := Lib.Config.Email.SSL;
    Host                 := Lib.Config.Email.Servidor;
    IDECharset           := Lib.Config.Email.Codificacao;
    IsHTML               := Lib.Config.Email.IsHTML;
    Password             := Lib.Config.Email.Senha;
    Port                 := IntToStr(Lib.Config.Email.Porta);
    Priority             := Lib.Config.Email.Priority;
    ReadingConfirmation  := Lib.Config.Email.Confirmacao;
    DeliveryConfirmation := Lib.Config.Email.ConfirmacaoEntrega;
    TimeOut              := Lib.Config.Email.TimeOut;
    Username             := Lib.Config.Email.Usuario;
    UseThread            := Lib.Config.Email.SegundoPlano;
  end;
end;

procedure TLibNFComDM.ConfigurarImpressao(NomeImpressora: String;
  GerarPDF: Boolean; MostrarPreview: String);
var
  LibConfig: TLibNFComConfig;
begin
  LibConfig := TLibNFComConfig(Lib.Config);

  GravarLog('ConfigurarImpressao - Iniciado', logNormal);

  FDANFComFortes := TACBrNFComDANFComRL.Create(Nil);
  ACBrNFCom1.DANFCom := FDANFComFortes;

  if GerarPDF then
  begin
    if (LibConfig.DANFComConfig.PathPDF <> '') then
      if not DirectoryExists(PathWithDelim(LibConfig.DANFComConfig.PathPDF))then
        ForceDirectories(PathWithDelim(LibConfig.DANFComConfig.PathPDF));
  end;

  LibConfig.DANFComConfig.Apply(FDANFComFortes, Lib);

  if NaoEstaVazio(NomeImpressora) then
    FDANFComFortes.Impressora := NomeImpressora;

  if NaoEstaVazio(MostrarPreview) then
    FDANFComFortes.MostraPreview := StrToBoolDef(MostrarPreview, False);

  GravarLog('ConfigurarImpressao - Feito', logNormal);
end;

procedure TLibNFComDM.FinalizarImpressao;
begin
  GravarLog('FinalizarImpressao - Iniciado', logNormal);
  FreeReports;
  GravarLog('FinalizarImpressao - Feito', logNormal);
end;

end.

