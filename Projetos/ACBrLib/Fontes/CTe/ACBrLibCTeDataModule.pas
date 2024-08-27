{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
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

unit ACBrLibCTeDataModule;

{$mode delphi}

interface

uses
  Classes, SysUtils, syncobjs,
  ACBrLibComum, ACBrLibConfig, ACBrLibDataModule,
  ACBrCTe, ACBrCTeDACTeRLClass, ACBrMail;

type

  { TLibCTeDM }

  TLibCTeDM = class(TLibDataModule)
    ACBrCTe1: TACBrCTe;
    ACBrMail1: TACBrMail;
  private
    DACTe: TACBrCTeDACTeRL;

  public
    procedure AplicarConfiguracoes; override;
    procedure ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False;
                                  Protocolo: String = ''; MostrarPreview: String = '');
    procedure FinalizarImpressao;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, FileUtil,
  {$IFDEF Demo}pcnConversao, ACBrCTeConhecimentos, ACBrCTe.EventoClass, ACBrCTe.EnvEvento, {$ENDIF}
  ACBrLibCTeConfig, ACBrLibCTeBase;

{$R *.lfm}

{ TLibCTeDM }
procedure TLibCTeDM.AplicarConfiguracoes;
var
  LibConfig: TLibCTeConfig;
begin
  ACBrCTe1.SSL.DescarregarCertificado;
  LibConfig := TLibCTeConfig(TACBrLibCTe(Lib).Config);
  ACBrCTe1.Configuracoes.Assign(LibConfig.CTe);

{$IFDEF Demo}
  GravarLog('Modo DEMO - Forçando ambiente para Homologação', logNormal);
  ACBrCTe1.Configuracoes.WebServices.Ambiente := taHomologacao;
{$ENDIF}

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

procedure TLibCTeDM.ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False;
                                        Protocolo: String = ''; MostrarPreview: String = '');
var
  LibConfig: TLibCTeConfig;
{$IFDEF Demo}
  I: Integer;
  AConhecimento: Conhecimento;
  AEvento: TInfEventoCollectionItem;
{$ENDIF}
begin
  LibConfig := TLibCTeConfig(Lib.Config);

  GravarLog('ConfigurarImpressao - Iniciado', logNormal);

  DACTe := TACBrCTeDACTeRL.Create(nil);
  ACBrCTe1.DACTE := DACTe;

  LibConfig.DACTe.Apply(DACTe, Lib);

{$IFDEF Demo}
  for I:= 0 to ACBrCTe1.Conhecimentos.Count -1 do
  begin
    AConhecimento := ACBrCTe1.Conhecimentos.Items[I];
    AConhecimento.CTe.ide.tpAmb := taHomologacao;
  end;

  for I:= 0 to ACBrCTe1.EventoCTe.Evento.Count -1 do
  begin
    AEvento := ACBrCTe1.EventoCTe.Evento.Items[I];
    AEvento.InfEvento.tpAmb := taHomologacao;
  end;
{$ENDIF}

  if GerarPDF and not DirectoryExists(PathWithDelim(LibConfig.DACTe.PathPDF))then
    ForceDirectories(PathWithDelim(LibConfig.DACTe.PathPDF));

  if NaoEstaVazio(NomeImpressora) then
    DACTe.Impressora := NomeImpressora;

  if NaoEstaVazio(MostrarPreview) then
    DACTe.MostraPreview := StrToBoolDef(MostrarPreview, False);

  if NaoEstaVazio(Protocolo) then
    DACTe.Protocolo := Protocolo
  else
    DACTe.Protocolo := '';

  GravarLog('ConfigurarImpressao - Feito', logNormal);
end;

procedure TLibCTeDM.FinalizarImpressao;
begin
  GravarLog('FinalizarImpressao - Iniciado', logNormal);

  ACBrCTe1.DACTE := nil;
  if Assigned(DACTe) then FreeAndNil(DACTe);

  GravarLog('FinalizarImpressao - Feito', logNormal);
end;

end.
