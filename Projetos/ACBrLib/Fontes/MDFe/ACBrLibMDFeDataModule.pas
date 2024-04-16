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

{$I ACBr.inc}

unit ACBrLibMDFeDataModule;

interface

uses
  Classes, SysUtils, syncobjs,
  ACBrMDFe, ACBrMDFeDAMDFeRLClass, ACBrMail,
  ACBrLibComum, ACBrLibDataModule, ACBrLibConfig;

type

  { TLibMDFeDM }

  TLibMDFeDM = class(TLibDataModule)
    ACBrMail1: TACBrMail;
    ACBrMDFe1: TACBrMDFe;
  private
    fpLib: TACBrLib;
    DAMDFe: TACBrMDFeDAMDFeRL;

  public
    procedure AplicarConfiguracoes; override;
    procedure AplicarConfigMail;
    procedure ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False;
                                  Protocolo: String = ''; MostrarPreview: String = '');
    procedure FinalizarImpressao;

  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, FileUtil,
{$IFDEF Demo}ACBrMDFeManifestos, pmdfeEnvEventoMDFe, pcnConversao,{$ENDIF}
  ACBrLibMDFeConfig, ACBrLibMDFeBase;

{$R *.lfm}

{ TLibMDFeDM }
procedure TLibMDFeDM.AplicarConfiguracoes;
var
  LibConfig: TLibMDFeConfig;
begin
  ACBrMDFe1.SSL.DescarregarCertificado;
  LibConfig := TLibMDFeConfig(TACBrLibMDFe(Lib).Config);
  ACBrMDFe1.Configuracoes.Assign(LibConfig.MDFe);

{$IFDEF Demo}
  GravarLog('Modo DEMO - Forçando ambiente para Homologação', logNormal);
  ACBrMDFe1.Configuracoes.WebServices.Ambiente := taHomologacao;
{$ENDIF}

  AplicarConfigMail;
end;

procedure TLibMDFeDM.AplicarConfigMail;
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

procedure TLibMDFeDM.ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False;
                                         Protocolo: String = ''; MostrarPreview: String = '');
{$IFDEF Demo}
Var
  I: Integer;
  AItem: TManifesto;
  AEvento: TInfEventoCollectionItem;
{$ENDIF}
begin
  GravarLog('ConfigurarImpressao - Iniciado', logNormal);

  DAMDFe := TACBrMDFeDAMDFeRL.Create(nil);
  ACBrMDFe1.DAMDFE := DAMDFe;

  if ACBrMDFe1.Manifestos.Count > 0 then
  begin
    if (ACBrMDFe1.Manifestos.Items[0].MDFe.procMDFe.cStat in [101, 151, 155]) then
      ACBrMDFe1.DAMDFe.Cancelada := True
    else
      ACBrMDFe1.DAMDFe.Cancelada := False;
  end;

  TLibMDFeConfig(Lib.Config).DAMDFe.Apply(DAMDFe, Lib);

{$IFDEF Demo}
    for I:= 0 to ACBrMDFe1.Manifestos.Count -1 do
    begin
      AItem := ACBrMDFe1.Manifestos.Items[I];
      AItem.MDFe.Ide.tpAmb := taHomologacao;
    end;

    for I:= 0 to ACBrMDFe1.EventoMDFe.Evento.Count -1 do
    begin
      AEvento := ACBrMDFe1.EventoMDFe.Evento.Items[I];
      AEvento.InfEvento.tpAmb := taHomologacao;
    end;
{$ENDIF}

  if NaoEstaVazio(NomeImpressora) then
    ACBrMDFe1.DAMDFe.Impressora := NomeImpressora;

  if NaoEstaVazio(MostrarPreview) then
    ACBrMDFe1.DAMDFe.MostraPreview := StrToBoolDef(MostrarPreview, False);

  if NaoEstaVazio(Protocolo) then
    ACBrMDFe1.DAMDFe.Protocolo := Protocolo
  else
    ACBrMDFe1.DAMDFe.Protocolo := '';

  if GerarPDF and not DirectoryExists(PathWithDelim(TLibMDFeConfig(Lib.Config).DAMDFe.PathPDF))then
        ForceDirectories(PathWithDelim(TLibMDFeConfig(Lib.Config).DAMDFe.PathPDF));

  GravarLog('ConfigurarImpressao - Feito', logNormal);
end;

procedure TLibMDFeDM.FinalizarImpressao;
begin
  GravarLog('FinalizarImpressao - Iniciado', logNormal);

  ACBrMDFe1.DAMDFE := nil;
  if Assigned(DAMDFe) then FreeAndNil(DAMDFe);

  GravarLog('FinalizarImpressao - Feito', logNormal);
end;

end.
