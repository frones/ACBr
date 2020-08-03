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
  ACBrLibComum, ACBrLibConfig,
  ACBrCTe, ACBrCTeDACTeRLClass, ACBrMail;

type

  { TLibCTeDM }

  TLibCTeDM = class(TDataModule)
    ACBrCTe1: TACBrCTe;
    ACBrCTeDACTeRL1: TACBrCTeDACTeRL;
    ACBrMail1: TACBrMail;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLock: TCriticalSection;
    fpLib: TACBrLib;

  public
    procedure AplicarConfiguracoes;
    procedure ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False;
                                  Protocolo: String = ''; MostrarPreview: String = '');
    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);
    procedure Travar;
    procedure Destravar;

    property Lib: TACBrLib read fpLib write fpLib;
  end;

implementation

uses
  ACBrUtil, FileUtil,
  ACBrLibCTeConfig, ACBrLibCTeBase;

{$R *.lfm}

{ TLibCTeDM }

procedure TLibCTeDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
end;

procedure TLibCTeDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;
end;

procedure TLibCTeDM.AplicarConfiguracoes;
var
  LibConfig: TLibCTeConfig;
begin
  ACBrCTe1.SSL.DescarregarCertificado;
  LibConfig := TLibCTeConfig(TACBrLibCTe(Lib).Config);
  ACBrCTe1.Configuracoes.Assign(LibConfig.CTe);

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

procedure TLibCTeDM.GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean);
begin
  if Assigned(Lib) then
    Lib.GravarLog(AMsg, NivelLog, Traduzir);
end;

procedure TLibCTeDM.ConfigurarImpressao(NomeImpressora: String = ''; GerarPDF: Boolean = False;
                                        Protocolo: String = ''; MostrarPreview: String = '');
var
  LibConfig: TLibCTeConfig;
begin
  LibConfig := TLibCTeConfig(Lib.Config);

  GravarLog('ConfigurarImpressao - Iniciado', logNormal);

   LibConfig.DACTe.Apply(ACBrCTeDACTeRL1, Lib);

   if NaoEstaVazio(NomeImpressora) then
     ACBrCTeDACTeRL1.Impressora := NomeImpressora;

   if GerarPDF and not DirectoryExists(PathWithDelim(LibConfig.DACTe.PathPDF))then
        ForceDirectories(PathWithDelim(LibConfig.DACTe.PathPDF));

   if NaoEstaVazio(NomeImpressora) then
     ACBrCTeDACTeRL1.Impressora := NomeImpressora;

   if NaoEstaVazio(MostrarPreview) then
     ACBrCTeDACTeRL1.MostraPreview := StrToBoolDef(MostrarPreview, False);

   if NaoEstaVazio(Protocolo) then
     ACBrCTeDACTeRL1.Protocolo := Protocolo
   else
     ACBrCTeDACTeRL1.Protocolo := '';

   GravarLog('ConfigurarImpressao - Feito', logNormal);
end;

procedure TLibCTeDM.Travar;
begin
  GravarLog('Travar', logParanoico);
  FLock.Acquire;
end;

procedure TLibCTeDM.Destravar;
begin
  GravarLog('Destravar', logParanoico);
  FLock.Release;
end;

end.
