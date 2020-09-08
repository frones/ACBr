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

unit ACBrLibBoletoDataModule;

interface

uses
  Classes, SysUtils, SyncObjs, ACBrBoleto, ACBrBoletoFCFortesFr, ACBrLibComum,
  ACBrLibConfig, ACBrMail;

type

  { TLibBoletoDM }

  TLibBoletoDM = class(TDataModule)
    ACBrBoleto1: TACBrBoleto;
    ACBrBoletoFCFortes1: TACBrBoletoFCFortes;
    ACBrMail1: TACBrMail;

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
    procedure AplicarConfigMail;

    property Lib: TACBrLib read fpLib write fpLib;

  end;

var
  LibBoletoDM: TLibBoletoDM;

implementation

uses
  ACBrUtil, FileUtil, ACBrLibBoletoConfig;

{$R *.lfm}

{ TLibBoletoDM }

procedure TLibBoletoDM.DataModuleCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
end;

procedure TLibBoletoDM.DataModuleDestroy(Sender: TObject);
begin
  FLock.Destroy;
end;

procedure TLibBoletoDM.AplicarConfiguracoes;
var
  LibConfig: TLibBoletoConfig;
begin
  LibConfig := TLibBoletoConfig(Lib.Config);

  with ACBrBoleto1 do
  begin
    DataArquivo := LibConfig.BoletoDiretorioConfig.DataArquivo;
    DataCreditoLanc := LibConfig.BoletoDiretorioConfig.DataCreditoLanc;
    DirArqRemessa := LibConfig.BoletoDiretorioConfig.DirArqRemessa;
    DirArqRetorno := LibConfig.BoletoDiretorioConfig.DirArqRetorno;
    Homologacao := LibConfig.BoletoDiretorioConfig.DirHomologacao;
    ImprimirMensagemPadrao := LibConfig.BoletoDiretorioConfig.ImprimirMensagemPadrao;
    LayoutRemessa := LibConfig.BoletoDiretorioConfig.LayoutRemessa;
    LeCedenteRetorno := LibConfig.BoletoDiretorioConfig.LeCedenteRetorno;
    NomeArqRemessa := LibConfig.BoletoDiretorioConfig.NomeArqRemessa;
    NomeArqRetorno := LibConfig.BoletoDiretorioConfig.NomeArqRetorno;
    NumeroArquivo := LibConfig.BoletoDiretorioConfig.NumeroArquivo;
    RemoveAcentosArqRemessa := LibConfig.BoletoDiretorioConfig.RemoveAcentosArqRemessa;

    with ACBrBoleto1.Banco do
    begin
      LayoutVersaoArquivo := LibConfig.BoletoBancoConfig.LayoutVersaoArquivo;
      LayoutVersaoLote := LibConfig.BoletoBancoConfig.LayoutVersaoLote;
      Digito := LibConfig.BoletoBancoConfig.Digito;
      LocalPagamento := LibConfig.BoletoBancoConfig.LocalPagamento;
      Numero := LibConfig.BoletoBancoConfig.Numero;
      NumeroCorrespondente := LibConfig.BoletoBancoConfig.NumeroCorrespondente;
      OrientacoesBanco.Text := LibConfig.BoletoBancoConfig.OrientacaoBanco;
      TipoCobranca := LibConfig.BoletoBancoConfig.TipoCobranca;
    end;

    with ACBrBoleto1.Cedente do
    begin
      TipoCarteira := LibConfig.BoletoCedenteConfig.TipoCarteira;
      TipoDocumento := LibConfig.BoletoCedenteConfig.TipoDocumento;
      TipoInscricao := LibConfig.BoletoCedenteConfig.TipoInscricao;
      Agencia := LibConfig.BoletoCedenteConfig.Agencia;
      AgenciaDigito := LibConfig.BoletoCedenteConfig.AgenciaDigito;
      Bairro := LibConfig.BoletoCedenteConfig.Bairro;
      CaracTitulo := LibConfig.BoletoCedenteConfig.CaracTitulo;
      CEP := LibConfig.BoletoCedenteConfig.CEP;
      Cidade := LibConfig.BoletoCedenteConfig.Cidade;
      CNPJCPF := LibConfig.BoletoCedenteConfig.CNPJCPF;
      CodigoCedente := LibConfig.BoletoCedenteConfig.CodigoCedente;
      CodigoTransmissao := LibConfig.BoletoCedenteConfig.CodigoTransmissao;
      Complemento := LibConfig.BoletoCedenteConfig.Complemento;
      Conta := LibConfig.BoletoCedenteConfig.Conta;
      ContaDigito := LibConfig.BoletoCedenteConfig.ContaDigito;
      Convenio := LibConfig.BoletoCedenteConfig.Convenio;
      Logradouro := LibConfig.BoletoCedenteConfig.Logradouro;
      Modalidade := LibConfig.BoletoCedenteConfig.Modalidade;
      Nome := LibConfig.BoletoCedenteConfig.Nome;
      NumeroRes := LibConfig.BoletoCedenteConfig.NumeroRes;
      ResponEmissao := LibConfig.BoletoCedenteConfig.ResponEmissao;
      Telefone := LibConfig.BoletoCedenteConfig.Telefone;
      UF := LibConfig.BoletoCedenteConfig.UF;
      DigitoVerificadorAgenciaConta := LibConfig.BoletoCedenteConfig.DigitoVerificadorAgenciaConta;
    end;
  end;

  with ACBrBoletoFCFortes1 do
  begin
     DirLogo := LibConfig.BoletoFCFortesConfig.DirLogo;
     Filtro := LibConfig.BoletoFCFortesConfig.Filtro;
     Layout := LibConfig.BoletoFCFortesConfig.Layout;
     MostrarPreview := LibConfig.BoletoFCFortesConfig.MostrarPreview;
     MostrarProgresso := LibConfig.BoletoFCFortesConfig.MostrarProgresso;
     MostrarSetup := LibConfig.BoletoFCFortesConfig.MostrarSetup;
     NomeArquivo := LibConfig.BoletoFCFortesConfig.NomeArquivo;
     NumCopias := LibConfig.BoletoFCFortesConfig.NumeroCopias;
     PrinterName := LibConfig.BoletoFCFortesConfig.PrinterName;
     SoftwareHouse := LibConfig.BoletoFCFortesConfig.SoftwareHouse;
  end;

  AplicarConfigMail;

end;

procedure TLibBoletoDM.GravarLog(AMsg: String; NivelLog: TNivelLog;
  Traduzir: Boolean);
begin
  if Assigned(Lib) then
    Lib.GravarLog(AMsg, NivelLog, Traduzir);
end;

procedure TLibBoletoDM.Travar;
begin
  GravarLog('Travar', logParanoico);
  FLock.Acquire;
end;

procedure TLibBoletoDM.Destravar;
begin
  GravarLog('Destravar', logParanoico);
  FLock.Release;
end;

procedure TLibBoletoDM.AplicarConfigMail;
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

{ TLibBoletoDM }

end.

