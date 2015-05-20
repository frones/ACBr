{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - NFe - http://www.nfe.fazenda.gov.br            }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{*******************************************************************************
|* Historico
|*
*******************************************************************************}

{$I ACBr.inc}

unit ACBrNFeDAInutQRRetrato;

// Atenção todos os comiters
// Quando enviar os fontes referentes ao DAEvento favor alterar
// a data e o nome da linha abaixo.
// Última liberação:

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, QuickRpt, QRCtrls, XMLIntf, XMLDoc,
  JPEG, ACBrDFeQRCodeBar, pcnConversao, DB,
  DBClient, ACBrNFeDAInutQR;

type
  TfrmNFeDAInutQRRetrato = class(TfrmNFeDAInutQR)
    qrb_01_Titulo: TQRBand;
    qrlProtocolo: TQRLabel;
    qrlOrgao: TQRLabel;
    qrlDescricao: TQRLabel;
    QRLabel2: TQRLabel;
    QRLabel78: TQRLabel;
    qrlModelo: TQRLabel;
    qrb_07_Rodape: TQRBand;
    qrb_03_Inutilizacao: TQRChildBand;
    qrsQuadro01: TQRShape;
    qrsLinhaV10: TQRShape;
    qrsLinhaV09: TQRShape;
    qrsLinhaH04: TQRShape;
    qrsLinhaV01: TQRShape;
    QRShape46: TQRShape;
    qrlLinha3: TQRLabel;
    qrlLinha2: TQRLabel;
    qrlLinha1: TQRLabel;
    QRShape88: TQRShape;
    qrlTituloEvento: TQRLabel;
    QRShape48: TQRShape;
    QRLabel9: TQRLabel;
    qrlTipoAmbiente: TQRLabel;
    QRLabel6: TQRLabel;
    qrlSerie: TQRLabel;
    QRLabel28: TQRLabel;
    qrlAno: TQRLabel;
    QRLabel17: TQRLabel;
    qrlNumeracao: TQRLabel;
    QRShape49: TQRShape;
    QRShape50: TQRShape;
    QRLabel18: TQRLabel;
    qrlStatus: TQRLabel;
    cdsCorrecao: TClientDataSet;
    cdsCorrecaoItem: TIntegerField;
    cdsCorrecaoGrupo: TStringField;
    cdsCorrecaoCampo: TStringField;
    cdsCorrecaoValor: TStringField;
    qrb_05_NaoUsado_Detalhe: TQRBand;
    qrb_02_Emitente: TQRBand;
    qrsQuadro02: TQRShape;
    qrsLinhaH07: TQRShape;
    qrsLinhaH06: TQRShape;
    qrlRazaoEmitente: TQRLabel;
    qrlMunEmitente: TQRLabel;
    qrlInscEstEmitente: TQRLabel;
    qrlEnderecoEmitente: TQRLabel;
    qrlCNPJEmitente: TQRLabel;
    qrlCEPEmitente: TQRLabel;
    QRLabel98: TQRLabel;
    QRLabel93: TQRLabel;
    QRLabel24: TQRLabel;
    QRLabel22: TQRLabel;
    QRLabel16: TQRLabel;
    QRLabel13: TQRLabel;
    QRLabel12: TQRLabel;
    QRShape51: TQRShape;
    QRShape53: TQRShape;
    QRShape82: TQRShape;
    QRShape99: TQRShape;
    QRLabel4: TQRLabel;
    qrlBairroEmitente: TQRLabel;
    QRShape108: TQRShape;
    QRLabel5: TQRLabel;
    qrlFoneEmitente: TQRLabel;
    QRShape109: TQRShape;
    QRSysData1: TQRSysData;
    qrlblSistema: TQRLabel;
    QRShape1: TQRShape;
    qrb_04_NaoUsado: TQRBand;
    qrb_06_NaoUsado_Summary: TQRBand;
    QRLabel15: TQRLabel;
    QRShape2: TQRShape;
    QRLabel1: TQRLabel;
    qrlJustificativa: TQRLabel;
    procedure QRInutBeforePrint(Sender: TCustomQuickRep; var PrintReport: Boolean);
    procedure qrb_02_EmitenteBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_03_InutilizacaoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_05_NaoUsado_DetalheBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_06_NaoUsado_SummaryBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_07_RodapeBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrdbtxtValorPrint(sender: TObject; var Value: String);
  private
    procedure Itens;
  public
    procedure ProtocoloNFe(const sProtocolo: String);
  end;

implementation

uses
  StrUtils, DateUtils,
  ACBrDFeUtil;

{$R *.dfm}

var
  FProtocoloNFe : String;

procedure TfrmNFeDAInutQRRetrato.Itens;
var
  i: Integer;
begin
 // Itens
 (*
  if ( cdsCorrecao.Active ) then
  begin
    cdsCorrecao.CancelUpdates;
  end
  else
  begin
    cdsCorrecao.CreateDataSet;
  end;

  for i := 0 to (FEventoNFe.InfEvento.detEvento.infCorrecao.Count -1) do
  begin
    cdsCorrecao.Append;
    cdsCorrecaoItem.AsInteger := FEventoNFe.InfEvento.detEvento.infCorrecao[i].nroItemAlterado;
    cdsCorrecaoGrupo.AsString := FEventoNFe.InfEvento.detEvento.infCorrecao[i].grupoAlterado;
    cdsCorrecaoCampo.AsString := FEventoNFe.InfEvento.detEvento.infCorrecao[i].campoAlterado;
    cdsCorrecaoValor.AsString := FEventoNFe.InfEvento.detEvento.infCorrecao[i].valorAlterado;
    cdsCorrecao.Post;
  end;
  *)
end;

procedure TfrmNFeDAInutQRRetrato.ProtocoloNFe(const sProtocolo: String);
begin
  FProtocoloNFe := sProtocolo;
end;

procedure TfrmNFeDAInutQRRetrato.QRInutBeforePrint(Sender: TCustomQuickRep; var PrintReport: Boolean);
begin
  inherited;

  Itens;

  QRNFeInut.ReportTitle := 'Inutilização';

  QRNFeInut.Page.TopMargin    := FMargemSuperior * 100;
  QRNFeInut.Page.BottomMargin := FMargemInferior * 100;
  QRNFeInut.Page.LeftMargin   := FMargemEsquerda * 100;
  QRNFeInut.Page.RightMargin  := FMargemDireita  * 100;

end;

procedure TfrmNFeDAInutQRRetrato.qrb_02_EmitenteBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  PrintBand := False;

 (*
  if FNFe <> nil
   then begin
    PrintBand := True;

    qrlRazaoEmitente.Caption    := FNFe.emit.xNome;
    qrlCNPJEmitente.Caption     := FormatarCNPJCPF(FNFe.emit.CNPJ);
    qrlEnderecoEmitente.Caption := FNFe.emit.EnderEmit.xLgr + ', ' + FNFe.emit.EnderEmit.nro;
    qrlBairroEmitente.Caption   := FNFe.emit.EnderEmit.xBairro;
    qrlCEPEmitente.Caption      := FormatarCEP(FormatFloat('00000000', FNFe.emit.EnderEmit.CEP));
    qrlMunEmitente.Caption      := FNFe.emit.EnderEmit.xMun + ' - ' + FNFe.emit.EnderEmit.UF;
    qrlFoneEmitente.Caption     := FormatarFone(FNFe.emit.enderEmit.fone);
    qrlInscEstEmitente.Caption  := FNFe.emit.IE;
   end;
   *)
end;

procedure TfrmNFeDAInutQRRetrato.qrb_03_InutilizacaoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  with FACBrNFe.InutNFe.RetInutNFe do
    begin
      qrlOrgao.Caption := IntToStr(cUF);

      case tpAmb of
       taProducao:    qrlTipoAmbiente.Caption := 'PRODUÇÃO';
       taHomologacao: qrlTipoAmbiente.Caption := 'HOMOLOGAÇÃO - SEM VALOR FISCAL';
      end;

      qrlAno.Caption       := IntToStr(ano);
      qrlModelo.Caption    := IntToStr(Modelo);
      qrlSerie.Caption     := IntToStr(Serie);
      qrlNumeracao.Caption := IntToStr(nNFIni) + ' a ' + IntToStr(nNFFin);

      qrlStatus.Caption    := IntToStr(cStat) + ' - ' + xMotivo;
      qrlProtocolo.Caption := nProt + ' ' +
                              FormatDateTime('dd/mm/yyyy hh:nn', dhRecbto);

      qrlJustificativa.Caption := xJust;
    end;
end;

procedure TfrmNFeDAInutQRRetrato.qrb_05_NaoUsado_DetalheBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;
//  PrintBand := True;
end;

procedure TfrmNFeDAInutQRRetrato.qrb_06_NaoUsado_SummaryBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
//  PrintBand := True;
end;

procedure TfrmNFeDAInutQRRetrato.qrb_07_RodapeBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  qrlblSistema.Caption := FSistema + ' - ' + FUsuario;
end;

procedure TfrmNFeDAInutQRRetrato.qrdbtxtValorPrint(sender: TObject;
  var Value: String);
var
  vLength: Integer;
begin
  inherited;
  (*
  vLength := 11 * ((Length(Value) div 90) + 1);

  qrb_08_Correcao_Detalhe.Height := vLength;

  qrdbtxtValor.Height := vLength;
  QRShape11.Height    := vLength;
  QRShape3.Height     := vLength;
  QRShape6.Height     := vLength;
  QRShape4.Height     := vLength;
  *)
end;

end.

