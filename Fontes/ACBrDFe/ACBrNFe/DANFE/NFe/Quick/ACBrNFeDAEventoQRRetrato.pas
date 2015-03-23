{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }
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

unit ACBrNFeDAEventoQRRetrato;

// Atenção todos os comiters
// Quando enviar os fontes referentes ao DAEvento favor alterar
// a data e o nome da linha abaixo.
// Última liberação:
// 27/11/2013 por Italo Jurisato Junior

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, QuickRpt, QRCtrls, XMLIntf, XMLDoc,
  JPEG, ACBrNFeQRCodeBar, pcnConversao, DB,
  DBClient, ACBrNFeDAEventoQR;

type
  TfrmNFeDAEventoQRRetrato = class(TfrmNFeDAEventoQR)
    qrb_09_Itens: TQRBand;
    qrdbtTpDoc1: TQRDBText;
    cdsDocumentos: TClientDataSet;
    qrdbtCnpjEmitente1: TQRDBText;
    qrdbtDocumento1: TQRDBText;
    qrdbtDocumento2: TQRDBText;
    qrdbtCnpjEmitente2: TQRDBText;
    qrdbtTpDoc2: TQRDBText;
    cdsDocumentosTIPO_1: TStringField;
    cdsDocumentosCNPJCPF_1: TStringField;
    cdsDocumentosDOCUMENTO_1: TStringField;
    cdsDocumentosTIPO_2: TStringField;
    cdsDocumentosCNPJCPF_2: TStringField;
    cdsDocumentosDOCUMENTO_2: TStringField;
    qrb_01_Titulo: TQRBand;
    qrlProtocolo: TQRLabel;
    qrlOrgao: TQRLabel;
    qrlDescricao: TQRLabel;
    QRLabel2: TQRLabel;
    QRLabel78: TQRLabel;
    qrlDescricaoEvento: TQRLabel;
    qrb_08_HeaderItens: TQRBand;
    qrb_10_Sistema: TQRBand;
    qrb_05_Evento: TQRChildBand;
    QRLabel13: TQRLabel;
    QRLabel16: TQRLabel;
    QRLabel22: TQRLabel;
    QRLabel24: TQRLabel;
    qrlRazaoEmitente: TQRLabel;
    qrlEnderecoEmitente: TQRLabel;
    qrlMunEmitente: TQRLabel;
    qrlCNPJEmitente: TQRLabel;
    QRLabel93: TQRLabel;
    qrlInscEstEmitente: TQRLabel;
    qrlCEPEmitente: TQRLabel;
    QRLabel98: TQRLabel;
    qrb_03_Emitente: TQRChildBand;
    qrb_04_Destinatario: TQRChildBand;
    qrb_06_Condicoes: TQRChildBand;
    QRLabel38: TQRLabel;
    qrmCorrecao: TQRMemo;
    QRShape17: TQRShape;
    lblTitulo_06: TQRLabel;
    QRShape5: TQRShape;
    qrmCondicoes: TQRMemo;
    qrsQuadro01: TQRShape;
    qrsQuadro02: TQRShape;
    qrsQuadro04: TQRShape;
    qrsQuadro05: TQRShape;
    qrsLinhaV10: TQRShape;
    qrsLinhaV09: TQRShape;
    qrsLinhaH04: TQRShape;
    qrsLinhaV01: TQRShape;
    qrsLinhaH06: TQRShape;
    qrsLinhaH07: TQRShape;
    QRShape10: TQRShape;
    lblModelo: TQRLabel;
    QRShape2: TQRShape;
    qrb_07_Correcao: TQRChildBand;
    QRShape46: TQRShape;
    qrlLinha3: TQRLabel;
    qrlLinha2: TQRLabel;
    qrlLinha1: TQRLabel;
    qrb_02_Documento: TQRChildBand;
    QRShape81: TQRShape;
    QRShape88: TQRShape;
    qrsQuadro03: TQRShape;
    QRLabel8: TQRLabel;
    qrlModelo: TQRLabel;
    QRLabel21: TQRLabel;
    qrlSerie: TQRLabel;
    QRLabel23: TQRLabel;
    qrlNumNFe: TQRLabel;
    qrsLinhaV05: TQRShape;
    qrsLinhaV06: TQRShape;
    qrsLinhaV08: TQRShape;
    QRLabel33: TQRLabel;
    qrlEmissao: TQRLabel;
    qrsLinhaV07: TQRShape;
    QRLabel74: TQRLabel;
    qrlChave: TQRLabel;
    qriBarCode: TQRImage;
    qrlTituloEvento: TQRLabel;
    QRShape48: TQRShape;
    QRLabel9: TQRLabel;
    qrlTipoAmbiente: TQRLabel;
    QRLabel6: TQRLabel;
    qrlEmissaoEvento: TQRLabel;
    QRLabel28: TQRLabel;
    qrlTipoEvento: TQRLabel;
    QRLabel17: TQRLabel;
    qrlSeqEvento: TQRLabel;
    QRShape49: TQRShape;
    QRShape50: TQRShape;
    QRLabel18: TQRLabel;
    qrlStatus: TQRLabel;
    QRLabel12: TQRLabel;
    QRShape51: TQRShape;
    QRLabel1: TQRLabel;
    QRShape52: TQRShape;
    QRShape53: TQRShape;
    QRShape82: TQRShape;
    QRShape99: TQRShape;
    QRLabel4: TQRLabel;
    qrlBairroEmitente: TQRLabel;
    QRShape108: TQRShape;
    QRLabel5: TQRLabel;
    qrlFoneEmitente: TQRLabel;
    QRShape109: TQRShape;
    QRLabel14: TQRLabel;
    qrlRazaoTomador: TQRLabel;
    QRLabel25: TQRLabel;
    qrlEnderecoTomador: TQRLabel;
    QRLabel27: TQRLabel;
    qrlMunTomador: TQRLabel;
    QRLabel30: TQRLabel;
    qrlCNPJTomador: TQRLabel;
    QRLabel32: TQRLabel;
    qrlBairroTomador: TQRLabel;
    QRLabel35: TQRLabel;
    qrlCEPTomador: TQRLabel;
    QRLabel37: TQRLabel;
    qrlFoneTomador: TQRLabel;
    QRLabel40: TQRLabel;
    qrlInscEstTomador: TQRLabel;
    QRShape7: TQRShape;
    QRShape8: TQRShape;
    QRShape9: TQRShape;
    QRShape55: TQRShape;
    QRShape56: TQRShape;
    QRShape58: TQRShape;
    QRShape59: TQRShape;
    qrlMsgTeste: TQRLabel;
    QRLabel15: TQRLabel;
    QRSysData1: TQRSysData;
    qrlblSistema: TQRLabel;
    procedure QREventoBeforePrint(Sender: TCustomQuickRep; var PrintReport: Boolean);
    procedure qrb_01_TituloBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_02_DocumentoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_05_EventoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_03_EmitenteBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_04_DestinatarioBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_06_CondicoesBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_07_CorrecaoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_08_HeaderItensBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_09_ItensBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_10_SistemaBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
  private
    procedure Itens;
  public
    procedure ProtocoloNFe(const sProtocolo: string);
  end;

implementation

uses
  StrUtils, DateUtils,
  ACBrDFeUtil;

{$R *.dfm}

var
  FProtocoloNFe : string;

procedure TfrmNFeDAEventoQRRetrato.Itens;
begin
 // Itens
end;

procedure TfrmNFeDAEventoQRRetrato.ProtocoloNFe(const sProtocolo: string);
begin
  FProtocoloNFe := sProtocolo;
end;

procedure TfrmNFeDAEventoQRRetrato.QREventoBeforePrint(Sender: TCustomQuickRep; var PrintReport: Boolean);
begin
  inherited;

  Itens;

  QRNFeEvento.ReportTitle:='Evento: ' + FormatFloat( '000,000,000', FEventoNFe.InfEvento.nSeqEvento );

  QRNFeEvento.Page.TopMargin    := FMargemSuperior * 100;
  QRNFeEvento.Page.BottomMargin := FMargemInferior * 100;
  QRNFeEvento.Page.LeftMargin   := FMargemEsquerda * 100;
  QRNFeEvento.Page.RightMargin  := FMargemDireita  * 100;
end;

procedure TfrmNFeDAEventoQRRetrato.qrb_01_TituloBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

//  TpcnTpEvento = (teCCe, teCancelamento, teManifDestConfirmacao, teManifDestCiencia,
//                  teManifDestDesconhecimento, teManifDestOperNaoRealizada,
//                  teEncerramento, teEPEC, teInclusaoCondutor, teMultiModal);
  case FEventoNFe.InfEvento.tpEvento of
   teCCe: begin
           qrlLinha1.Caption := 'CARTA DE CORREÇÃO ELETRÔNICA';
           qrlLinha2.Caption := 'Não possui valor fiscal, simples representação da CC-e indicada abaixo.';
           qrlLinha3.Caption := 'CONSULTE A AUTENTICIDADE DA CARTA DE CORREÇÃO ELETRÔNICA NO SITE DA SEFAZ AUTORIZADORA.';
          end;
   teCancelamento: begin
                    qrlLinha1.Caption := 'CANCELAMENTO';
                    qrlLinha2.Caption := 'Não possui valor fiscal, simples representação do Cancelamento indicado abaixo.';
                    qrlLinha3.Caption := 'CONSULTE A AUTENTICIDADE DO CANCELAMENTO NO SITE DA SEFAZ AUTORIZADORA.';
                   end;
   teEPECNFe: begin
               qrlLinha1.Caption := 'EVENTO PRÉVIO DE EMISSÃO EM CONTINGÊNCIA - EPEC';
               qrlLinha2.Caption := 'Não possui valor fiscal, simples representação da EPEC indicada abaixo.';
               qrlLinha3.Caption := 'CONSULTE A AUTENTICIDADE DA EPEC NO SITE DA SEFAZ VIRTUAL DE CONTINGÊNCIA DO RS/AN.';
              end;
  end;
end;

procedure TfrmNFeDAEventoQRRetrato.qrb_02_DocumentoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := False;

  if FNFe <> nil
   then begin
    PrintBand := True;

    if FNFe.Ide.modelo = 55 then
      lblModelo.Caption := 'NOTA FISCAL ELETRÔNICA - NF-e'
    else
      lblModelo.Caption := 'NOTA FISCAL AO CONSUMIDOR ELETRÔNICA - NFC-e';

    qrlModelo.Caption  := IntToStr(FNFe.ide.modelo);
    qrlSerie.Caption   := IntToStr(FNFe.ide.serie);
    qrlNumNFe.Caption  := FormatFloat( '000,000,000', FNFe.Ide.nNF );
    qrlEmissao.Caption := FormatDateTime(DateTimeToStr(FNFe.Ide.dEmi));
    SetBarCodeImage(Copy(FNFe.InfNFe.Id, 4, 44), qriBarCode);
    qrlChave.Caption := NotaUtil.FormatarChaveAcesso(Copy(FNFe.InfNFe.Id, 4, 44));
   end;
end;

procedure TfrmNFeDAEventoQRRetrato.qrb_05_EventoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  with FEventoNFe do
    begin
      case InfEvento.tpEvento of
       teCCe:          qrlTituloEvento.Caption := 'CARTA DE CORREÇÃO ELETRÔNICA';
       teCancelamento: qrlTituloEvento.Caption := 'CANCELAMENTO';
       teEPECNFe:      qrlTituloEvento.Caption := 'EVENTO PRÉVIO DE EMISSÃO EM CONTINGÊNCIA';
      end;

      qrlOrgao.Caption := IntToStr(InfEvento.cOrgao);
      case InfEvento.tpAmb of
       taProducao:    qrlTipoAmbiente.Caption := 'PRODUÇÃO';
       taHomologacao: qrlTipoAmbiente.Caption := 'HOMOLOGAÇÃO - SEM VALOR FISCAL';
      end;
      qrlEmissaoEvento.Caption   := FormatDateTime(DateTimeToStr(InfEvento.dhEvento));
      qrlTipoEvento.Caption      := InfEvento.TipoEvento;
      qrlDescricaoEvento.Caption := InfEvento.DescEvento;
      qrlSeqEvento.Caption       := IntToStr(InfEvento.nSeqEvento);
      qrlStatus.Caption          := IntToStr(RetInfEvento.cStat) + ' - ' +
                                    RetInfEvento.xMotivo;
      qrlProtocolo.Caption       := RetInfEvento.nProt + ' ' +
                                    FormatDateTime(DateTimeToStr(RetInfEvento.dhRegEvento));
    end;
end;

procedure TfrmNFeDAEventoQRRetrato.qrb_03_EmitenteBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := False;

  if FNFe <> nil
   then begin
    PrintBand := True;

    qrlRazaoEmitente.Caption    := FNFe.emit.xNome;
    qrlCNPJEmitente.Caption     := FormatarCNPJCPF(FNFe.emit.CNPJCPF);
    qrlEnderecoEmitente.Caption := FNFe.emit.EnderEmit.xLgr + ', ' + FNFe.emit.EnderEmit.nro;
    qrlBairroEmitente.Caption   := FNFe.emit.EnderEmit.xBairro;
    qrlCEPEmitente.Caption      := FormatarCEP(FormatFloat( '00000000', FNFe.emit.EnderEmit.CEP ));
    qrlMunEmitente.Caption      := FNFe.emit.EnderEmit.xMun+' - '+FNFe.emit.EnderEmit.UF;
    qrlFoneEmitente.Caption     := FormatarFone(FNFe.emit.enderEmit.fone);
    qrlInscEstEmitente.Caption  := FNFe.emit.IE;
   end;
end;

procedure TfrmNFeDAEventoQRRetrato.qrb_04_DestinatarioBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := False;

  if FNFe <> nil
   then begin
    PrintBand := True;

    qrlRazaoTomador.Caption    := FNFe.Dest.xNome;
    qrlCNPJTomador.Caption     := FormatarCNPJCPF(FNFe.Dest.CNPJCPF);
    qrlEnderecoTomador.Caption := FNFe.Dest.EnderDest.xLgr + ', ' + FNFe.Dest.EnderDest.nro;
    qrlBairroTomador.Caption   := FNFe.Dest.EnderDest.xBairro;
    qrlCEPTomador.Caption      := FormatarCEP(FormatFloat( '00000000', FNFe.Dest.EnderDest.CEP));
    qrlMunTomador.Caption      := FNFe.Dest.EnderDest.xMun+' - '+FNFe.Dest.EnderDest.UF;
    qrlFoneTomador.Caption     := FormatarFone(FNFe.Dest.EnderDest.fone);
    qrlInscEstTomador.Caption  := FNFe.Dest.IE;
   end;
end;

procedure TfrmNFeDAEventoQRRetrato.qrb_06_CondicoesBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
var
  i: Integer;
begin
  inherited;

  PrintBand := (FEventoNFe.InfEvento.tpEvento = teCCe) or
               (FEventoNFe.InfEvento.tpEvento = teCancelamento) or
               (FEventoNFe.InfEvento.tpEvento = teEPECNFe) or
               (FEventoNFe.InfEvento.tpAmb = taHomologacao);

  qrlMsgTeste.Visible := False;
  qrlMsgTeste.Enabled := False;

  if FEventoNFe.InfEvento.tpAmb = taHomologacao then
   begin
    qrlMsgTeste.Caption := 'AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL';
    qrlMsgTeste.Visible := True;
    qrlMsgTeste.Enabled := True;
   end;

  qrmCondicoes.Visible := (FEventoNFe.InfEvento.tpEvento = teCCe) or
                          (FEventoNFe.InfEvento.tpEvento = teCancelamento) or
                          (FEventoNFe.InfEvento.tpEvento = teEPECNFe);
  qrmCondicoes.Enabled := (FEventoNFe.InfEvento.tpEvento = teCCe) or
                          (FEventoNFe.InfEvento.tpEvento = teCancelamento) or
                          (FEventoNFe.InfEvento.tpEvento = teEPECNFe);

  case FEventoNFe.InfEvento.tpEvento of
   teCCe: begin
           lblTitulo_06.Caption := 'CONDIÇÕES DE USO';
           qrmCondicoes.Lines.Clear;
           qrmCondicoes.Lines.Add('A Carta de Correcao e disciplinada pelo paragrafo 1o-A do art. 7o do Convenio S/N, de 15 de dezembro de 1970 e pode ser utilizada para regularizacao');
           qrmCondicoes.Lines.Add('de erro ocorrido na emissao de documento fiscal, desde que o erro nao esteja relacionado com:');
           qrmCondicoes.Lines.Add('I - as variaveis que determinam o valor do imposto tais como: base de calculo, aliquota, diferenca de preco, quantidade, valor da operacao ou da prestacao;');
           qrmCondicoes.Lines.Add('II - a correcao de dados cadastrais que implique mudanca do remetente ou do destinatario;');
           qrmCondicoes.Lines.Add('III - a data de emissao ou de saida.');
          end;
   teCancelamento: begin
           lblTitulo_06.Caption := 'DESCRIÇÃO';
           qrmCondicoes.Lines.Clear;
           if FNFe.Ide.modelo = 55 then
             qrmCondicoes.Lines.Add('Protocolo da NF-e Cancelada: ' + FEventoNFe.InfEvento.detEvento.nProt)
           else
             qrmCondicoes.Lines.Add('Protocolo da NFCe Cancelada: ' + FEventoNFe.InfEvento.detEvento.nProt);
           qrmCondicoes.Lines.Add('Motivo do Cancelamento     : ' + FEventoNFe.InfEvento.detEvento.xJust);
          end;
   teEPECNFe: begin
           lblTitulo_06.Caption := 'DESCRIÇÃO';
           qrmCondicoes.Lines.Clear;
           if FEventoNFe.RetInfEvento.cStat = 136 then
           begin
             qrmCondicoes.Lines.Add('Destinatário    : ' + FEventoNFe.InfEvento.detEvento.dest.CNPJCPF);
             qrmCondicoes.Lines.Add('Valor da Nota   : ' + FormatFloat('#0.00', FEventoNFe.InfEvento.detEvento.vNF));
             qrmCondicoes.Lines.Add('Valor do ICMS   : ' + FormatFloat('#0.00', FEventoNFe.InfEvento.detEvento.vICMS));
             qrmCondicoes.Lines.Add('Valor do ICMS ST: ' + FormatFloat('#0.00', FEventoNFe.InfEvento.detEvento.vST));
           end
           else
           begin
             qrmCondicoes.Lines.Add('Motivo Rejeição: ' + FEventoNFe.RetInfEvento.xMotivo);
             qrmCondicoes.Lines.Add('QTDE: ' + inttostr(FEventoNFe.RetInfEvento.chNFePend.Count));
             for i := 0 to FEventoNFe.RetInfEvento.chNFePend.Count -1 do
             begin
               qrmCondicoes.Lines.Add('Chave Pendente : ' + FEventoNFe.RetInfEvento.chNFePend.Items[i].ChavePend);
             end;
           end;
          end;
  end;
end;

procedure TfrmNFeDAEventoQRRetrato.qrb_07_CorrecaoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := FEventoNFe.InfEvento.tpEvento = teCCe;

  qrmCorrecao.Lines.Clear;
  qrmCorrecao.Lines.Add(FEventoNFe.InfEvento.detEvento.xCorrecao);
end;

procedure TfrmNFeDAEventoQRRetrato.qrb_08_HeaderItensBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  // Imprime os Documentos Originários se o Tipo de NFe for Normal
end;

procedure TfrmNFeDAEventoQRRetrato.qrb_09_ItensBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
//var
//  i : integer;
begin
  inherited;

  qrb_09_Itens.Enabled := True;
  (*
  for i := 1 to 2 do
    if Trim(cdsDocumentos.FieldByName('DOCUMENTO_' + IntToStr(i)).AsString) = '' then
      TQRDBText(FindComponent('qrdbtCnpjEmitente' + intToStr(i))).Width := 325
    else
      TQRDBText(FindComponent('qrdbtCnpjEmitente' + intToStr(i))).Width := 128;
  *)
end;

procedure TfrmNFeDAEventoQRRetrato.qrb_10_SistemaBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  qrlblSistema.Caption := FSistema + ' - ' + FUsuario;
end;

end.

