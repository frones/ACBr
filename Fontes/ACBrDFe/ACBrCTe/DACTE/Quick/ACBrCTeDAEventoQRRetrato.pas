{******************************************************************************}
{ Projeto: Componente ACBrCTe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - CTe - http://www.cte.fazenda.gov.br            }
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

unit ACBrCTeDAEventoQRRetrato;

// Atenção todos os comiters
// Quando enviar os fontes referentes ao DAEvento favor alterar
// a data e o nome da linha abaixo.
// Última liberação:
// 25/11/2013 por Italo Jurisato Junior

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, QuickRpt, QRCtrls, XMLIntf, XMLDoc,
  JPEG, ACBrCTeQRCodeBar, pcnConversao, DB,
  DBClient, ACBrCTeDAEventoQR;

type
  TfrmCTeDAEventoQRRetrato = class(TfrmCTeDAEventoQR)
    qrb_01_Titulo: TQRBand;
    qrlProtocolo: TQRLabel;
    qrlOrgao: TQRLabel;
    qrlDescricao: TQRLabel;
    QRLabel2: TQRLabel;
    QRLabel78: TQRLabel;
    qrlDescricaoEvento: TQRLabel;
    qrb_10_HeaderItens: TQRBand;
    qrb_05_Evento: TQRChildBand;
    qrb_04_Tomador: TQRChildBand;
    qrb_06_Condicoes: TQRChildBand;
    lblTitulo_06: TQRLabel;
    QRShape5: TQRShape;
    qrmCondicoes: TQRMemo;
    qrsQuadro01: TQRShape;
    qrsQuadro04: TQRShape;
    qrsLinhaV10: TQRShape;
    qrsLinhaV09: TQRShape;
    qrsLinhaH04: TQRShape;
    qrsLinhaV01: TQRShape;
    QRShape10: TQRShape;
    QRLabel65: TQRLabel;
    QRShape2: TQRShape;
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
    qrlNumCte: TQRLabel;
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
    QRLabel1: TQRLabel;
    QRShape52: TQRShape;
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
    cdsCorrecao: TClientDataSet;
    cdsCorrecaoItem: TIntegerField;
    cdsCorrecaoGrupo: TStringField;
    cdsCorrecaoCampo: TStringField;
    cdsCorrecaoValor: TStringField;
    qrb_08_Correcao_Detalhe: TQRBand;
    QRShape3: TQRShape;
    QRShape6: TQRShape;
    QRShape4: TQRShape;
    qrdbtxtItem: TQRDBText;
    qrdbtxtGrupo: TQRDBText;
    qrdbtxtCampo: TQRDBText;
    qrdbtxtValor: TQRDBText;
    qrb_03_Documento: TQRBand;
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
    QRShape11: TQRShape;
    QRShape1: TQRShape;
    qrb_07_Correcao: TQRBand;
    QRShape18: TQRShape;
    QRShape19: TQRShape;
    QRShape15: TQRShape;
    QRLabel46: TQRLabel;
    QRLabel45: TQRLabel;
    QRLabel44: TQRLabel;
    QRLabel42: TQRLabel;
    QRLabel38: TQRLabel;
    QRShape17: TQRShape;
    qrsQuadro05: TQRShape;
    qrb_09_Correcao_Summary: TQRBand;
    QRShape12: TQRShape;
    QRLabel15: TQRLabel;
    qrlPaginaDe: TQRLabel;
    procedure QREventoBeforePrint(Sender: TCustomQuickRep; var PrintReport: Boolean);
    procedure qrb_01_TituloBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_02_DocumentoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_05_EventoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure ABeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_04_TomadorBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_06_CondicoesBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_10_HeaderItensBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_08_Correcao_DetalheBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_09_Correcao_SummaryBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrdbtxtValorPrint(sender: TObject; var Value: String);
  private
    procedure Itens;
  public
    procedure ProtocoloCTe(const sProtocolo: String);
  end;

implementation

uses
  StrUtils, DateUtils,
  ACBrDFeUtil, ACBrCTeUtil;

{$R *.dfm}

var
  FProtocoloCTe : String;

procedure TfrmCTeDAEventoQRRetrato.qrb_08_Correcao_DetalheBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;
  PrintBand := FEventoCTe.InfEvento.tpEvento = teCCe;
end;

procedure TfrmCTeDAEventoQRRetrato.Itens;
var
  i: Integer;
begin
 // Itens
  if ( cdsCorrecao.Active ) then
  begin
    cdsCorrecao.CancelUpdates;
  end
  else
  begin
    cdsCorrecao.CreateDataSet;
  end;

  for i := 0 to (FEventoCTe.InfEvento.detEvento.infCorrecao.Count -1) do
  begin
    cdsCorrecao.Append;
    cdsCorrecaoItem.AsInteger := FEventoCTe.InfEvento.detEvento.infCorrecao[i].nroItemAlterado;
    cdsCorrecaoGrupo.AsString := FEventoCTe.InfEvento.detEvento.infCorrecao[i].grupoAlterado;
    cdsCorrecaoCampo.AsString := FEventoCTe.InfEvento.detEvento.infCorrecao[i].campoAlterado;
    cdsCorrecaoValor.AsString := FEventoCTe.InfEvento.detEvento.infCorrecao[i].valorAlterado;
    cdsCorrecao.Post;
  end;
end;

procedure TfrmCTeDAEventoQRRetrato.ProtocoloCTe(const sProtocolo: String);
begin
  FProtocoloCTe := sProtocolo;
end;

procedure TfrmCTeDAEventoQRRetrato.QREventoBeforePrint(Sender: TCustomQuickRep; var PrintReport: Boolean);
begin
  inherited;

  Itens;

  QRCTeEvento.ReportTitle := 'Evento: ' + FormatFloat('000,000,000', FEventoCTe.InfEvento.nSeqEvento);

  QRCTeEvento.Page.TopMargin    := FMargemSuperior * 100;
  QRCTeEvento.Page.BottomMargin := FMargemInferior * 100;
  QRCTeEvento.Page.LeftMargin   := FMargemEsquerda * 100;
  QRCTeEvento.Page.RightMargin  := FMargemDireita  * 100;
end;

procedure TfrmCTeDAEventoQRRetrato.qrb_09_Correcao_SummaryBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  PrintBand := FEventoCTe.InfEvento.tpEvento = teCCe;
end;

procedure TfrmCTeDAEventoQRRetrato.qrb_01_TituloBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  case FEventoCTe.InfEvento.tpEvento of
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
   teEPEC: begin
            qrlLinha1.Caption := 'EVENTO PRÉVIO DE EMISSÃO EM CONTINGÊNCIA - EPEC';
            qrlLinha2.Caption := 'Não possui valor fiscal, simples representação da EPEC indicada abaixo.';
            qrlLinha3.Caption := 'CONSULTE A AUTENTICIDADE DA EPEC NO SITE DA SEFAZ VIRTUAL DE CONTINGÊNCIA DO RS/SP.';
           end;
   teMultiModal: begin
                  qrlLinha1.Caption := 'REGISTRO DO MULTIMODAL';
                  qrlLinha2.Caption := 'Não possui valor fiscal, simples representação do Registro indicado abaixo.';
                  qrlLinha3.Caption := 'CONSULTE A AUTENTICIDADE DO REGISTRO NO SITE DA SEFAZ AUTORIZADORA.';
                 end;
  end;
end;

procedure TfrmCTeDAEventoQRRetrato.qrb_02_DocumentoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := False;

  if FCTe <> nil
   then begin
    PrintBand := True;

    qrlModelo.Caption  := FCTe.ide.modelo;
    qrlSerie.Caption   := IntToStr(FCTe.ide.serie);
    qrlNumCTe.Caption  := FormatFloat('000,000,000', FCTe.Ide.nCT);
    qrlEmissao.Caption := FormatDateTime(DateTimeToStr(FCTe.Ide.dhEmi));

    SetBarCodeImage(Copy(FCTe.InfCTe.Id, 4, 44), qriBarCode);
    qrlChave.Caption := CTeUtil.FormatarChaveAcesso(Copy(FCTe.InfCTe.Id, 4, 44));
   end;
end;

procedure TfrmCTeDAEventoQRRetrato.qrb_05_EventoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  with FEventoCTe do
    begin
      case InfEvento.tpEvento of
       teCCe:          qrlTituloEvento.Caption := 'CARTA DE CORREÇÃO ELETRÔNICA';
       teCancelamento: qrlTituloEvento.Caption := 'CANCELAMENTO';
       teEPEC:         qrlTituloEvento.Caption := 'EVENTO PRÉVIO DE EMISSÃO EM CONTINGÊNCIA';
       teMultimodal:   qrlTituloEvento.Caption := 'REGISTRO DO MULTIMODAL';
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

procedure TfrmCTeDAEventoQRRetrato.ABeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := False;

  if FCTe <> nil
   then begin
    PrintBand := True;

    qrlRazaoEmitente.Caption    := FCTe.emit.xNome;
    qrlCNPJEmitente.Caption     := FormatarCNPJCPF(FCTe.emit.CNPJ);
    qrlEnderecoEmitente.Caption := FCTe.emit.EnderEmit.xLgr + ', ' + FCTe.emit.EnderEmit.nro;
    qrlBairroEmitente.Caption   := FCTe.emit.EnderEmit.xBairro;
    qrlCEPEmitente.Caption      := FormatarCEP(FormatFloat('00000000', FCTe.emit.EnderEmit.CEP));
    qrlMunEmitente.Caption      := FCTe.emit.EnderEmit.xMun + ' - ' + FCTe.emit.EnderEmit.UF;
    qrlFoneEmitente.Caption     := FormatarFone(FCTe.emit.enderEmit.fone);
    qrlInscEstEmitente.Caption  := FCTe.emit.IE;
   end;
end;

procedure TfrmCTeDAEventoQRRetrato.qrb_04_TomadorBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := False;

  if FCTe <> nil
   then begin
    PrintBand := True;

    if FCTe.Ide.Toma4.xNome = ''
     then begin
      case FCTe.Ide.Toma03.Toma of
      tmRemetente:
        begin
          qrlRazaoTomador.Caption    := FCTe.Rem.xNome;
          qrlCNPJTomador.Caption     := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
          qrlEnderecoTomador.Caption := FCTe.Rem.EnderReme.xLgr + ', ' + FCTe.Rem.EnderReme.nro;
          qrlBairroTomador.Caption   := FCTe.Rem.EnderReme.xBairro;
          qrlCEPTomador.Caption      := FormatarCEP(FormatFloat('00000000', FCTe.Rem.EnderReme.CEP));
          qrlMunTomador.Caption      := FCTe.Rem.EnderReme.xMun + ' - ' + FCTe.Rem.EnderReme.UF;
          qrlFoneTomador.Caption     := FormatarFone(FCTe.Rem.fone);
          qrlInscEstTomador.Caption  := FCTe.Rem.IE;
        end;
      tmExpedidor:
        begin
          qrlRazaoTomador.Caption    := FCTe.Exped.xNome;
          qrlCNPJTomador.Caption     := FormatarCNPJCPF(FCTe.Exped.CNPJCPF);
          qrlEnderecoTomador.Caption := FCTe.Exped.EnderExped.xLgr + ', ' + FCTe.Exped.EnderExped.nro;
          qrlBairroTomador.Caption   := FCTe.Exped.EnderExped.xBairro;
          qrlCEPTomador.Caption      := FormatarCEP(FormatFloat('00000000', FCTe.Exped.EnderExped.CEP));
          qrlMunTomador.Caption      := FCTe.Exped.EnderExped.xMun + ' - ' + FCTe.Exped.EnderExped.UF;
          qrlFoneTomador.Caption     := FormatarFone(FCTe.Exped.fone);
          qrlInscEstTomador.Caption  := FCTe.Exped.IE;
        end;
      tmRecebedor:
        begin
          qrlRazaoTomador.Caption    := FCTe.Receb.xNome;
          qrlCNPJTomador.Caption     := FormatarCNPJCPF(FCTe.Receb.CNPJCPF);
          qrlEnderecoTomador.Caption := FCTe.Receb.EnderReceb.xLgr + ', ' + FCTe.Receb.EnderReceb.nro;
          qrlBairroTomador.Caption   := FCTe.Receb.EnderReceb.xBairro;
          qrlCEPTomador.Caption      := FormatarCEP(FormatFloat('00000000', FCTe.Receb.EnderReceb.CEP));
          qrlMunTomador.Caption      := FCTe.Receb.EnderReceb.xMun + ' - ' + FCTe.Receb.EnderReceb.UF;
          qrlFoneTomador.Caption     := FormatarFone(FCTe.Receb.fone);
          qrlInscEstTomador.Caption  := FCTe.Receb.IE;
        end;
      tmDestinatario:
        begin
          qrlRazaoTomador.Caption    := FCTe.Dest.xNome;
          qrlCNPJTomador.Caption     := FormatarCNPJCPF(FCTe.Dest.CNPJCPF);
          qrlEnderecoTomador.Caption := FCTe.Dest.EnderDest.xLgr + ', ' + FCTe.Dest.EnderDest.nro;
          qrlBairroTomador.Caption   := FCTe.Dest.EnderDest.xBairro;
          qrlCEPTomador.Caption      := FormatarCEP(FormatFloat('00000000', FCTe.Dest.EnderDest.CEP));
          qrlMunTomador.Caption      := FCTe.Dest.EnderDest.xMun + ' - ' + FCTe.Dest.EnderDest.UF;
          qrlFoneTomador.Caption     := FormatarFone(FCTe.Dest.fone);
          qrlInscEstTomador.Caption  := FCTe.Dest.IE;
        end;
      end;
     end
     else begin
      qrlRazaoTomador.Caption    := FCTe.Ide.Toma4.xNome;
      qrlCNPJTomador.Caption     := FormatarCNPJCPF(FCTe.Ide.Toma4.CNPJCPF);
      qrlEnderecoTomador.Caption := FCTe.Ide.Toma4.EnderToma.xLgr + ', ' + FCTe.Ide.Toma4.EnderToma.nro;
      qrlBairroTomador.Caption   := FCTe.Ide.Toma4.EnderToma.xBairro;
      qrlCEPTomador.Caption      := FormatarCEP(FormatFloat('00000000', FCTe.Ide.Toma4.EnderToma.CEP));
      qrlMunTomador.Caption      := FCTe.Ide.Toma4.EnderToma.xMun + ' - ' + FCTe.Ide.Toma4.EnderToma.UF;
      qrlFoneTomador.Caption     := FormatarFone(FCTe.Ide.Toma4.fone);
      qrlInscEstTomador.Caption  := FCTe.Ide.Toma4.IE;
     end;
   end;
end;

procedure TfrmCTeDAEventoQRRetrato.qrb_06_CondicoesBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := (FEventoCTe.InfEvento.tpEvento = teCCe) or
               (FEventoCTe.InfEvento.tpEvento = teCancelamento) or
               (FEventoCTe.InfEvento.tpEvento = teEPEC) or
               (FEventoCTe.InfEvento.tpEvento = teMultimodal) or
               (FEventoCTe.InfEvento.tpAmb = taHomologacao);

  qrlMsgTeste.Visible := False;
  qrlMsgTeste.Enabled := False;

  if FEventoCTe.InfEvento.tpAmb = taHomologacao then
   begin
    qrlMsgTeste.Caption := 'AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL';
    qrlMsgTeste.Visible := True;
    qrlMsgTeste.Enabled := True;
   end;

  qrmCondicoes.Visible := (FEventoCTe.InfEvento.tpEvento = teCCe) or
                          (FEventoCTe.InfEvento.tpEvento = teCancelamento) or
                          (FEventoCTe.InfEvento.tpEvento = teMultimodal) or
                          (FEventoCTe.InfEvento.tpEvento = teEPEC);
  qrmCondicoes.Enabled := (FEventoCTe.InfEvento.tpEvento = teCCe) or
                          (FEventoCTe.InfEvento.tpEvento = teCancelamento) or
                          (FEventoCTe.InfEvento.tpEvento = teMultimodal) or
                          (FEventoCTe.InfEvento.tpEvento = teEPEC);

  case FEventoCTe.InfEvento.tpEvento of
   teCCe: begin
           lblTitulo_06.Caption := 'CONDIÇÕES DE USO';
           qrmCondicoes.Lines.Clear;
           qrmCondicoes.Lines.Add('A Carta de Correcao e disciplinada pelo Art. 58-B do CONVENIO/SINIEF 06/89: Fica permitida a utilizacao de carta de correcao, para regularizacao');
           qrmCondicoes.Lines.Add('de erro ocorrido na emissao de documentos fiscais relativos a prestacao de servico de transporte, desde que o erro nao esteja relacionado com:');
           qrmCondicoes.Lines.Add('I - as variaveis que determinam o valor do imposto tais como: base de calculo, aliquota, diferenca de preco, quantidade, valor da prestacao;');
           qrmCondicoes.Lines.Add('II - a correcao de dados cadastrais que implique mudanca do emitente, tomador, remetente ou do destinatario;');
           qrmCondicoes.Lines.Add('III - a data de emissao ou de saida.');
          end;
   teCancelamento: begin
           lblTitulo_06.Caption := 'DESCRIÇÃO';
           qrmCondicoes.Lines.Clear;
           qrmCondicoes.Lines.Add('Protocolo do CTe Cancelado: ' + FEventoCTe.InfEvento.detEvento.nProt);
           qrmCondicoes.Lines.Add('Motivo do Cancelamento    : ' + FEventoCTe.InfEvento.detEvento.xJust);
          end;
   teEPEC: begin
           lblTitulo_06.Caption := 'DESCRIÇÃO';
           qrmCondicoes.Lines.Clear;
           qrmCondicoes.Lines.Add('Motivo do EPEC     : ' + FEventoCTe.InfEvento.detEvento.xJust);
           qrmCondicoes.Lines.Add('Valor do ICMS      : ' + FormatFloat('#0.00', FEventoCTe.InfEvento.detEvento.vICMS));
           qrmCondicoes.Lines.Add('Valor da Prestação : ' + FormatFloat('#0.00', FEventoCTe.InfEvento.detEvento.vTPrest));
           qrmCondicoes.Lines.Add('Valor da Carga     : ' + FormatFloat('#0.00', FEventoCTe.InfEvento.detEvento.vCarga));
           qrmCondicoes.Lines.Add('UF de inicio/fim da prestação: ' + FEventoCTe.InfEvento.detEvento.UFIni + ' / ' +
                                                                      FEventoCTe.InfEvento.detEvento.UFFim);
          end;
   teMultimodal: begin
           lblTitulo_06.Caption := 'DESCRIÇÃO';
           qrmCondicoes.Lines.Clear;
           qrmCondicoes.Lines.Add('Documento : ' + FEventoCTe.InfEvento.detEvento.nDoc);
           qrmCondicoes.Lines.Add('Registro  : ' + FEventoCTe.InfEvento.detEvento.xRegistro);
          end;
  end;
end;

procedure TfrmCTeDAEventoQRRetrato.qrb_10_HeaderItensBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  qrlPaginaDe.Caption := ''; //' de ' + IntToStr(QRCTeEvento.QRPrinter.PageNumber);
  qrlblSistema.Caption := FSistema + ' - ' + FUsuario;
end;

procedure TfrmCTeDAEventoQRRetrato.qrdbtxtValorPrint(sender: TObject;
  var Value: String);
var
  vLength: Integer;
begin
  inherited;
  vLength := 11 * ((Length(Value) div 90) + 1);

  qrb_08_Correcao_Detalhe.Height := vLength;

  qrdbtxtValor.Height := vLength;
  QRShape11.Height    := vLength;
  QRShape3.Height     := vLength;
  QRShape6.Height     := vLength;
  QRShape4.Height     := vLength;
end;

end.

