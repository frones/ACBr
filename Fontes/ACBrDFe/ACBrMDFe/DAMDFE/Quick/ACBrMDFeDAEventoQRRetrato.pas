{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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

unit ACBrMDFeDAEventoQRRetrato;

// Atenção todos os comiters
// Quando enviar os fontes referentes ao DAEvento favor alterar
// a data e o nome da linha abaixo.
// Última liberação:
// 25/11/2013 por Italo Jurisato Junior

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, QuickRpt, QRCtrls, XMLIntf, XMLDoc,
  JPEG, ACBrMDFeDAMDFeQRCodeBar, pcnConversao, DB,
  DBClient, ACBrMDFeDAEventoQR;

type
  TfrmMDFeDAEventoQRRetrato = class(TfrmMDFeDAEventoQR)
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
    qrb_04_Tomador: TQRChildBand;
    qrb_06_Descricao: TQRChildBand;
    QRLabel38: TQRLabel;
    QRLabel44: TQRLabel;
    qrmGrupoAlterado: TQRMemo;
    QRLabel46: TQRLabel;
    qrmCampoAlterado: TQRMemo;
    QRLabel42: TQRLabel;
    qrmValorAlterado: TQRMemo;
    QRLabel45: TQRLabel;
    qrmNumItemAlterado: TQRMemo;
    QRShape18: TQRShape;
    QRShape17: TQRShape;
    QRShape15: TQRShape;
    QRShape19: TQRShape;
    QRLabel59: TQRLabel;
    QRShape5: TQRShape;
    qrmDescricao: TQRMemo;
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
    QRLabel65: TQRLabel;
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
    qrlNumMDFe: TQRLabel;
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
    procedure qrb_04_TomadorBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_06_DescricaoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_07_CorrecaoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_08_HeaderItensBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_09_ItensBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_10_SistemaBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
  private
    procedure Itens;
  public
    procedure ProtocoloMDFe(const sProtocolo: String);
  end;

implementation

uses
  StrUtils, DateUtils,
  ACBrDFeUtil, ACBrMDFeUtil;

{$R *.dfm}

var
  FProtocoloMDFe: String;

procedure TfrmMDFeDAEventoQRRetrato.Itens;
begin
 // Itens
end;

procedure TfrmMDFeDAEventoQRRetrato.ProtocoloMDFe(const sProtocolo: String);
begin
  FProtocoloMDFe := sProtocolo;
end;

procedure TfrmMDFeDAEventoQRRetrato.QREventoBeforePrint(Sender: TCustomQuickRep; var PrintReport: Boolean);
begin
  inherited;

  Itens;

  QRMDFeEvento.ReportTitle := 'Evento: ' + FormatFloat('000,000,000', FEventoMDFe.InfEvento.nSeqEvento);

  QRMDFeEvento.Page.TopMargin    := FMargemSuperior * 100;
  QRMDFeEvento.Page.BottomMargin := FMargemInferior * 100;
  QRMDFeEvento.Page.LeftMargin   := FMargemEsquerda * 100;
  QRMDFeEvento.Page.RightMargin  := FMargemDireita  * 100;
end;

procedure TfrmMDFeDAEventoQRRetrato.qrb_01_TituloBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

//  TpcnTpEvento = (teCCe, teCancelamento, teManifDestConfirmacao, teManifDestCiencia,
//                  teManifDestDesconhecimento, teManifDestOperNaoRealizada,
//                  teEncerramento, teEPEC, teInclusaoCondutor, teMultiModal);
  case FEventoMDFe.InfEvento.tpEvento of
   teCancelamento: begin
                    qrlLinha1.Caption := 'CANCELAMENTO';
                    qrlLinha2.Caption := 'Não possui valor fiscal, simples representação do Cancelamento indicado abaixo.';
                    qrlLinha3.Caption := 'CONSULTE A AUTENTICIDADE DO CANCELAMENTO NO SITE DA SEFAZ AUTORIZADORA.';
                   end;
   teEncerramento: begin
                    qrlLinha1.Caption := 'ENCERRAMENTO';
                    qrlLinha2.Caption := 'Não possui valor fiscal, simples representação do Encerramento indicado abaixo.';
                    qrlLinha3.Caption := 'CONSULTE A AUTENTICIDADE DO ENCERRAMENTO NO SITE DA SEFAZ AUTORIZADORA.';
                   end;
   teInclusaoCondutor: begin
                        qrlLinha1.Caption := 'INCLUSÃO DE CONDUTOR';
                        qrlLinha2.Caption := 'Não possui valor fiscal, simples representação da Inclusão de Condutor indicada abaixo.';
                        qrlLinha3.Caption := 'CONSULTE A AUTENTICIDADE DA INCLUSÃO DE CONDUTOR NO SITE DA SEFAZ AUTORIZADORA.';
                       end;
  end;
end;

procedure TfrmMDFeDAEventoQRRetrato.qrb_02_DocumentoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := False;

  if FMDFe <> nil
   then begin
    PrintBand := True;

    qrlModelo.Caption  := FMDFe.ide.modelo;
    qrlSerie.Caption   := IntToStr(FMDFe.ide.serie);
    qrlNumMDFe.Caption := FormatFloat('000,000,000', FMDFe.Ide.nMDF);
    qrlEmissao.Caption := FormatDateTime(DateTimeToStr(FMDFe.Ide.dhEmi));

    SetBarCodeImage(Copy(FMDFe.InfMDFe.Id, 5, 44), qriBarCode);

    qrlChave.Caption := MFormatarChaveAcesso(Copy(FMDFe.InfMDFe.Id, 5, 44));
   end;
end;

procedure TfrmMDFeDAEventoQRRetrato.qrb_05_EventoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  with FEventoMDFe do
    begin
      case InfEvento.tpEvento of
       teCancelamento:     qrlTituloEvento.Caption := 'CANCELAMENTO';
       teEncerramento:     qrlTituloEvento.Caption := 'ENCERRAMENTO';
       teInclusaoCondutor: qrlTituloEvento.Caption := 'INCLUSÃO DE CONDUTOR';
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

procedure TfrmMDFeDAEventoQRRetrato.qrb_03_EmitenteBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := False;

  if FMDFe <> nil
   then begin
    PrintBand := True;

    qrlRazaoEmitente.Caption    := FMDFe.emit.xNome;
    qrlCNPJEmitente.Caption     := FormatarCNPJCPF(FMDFe.emit.CNPJ);
    qrlEnderecoEmitente.Caption := FMDFe.emit.EnderEmit.xLgr + ', ' + FMDFe.emit.EnderEmit.nro;
    qrlBairroEmitente.Caption   := FMDFe.emit.EnderEmit.xBairro;
    qrlCEPEmitente.Caption      := FormatarCEP(FormatFloat( '00000000', FMDFe.emit.EnderEmit.CEP ));
    qrlMunEmitente.Caption      := FMDFe.emit.EnderEmit.xMun+' - '+FMDFe.emit.EnderEmit.UF;
    qrlFoneEmitente.Caption     := FormatarFone(FMDFe.emit.enderEmit.fone);
    qrlInscEstEmitente.Caption  := FMDFe.emit.IE;
   end;
end;

procedure TfrmMDFeDAEventoQRRetrato.qrb_04_TomadorBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := False;

  if FMDFe <> nil
   then begin
    (*
    PrintBand := True;
    if FMDFe.Ide.Toma4.xNome = ''
     then begin
      case FMDFe.Ide.Toma03.Toma of
      tmRemetente:
        begin
          qrlRazaoTomador.Caption    := FMDFe.Rem.xNome;
          qrlCNPJTomador.Caption     := FormatarCNPJCPF(FMDFe.Rem.CNPJCPF);
          qrlEnderecoTomador.Caption := FMDFe.Rem.EnderReme.xLgr + ', ' + FMDFe.Rem.EnderReme.nro;
          qrlBairroTomador.Caption   := FMDFe.Rem.EnderReme.xBairro;
          qrlCEPTomador.Caption      := FormatarCEP(FormatFloat( '00000000', FMDFe.Rem.EnderReme.CEP));
          qrlMunTomador.Caption      := FMDFe.Rem.EnderReme.xMun+' - '+FMDFe.Rem.EnderReme.UF;
          qrlFoneTomador.Caption     := FormatarFone(FMDFe.Rem.fone);
          qrlInscEstTomador.Caption  := FMDFe.Rem.IE;
        end;
      tmExpedidor:
        begin
          qrlRazaoTomador.Caption    := FMDFe.Exped.xNome;
          qrlCNPJTomador.Caption     := FormatarCNPJCPF(FMDFe.Exped.CNPJCPF);
          qrlEnderecoTomador.Caption := FMDFe.Exped.EnderExped.xLgr + ', ' + FMDFe.Exped.EnderExped.nro;
          qrlBairroTomador.Caption   := FMDFe.Exped.EnderExped.xBairro;
          qrlCEPTomador.Caption      := FormatarCEP(FormatFloat( '00000000', FMDFe.Exped.EnderExped.CEP));
          qrlMunTomador.Caption      := FMDFe.Exped.EnderExped.xMun+' - '+FMDFe.Exped.EnderExped.UF;
          qrlFoneTomador.Caption     := FormatarFone(FMDFe.Exped.fone);
          qrlInscEstTomador.Caption  := FMDFe.Exped.IE;
        end;
      tmRecebedor:
        begin
          qrlRazaoTomador.Caption    := FMDFe.Receb.xNome;
          qrlCNPJTomador.Caption     := FormatarCNPJCPF(FMDFe.Receb.CNPJCPF);
          qrlEnderecoTomador.Caption := FMDFe.Receb.EnderReceb.xLgr + ', ' + FMDFe.Receb.EnderReceb.nro;
          qrlBairroTomador.Caption   := FMDFe.Receb.EnderReceb.xBairro;
          qrlCEPTomador.Caption      := FormatarCEP(FormatFloat( '00000000', FMDFe.Receb.EnderReceb.CEP));
          qrlMunTomador.Caption      := FMDFe.Receb.EnderReceb.xMun+' - '+FMDFe.Receb.EnderReceb.UF;
          qrlFoneTomador.Caption     := FormatarFone(FMDFe.Receb.fone);
          qrlInscEstTomador.Caption  := FMDFe.Receb.IE;
        end;
      tmDestinatario:
        begin
          qrlRazaoTomador.Caption    := FMDFe.Dest.xNome;
          qrlCNPJTomador.Caption     := FormatarCNPJCPF(FMDFe.Dest.CNPJCPF);
          qrlEnderecoTomador.Caption := FMDFe.Dest.EnderDest.xLgr + ', ' + FMDFe.Dest.EnderDest.nro;
          qrlBairroTomador.Caption   := FMDFe.Dest.EnderDest.xBairro;
          qrlCEPTomador.Caption      := FormatarCEP(FormatFloat( '00000000', FMDFe.Dest.EnderDest.CEP));
          qrlMunTomador.Caption      := FMDFe.Dest.EnderDest.xMun+' - '+FMDFe.Dest.EnderDest.UF;
          qrlFoneTomador.Caption     := FormatarFone(FMDFe.Dest.fone);
          qrlInscEstTomador.Caption  := FMDFe.Dest.IE;
        end;
      end;
     end
     else begin
      qrlRazaoTomador.Caption    := FMDFe.Ide.Toma4.xNome;
      qrlCNPJTomador.Caption     := FormatarCNPJCPF(FMDFe.Ide.Toma4.CNPJCPF);
      qrlEnderecoTomador.Caption := FMDFe.Ide.Toma4.EnderToma.xLgr + ', ' + FMDFe.Ide.Toma4.EnderToma.nro;
      qrlBairroTomador.Caption   := FMDFe.Ide.Toma4.EnderToma.xBairro;
      qrlCEPTomador.Caption      := FormatarCEP(FormatFloat( '00000000', FMDFe.Ide.Toma4.EnderToma.CEP));
      qrlMunTomador.Caption      := FMDFe.Ide.Toma4.EnderToma.xMun+' - '+FMDFe.Ide.Toma4.EnderToma.UF;
      qrlFoneTomador.Caption     := FormatarFone(FMDFe.Ide.Toma4.fone);
      qrlInscEstTomador.Caption  := FMDFe.Ide.Toma4.IE;
     end;
   *)  
   end;
end;

procedure TfrmMDFeDAEventoQRRetrato.qrb_06_DescricaoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := (FEventoMDFe.InfEvento.tpEvento = teCancelamento) or
               (FEventoMDFe.InfEvento.tpEvento = teEncerramento) or
               (FEventoMDFe.InfEvento.tpEvento = teInclusaoCondutor) or
               (FEventoMDFe.InfEvento.tpAmb = taHomologacao);

  qrlMsgTeste.Visible := False;
  qrlMsgTeste.Enabled := False;

  if FEventoMDFe.InfEvento.tpAmb = taHomologacao then
   begin
    qrlMsgTeste.Caption := 'AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL';
    qrlMsgTeste.Visible := True;
    qrlMsgTeste.Enabled := True;
   end;

  qrmDescricao.Visible := (FEventoMDFe.InfEvento.tpEvento = teCancelamento) or
                          (FEventoMDFe.InfEvento.tpEvento = teEncerramento) or
                          (FEventoMDFe.InfEvento.tpEvento = teInclusaoCondutor);
  qrmDescricao.Enabled := (FEventoMDFe.InfEvento.tpEvento = teCancelamento) or
                          (FEventoMDFe.InfEvento.tpEvento = teEncerramento) or
                          (FEventoMDFe.InfEvento.tpEvento = teInclusaoCondutor);

  qrmDescricao.Lines.Clear;
  case FEventoMDFe.InfEvento.tpEvento of
  teCancelamento: begin
                   qrmDescricao.Lines.Add('Protocolo do MDFe Cancelado: ' + FEventoMDFe.InfEvento.detEvento.nProt);
                   qrmDescricao.Lines.Add('Motivo do Cancelamento     : ' + FEventoMDFe.InfEvento.detEvento.xJust);
                  end;
  teEncerramento: begin
                   qrmDescricao.Lines.Add('Protocolo do MDFe Encerrado: ' + FEventoMDFe.InfEvento.detEvento.nProt);
                   qrmDescricao.Lines.Add('Data do Encerramento       : ' + DateToStr(FEventoMDFe.InfEvento.detEvento.dtEnc));
                   qrmDescricao.Lines.Add('Código da UF               : ' + IntToStr(FEventoMDFe.InfEvento.detEvento.cUF));
                   qrmDescricao.Lines.Add('Código do Município        : ' + IntToStr(FEventoMDFe.InfEvento.detEvento.cMun));
                  end;
  teInclusaoCondutor: begin
                       qrmDescricao.Lines.Add('Dados do Motorista');
                       qrmDescricao.Lines.Add('CPF : ' + FEventoMDFe.InfEvento.detEvento.CPF);
                       qrmDescricao.Lines.Add('Nome: ' + FEventoMDFe.InfEvento.detEvento.xNome);
                      end;
  end;
end;

procedure TfrmMDFeDAEventoQRRetrato.qrb_07_CorrecaoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := False;
end;

procedure TfrmMDFeDAEventoQRRetrato.qrb_08_HeaderItensBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  // Imprime os Documentos Originários se o Tipo de MDFe for Normal
end;

procedure TfrmMDFeDAEventoQRRetrato.qrb_09_ItensBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
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

procedure TfrmMDFeDAEventoQRRetrato.qrb_10_SistemaBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  qrlblSistema.Caption := FSistema + ' - ' + FUsuario;
end;

end.

