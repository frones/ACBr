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

{******************************************************************************
|* Historico
|*
*******************************************************************************}

{$I ACBr.inc}

unit ACBrMDFeDAEventoRLRetrato;

interface

uses
  Messages, SysUtils, Variants, Classes, db, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, RLReport, RLPDFFilter, RLBarcode, pcnConversao,
  ACBrMDFeDAEventoRL, RLFilters;

type

  { TfrmMDFeDAEventoRLRetrato }

  TfrmMDFeDAEventoRLRetrato = class(TfrmMDFeDAEventoRL)
    RLBarcode1: TRLBarcode;
    rlb_09_Itens: TRLBand;
    rldbtTpDoc1: TRLDBText;
    rldbtCnpjEmitente1: TRLDBText;
    rldbtDocumento1: TRLDBText;
    rldbtDocumento2: TRLDBText;
    rldbtCnpjEmitente2: TRLDBText;
    rldbtTpDoc2: TRLDBText;
    rlb_01_Titulo: TRLBand;
    rllProtocolo: TRLLabel;
    rllOrgao: TRLLabel;
    rllDescricao: TRLLabel;
    rlLabel2: TRLLabel;
    rlLabel78: TRLLabel;
    rllDescricaoEvento: TRLLabel;
    rlb_08_HeaderItens: TRLBand;
    rlb_10_Sistema: TRLBand;
    rlb_05_Evento: TRLBand;
    rlLabel13: TRLLabel;
    rlLabel16: TRLLabel;
    rlLabel22: TRLLabel;
    rlLabel24: TRLLabel;
    rllRazaoEmitente: TRLLabel;
    rllEnderecoEmitente: TRLLabel;
    rllMunEmitente: TRLLabel;
    rllCNPJEmitente: TRLLabel;
    rlLabel93: TRLLabel;
    rllInscEstEmitente: TRLLabel;
    rllCEPEmitente: TRLLabel;
    rlLabel98: TRLLabel;
    rlb_03_Emitente: TRLBand;
    rlb_04_Tomador: TRLBand;
    rlb_06_Descricao: TRLBand;
    rlLabel38: TRLLabel;
    rlLabel44: TRLLabel;
    rlmGrupoAlterado: TRLMemo;
    rlLabel46: TRLLabel;
    rlmCampoAlterado: TRLMemo;
    rlLabel42: TRLLabel;
    rlmValorAlterado: TRLMemo;
    rlLabel45: TRLLabel;
    rlmNumItemAlterado: TRLMemo;
    rlShape18: TRLDraw;
    rlShape17: TRLDraw;
    rlShape15: TRLDraw;
    rlShape19: TRLDraw;
    rlLabel59: TRLLabel;
    rlShape5: TRLDraw;
    rlmDescricao: TRLMemo;
    rlsQuadro01: TRLDraw;
    rlsQuadro02: TRLDraw;
    rlsQuadro04: TRLDraw;
    rlsQuadro05: TRLDraw;
    rlsLinhaV10: TRLDraw;
    rlsLinhaV09: TRLDraw;
    rlsLinhaH04: TRLDraw;
    rlsLinhaV01: TRLDraw;
    rlsLinhaH06: TRLDraw;
    rlsLinhaH07: TRLDraw;
    rlShape10: TRLDraw;
    rlLabel65: TRLLabel;
    rlShape2: TRLDraw;
    rlb_07_Correcao: TRLBand;
    rlShape46: TRLDraw;
    rllLinha3: TRLLabel;
    rllLinha2: TRLLabel;
    rllLinha1: TRLLabel;
    rlb_02_Documento: TRLBand;
    rlShape81: TRLDraw;
    rlShape88: TRLDraw;
    rlsQuadro03: TRLDraw;
    rlLabel8: TRLLabel;
    rllModelo: TRLLabel;
    rlLabel21: TRLLabel;
    rllSerie: TRLLabel;
    rlLabel23: TRLLabel;
    rllNumMDFe: TRLLabel;
    rlsLinhaV05: TRLDraw;
    rlsLinhaV06: TRLDraw;
    rlsLinhaV08: TRLDraw;
    rlLabel33: TRLLabel;
    rllEmissao: TRLLabel;
    rlsLinhaV07: TRLDraw;
    rlLabel74: TRLLabel;
    rllChave: TRLLabel;
    rllTituloEvento: TRLLabel;
    rlShape48: TRLDraw;
    rlLabel9: TRLLabel;
    rllTipoAmbiente: TRLLabel;
    rlLabel6: TRLLabel;
    rllEmissaoEvento: TRLLabel;
    rlLabel28: TRLLabel;
    rllTipoEvento: TRLLabel;
    rlLabel17: TRLLabel;
    rllSeqEvento: TRLLabel;
    rlShape49: TRLDraw;
    rlShape50: TRLDraw;
    rlLabel18: TRLLabel;
    rllStatus: TRLLabel;
    rlLabel12: TRLLabel;
    rlShape51: TRLDraw;
    rlLabel1: TRLLabel;
    rlShape52: TRLDraw;
    rlShape53: TRLDraw;
    rlShape82: TRLDraw;
    rlShape99: TRLDraw;
    rlLabel4: TRLLabel;
    rllBairroEmitente: TRLLabel;
    rlShape108: TRLDraw;
    rlLabel5: TRLLabel;
    rllFoneEmitente: TRLLabel;
    rlShape109: TRLDraw;
    rlLabel14: TRLLabel;
    rllRazaoTomador: TRLLabel;
    rlLabel25: TRLLabel;
    rllEnderecoTomador: TRLLabel;
    rlLabel27: TRLLabel;
    rllMunTomador: TRLLabel;
    rlLabel30: TRLLabel;
    rllCNPJTomador: TRLLabel;
    rlLabel32: TRLLabel;
    rllBairroTomador: TRLLabel;
    rlLabel35: TRLLabel;
    rllCEPTomador: TRLLabel;
    rlLabel37: TRLLabel;
    rllFoneTomador: TRLLabel;
    rlLabel40: TRLLabel;
    rllInscEstTomador: TRLLabel;
    rlShape7: TRLDraw;
    rlShape8: TRLDraw;
    rlShape9: TRLDraw;
    rlShape55: TRLDraw;
    rlShape56: TRLDraw;
    rlShape58: TRLDraw;
    rlShape59: TRLDraw;
    rllMsgTeste: TRLLabel;
    rlLabel15: TRLLabel;
    rlSysData1: TRLSysteminfo;
    rllblSistema: TRLLabel;
    procedure rlEventoBeforePrint(Sender: TObject; var PrintReport: boolean);
    procedure rlb_01_TituloBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_02_DocumentoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_05_EventoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_03_EmitenteBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_04_TomadorBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_06_DescricaoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_07_CorrecaoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_08_HeaderItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_09_ItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_10_SistemaBeforePrint(Sender: TObject; var PrintIt: Boolean);
  private
    procedure Itens;
  public
    procedure ProtocoloMDFe(const sProtocolo: string);
  end;

implementation

uses
  StrUtils, DateUtils, ACBrDFeUtil, ACBrUtil, ACBrValidador;

{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}
{$endif}

var
  FProtocoloMDFe: string;

procedure TfrmMDFeDAEventoRLRetrato.Itens;
begin
  // Itens
end;

procedure TfrmMDFeDAEventoRLRetrato.ProtocoloMDFe(const sProtocolo: string);
begin
  FProtocoloMDFe := sProtocolo;
end;

procedure TfrmMDFeDAEventoRLRetrato.rlEventoBeforePrint(Sender: TObject; var PrintReport: boolean);
begin
  inherited;

  Itens;

  RLMDFeEvento.Title := 'Evento: ' + FormatFloat('000,000,000', FEventoMDFe.InfEvento.nSeqEvento);

  with RLMDFeEvento.Margins do
  begin
    TopMargin := FMargemSuperior * 10;
    BottomMargin := FMargemInferior * 10;
    LeftMargin := FMargemEsquerda * 10;
    RightMargin := FMargemDireita * 10;
  end;
end;

procedure TfrmMDFeDAEventoRLRetrato.rlb_01_TituloBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  //  TpcnTpEvento = (teCCe, teCancelamento, teManifDestConfirmacao, teManifDestCiencia,
  //                  teManifDestDesconhecimento, teManifDestOperNaoRealizada,
  //                  teEncerramento, teEPEC, teInclusaoCondutor, teMultiModal);
  case FEventoMDFe.InfEvento.tpEvento of
    teCancelamento:
    begin
      rllLinha1.Caption := 'CANCELAMENTO';
      rllLinha2.Caption := ACBrStr(
        'Não possui valor fiscal, simples representação do Cancelamento indicado abaixo.');
      rllLinha3.Caption := 'CONSULTE A AUTENTICIDADE DO CANCELAMENTO NO SITE DA SEFAZ AUTORIZADORA.';
    end;
    teEncerramento:
    begin
      rllLinha1.Caption := 'ENCERRAMENTO';
      rllLinha2.Caption := ACBrStr(
        'Não possui valor fiscal, simples representação do Encerramento indicado abaixo.');
      rllLinha3.Caption := 'CONSULTE A AUTENTICIDADE DO ENCERRAMENTO NO SITE DA SEFAZ AUTORIZADORA.';
    end;
    teInclusaoCondutor:
    begin
      rllLinha1.Caption := ACBrStr('INCLUSÃO DE CONDUTOR');
      rllLinha2.Caption := ACBrStr(
        'Não possui valor fiscal, simples representação da Inclusão de Condutor indicada abaixo.');
      rllLinha3.Caption := ACBrStr(
        'CONSULTE A AUTENTICIDADE DA INCLUSÃO DE CONDUTOR NO SITE DA SEFAZ AUTORIZADORA.');
    end;
  end;
end;

procedure TfrmMDFeDAEventoRLRetrato.rlb_02_DocumentoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  if FMDFe <> nil then
  begin
    PrintIt := True;

    rllModelo.Caption := FMDFe.ide.modelo;
    rllSerie.Caption := IntToStr(FMDFe.ide.serie);
    rllNumMDFe.Caption := FormatFloat('000,000,000', FMDFe.Ide.nMDF);
    rllEmissao.Caption := FormatDateTimeBr(FMDFe.Ide.dhEmi);
    //SetBarCodeImage(Copy(FMDFe.InfMDFe.Id, 5, 44), rliBarCode);
    rllChave.Caption := FormatarChaveAcesso(Copy(FMDFe.InfMDFe.Id, 5, 44));
  end
  else
    PrintIt := False;
end;

procedure TfrmMDFeDAEventoRLRetrato.rlb_05_EventoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  with FEventoMDFe do
  begin
    case InfEvento.tpEvento of
      teCancelamento: rllTituloEvento.Caption := 'CANCELAMENTO';
      teEncerramento: rllTituloEvento.Caption := 'ENCERRAMENTO';
      teInclusaoCondutor: rllTituloEvento.Caption := ACBrStr('INCLUSÃO DE CONDUTOR');
    end;

    rllOrgao.Caption := IntToStr(InfEvento.cOrgao);
    case InfEvento.tpAmb of
      taProducao: rllTipoAmbiente.Caption := ACBrStr('PRODUÇÃO');
      taHomologacao: rllTipoAmbiente.Caption := ACBrStr('HOMOLOGAÇÃO - SEM VALOR FISCAL');
    end;
    rllEmissaoEvento.Caption := FormatDateTimeBr(InfEvento.dhEvento);
    rllTipoEvento.Caption := InfEvento.TipoEvento;
    rllDescricaoEvento.Caption := InfEvento.DescEvento;
    rllSeqEvento.Caption := IntToStr(InfEvento.nSeqEvento);
    rllStatus.Caption := IntToStr(RetInfEvento.cStat) + ' - ' +
      RetInfEvento.xMotivo;
    rllProtocolo.Caption := RetInfEvento.nProt + ' ' +
      FormatDateTimeBr(RetInfEvento.dhRegEvento);
  end;
end;

procedure TfrmMDFeDAEventoRLRetrato.rlb_03_EmitenteBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  printIt := False;

  if FMDFe <> nil then
  begin
    printIt := True;

    rllRazaoEmitente.Caption := FMDFe.emit.xNome;
    rllCNPJEmitente.Caption := FormatarCNPJouCPF(FMDFe.emit.CNPJCPF);
    rllEnderecoEmitente.Caption := FMDFe.emit.EnderEmit.xLgr + ', ' + FMDFe.emit.EnderEmit.nro;
    rllBairroEmitente.Caption := FMDFe.emit.EnderEmit.xBairro;
    rllCEPEmitente.Caption := FormatarCEP(FMDFe.emit.EnderEmit.CEP);
    rllMunEmitente.Caption := FMDFe.emit.EnderEmit.xMun + ' - ' + FMDFe.emit.EnderEmit.UF;
    rllFoneEmitente.Caption := FormatarFone(FMDFe.emit.enderEmit.fone);
    rllInscEstEmitente.Caption := FMDFe.emit.IE;
  end;
end;

procedure TfrmMDFeDAEventoRLRetrato.rlb_04_TomadorBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  printIt := False;

  if FMDFe <> nil then
  begin
    (*
    printIt := True;
    if FMDFe.Ide.Toma4.xNome = ''
     then begin
      case FMDFe.Ide.Toma03.Toma of
      tmRemetente:
        begin
          rllRazaoTomador.Caption    := FMDFe.Rem.xNome;
          rllCNPJTomador.Caption     := FormatarCNPJCPF(FMDFe.Rem.CNPJCPF);
          rllEnderecoTomador.Caption := FMDFe.Rem.EnderReme.xLgr + ', ' + FMDFe.Rem.EnderReme.nro;
          rllBairroTomador.Caption   := FMDFe.Rem.EnderReme.xBairro;
          rllCEPTomador.Caption      := FormatarCEP(FMDFe.Rem.EnderReme.CEP);
          rllMunTomador.Caption      := FMDFe.Rem.EnderReme.xMun+' - '+FMDFe.Rem.EnderReme.UF;
          rllFoneTomador.Caption     := FormatarFone(FMDFe.Rem.fone);
          rllInscEstTomador.Caption  := FMDFe.Rem.IE;
        end;
      tmExpedidor:
        begin
          rllRazaoTomador.Caption    := FMDFe.Exped.xNome;
          rllCNPJTomador.Caption     := FormatarCNPJCPF(FMDFe.Exped.CNPJCPF);
          rllEnderecoTomador.Caption := FMDFe.Exped.EnderExped.xLgr + ', ' + FMDFe.Exped.EnderExped.nro;
          rllBairroTomador.Caption   := FMDFe.Exped.EnderExped.xBairro;
          rllCEPTomador.Caption      := FormatarCEP(FMDFe.Exped.EnderExped.CEP);
          rllMunTomador.Caption      := FMDFe.Exped.EnderExped.xMun+' - '+FMDFe.Exped.EnderExped.UF;
          rllFoneTomador.Caption     := FormatarFone(FMDFe.Exped.fone);
          rllInscEstTomador.Caption  := FMDFe.Exped.IE;
        end;
      tmRecebedor:
        begin
          rllRazaoTomador.Caption    := FMDFe.Receb.xNome;
          rllCNPJTomador.Caption     := FormatarCNPJCPF(FMDFe.Receb.CNPJCPF);
          rllEnderecoTomador.Caption := FMDFe.Receb.EnderReceb.xLgr + ', ' + FMDFe.Receb.EnderReceb.nro;
          rllBairroTomador.Caption   := FMDFe.Receb.EnderReceb.xBairro;
          rllCEPTomador.Caption      := FormatarCEP(FMDFe.Receb.EnderReceb.CEP);
          rllMunTomador.Caption      := FMDFe.Receb.EnderReceb.xMun+' - '+FMDFe.Receb.EnderReceb.UF;
          rllFoneTomador.Caption     := FormatarFone(FMDFe.Receb.fone);
          rllInscEstTomador.Caption  := FMDFe.Receb.IE;
        end;
      tmDestinatario:
        begin
          rllRazaoTomador.Caption    := FMDFe.Dest.xNome;
          rllCNPJTomador.Caption     := FormatarCNPJCPF(FMDFe.Dest.CNPJCPF);
          rllEnderecoTomador.Caption := FMDFe.Dest.EnderDest.xLgr + ', ' + FMDFe.Dest.EnderDest.nro;
          rllBairroTomador.Caption   := FMDFe.Dest.EnderDest.xBairro;
          rllCEPTomador.Caption      := FormatarCEP(FMDFe.Dest.EnderDest.CEP);
          rllMunTomador.Caption      := FMDFe.Dest.EnderDest.xMun+' - '+FMDFe.Dest.EnderDest.UF;
          rllFoneTomador.Caption     := FormatarFone(FMDFe.Dest.fone);
          rllInscEstTomador.Caption  := FMDFe.Dest.IE;
        end;
      end;
     end
     else begin
      rllRazaoTomador.Caption    := FMDFe.Ide.Toma4.xNome;
      rllCNPJTomador.Caption     := FormatarCNPJCPF(FMDFe.Ide.Toma4.CNPJCPF);
      rllEnderecoTomador.Caption := FMDFe.Ide.Toma4.EnderToma.xLgr + ', ' + FMDFe.Ide.Toma4.EnderToma.nro;
      rllBairroTomador.Caption   := FMDFe.Ide.Toma4.EnderToma.xBairro;
      rllCEPTomador.Caption      := FormatarCEP(FMDFe.Ide.Toma4.EnderToma.CEP);
      rllMunTomador.Caption      := FMDFe.Ide.Toma4.EnderToma.xMun+' - '+FMDFe.Ide.Toma4.EnderToma.UF;
      rllFoneTomador.Caption     := FormatarFone(FMDFe.Ide.Toma4.fone);
      rllInscEstTomador.Caption  := FMDFe.Ide.Toma4.IE;
     end;
   *)
  end;
end;

procedure TfrmMDFeDAEventoRLRetrato.rlb_06_DescricaoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  printIt := (FEventoMDFe.InfEvento.tpEvento = teCancelamento) or
    (FEventoMDFe.InfEvento.tpEvento = teEncerramento) or
    (FEventoMDFe.InfEvento.tpEvento = teInclusaoCondutor) or
    (FEventoMDFe.InfEvento.tpAmb = taHomologacao);

  rllMsgTeste.Visible := False;
  rllMsgTeste.Enabled := False;

  if FEventoMDFe.InfEvento.tpAmb = taHomologacao then
  begin
    rllMsgTeste.Caption := ACBrStr('AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL');
    rllMsgTeste.Visible := True;
    rllMsgTeste.Enabled := True;
  end;

  rlmDescricao.Visible := (FEventoMDFe.InfEvento.tpEvento = teCancelamento) or
    (FEventoMDFe.InfEvento.tpEvento = teEncerramento) or
    (FEventoMDFe.InfEvento.tpEvento = teInclusaoCondutor);
  rlmDescricao.Enabled := (FEventoMDFe.InfEvento.tpEvento = teCancelamento) or
    (FEventoMDFe.InfEvento.tpEvento = teEncerramento) or
    (FEventoMDFe.InfEvento.tpEvento = teInclusaoCondutor);

  rlmDescricao.Lines.Clear;
  case FEventoMDFe.InfEvento.tpEvento of
    teCancelamento:
    begin
      rlmDescricao.Lines.Add('Protocolo do MDFe Cancelado: ' + FEventoMDFe.InfEvento.detEvento.nProt);
      rlmDescricao.Lines.Add('Motivo do Cancelamento     : ' + FEventoMDFe.InfEvento.detEvento.xJust);
    end;
    teEncerramento:
    begin
      rlmDescricao.Lines.Add('Protocolo do MDFe Encerrado: ' + FEventoMDFe.InfEvento.detEvento.nProt);
      rlmDescricao.Lines.Add('Data do Encerramento       : ' +
        DateToStr(FEventoMDFe.InfEvento.detEvento.dtEnc));
      rlmDescricao.Lines.Add(ACBrStr('Código da UF               : ') +
        IntToStr(FEventoMDFe.InfEvento.detEvento.cUF));
      rlmDescricao.Lines.Add(ACBrStr('Código do Município        : ') +
        IntToStr(FEventoMDFe.InfEvento.detEvento.cMun));
    end;
    teInclusaoCondutor:
    begin
      rlmDescricao.Lines.Add('Dados do Motorista');
      rlmDescricao.Lines.Add('CPF : ' + FEventoMDFe.InfEvento.detEvento.CPF);
      rlmDescricao.Lines.Add('Nome: ' + FEventoMDFe.InfEvento.detEvento.xNome);
    end;
  end;
end;

procedure TfrmMDFeDAEventoRLRetrato.rlb_07_CorrecaoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  printIt := False;
end;

procedure TfrmMDFeDAEventoRLRetrato.rlb_08_HeaderItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
  // Imprime os Documentos Originários se o Tipo de MDFe for Normal
end;

procedure TfrmMDFeDAEventoRLRetrato.rlb_09_ItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
  rlb_09_Itens.Enabled := True;
end;

procedure TfrmMDFeDAEventoRLRetrato.rlb_10_SistemaBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;

  rllblSistema.Caption := FSistema + ' - ' + FUsuario;
end;

end.
