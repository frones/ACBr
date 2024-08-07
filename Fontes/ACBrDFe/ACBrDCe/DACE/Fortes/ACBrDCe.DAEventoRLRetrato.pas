{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrDCe.DAEventoRLRetrato;

interface

uses
  SysUtils, Variants, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, Qt, QStdCtrls,
  {$ELSE}
  Messages, Graphics, Controls, Forms, Dialogs, ExtCtrls, MaskUtils, StdCtrls,
  {$ENDIF}
  RLReport, RLFilters, RLPDFFilter,
  RLBarcode, DB, StrUtils, RLRichText, ACBrDCe.DAEventoRL;

type

  { TfrmDCeDAEventoRLRetrato }

  TfrmDCeDAEventoRLRetrato = class(TfrmDCeDAEventoRL)
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
    rlb_04_Destinatario: TRLBand;
    rlb_06_Condicoes: TRLBand;
    lblTitulo_06: TRLLabel;
    RLDraw5: TRLDraw;
    rlmCondicoes: TRLMemo;
    rlsQuadro01: TRLDraw;
    rlsQuadro02: TRLDraw;
    rlsQuadro04: TRLDraw;
    rlsLinhaV10: TRLDraw;
    rlsLinhaV09: TRLDraw;
    rlsLinhaH04: TRLDraw;
    rlsLinhaV01: TRLDraw;
    rlsLinhaH06: TRLDraw;
    rlsLinhaH07: TRLDraw;
    RLDraw10: TRLDraw;
    rlLabel65: TRLLabel;
    RLDraw2: TRLDraw;
    RLDraw46: TRLDraw;
    rllLinha3: TRLLabel;
    rllLinha2: TRLLabel;
    rllLinha1: TRLLabel;
    rlb_02_Documento: TRLBand;
    RLDraw81: TRLDraw;
    RLDraw88: TRLDraw;
    rlsQuadro03: TRLDraw;
    rlLabel8: TRLLabel;
    rllModelo: TRLLabel;
    rlLabel21: TRLLabel;
    rllSerie: TRLLabel;
    rlLabel23: TRLLabel;
    rllNumDCe: TRLLabel;
    rlsLinhaV05: TRLDraw;
    rlsLinhaV06: TRLDraw;
    rlsLinhaV08: TRLDraw;
    rlLabel33: TRLLabel;
    rllEmissao: TRLLabel;
    rlsLinhaV07: TRLDraw;
    rlLabel74: TRLLabel;
    rllChave: TRLLabel;
    rllTituloEvento: TRLLabel;
    RLDraw48: TRLDraw;
    rlLabel9: TRLLabel;
    rllTipoAmbiente: TRLLabel;
    rlLabel6: TRLLabel;
    rllEmissaoEvento: TRLLabel;
    rlLabel28: TRLLabel;
    rllTipoEvento: TRLLabel;
    rlLabel17: TRLLabel;
    rllSeqEvento: TRLLabel;
    RLDraw49: TRLDraw;
    RLDraw50: TRLDraw;
    rlLabel18: TRLLabel;
    rllStatus: TRLLabel;
    rlLabel12: TRLLabel;
    RLDraw51: TRLDraw;
    rlLabel1: TRLLabel;
    RLDraw52: TRLDraw;
    RLDraw53: TRLDraw;
    RLDraw82: TRLDraw;
    RLDraw99: TRLDraw;
    rlLabel4: TRLLabel;
    rllBairroEmitente: TRLLabel;
    RLDraw108: TRLDraw;
    rlLabel5: TRLLabel;
    rllFoneEmitente: TRLLabel;
    RLDraw109: TRLDraw;
    rlLabel14: TRLLabel;
    rllRazaoDestinatario: TRLLabel;
    rlLabel25: TRLLabel;
    rllEnderecoDestinatario: TRLLabel;
    rlLabel27: TRLLabel;
    rllMunDestinatario: TRLLabel;
    rlLabel30: TRLLabel;
    rllCNPJDestinatario: TRLLabel;
    rlLabel32: TRLLabel;
    rllBairroDestinatario: TRLLabel;
    rlLabel35: TRLLabel;
    rllCEPDestinatario: TRLLabel;
    rlLabel37: TRLLabel;
    rllFoneDestinatario: TRLLabel;
    rlLabel40: TRLLabel;
    rllInscEstDestinatario: TRLLabel;
    RLDraw7: TRLDraw;
    RLDraw8: TRLDraw;
    RLDraw9: TRLDraw;
    RLDraw55: TRLDraw;
    RLDraw56: TRLDraw;
    RLDraw58: TRLDraw;
    RLDraw59: TRLDraw;
    rllMsgTeste: TRLLabel;
    rlLabel15: TRLLabel;
    rllblSistema: TRLLabel;
    rliBarCode: TRLBarcode;

    procedure rlb_01_TituloBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_02_DocumentoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_05_EventoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_03_EmitenteBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_04_DestinatarioBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_06_CondicoesBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_09_ItensBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_10_SistemaBeforePrint(Sender: TObject; var PrintIt: boolean);

    procedure RLDCeEventoBeforePrint(Sender: TObject; var PrintIt: Boolean);
  private
    procedure Itens;
  public
    procedure ProtocoloDCe(const sProtocolo: string);
  end;

implementation

uses
  DateUtils, ACBrDFeUtil,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrValidador,
  ACBrXmlBase,
  ACBrDCe.Conversao,
  pcnConversao;

{$IFnDEF FPC}
  {$R *.dfm}

{$ELSE}
  {$R *.lfm}
{$ENDIF}

var
  FProtocoloDCe: string;

procedure TfrmDCeDAEventoRLRetrato.Itens;
begin
  // Itens
end;

procedure TfrmDCeDAEventoRLRetrato.ProtocoloDCe(const sProtocolo: string);
begin
  FProtocoloDCe := sProtocolo;
end;

procedure TfrmDCeDAEventoRLRetrato.rlb_01_TituloBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  case fpEventoDCe.InfEvento.tpEvento of
    teCancelamento:
      rllLinha1.Caption := 'CANCELAMENTO';
  end;

  rllLinha2.Caption := ACBrStr(
    'Não possui valor fiscal, simples representação do evento indicado abaixo.');
  rllLinha3.Caption := ACBrStr(
           'CONSULTE A AUTENTICIDADE DO EVENTO NO SITE DA SEFAZ AUTORIZADORA.');
end;

procedure TfrmDCeDAEventoRLRetrato.rlb_02_DocumentoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  PrintIt := False;

  if fpDCe <> nil then
  begin
    PrintIt := True;

    rllModelo.Caption := IntToStr(fpDCe.ide.modelo);
    rllSerie.Caption := IntToStr(fpDCe.ide.serie);
    rllNumDCe.Caption := FormatFloat('000,000,000', fpDCe.Ide.nDC);
    rllEmissao.Caption := FormatDateTimeBr(fpDCe.Ide.dhEmi);
    rliBarCode.Caption := OnlyNumber(fpDCe.InfDCe.Id);
    rllChave.Caption := FormatarChaveAcesso(OnlyNumber(fpDCe.InfDCe.Id));
  end;
end;

procedure TfrmDCeDAEventoRLRetrato.rlb_05_EventoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  with fpEventoDCe do
  begin
    case InfEvento.tpEvento of
      teCancelamento:
        rllTituloEvento.Caption := 'CANCELAMENTO';
    end;

    rllOrgao.Caption := IntToStr(InfEvento.cOrgao);

    case InfEvento.tpAmb of
      TACBrTipoAmbiente.taProducao:
        rllTipoAmbiente.Caption := ACBrStr('PRODUÇÃO');
      TACBrTipoAmbiente.taHomologacao:
        rllTipoAmbiente.Caption := ACBrStr('HOMOLOGAÇÃO - SEM VALOR FISCAL');
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

procedure TfrmDCeDAEventoRLRetrato.rlb_03_EmitenteBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  PrintIt := False;

  if fpDCe <> nil then
  begin
    PrintIt := True;

    rllRazaoEmitente.Caption := fpDCe.emit.xNome;
    rllCNPJEmitente.Caption := FormatarCNPJouCPF(fpDCe.emit.CNPJCPF);
    rllEnderecoEmitente.Caption :=
      fpDCe.emit.EnderEmit.xLgr + ', ' + fpDCe.emit.EnderEmit.nro;
    rllBairroEmitente.Caption := fpDCe.emit.EnderEmit.xBairro;
    rllCEPEmitente.Caption := FormatarCEP(fpDCe.emit.EnderEmit.CEP);
    rllMunEmitente.Caption := fpDCe.emit.EnderEmit.xMun + ' - ' + fpDCe.emit.EnderEmit.UF;
    rllFoneEmitente.Caption := FormatarFone(fpDCe.emit.enderEmit.fone);
//    rllInscEstEmitente.Caption := fpDCe.emit.IE;
  end;
end;

procedure TfrmDCeDAEventoRLRetrato.rlb_04_DestinatarioBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  PrintIt := False;

  if fpDCe <> nil then
  begin
    PrintIt := True;

    rllRazaoDestinatario.Caption := fpDCe.Dest.xNome;
    rllCNPJDestinatario.Caption := FormatarCNPJouCPF(fpDCe.Dest.CNPJCPF);
    rllEnderecoDestinatario.Caption :=
      fpDCe.Dest.EnderDest.xLgr + ', ' + fpDCe.Dest.EnderDest.nro;
    rllBairroDestinatario.Caption := fpDCe.Dest.EnderDest.xBairro;
    rllCEPDestinatario.Caption := FormatarCEP(fpDCe.Dest.EnderDest.CEP);
    rllMunDestinatario.Caption :=
      fpDCe.Dest.EnderDest.xMun + ' - ' + fpDCe.Dest.EnderDest.UF;
    rllFoneDestinatario.Caption := FormatarFone(fpDCe.Dest.EnderDest.fone);
//    rllInscEstDestinatario.Caption := fpDCe.Dest.IE;
  end;
end;

procedure TfrmDCeDAEventoRLRetrato.rlb_06_CondicoesBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  Exibir: Boolean;
begin
  inherited;

  Exibir := (fpEventoDCe.InfEvento.tpEvento = teCancelamento);

  PrintIt := Exibir or (fpEventoDCe.InfEvento.tpAmb = TACBrTipoAmbiente.taHomologacao);

  rllMsgTeste.Visible := False;
  rllMsgTeste.Enabled := False;

  if fpEventoDCe.InfEvento.tpAmb = TACBrTipoAmbiente.taHomologacao then
  begin
    rllMsgTeste.Caption := ACBrStr('AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL');
    rllMsgTeste.Visible := True;
    rllMsgTeste.Enabled := True;
  end;

  rlmCondicoes.Visible := Exibir;
  rlmCondicoes.Enabled := Exibir;

  rlmCondicoes.Lines.Clear;

  case fpEventoDCe.InfEvento.tpEvento of
    teCancelamento:
    begin
      lblTitulo_06.Caption := ACBrStr('DESCRIÇÃO');
      rlmCondicoes.Lines.Add('Protocolo do DCe Cancelado: ' +
        fpEventoDCe.InfEvento.detEvento.nProt);
      rlmCondicoes.Lines.Add('Motivo do Cancelamento    : ' +
        fpEventoDCe.InfEvento.detEvento.xJust);
      rlmCondicoes.Lines.Add('Chave do DCe Cancelado    : ' +
        fpEventoDCe.InfEvento.chDCe);
    end;
  end;
end;

procedure TfrmDCeDAEventoRLRetrato.rlb_09_ItensBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  rlb_09_Itens.Enabled := True;
end;

procedure TfrmDCeDAEventoRLRetrato.rlb_10_SistemaBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  rllblSistema.Caption := fpDADCe.Sistema + ' - ' + fpDADCe.Usuario;
end;

procedure TfrmDCeDAEventoRLRetrato.RLDCeEventoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  Itens;
  rlDCeEvento.Title := 'Evento: ' + FormatFloat('000,000,000',
    fpEventoDCe.InfEvento.nSeqEvento);
end;

end.
