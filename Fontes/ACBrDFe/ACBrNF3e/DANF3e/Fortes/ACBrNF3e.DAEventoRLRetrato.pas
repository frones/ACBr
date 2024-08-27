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

unit ACBrNF3e.DAEventoRLRetrato;

interface

uses
  SysUtils, Variants, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, Qt, QStdCtrls,
  {$ELSE}
  Messages, Graphics, Controls, Forms, Dialogs, ExtCtrls, MaskUtils, StdCtrls,
  {$ENDIF}
  RLReport, RLFilters, RLPDFFilter,
  RLBarcode, DB, StrUtils, RLRichText, ACBrNF3e.DAEventoRL;

type

  { TfrmNF3eDAEventoRLRetrato }

  TfrmNF3eDAEventoRLRetrato = class(TfrmNF3eDAEventoRL)
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
    rllNumNFCom: TRLLabel;
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
    procedure RLNF3eEventoBeforePrint(Sender: TObject; var PrintIt: Boolean);
  private
    procedure Itens;
  public
    procedure ProtocoloNFCom(const sProtocolo: string);
  end;

implementation

uses
  DateUtils, ACBrDFeUtil,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrValidador,
  ACBrXmlBase,
  ACBrNF3eConversao,
  pcnConversao;

{$IFnDEF FPC}
  {$R *.dfm}

{$ELSE}
  {$R *.lfm}
{$ENDIF}

var
  FProtocoloNFCom: string;

procedure TfrmNF3eDAEventoRLRetrato.Itens;
begin
  // Itens
end;

procedure TfrmNF3eDAEventoRLRetrato.ProtocoloNFCom(const sProtocolo: string);
begin
  FProtocoloNFCom := sProtocolo;
end;

procedure TfrmNF3eDAEventoRLRetrato.rlb_01_TituloBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  case fpEventoNF3e.InfEvento.tpEvento of
    teCancelamento:
      rllLinha1.Caption := 'CANCELAMENTO';
  end;

  rllLinha2.Caption := ACBrStr(
    'Não possui valor fiscal, simples representação do evento indicado abaixo.');
  rllLinha3.Caption := ACBrStr(
           'CONSULTE A AUTENTICIDADE DO EVENTO NO SITE DA SEFAZ AUTORIZADORA.');
end;

procedure TfrmNF3eDAEventoRLRetrato.rlb_02_DocumentoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  PrintIt := False;

  if fpNF3e <> nil then
  begin
    PrintIt := True;

    rllModelo.Caption := IntToStr(fpNF3e.ide.modelo);
    rllSerie.Caption := IntToStr(fpNF3e.ide.serie);
    rllNumNFCom.Caption := FormatFloat('000,000,000', fpNF3e.Ide.nNF);
    rllEmissao.Caption := FormatDateTimeBr(fpNF3e.Ide.dhEmi);
    rliBarCode.Caption := OnlyNumber(fpNF3e.infNF3e.Id);
    rllChave.Caption := FormatarChaveAcesso(OnlyNumber(fpNF3e.infNF3e.Id));
  end;
end;

procedure TfrmNF3eDAEventoRLRetrato.rlb_05_EventoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  Ambiente: Integer;
begin
  inherited;

  with fpEventoNF3e do
  begin
    case InfEvento.tpEvento of
      teCancelamento:
        rllTituloEvento.Caption := 'CANCELAMENTO';
    end;

    rllOrgao.Caption := IntToStr(InfEvento.cOrgao);

    Ambiente := Integer(InfEvento.tpAmb);
    case Ambiente of
      0:
        rllTipoAmbiente.Caption := ACBrStr('PRODUÇÃO');
      1:
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

procedure TfrmNF3eDAEventoRLRetrato.rlb_03_EmitenteBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  PrintIt := False;

  if fpNF3e <> nil then
  begin
    PrintIt := True;

    rllRazaoEmitente.Caption := fpNF3e.emit.xNome;
    rllCNPJEmitente.Caption := FormatarCNPJouCPF(fpNF3e.emit.CNPJ);
    rllEnderecoEmitente.Caption :=
      fpNF3e.emit.EnderEmit.xLgr + ', ' + fpNF3e.emit.EnderEmit.nro;
    rllBairroEmitente.Caption := fpNF3e.emit.EnderEmit.xBairro;
    rllCEPEmitente.Caption := FormatarCEP(fpNF3e.emit.EnderEmit.CEP);
    rllMunEmitente.Caption := fpNF3e.emit.EnderEmit.xMun + ' - ' + fpNF3e.emit.EnderEmit.UF;
    rllFoneEmitente.Caption := FormatarFone(fpNF3e.emit.enderEmit.fone);
//    rllInscEstEmitente.Caption := fpNF3e.emit.IE;
  end;
end;

procedure TfrmNF3eDAEventoRLRetrato.rlb_04_DestinatarioBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  PrintIt := False;

  if fpNF3e <> nil then
  begin
    PrintIt := True;

    rllRazaoDestinatario.Caption := fpNF3e.Dest.xNome;
    rllCNPJDestinatario.Caption := FormatarCNPJouCPF(fpNF3e.Dest.CNPJCPF);
    rllEnderecoDestinatario.Caption :=
      fpNF3e.Dest.EnderDest.xLgr + ', ' + fpNF3e.Dest.EnderDest.nro;
    rllBairroDestinatario.Caption := fpNF3e.Dest.EnderDest.xBairro;
    rllCEPDestinatario.Caption := FormatarCEP(fpNF3e.Dest.EnderDest.CEP);
    rllMunDestinatario.Caption :=
      fpNF3e.Dest.EnderDest.xMun + ' - ' + fpNF3e.Dest.EnderDest.UF;
    rllFoneDestinatario.Caption := FormatarFone(fpNF3e.Dest.EnderDest.fone);
//    rllInscEstDestinatario.Caption := fpNF3e.Dest.IE;
  end;
end;

procedure TfrmNF3eDAEventoRLRetrato.rlb_06_CondicoesBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  Exibir: Boolean;
begin
  inherited;

  Exibir := (fpEventoNF3e.InfEvento.tpEvento = teCancelamento);

  PrintIt := Exibir or (Integer(fpEventoNF3e.InfEvento.tpAmb) = Integer(taHomologacao));

  rllMsgTeste.Visible := False;
  rllMsgTeste.Enabled := False;

  if Integer(fpEventoNF3e.InfEvento.tpAmb) = Integer(taHomologacao) then
  begin
    rllMsgTeste.Caption := ACBrStr('AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL');
    rllMsgTeste.Visible := True;
    rllMsgTeste.Enabled := True;
  end;

  rlmCondicoes.Visible := Exibir;
  rlmCondicoes.Enabled := Exibir;

  rlmCondicoes.Lines.Clear;

  case fpEventoNF3e.InfEvento.tpEvento of
    teCancelamento:
    begin
      lblTitulo_06.Caption := ACBrStr('DESCRIÇÃO');
      rlmCondicoes.Lines.Add('Protocolo do NF3e Cancelado: ' +
        fpEventoNF3e.InfEvento.detEvento.nProt);
      rlmCondicoes.Lines.Add('Motivo do Cancelamento    : ' +
        fpEventoNF3e.InfEvento.detEvento.xJust);
      rlmCondicoes.Lines.Add('Chave do NF3e Cancelado    : ' +
        fpEventoNF3e.InfEvento.chNF3e);
    end;
  end;
end;

procedure TfrmNF3eDAEventoRLRetrato.rlb_09_ItensBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  rlb_09_Itens.Enabled := True;
end;

procedure TfrmNF3eDAEventoRLRetrato.rlb_10_SistemaBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  rllblSistema.Caption := fpDANF3e.Sistema + ' - ' + fpDANF3e.Usuario;
end;

procedure TfrmNF3eDAEventoRLRetrato.RLNF3eEventoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  Itens;
  RLNF3eEvento.Title := 'Evento: ' + FormatFloat('000,000,000',
    fpEventoNF3e.InfEvento.nSeqEvento);
end;

end.
