{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Mark dos Santos Gonçalves                       }
{                              Juliomar Marchetti                              }
{                              André Ferreira de Moraes                        }
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

unit ACBrCTeDAEventoRLRetrato;

interface

uses
  SysUtils, Variants, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, Qt, QStdCtrls,
  {$ELSE}
  Messages, Graphics, Controls, Forms, Dialogs, ExtCtrls, MaskUtils, StdCtrls,
  {$ENDIF}
  RLReport, RLFilters, RLPDFFilter,
  pcnConversao, RLBarcode, DB, StrUtils, RLRichText, ACBrCTeDAEventoRL;

type

  { TfrmCTeDAEventoRLRetrato }

  TfrmCTeDAEventoRLRetrato = class(TfrmCTeDAEventoRL)
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
    rlb_06_Condicoes: TRLBand;
    rlLabel38: TRLLabel;
    rlLabel44: TRLLabel;
    rlmGrupoAlterado: TRLMemo;
    rlLabel46: TRLLabel;
    rlmCampoAlterado: TRLMemo;
    rlLabel42: TRLLabel;
    rlmValorAlterado: TRLMemo;
    rlLabel45: TRLLabel;
    rlmNumItemAlterado: TRLMemo;
    RLDraw18: TRLDraw;
    RLDraw17: TRLDraw;
    RLDraw15: TRLDraw;
    RLDraw19: TRLDraw;
    lblTitulo_06: TRLLabel;
    RLDraw5: TRLDraw;
    rlmCondicoes: TRLMemo;
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
    RLDraw10: TRLDraw;
    rlLabel65: TRLLabel;
    RLDraw2: TRLDraw;
    rlb_07_Correcao: TRLBand;
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
    rllNumCte: TRLLabel;
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
    procedure rlb_04_TomadorBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_06_CondicoesBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_07_CorrecaoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_08_HeaderItensBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_09_ItensBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_10_SistemaBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure RLCTeEventoBeforePrint(Sender: TObject; var PrintIt: boolean);
  private
    procedure Itens;
  public
    procedure ProtocoloCTe(const sProtocolo: string);
  end;

implementation

uses
  DateUtils, ACBrDFeUtil,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrValidador, pcteConversaoCTe;

{$IFnDEF FPC}
  {$R *.dfm}

{$ELSE}
  {$R *.lfm}
{$ENDIF}

var
  FProtocoloCTe: string;

procedure TfrmCTeDAEventoRLRetrato.Itens;
begin
  // Itens
end;

procedure TfrmCTeDAEventoRLRetrato.ProtocoloCTe(const sProtocolo: string);
begin
  FProtocoloCTe := sProtocolo;
end;

procedure TfrmCTeDAEventoRLRetrato.rlb_01_TituloBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  case fpEventoCTe.InfEvento.tpEvento of
    teCCe:
      rllLinha1.Caption := ACBrStr('CARTA DE CORREÇÃO ELETRÔNICA');

    teCancelamento:
      rllLinha1.Caption := 'CANCELAMENTO';

    teEPEC:
      rllLinha1.Caption := ACBrStr('EVENTO PRÉVIO DE EMISSÃO EM CONTINGÊNCIA - EPEC');

    teMultiModal:
      rllLinha1.Caption := 'REGISTROS DO MULTIMODAL';

    tePrestDesacordo:
      rllLinha1.Caption := 'PRESTAÇÃO DE SERVIÇO EM DESACORDO';

    teCancPrestDesacordo:
      rllLinha1.Caption := 'CANCELAMENTO PRESTAÇÃO DE SERVIÇO EM DESACORDO';

    teGTV:
      rllLinha1.Caption := 'INFORMAÇÕES DA GTV';

    teComprEntrega:
      rllLinha1.Caption := 'COMPROVANTE DE ENTREGA';

    teCancComprEntrega:
      rllLinha1.Caption := 'CANCELAMENTO DO COMPROVANTE DE ENTREGA';

    teInsucessoEntregaCTe:
      rllLinha1.Caption := 'INSUCESSO NA ENTREGA';

    teCancInsucessoEntregaCTe:
      rllLinha1.Caption := 'CANCELAMENTO DO INSUCESSO NA ENTREGA';
  end;

  rllLinha2.Caption := ACBrStr(
    'Não possui valor fiscal, simples representação do evento indicado abaixo.');
  rllLinha3.Caption := ACBrStr(
           'CONSULTE A AUTENTICIDADE DO EVENTO NO SITE DA SEFAZ AUTORIZADORA.');
end;

procedure TfrmCTeDAEventoRLRetrato.rlb_02_DocumentoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  PrintIt := False;

  if fpCTe <> nil then
  begin
    PrintIt := True;

    rllModelo.Caption := IntToStr(fpCTe.ide.modelo);
    rllSerie.Caption := IntToStr(fpCTe.ide.serie);
    rllNumCTe.Caption := FormatFloat('000,000,000', fpCTe.Ide.nCT);
    rllEmissao.Caption := FormatDateTimeBr(fpCTe.Ide.dhEmi);
    rliBarCode.Caption := OnlyNumber(fpCTe.InfCTe.Id);
    rllChave.Caption := FormatarChaveAcesso(OnlyNumber(fpCTe.InfCTe.Id));
  end;
end;

procedure TfrmCTeDAEventoRLRetrato.rlb_05_EventoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  with fpEventoCTe do
  begin
    case InfEvento.tpEvento of
      teCCe:
        rllTituloEvento.Caption := ACBrStr('CARTA DE CORREÇÃO ELETRÔNICA');

      teCancelamento:
        rllTituloEvento.Caption := 'CANCELAMENTO';

      teEPEC:
        rllTituloEvento.Caption := ACBrStr('EVENTO PRÉVIO DE EMISSÃO EM CONTINGÊNCIA');

      teMultiModal:
        rllTituloEvento.Caption := 'REGISTROS DO MULTIMODAL';

      tePrestDesacordo:
        rllTituloEvento.Caption := ACBrStr('PRESTAÇÃO DE SERVIÇO EM DESACORDO');

      teCancPrestDesacordo:
        rllTituloEvento.Caption := ACBrStr('CANCELAMENTO PRESTAÇÃO DE SERVIÇO EM DESACORDO');

      teGTV:
        rllTituloEvento.Caption := 'INFORMAÇÕES DA GTV';

      teComprEntrega:
        rllTituloEvento.Caption := 'COMPROVANTE DE ENTREGA';

      teCancComprEntrega:
        rllTituloEvento.Caption := 'CANCELAMENTO DO COMPROVANTE DE ENTREGA';

      teInsucessoEntregaCTe:
        rllTituloEvento.Caption := 'INSUCESSO NA ENTREGA';

      teCancInsucessoEntregaCTe:
        rllTituloEvento.Caption := 'CANCELAMENTO DO INSUCESSO NA ENTREGA';
    end;

    rllOrgao.Caption := IntToStr(InfEvento.cOrgao);

    case InfEvento.tpAmb of
      taProducao:
        rllTipoAmbiente.Caption := ACBrStr('PRODUÇÃO');
      taHomologacao:
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

procedure TfrmCTeDAEventoRLRetrato.rlb_03_EmitenteBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  PrintIt := False;

  if fpCTe <> nil then
  begin
    PrintIt := True;

    rllRazaoEmitente.Caption := fpCTe.emit.xNome;
    rllCNPJEmitente.Caption := FormatarCNPJouCPF(fpCTe.emit.CNPJ);
    rllEnderecoEmitente.Caption :=
      fpCTe.emit.EnderEmit.xLgr + ', ' + fpCTe.emit.EnderEmit.nro;
    rllBairroEmitente.Caption := fpCTe.emit.EnderEmit.xBairro;
    rllCEPEmitente.Caption := FormatarCEP(fpCTe.emit.EnderEmit.CEP);
    rllMunEmitente.Caption := fpCTe.emit.EnderEmit.xMun + ' - ' + fpCTe.emit.EnderEmit.UF;
    rllFoneEmitente.Caption := FormatarFone(fpCTe.emit.enderEmit.fone);
    rllInscEstEmitente.Caption := fpCTe.emit.IE;
  end;
end;

procedure TfrmCTeDAEventoRLRetrato.rlb_04_TomadorBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  PrintIt := False;

  if fpCTe <> nil then
  begin
    PrintIt := True;
    case fpCTe.Ide.modelo of
      57:
      begin
        if fpCTe.Ide.Toma4.xNome = '' then
        begin
          case fpCTe.Ide.Toma03.Toma of
            tmRemetente:
            begin
              rllRazaoTomador.Caption := fpCTe.Rem.xNome;
              rllCNPJTomador.Caption := FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
              rllEnderecoTomador.Caption :=
                fpCTe.Rem.EnderReme.xLgr + ', ' + fpCTe.Rem.EnderReme.nro;
              rllBairroTomador.Caption := fpCTe.Rem.EnderReme.xBairro;
              rllCEPTomador.Caption := FormatarCEP(fpCTe.Rem.EnderReme.CEP);
              rllMunTomador.Caption :=
                fpCTe.Rem.EnderReme.xMun + ' - ' + fpCTe.Rem.EnderReme.UF;
              rllFoneTomador.Caption := FormatarFone(fpCTe.Rem.fone);
              rllInscEstTomador.Caption := fpCTe.Rem.IE;
            end;

            tmExpedidor:
            begin
              rllRazaoTomador.Caption := fpCTe.Exped.xNome;
              rllCNPJTomador.Caption := FormatarCNPJouCPF(fpCTe.Exped.CNPJCPF);
              rllEnderecoTomador.Caption :=
                fpCTe.Exped.EnderExped.xLgr + ', ' + fpCTe.Exped.EnderExped.nro;
              rllBairroTomador.Caption := fpCTe.Exped.EnderExped.xBairro;
              rllCEPTomador.Caption := FormatarCEP(fpCTe.Exped.EnderExped.CEP);
              rllMunTomador.Caption :=
                fpCTe.Exped.EnderExped.xMun + ' - ' + fpCTe.Exped.EnderExped.UF;
              rllFoneTomador.Caption := FormatarFone(fpCTe.Exped.fone);
              rllInscEstTomador.Caption := fpCTe.Exped.IE;
            end;

            tmRecebedor:
            begin
              rllRazaoTomador.Caption := fpCTe.Receb.xNome;
              rllCNPJTomador.Caption := FormatarCNPJouCPF(fpCTe.Receb.CNPJCPF);
              rllEnderecoTomador.Caption :=
                fpCTe.Receb.EnderReceb.xLgr + ', ' + fpCTe.Receb.EnderReceb.nro;
              rllBairroTomador.Caption := fpCTe.Receb.EnderReceb.xBairro;
              rllCEPTomador.Caption := FormatarCEP(fpCTe.Receb.EnderReceb.CEP);
              rllMunTomador.Caption :=
                fpCTe.Receb.EnderReceb.xMun + ' - ' + fpCTe.Receb.EnderReceb.UF;
              rllFoneTomador.Caption := FormatarFone(fpCTe.Receb.fone);
              rllInscEstTomador.Caption := fpCTe.Receb.IE;
            end;

            tmDestinatario:
            begin
              rllRazaoTomador.Caption := fpCTe.Dest.xNome;
              rllCNPJTomador.Caption := FormatarCNPJouCPF(fpCTe.Dest.CNPJCPF);
              rllEnderecoTomador.Caption :=
                fpCTe.Dest.EnderDest.xLgr + ', ' + fpCTe.Dest.EnderDest.nro;
              rllBairroTomador.Caption := fpCTe.Dest.EnderDest.xBairro;
              rllCEPTomador.Caption := FormatarCEP(fpCTe.Dest.EnderDest.CEP);
              rllMunTomador.Caption :=
                fpCTe.Dest.EnderDest.xMun + ' - ' + fpCTe.Dest.EnderDest.UF;
              rllFoneTomador.Caption := FormatarFone(fpCTe.Dest.fone);
              rllInscEstTomador.Caption := fpCTe.Dest.IE;
            end;
          end;
        end
        else
        begin
          rllRazaoTomador.Caption := fpCTe.Ide.Toma4.xNome;
          rllCNPJTomador.Caption := FormatarCNPJouCPF(fpCTe.Ide.Toma4.CNPJCPF);
          rllEnderecoTomador.Caption :=
            fpCTe.Ide.Toma4.EnderToma.xLgr + ', ' + fpCTe.Ide.Toma4.EnderToma.nro;
          rllBairroTomador.Caption := fpCTe.Ide.Toma4.EnderToma.xBairro;
          rllCEPTomador.Caption := FormatarCEP(fpCTe.Ide.Toma4.EnderToma.CEP);
          rllMunTomador.Caption :=
            fpCTe.Ide.Toma4.EnderToma.xMun + ' - ' + fpCTe.Ide.Toma4.EnderToma.UF;
          rllFoneTomador.Caption := FormatarFone(fpCTe.Ide.Toma4.fone);
          rllInscEstTomador.Caption := fpCTe.Ide.Toma4.IE;
        end;
      end;
      67:
      begin
        rllRazaoTomador.Caption := fpCTe.toma.xNome;
        rllCNPJTomador.Caption := FormatarCNPJouCPF(fpCTe.toma.CNPJCPF);
        rllEnderecoTomador.Caption :=
          fpCTe.toma.EnderToma.xLgr + ', ' + fpCTe.toma.EnderToma.nro;
        rllBairroTomador.Caption := fpCTe.toma.EnderToma.xBairro;
        rllCEPTomador.Caption := FormatarCEP(fpCTe.toma.EnderToma.CEP);
        rllMunTomador.Caption :=
          fpCTe.toma.EnderToma.xMun + ' - ' + fpCTe.toma.EnderToma.UF;
        rllFoneTomador.Caption := FormatarFone(fpCTe.toma.fone);
        rllInscEstTomador.Caption := fpCTe.toma.IE;
      end;
    end;
  end;
end;

procedure TfrmCTeDAEventoRLRetrato.rlb_06_CondicoesBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  Exibir: Boolean;
begin
  inherited;

  Exibir := (fpEventoCTe.InfEvento.tpEvento = teCCe) or
            (fpEventoCTe.InfEvento.tpEvento = teCancelamento) or
            (fpEventoCTe.InfEvento.tpEvento = teEPEC) or
            (fpEventoCTe.InfEvento.tpEvento = teMultiModal) or
            (fpEventoCTe.InfEvento.tpEvento = tePrestDesacordo) or
            (fpEventoCTe.InfEvento.tpEvento = teCancPrestDesacordo) or
            (fpEventoCTe.InfEvento.tpEvento = teGTV) or
            (fpEventoCTe.InfEvento.tpEvento = teComprEntrega) or
            (fpEventoCTe.InfEvento.tpEvento = teCancComprEntrega) or
            (fpEventoCTe.InfEvento.tpEvento = teInsucessoEntregaCTe) or
            (fpEventoCTe.InfEvento.tpEvento = teCancInsucessoEntregaCTe);

  PrintIt := Exibir or (fpEventoCTe.InfEvento.tpAmb = taHomologacao);

  rllMsgTeste.Visible := False;
  rllMsgTeste.Enabled := False;

  if fpEventoCTe.InfEvento.tpAmb = taHomologacao then
  begin
    rllMsgTeste.Caption := ACBrStr('AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL');
    rllMsgTeste.Visible := True;
    rllMsgTeste.Enabled := True;
  end;

  rlmCondicoes.Visible := Exibir;
  rlmCondicoes.Enabled := Exibir;

  rlmCondicoes.Lines.Clear;

  case fpEventoCTe.InfEvento.tpEvento of
    teCCe:
    begin
      lblTitulo_06.Caption := ACBrStr('CONDIÇÕES DE USO');
      rlmCondicoes.Lines.Add(ACBrStr(
        'A Carta de Correção e disciplinada pelo Art. 58-B do CONVENIO/SINIEF 06/89: Fica permitida a utilizacao de carta de correcao, para regularização'));
      rlmCondicoes.Lines.Add(ACBrStr(
        'de erro ocorrido na emissão de documentos fiscais relativos a prestação de serviço de transporte, desde que o erro nao esteja relacionado com:'));
      rlmCondicoes.Lines.Add(ACBrStr(
        'I - as variaveis que determinam o valor do imposto tais como: base de calculo, alíquota, diferença de preço, quantidade, valor da prestação;'));
      rlmCondicoes.Lines.Add(ACBrStr(
        'II - a correção de dados cadastrais que implique mudanca do emitente, tomador, remetente ou do destinatário;'));
      rlmCondicoes.Lines.Add(ACBrStr('III - a data de emissão ou de saída.'));
    end;

    teCancelamento:
    begin
      lblTitulo_06.Caption := ACBrStr('DESCRIÇÃO');
      rlmCondicoes.Lines.Add('Protocolo do CTe Cancelado: ' +
        fpEventoCTe.InfEvento.detEvento.nProt);
      rlmCondicoes.Lines.Add('Motivo do Cancelamento    : ' +
        fpEventoCTe.InfEvento.detEvento.xJust);
      rlmCondicoes.Lines.Add('Chave do CTe Cancelado    : ' +
        fpEventoCTe.InfEvento.chCTe);
    end;

    teEPEC:
    begin
      lblTitulo_06.Caption := ACBrStr('DESCRIÇÃO');
      rlmCondicoes.Lines.Add('Motivo do EPEC     : ' +
        fpEventoCTe.InfEvento.detEvento.xJust);
      rlmCondicoes.Lines.Add('Valor do ICMS      : ' + FormatFloat(
        '#0.00', fpEventoCTe.InfEvento.detEvento.vICMS));
      rlmCondicoes.Lines.Add(ACBrStr('Valor da Prestação : ') +
        FormatFloat('#0.00', fpEventoCTe.InfEvento.detEvento.vTPrest));
      rlmCondicoes.Lines.Add('Valor da Carga     : ' + FormatFloat(
        '#0.00', fpEventoCTe.InfEvento.detEvento.vCarga));
      rlmCondicoes.Lines.Add(ACBrStr('UF de inicio/fim da prestação: ') +
        fpEventoCTe.InfEvento.detEvento.UFIni + ' / ' +
        fpEventoCTe.InfEvento.detEvento.UFFim);
    end;

    teMultiModal:
    begin
      lblTitulo_06.Caption := ACBrStr('DESCRIÇÃO');
      rlmCondicoes.Lines.Add('Registro : ' + fpEventoCTe.InfEvento.detEvento.xRegistro);
      rlmCondicoes.Lines.Add('Documento: ' + fpEventoCTe.InfEvento.detEvento.nDoc);
    end;

    tePrestDesacordo:
    begin
      lblTitulo_06.Caption := ACBrStr('JUSTIFICATIVA');
      rlmCondicoes.Lines.Add(fpEventoCTe.InfEvento.detEvento.xOBS);
    end;

    teCancPrestDesacordo:
    begin
      lblTitulo_06.Caption := ACBrStr('DESCRIÇÃO');
      rlmCondicoes.Lines.Add('Protocolo do Evento de Prestação Cancelado: ' +
        fpEventoCTe.InfEvento.detEvento.nProt);
    end;

    teGTV:
    begin
      lblTitulo_06.Caption := ACBrStr('INFORMAÇÕES DO GTV');

      if fpEventoCTe.InfEvento.detEvento.infGTV.Count > 0 then
      begin
        rlmCondicoes.Lines.Add('Documento    : ' + fpEventoCTe.InfEvento.detEvento.infGTV.Items[0].nDoc);
        rlmCondicoes.Lines.Add('Identificador: ' + fpEventoCTe.InfEvento.detEvento.infGTV.Items[0].id);
      end;
    end;

    teComprEntrega:
    begin
      lblTitulo_06.Caption := ACBrStr('DESCRIÇÃO');
      rlmCondicoes.Lines.Add('Documento: ' + fpEventoCTe.InfEvento.detEvento.nDoc);
      rlmCondicoes.Lines.Add('Nome     : ' + fpEventoCTe.InfEvento.detEvento.xNome);
      rlmCondicoes.Lines.Add('Latitude : ' + FormatFloat('#0.000000', fpEventoCTe.InfEvento.detEvento.latitude));
      rlmCondicoes.Lines.Add('Longitude: ' + FormatFloat('#0.000000', fpEventoCTe.InfEvento.detEvento.longitude));
    end;

    teCancComprEntrega:
    begin
      lblTitulo_06.Caption := ACBrStr('DESCRIÇÃO');
      rlmCondicoes.Lines.Add('Protocolo do Evento de Cancelamento: ' +
        fpEventoCTe.InfEvento.detEvento.nProtCE);
    end;

    teInsucessoEntregaCTe:
    begin
      lblTitulo_06.Caption := ACBrStr('DESCRIÇÃO');
      rlmCondicoes.Lines.Add('Protocolo: ' + fpEventoCTe.InfEvento.detEvento.nProt);
      rlmCondicoes.Lines.Add('Data/Hora: ' + DateTimeToStr(fpEventoCTe.InfEvento.detEvento.dhTentativaEntrega));
      rlmCondicoes.Lines.Add('Tentativa: ' + IntToStr(fpEventoCTe.InfEvento.detEvento.nTentativa));
      rlmCondicoes.Lines.Add('Latitude : ' + FormatFloat('#0.000000', fpEventoCTe.InfEvento.detEvento.latitude));
      rlmCondicoes.Lines.Add('Longitude: ' + FormatFloat('#0.000000', fpEventoCTe.InfEvento.detEvento.longitude));

      if fpEventoCTe.InfEvento.detEvento.tpMotivo = tmOutro then
        rlmCondicoes.Lines.Add('Motivo   : ' + fpEventoCTe.InfEvento.detEvento.xJustMotivo)
      else
        rlmCondicoes.Lines.Add('Motivo   : ' + tpMotivoToDesc(fpEventoCTe.InfEvento.detEvento.tpMotivo));
    end;

    teCancInsucessoEntregaCTe:
    begin
      lblTitulo_06.Caption := ACBrStr('DESCRIÇÃO');
      rlmCondicoes.Lines.Add('Protocolo do Evento de Cancelamento: ' +
        fpEventoCTe.InfEvento.detEvento.nProt);
    end;
  end;
end;

procedure TfrmCTeDAEventoRLRetrato.rlb_07_CorrecaoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  i: integer;
begin
  inherited;

  PrintIt := fpEventoCTe.InfEvento.tpEvento = teCCe;

  rlmNumItemAlterado.Lines.Clear;
  rlmGrupoAlterado.Lines.Clear;
  rlmCampoAlterado.Lines.Clear;
  rlmValorAlterado.Lines.Clear;

  for i := 0 to (fpEventoCTe.InfEvento.detEvento.infCorrecao.Count - 1) do
  begin
    rlmNumItemAlterado.Lines.Add(
      IntToStr(fpEventoCTe.InfEvento.detEvento.infCorrecao[i].nroItemAlterado));
    rlmGrupoAlterado.Lines.Add(
      fpEventoCTe.InfEvento.detEvento.infCorrecao[i].grupoAlterado);
    rlmCampoAlterado.Lines.Add(
      fpEventoCTe.InfEvento.detEvento.infCorrecao[i].campoAlterado);
    rlmValorAlterado.Lines.Add(
      fpEventoCTe.InfEvento.detEvento.infCorrecao[i].valorAlterado);
  end;
end;

procedure TfrmCTeDAEventoRLRetrato.rlb_08_HeaderItensBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  // Imprime os Documentos Originários se o Tipo de CTe for Normal
end;

procedure TfrmCTeDAEventoRLRetrato.rlb_09_ItensBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  rlb_09_Itens.Enabled := True;
end;

procedure TfrmCTeDAEventoRLRetrato.rlb_10_SistemaBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  rllblSistema.Caption := fpDACTe.Sistema + ' - ' + fpDACTe.Usuario;
end;

procedure TfrmCTeDAEventoRLRetrato.RLCTeEventoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  Itens;
  rlCTeEvento.Title := 'Evento: ' + FormatFloat('000,000,000',
    fpEventoCTe.InfEvento.nSeqEvento);
end;

end.
