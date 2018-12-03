{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - NFe - http://www.nfe.fazenda.gov.br            }

{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrNFeDAInutRLRetrato;

interface

uses
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, Qt,
  {$ELSE}
  Graphics, Controls, Forms,
  {$ENDIF}
  ACBrNFeDAInutRL,
  RLReport;

type

  { TfrmNFeDAInutRLRetrato }

  TfrmNFeDAInutRLRetrato = class(TfrmNFeDAInutRL)
    rlb_01_Titulo: TRLBand;
    rllProtocolo: TRLLabel;
    rllOrgao: TRLLabel;
    rllDescricao: TRLLabel;
    rlLabel2: TRLLabel;
    rlLabel78: TRLLabel;
    rllModelo: TRLLabel;
    rlb_07_Rodape: TRLBand;
    rlb_03_Inutilizacao: TRLBand;
    rlsLinhaV10: TRLDraw;
    rlsLinhaV09: TRLDraw;
    rlsLinhaH04: TRLDraw;
    rlsLinhaV01: TRLDraw;
    rllLinha3: TRLLabel;
    rllLinha2: TRLLabel;
    rllLinha1: TRLLabel;
    rlShape88: TRLDraw;
    rllTituloEvento: TRLLabel;
    rlShape48: TRLDraw;
    rlLabel9: TRLLabel;
    rllTipoAmbiente: TRLLabel;
    rlLabel6: TRLLabel;
    rllSerie: TRLLabel;
    rlLabel28: TRLLabel;
    rllAno: TRLLabel;
    rlLabel17: TRLLabel;
    rllNumeracao: TRLLabel;
    rlShape49: TRLDraw;
    rlShape50: TRLDraw;
    rlLabel18: TRLLabel;
    rllStatus: TRLLabel;
    rlb_02_Emitente: TRLBand;
    rlsLinhaH07: TRLDraw;
    rlsLinhaH06: TRLDraw;
    rllRazaoEmitente: TRLLabel;
    rllMunEmitente: TRLLabel;
    rllInscEstEmitente: TRLLabel;
    rllEnderecoEmitente: TRLLabel;
    rllCNPJEmitente: TRLLabel;
    rllCEPEmitente: TRLLabel;
    rlLabel98: TRLLabel;
    rlLabel93: TRLLabel;
    rlLabel24: TRLLabel;
    rlLabel22: TRLLabel;
    rlLabel16: TRLLabel;
    rlLabel13: TRLLabel;
    rlLabel12: TRLLabel;
    rlShape51: TRLDraw;
    rlShape53: TRLDraw;
    rlShape82: TRLDraw;
    rlShape99: TRLDraw;
    rlLabel4: TRLLabel;
    rllBairroEmitente: TRLLabel;
    rlShape108: TRLDraw;
    rlLabel5: TRLLabel;
    rllFoneEmitente: TRLLabel;
    rlShape109: TRLDraw;
    rllblSistema: TRLLabel;
    rlShape1: TRLDraw;
    rlLabel15: TRLLabel;
    rlShape2: TRLDraw;
    rlLabel1: TRLLabel;
    rllJustificativa: TRLMemo;
    procedure RLInutBeforePrint(Sender: TObject; var PrintReport: Boolean);
    procedure rlb_03_InutilizacaoBeforePrint(Sender: TObject; var PrintBand: Boolean);
    procedure rlb_07_RodapeBeforePrint(Sender: TObject; var PrintBand: Boolean);
  private
  public
  end;

implementation

uses
  StrUtils, DateUtils,
  pcnConversao,
  ACBrUtil;

{$IfNDef FPC}
  {$R *.dfm}
{$Else}
  {$R *.lfm}
{$EndIf}

procedure TfrmNFeDAInutRLRetrato.RLInutBeforePrint(Sender: TObject; var PrintReport: Boolean);
begin
  inherited;

  RLNFeInut.Title := ACBrStr('Inutilização');
end;

procedure TfrmNFeDAInutRLRetrato.rlb_03_InutilizacaoBeforePrint(Sender: TObject; var PrintBand: Boolean);
begin
  inherited;

  with fpInutNFe do
  begin
    rllOrgao.Caption := IntToStr(RetInutNFe.cUF);

    case RetInutNFe.tpAmb of
      taProducao:
        rllTipoAmbiente.Caption := ACBrStr('PRODUÇÃO');
      taHomologacao:
        rllTipoAmbiente.Caption := ACBrStr('HOMOLOGAÇÃO - SEM VALOR FISCAL');
    end;

    rllAno.Caption := IntToStr(RetInutNFe.ano);
    rllModelo.Caption := IntToStr(RetInutNFe.Modelo);
    rllSerie.Caption := IntToStr(RetInutNFe.Serie);
    rllNumeracao.Caption := IntToStr(RetInutNFe.nNFIni) + ' a ' + IntToStr(RetInutNFe.nNFFin);

    rllStatus.Caption := IntToStr(RetInutNFe.cStat) + ' - ' + RetInutNFe.xMotivo;
    rllProtocolo.Caption := RetInutNFe.nProt + ' ' + FormatDateTimeBr(RetInutNFe.dhRecbto);

    rllJustificativa.Lines.Text := RetInutNFe.xJust;
  end;
end;

procedure TfrmNFeDAInutRLRetrato.rlb_07_RodapeBeforePrint(Sender: TObject; var PrintBand: Boolean);
begin
  inherited;

  if (fpDANFe.Sistema <> EmptyStr) or (fpDANFe.Usuario <> EmptyStr) then
    rllblSistema.Caption := fpDANFe.Sistema + ' - ' + fpDANFe.Usuario;
end;

end.

