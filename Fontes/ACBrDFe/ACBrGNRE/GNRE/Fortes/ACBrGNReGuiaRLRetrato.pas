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
******************************************************************************}

{$I ACBr.inc}

unit ACBrGNReGuiaRLRetrato;

interface

uses
SysUtils, Variants, Classes, db, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, RLReport, RLBarcode, RLPDFFilter, pcnConversao,
   ACBrGNReGuiaRL,RLFilters,pgnreGNRERetorno,maskutils;

type

  { TfrlGuiaRLRetrato }

    TfrlGuiaRLRetrato = class(TfrlGuiaRL)
      RLBand1: TRLBand;
      RLDraw1: TRLDraw;
      RLLabel1: TRLLabel;
      RLDraw2: TRLDraw;
      RLLabel2: TRLLabel;
      RLLabel3: TRLLabel;
      RLDBText1: TRLDBText;
      RLLabel4: TRLLabel;
      RLDBText2: TRLDBText;
      RLLabel5: TRLLabel;
      RLDBText3: TRLDBText;
      RLDraw3: TRLDraw;
      RLDraw4: TRLDraw;
      RLLabel6: TRLLabel;
      RLDBText4: TRLDBText;
      RLLabel7: TRLLabel;
      RLDBText5: TRLDBText;
      RLLabel8: TRLLabel;
      RLDBText6: TRLDBText;
      RLLabel9: TRLLabel;
      RLDBText7: TRLDBText;
      RLLabel10: TRLLabel;
      RLDBText8: TRLDBText;
      RLLabel11: TRLLabel;
      RLDBText9: TRLDBText;
      RLDraw5: TRLDraw;
      RLDraw6: TRLDraw;
      RLDraw7: TRLDraw;
      RLLabel12: TRLLabel;
      RLDBText10: TRLDBText;
      RLLabel13: TRLLabel;
      RLDBText11: TRLDBText;
      RLLabel14: TRLLabel;
      RLDBText12: TRLDBText;
      RLDraw8: TRLDraw;
      RLDraw9: TRLDraw;
      RLDraw10: TRLDraw;
      RLDraw11: TRLDraw;
      RLDraw12: TRLDraw;
      RLDraw13: TRLDraw;
      RLDraw14: TRLDraw;
      RLDraw15: TRLDraw;
      RLDraw16: TRLDraw;
      RLLabel15: TRLLabel;
      RLLabel16: TRLLabel;
      RLLabel17: TRLLabel;
      RLDBText14: TRLDBText;
      RLLabel18: TRLLabel;
      RLLabel19: TRLLabel;
      RLLabel20: TRLLabel;
      RLLabel21: TRLLabel;
      RLDBText13: TRLDBText;
      RLDBText15: TRLDBText;
      RLLabel22: TRLLabel;
      RLDBMemo1: TRLDBMemo;
      RLLabel23: TRLLabel;
      RLDBText16: TRLDBText;
      RLDBText17: TRLDBText;
      RLDBBarcode1: TRLDBBarcode;
      RLLabel24: TRLLabel;
      RLDBText18: TRLDBText;
      RLLabel25: TRLLabel;
      RLDraw17: TRLDraw;
      RLLabel26: TRLLabel;
      RLLabel27: TRLLabel;
      RLLabel28: TRLLabel;
      RLLabel29: TRLLabel;
      RLLabel30: TRLLabel;
      RLLabel31: TRLLabel;
      RLLabel32: TRLLabel;
      RLLabel33: TRLLabel;
      RLLabel34: TRLLabel;
      RLLabel35: TRLLabel;
      RLLabel36: TRLLabel;
      RLLabel37: TRLLabel;
      RLMemo1: TRLMemo;
      RLMemo2: TRLMemo;
      RLDraw18: TRLDraw;
      RLDraw19: TRLDraw;
      RLDraw20: TRLDraw;
      RLDraw21: TRLDraw;
      RLDraw22: TRLDraw;
      RLDraw23: TRLDraw;
      RLDraw24: TRLDraw;
      RLDraw25: TRLDraw;
      RLDraw26: TRLDraw;
      RLDraw27: TRLDraw;
      RLDraw28: TRLDraw;
      RLDraw29: TRLDraw;
      RLDraw30: TRLDraw;
      RLDraw31: TRLDraw;
      RLDraw32: TRLDraw;
      RLDraw33: TRLDraw;
      RLDraw34: TRLDraw;
      RLMemo3: TRLMemo;
      RLLabel38: TRLLabel;
      RLLabel39: TRLLabel;
      RLLabel40: TRLLabel;
      RLDBText19: TRLDBText;
      RLLabel41: TRLLabel;
      RLDBText20: TRLDBText;
      RLLabel42: TRLLabel;
      RLDBText21: TRLDBText;
      RLLabel43: TRLLabel;
      RLDBText22: TRLDBText;
      RLLabel44: TRLLabel;
      RLDBText23: TRLDBText;
      RLLabel45: TRLLabel;
      RLDBText24: TRLDBText;
      RLLabel46: TRLLabel;
      RLDBText25: TRLDBText;
      RLLabel47: TRLLabel;
      RLDBText26: TRLDBText;
      RLLabel48: TRLLabel;
      RLDBText27: TRLDBText;
      RLLabel49: TRLLabel;
      RLDBText28: TRLDBText;
      RLLabel50: TRLLabel;
      RLDBText29: TRLDBText;
      RLLabel51: TRLLabel;
      RLDBText30: TRLDBText;
      RLLabel52: TRLLabel;
      RLLabel53: TRLLabel;
      RLLabel54: TRLLabel;
      RLDBText31: TRLDBText;
      RLLabel55: TRLLabel;
      RLLabel56: TRLLabel;
      RLLabel57: TRLLabel;
      RLLabel58: TRLLabel;
      RLDBText32: TRLDBText;
      RLDBText33: TRLDBText;
      RLLabel59: TRLLabel;
      RLDBMemo2: TRLDBMemo;
      RLLabel60: TRLLabel;
      RLDBText34: TRLDBText;
      RLDBText35: TRLDBText;
      RLDBBarcode2: TRLDBBarcode;
      RLLabel61: TRLLabel;
      RLDBText36: TRLDBText;
      RLLabel62: TRLLabel;
      RLLabel63: TRLLabel;
      RLLabel64: TRLLabel;
      RLLabel65: TRLLabel;
      RLLabel66: TRLLabel;
      RLLabel67: TRLLabel;
      RLLabel68: TRLLabel;
      RLLabel69: TRLLabel;
      RLLabel70: TRLLabel;
      RLLabel71: TRLLabel;
      RLLabel72: TRLLabel;
      RLMemo4: TRLMemo;
      RLLabel73: TRLLabel;
      RLLabel74: TRLLabel;
      RLDraw35: TRLDraw;
      RLDraw36: TRLDraw;
      RLDraw37: TRLDraw;
      RLDraw38: TRLDraw;
      RLDraw39: TRLDraw;
      RLDraw40: TRLDraw;
      RLDraw41: TRLDraw;
      RLDraw42: TRLDraw;
      RLDraw43: TRLDraw;
      RLDraw44: TRLDraw;
      RLDraw45: TRLDraw;
      RLDraw46: TRLDraw;
      RLDraw47: TRLDraw;
      RLDraw48: TRLDraw;
      RLDraw49: TRLDraw;
      RLDraw50: TRLDraw;
      RLDraw51: TRLDraw;
      RLMemo5: TRLMemo;
      RLLabel75: TRLLabel;
      RLLabel76: TRLLabel;
      RLLabel77: TRLLabel;
      RLDBText37: TRLDBText;
      RLLabel78: TRLLabel;
      RLDBText38: TRLDBText;
      RLLabel79: TRLLabel;
      RLDBText39: TRLDBText;
      RLLabel80: TRLLabel;
      RLDBText40: TRLDBText;
      RLLabel81: TRLLabel;
      RLDBText41: TRLDBText;
      RLLabel82: TRLLabel;
      RLDBText42: TRLDBText;
      RLLabel83: TRLLabel;
      RLDBText43: TRLDBText;
      RLLabel84: TRLLabel;
      RLDBText44: TRLDBText;
      RLLabel85: TRLLabel;
      RLDBText45: TRLDBText;
      RLLabel86: TRLLabel;
      RLDBText46: TRLDBText;
      RLLabel87: TRLLabel;
      RLDBText47: TRLDBText;
      RLLabel88: TRLLabel;
      RLDBText48: TRLDBText;
      RLLabel89: TRLLabel;
      RLLabel90: TRLLabel;
      RLLabel91: TRLLabel;
      RLDBText49: TRLDBText;
      RLLabel92: TRLLabel;
      RLLabel93: TRLLabel;
      RLLabel94: TRLLabel;
      RLLabel95: TRLLabel;
      RLDBText50: TRLDBText;
      RLDBText51: TRLDBText;
      RLLabel96: TRLLabel;
      RLDBMemo3: TRLDBMemo;
      RLLabel97: TRLLabel;
      RLDBText52: TRLDBText;
      RLDBText53: TRLDBText;
      RLDBBarcode3: TRLDBBarcode;
      RLLabel98: TRLLabel;
      RLDBText54: TRLDBText;
      RLLabel99: TRLLabel;
      RLLabel100: TRLLabel;
      RLLabel101: TRLLabel;
      RLLabel102: TRLLabel;
      RLLabel103: TRLLabel;
      RLLabel104: TRLLabel;
      RLLabel105: TRLLabel;
      RLLabel106: TRLLabel;
      RLLabel107: TRLLabel;
      RLLabel108: TRLLabel;
      RLLabel109: TRLLabel;
      RLMemo6: TRLMemo;
      RLLabel110: TRLLabel;
      RLLabel111: TRLLabel;
      procedure RLBand1BeforePrint(Sender: TObject; var PrintIt: Boolean);
    private
    { Private declarations }
    FGNRE: TGNRERetorno;
    procedure Itens;
  public
    { Public declarations }
  published
//  procedure CarregaDados;
//  property GNRE: TGNRERetorno read FGNRE write FGNRE;
  end;


implementation

uses
  StrUtils, DateUtils, pGNReGNRe, ACBrUtil, ACBrDFeUtil, ACBrValidador,ACBrGNRE2;

{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}
{$endif}


var
  nItemControle: integer;

procedure TfrlGuiaRLRetrato.RLBand1BeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  // 1ª Via
  if cdsItens.FieldByName('DocDestinatario').AsString <> '0' then
    RLLabel18.Caption := cdsItens.FieldByName('DocDestinatario').AsString;

  if cdsItens.FieldByName('Parcela').AsString <> '0' then
    RLLabel26.Caption := cdsItens.FieldByName('Parcela').AsString;

  RLLabel28.Caption := FormatFloat('R$ ###,###,###,##0.00',
    cdsItens.FieldByName('ValorPrincipal').AsFloat);

  RLLabel30.Caption := FormatFloat('R$ ###,###,###,##0.00',
    cdsItens.FieldByName('AtualizacaoMonetaria').AsFloat);

  if cdsItens.FieldByName('Juros').AsFloat > 0 then
    RLLabel32.Caption := FormatFloat('R$ ###,###,###,##0.00',
      cdsItens.FieldByName('Juros').AsFloat);

  if cdsItens.FieldByName('Multa').AsFloat > 0 then
    RLLabel34.Caption := FormatFloat('R$ ###,###,###,##0.00',
      cdsItens.FieldByName('Multa').AsFloat);

  RLLabel36.Caption := FormatFloat('R$ ###,###,###,##0.00',
    (cdsItens.FieldByName('ValorPrincipal')
        .AsFloat + cdsItens.FieldByName('AtualizacaoMonetaria')
        .AsFloat + cdsItens.FieldByName('Juros').AsFloat + cdsItens.FieldByName
        ('Multa').AsFloat));

  RLMemo2.Visible := cdsItens.FieldByName('Ambiente').AsInteger = 9;

  // 2ª Via
  if cdsItens.FieldByName('DocDestinatario').AsString <> '0' then
    RLLabel55.Caption := cdsItens.FieldByName('DocDestinatario').AsString;

  if cdsItens.FieldByName('Parcela').AsString <> '0' then
    RLLabel63.Caption := cdsItens.FieldByName('Parcela').AsString;

  RLLabel65.Caption := FormatFloat('R$ ###,###,###,##0.00',
    cdsItens.FieldByName('ValorPrincipal').AsFloat);

  RLLabel66.Caption := FormatFloat('R$ ###,###,###,##0.00',
    cdsItens.FieldByName('AtualizacaoMonetaria').AsFloat);

  if cdsItens.FieldByName('Juros').AsFloat > 0 then
    RLLabel67.Caption := FormatFloat('R$ ###,###,###,##0.00',
      cdsItens.FieldByName('Juros').AsFloat);

  if cdsItens.FieldByName('Multa').AsFloat > 0 then
    RLLabel69.Caption := FormatFloat('R$ ###,###,###,##0.00',
      cdsItens.FieldByName('Multa').AsFloat);

  RLLabel71.Caption := FormatFloat('R$ ###,###,###,##0.00',
    (cdsItens.FieldByName('ValorPrincipal')
        .AsFloat + cdsItens.FieldByName('AtualizacaoMonetaria')
        .AsFloat + cdsItens.FieldByName('Juros').AsFloat + cdsItens.FieldByName
        ('Multa').AsFloat));

  RLMemo3.Visible := cdsItens.FieldByName('Ambiente').AsInteger = 9;

  // 3ª Via
  if cdsItens.FieldByName('DocDestinatario').AsString <> '0' then
    RLLabel92.Caption := cdsItens.FieldByName('DocDestinatario').AsString;

  if cdsItens.FieldByName('Parcela').AsString <> '0' then
    RLLabel100.Caption := cdsItens.FieldByName('Parcela').AsString;

  RLLabel102.Caption := FormatFloat('R$ ###,###,###,##0.00',
    cdsItens.FieldByName('ValorPrincipal').AsFloat);

  RLLabel103.Caption := FormatFloat('R$ ###,###,###,##0.00',
    cdsItens.FieldByName('AtualizacaoMonetaria').AsFloat);

  if cdsItens.FieldByName('Juros').AsFloat > 0 then
    RLLabel104.Caption := FormatFloat('R$ ###,###,###,##0.00',
      cdsItens.FieldByName('Juros').AsFloat);

  if cdsItens.FieldByName('Multa').AsFloat > 0 then
    RLLabel106.Caption := FormatFloat('R$ ###,###,###,##0.00',
      cdsItens.FieldByName('Multa').AsFloat);

  RLLabel108.Caption := FormatFloat('R$ ###,###,###,##0.00',
    (cdsItens.FieldByName('ValorPrincipal').AsFloat + cdsItens.FieldByName
        ('AtualizacaoMonetaria').AsFloat + cdsItens.FieldByName('Juros')
        .AsFloat + cdsItens.FieldByName('Multa').AsFloat));
  RLMemo5.Visible := cdsItens.FieldByName('Ambiente').AsInteger = 9;
end;

procedure TfrlGuiaRLRetrato.Itens;
var
  I, J, nItem: integer;
begin
  //cdsItens.Close;
  //cdsItens.CreateDataSet;
  //cdsItens.Open;
  nItem := 0;

  cdsItens.First;
end;

end.
