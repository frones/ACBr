{******************************************************************************}
{ Projeto: Componente ACBrGNRE                                                 }
{  Biblioteca multiplataforma de componentes Delphi/Lazarus para emissão da    }
{  Guia Nacional de Recolhimento de Tributos Estaduais                         }
{  http://www.gnre.pe.gov.br/                                                  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Claudemir Vitor Pereira                }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                       Juliomar Marchetti                     }
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

{******************************************************************************
|* Historico
|*
|* 09/12/2013 - Claudemir Vitor Pereira
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}
{$I ACBr.inc}

unit ACBrGNREGuiaFRDM;

interface

uses
  SysUtils, Classes, ACBrGNREGuiaClass, pgnreGNRE, frxClass, frxExportPDF, DB,
  DBClient, frxDBSet, pcnConversao, frxBarcode, MaskUtils, pgnreGNRERetorno,
  FmtBcd;

type
  TdmACBrGNREFR = class
    frxPDFExport: TfrxPDFExport;
    cdsGuia: TClientDataSet;
    frxGuia: TfrxDBDataset;
    frxBarCodeObject: TfrxBarCodeObject;
    frxReport: TfrxReport;
  private
    { Private declarations }
    FGNREGuiaClassOwner     : TACBrGNREGuiaClass;
    FGNRE                   : TGNRERetorno;
    FIncorporarFontesPdf    : Boolean;
    FIncorporarBackgroundPdf: Boolean;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property GNRE: TGNRERetorno read FGNRE write FGNRE;
    property GNREGuiaClassOwner: TACBrGNREGuiaClass read FGNREGuiaClassOwner;

    property IncorporarBackgroundPdf: Boolean read FIncorporarBackgroundPdf write FIncorporarBackgroundPdf;
    property IncorporarFontesPdf: Boolean read FIncorporarFontesPdf write FIncorporarFontesPdf;

    procedure CarregaDados;
  end;

var
  dmACBrGNREFR: TdmACBrGNREFR;

implementation

uses ACBrGNRE2, ACBrDFeUtil, StrUtils, Math, pgnreRetConsResLoteGNRE;

  { TdmACBrNFeFR }

procedure TdmACBrGNREFR.CarregaDados;
var
  Referencia : String;

  function FormatarData(Str: string): string;
  begin
    Result := Copy(Str, 1, 2) + '/' + Copy(Str, 3, 2) + '/' + Copy(Str, 5, 4);
  end;

  function RemoverZeros(Str: string): string;
  begin
    if Str <> '' then
    begin
      while (Str <> '') and (Str[1] = '0') do
        Str := Copy(Str, 2, Length(Str));
    end;

    Result := Str;
  end;

begin
  with cdsGuia do
  begin
    Close;
    FieldDefs.Clear;
    FieldDefs.Add('Ambiente', ftInteger);
    FieldDefs.Add('SequencialGuia', ftInteger);
    FieldDefs.Add('UFFavorecida', ftString, 2);
    FieldDefs.Add('CodReceita', ftInteger);
    FieldDefs.Add('TipoDocEmitente', ftInteger);
    FieldDefs.Add('DocEmitente', ftString, 18);
    FieldDefs.Add('RazaoSocialEmitente', ftString, 60);
    FieldDefs.Add('EnderecoEmitente', ftString, 60);
    FieldDefs.Add('MunicipioEmitente', ftString, 50);
    FieldDefs.Add('UFEmitente', ftString, 2);
    FieldDefs.Add('CEPEmitente', ftString, 8);
    FieldDefs.Add('TelefoneEmitente', ftString, 11);
    FieldDefs.Add('TipoDocDestinatario', ftInteger);
    FieldDefs.Add('DocDestinatario', ftString, 18);
    FieldDefs.Add('MunicipioDestinatario', ftString, 50);
    FieldDefs.Add('Produto', ftString, 255);
    FieldDefs.Add('NumDocOrigem', ftString, 18);
    FieldDefs.Add('Convenio', ftString, 30);
    FieldDefs.Add('InfoComplementares', ftString, 300);
    FieldDefs.Add('DataVencimento', ftDate);
    FieldDefs.Add('DataLimitePagamento', ftDate);
    FieldDefs.Add('PeriodoReferencia', ftString, 1);
    FieldDefs.Add('PerMesAnoRef', ftString, 25);
    FieldDefs.Add('MesAnoReferencia', ftString, 6);
    FieldDefs.Add('Parcela', ftString, 2);
    FieldDefs.Add('ValorPrincipal', ftFloat);
    FieldDefs.Add('AtualizacaoMonetaria', ftFloat);
    FieldDefs.Add('Juros', ftFloat);
    FieldDefs.Add('Multa', ftFloat);
    FieldDefs.Add('RepresentacaoNumerica', ftString, 48);
    FieldDefs.Add('CodigoBarras', ftString, 54);
    FieldDefs.Add('QtdeVias', ftInteger);
    FieldDefs.Add('NumeroControle', ftString, 16);
    FieldDefs.Add('IdentificadorGuia', ftString, 10);
    FieldDefs.Add('Reservado', ftString, 126);
    CreateDataSet;
    Append;

    with GNRE do
    begin
      FieldByName('Ambiente').AsInteger        := InfoCabec.Ambiente;
      FieldByName('SequencialGuia').AsInteger  := SequencialGuia;
      FieldByName('UFFavorecida').AsString     := UFFavorecida;
      FieldByName('CodReceita').AsInteger      := CodReceita;
      FieldByName('TipoDocEmitente').AsInteger := TipoDocEmitente;

      case TipoDocEmitente of
        1: FieldByName('DocEmitente').AsString := FormatMaskText('000\.000\.000\-00;0', DocEmitente);
        2: FieldByName('DocEmitente').AsString := FormatMaskText('00\.000\.000\/0000\-00;0', DocEmitente);
        3: FieldByName('DocEmitente').AsString := RemoverZeros(DocEmitente);
      end;

      FieldByName('RazaoSocialEmitente').AsString  := RazaoSocialEmitente;
      FieldByName('EnderecoEmitente').AsString     := EnderecoEmitente;
      FieldByName('MunicipioEmitente').AsString    := MunicipioEmitente;
      FieldByName('UFEmitente').AsString           := UFEmitente;
      FieldByName('CEPEmitente').AsString          := CEPEmitente;
      FieldByName('TelefoneEmitente').AsString     := RemoverZeros(TelefoneEmitente);
      FieldByName('TipoDocDestinatario').AsInteger := TipoDocDestinatario;

      case TipoDocDestinatario of
        1: FieldByName('DocDestinatario').AsString := FormatMaskText('000\.000\.000\-00;0', DocDestinatario);
        2: FieldByName('DocDestinatario').AsString := FormatMaskText('00\.000\.000\/0000\-00;0', DocDestinatario);
        3: FieldByName('DocDestinatario').AsString := RemoverZeros(DocDestinatario);
      end;

      FieldByName('MunicipioDestinatario').AsString := MunicipioDestinatario;
      FieldByName('Produto').AsString               := Produto;
      FieldByName('NumDocOrigem').AsString          := RemoverZeros(NumDocOrigem);
      FieldByName('Convenio').AsString              := Convenio;
      FieldByName('InfoComplementares').AsString    := InfoComplementares;
      FieldByName('DataVencimento').AsDateTime      := StrToDate(FormatarData(DataVencimento));

      if DataLimitePagamento = '00000000' then
        FieldByName('DataLimitePagamento').AsDateTime := FieldByName('DataVencimento').AsDateTime
      else
        FieldByName('DataLimitePagamento').AsDateTime := StrToDate(FormatarData(DataLimitePagamento));

      FieldByName('PeriodoReferencia').AsString      := PeriodoReferencia;
      FieldByName('MesAnoReferencia').AsString       := MesAnoReferencia;
      FieldByName('Parcela').AsString                := IntToStr(Parcela);
      FieldByName('ValorPrincipal').AsCurrency       := ValorPrincipal;
      FieldByName('AtualizacaoMonetaria').AsCurrency := AtualizacaoMonetaria;
      FieldByName('Juros').AsCurrency                := Juros;
      FieldByName('Multa').AsCurrency                := Multa;
      FieldByName('RepresentacaoNumerica').AsString  := RepresentacaoNumerica;
      FieldByName('CodigoBarras').AsString           := CodigoBarras;
      FieldByName('QtdeVias').AsInteger              := QtdeVias;
      FieldByName('NumeroControle').AsString         := NumeroControle;
      FieldByName('IdentificadorGuia').AsString      := IdentificadorGuia;
      FieldByName('Reservado').AsString              := Reservado;

      if Trim(FieldByName('PeriodoReferencia').AsString) <> '' then
      begin
        case FieldByName('PeriodoReferencia').AsInteger of
          0: Referencia := 'Mensal';
          1: Referencia := '1a Quinzena';
          2: Referencia := '2a Quinzena';
          3: Referencia := '1o Decêndio';
          4: Referencia := '2o Decêndio';
          5: Referencia := '3o Decêndio';
        end;

        Referencia := Referencia + '-';
      end;

      FieldByName('PerMesAnoRef').AsString := Referencia
                                            + LeftStr(FieldByName('MesAnoReferencia').AsString, 2)
                                            + '/'
                                            + RightStr(FieldByName('MesAnoReferencia').AsString, 4);
    end;
    Post;
  end;
end;

constructor TdmACBrGNREFR.Create(AOwner: TComponent);
begin
  FGNREGuiaClassOwner := TACBrGNREGuiaClass(AOwner);

  frxReport := TfrxReport.Create(nil);

  with frxReport do
  begin
    // Version = '5.5.8'
    DotMatrixReport           := False;
    IniFile                   := '\Software\Fast Reports';
    PreviewOptions.AllowEdit  := False;
    PreviewOptions.Buttons    := [pbPrint, pbLoad, pbSave, pbExport, pbZoom, pbFind, pbOutline, pbPageSetup, pbTools, pbNavigator, pbExportQuick];
    PreviewOptions.Zoom       := 1.000000000000000000;
    PrintOptions.Printer      := 'Default';
    PrintOptions.PrintOnSheet := 0;
    // ReportOptions.CreateDate = 40401.475989294000000000
    // ReportOptions.LastChange = 41075.662367303240000000
    ScriptLanguage := 'PascalScript';
    StoreInDFM     := False;
  end;

  frxPDFExport := TfrxPDFExport.Create(nil);
  with frxPDFExport do
  begin
    Background      := IncorporarBackgroundPdf;
    EmbeddedFonts   := IncorporarFontesPdf;
    Subject         := 'Exportando DANFE para PDF';
    UseFileCache    := True;
    ShowProgress    := True;
    OverwritePrompt := False;
    DataOnly        := False;
    PrintOptimized  := True;
    Outline         := False;
    HTMLTags        := True;
    Quality         := 95;
    Author          := 'FastReport';
    ProtectionFlags := [ePrint, eModify, eCopy, eAnnot];
    HideToolbar     := False;
    HideMenubar     := False;
    HideWindowUI    := False;
    FitWindow       := False;
    CenterWindow    := False;
    PrintScaling    := False;
  end;

  cdsGuia := TClientDataSet.Create(nil);
  frxGuia := TfrxDBDataset.Create(nil);
  with frxGuia do
  begin
    UserName        := 'Guia';
    CloseDataSource := False;
    OpenDataSource  := False;
    DataSet         := cdsGuia;
    BCDToCurrency   := False;
  end;
  frxBarCodeObject := TfrxBarCodeObject.Create(nil);

  frxReport.EnabledDataSets.Clear;
  frxReport.DataSets.Clear;

  frxReport.EnabledDataSets.Add(frxGuia);
  frxReport.DataSets.Add(frxGuia);
end;

destructor TdmACBrGNREFR.Destroy;
begin
  frxPDFExport.Free;
  cdsGuia.Free;
  frxGuia.Free;
  frxBarCodeObject.Free;
  frxReport.Free;
  inherited;
end;

end.
