{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
{                              Juliana Rodrigues Prado                         }
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

unit ACBrGNREGuiaLazReportDm;


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ACBrGNREGuiaClass, LR_Class, LR_DSet, LR_BarC, LR_Shape, LR_RRect, LR_E_HTM,
  lr_e_pdf, PrintersDlgs, Printers, strutils, BufDataset, LResources, PReport,
  DB, pgnreGNRERetorno, maskutils;

const
  CACBrGNREGuiaLazReport_Versao = '0.1.0';

type

  { TACBrGNREGuiaLazReport }

  TACBrGNREGuiaLazReport = class(TACBrGNREGuiaClass)
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure ImprimirGuia(GNRE: TGNRERetorno = nil); override;
    procedure ImprimirGuiaPDF(GNRE: TGNRERetorno = nil); override;
  published
  end;
  { TdmACBrGNREGuiaLazReport }
  TdmACBrGNREGuiaLazReport = class(TDataModule)
    BufGuia: TBufDataset;
    frBarCodeObject1: TfrBarCodeObject;
    frHTMExport1: TfrHTMExport;
    frReport1: TfrReport;
    frShapeObject1: TfrShapeObject;
    frTNPDFExport1: TfrTNPDFExport;
    frUserDataset1: TfrUserDataset;
    PrintDialog1: TPrintDialog;
    constructor Create(AOwner: TComponent); override;
  private
    { private declarations }
    FGNREGuiaClassOwner: TACBrGNREGuiaClass;
    FGNRE: TGNRERetorno;
  public
    { public declarations }
    property GNRE: TGNRERetorno read FGNRE write FGNRE;
    property GNREGuiaClassOwner: TACBrGNREGuiaClass read FGNREGuiaClassOwner;
    procedure CarregarDados;
  end;

procedure Register;

implementation

uses ACBrUtil;

{$R *.lfm}

procedure Register;
begin
  RegisterComponents('ACBrGNRE', [TACBrGNREGuiaLazReport]);
end;

{ TdmACBrGNREGuiaLazReport }

constructor TdmACBrGNREGuiaLazReport.Create(AOwner: TComponent);
begin
  inherited ;
  FGNREGuiaClassOwner := TACBrGNREGuiaClass(AOwner);
end;

procedure TdmACBrGNREGuiaLazReport.CarregarDados;
  function FormatarData(Str: string): string;
  begin
    Result := Copy(Str, 1, 2) + '/' + Copy(Str, 3, 2) + '/' + Copy(Str, 5, 4);
  end;

  function RemoverZeros(Str: string): string;
  begin
    if Str <> '' then
    begin
      while Str[1] = '0' do
        Str := Copy(Str, 2, Length(Str));
    end;

    Result := Str;
  end;

begin
  with BufGuia do
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
      FieldByName('Ambiente').AsInteger := InfoCabec.Ambiente;
      FieldByName('SequencialGuia').AsInteger := SequencialGuia;
      FieldByName('UFFavorecida').AsString := UFFavorecida;
      FieldByName('CodReceita').AsInteger := CodReceita;
      FieldByName('TipoDocEmitente').AsInteger := TipoDocEmitente;

      case TipoDocEmitente of
        1: FieldByName('DocEmitente').AsString := FormatMaskText('000\.000\.000\-00;0', DocEmitente);
        2: FieldByName('DocEmitente').AsString := FormatMaskText('00\.000\.000\/0000\-00;0', DocEmitente);
        3: FieldByName('DocEmitente').AsString := RemoverZeros(DocEmitente);
      end;

      FieldByName('RazaoSocialEmitente').AsString := RazaoSocialEmitente;
      FieldByName('EnderecoEmitente').AsString := EnderecoEmitente;
      FieldByName('MunicipioEmitente').AsString := MunicipioEmitente;
      FieldByName('UFEmitente').AsString := UFEmitente;
      FieldByName('CEPEmitente').AsString := CEPEmitente;
      FieldByName('TelefoneEmitente').AsString := RemoverZeros(TelefoneEmitente);
      FieldByName('TipoDocDestinatario').AsInteger := TipoDocDestinatario;

      case TipoDocDestinatario of
        1: FieldByName('DocDestinatario').AsString := FormatMaskText('000\.000\.000\-00;0', DocDestinatario);
        2: FieldByName('DocDestinatario').AsString := FormatMaskText('00\.000\.000\/0000\-00;0', DocDestinatario);
        3: FieldByName('DocDestinatario').AsString := RemoverZeros(DocDestinatario);
      end;

      FieldByName('MunicipioDestinatario').AsString := MunicipioDestinatario;
      FieldByName('Produto').AsString := Produto;
      FieldByName('NumDocOrigem').AsString := RemoverZeros(NumDocOrigem);
      FieldByName('Convenio').AsString := Convenio;
      FieldByName('InfoComplementares').AsString := InfoComplementares;
      FieldByName('DataVencimento').AsDateTime := StrToDate(FormatarData(DataVencimento));
      FieldByName('DataLimitePagamento').AsDateTime := StrToDate(FormatarData(DataLimitePagamento));
      FieldByName('PeriodoReferencia').AsString := PeriodoReferencia;
      FieldByName('MesAnoReferencia').AsString := MesAnoReferencia;
      FieldByName('Parcela').AsString := IntToStr(Parcela);
      FieldByName('ValorPrincipal').AsCurrency := ValorPrincipal;
      FieldByName('AtualizacaoMonetaria').AsCurrency := AtualizacaoMonetaria;
      FieldByName('Juros').AsCurrency := Juros;
      FieldByName('Multa').AsCurrency := Multa;
      FieldByName('RepresentacaoNumerica').AsString := RepresentacaoNumerica;
      FieldByName('CodigoBarras').AsString := CodigoBarras;
      FieldByName('QtdeVias').AsInteger := QtdeVias;
      FieldByName('NumeroControle').AsString := NumeroControle;
      FieldByName('IdentificadorGuia').AsString := IdentificadorGuia;
      FieldByName('Reservado').AsString := Reservado;
    end;
    Post;
  end;
end;


{ TACBrGNREGuiaLazReport }

constructor TACBrGNREGuiaLazReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TACBrGNREGuiaLazReport.ImprimirGuia(GNRE: TGNRERetorno);
begin
  inherited ImprimirGuia(GNRE);
end;

procedure TACBrGNREGuiaLazReport.ImprimirGuiaPDF(GNRE: TGNRERetorno);
var
  frACBrGNREGuiaLazReport : TdmACBrGNREGuiaLazReport;
  RelGNRE, Dir : string;
  {PageIni, PageFim,} PInd : Integer;
  Res : TLResource ;
  MS  : TMemoryStream ;
begin
  inherited ImprimirGuiaPDF(GNRE);

    frACBrGNREGuiaLazReport := TdmACBrGNREGuiaLazReport.Create(self);
    try
       with frACBrGNREGuiaLazReport do
       begin
          RelGNRE := 'ACBrGNREGuiaLazReport.lrf';
          // Verificando se o Relatório existe no disco //
          Dir := ExtractFilePath(Application.ExeName) ;
          if FileExists( Dir + RelGNRE ) then
             frReport1.LoadFromFile( Dir + RelGNRE )
          else
           begin
             // Lendo Relatório de Resource Interno //
             Res := LazarusResources.Find(RelGNRE,'LRF');  // Le de ACBrBoletoFCLazReport.lrs
             if Res = nil then
                raise Exception.Create('Resource: '+RelGNRE+' não encontrado');

             MS := TMemoryStream.Create ;
             try
                MS.Write(Pointer(Res.Value)^,Length(Res.Value)) ;
                MS.Position := 0;
                frReport1.LoadFromXMLStream( MS );
             finally
                MS.Free ;
             end;
           end ;

          PInd := Printer.PrinterIndex;
          frReport1.ChangePrinter( PInd, 0 );

          if not frReport1.PrepareReport then
             Exit;

          with PrintDialog1 do
          begin
             Options  := [poPageNums];
             Collate  := true;
             //FromPage := 0;
             //ToPage   := fBoletoFC.ACBrBoleto.ListadeBoletos.Count;
             //MaxPage  := ToPage;
             Copies   := NumCopias;
             //PageIni  := FromPage;
             //PageFim  := ToPage;
          end;
          //
          //Case Filtro of
          //  fiNenhum :
          //    begin
          //      if MostrarPreview then
          //         frReport1.ShowReport
          //      else
          //      begin
          //         if MostrarSetup then
          //         begin
          //            if PrintDialog1.Execute then
          //            begin
          //               if (Printer.PrinterIndex <> PInd ) or
          //                  frReport1.CanRebuild            or
          //                  frReport1.ChangePrinter( PInd, Printer.PrinterIndex ) then
          //               begin
          //                  frReport1.PrepareReport
          //               end ;
          //
          //               if PrintDialog1.PrintRange = prPageNums then
          //               begin
          //                  PageIni := PrintDialog1.FromPage;
          //                  PageFim := PrintDialog1.ToPage;
          //               end;
          //            end;
          //         end
          //         else
          //           frReport1.PrepareReport;
          //
          //        frReport1.PrintPreparedReport( IntToStr(PageIni) + '-' +
          //        IntToStr(PageFim), NumCopias );
          //      end;
          //    end ;
          //
          //
          //  fiPDF :
          //    begin
          //      frReport1.ExportTo(TfrTNPDFExportFilter, NomeArquivo) ;
          //    end;
          //end ;
       end;
    finally
       frACBrGNREGuiaLazReport.Free;
    end;
end;


initialization
//{$I ACBrGNREGuiaLazReport.lrs}

end.

