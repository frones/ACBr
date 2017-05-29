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

unit ACBrGNReGuiaRL;

interface

uses
Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, pgnreConversao,pgnreGNRERetorno,pgnreGNRE, ACBrGNRE2,
  RLReport, RLFilters, RLPrinters, RLPDFFilter, RLConsts,
  {$IFDEF BORLAND} DBClient, {$ELSE} BufDataset, {$ENDIF} DB;

type

  { TfrlGuiaRL }

  TfrlGuiaRL = class(TForm)
  dsItens: TDatasource;
  RLGNRe: TRLReport;
  RLPDFFilter1: TRLPDFFilter;
  procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  protected
    FACBrGNRe: TACBrGNRE;
    FGNRe: TGNRERetorno;
    FEmail: string;
    FFax: string;
    FNumCopias: integer;
    FSistema: string;
    FSite: string;
    FUsuario: string;
    AfterPreview: boolean;
    ChangedPos: boolean;
    FSemValorFiscal: boolean;
    FMargemSuperior: double;
    FMargemInferior: double;
    FMargemEsquerda: double;
    FMargemDireita: double;
    FImpressora: string;
    cdsItens:  {$IFDEF BORLAND} TClientDataSet {$ELSE} TBufDataset{$ENDIF};
    procedure ConfigDataSet;
    procedure CarregaDados;
  public
    { Public declarations }
    class procedure Imprimir(AOwner: TComponent;
      AGNRe: TGNRERetorno;
      AEmail: string = '';
      AFax: string = '';
      ANumCopias: integer = 1;
      ASistema: string = '';
      ASite: string = '';
      AUsuario: string = '';
      APreview: boolean = True;
      AMargemSuperior: double = 0.8;
      AMargemInferior: double = 0.8;
      AMargemEsquerda: double = 0.6;
      AMargemDireita: double = 0.51;
      AImpressora: string = '';
      APrintDialog  : Boolean = True  );

    class procedure SavePDF(AOwner: TComponent;
      AFile: string;
      AGNRe: TGNRERetorno;
      AEmail: string = '';
      AFax: string = '';
      ANumCopias: integer = 1;
      ASistema: string = '';
      ASite: string = '';
      AUsuario: string = '';
      AMargemSuperior: double = 0.8;
      AMargemInferior: double = 0.8;
      AMargemEsquerda: double = 0.6;
      AMargemDireita: double = 0.51);
  end;

implementation

uses
  MaskUtils, ACBrUtil;

{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}
{$endif}

class procedure TfrlGuiaRL.Imprimir(AOwner: TComponent;
  AGNRe: TGNRERetorno;
  AEmail: string = '';
  AFax: string = '';
  ANumCopias: integer = 1;
  ASistema: string = '';
  ASite: string = '';
  AUsuario: string = '';
  APreview: boolean = True;
  AMargemSuperior: double = 0.8;
  AMargemInferior: double = 0.8;
  AMargemEsquerda: double = 0.6;
  AMargemDireita: double = 0.51;
  AImpressora: string = '';
  APrintDialog: Boolean = True);
begin
  with Create(AOwner) do
    //with TfrlGuiaRL do
    try
      FGNRe := AGNRe;
      FEmail := AEmail;
      FNumCopias := ANumCopias;
      FMargemSuperior := AMargemSuperior;
      FMargemInferior := AMargemInferior;
      FMargemEsquerda := AMargemEsquerda;
      FMargemDireita := AMargemDireita;
      FImpressora := AImpressora;

      ConfigDataSet;
      CarregaDados;

      if FImpressora > '' then
        RLPrinter.PrinterName := FImpressora;

      if FNumCopias > 0 then
        RLPrinter.Copies := FNumCopias
      else
        RLPrinter.Copies := 1;

      RLGNRe.PrintDialog := APrintDialog;
      if APreview = True then
        RLGNRe.PreviewModal
      else
        RLGNRe.Print;
    finally
      Free;
    end;
end;

class procedure TfrlGuiaRL.SavePDF(AOwner: TComponent;
  AFile: string;
  AGNRe: TGNRERetorno;
  AEmail: string = '';
  AFax: string = '';
  ANumCopias: integer = 1;
  ASistema: string = '';
  ASite: string = '';
  AUsuario: string = '';
  AMargemSuperior: double = 0.8;
  AMargemInferior: double = 0.8;
  AMargemEsquerda: double = 0.6;
  AMargemDireita: double = 0.51);
begin
  with Create(AOwner) do
    //with TfrlGuiaRL do
    try
      FGNRe := AGNRe;
      FEmail := AEmail;
      FFax := AFax;
      FNumCopias := ANumCopias;
      FSistema := ASistema;
      FSite := ASite;
      FUsuario := AUsuario;
      FMargemSuperior := AMargemSuperior;
      FMargemInferior := AMargemInferior;
      FMargemEsquerda := AMargemEsquerda;
      FMargemDireita := AMargemDireita;

      with RLPDFFilter1.DocumentInfo do
      begin
//        Title := ACBrStr('Guia - GNRe nº ') +  FGNRe.
        //KeyWords := ACBrStr('Número:') + FGNRe.c42_identificadorGuia;
          //ACBrStr('; Data de emissão: ') + FormatDateTime('dd/mm/yyyy', FGNRe.Ide.dhEmi) +'; CNPJ: ' + FGNRe.emit.CNPJ;
      end;

      RLGNRe.SaveToFile(AFile);
    finally
      Free;
    end;
end;


procedure TfrlGuiaRL.FormCreate(Sender: TObject);
begin
//  ConfigDataSet;
end;

procedure TfrlGuiaRL.ConfigDataSet;
begin
  if not Assigned(cdsItens) then
    cdsItens := {$IFDEF BORLAND}  TClientDataSet.create(nil)  {$ELSE} TBufDataset.Create(nil){$ENDIF};

  if cdsItens.Active then
  begin
 {$IFDEF BORLAND}
  if cdsItens is TClientDataSet then
  TClientDataSet(cdsItens).EmptyDataSet;
 {$ENDIF}
    cdsItens.Active := False;
  end;

 {$IFDEF BORLAND}
 if cdsItens is TClientDataSet then
  begin
  TClientDataSet(cdsItens).StoreDefs := False;
  TClientDataSet(cdsItens).IndexDefs.Clear;
  TClientDataSet(cdsItens).IndexFieldNames := '';
  TClientDataSet(cdsItens).IndexName := '';
  TClientDataSet(cdsItens).Aggregates.Clear;
  TClientDataSet(cdsItens).AggFields.Clear;
  end;
 {$ELSE}
  if cdsItens is TBufDataset then
  begin
    TBufDataset(cdsItens).IndexDefs.Clear;
    TBufDataset(cdsItens).IndexFieldNames := '';
    TBufDataset(cdsItens).IndexName := '';
  end;
 {$ENDIF}

  with cdsItens do
    if FieldCount = 0 then
    begin
      FieldDefs.Clear;
      Fields.Clear;
      FieldDefs.Add('CHAVE1', ftString, 84);
      FieldDefs.Add('CHAVE2', ftString, 84);

   {$IFDEF BORLAND}
    if cdsItens is TClientDataSet then
    TClientDataSet(cdsItens).CreateDataSet;
   {$ELSE}
      if cdsItens is TBufDataset then
        TBufDataset(cdsItens).CreateDataSet;
   {$ENDIF}
    end;

 {$IFDEF BORLAND}
  if cdsItens is TClientDataSet then
  TClientDataSet(cdsItens).StoreDefs := False;
 {$ENDIF}

  if not cdsItens.Active then
    cdsItens.Active := True;

  {$IFDEF BORLAND}
   if cdsItens is TClientDataSet then
   if cdsItens.Active then
   TClientDataSet(cdsItens).LogChanges := False;
 {$ENDIF}

  dsItens.dataset := cdsItens;
end;

procedure TfrlGuiaRL.CarregaDados;
  function FormatarData(Str: string): string;
  begin
    Result := Copy(Str, 1, 2) + '/' + Copy(Str, 3, 2) + '/' + Copy(Str, 5, 4);
  end;

  function RemoverZeros(Str: string): string;
  begin
    while (Trim(Str) <> '') and (Str[1] = '0') do
      Str := Copy(Str, 2, Length(Str));

    Result := Trim(Str);
  end;

  var
  sReferencia : String;

begin

  cdsItens.Close;
  cdsItens.FieldDefs.Clear;
  cdsItens.Fields.Clear;
  cdsItens.FieldDefs.Add('Ambiente', ftInteger);
  cdsItens.FieldDefs.Add('SequencialGuia', ftInteger);
  cdsItens.FieldDefs.Add('UFFavorecida', ftString, 2);
  cdsItens.FieldDefs.Add('CodReceita', ftInteger);
  cdsItens.FieldDefs.Add('TipoDocEmitente', ftInteger);
  cdsItens.FieldDefs.Add('DocEmitente', ftString, 18);
  cdsItens.FieldDefs.Add('RazaoSocialEmitente', ftString, 60);
  cdsItens.FieldDefs.Add('EnderecoEmitente', ftString, 60);
  cdsItens.FieldDefs.Add('MunicipioEmitente', ftString, 50);
  cdsItens.FieldDefs.Add('UFEmitente', ftString, 2);
  cdsItens.FieldDefs.Add('CEPEmitente', ftString, 8);
  cdsItens.FieldDefs.Add('TelefoneEmitente', ftString, 11);
  cdsItens.FieldDefs.Add('TipoDocDestinatario', ftInteger);
  cdsItens.FieldDefs.Add('DocDestinatario', ftString, 18);
  cdsItens.FieldDefs.Add('MunicipioDestinatario', ftString, 50);
  cdsItens.FieldDefs.Add('Produto', ftString, 255);
  cdsItens.FieldDefs.Add('NumDocOrigem', ftString, 18);
  cdsItens.FieldDefs.Add('Convenio', ftString, 30);
  cdsItens.FieldDefs.Add('InfoComplementares', ftString, 300);
  cdsItens.FieldDefs.Add('DataVencimento', ftDate);
  cdsItens.FieldDefs.Add('DataLimitePagamento', ftDate);
  cdsItens.FieldDefs.Add('PeriodoReferencia', ftString, 1);
  cdsItens.FieldDefs.Add('MesAnoReferencia', ftString, 6);
  cdsItens.FieldDefs.Add('Parcela', ftString, 2);
  cdsItens.FieldDefs.Add('ValorPrincipal', ftFloat);
  cdsItens.FieldDefs.Add('AtualizacaoMonetaria', ftFloat);
  cdsItens.FieldDefs.Add('Juros', ftFloat);
  cdsItens.FieldDefs.Add('Multa', ftFloat);
  cdsItens.FieldDefs.Add('RepresentacaoNumerica', ftString, 48);
  cdsItens.FieldDefs.Add('CodigoBarras', ftString, 54);
  cdsItens.FieldDefs.Add('QtdeVias', ftInteger);
  cdsItens.FieldDefs.Add('NumeroControle', ftString, 16);
  cdsItens.FieldDefs.Add('IdentificadorGuia', ftString, 10);
  cdsItens.FieldDefs.Add('Reservado', ftString, 126);
  cdsItens.FieldDefs.Add('PerMesAnoRef', ftString, 20);

  {$IFDEF BORLAND}
   if cdsItens is TClientDataSet then
   TClientDataSet(cdsItens).CreateDataSet;
  {$ELSE}
     if cdsItens is TBufDataset then
       TBufDataset(cdsItens).CreateDataSet;
  {$ENDIF}
  cdsItens.Append;

  with FGNRE do
  begin
    cdsItens.FieldByName('Ambiente').AsInteger := InfoCabec.Ambiente;
    cdsItens.FieldByName('SequencialGuia').AsInteger := SequencialGuia;
    cdsItens.FieldByName('UFFavorecida').AsString := UFFavorecida;
    cdsItens.FieldByName('CodReceita').AsInteger := CodReceita;
    cdsItens.FieldByName('TipoDocEmitente').AsInteger := TipoDocEmitente;

    case TipoDocEmitente of
      1:
        cdsItens.FieldByName('DocEmitente').AsString := FormatMaskText
          ('000\.000\.000\-00;0', DocEmitente);
      2:
        cdsItens.FieldByName('DocEmitente').AsString := FormatMaskText
          ('00\.000\.000\/0000\-00;0', DocEmitente);
      3:
        cdsItens.FieldByName('DocEmitente').AsString := RemoverZeros(DocEmitente);
    end;

    cdsItens.FieldByName('RazaoSocialEmitente').AsString := RazaoSocialEmitente;
    cdsItens.FieldByName('EnderecoEmitente').AsString := EnderecoEmitente;
    cdsItens.FieldByName('MunicipioEmitente').AsString := MunicipioEmitente;
    cdsItens.FieldByName('UFEmitente').AsString := UFEmitente;
    cdsItens.FieldByName('CEPEmitente').AsString := CEPEmitente;
    cdsItens.FieldByName('TelefoneEmitente').AsString      := RemoverZeros(TelefoneEmitente);
    cdsItens.FieldByName('TipoDocDestinatario').AsInteger  := TipoDocDestinatario;

    case TipoDocDestinatario of
      1:
        cdsItens.FieldByName('DocDestinatario').AsString := FormatMaskText
          ('000\.000\.000\-00;0', DocDestinatario);
      2:
        cdsItens.FieldByName('DocDestinatario').AsString := FormatMaskText
          ('00\.000\.000\/0000\-00;0', DocDestinatario);
      3:
        cdsItens.FieldByName('DocDestinatario').AsString := RemoverZeros
          (DocDestinatario);
    end;

    cdsItens.FieldByName('MunicipioDestinatario').AsString := MunicipioDestinatario;
    cdsItens.FieldByName('Produto').AsString := Produto;
    cdsItens.FieldByName('NumDocOrigem').AsString := RemoverZeros(NumDocOrigem);
    cdsItens.FieldByName('Convenio').AsString := Convenio;
    cdsItens.FieldByName('InfoComplementares').AsString := InfoComplementares;
    cdsItens.FieldByName('DataVencimento').AsDateTime := StrToDateDef(FormatarData(DataVencimento), 0);
    cdsItens.FieldByName('DataLimitePagamento').AsDateTime := StrToDateDef(FormatarData(DataLimitePagamento), cdsItens.FieldByName('DataVencimento').AsDateTime);
    cdsItens.FieldByName('PeriodoReferencia').AsString := PeriodoReferencia ;
    cdsItens.FieldByName('MesAnoReferencia').AsString := MesAnoReferencia;
    cdsItens.FieldByName('Parcela').AsString := IntToStr(Parcela);
    cdsItens.FieldByName('ValorPrincipal').AsCurrency := ValorPrincipal;
    cdsItens.FieldByName('AtualizacaoMonetaria').AsCurrency := AtualizacaoMonetaria;
    cdsItens.FieldByName('Juros').AsCurrency := Juros;
    cdsItens.FieldByName('Multa').AsCurrency := Multa;
    cdsItens.FieldByName('RepresentacaoNumerica').AsString := RepresentacaoNumerica;
    cdsItens.FieldByName('CodigoBarras').AsString := CodigoBarras;
    cdsItens.FieldByName('QtdeVias').AsInteger := QtdeVias;
    cdsItens.FieldByName('NumeroControle').AsString := NumeroControle;
    cdsItens.FieldByName('IdentificadorGuia').AsString := IdentificadorGuia;
    cdsItens.FieldByName('Reservado').AsString := Reservado;

    if Trim(cdsItens.FieldByName('PeriodoReferencia').Text) = '' then
      sReferencia := 'Mensal'
    else
      Case cdsItens.FieldByName('PeriodoReferencia').AsInteger of
        0: sReferencia := 'Mensal';
        1: sReferencia := '1a Quinzena';
        2: sReferencia := '2a Quinzena';
        3: sReferencia := '1o Decêndio';
        4: sReferencia := '2o Decêndio';
        5: sReferencia := '3o Decêndio';
      end;

    cdsItens.FieldByName('PerMesAnoRef').AsString := sReferencia + '-' + Copy(cdsItens.FieldByName('MesAnoReferencia').AsString,
                                                                              1, Length(cdsItens.FieldByName('MesAnoReferencia').AsString)-4) +
                                                                              '/' +
                                                                              Copy(cdsItens.FieldByName('MesAnoReferencia').AsString,
                                                                              Length(cdsItens.FieldByName('MesAnoReferencia').AsString)-3,
                                                                              Length(cdsItens.FieldByName('MesAnoReferencia').AsString));


  end;
  cdsItens.Post;
end;

end.
