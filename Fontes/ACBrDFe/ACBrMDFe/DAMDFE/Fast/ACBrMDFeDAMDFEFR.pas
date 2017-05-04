{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{ Biblioteca multiplataforma de componentes Delphi                             }
{                                                                              }
{ Você pode obter a última versão desse arquivo na pagina do Projeto ACBr      }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
{                                                                              }
{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{ Praça Anita Costa, 34 - Tatuí - SP - 18270-410                               }
{                                                                              }
{******************************************************************************}
{******************************************************************************
  |* Historico
  |*
  |* 18/10/2013: Jeanny Paiva Lopes
  |*  - Inicio do desenvolvimento DAMDFE FastReport
 ******************************************************************************}

{$I ACBr.inc}

unit ACBrMDFeDAMDFEFR;

interface

uses
  SysUtils, Classes, DB, DBClient, ACBrMDFeDAMDFeClass, pcnConversao,
  pmdfeMDFe, frxClass, ACBrDFeUtil, pmdfeEnvEventoMDFe, frxDBSet,
  frxExportPDF, frxBarcode;

type
  EACBrMDFeDAMDFEFR = class(Exception);

  TACBrMDFeDAMDFEFR = class(TACBrMDFeDAMDFEClass)
  private
    FDAMDFEClassOwner: TACBrMDFeDAMDFeClass;
    FMDFe            : TMDFe;
    FEvento          : TEventoMDFe;
    FFastFile:            string;
    FFastFileEvento:      string;
    FEspessuraBorda:      Integer;
    FSelecionaImpressora: Boolean;
    FTamanhoCanhoto:      Extended;
    FAlturaEstampaFiscal: Extended;

    cdsIdentificacao: TClientDataSet;
    cdsEmitente: TClientDataSet;
    cdsModalAereo: TClientDataSet;
    cdsModalAqua: TClientDataSet;
    cdsDocumentos: TClientDataSet;
    cdsMunCarrega: TClientDataSet;
    cdsModalRodo: TClientDataSet;
    cdsParametros: TClientDataSet;
    cdsPercurso: TClientDataSet;
    cdsModalFerrov: TClientDataSet;
    cdsModalFerrovVagoes: TClientDataSet;
    cdsEventos: TClientDataSet;
    cdsTermCarrega: TClientDataSet;
    cdsTermDescarrega: TClientDataSet;
    cdsEmbarcaComboio: TClientDataSet;
    cdsInfUnidCargaVazia: TClientDataSet;

    frxIdentificacao: TfrxDBDataset;
    frxEmitente: TfrxDBDataset;
    frxModalAereo: TfrxDBDataset;
    frxModalAqua: TfrxDBDataset;
    frxDocumentos: TfrxDBDataset;
    frxMunCarrega: TfrxDBDataset;
    frxModalRodo: TfrxDBDataset;
    frxParametros: TfrxDBDataset;
    frxPercurso: TfrxDBDataset;
    frxModalFerrov: TfrxDBDataset;
    frxModalFerrovVagoes: TfrxDBDataset;
    frxEventos: TfrxDBDataset;
    frxTermCarrega: TfrxDBDataset;
    frxTermDescarrega: TfrxDBDataset;
    frxEmbarcaComboio: TfrxDBDataset;
    frxInfUnidCargaVazia: TfrxDBDataset;

    procedure CarregaIdentificacao;
    procedure CarregaParametros;
    procedure CarregaEmitente;
    procedure CarregaDocumentos;
    procedure CarregaModal;
    procedure CarregaModalRodoviario;
    procedure CarregaModalAereo;
    procedure CarregaModalAquaviario;

    procedure CarregaTermCarreg;
    procedure CarregaTermDescarreg;
    procedure CarregaEmbarcacaoComboio;
    procedure CarregaInfUnidCargaVazia;

    procedure CarregaModalFerroviario;
    procedure CarregaMunCarrega;
    procedure CarregaPercurso;

    function  GetPreparedReport: TfrxReport;
    function  GetPreparedReportEvento: TfrxReport;
    function  PrepareReport(MDFe: TMDFe = nil): Boolean;
    function  PrepareReportEvento: Boolean;
    procedure CriarDataSetsFrx;
  public
    frxReport: TfrxReport;
    frxPDFExport: TfrxPDFExport;
    frxBarCodeObject: TfrxBarCodeObject;
    VersaoDAMDFe: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirDAMDFe(MDFe: TMDFe = nil); override;
    procedure ImprimirDAMDFePDF(MDFe: TMDFe = nil); override;
    procedure ImprimirEVENTO(MDFe: TMDFe = nil); override;
    procedure ImprimirEVENTOPDF(MDFe: TMDFe = nil); override;

    procedure CarregaDados;
    procedure LimpaDados;
    procedure CarregaDadosEventos;
    procedure SetDataSetsToFrxReport;
    procedure frxReportGetValue(const VarName: string; var Value: Variant);

    property MDFe            : TMDFe read FMDFe write FMDFe;
    property Evento          : TEventoMDFe read FEvento write FEvento;
    property DAMDFEClassOwner: TACBrMDFeDAMDFeClass read FDAMDFEClassOwner;
  published
    property FastFile:             string read FFastFile write FFastFile;
    property FastFileEvento:       string read FFastFileEvento write FFastFileEvento;
    property SelecionaImpressora:  Boolean read FSelecionaImpressora write FSelecionaImpressora;
    property EspessuraBorda:       Integer read FEspessuraBorda write FEspessuraBorda;
    property TamanhoCanhoto:       Extended read FTamanhoCanhoto write FTamanhoCanhoto;
    property AlturaEstampaFiscal:  Extended read FAlturaEstampaFiscal write FAlturaEstampaFiscal;
    property PreparedReport:       TfrxReport read GetPreparedReport;
    property PreparedReportEvento: TfrxReport read GetPreparedReportEvento;
  end;

type
  TSplitResult = array of string;

implementation

uses ACBrMDFe, ACBrUtil, StrUtils, pmdfeConversaoMDFe, ACBrValidador;

function SubstrCount(const ASubString, AString: string): Integer;
var
  i: integer;
begin
  Result := -1;
  i := 0;
  repeat
    Inc(Result);
    i := PosEx(ASubString, AString, i + 1);
  until i = 0;
end;

function Split(const ADelimiter, AString: string): TSplitResult;
var
  Step: ^string;
  Chr: PChar;
  iPos, iLast, iDelLen, iLen, x: integer;
label
  EndLoop;
begin
  SetLength(Result, SubstrCount(ADelimiter, AString) + 1);
  if High(Result) = 0 then
    Result[0] := AString
  else
  begin
    iDelLen := PCardinal(Cardinal(ADelimiter) - SizeOf(Cardinal))^;
    iLen := PCardinal(Cardinal(AString) - SizeOf(Cardinal))^;
    Step := @Result[0];
    iLast := 0;
    iPos := 0;
    repeat
      if iPos + iDelLen > iLen then
      begin
        if iLast <> iPos then
          iPos := iLen;
      end else
        for x := 1 to iDelLen do
          if AString[iPos + x] <> ADelimiter[x] then
            goto EndLoop;

      if iPos - iLast > 0 then
      begin
        SetLength(Step^, iPos - iLast);
        Chr := PChar(Step^);
        for x := 1 to PCardinal(Cardinal(Step^) - SizeOf(Cardinal))^ do
        begin
          Chr^ := AString[iLast + x];
          Inc(Chr);
        end;
      end else
        Step^ := '';

      Cardinal(Step) := Cardinal(Step) + SizeOf(Cardinal);
      iLast := iPos + iDelLen;

      EndLoop:
      Inc(iPos);
    until iLast >= iLen;
  end;
end;

function CollateBr(Str: string): string;
var
  Resultado, Temp: string;
  vChar          : Char;
  Tamanho, i     : integer;
begin
  Result  := '';
  Tamanho := Length(Str);
  i       := 1;
  while i <= Tamanho do
  begin
    Temp  := Copy(Str, i, 1);
    vChar := Temp[1];
    case vChar of
      'á', 'â', 'ã', 'à', 'ä', 'å', 'Á', 'Â', 'Ã', 'À', 'Ä', 'Å':
        Resultado := 'A';
      'é', 'ê', 'è', 'ë', 'É', 'Ê', 'È', 'Ë':
        Resultado := 'E';
      'í', 'î', 'ì', 'ï', 'Í', 'Î', 'Ì', 'Ï':
        Resultado := 'I';
      'ó', 'ô', 'õ', 'ò', 'ö', 'Ó', 'Ô', 'Õ', 'Ò', 'Ö':
        Resultado := 'O';
      'ú', 'û', 'ù', 'ü', 'Ú', 'Û', 'Ù', 'Ü':
        Resultado := 'U';
      'ç', 'Ç':
        Resultado := 'C';
      'ñ', 'Ñ':
        Resultado := 'N';
      'ý', 'ÿ', 'Ý', 'Y':
        Resultado := 'Y';
    else
      if vChar > #127 then
        Resultado := #32

{$IFDEF DELPHI12_UP}
      else if CharInset(vChar, ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '-', ' ']) then

{$ELSE}
      else if vChar in ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '-', ' '] then

{$ENDIF}
        Resultado := UpperCase(vChar);
    end;
    Result := Result + Resultado;
    i      := i + 1;
  end;
end;

constructor TACBrMDFeDAMDFEFR.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FDAMDFEClassOwner := TACBrMDFeDAMDFeClass(Self);
  FFastFile       := '';
  FEspessuraBorda := 1;
  CriarDataSetsFrx;
end;

procedure TACBrMDFeDAMDFEFR.CriarDataSetsFrx;
begin
  frxReport := TfrxReport.Create(FDAMDFEClassOwner);

  with frxReport do
  begin
    EngineOptions.UseGlobalDataSetList := False;
    ScriptLanguage := 'PascalScript';
    StoreInDFM     := False;
    OnGetValue := frxReportGetValue;
    PreviewOptions.Buttons :=[pbExport, pbPrint, pbZoom, pbFind, pbNavigator, pbExportQuick];
    PreviewOptions.Zoom := 2;
  end;

  frxPDFExport := TfrxPDFExport.Create(FDAMDFEClassOwner);
  frxPDFExport.ShowProgress := False;

  frxBarCodeObject := TfrxBarCodeObject.Create(nil);

  cdsIdentificacao := TClientDataSet.Create(nil);
  with cdsIdentificacao, FieldDefs do
  begin
    Close;
    Clear;
    Add('Id', ftString, 44);
    Add('Chave', ftString, 60);
    Add('Protocolo', ftString, 120);
    Add('tpAmb', ftInteger);
    Add('tpEmit', ftInteger);
    Add('Modelo', ftString, 5);
    Add('serie', ftString, 3);
    Add('nMDF', ftString, 15);
    Add('modal', ftInteger);
    Add('dhEmi', ftDateTime);
    Add('tpEmis', ftInteger);
    Add('UFIni', ftString, 2);
    Add('UFFim', ftString, 2);
    Add('OBS', ftMemo);

    // Totalização
    Add('qCTe', ftInteger);
    Add('qCT', ftInteger);
    Add('qNFe', ftInteger);
    Add('qNF', ftInteger);
    Add('qMDFe', ftInteger);
    Add('qCarga', ftCurrency);
    CreateDataSet;
  end;

  cdsEmitente := TClientDataSet.Create(nil);
  with cdsEmitente, FieldDefs do
  begin
    Close;
    Clear;
    Add('CNPJ', ftString, 18);
    Add('IE', ftString, 14);
    Add('xNome', ftString, 60);
    Add('xFant', ftString, 60);
    Add('xLgr', ftString, 60);
    Add('Nro', ftString, 60);
    Add('xCpl', ftString, 60);
    Add('xBairro', ftString, 60);
    Add('CMun', ftString, 7);
    Add('xMun', ftString, 60);
    Add('UF', ftString, 2);
    Add('CEP', ftString, 9);
    Add('Fone', ftString, 15);
    Add('email', ftString, 60);
    Add('site', ftString, 60);
    CreateDataSet;
  end;

  cdsModalAereo := TClientDataSet.Create(nil);
  with cdsModalAereo, FieldDefs do
  begin
    Close;
    Clear;
    // Aereo
    Add('nac', ftInteger);
    Add('matr', ftInteger);
    Add('nVoo', ftString, 9);
    Add('cAerEmb', ftString, 4);
    Add('cAerDes', ftString, 4);
    Add('dVoo', ftDateTime);
    CreateDataSet;
  end;

  cdsModalAqua := TClientDataSet.Create(nil);
  with cdsModalAqua, FieldDefs do
  begin
    Close;
    Clear;
    // Aquaviario
    Add('CNPJAgeNav', ftString, 18);
    Add('tpEmb', ftString, 2);
    Add('cEmbar', ftString, 10);
    Add('xEmbar', ftString, 60);
    Add('nViag', ftString, 10);
    Add('cPrtEmb', ftString, 5);
    Add('cPrtDest', ftString, 5);
    CreateDataSet;
  end;

  cdsDocumentos := TClientDataSet.Create(nil);
  with cdsDocumentos, FieldDefs do
  begin
    Close;
    Clear;
    Add('Tipo', ftString, 5);
    Add('Chave', ftString, 70);
    Add('idUnidTransp', ftString, 70);
    Add('idUnidCarga', ftString, 70);
    CreateDataSet;
  end;

  cdsMunCarrega := TClientDataSet.Create(nil);
  with cdsMunCarrega, FieldDefs do
  begin
    Close;
    Clear;
    Add('xMunCarrega', ftString, 60);
    CreateDataSet;
  end;

  cdsModalRodo := TClientDataSet.Create(nil);
  with cdsModalRodo, FieldDefs do
  begin
    Close;
    Clear;
    Add('RNTRC', ftString, 8);
    Add('CIOT', ftString, 12);
    // Vale Pedagio
    Add('CNPJForn', ftMemo);
    Add('CNPJPg', ftMemo);
    Add('nCompra', ftMemo);
    // Veiculos
    Add('placa', ftMemo);
    Add('RNTRCProp', ftMemo);
    Add('RENAVAM', ftString, 11);
    // Condutor
    Add('xNome', ftMemo);
    Add('CPF', ftMemo);
    CreateDataSet;
  end;

  cdsParametros := TClientDataSet.Create(nil);
  with cdsParametros, FieldDefs do
  begin
    Close;
    Clear;
    Add('Versao', ftString, 5);
    Add('Imagem', ftString, 256);
    Add('Sistema', ftString, 150);
    Add('Usuario', ftString, 60);
    CreateDataSet;
  end;

  cdsPercurso := TClientDataSet.Create(nil);
  with cdsPercurso, FieldDefs do
  begin
    Close;
    Clear;
    Add('UFPer', ftString, 2);
    CreateDataSet;
  end;

  cdsModalFerrov := TClientDataSet.Create(nil);
  with cdsModalFerrov, FieldDefs do
  begin
    Close;
    Clear;
    Add('xPref', ftString, 10);
    Add('dhTrem', ftDateTime);
    Add('xOri', ftString, 3);
    Add('xDest', ftString, 3);
    Add('qVag', ftInteger);
    CreateDataSet;
  end;

  cdsModalFerrovVagoes := TClientDataSet.Create(nil);
  with cdsModalFerrovVagoes, FieldDefs do
  begin
    Close;
    Clear;
    Add('serie', ftString, 3);
    Add('nVag', ftInteger);
    Add('nSeq', ftInteger);
    Add('TU', ftCurrency);
    CreateDataSet;
  end;

  cdsTermCarrega := TClientDataSet.Create(nil);
  with cdsTermCarrega, FieldDefs do
  begin
    Close;
    Clear;
    Add('cTermCarreg', ftString, 8);
    Add('xTermCarreg', ftString, 60);
    CreateDataSet;
  end;

  cdsTermDescarrega := TClientDataSet.Create(nil);
  with cdsTermDescarrega, FieldDefs do
  begin
    Close;
    Clear;
    Add('cTermDescarreg', ftString, 8);
    Add('xTermDescarreg', ftString, 60);
    CreateDataSet;
  end;

  cdsEmbarcaComboio := TClientDataSet.Create(nil);
  with cdsEmbarcaComboio, FieldDefs do
  begin
    Close;
    Clear;
    Add('cEmbComb', ftString, 10);
    CreateDataSet;
  end;

  cdsEventos := TClientDataSet.Create(nil);
  with cdsEventos, FieldDefs do
  begin
    Close;
    Clear;
    Add('DescricaoTipoEvento', ftString, 150);
    Add('Modelo', ftString, 2);
    Add('Serie', ftString, 3);
    Add('Numero', ftString, 9);
    Add('MesAno', ftString, 5);
    Add('Barras', ftString, 44);
    Add('ChaveAcesso', ftString, 60);
    Add('cOrgao', ftInteger);
    Add('tpAmb', ftString, 100);
    Add('dhEvento', ftDateTime);
    Add('TipoEvento', ftString, 6);
    Add('DescEvento', ftString, 100);
    Add('nSeqEvento', ftInteger);
    Add('versaoEvento', ftString, 10);
    Add('cStat', ftInteger);
    Add('xMotivo', ftString, 100);
    Add('nProt', ftString, 20);
    Add('dhRegEvento', ftDateTime);
    Add('xJust', ftBlob);
    Add('xCondUso', ftBlob);
    Add('xCorrecao', ftBlob);
    Add('xNome', ftString, 60); // Condutor
    Add('CPF', ftString, 14);
    Add('nProtEvento', ftString, 15);
    Add('dtEnc', ftDateTime);
    Add('cUf', ftInteger);
    Add('cMun', ftInteger);
    CreateDataSet;
  end;

  cdsInfUnidCargaVazia := TClientDataSet.Create(nil);
  with cdsInfUnidCargaVazia, FieldDefs do
  begin
    Close;
    Clear;
    Add('idUnidCargaVazia', ftString, 20);
    Add('tpUnidCargaVazia', ftString, 20);
    CreateDataSet;
  end;

  frxIdentificacao := TfrxDBDataset.Create(nil);
  with frxIdentificacao do
  begin
    UserName := 'Identificacao';
    OpenDataSource := False;
    DataSet := cdsIdentificacao;
  end;

  frxEmitente := TfrxDBDataset.Create(nil);
  with frxEmitente do
  begin
    UserName := 'Emitente';
    OpenDataSource := False;
    DataSet := cdsEmitente;
  end;

  frxModalAereo := TfrxDBDataset.Create(nil);
  with frxModalAereo do
  begin
    UserName := 'ModalAereo';
    OpenDataSource := False;
    DataSet := cdsModalAereo;
  end;

  frxModalAqua := TfrxDBDataset.Create(nil);
  with frxModalAqua do
  begin
    UserName := 'ModalAqua';
    OpenDataSource := False;
    DataSet := cdsModalAqua;
  end;

  frxDocumentos := TfrxDBDataset.Create(nil);
  with frxDocumentos do
  begin
    UserName := 'Documentos';
    OpenDataSource := False;
    DataSet := cdsDocumentos;
  end;

  frxMunCarrega := TfrxDBDataset.Create(nil);
  with frxMunCarrega do
  begin
    UserName := 'MunCarrega';
    OpenDataSource := False;
    DataSet := cdsMunCarrega;
  end;

  frxModalRodo := TfrxDBDataset.Create(nil);
  with frxModalRodo do
  begin
    UserName := 'ModalRodo';
    OpenDataSource := False;
    DataSet := cdsModalRodo;
  end;

  frxParametros := TfrxDBDataset.Create(nil);
  with frxParametros do
  begin
    UserName := 'Parametros';
    OpenDataSource := False;
    DataSet := cdsParametros;
  end;

  frxPercurso := TfrxDBDataset.Create(nil);
  with frxPercurso do
  begin
    UserName := 'Percurso';
    OpenDataSource := False;
    DataSet := cdsPercurso;
  end;

  frxModalFerrov := TfrxDBDataset.Create(nil);
  with frxModalFerrov do
  begin
    UserName := 'ModalFerrov';
    OpenDataSource := False;
    DataSet := cdsModalFerrov;
  end;

  frxModalFerrovVagoes := TfrxDBDataset.Create(nil);
  with frxModalFerrovVagoes do
  begin
    UserName := 'ModalFerrovVagoes';
    OpenDataSource := False;
    DataSet := cdsModalFerrovVagoes;
  end;

  frxEventos := TfrxDBDataset.Create(nil);
  with frxEventos do
  begin
    UserName := 'Eventos';
    OpenDataSource := False;
    DataSet := cdsEventos;
  end;

  frxTermCarrega := TfrxDBDataset.Create(nil);
  with frxTermCarrega do
  begin
    UserName := 'TermCarrega';
    OpenDataSource := False;
    DataSet := cdsTermCarrega;
  end;

  frxTermDescarrega := TfrxDBDataset.Create(nil);
  with frxTermDescarrega do
  begin
    UserName := 'TermDescarrega';
    OpenDataSource := False;
    DataSet := cdsTermDescarrega;
  end;

  frxEmbarcaComboio := TfrxDBDataset.Create(nil);
  with frxEmbarcaComboio do
  begin
    UserName := 'EmbarcaComboio';
    OpenDataSource := False;
    DataSet := cdsEmbarcaComboio;
  end;

  frxInfUnidCargaVazia := TfrxDBDataset.Create(nil);
  with frxInfUnidCargaVazia do
  begin
    UserName := 'InfUnidCargaVazia';
    OpenDataSource := False;
    DataSet := cdsInfUnidCargaVazia;
  end;
end;

destructor TACBrMDFeDAMDFEFR.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrMDFeDAMDFEFR.frxReportGetValue(const VarName: string; var Value: Variant);
begin
  if VarName = 'CANCELADO' then
    Value := (DAMDFEClassOwner.MDFeCancelada) or (FMDFe.procMDFe.cStat = 101);
  if VarName = 'ENCERRADO' then
    Value := DAMDFEClassOwner.MDFeEncerrado;
end;

function TACBrMDFeDAMDFEFR.GetPreparedReport: TfrxReport;
begin
  if Trim(FFastFile) = '' then
    Result := nil
  else
  begin
    if PrepareReport(nil) then
      Result := frxReport
    else
      Result := nil;
  end;
end;

function TACBrMDFeDAMDFEFR.GetPreparedReportEvento: TfrxReport;
begin
  if Trim(FFastFileEvento) = '' then
    Result := nil
  else
  begin
    if PrepareReportEvento then
      Result := frxReport
    else
      Result := nil;
  end;
end;

procedure TACBrMDFeDAMDFEFR.ImprimirDAMDFe(MDFe: TMDFe);
begin
  if PrepareReport(MDFe) then
  begin
    if MostrarPreview then
      frxReport.ShowPreparedReport
    else
    begin
      frxReport.PrintOptions.ShowDialog  := SelecionaImpressora;
      frxReport.PrintOptions.Copies      := NumCopias;
      frxReport.PreviewOptions.AllowEdit := False;
      frxReport.Print;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.ImprimirDAMDFePDF(MDFe: TMDFe);
var
  I:          Integer;
  TITULO_PDF: string;
  OldShowDialog : Boolean;
begin
  if PrepareReport(MDFe) then
  begin
    for I := 0 to TACBrMDFe(ACBrMDFe).Manifestos.Count - 1 do
    begin
      TITULO_PDF := OnlyNumber(TACBrMDFe(ACBrMDFe).Manifestos.Items[i].MDFe.infMDFe.ID);

      frxPDFExport.Author     := Sistema;
      frxPDFExport.Creator    := Sistema;
      frxPDFExport.Producer   := Sistema;
      frxPDFExport.Title      := TITULO_PDF;
      frxPDFExport.Subject    := TITULO_PDF;
      frxPDFExport.Keywords   := TITULO_PDF;
      OldShowDialog := frxPDFExport.ShowDialog;
	    try
        frxPDFExport.ShowDialog := False;
        frxPDFExport.FileName   := IncludeTrailingPathDelimiter(PathPDF) + TITULO_PDF + '-mdfe.pdf';

        if not DirectoryExists(ExtractFileDir(frxPDFExport.FileName)) then
          ForceDirectories(ExtractFileDir(frxPDFExport.FileName));

        frxReport.Export(frxPDFExport);
      finally
        frxPDFExport.ShowDialog := OldShowDialog;
      end;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.ImprimirEVENTO(MDFe: TMDFe);
begin
  if PrepareReportEvento then
  begin
    if MostrarPreview then
      frxReport.ShowPreparedReport
    else
      frxReport.Print;
  end;
end;

procedure TACBrMDFeDAMDFEFR.ImprimirEVENTOPDF(MDFe: TMDFe);
const
  TITULO_PDF = 'Manifesto de Documento Eletrônico - Evento';
var
  NomeArq      : String;
  OldShowDialog: Boolean;
begin
  if PrepareReportEvento then
  begin
    frxPDFExport.Author   := Sistema;
    frxPDFExport.Creator  := Sistema;
    frxPDFExport.Producer := Sistema;
    frxPDFExport.Title    := TITULO_PDF;
    frxPDFExport.Subject  := TITULO_PDF;
    frxPDFExport.Keywords := TITULO_PDF;
    OldShowDialog := frxPDFExport.ShowDialog;
    try
      frxPDFExport.ShowDialog := False;
      NomeArq                 := StringReplace(TACBrMDFe(ACBrMDFe).EventoMDFe.Evento.Items[0].InfEvento.Id, 'ID', '', [rfIgnoreCase]);
      frxPDFExport.FileName   := IncludeTrailingPathDelimiter(PathPDF) + NomeArq + '-procEventoMDFe.pdf';

      if not DirectoryExists(ExtractFileDir(frxPDFExport.FileName)) then
        ForceDirectories(ExtractFileDir(frxPDFExport.FileName));

      frxReport.Export(frxPDFExport);
    finally
      frxPDFExport.ShowDialog := OldShowDialog;
    end;
  end;
end;

function TACBrMDFeDAMDFEFR.PrepareReport(MDFe: TMDFe): Boolean;
var
  i: Integer;
  Stream: TStringStream;
begin
  Result := False;
  SetDataSetsToFrxReport;

  if Trim(FastFile) <> '' then
  begin
    if not (UpperCase(Copy(FastFile, Length(FastFile)-3, 4)) = '.FR3') then
    begin
      Stream := TStringStream.Create(FastFile);
      frxReport.FileName := '';
      frxReport.LoadFromStream(Stream);
      Stream.Free;
    end
    else
    begin
      if FileExists(FastFile) then
        frxReport.LoadFromFile(FastFile)
      else
        raise EACBrMDFeDAMDFEFR.CreateFmt('Caminho do arquivo de impressão do DAMDFe "%s" inválido.', [FastFile]);
    end;
  end
  else
    raise EACBrMDFeDAMDFEFR.Create('Caminho do arquivo de impressão do DAMDFe não assinalado.');

  if Assigned(MDFe) then
  begin
    FMDFe := MDFe;
    CarregaDados;
    SetDataSetsToFrxReport;
    Result := frxReport.PrepareReport;
  end
  else
  begin
    if Assigned(ACBrMDFe) then
    begin
      for i := 0 to TACBrMDFe(ACBrMDFe).Manifestos.Count - 1 do
      begin
        FMDFe := TACBrMDFe(ACBrMDFe).Manifestos.Items[i].MDFe;
        CarregaDados;
        if (i > 0) then
          Result := frxReport.PrepareReport(False)
        else
          Result := frxReport.PrepareReport;
      end;
    end
    else
      raise EACBrMDFeDAMDFEFR.Create('Propriedade ACBrMDFe não assinalada.');
  end;
end;

function TACBrMDFeDAMDFEFR.PrepareReportEvento: Boolean;
var
  Stream: TStringStream;
begin
  SetDataSetsToFrxReport;

  if Trim(FastFileEvento) <> '' then
  begin
    if not (UpperCase(Copy(FastFileEvento, Length(FastFileEvento)-3, 4)) = '.FR3') then
    begin
      Stream := TStringStream.Create(FastFileEvento);
      frxReport.FileName := '';
      frxReport.LoadFromStream(Stream);
      Stream.Free;
    end
    else
    if FileExists(FastFileEvento) then
      frxReport.LoadFromFile(FastFileEvento)
    else
      raise EACBrMDFeDAMDFEFR.CreateFmt('Caminho do arquivo de impressão do EVENTO "%s" inválido.', [FastFileEvento]);
  end
  else
    raise EACBrMDFeDAMDFEFR.Create('Caminho do arquivo de impressão do EVENTO não assinalado.');

  frxReport.PrintOptions.Copies := NumCopias;
  frxReport.PreviewOptions.AllowEdit := False;

  // preparar relatorio
  if Assigned(ACBrMDFe) then
  begin
    if assigned(TACBrMDFe(ACBrMDFe).EventoMDFe) then
    begin
      Evento := TACBrMDFe(ACBrMDFe).EventoMDFe;
      CarregaDadosEventos;
    end
    else
      raise EACBrMDFeDAMDFEFR.Create('Evento não foi assinalado.');

    if TACBrMDFe(ACBrMDFe).Manifestos.Count > 0 then
    begin
      MDFe := TACBrMDFe(ACBrMDFe).Manifestos.Items[0].MDFe;
      CarregaDados;
    end;

    Result := frxReport.PrepareReport;
  end
  else
    raise EACBrMDFeDAMDFEFR.Create('Propriedade ACBrMDFe não assinalada.');
end;

procedure TACBrMDFeDAMDFEFR.SetDataSetsToFrxReport;
begin
  frxReport.EnabledDataSets.Clear;
  frxReport.EnabledDataSets.Add(frxIdentificacao);
  frxReport.EnabledDataSets.Add(frxEmitente);
  frxReport.EnabledDataSets.Add(frxMunCarrega);
  frxReport.EnabledDataSets.Add(frxModalRodo);
  frxReport.EnabledDataSets.Add(frxModalAereo);
  frxReport.EnabledDataSets.Add(frxModalAqua);
  frxReport.EnabledDataSets.Add(frxModalFerrov);
  frxReport.EnabledDataSets.Add(frxModalFerrovVagoes);
  frxReport.EnabledDataSets.Add(frxDocumentos);
  frxReport.EnabledDataSets.Add(frxParametros);
  frxReport.EnabledDataSets.Add(frxPercurso);
  frxReport.EnabledDataSets.Add(frxEventos);
  frxReport.EnabledDataSets.Add(frxTermCarrega);
  frxReport.EnabledDataSets.Add(frxTermDescarrega);
  frxReport.EnabledDataSets.Add(frxEmbarcaComboio);
  frxReport.EnabledDataSets.Add(frxInfUnidCargaVazia);
end;

procedure TACBrMDFeDAMDFEFR.LimpaDados;
begin
  cdsIdentificacao.EmptyDataSet;
  cdsEmitente.EmptyDataSet;
  cdsModalAereo.EmptyDataSet;
  cdsModalAqua.EmptyDataSet;
  cdsDocumentos.EmptyDataSet;
  cdsMunCarrega.EmptyDataSet;
  cdsModalRodo.EmptyDataSet;
  cdsParametros.EmptyDataSet;
  cdsPercurso.EmptyDataSet;
  cdsModalFerrov.EmptyDataSet;
  cdsModalFerrovVagoes.EmptyDataSet;
  cdsEventos.EmptyDataSet;
  cdsTermCarrega.EmptyDataSet;
  cdsTermDescarrega.EmptyDataSet;
  cdsEmbarcaComboio.EmptyDataSet;
  cdsInfUnidCargaVazia.EmptyDataSet;
end;

procedure TACBrMDFeDAMDFEFR.CarregaDados;
begin
  LimpaDados;
  CarregaParametros;
  CarregaIdentificacao;
  CarregaMunCarrega;
  CarregaPercurso;
  CarregaEmitente;
  CarregaDocumentos;
  CarregaModal;
  CarregaTermCarreg;
  CarregaTermDescarreg;
  CarregaEmbarcacaoComboio;
  CarregaInfUnidCargaVazia;
end;

procedure TACBrMDFeDAMDFEFR.CarregaParametros;
begin
  with cdsParametros do
  begin
    Append;

    FieldByName('Versao').AsString := FloatToString(FMDFe.infMDFe.Versao,'.','#0.00');

    // Carregamento da imagem
    FieldByName('Imagem').AsString := Ifthen(DAMDFEClassOwner.Logo <> '', DAMDFEClassOwner.Logo,'');
    FieldByName('Sistema').AsString := Ifthen(DAMDFEClassOwner.Sistema <> '',DAMDFEClassOwner.Sistema,'Projeto ACBr - http://acbr.sf.net');
    FieldByName('Usuario').AsString := Ifthen(DAMDFEClassOwner.Usuario <> '', DAMDFEClassOwner.Usuario,'');
    Post;

  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaIdentificacao;
var
  vTemp:TStringList;
  wObs:String;
  Campos:TSplitResult;
  IndexCampo:Integer;
  TmpStr:String;
  BufferObs:String;
begin
  with cdsIdentificacao do
  begin
    Append;

    with FMDFe.infMDFe do
    begin
      FieldByName('Id').AsString    := OnlyNumber(Id);
      FieldByName('Chave').AsString := FormatarChaveAcesso(Id);
    end;

    with FMDFe.Ide do
    begin
      FieldByName('tpAmb').AsInteger  := StrToIntDef(TpAmbToStr(tpAmb), 0);
      FieldByName('tpEmit').AsInteger := StrToIntDef(TpEmitenteToStr(tpEmit), 0);
      FieldByName('Modelo').AsString  := modelo;
      FieldByName('serie').AsString   := Poem_Zeros(serie, 3);
      FieldByName('nMDF').AsString    := FormatFloat('000,000,000', nMDF);
      FieldByName('modal').AsInteger  := StrToIntDef(ModalToStr(modal), 0);
      FieldByName('dhEmi').AsDateTime := dhEmi;
      FieldByName('tpEmis').AsInteger := StrToIntDef(TpEmisToStr(tpEmis), 0);
      FieldByName('UFIni').AsString   := UFIni;
      FieldByName('UFFim').AsString   := UFFim;
      if (tpEmis = teNormal) or (not EstaVazio(FDAMDFEClassOwner.ProtocoloMDFE)) or (not EstaVazio(FMDFe.procMDFe.nProt))
      then
      begin
        if not EstaVazio(FDAMDFEClassOwner.ProtocoloMDFE) then
          FieldByName('Protocolo').AsString := FDAMDFEClassOwner.ProtocoloMDFE
        else if not EstaVazio(FMDFe.procMDFe.nProt) then
          FieldByName('Protocolo').AsString := FMDFe.procMDFe.nProt + '   ' +
            IfThen(FMDFe.procMDFe.dhRecbto <> 0, DateTimeToStr(FMDFe.procMDFe.dhRecbto), '')
        else
          FieldByName('Protocolo').AsString := 'MDFe sem Autorização de Uso da SEFAZ';
      end
      else
        FieldByName('Protocolo').AsString := 'Impressão em contingência. Obrigatória a autorização em 168 horas' +
          ' após esta impressão (' + FormatDateTimeBr(dhEmi) + ')';

    end;
    with FMDFe.tot do
    begin
      FieldByName('qCTe').AsInteger    := qCTe;
      FieldByName('qCT').AsInteger     := qCT;
      FieldByName('qNFe').AsInteger    := qNFe;
      FieldByName('qNF').AsInteger     := qNF;
      FieldByName('qMDFe').AsInteger   := qMDFe;
      if cUnid = uTon then
        FieldByName('qCarga').AsCurrency := qCarga * 1000
      else
        FieldByName('qCarga').AsCurrency := qCarga;
    end;

    // Incluido por Paulo Hostert em 18/11/2014.
    wObs := FMDFe.infAdic.infCpl;
    vTemp := TStringList.Create;
    try
      if Trim(wObs) <> '' then
      begin
        Campos := Split(';', wObs);
        for IndexCampo := 0 to Length(Campos) - 1 do
          vTemp.Add(Trim(Campos[IndexCampo]));

        TmpStr := vTemp.Text;
        BufferObs := TmpStr;
      end
      else
        BufferObs := '';
    finally
      vTemp.Free;
    end;
    FieldByName('OBS').AsString := BufferObs;

    Post;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaMunCarrega;
var i:integer;
begin
  with cdsMunCarrega do
  begin
    for i := 0 to FMDFe.Ide.infMunCarrega.Count - 1 do
    begin
      Append;
      FieldByName('xMunCarrega').AsString:=FMDFe.Ide.infMunCarrega[i].xMunCarrega;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaPercurso;
var i:integer;
begin
  with cdsPercurso do
  begin
    for i := 0 to FMDFe.Ide.infPercurso.Count - 1 do
    begin
      Append;
      FieldByName('UFPer').AsString:=FMDFe.Ide.infPercurso[i].UFPer;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaEmitente;
begin
  with cdsEmitente do
  begin
    Append;
    with FMDFe.emit do
    begin
      FieldByName('CNPJ').AsString  := FormatarCNPJ(CNPJ);
      FieldByName('IE').AsString    := IE;
      FieldByName('XNome').AsString := xNome;
      FieldByName('XFant').AsString := XFant;
      with EnderEmit do
      begin
        FieldByName('Xlgr').AsString    := XLgr;
        FieldByName('Nro').AsString     := Nro;
        FieldByName('XCpl').AsString    := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString    := IntToStr(CMun);
        FieldByName('XMun').AsString    := CollateBr(XMun);
        FieldByName('UF').AsString      := UF;
        FieldByName('CEP').AsString     := FormatarCEP(Poem_Zeros(CEP, 8));
        FieldByName('Fone').AsString    := FormatarFone(Fone);
        FieldByName('email').AsString   := email;
        FieldByName('site').AsString    := FDAMDFEClassOwner.Site;
      end;
    end;
    Post;

  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaDocumentos;
var
  i, j, x, y: integer;
begin
  with CDSDocumentos do
  begin
    with FMDFe.infDoc do
    begin
      for i := 0 to infMunDescarga.Count - 1 do
      begin
        with infMunDescarga.Items[i] do
        begin

          for j := 0 to infCTe.Count - 1 do
          begin
            Append;
            FieldByName('Tipo').AsString  := 'CTe';
            FieldByName('Chave').AsString := FormatarChaveAcesso(infCTe.Items[j].chCTe);
            Post;
            with infCTe[j] do
              for x := 0 to infUnidTransp.Count - 1 do
              begin
                if x = 0 then
                  Edit
                else
                  Append;
                FieldByName('idUnidTransp').AsString := infUnidTransp[x].idUnidTransp;
                Post;
                for y := 0 to infUnidTransp[x].infUnidCarga.Count - 1 do
                begin
                  if y = 0 then
                    Edit
                  else
                    Append;
                  FieldByName('idUnidCarga').AsString := infUnidTransp[x].infUnidCarga[y].idUnidCarga;
                  Post;
                end;
              end;
          end;
          for j := 0 to infCT.Count - 1 do
          begin
            Append;
            FieldByName('Tipo').AsString  := 'CT';
            FieldByName('Chave').AsString := FormatarCNPJouCPF(FMDFe.emit.CNPJ) + '        ' +
              IntToStr(infCT.Items[j].serie) + '-' + infCT.Items[j].nCT;
            Post;
            with infCT[j] do
              for x := 0 to infUnidTransp.Count - 1 do
              begin
                if x = 0 then
                  Edit
                else
                  Append;
                FieldByName('idUnidTransp').AsString := infUnidTransp[x].idUnidTransp;
                Post;
                for y := 0 to infUnidTransp[x].infUnidCarga.Count - 1 do
                begin
                  if y = 0 then
                    Edit
                  else
                    Append;
                  FieldByName('idUnidCarga').AsString := infUnidTransp[x].infUnidCarga[y].idUnidCarga;
                  Post;
                end;
              end;
          end;
          for j := 0 to infNFe.Count - 1 do
          begin
            Append;
            FieldByName('Tipo').AsString  := 'NFe';
            FieldByName('Chave').AsString := FormatarChaveAcesso(infNFe.Items[j].chNFe);
            Post;
            with infNFe[j] do
              for x := 0 to infUnidTransp.Count - 1 do
              begin
                if x = 0 then
                  Edit
                else
                  Append;
                FieldByName('idUnidTransp').AsString := infUnidTransp[x].idUnidTransp;
                Post;
                for y := 0 to infUnidTransp[x].infUnidCarga.Count - 1 do
                begin
                  if y = 0 then
                    Edit
                  else
                    Append;
                  FieldByName('idUnidCarga').AsString := infUnidTransp[x].infUnidCarga[y].idUnidCarga;
                  Post;
                end;
              end;
          end;
          for j := 0 to infNF.Count - 1 do
          begin
            Append;
            FieldByName('Tipo').AsString  := 'NF';
            FieldByName('Chave').AsString := FormatarCNPJouCPF(FMDFe.emit.CNPJ) + '        ' +
              IntToStr(infNF.Items[j].serie) + '-' + IntToStr(infNF.Items[j].nNF);
            Post;
            with infNF[j] do
              for x := 0 to infUnidTransp.Count - 1 do
              begin
                if x = 0 then
                  Edit
                else
                  Append;
                FieldByName('idUnidTransp').AsString := infUnidTransp[x].idUnidTransp;
                Post;
                for y := 0 to infUnidTransp[x].infUnidCarga.Count - 1 do
                begin
                  if y = 0 then
                    Edit
                  else
                    Append;
                  FieldByName('idUnidCarga').AsString := infUnidTransp[x].infUnidCarga[y].idUnidCarga;
                  Post;
                end;
              end;
          end;
          for j := 0 to infMDFeTransp.Count - 1 do
          begin
            Append;
            FieldByName('Tipo').AsString  := 'MDF-e';
            FieldByName('Chave').AsString := FormatarChaveAcesso(infMDFeTransp.Items[j].chMDFe);
            Post;
            with infMDFeTransp[j] do
              for x := 0 to infUnidTransp.Count - 1 do
              begin
                if x = 0 then
                  Edit
                else
                  Append;
                FieldByName('idUnidTransp').AsString := infUnidTransp[x].idUnidTransp;
                Post;
                for y := 0 to infUnidTransp[x].infUnidCarga.Count - 1 do
                begin
                  if y = 0 then
                    Edit
                  else
                    Append;
                  FieldByName('idUnidCarga').AsString := infUnidTransp[x].infUnidCarga[y].idUnidCarga;
                  Post;
                end;
              end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaModal;
begin
  with FMDFe do
  begin
    case Ide.modal of
      moRodoviario:
        CarregaModalRodoviario;
      moAereo:
        CarregaModalAereo;
      moAquaviario:
        CarregaModalAquaviario;
      moFerroviario:
        CarregaModalFerroviario;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaTermCarreg;
var
  i: integer;
begin
  with cdsTermCarrega, FMDFe.aquav.infTermCarreg do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('cTermCarreg').AsString := Items[i].cTermCarreg;
      FieldByName('xTermCarreg').AsString := Items[i].xTermCarreg;
      Post;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaTermDescarreg;
var
  i: integer;
begin
  with cdsTermDescarrega, FMDFe.aquav.infTermDescarreg do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('cTermDescarreg').AsString := Items[i].cTermDescarreg;
      FieldByName('xTermDescarreg').AsString := Items[i].xTermDescarreg;
      Post;
    end;
  end;
end;


procedure TACBrMDFeDAMDFEFR.CarregaEmbarcacaoComboio;
var
  i: integer;
begin
  with cdsEmbarcaComboio, FMDFe.aquav.infEmbComb do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('cEmbComb').AsString := Items[i].cEmbComb;
      Post;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaInfUnidCargaVazia;
var
  i: integer;
begin
  with cdsInfUnidCargaVazia, FMDFe.aquav.infUnidCargaVazia do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('idUnidCargaVazia').AsString := Items[i].idUnidCargaVazia;
      case Items[i].tpUnidCargaVazia of
         ucContainer : FieldByName('tpUnidCargaVazia').AsString := '1 - Container';
         ucULD : FieldByName('tpUnidCargaVazia').AsString := '2 - Carreta';
      end;
      Post;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaModalAereo;
begin
  with cdsModalAereo, FMDFe.aereo do
  begin
    Append;
    FieldByName('nac').AsInteger    := nac;
    FieldByName('matr').AsInteger   := matr;
    FieldByName('nVoo').AsString    := nVoo;
    FieldByName('cAerEmb').AsString := cAerEmb;
    FieldByName('cAerDes').AsString := cAerDes;
    FieldByName('dVoo').AsDateTime  := dVoo;
    Post;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaModalAquaviario;
begin
  with CDSModalAqua, FMDFe.aquav do
  begin
    Append;
    FieldByName('CNPJAgeNav').AsString := FormatarCNPJ(CNPJAgeNav);
    FieldByName('tpEmb').AsString      := tpEmb;
    FieldByName('cEmbar').AsString     := cEmbar;
    FieldByName('xEmbar').AsString     := xEmbar;
    FieldByName('nViag').AsString      := nViagem;
    FieldByName('cPrtEmb').AsString    := cPrtEmb;
    FieldByName('cPrtDest').AsString   := cPrtDest;
    Post;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaModalFerroviario;
var
  i: integer;
begin
  with CDSModalFerrov, FMDFe.ferrov do
  begin
    Append;
    FieldByName('xPref').AsString    := xPref;
    FieldByName('dhTrem').AsDateTime := dhTrem;
    FieldByName('xOri').AsString     := xOri;
    FieldByName('xDest').AsString    := xDest;
    FieldByName('qVag').AsInteger    := qVag;
    Post;
  end;

  with CDSModalFerrovVagoes, FMDFe.ferrov.vag do
  begin
    for i := 0 to Count - 1 do
    begin
      Append;
      FieldByName('serie').AsString := Items[i].serie;
      FieldByName('nVag').AsInteger := Items[i].nVag;
      FieldByName('nSeq').AsInteger := Items[i].nSeq;
      FieldByName('TU').AsCurrency  := Items[i].TU;
      Post;
    end;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaModalRodoviario;
var
  i: integer;
begin
  with cdsModalRodo, FMDFe.rodo do
  begin
    Append;
    FieldByName('RNTRC').AsString := RNTRC;
    FieldByName('CIOT').AsString  := CIOT;
    if veicTracao.placa <> '' then
    begin
      FieldByName('placa').AsString     := FormatarPlaca(veicTracao.placa);
      FieldByName('RENAVAM').AsString   := veicTracao.RENAVAM;
      FieldByName('RNTRCProp').AsString := veicTracao.prop.RNTRC;

      for i := 0 to veicTracao.condutor.Count - 1 do
      begin
        // Alteração proposta por Maciel Goettms (27/02/2014) Concatenação dos condutores já adicionados.
        FieldByName('CPF').AsString   := FieldByName('CPF').AsString + FormatarCPF(veicTracao.condutor.Items[i].CPF) + #13#10;
        FieldByName('xNome').AsString := FieldByName('xNome').AsString + veicTracao.condutor.Items[i].xNome + #13#10;
      end;
    end;
    for i := 0 to veicReboque.Count - 1 do
    begin
      FieldByName('placa').AsString     := FieldByName('placa').AsString + #13#10 + FormatarPlaca(FMDFe.rodo.veicReboque.Items[i].placa);
      FieldByName('RENAVAM').AsString   := FieldByName('RENAVAM').AsString + #13#10 + veicReboque.Items[i].RENAVAM;
      FieldByName('RNTRCProp').AsString := FieldByName('RNTRCProp').AsString + #13#10 + IfThen(FMDFe.rodo.veicReboque.Items[i].prop.RNTRC <> '', FMDFe.rodo.veicReboque.Items[i].prop.RNTRC, FMDFe.rodo.RNTRC);
    end;

    if FMDFe.rodo.valePed.disp.Count > 0 then
    begin
      for i := 0 to valePed.disp.Count - 1 do
      begin
        FieldByName('CNPJForn').AsString := FieldByName('CNPJForn').AsString + FormatarCNPJ(valePed.disp.Items[i].CNPJForn) + #13#10;
        FieldByName('CNPJPg').AsString   := FieldByName('CNPJPg').AsString + FormatarCNPJ(valePed.disp.Items[i].CNPJPg) + #13#10;
        FieldByName('nCompra').AsString  := FieldByName('nCompra').AsString + valePed.disp.Items[i].nCompra + #13#10;
      end;
    end
    else
    if FMDFe.rodo.infANTT.valePed.disp.Count > 0 then
    begin
      for i := 0 to infANTT.valePed.disp.Count - 1 do
      begin
        FieldByName('CNPJForn').AsString := FieldByName('CNPJForn').AsString + FormatarCNPJ(infANTT.valePed.disp.Items[i].CNPJForn) + #13#10;
        FieldByName('CNPJPg').AsString   := FieldByName('CNPJPg').AsString + FormatarCNPJ(infANTT.valePed.disp.Items[i].CNPJPg) + #13#10;
        FieldByName('nCompra').AsString  := FieldByName('nCompra').AsString + infANTT.valePed.disp.Items[i].nCompra + #13#10;
      end;
    end;
    Post;
  end;
end;

procedure TACBrMDFeDAMDFEFR.CarregaDadosEventos;
var
  i: Integer;
begin
  with cdsEventos do
  begin
    EmptyDataSet;

    for i := 0 to FEvento.Evento.Count - 1 do
    begin
      Append;

      with Evento.Evento[i] do
      begin
        FieldByName('DescricaoTipoEvento').AsString := InfEvento.DescricaoTipoEvento(InfEvento.tpEvento);

        // nota fiscal eletronica
        FieldByName('Modelo').AsString      := Copy(InfEvento.chMDFe, 21, 2);
        FieldByName('Serie').AsString       := Copy(InfEvento.chMDFe, 23, 3);
        FieldByName('Numero').AsString      := Copy(InfEvento.chMDFe, 26, 9);
        FieldByName('MesAno').AsString      := Copy(InfEvento.chMDFe, 05, 2) + '/' + Copy(InfEvento.chMDFe, 03, 2);
        FieldByName('Barras').AsString      := InfEvento.chMDFe;
        FieldByName('ChaveAcesso').AsString := FormatarChaveAcesso(InfEvento.chMDFe);

        // Carta de correção eletrônica
        FieldByName('cOrgao').AsInteger := InfEvento.cOrgao;

        case InfEvento.tpAmb of
          taProducao:
            FieldByName('tpAmb').AsString := 'PRODUÇÃO';
          taHomologacao:
            begin
              FieldByName('tpAmb').AsString      := 'HOMOLOGAÇÃO - SEM VALOR FISCAL';
              frxReport.Variables['HOMOLOGACAO'] := True;
            end;
        end;

        FieldByName('dhEvento').AsDateTime    := InfEvento.dhEvento;
        FieldByName('TipoEvento').AsString    := InfEvento.TipoEvento;
        FieldByName('DescEvento').AsString    := InfEvento.DescEvento;
        FieldByName('nSeqEvento').AsInteger   := InfEvento.nSeqEvento;
        FieldByName('versaoEvento').AsString  := InfEvento.versaoEvento;
        FieldByName('cStat').AsInteger        := RetInfEvento.cStat;
        FieldByName('xMotivo').AsString       := RetInfEvento.xMotivo;
        FieldByName('nProt').AsString         := RetInfEvento.nProt;
        FieldByName('dhRegEvento').AsDateTime := RetInfEvento.dhRegEvento;
        FieldByName('xJust').AsString         := InfEvento.detEvento.xJust;
        FieldByName('xNome').AsString         := InfEvento.detEvento.xNome;
        FieldByName('CPF').AsString           := FormatarCPF(InfEvento.detEvento.CPF);
        FieldByName('nProtEvento').AsString   := InfEvento.detEvento.nProt;
        FieldByName('dtEnc').AsDateTime       := InfEvento.detEvento.dtEnc;
        FieldByName('cUf').AsInteger          := InfEvento.detEvento.cUF;
        FieldByName('cMun').AsInteger         := InfEvento.detEvento.cMun;
      end;

      Post;
    end;
  end;
end;

end.
