{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
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

unit ACBrSATExtratoFR;

{$I ACBr.inc}

interface

uses 
  Classes, 
	SysUtils, 
	ACBrBase, 
	ACBrSATExtratoClass, 
	ACBrSATExtratoReportClass,
  pcnCFe, 
	pcnCFeCanc, 
	pcnConversao,
	DB, 
	DBClient,
	frxClass, 
	frxExportPDF, 
	frxDBSet, 
	frxBarcode;

type
  EACBrSATExtratoFR = class(Exception);

  { TACBrSATExtratoFR }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrSATExtratoFR = class( TACBrSATExtratoReportClass )
  private
	FCFe: TCFe;
    FCFeCanc: TCFeCanc;
	
    cdsIdentificacao: TClientDataSet;
    cdsEmitente: TClientDataSet;
    cdsParametros: TClientDataSet;
    cdsDadosProdutos: TClientDataSet;
    cdsInformacoesAdicionais: TClientDataSet;
    cdsCalculoImposto: TClientDataSet;
    cdsFormaPagamento: TClientDataSet;
    cdsEntrega: TClientDataSet;
    frxIdentificacao: TfrxDBDataset;
    frxEmitente: TfrxDBDataset;
    frxParametros: TfrxDBDataset;
    frxDadosProdutos: TfrxDBDataset;
    frxInformacoesAdicionais: TfrxDBDataset;
    frxCalculoImposto: TfrxDBDataset;
    frxFormaPagamento: TfrxDBDataset;
    frxEntrega: TfrxDBDataset;
	frxReport : TfrxReport;
	frxPDFExport : TfrxPDFExport;
	frxBarCodeObject : TfrxBarCodeObject;

    FFastExtrato             : string;
    FFastExtratoResumido     : string;
    FFastExtratoCancelamento : string;
    procedure CriarDataSetsFrx;
	procedure SetDataSetsToFrxReport;
	procedure frxReportBeforePrint(Sender: TfrxReportComponent);
	function PrepareReport(ACFe: TCFe): Boolean;
	
	procedure CarregaIdentificacao;
    procedure CarregaEmitente;
    procedure CarregaParametros;
    procedure CarregaInformacoesAdicionais;
    procedure CarregaDadosEntrega;
    procedure CarregaDadosProdutos;
    procedure CarregaCalculoImposto;
    procedure CarregaFormaPagamento;
    procedure AjustaMargensReports;

	procedure CarregaDados;
    procedure LimpaDados;	
	
  protected
    procedure Imprimir;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure ImprimirExtrato(ACFe: TCFe = nil); override;
    procedure ImprimirExtratoResumido(ACFe : TCFe = nil); override;
    procedure ImprimirExtratoCancelamento(ACFe : TCFe = nil; ACFeCanc: TCFeCanc = nil); override;
  published
    property FastExtrato             : string read FFastExtrato             write FFastExtrato;
    property FastExtratoResumido     : string read FFastExtratoResumido     write FFastExtratoResumido;
    property FastExtratoCancelamento : string read FFastExtratoCancelamento write FFastExtratoCancelamento;
  end ;

implementation

uses
  ACBrDFeReport, ACBrValidador, StrUtils, ACBrDelphiZXingQRCode, ACBrUtil, ACBrDFeUtil, ACBrSAT;

{ TACBrSATExtratoFR }

procedure TACBrSATExtratoFR.frxReportBeforePrint(Sender: TfrxReportComponent);
var
  qrCode: string;
begin
  if Assigned(Self.CFe) then
  begin
    with Self.CFe do
      qrCode := Self.CalcularConteudoQRCode(infCFe.ID,
                                                             ide.dEmi+ide.hEmi,
                                                             Total.vCFe,
                                                             Trim(Dest.CNPJCPF),
                                                             ide.assinaturaQRCODE);

    if Assigned(Sender) and (Trim(qrCode) <> '') and (Sender.Name = 'ImgQrCode') then
       PintarQRCode(qrCode, TfrxPictureView(Sender).Picture.Bitmap, qrUTF8NoBOM);
  end;
end;

procedure TACBrSATExtratoFR.SetDataSetsToFrxReport;
begin
  frxReport.EnabledDataSets.Clear;

  frxReport.EnabledDataSets.Add(frxIdentificacao);
  frxReport.EnabledDataSets.Add(frxEmitente);
  frxReport.EnabledDataSets.Add(frxParametros);
  frxReport.EnabledDataSets.Add(frxDadosProdutos);
  frxReport.EnabledDataSets.Add(frxInformacoesAdicionais);
  frxReport.EnabledDataSets.Add(frxCalculoImposto);
  frxReport.EnabledDataSets.Add(frxFormaPagamento);
  frxReport.EnabledDataSets.Add(frxEntrega);
end;

procedure TACBrSATExtratoFR.CarregaDados;
begin
  LimpaDados;

  CarregaParametros;
  CarregaIdentificacao;
  CarregaEmitente;
  CarregaDadosProdutos;
  CarregaInformacoesAdicionais;
  CarregaCalculoImposto;
  CarregaFormaPagamento;
  CarregaDadosEntrega
end;

procedure TACBrSATExtratoFR.CarregaParametros;
begin
  with cdsParametros do
  begin
    Append;

    FieldByName('QtdeItens').AsInteger := FCFe.Det.Count;
    FieldByName('Imagem').AsString := Ifthen(Self.Logo <> '', Self.Logo,'');
    FieldByName('Sistema').AsString := Ifthen(Self.Sistema <> '',Self.Sistema,'Projeto ACBr - https://www.projetoacbr.com.br');
    FieldByName('Usuario').AsString := Ifthen(Self.Usuario <> '', Self.Usuario,'');

    Post;
  end;
end;

procedure TACBrSATExtratoFR.LimpaDados;
begin
  cdsIdentificacao.EmptyDataSet;
  cdsEmitente.EmptyDataSet;
  cdsParametros.EmptyDataSet;
  cdsDadosProdutos.EmptyDataSet;
  cdsInformacoesAdicionais.EmptyDataSet;
  cdsCalculoImposto.EmptyDataSet;
  cdsFormaPagamento.EmptyDataSet;
  cdsEntrega.EmptyDataSet;
end;

function TACBrSATExtratoFR.PrepareReport(ACFe: TCFe): Boolean;
var
  Stream: TStringStream;
begin
  Result := False;
  SetDataSetsToFrxReport;

  if Trim(FastExtrato) <> '' then
  begin
    if not (UpperCase(Copy(FastExtrato, Length(FastExtrato)-3, 4)) = '.FR3') then
    begin
      Stream := TStringStream.Create(FastExtrato);
	  try
		frxReport.FileName := '';
		frxReport.LoadFromStream(Stream);
	  finally
		Stream.Free;
	  end;
    end
    else
    begin
      if FileExists(FastExtrato) then
        frxReport.LoadFromFile(FastExtrato)
      else
        raise EACBrSATExtratoFR.CreateFmt('Caminho do arquivo de impressão do Extrato SAT "%s" inválido.', [FastExtrato]);
    end;
  end
  else
    raise EACBrSATExtratoFR.Create('Caminho do arquivo de impressão do Extrato SAT não assinalado.');

  frxReport.PrintOptions.Copies      := NumCopias;
  frxReport.PrintOptions.ShowDialog  := MostraSetup;
  frxReport.ShowProgress             := MostraStatus;
  frxReport.PreviewOptions.AllowEdit := False;

  // Define a impressora
  if NaoEstaVazio(frxReport.PrintOptions.Printer) then
    frxReport.PrintOptions.Printer := Impressora;

  if Assigned(ACFe) then
  begin
    FCFe := ACFe;
    SetDataSetsToFrxReport;
    Result := frxReport.PrepareReport;
  end
  else
  begin
    if Assigned(ACBrSAT) then
    begin
      FCFe := TACBrSAT (ACBrSAT).CFe;
      CarregaDados;

      Result := frxReport.PrepareReport(False);
    end
    else
      raise EACBrSATExtratoFR.Create('Propriedade ACBrSAT não assinalada.');
  end;

  AjustaMargensReports;
end;


constructor TACBrSATExtratoFR.Create(AOwner: TComponent);
begin
   inherited create(AOwner);
   FFastExtrato             := '';
   FFastExtratoResumido     := '';
   FFastExtratoCancelamento := '';

   CriarDataSetsFrx;
end;

destructor TACBrSATExtratoFR.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrSATExtratoFR.Imprimir;
begin

end;

procedure TACBrSATExtratoFR.ImprimirExtrato(ACFe: TCFe);
begin
  inherited;
  Imprimir;
end;

procedure TACBrSATExtratoFR.ImprimirExtratoCancelamento(ACFe: TCFe;
  ACFeCanc: TCFeCanc);
begin
  inherited;
  Imprimir;
end;

procedure TACBrSATExtratoFR.ImprimirExtratoResumido(ACFe: TCFe);
begin
  inherited;
  Imprimir;
end;

procedure TACBrSATExtratoFR.CriarDataSetsFrx;
begin
  frxReport := TfrxReport.Create(ACBrSAT);

  with frxReport do
  begin
    EngineOptions.UseGlobalDataSetList := False;
    ScriptLanguage := 'PascalScript';
    StoreInDFM     := False;
    OnBeforePrint  := frxReportBeforePrint;
    OnReportPrint  := 'frxReportOnReportPrint';
    PreviewOptions.Buttons :=[pbExport, pbPrint, pbZoom, pbFind, pbNavigator, pbExportQuick];
  end;

  frxPDFExport := TfrxPDFExport.Create(Self);
  frxPDFExport.PrintOptimized := True;
  frxPDFExport.ShowProgress := False;

  frxBarCodeObject := TfrxBarCodeObject.Create(Self);

  cdsIdentificacao := TClientDataSet.Create(Self);
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
    Add('nserieSAT', ftString, 15);
    Add('nCFe', ftString, 15);
    Add('modal', ftInteger);
    Add('dhEmi', ftDateTime);
    Add('tpEmis', ftInteger);
    Add('UFIni', ftString, 2);
    Add('UFFim', ftString, 2);
    Add('CPFConsumidor', ftString, 45);

    CreateDataSet;
  end;

  cdsEmitente := TClientDataSet.Create(Self);
  with cdsEmitente, FieldDefs do
  begin
    Close;
    Clear;
    Add('CNPJ', ftString, 18);
    Add('IE', ftString, 14);
    Add('IM', ftString, 14);
    Add('xNome', ftString, 60);
    Add('xFant', ftString, 60);
    Add('xLgr', ftString, 60);
    Add('Nro', ftString, 60);
    Add('xBairro', ftString, 60);
    Add('xMun', ftString, 60);
    Add('CEP', ftString, 9);
    Add('email', ftString, 60);
    Add('site', ftString, 60);
    CreateDataSet;
  end;

  cdsParametros := TClientDataSet.Create(Self);
  with cdsParametros, FieldDefs do
  begin
    Close;
    Clear;
    Add('Versao', ftString, 5);
    Add('Imagem', ftString, 256);
    Add('Sistema', ftString, 150);
    Add('Usuario', ftString, 60);
    Add('QrCodeCarregado', ftGraphic, 1000);
    Add('LogoCarregado', ftBlob);
    Add('QtdeItens', ftInteger);
    CreateDataSet;
  end;

  cdsDadosProdutos := TClientDataSet.Create(Self);
  with cdsDadosProdutos, FieldDefs do
  begin
    Close;
    Clear;
    Add('nItem', ftInteger);
    Add('ChaveCFe', ftString, 50);
    Add('CProd', ftString, 60);
    Add('xProd', ftString, 120);
    Add('CFOP', ftString, 4);
    Add('UCom', ftString, 6);
    Add('QCom', ftFloat);
    Add('VUnCom', ftFloat);
    Add('VProd' , ftString, 18);
    Add('indRegra', ftString, 1);
    Add('vItem',ftFloat);
    Add('vTR', ftString, 25);
    Add('vOutro', ftString, 18);
    Add('vDesc', ftString, 18);
    Add('vRatDesc', ftString, 18);
    Add('vRatAcr', ftString, 18);
    Add('vBC', ftFloat);
    Add('vDeducISSQN', ftFloat);
    Add('Valorliquido', ftString, 18);
    Add('ValorAcrescimos', ftString, 18);
    CreateDataSet;
  end;

  cdsCalculoImposto := TClientDataSet.Create(Self);
  with cdsCalculoImposto, FieldDefs do
  begin
    Add('VICMS', ftFloat);
    Add('VProd', ftFloat);
    Add('VDesc', ftFloat);
    Add('VPIS', ftFloat);
    Add('VCOFINS', ftFloat);
    Add('VOutro', ftFloat);
    Add('vCFe', ftFloat);
    Add('vTotPago', ftFloat);
    Add('vTroco', ftFloat);
    Add('vPISST', ftFloat);
    Add('vCOFINSST', ftFloat);
    Add('vAcresSubtot', ftFloat);
    Add('vDescSubtot', ftFloat);
    Add('vCFeLei12741', ftFloat);
    Add('vDescAcresItens', ftString, 18);
    CreateDataSet;
  end;

  cdsFormaPagamento := TClientDataSet.Create(Self);
  with cdsFormaPagamento, FieldDefs do
  begin
    Add('tPag', ftString, 17);
    Add('vMP', ftFloat);
    CreateDataSet;
  end;

  cdsEntrega := TClientDataSet.Create(Self);
  with cdsEntrega, FieldDefs do
  begin
    Add('EnderecoEntrega', ftString, 50);
    CreateDataSet;
  end;

  cdsInformacoesAdicionais := TClientDataSet.Create(Self);
  with cdsInformacoesAdicionais, FieldDefs do
  begin
    Add('infAdic', ftString, 6900);
    Add('obsFisco', ftString, 6900);
    CreateDataSet;
  end;

  frxIdentificacao := TfrxDBDataset.Create(Self);
  with frxIdentificacao do
  begin
     UserName := 'Identificacao';
     OpenDataSource := False;
     DataSet := cdsIdentificacao;
  end;

  frxEmitente := TfrxDBDataset.Create(Self);
  with frxEmitente do
  begin
     UserName := 'Emitente';
     OpenDataSource := False;
     DataSet := cdsEmitente;
  end;

  frxParametros := TfrxDBDataset.Create(Self);
  with frxParametros do
  begin
     UserName := 'Parametros';
     OpenDataSource := False;
     DataSet := cdsParametros;
  end;

  frxDadosProdutos := TfrxDBDataset.Create(Self);
  with frxDadosProdutos do
  begin
     UserName := 'DadosProdutos';
     OpenDataSource := False;
     DataSet := cdsDadosProdutos;
  end;

  frxInformacoesAdicionais := TfrxDBDataset.Create(Self);
  with frxInformacoesAdicionais do
  begin
     UserName := 'InformacoesAdicionais';
     OpenDataSource := False;
     DataSet := cdsInformacoesAdicionais;
  end;

  frxCalculoImposto := TfrxDBDataset.Create(Self);
  with frxCalculoImposto do
  begin
     UserName := 'CalculoImposto';
     OpenDataSource := False;
     DataSet := cdsCalculoImposto;
  end;

  frxFormaPagamento := TfrxDBDataset.Create(Self);
  with frxFormaPagamento do
  begin
     UserName := 'FormaPagamento';
     OpenDataSource := False;
     DataSet := cdsFormaPagamento;
  end;

  frxEntrega := TfrxDBDataset.Create(Self);
  with frxEntrega do
  begin
     UserName := 'DadosEntrega';
     OpenDataSource := False;
     DataSet := cdsEntrega;
  end;
end;

procedure TACBrSATExtratoFR.CarregaIdentificacao;
var NumExtrato: String;
begin
  with cdsIdentificacao do
  begin
    Append;

    with FCFe.infCFe do
    begin
      FieldByName('Id').AsString    := OnlyNumber(Id);
      FieldByName('Chave').AsString := FormatarChaveAcesso(Id);
    end;

    with FCFe.Ide do
    begin
      if tpAmb = taHomologacao then
        NumExtrato := '000,000,000'
      else
        NumExtrato := FormatFloat('000,000,000', nCFe);

      FieldByName('nCFe').AsString      := NumExtrato;
      FieldByName('tpAmb').AsInteger    := StrToIntDef(TpAmbToStr(tpAmb), 0);
      FieldByName('nserieSAT').AsString := FormatFloat('000,000,000', nserieSAT);
      FieldByName('dhEmi').AsString     := FormatDateTimeBr(dEmi + hEmi, 'DD/MM/YYYY - hh:nn:ss');
    end;

    if FCFe.Dest.CNPJCPF <> '' then
      FieldByName('CPFConsumidor').AsString := Format('CONSUMIDOR - CPF %s', [FormatarCPF(OnlyNumber(CFe.Dest.CNPJCPF))])
    else
      FieldByName('CPFConsumidor').AsString := 'CONSUMIDOR NÃO IDENTIFICADO';

    Post;
  end;
end;

procedure TACBrSATExtratoFR.CarregaEmitente;
begin
  with cdsEmitente do
  begin
    Append;

    with FCFe.emit do
    begin
      FieldByName('CNPJ').AsString  := FormatarCNPJ(CNPJ);
      FieldByName('IE').AsString    := IE;
      FieldByName('IM').AsString    := IM;
      FieldByName('XNome').AsString := xNome;
      FieldByName('XFant').AsString := XFant;

      with EnderEmit do
      begin
        FieldByName('Xlgr').AsString    := XLgr;
        FieldByName('Nro').AsString     := Nro;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('XMun').AsString    := XMun;
        FieldByName('CEP').AsString     := FormatarCEP(Poem_Zeros(CEP, 8));
        FieldByName('email').AsString   := email;
        FieldByName('site').AsString    := Self.Site;
      end;
    end;

    Post;
  end;
end;

procedure TACBrSATExtratoFR.CarregaDadosProdutos;
var
  inItem: Integer;
begin
  with cdsDadosProdutos do
  begin
    for inItem := 0 to (FCFe.Det.Count - 1) do
    begin
      Append;

      with FCFe.Det.Items[inItem] do
      begin
        FieldByName('nItem').AsInteger   := inItem + 1;
        FieldByName('ChaveCFe').AsString := FCFe.infCFe.ID;
        FieldByName('cProd').AsString    := Trim(Prod.cProd);
        FieldByName('xProd').AsString    := StringReplace( Prod.xProd, ';', #13, [rfReplaceAll]);
        FieldByName('CFOP').AsString     := Prod.CFOP;
        FieldByName('Ucom').AsString     := Prod.UCom;
        FieldByName('QCom').AsFloat      := Prod.QCom;
        FieldByName('vUnCom').AsFloat    := Prod.VUnCom;
        FieldByName('vProd').AsFloat     := Prod.VProd;
        FieldByName('indRegra').AsString := indRegraToStr(Prod.indRegra);
        FieldByName('vItem').AsFloat     := Prod.vItem;
        FieldByName('vDesc').AsString    := FormatFloatBr( Prod.vDesc,',0.00');
        FieldByName('vOutro').AsString   := FormatFloatBr( Prod.vOutro ,',0.00');

        if Imposto.vItem12741 > 0 then
          FieldByName('vTR').AsString := ' ('+FormatFloatBr(Imposto.vItem12741)+') '
        else
          FieldByName('vTR').AsString := '';

        FieldByName('Valorliquido').AsString    := FormatFloatBr( Prod.vProd - Prod.vDesc ,',0.00');
        FieldByName('ValorAcrescimos').AsString := FormatFloatBr( Prod.vProd + Prod.vOutro,',0.00');

        FieldByName('vDeducISSQN').AsFloat := Imposto.ISSQN.vDeducISSQN;
        FieldByName('vBC').AsFloat         := Imposto.ISSQN.vBC;
      end;

      Post;
    end;
  end;
end;

procedure TACBrSATExtratoFR.CarregaInformacoesAdicionais;
var i: Integer;
begin
  with FCFe, cdsInformacoesAdicionais do
  begin
    Append;

    if (Emit.cRegTrib = RTSimplesNacional) then
      FieldByName('ObsFisco').AsString := Msg_ICMS_123_2006;

    for i := 0 to obsFisco.Count - 1 do
      FieldByName('ObsFisco').AsString := FieldByName('ObsFisco').AsString + obsFisco[i].xCampo + '-' + obsFisco[i].xTexto;

    if (InfAdic.infCpl <> '') or (Self.ImprimeMsgOlhoNoImposto and (Total.vCFeLei12741 > 0)) then
      FieldByName('infAdic').AsString := StringReplace(InfAdic.infCpl,';',sLineBreak,[rfReplaceAll]);;

    Post;
  end;
end;

procedure TACBrSATExtratoFR.CarregaCalculoImposto;
begin
  with cdsCalculoImposto do
  begin
    Append;

    with FCFe.Total do
    begin
      FieldByName('VICMS').AsFloat     := ICMSTot.VICMS;
      FieldByName('VProd').AsFloat     := ICMSTot.VProd;
      FieldByName('VPIS').AsFloat      := ICMSTot.VPIS;
      FieldByName('VCOFINS').AsFloat   := ICMSTot.VCOFINS;
      FieldByName('vPISST').AsFloat    := ICMSTot.vPISST;
      FieldByName('vCOFINSST').AsFloat := ICMSTot.vCOFINSST;

      FieldByName('vDescSubtot').AsFloat  := DescAcrEntr.vDescSubtot;
      FieldByName('vAcresSubtot').AsFloat := DescAcrEntr.vAcresSubtot;

      if ICMSTot.vDesc > 0 then
         FieldByName('vDescAcresItens').AsString := FormatFloatBr(ICMSTot.vDesc, '-,0.00')
      else
      if ICMSTot.vOutro > 0 then
         FieldByName('vDescAcresItens').AsString := FormatFloatBr(ICMSTot.vOutro, '+,0.00');

      FieldByName('vCFe').AsFloat := vCFe;
      FieldByName('vCFeLei12741').AsFloat := vCFeLei12741;
    end;

    if FCFe.Pagto.vTroco > 0 then
    begin
      FieldByName('vTroco').AsCurrency   := FCFe.Pagto.vTroco;
      FieldByName('vTotPago').AsCurrency := FCFe.Pagto.vTroco+FieldByName('vCFe').AsFloat;
    end;

    Post;
  end;
end;

procedure TACBrSATExtratoFR.CarregaFormaPagamento;
var 
	i: Integer;
begin
  with cdsFormaPagamento do
  begin
    for i := 0 to FCFe.Pagto.Count - 1 do
    begin
      Append;

      with FCFe.Pagto.Items[i] do
      begin
        FieldByName('tPag').AsString := CodigoMPToDescricao(cMP);
        FieldByName('vMP').AsFloat   := vMP;
      end;

      Post;
    end;
  end;
end;

procedure TACBrSATExtratoFR.CarregaDadosEntrega;
begin
  with cdsEntrega, FCFe.Entrega do
  begin
    if xLgr <> '' then
    begin
      Append;
      FieldByName('EnderecoEntrega').AsString := Format('%s, nº %s - %s - %s', [xLgr, nro, xBairro, xMun]);
      Post;
    end;
  end;
end;

procedure TACBrSATExtratoFR.AjustaMargensReports;
var
  Page: TfrxReportPage;
  i: Integer;
begin
  for i := 0 to (frxReport.PreviewPages.Count - 1) do
  begin
    Page := frxReport.PreviewPages.Page[i];
    if (MargemSuperior > 0) then
      Page.TopMargin := MargemSuperior;
    if (MargemInferior > 0) then
      Page.BottomMargin := MargemInferior;
    if (MargemEsquerda > 0) then
      Page.LeftMargin := MargemEsquerda;
    if (MargemDireita > 0) then
      Page.RightMargin := MargemDireita;
  end;
end;


end.
