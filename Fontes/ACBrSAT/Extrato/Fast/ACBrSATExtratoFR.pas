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

  { TACBrSATExtratoFR }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrSATExtratoFR = class( TACBrSATExtratoReportClass )
  private

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
		procedure frxReportBeforePrint(Sender: TfrxReportComponent);
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
  ACBrDFeReport, ACBrValidador, StrUtils, ACBrDelphiZXingQRCode;

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

end.
