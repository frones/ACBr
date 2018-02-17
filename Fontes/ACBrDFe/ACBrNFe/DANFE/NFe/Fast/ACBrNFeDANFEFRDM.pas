{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                          }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
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
|* 11/08/2010: Itamar Luiz Bermond
|*  - Inicio do desenvolvimento
|* 24/08/2010: Régys Silveira
|*  - Acerto nas diretivas de compilação para Delphi 2009 e superior
|*  - Acertos gerais no DANFE
|        . Layout
|        . Exibição da logomarca
|        . Tamanho das colunas para conter valores grandes
|        . marca d'agua para ambiente de homologação
|        . Adicionado o complemento a descrição da mercadoria
|        . Adicionado a origem no CST
|        . Acerto para mostrar o CST corretamente quando for Simples Nacional
|*  - Padronização da logomarca para utilizar o caminho como nos outros DANFEs
|*  - Acerto no CST para o Simples Nacional
|*  - Acertos no DANFE para o Simples Nacional
|* 25/08/2010: Régys Silveira
|*  - Configuração do preview do DANFE.
|* 26/08/2010: Régys Silveira, Itamar Bermond
|*  - Desmarcada a propriedade StoreInDFM do FastReport para não gravar
|*    o relatório no DFM e evitar o erro de compilação em versão menores
|*    do delphi, favor utilizar o arquivo externo.
|* 26/02/2013: João Henrique de Souza
|*  - Foi realizado inúmeras modificações para Normalizar o Danfe com o Manual
|*    e ter uma versão que fosse possível imprimir com o FR que vem com o Delphi
|* 02/04/2015: Isaque Pinheiro
|*  - Criação de uma class, removendo o datamodule e os componentes não visuais
|*    dele, sendo criado todos em tempo de execução.
******************************************************************************}
{$I ACBr.inc}

unit ACBrNFeDANFEFRDM;

interface

uses
  pcnEnvEventoNFe, pcnRetInutNFe,
  SysUtils, Classes, ACBrNFeDANFEClass, pcnNFe, frxClass, frxExportPDF, DB,
  DBClient, frxDBSet, pcnConversao, ACBrUtil, frxBarcode,
  ACBrDelphiZXingQrCode, Graphics;

type
  ArrOfStr = Array of String;
  TSplitResult = array of string;

  TACBrNFeFRClass = class
  private
    FDANFEClassOwner: TACBrNFeDANFEClass;
    FNFe: TNFe;
    FEvento: TEventoNFe;
    FExibirTotalTributosItem: Boolean;
    FExibeCampoFatura: Boolean;
    FTributosFonte: string;
    FTributosPercentual: TpcnPercentualTributos;
    FTributosPercentualPersonalizado: double;
    FMarcaDaguaMSG: string;
    FvTroco: Currency;
    FDetalhado: Boolean;
    FURLConsultaPublica:String;
    FDescricaoViaEstabelec: string;
    FImprimirUnQtVlComercial: TImprimirUnidQtdeValor;
    FExpandirDadosAdicionaisAuto: boolean;
    FImprimirDadosArma: Boolean;
    fQuebraLinhaEmDetalhamentoEspecifico : Boolean;
    fsQuebraLinha : String;
    FfrxReport: TfrxReport;
    FfrxPDFExport: TfrxPDFExport;
    FfrxBarCodeObject: TfrxBarCodeObject;
    cdsIdentificacao: TClientDataSet;
    FfrxIdentificacao: TfrxDBDataset;
    cdsEmitente: TClientDataSet;
    FfrxEmitente: TfrxDBDataset;
    cdsDestinatario: TClientDataSet;
    FfrxDestinatario: TfrxDBDataset;
    cdsDadosProdutos: TClientDataSet;
    FfrxDadosProdutos: TfrxDBDataset;
    cdsParametros: TClientDataSet;
    FfrxParametros: TfrxDBDataset;
    cdsDuplicatas: TClientDataSet;
    FfrxDuplicatas: TfrxDBDataset;
    cdsCalculoImposto: TClientDataSet;
    FfrxCalculoImposto: TfrxDBDataset;
    cdsTransportador: TClientDataSet;
    FfrxTransportador: TfrxDBDataset;
    cdsVeiculo: TClientDataSet;
    FfrxVeiculo: TfrxDBDataset;
    cdsVolumes: TClientDataSet;
    FfrxVolumes: TfrxDBDataset;
    cdsEventos: TClientDataSet;
    FfrxEventos: TfrxDBDataset;
    cdsISSQN: TClientDataSet;
    FfrxISSQN: TfrxDBDataset;
    cdsFatura: TClientDataSet;
    FfrxFatura: TfrxDBDataset;
    cdsLocalRetirada: TClientDataSet;
    FfrxLocalRetirada: TfrxDBDataset;
    cdsLocalEntrega: TClientDataSet;
    FfrxLocalEntrega: TfrxDBDataset;
    cdsInformacoesAdicionais: TClientDataSet;
    FfrxInformacoesAdicionais: TfrxDBDataset;
    cdsPagamento: TClientDataSet;
    FfrxPagamento: TfrxDBDataset;
    FIncorporarFontesPdf: Boolean;
    FIncorporarBackgroundPdf: Boolean;
    FInutilizacao: TRetInutNFe;
    FfrxInutilizacao: TfrxDBDataset;
    cdsInutilizacao: TClientDataSet;
    FImprimirDadosDocReferenciados: Boolean;

    procedure frxReportBeforePrint(Sender: TfrxReportComponent);
    procedure CarregaIdentificacao;
    procedure CarregaEmitente;
    procedure CarregaDestinatario;
    procedure CarregaDadosProdutos;
    procedure CarregaParametros;
    procedure CarregaCalculoImposto;
    procedure CarregaTransportador;
    procedure CarregaVeiculo;
    procedure CarregaVolumes;
    procedure CarregaDuplicatas;
    procedure CarregaISSQN;
    procedure CarregaLocalRetirada;
    procedure CarregaLocalEntrega;
    procedure CarregaFatura;
    procedure CarregaPagamento;
    procedure CarregaInformacoesAdicionais;
    function QuebraLinha: String;

    function SubstrCount(const ASubString, AString: string): Integer;
    function Split(const ADelimiter, AString: string): TSplitResult;
    function CollateBr(Str: String): String;
    function Explode(sPart, sInput: String): ArrOfStr;
    function ManterVprod(dVProd, dvDesc: Double): String;
    function ManterdvTotTrib(dvTotTrib: Double):  String;
    function ManterVDesc(dvDesc: Currency; dVUnCom , dQCom : double ) : Double;
    function ManterCst(dCRT: TpcnCRT; dCSOSN: TpcnCSOSNIcms;
      dCST: TpcnCSTIcms): String;
    function ManterArma(inItem: integer): String;
    function ManterMedicamentos(inItem: integer): String;
    function ManterVeiculos( inItem : integer): String;
    function ManterValAprox( inItem : Integer ): String;
    function ManterInfAProd( inItem : Integer; sinfAdProd : String ) : String;
    function ManterDescricaoProduto(sXProd, sinfAdProd: String): String;
    function ManterVTribPerc(dVTotTrib, dVProd, dVNF: Double): Double;
    function ManterCombustivel(inItem: integer): String;
    function FormatQuantidade(dValor: Double): String;
    function FormatValorUnitario(dValor: Double): String;
    function ManterContingencia(swObs: String): String;
    function ManterInfAdi(swObs: String): String;
    function ManterRastro(inItem: integer): String;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property NFe: TNFe read FNFe write FNFe;
    property Evento: TEventoNFe read FEvento write FEvento;
    property Inutilizacao: TRetInutNFe read FInutilizacao write FInutilizacao;
    property DANFEClassOwner: TACBrNFeDANFEClass read FDANFEClassOwner;
    property ExibirTotalTributosItem: Boolean read FExibirTotalTributosItem write FExibirTotalTributosItem default False;
    property ExibeCampoFatura: Boolean read FExibeCampoFatura write FExibeCampoFatura default True;
    property TributosFonte: string read FTributosFonte write FTributosFonte;
    property TributosPercentual: TpcnPercentualTributos read FTributosPercentual write FTributosPercentual;
    property TributosPercentualPersonalizado: double read FTributosPercentualPersonalizado write FTributosPercentualPersonalizado;
    property MarcaDaguaMSG: string read FMarcaDaguaMSG write FMarcaDaguaMSG;
    property ImprimirUnQtVlComercial: TImprimirUnidQtdeValor read FImprimirUnQtVlComercial write FImprimirUnQtVlComercial;
    property ExpandirDadosAdicionaisAuto: boolean read FExpandirDadosAdicionaisAuto write FExpandirDadosAdicionaisAuto;
    property vTroco: Currency read FvTroco write FvTroco;
    property Detalhado: Boolean read FDetalhado write FDetalhado;
    property URLConsultaPublica:String read FURLConsultaPublica write FURLConsultaPublica;
    property DescricaoViaEstabelec: string read FDescricaoViaEstabelec write FDescricaoViaEstabelec;
    property frxReport: TfrxReport read FfrxReport write FfrxReport;
    property frxPDFExport: TfrxPDFExport read FfrxPDFExport write FfrxPDFExport;
    property ImprimirDadosArma: Boolean read FImprimirDadosArma write FImprimirDadosArma;
    property QuebraLinhaEmDetalhamentoEspecifico : Boolean  read fQuebraLinhaEmDetalhamentoEspecifico write fQuebraLinhaEmDetalhamentoEspecifico;
    property sQuebraLinha : String read fsQuebraLinha Write    fsQuebraLinha;
    property IncorporarBackgroundPdf: Boolean read FIncorporarBackgroundPdf write FIncorporarBackgroundPdf;
    property IncorporarFontesPdf: Boolean read FIncorporarFontesPdf write FIncorporarFontesPdf;
    property ImprimirDadosDocReferenciados: Boolean read FImprimirDadosDocReferenciados write FImprimirDadosDocReferenciados; 

    procedure SetDataSetsToFrxReport;
    procedure CarregaDadosNFe;
    procedure CarregaDadosEventos;
    procedure CarregaDadosInutilizacao;
    procedure PintarQRCode(QRCodeData: String; APict: TPicture);

  end;


implementation

uses ACBrNFe, ACBrDFeUtil, StrUtils, Math, DateUtils, pcnConversaoNFe,
  ACBrValidador;

{ TACBrNFeFRClass }

function TACBrNFeFRClass.SubstrCount(const ASubString, AString: string): Integer;
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

procedure TACBrNFeFRClass.SetDataSetsToFrxReport;
begin
  frxReport.EnabledDataSets.Clear;
  frxReport.EnabledDataSets.Add(FfrxIdentificacao);
  frxReport.EnabledDataSets.Add(FfrxEmitente);
  frxReport.EnabledDataSets.Add(FfrxDestinatario);
  frxReport.EnabledDataSets.Add(FfrxDadosProdutos);
  frxReport.EnabledDataSets.Add(FfrxCalculoImposto);
  frxReport.EnabledDataSets.Add(FfrxTransportador);
  frxReport.EnabledDataSets.Add(FfrxVeiculo);
  frxReport.EnabledDataSets.Add(FfrxVolumes);
  frxReport.EnabledDataSets.Add(FfrxEventos);
  frxReport.EnabledDataSets.Add(FfrxISSQN);
  frxReport.EnabledDataSets.Add(FfrxFatura);
  frxReport.EnabledDataSets.Add(FfrxLocalRetirada);
  frxReport.EnabledDataSets.Add(FfrxLocalEntrega);
  frxReport.EnabledDataSets.Add(FfrxInformacoesAdicionais);
  frxReport.EnabledDataSets.Add(FfrxPagamento);
  frxReport.EnabledDataSets.Add(FfrxParametros);
  frxReport.EnabledDataSets.Add(FfrxDuplicatas);
  frxReport.EnabledDataSets.Add(FfrxInutilizacao);
end;

function TACBrNFeFRClass.Split(const ADelimiter, AString: string): TSplitResult;
var
  vRows: TStrings;
  vI: Integer;
begin
  vRows := TStringList.Create;
  try
    vRows.Delimiter := ADelimiter[1];
    vRows.DelimitedText := AString;
    SetLength(Result, vRows.Count);

    for vI := 0 to vRows.Count - 1 do
      Result[vI] := vRows.Strings[vI];

  finally
    FreeAndNil(vRows);
  end;
end;

function TACBrNFeFRClass.Explode(sPart, sInput: String): ArrOfStr;
begin
  while Pos(sPart, sInput) <> 0 do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := Copy(sInput, 0, Pos(sPart, sInput) - 1);
      Delete(sInput, 1, Pos(sPart, sInput));
    end;

  SetLength(Result, Length(Result) + 1);
  Result[Length(Result) - 1] := sInput;
end;

function TACBrNFeFRClass.CollateBr(Str: String): String;
var
  Resultado,Temp: string;
  vChar: Char;
  Tamanho, i: integer;
begin
  Result := '';
  Tamanho := Length(str);
  i := 1;
  while i <= Tamanho do
  begin
    Temp := Copy(str,i,1);
    vChar := Temp[1];
    case vChar of
      'á', 'â', 'ã', 'à', 'ä', 'å', 'Á', 'Â', 'Ã', 'À', 'Ä', 'Å': Resultado := 'A';
      'é', 'ê', 'è', 'ë', 'É', 'Ê', 'È', 'Ë': Resultado := 'E';
      'í', 'î', 'ì', 'ï', 'Í', 'Î', 'Ì', 'Ï': Resultado := 'I';
      'ó', 'ô', 'õ', 'ò', 'ö', 'Ó', 'Ô', 'Õ', 'Ò', 'Ö': Resultado := 'O';
      'ú', 'û', 'ù', 'ü', 'Ú', 'Û', 'Ù', 'Ü': Resultado := 'U';
      'ç', 'Ç': Resultado := 'C';
      'ñ', 'Ñ': Resultado := 'N';
      'ý', 'ÿ', 'Ý', 'Y': Resultado := 'Y';
    else
      if vChar > #127 then Resultado := #32
      {$IFDEF DELPHI12_UP}
      else if CharInset(vChar, ['a'..'z','A'..'Z','0'..'9','-',' ',Chr(39)]) then
      {$ELSE}
      else if vChar in ['a'..'z','A'..'Z','0'..'9','-',' ',Chr(39)] then
      {$ENDIF}
        Resultado := UpperCase(vCHAR);
    end;
    Result := Result + Resultado;
    i := i + 1;
  end;
end;

procedure TACBrNFeFRClass.CarregaCalculoImposto;
begin
  with cdsCalculoImposto do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.Total.ICMSTot do
    begin
      FieldByName('VBC').AsFloat          := VBC;
      FieldByName('VICMS').AsFloat        := VICMS;
      FieldByName('VBCST').AsFloat        := VBCST;
      FieldByName('VST').AsFloat          := VST;
      FieldByName('VProd').AsFloat        := VProd;
      FieldByName('VFrete').AsFloat       := VFrete;
      FieldByName('VSeg').AsFloat         := VSeg;
      FieldByName('VDesc').AsFloat        := VDesc;
      FieldByName('VII').AsFloat          := VII;
      FieldByName('VIPI').AsFloat         := VIPI;
      FieldByName('VPIS').AsFloat         := VPIS;
      FieldByName('VCOFINS').AsFloat      := VCOFINS;
      FieldByName('VOutro').AsFloat       := VOutro;
      FieldByName('VNF').AsFloat          := VNF;
      FieldByName('VTotTrib').AsFloat     := VTotTrib;
      FieldByName('ValorApagar').AsFloat  := VProd- VDesc + VOutro;
      FieldByName('VTribPerc').AsFloat    := ManterVTribPerc( VTotTrib , VProd ,VNF );
      if NaoEstaVazio(TributosFonte) then
        FieldByName('VTribFonte').AsString := '(Fonte: '+TributosFonte+')';
    end;
    if FNFe.pag.vTroco > 0 then
    begin
      FieldByName('vTroco').AsCurrency    := FNFe.pag.vTroco;
      FieldByName('vTotPago').AsCurrency  := FNFe.pag.vTroco+FieldByName('VProd').AsFloat;
    end
    else
    begin
      FieldByName('vTroco').AsCurrency    := FvTroco;
      FieldByName('vTotPago').AsCurrency  := FvTroco+FieldByName('VProd').AsFloat;
    end;
    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaDadosNFe;
begin
  CarregaParametros;
  CarregaIdentificacao;
  CarregaEmitente;
  CarregaDestinatario;
  CarregaDadosProdutos;
  CarregaCalculoImposto;
  CarregaTransportador;
  CarregaVeiculo;
  CarregaVolumes;
  CarregaDuplicatas;
  CarregaISSQN;
  CarregaLocalRetirada;
  CarregaLocalEntrega;
  CarregaFatura;
  CarregaPagamento;
  CarregaInformacoesAdicionais;
end;

procedure TACBrNFeFRClass.CarregaDadosProdutos;
var
  inItem : Integer;
begin
  if not cdsParametros.Active then
    CarregaParametros;
  cdsParametros.First;

  // verificar se e DANFE detalhado
  // dados dos produtos
  with cdsDadosProdutos do
  begin
    Close;
    CreateDataSet;
    if (NFe.Ide.modelo <> 65) or FDetalhado then
    begin
      for inItem := 0 to NFe.Det.Count - 1 do
      begin
        Append;
        with FNFe.Det.Items[inItem] do
        begin
          FieldByName('ChaveNFe').AsString          := FNFe.infNFe.ID;
          FieldByName('cProd').AsString             := FDANFEClassOwner.ManterCodigo( Prod.cEAN,Prod.cProd);
          FieldByName('cEAN').AsString              := Prod.cEAN;
          FieldByName('XProd').AsString             := StringReplace( Prod.xProd, ';', #13, [rfReplaceAll]);
          FieldByName('VProd').AsString             := ManterVprod( Prod.VProd , Prod.vDesc );
          FieldByName('vTotTrib').AsString          := ManterdvTotTrib( Imposto.vTotTrib );
          FieldByName('infAdProd').AsString         := ManterInfAProd( inItem, infAdProd );
          FieldByName('DescricaoProduto').AsString  := ManterDescricaoProduto( FieldByName('XProd').AsString , FieldByName('infAdProd').AsString );
          FieldByName('NCM').AsString               := Prod.NCM;
          FieldByName('EXTIPI').AsString            := Prod.EXTIPI;
          FieldByName('genero').AsString            := '';
          FieldByName('CFOP').AsString              := Prod.CFOP;
          FieldByName('Ucom').AsString              := Prod.UCom;
          FieldByName('QCom').AsFloat               := Prod.QCom;
          FieldByName('VUnCom').AsFloat             := Prod.VUnCom;
          FieldByName('cEANTrib').AsString          := Prod.cEANTrib;
          FieldByName('UTrib').AsString             := Prod.uTrib;
          FieldByName('QTrib').AsFloat              := Prod.qTrib;
          FieldByName('VUnTrib').AsFloat            := Prod.vUnTrib;
          FieldByName('vFrete').AsString            := FormatFloatBr( Prod.vFrete ,'###,###,##0.00');
          FieldByName('vSeg').AsString              := FormatFloatBr( Prod.vSeg   ,'###,###,##0.00');
          FieldByName('vOutro').AsString            := FormatFloatBr( Prod.vOutro ,'###,###,##0.00');
          FieldByName('vDesc').AsString             := FormatFloatBr( ManterVDesc( Prod.vDesc , Prod.VUnCom , Prod.QCom),'###,###,##0.00');
          FieldByName('ORIGEM').AsString            := OrigToStr( Imposto.ICMS.orig);
          FieldByName('CST').AsString               := ManterCst( FNFe.Emit.CRT , Imposto.ICMS.CSOSN , Imposto.ICMS.CST );
          FieldByName('VBC').AsString               := FormatFloatBr( Imposto.ICMS.vBC        ,'###,###,##0.00');
          FieldByName('PICMS').AsString             := FormatFloatBr( Imposto.ICMS.pICMS      ,'###,###,##0.00');
          FieldByName('VICMS').AsString             := FormatFloatBr( Imposto.ICMS.vICMS      ,'###,###,##0.00');
          FieldByName('VBCST').AsString             := FormatFloatBr( Imposto.ICMS.vBcST      ,'###,###,##0.00');
          FieldByName('VICMSST').AsString           := FormatFloatBr( Imposto.ICMS.vICMSST    ,'###,###,##0.00');
          FieldByName('VIPI').AsString              := FormatFloatBr( Imposto.IPI.VIPI        ,'###,###,##0.00');
          FieldByName('PIPI').AsString              := FormatFloatBr( Imposto.IPI.PIPI        ,'###,###,##0.00');
          FieldByName('vISSQN').AsString            := FormatFloatBr( Imposto.ISSQN.vISSQN    ,'###,###,##0.00');
          FieldByName('vBcISSQN').AsString          := FormatFloatBr( Imposto.ISSQN.vBC       ,'###,###,##0.00');
          FieldByName('Valorliquido').AsString      := FormatFloatBr( Prod.vProd - Prod.vDesc ,'###,###,##0.00');
          FieldByName('ValorAcrescimos').AsString   := FormatFloatBr( Prod.vProd + Prod.vOutro,'###,###,##0.00');

          case FImprimirUnQtVlComercial of
          iuComercial:
            begin
              FieldByName('Unidade').AsString       := FieldByName('Ucom').AsString;
              FieldByName('Quantidade').AsString    := FormatQuantidade( FieldByName('QCom').AsFloat );
              FieldByName('ValorUnitario').AsString := FormatValorUnitario( FieldByName('VUnCom').AsFloat );
            end;
          iuTributavel:
            begin
              FieldByName('Unidade').AsString       := FieldByName('UTrib').AsString;
              FieldByName('Quantidade').AsString    := FormatQuantidade( FieldByName('QTrib').AsFloat );
              FieldByName('ValorUnitario').AsString := FormatValorUnitario( FieldByName('VUnTrib').AsFloat);
            end;
          iuComercialETributavel:
            begin
              if FieldByName('Ucom').AsString = FieldByName('UTrib').AsString then
              begin
                FieldByName('Unidade').AsString       := FieldByName('Ucom').AsString;
                FieldByName('Quantidade').AsString    := FormatQuantidade( FieldByName('QCom').AsFloat );
                FieldByName('ValorUnitario').AsString := FormatValorUnitario( FieldByName('VUnCom').AsFloat );
              end
              else
              begin
                FieldByName('Unidade').AsString       := FDANFEClassOwner.ManterUnidades(FieldByName('Ucom').AsString, FieldByName('UTrib').AsString);
                FieldByName('Quantidade').AsString    := FDANFEClassOwner.ManterQuantidades(FieldByName('QCom').AsFloat, FieldByName('QTrib').AsFloat);
                FieldByName('ValorUnitario').AsString := FDANFEClassOwner.ManterValoresUnitarios(FieldByName('VUnCom').AsFloat, FieldByName('VUnTrib').AsFloat);
              end;
            end;
          end;
          Post;
        end;
      end;
    end;
  end;
end;

procedure TACBrNFeFRClass.CarregaDestinatario;
begin
  { destinatário }
  with cdsDestinatario do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.Dest do
    begin
      if NaoEstaVazio(idEstrangeiro) then
        FieldByName('CNPJCPF').AsString := idEstrangeiro
      else
        FieldByName('CNPJCPF').AsString := FormatarCNPJouCPF(CNPJCPF);

      FieldByName('IE').AsString        := IE;
      FieldByName('XNome').AsString     := XNome;
      with EnderDest do
      begin
        FieldByName('XLgr').AsString    := XLgr;
        FieldByName('Nro').AsString     := Nro;
        FieldByName('XCpl').AsString    := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString    := IntToStr(CMun);
        FieldByName('XMun').AsString    := CollateBr(XMun);
        FieldByName('UF').AsString      := UF;
        FieldByName('CEP').AsString     := FormatarCEP(CEP);
        FieldByName('CPais').AsString   := IntToStr(CPais);
        FieldByName('XPais').AsString   := XPais;
        FieldByName('Fone').AsString    := FormatarFone(Fone);
      end;

      FieldByName('Consumidor').AsString := '';

      if (cdsIdentificacao.FieldByName('Mod_').AsString = '65') then
      begin
        if NaoEstaVazio(idEstrangeiro) then
          FieldByName('Consumidor').AsString := 'ESTRANGEIRO: ' + Trim(FieldByName('CNPJCPF').AsString) + ' ' + trim(FieldByName('XNome').AsString)
        else
        begin
          if (FieldByName('CNPJCPF').AsString = '') then
            FieldByName('Consumidor').AsString := ACBrStr('CONSUMIDOR NÃO IDENTIFICADO')
          else
            FieldByName('Consumidor').AsString :=
              IfThen(Length(CNPJCPF) = 11, 'CPF: ', 'CNPJ: ') + Trim(FieldByName('CNPJCPF').AsString) + ' ' + trim(FieldByName('XNome').AsString);
        end;

        if Trim(FieldByName('XLgr').AsString) <> '' then
          FieldByName('Consumidor').AsString := FieldByName('Consumidor').AsString + #13 +
            Trim(FieldByName('XLgr').AsString) + ', ' + Trim(FieldByName('Nro').AsString);
        if Trim(FieldByName('XCpl').AsString) <> '' then
          FieldByName('Consumidor').AsString := FieldByName('Consumidor').AsString + #13 +
            Trim(FieldByName('XCpl').AsString);

        if Trim(FieldByName('XMun').AsString) <> '' then
          FieldByName('Consumidor').AsString := FieldByName('Consumidor').AsString + #13 +
            Trim(FieldByName('XBairro').AsString) + ' - ' +
            Trim(FieldByName('XMun').AsString) + '/' +
            Trim(FieldByName('UF').AsString);
      end;
    end;
    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaDuplicatas;
var
  i: Integer;
begin
  cdsDuplicatas.Close;
  cdsDuplicatas.CreateDataSet;
  if Not ( fExibeCampoFatura and (FNFe.Ide.indPag = ipVista) and (FNFe.infNFe.Versao <= 3.10) ) then
  Begin

    with cdsDuplicatas do
    begin
      for i := 0 to NFe.Cobr.Dup.Count - 1 do
      begin
        Append;
        with FNFe.Cobr.Dup[i] do
        begin
          FieldByName('ChaveNFe').AsString  := FNFe.infNFe.ID;
          FieldByName('NDup').AsString      := NDup;
          FieldByName('DVenc').AsString     := FormatDateBr(DVenc);
          FieldByName('VDup').AsFloat       := VDup;
        end;
        Post;
      end;
    end;
  End;
end;

procedure TACBrNFeFRClass.CarregaEmitente;
begin
  { emitente }
  with cdsEmitente do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.Emit do
    begin
      FieldByName('CNPJ').AsString  := FormatarCNPJ(CNPJCPF);
      FieldByName('XNome').AsString := DANFEClassOwner.ManterNomeImpresso( XNome , XFant );
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
        FieldByName('CEP').AsString     := FormatarCEP(CEP);
        FieldByName('CPais').AsString   := IntToStr(CPais);
        FieldByName('XPais').AsString   := XPais;
        FieldByName('Fone').AsString    := FormatarFone(Fone);
      end;
      FieldByName('IE').AsString        := IE;
      FieldByName('IM').AsString        := IM;
      FieldByName('IEST').AsString      := IEST;
      FieldByName('CRT').AsString       := CRTToStr(CRT);

      if Trim(FieldByName('CRT').AsString) = '1' then
        FieldByName('DESCR_CST').AsString := 'CSOSN'
      else
        FieldByName('DESCR_CST').AsString := 'CST';

      cdsEmitente.FieldByName('DADOS_ENDERECO').AsString    := Trim(FieldByName('XLgr').AsString) + ', ' +
                                                                Trim(FieldByName('Nro').AsString);
	    if (trim(FieldByName('XCpl').AsString) <> '') then
        cdsEmitente.FieldByName('DADOS_ENDERECO').AsString  := cdsEmitente.FieldByName('DADOS_ENDERECO').AsString + ', ' +
                                                                Trim(FieldByName('XCpl').AsString);

      cdsEmitente.FieldByName('DADOS_ENDERECO').AsString    := cdsEmitente.FieldByName('DADOS_ENDERECO').AsString + ' - ' +
  										  	                                      Trim(FieldByName('XBairro').AsString) + ' - ' +
                                                                Trim(FieldByName('XMun').AsString) + ' - ' +
                                                                Trim(FieldByName('UF').AsString) + #13 +
		  	  				  				                                    'Fone: ' + Trim(FieldByName('Fone').AsString) +
                                                                IfThen(trim(FDANFEClassOwner.Fax) <> '', ' - FAX: ' + FormatarFone(trim(FDANFEClassOwner.Fax)),'')+
                                                                ' - CEP: ' + Trim(FieldByName('CEP').AsString);
      if trim(FDANFEClassOwner.Site) <> '' then
        cdsEmitente.FieldByName('DADOS_ENDERECO').AsString  := cdsEmitente.FieldByName('DADOS_ENDERECO').AsString + #13 +
                                                                trim(FDANFEClassOwner.Site);
      if trim(FDANFEClassOwner.Email) <> '' then
        cdsEmitente.FieldByName('DADOS_ENDERECO').AsString  := cdsEmitente.FieldByName('DADOS_ENDERECO').AsString + #13 +
                                                                Trim(FDANFEClassOwner.Email);
    end;

    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaFatura;
begin
  with cdsFatura do
  begin
    Close;
    CreateDataSet;

    if ExibeCampoFatura then
    begin
      Append;

      FieldByName('iForma').asInteger := Integer( FNFe.Ide.indPag);

      if FNFe.infNFe.Versao >= 4 then
        FieldByName('Pagamento').AsString := ACBrStr('DADOS DA FATURA')
      else
      begin
        case FNFe.Ide.indPag of
          ipVista : FieldByName('Pagamento').AsString := ACBrStr('PAGAMENTO À VISTA');
          ipPrazo : FieldByName('Pagamento').AsString := ACBrStr('PAGAMENTO A PRAZO');
          ipOutras: FieldByName('Pagamento').AsString := ACBrStr('OUTROS');
        end;
      end;

      if NaoEstaVazio(FNFe.Cobr.Fat.nFat) then
      begin
        with FNFe.Cobr.Fat do
        begin
          FieldByName('nfat').AsString  := nFat;
          FieldByName('vOrig').AsFloat  := vOrig;
          FieldByName('vDesc').AsFloat  := vDesc;
          FieldByName('vLiq').AsFloat   := vLiq;
        end;
      end;

      if ((FNFe.infNFe.Versao >= 4) or (FNFe.Ide.indPag = ipOutras)) and EstaVazio(FNFe.Cobr.Fat.nFat) then
        Cancel
      else
        Post;

    end;
  end;
end;

procedure TACBrNFeFRClass.CarregaPagamento;
var
  i: Integer;
begin
  with cdsPagamento do
  begin
    Close;
    CreateDataSet;
    for i := 0 to NFe.Pag.Count - 1 do
    begin
      Append;
      with FNFe.Pag[i] do
      begin
        FieldByName('tPag').AsString  := FormaPagamentoToDescricao( tPag );
        FieldByName('vPag').AsFloat   := vPag;
        // ver tpIntegra
        FieldByName('CNPJ').AsString  := FormatarCNPJ(CNPJ);
        FieldByName('tBand').AsString := BandeiraCartaoToDescStr( tBand );
        FieldByName('cAut').AsString  := cAut;
      end;
      Post;
    end;

    // acrescenta o troco
    if vTroco > 0 then
    begin
      Append;
      FieldByName('tPag').AsString  := 'Troco R$';
      FieldByName('vPag').AsFloat   := vTroco;
      Post;
    end;
  end;
end;

procedure TACBrNFeFRClass.CarregaIdentificacao;
begin
  with cdsIdentificacao do
  begin
    Close;
    CreateDataSet;
    Append;

    FieldByName('Id').AsString      := OnlyNumber(FNFe.infNFe.Id);
    FieldByName('Chave').AsString   := FormatarChaveAcesso(FNFe.infNFe.Id);
    FieldByName('CUF').AsString     := IntToStr(FNFe.Ide.CUF);
    FieldByName('CNF').AsString     := IntToStr(FNFe.Ide.CNF);
    FieldByName('NatOp').AsString   := FNFe.Ide.NatOp;
    FieldByName('IndPag').AsString  := IndpagToStr(FNFe.Ide.IndPag );
    FieldByName('Mod_').AsString    := IntToStr(FNFe.Ide.Modelo);
    FieldByName('Serie').AsString   := IntToStr(FNFe.Ide.Serie);
    FieldByName('NNF').AsString     := FormatarNumeroDocumentoFiscal(IntToStr(FNFe.Ide.NNF));
    FieldByName('DEmi').AsString    := FormatDateBr(FNFe.Ide.DEmi);
    FieldByName('DSaiEnt').AsString := IfThen(FNFe.Ide.DSaiEnt <> 0, FormatDateBr(FNFe.Ide.DSaiEnt));
    FieldByName('TpNF').AsString    := tpNFToStr( FNFe.Ide.TpNF );
    FieldByName('CMunFG').AsString  := IntToStr(FNFe.Ide.CMunFG);
    FieldByName('TpImp').AsString   := TpImpToStr( FNFe.Ide.TpImp );
    FieldByName('TpEmis').AsString  := TpEmisToStr( FNFe.Ide.TpEmis );
    FieldByName('CDV').AsString     := IntToStr(FNFe.Ide.CDV);
    FieldByName('TpAmb').AsString   := TpAmbToStr( FNFe.Ide.TpAmb );
    FieldByName('FinNFe').AsString  := FinNFeToStr( FNFe.Ide.FinNFe );
    FieldByName('ProcEmi').AsString := procEmiToStr( FNFe.Ide.ProcEmi );
    FieldByName('VerProc').AsString := FNFe.Ide.VerProc;
    if FNFe.infNFe.versao = 2.00 then
      FieldByName('HoraSaida').AsString := ifthen(FNFe.ide.hSaiEnt = 0, '', TimeToStr(FNFe.ide.hSaiEnt))
    else
      FieldByName('HoraSaida').AsString := ifthen(TimeOf(FNFe.ide.dSaiEnt)=0, '', TimeToStr(FNFe.ide.dSaiEnt));

    if (FNFe.Ide.Modelo = 65) then
    begin
      FieldByName('DEmi').AsString := FormatDateTimeBr(FNFe.Ide.DEmi);
      if FNFe.Ide.TpAmb = taHomologacao then
          FieldByName('MensagemFiscal').AsString := ACBrStr('EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL')
      else
      begin
        if FNFe.Ide.tpEmis <> teNormal then
          FieldByName('MensagemFiscal').AsString := ACBrStr('EMITIDA EM CONTINGÊNCIA'+LineBreak+'Pendente de autorização')
        else
          FieldByName('MensagemFiscal').AsString := ACBrStr('ÁREA DE MENSAGEM FISCAL');
      end;

      FieldByName('URL').AsString := TACBrNFe(DANFEClassOwner.ACBrNFe).GetURLConsultaNFCe(FNFe.Ide.cUF, FNFe.Ide.tpAmb, FNFe.infNFe.Versao);
    end
    else
    begin
      FieldByName('MensagemFiscal').AsString := '';
      FieldByName('URL').AsString            := '';
    end;
    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaInformacoesAdicionais;
var
  vTemp         : TStringList;
  IndexCampo    : Integer;
  Campos        : TSplitResult;
  BufferInfCpl  : String;
  wObs          : string;
  wLinhasObs    : integer;
begin
  wLinhasObs  := 0;
  BufferInfCpl:= '';
  vTemp       := TStringList.Create;
  try
    wObs  := FDANFEClassOwner.ManterDocreferenciados( FNFe,FImprimirDadosDocReferenciados , ';' );
    wObs  := ManterInfAdi( wObs );
    wObs  := ManterContingencia( wObs );
    if Trim(wObs) <> '' then
    begin
      Campos := Split(';', wObs);
      for IndexCampo := 0 to Length(Campos) - 1 do
        vTemp.Add(Campos[IndexCampo]);

      wLinhasObs    := 1; //TotalObS(vTemp.Text);
      BufferInfCpl  := vTemp.Text;
    end;
    with cdsInformacoesAdicionais do
    begin
      Close;
      CreateDataSet;
      Append;
      FieldByName('OBS').AsString        := BufferInfCpl;
      FieldByName('LinhasOBS').AsInteger := wLinhasObs;
      Post;
    end;
  finally
    vTemp.Free;
  end;
end;

procedure TACBrNFeFRClass.CarregaISSQN;
begin
  with cdsISSQN do
  begin
    Close;
    CreateDataSet;
    Append;
    with FNFe.Total.ISSQNtot do
    begin
      FieldByName('vSERV').AsFloat  := VServ;
      FieldByName('vBC').AsFloat    := VBC;
      FieldByName('vISS').AsFloat   := VISS;
      FieldByName('vDescIncond').AsFloat := vDescIncond;
    end;
    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaLocalEntrega;
begin
  { local de entrega }
  with cdsLocalEntrega do
  begin
    Close;
    CreateDataSet;

    if NaoEstaVazio(FNFe.Entrega.CNPJCPF) then
    begin
      Append;

      with FNFe.Entrega do
      begin
        FieldByName('CNPJ').AsString    := FormatarCNPJouCPF(CNPJCPF);;
        FieldByName('Xlgr').AsString    := XLgr;
        FieldByName('Nro').AsString     := Nro;
        FieldByName('XCpl').AsString    := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString    := inttostr(CMun);
        FieldByName('XMun').AsString    := CollateBr(XMun);
        FieldByName('UF').AsString      := UF;
      end;
      Post;
    end;
  end;
end;

procedure TACBrNFeFRClass.CarregaLocalRetirada;
begin
  { local de retirada }
  with cdsLocalRetirada do
  begin
    Close;
    CreateDataSet;

    if NaoEstaVazio(FNFe.Retirada.CNPJCPF) then
    begin
      Append;

      with FNFe.Retirada do
      begin
        FieldByName('CNPJ').AsString    := FormatarCNPJouCPF(CNPJCPF);
        FieldByName('Xlgr').AsString    := XLgr;
        FieldByName('Nro').AsString     := Nro;
        FieldByName('XCpl').AsString    := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString    := inttostr(CMun);
        FieldByName('XMun').AsString    := CollateBr(XMun);
        FieldByName('UF').AsString      := UF;
      end;
      Post;
    end;
  end;
end;

procedure TACBrNFeFRClass.CarregaParametros;
var
  vChave_Contingencia : String;
  vStream             : TMemoryStream;
  vStringStream       : TStringStream;
  P: Integer;
begin
  { parâmetros }
  with cdsParametros do
  begin
    Close;
    CreateDataSet;
    Append;

    FieldByName('poscanhoto').AsString            := IntToStr( Ord(DANFEClassOwner.PosCanhoto));
    FieldByName('ResumoCanhoto').AsString         := '';
    FieldByName('Mensagem0').AsString             := '';
    FieldByName('Contingencia_ID').AsString       := '';
    FieldByName('ConsultaAutenticidade').AsString := 'Consulta de autenticidade no portal nacional da NF-e'+#13+
                                                     'www.nfe.fazenda.gov.br/portal ou no site da Sefaz autorizadora';

    if Assigned(FNFe) then
    begin
      if DANFEClassOwner.ExibirResumoCanhoto then
      begin
         if EstaVazio(DANFEClassOwner.ExibirResumoCanhoto_Texto) then
          FieldByName('ResumoCanhoto').AsString := ACBrStr('Emissão: ' )+ FormatDateBr(FNFe.Ide.DEmi) + '  Dest/Reme: ' + FNFe.Dest.XNome + '  Valor Total: ' + FormatFloatBr(FNFe.Total.ICMSTot.VNF)
        else
          FieldByName('ResumoCanhoto').AsString := DANFEClassOwner.ExibirResumoCanhoto_Texto;
      end;

      if (FNFe.Ide.TpAmb = taHomologacao) then
      begin
        if (FNFe.Ide.tpEmis in [teContingencia, teFSDA, teSCAN, teDPEC, teSVCAN, teSVCRS, teSVCSP]) then
        begin
          if (FNFe.procNFe.cStat in [101,135, 151, 155]) then
            FieldByName('Mensagem0').AsString := ACBrStr('NFe sem Valor Fiscal - HOMOLOGAÇÃO ' +
                                                        #10#13+'NFe em Contingência - CANCELADA')
          else
            FieldByName('Mensagem0').AsString := ACBrStr('NFe sem Valor Fiscal - HOMOLOGAÇÃO'+
                                                        #10#13+'NFe em Contingência');
        end
        else
          FieldByName('Mensagem0').AsString :=ACBrStr( 'NFe sem Valor Fiscal - HOMOLOGAÇÃO')
      end
      else
      begin
        if not (FNFe.Ide.tpEmis in [teContingencia, teFSDA, teSVCAN, teSVCRS, teSVCSP]) then
        begin
          //prioridade para opção NFeCancelada
          if (FDANFEClassOwner.NFeCancelada) or
             ((NaoEstaVazio(FNFe.procNFe.nProt)) and
              (FNFe.procNFe.cStat in [101,135,151,155])) then
            FieldByName('Mensagem0').AsString := 'NFe Cancelada'
          else if ( FNFe.procNFe.cStat = 110 ) or
                  ( FNFe.procNFe.cStat = 301 ) or
                  ( FNFe.procNFe.cStat = 302 ) or
                  ( FNFe.procNFe.cStat = 303 ) then
            FieldByName('Mensagem0').AsString := 'NFe denegada pelo Fisco'
          else if ((EstaVazio(FDANFEClassOwner.ProtocoloNFe)) and
                   (EstaVazio(FNFe.procNFe.nProt))) then
            FieldByName('Mensagem0').AsString := ACBrStr( 'NFe sem Autorização de Uso da SEFAZ')
          else if (FNFe.Ide.tpImp = tiSimplificado) then
            FieldByName('Mensagem0').AsString := ACBrStr( 'EMISSÃO NORMAL' );
        end;
      end;

      case FNFe.Ide.tpEmis of
        teNormal,
        teSVCAN,
        teSCAN,
        teSVCRS,
        teSVCSP :   begin
                      FieldByName('ChaveAcesso_Descricao').AsString := 'CHAVE DE ACESSO';
                      FieldByName('Contingencia_ID').AsString := '';

                      if ((FDANFEClassOwner.NFeCancelada) or (FNFe.procNFe.cStat in [101,151,155])) then
                        FieldByName('Contingencia_Descricao').AsString := ACBrStr('PROTOCOLO DE HOMOLOGAÇÃO DO CANCELAMENTO' )
                      else if ( FNFe.procNFe.cStat = 110 ) or
                              ( FNFe.procNFe.cStat = 301 ) or
                              ( FNFe.procNFe.cStat = 302 ) or
                              ( FNFe.procNFe.cStat = 303 ) then
                        FieldByName('Contingencia_Descricao').AsString := ACBrStr('PROTOCOLO DE DENEGAÇÃO DE USO')
                      else
                        FieldByName('Contingencia_Descricao').AsString := ACBrStr('PROTOCOLO DE AUTORIZAÇÃO DE USO');

                      if EstaVazio(FDANFEClassOwner.ProtocoloNFe) then
                      begin
                        if EstaVazio(FNFe.procNFe.nProt) then
                          FieldByName('Contingencia_Valor').AsString := ACBrStr('NFe sem Autorização de Uso da SEFAZ')
                        else
                        begin
                          FieldByName('Contingencia_Valor').AsString := FNFe.procNFe.nProt + ' ' + IfThen(FNFe.procNFe.dhRecbto <> 0, DateTimeToStr(FNFe.procNFe.dhRecbto), '');
                          FieldByName('nProt').AsString := FNFe.procNfe.nProt;
                          FieldByName('dhRecbto').AsDateTime := FNFe.procNFe.dhRecbto;
                        end;
                      end
                      else
                      begin
                        FieldByName('Contingencia_Valor').AsString := FDANFEClassOwner.ProtocoloNFe;
                        P := Pos('-', FDANFEClassOwner.ProtocoloNFe);
                        if P = 0 then
                        begin
                          FieldByName('nProt').AsString := Trim(FDANFEClassOwner.ProtocoloNFe);
                          FieldByName('dhRecbto').AsDateTime := 0;
                        end
                        else
                        begin
                          FieldByName('nProt').AsString := Trim(Copy(FDANFEClassOwner.ProtocoloNFe, 1, P - 1));
                          FieldByName('dhRecbto').AsDateTime := StringToDateTimeDef(Trim(
                            Copy(FDANFEClassOwner.ProtocoloNFe, P + 1, Length(FDANFEClassOwner.ProtocoloNFe) - P)
                            ), 0, 'dd/mm/yyyy hh:nn:ss');
                        end;
                      end;
                    end;

        teContingencia ,
        teFSDA :    begin
                      vChave_Contingencia := TACBrNFe(DANFEClassOwner.ACBrNFe).GerarChaveContingencia(FNFe);
                      FieldByName('ChaveAcesso_Descricao').AsString  := 'CHAVE DE ACESSO';
                      FieldByName('Contingencia_ID').AsString        := vChave_Contingencia;
                      FieldByName('Contingencia_Descricao').AsString := 'DADOS DA NF-E';
                      FieldByName('Contingencia_Valor').AsString     := FormatarChaveAcesso(vChave_Contingencia);
                      FieldByName('ConsultaAutenticidade').AsString  := '';
                    end;

         teDPEC  :  begin
                      if NaoEstaVazio(FNFe.procNFe.nProt) then // DPEC TRANSMITIDO
                      begin
                        FieldByName('Contingencia_Descricao').AsString := ACBrStr( 'PROTOCOLO DE AUTORIZAÇÃO DE USO');
                        FieldByName('Contingencia_Valor').AsString     := FNFe.procNFe.nProt + ' ' + IfThen(FNFe.procNFe.dhRecbto <> 0, DateTimeToStr(FNFe.procNFe.dhRecbto), '');
                      end
                      else
                      begin
                        FieldByName('Contingencia_Descricao').AsString := ACBrStr('NÚMERO DE REGISTRO DPEC');
                        if NaoEstaVazio(FDANFEClassOwner.ProtocoloNFe) then
                          FieldByName('Contingencia_Valor').AsString := FDANFEClassOwner.ProtocoloNFe;
                      end;
                    end;

         teOffLine: begin
                      FieldByName('Contingencia_Valor').AsString := FNFe.procNFe.nProt + ' ' + IfThen(FNFe.procNFe.dhRecbto <> 0, DateTimeToStr(FNFe.procNFe.dhRecbto), '');
                      FieldByName('nProt').AsString := FNFe.procNfe.nProt;
                      FieldByName('dhRecbto').AsDateTime := FNFe.procNFe.dhRecbto;
                    end;
      end;

      FieldByName('QtdeItens').AsInteger := NFe.Det.Count;

    end;

    if NaoEstaVazio(FieldByName('Mensagem0').AsString) then
      FieldByName('Mensagem0').AsString  := FieldByName('Mensagem0').AsString+#10#13;

    FieldByName('Mensagem0').AsString                   := FieldByName('Mensagem0').AsString + MarcaDaguaMSG;
    FieldByName('LogoExpandido').AsString               := IfThen( FDANFEClassOwner.ExpandirLogoMarca, '1' , '0' );
    FieldByName('Sistema').AsString                     := IfThen( FDANFEClassOwner.Sistema <> '' , FDANFEClassOwner.Sistema, 'Projeto ACBr - http://acbr.sf.net');
    FieldByName('Usuario').AsString                     := IfThen( FDANFEClassOwner.Usuario <> '' , ' - ' + FDANFEClassOwner.Usuario , '' );
    FieldByName('Fax').AsString                         := IfThen( FDANFEClassOwner.Fax     <> '' , ' - FAX ' + FDANFEClassOwner.Fax , '');
    FieldByName('Site').AsString                        := FDANFEClassOwner.Site;
    FieldByName('Email').AsString                       := FDANFEClassOwner.Email;
    FieldByName('Desconto').AsString                    := IfThen( FDANFEClassOwner.ImprimirDescPorc , '%' , 'VALOR');
    FieldByName('TotalLiquido').AsString                := IfThen( FDANFEClassOwner.ImprimirTotalLiquido ,ACBrStr('LÍQUIDO') ,'TOTAL');
    FieldByName('LinhasPorPagina').AsInteger            := FDANFEClassOwner.ProdutosPorPagina;
    FieldByName('ExpandirDadosAdicionaisAuto').AsString := IfThen( ExpandirDadosAdicionaisAuto , 'S' , 'N');
    FieldByName('sDisplayFormat').AsString              := '###,###,###,##0.%.*d';
    FieldByName('iFormato').AsInteger                   := integer( FDANFEClassOwner.CasasDecimais.Formato );
    FieldByName('Mask_qCom').AsString                   := FDANFEClassOwner.CasasDecimais._Mask_qCom;
    FieldByName('Mask_vUnCom').AsString                 := FDANFEClassOwner.CasasDecimais._Mask_vUnCom;
    FieldByName('Casas_qCom').AsInteger                 := FDANFEClassOwner.CasasDecimais._qCom;
    FieldByName('Casas_vUnCom').AsInteger               := FDANFEClassOwner.CasasDecimais._vUnCom;
    FieldByName('DescricaoViaEstabelec').AsString       := FDescricaoViaEstabelec;
    FieldByName('ImprimeDescAcrescItem').AsInteger      := IfThen( FDANFEClassOwner.ImprimeDescAcrescItem, 1 , 0 );

    // Carregamento da imagem
    if NaoEstaVazio(DANFEClassOwner.Logo) then
    begin
      FieldByName('Imagem').AsString := DANFEClassOwner.Logo;
      vStream := TMemoryStream.Create;
      try
        if FileExists(DANFEClassOwner.Logo) then
          vStream.LoadFromFile(DANFEClassOwner.Logo)
        else
        begin
          vStringStream:= TStringStream.Create(DANFEClassOwner.Logo);
          try
            vStream.LoadFromStream(vStringStream);
          finally
            vStringStream.Free;
          end;
        end;
        vStream.Position := 0;
        TBlobField(cdsParametros.FieldByName('LogoCarregado')).LoadFromStream(vStream);
      finally
        vStream.Free;
      end;
    end;

    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaTransportador;
begin
  with cdsTransportador do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.Transp do
    begin
      FieldByName('ModFrete').AsString :=modFreteToDesStr( ModFrete );
      with Transporta do
      begin
        FieldByName('CNPJCPF').AsString := FormatarCNPJouCPF(CNPJCPF);
        FieldByName('XNome').AsString   := XNome;
        FieldByName('IE').AsString      := IE;
        FieldByName('XEnder').AsString  := XEnder;
        FieldByName('XMun').AsString    := CollateBr(XMun);
        FieldByName('UF').AsString      := UF;
      end;
    end;
    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaVeiculo;
begin
  with cdsVeiculo do
  begin
    Close;
    CreateDataSet;
    Append;
    with FNFe.Transp.VeicTransp do
    begin
      FieldByName('PLACA').AsString := Placa;
      FieldByName('UF').AsString    := UF;
      FieldByName('RNTC').AsString  := RNTC;
    end;

    Post;
  end;
end;

procedure TACBrNFeFRClass.CarregaVolumes;
var
  i: Integer;
begin
  with cdsVolumes do
  begin
    Close;
    CreateDataSet;
    for i := 0 to NFe.Transp.Vol.Count - 1 do
    begin
      Append;
      with FNFe.Transp.Vol[i] do
      begin
        FieldByName('QVol').AsFloat   := QVol;
        FieldByName('Esp').AsString   := Esp;
        FieldByName('Marca').AsString := Marca;
        FieldByName('NVol').AsString  := NVol;
        FieldByName('PesoL').AsFloat  := PesoL;
        FieldByName('PesoB').AsFloat  := PesoB;
      end;
      Post;
    end;
  end;
end;

procedure TACBrNFeFRClass.CarregaDadosEventos;
var
  i: Integer;
  CondicoesUso, Correcao: String;
begin
  with cdsEventos do
  begin
    Close;

    FieldDefs.Clear;
    FieldDefs.Add('DescricaoTipoEvento', ftString, 150);
    FieldDefs.Add('Modelo', ftString, 2);
    FieldDefs.Add('Serie', ftString, 3);
    FieldDefs.Add('Numero', ftString, 9);
    FieldDefs.Add('MesAno', ftString, 5);
    FieldDefs.Add('Barras', ftString, 44);
    FieldDefs.Add('ChaveAcesso', ftString, 60);
    FieldDefs.Add('cOrgao', ftInteger);
    FieldDefs.Add('tpAmb', ftString, 100);
    FieldDefs.Add('dhEvento', ftDateTime);
    FieldDefs.Add('TipoEvento', ftString, 6);
    FieldDefs.Add('DescEvento', ftString, 100);
    FieldDefs.Add('nSeqEvento', ftInteger);
    FieldDefs.Add('versaoEvento', ftString, 10);
    FieldDefs.Add('cStat', ftInteger);
    FieldDefs.Add('xMotivo', ftString, 100);
    FieldDefs.Add('nProt', ftString, 20);
    FieldDefs.Add('dhRegEvento', ftDateTime);
    FieldDefs.Add('xJust', ftBlob);
    FieldDefs.Add('xCondUso', ftBlob);
    FieldDefs.Add('xCorrecao', ftBlob);

    CreateDataSet;

    for i := 0 to FEvento.Evento.Count - 1 do
    begin
      Append;

      with Evento.Evento[i] do
      begin
        FieldByName('DescricaoTipoEvento').AsString := InfEvento.DescricaoTipoEvento(InfEvento.tpEvento);

        // nota fiscal eletronica
        FieldByName('Modelo').AsString      := Copy(InfEvento.chNFe, 21, 2);
        FieldByName('Serie').AsString       := Copy(InfEvento.chNFe, 23, 3);
        FieldByName('Numero').AsString      := Copy(InfEvento.chNFe, 26, 9);
        FieldByName('MesAno').AsString      := Copy(InfEvento.chNFe, 05, 2) + '/' + copy(InfEvento.chNFe, 03, 2);
        FieldByName('Barras').AsString      := InfEvento.chNFe;
        FieldByName('ChaveAcesso').AsString := FormatarChaveAcesso(InfEvento.chNFe);

        // Carta de correção eletrônica
        FieldByName('cOrgao').AsInteger := InfEvento.cOrgao;

        case InfEvento.tpAmb of
          taProducao:    FieldByName('tpAmb').AsString := ACBrStr('PRODUÇÃO');
          taHomologacao: FieldByName('tpAmb').AsString := ACBrStr('HOMOLOGAÇÃO - SEM VALOR FISCAL');
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

        if InfEvento.tpEvento <> teCCe then
        begin
          FieldByName('xJust').AsString := InfEvento.detEvento.xJust;
        end
        else
        begin
          CondicoesUso := InfEvento.detEvento.xCondUso;
          CondicoesUso := StringReplace(CondicoesUso, 'com: I', 'com:'+#13+' I', [rfReplaceAll]);
          CondicoesUso := StringReplace(CondicoesUso, ';', ';' + #13, [rfReplaceAll]);

          Correcao := InfEvento.detEvento.xCorrecao;
          Correcao := StringReplace(InfEvento.detEvento.xCorrecao, ';', #13, [rfReplaceAll]);

          FieldByName('xCondUso').AsString  := CondicoesUso;
          FieldByName('xCorrecao').AsString := Correcao;
        end;
      end;
      Post;
    end;
  end;
end;

procedure TACBrNFeFRClass.CarregaDadosInutilizacao;
begin
   CarregaParametros;

   with cdsInutilizacao do
   begin
      Close;
      FieldDefs.Clear;
      FieldDefs.Add('ID', ftString, 44);
      FieldDefs.Add('CNPJ', ftString, 20);
      FieldDefs.Add('nProt', ftString, 20);
      FieldDefs.Add('Modelo', ftInteger);
      FieldDefs.Add('Serie', ftInteger);
      FieldDefs.Add('Ano', ftInteger);
      FieldDefs.Add('nNFIni', ftInteger);
      FieldDefs.Add('nNFFin', ftInteger);
      FieldDefs.Add('xJust', ftString, 50);
      FieldDefs.Add('versao', ftString, 20);
      FieldDefs.Add('TpAmb', ftString, 32);
      FieldDefs.Add('verAplic', ftString, 20);
      FieldDefs.Add('cStat', ftInteger);
      FieldDefs.Add('xMotivo', ftString, 50);
      FieldDefs.Add('cUF', ftString, 2);
      FieldDefs.Add('dhRecbto', ftDateTime);
      CreateDataSet;

      Append;

      with FInutilizacao do
      begin
         FieldByName('ID').AsString         := OnlyNumber(ID);
         FieldByName('CNPJ').AsString       := FormatarCNPJ(CNPJ);
         FieldByName('nProt').AsString      := nProt;
         FieldByName('Modelo').AsInteger    := Modelo;
         FieldByName('Serie').AsInteger     := Serie;
         FieldByName('Ano').AsInteger       := Ano;
         FieldByName('nNFIni').AsInteger    := nNFIni;
         FieldByName('nNFFin').AsInteger    := nNFFin;
         FieldByName('xJust').AsString      := xJust;
         FieldByName('versao').AsString     := versao;
         FieldByName('verAplic').AsString   := verAplic;
         FieldByName('cStat').AsInteger     := cStat;
         FieldByName('xMotivo').AsString    := xMotivo;
         FieldByName('dhRecbto').AsDateTime := dhRecbto;
         FieldByName('cUF').AsString        := CUFtoUF(cUF);

         case tpAmb of
            taProducao:    FieldByName('tpAmb').AsString := ACBrStr('PRODUÇÃO');
            taHomologacao: FieldByName('tpAmb').AsString := ACBrStr('HOMOLOGAÇÃO - SEM VALOR FISCAL');
         end;

         Post;
      end;
   end;
end;

constructor TACBrNFeFRClass.Create(AOwner: TComponent);
begin
  FDANFEClassOwner := TACBrNFeDANFEClass(AOwner);

  FfrxReport := TfrxReport.Create( nil);
  FfrxReport.EngineOptions.UseGlobalDataSetList := False;
  FfrxReport.PreviewOptions.Buttons := [pbPrint, pbLoad, pbSave, pbExport, pbZoom, pbFind,
    pbOutline, pbPageSetup, pbTools, pbNavigator, pbExportQuick];	
  with FfrxReport do
  begin
     EngineOptions.DoublePass := True;
     StoreInDFM := False;
     OnBeforePrint := frxReportBeforePrint;
     OnReportPrint := 'frxReportOnReportPrint';
  end;
  FfrxPDFExport := TfrxPDFExport.Create(nil);
  with FfrxPDFExport do
  begin
     Background    := IncorporarBackgroundPdf;
     EmbeddedFonts := IncorporarFontesPdf;
     Subject       := 'Exportando DANFE para PDF';
     ShowProgress  := False;
  end;

  // cdsIdentificacao
  if not Assigned(cdsIdentificacao) then
  begin
     cdsIdentificacao := TClientDataSet.Create(nil);
     FfrxIdentificacao := TfrxDBDataset.Create(nil);
     with FfrxIdentificacao do
     begin
        DataSet := cdsIdentificacao;
        OpenDataSource := False;
        Enabled := False; 
        UserName := 'Identificacao';
     end;
     with cdsIdentificacao do
     begin
        FieldDefs.Add('Id', ftString, 44);
        FieldDefs.Add('Chave', ftString, 60);
        FieldDefs.Add('cUF', ftString, 2);
        FieldDefs.Add('cNF', ftString, 9);
        FieldDefs.Add('NatOp', ftString, 60);
        FieldDefs.Add('IndPag', ftString, 1);
        FieldDefs.Add('Mod_', ftString, 2);
        FieldDefs.Add('Serie', ftString, 3);
        FieldDefs.Add('NNF', ftString, 11);
        FieldDefs.Add('DEmi', ftString, 19);
        FieldDefs.Add('DSaiEnt', ftString, 10);
        FieldDefs.Add('TpNF', ftString, 1);
        FieldDefs.Add('CMunFG', ftString, 7);
        FieldDefs.Add('TpImp', ftString, 1);
        FieldDefs.Add('TpEmis', ftString, 1);
        FieldDefs.Add('CDV', ftString, 1);
        FieldDefs.Add('TpAmb', ftString, 1);
        FieldDefs.Add('FinNFe', ftString, 1);
        FieldDefs.Add('ProcEmi', ftString, 1);
        FieldDefs.Add('VerProc', ftString, 6);
        FieldDefs.Add('HoraSaida', ftString, 10);
        FieldDefs.Add('MensagemFiscal', ftString, 200);
        FieldDefs.Add('URL', ftString, 1000);
        CreateDataSet;
     end;
   end;

   // cdsEmitente
   if not Assigned(cdsEmitente) then
   begin
     cdsEmitente := TClientDataSet.Create(nil);
     FfrxEmitente := TfrxDBDataset.Create(nil);
     with FfrxEmitente do
     begin
        DataSet := cdsEmitente;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'Emitente';
     end;
     with cdsEmitente do
     begin
        FieldDefs.Add('CNPJ', ftString, 18);
        FieldDefs.Add('XNome', ftString, 60);
        FieldDefs.Add('XFant', ftString, 60);
        FieldDefs.Add('XLgr', ftString, 60);
        FieldDefs.Add('Nro', ftString, 60);
        FieldDefs.Add('XCpl', ftString, 60);
        FieldDefs.Add('XBairro', ftString, 60);
        FieldDefs.Add('CMun', ftString, 7);
        FieldDefs.Add('XMun', ftString, 60);
        FieldDefs.Add('UF', ftString, 2);
        FieldDefs.Add('CEP', ftString, 9);
        FieldDefs.Add('CPais', ftString, 4);
        FieldDefs.Add('XPais', ftString, 60);
        FieldDefs.Add('Fone', ftString, 15);
        FieldDefs.Add('IE', ftString, 15);
        FieldDefs.Add('IM', ftString, 15);
        FieldDefs.Add('IEST', ftString, 15);
        FieldDefs.Add('CRT', ftString, 1);
        FieldDefs.Add('DESCR_CST', ftString, 30);
        FieldDefs.Add('DADOS_ENDERECO', ftString, 1000);
        CreateDataSet;
     end;
   end;

   // cdsDestinatario
   if not Assigned(cdsDestinatario) then
   begin
     cdsDestinatario := TClientDataSet.Create(nil);
     FfrxDestinatario := TfrxDBDataset.Create(nil);
     with FfrxDestinatario do
     begin
        DataSet := cdsDestinatario;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'Destinatario';
     end;
     with cdsDestinatario do
     begin
        FieldDefs.Add('CNPJCPF', ftString, 18);
        FieldDefs.Add('XNome', ftString, 60);
        FieldDefs.Add('XLgr', ftString, 60);
        FieldDefs.Add('Nro', ftString, 60);
        FieldDefs.Add('XCpl', ftString, 60);
        FieldDefs.Add('XBairro', ftString, 60);
        FieldDefs.Add('CMun', ftString, 7);
        FieldDefs.Add('XMun', ftString, 60);
        FieldDefs.Add('UF', ftString, 2);
        FieldDefs.Add('CEP', ftString, 9);
        FieldDefs.Add('CPais', ftString, 4);
        FieldDefs.Add('XPais', ftString, 60);
        FieldDefs.Add('Fone', ftString, 15);
        FieldDefs.Add('IE', ftString, 18);
        FieldDefs.Add('Consumidor', ftString, 150);
        CreateDataSet;
     end;
   end;

   // cdsDadosProdutos
   if not Assigned(cdsDadosProdutos) then
   begin
     cdsDadosProdutos   := TClientDataSet.Create(nil);
     FfrxDadosProdutos  := TfrxDBDataset.Create(nil);
     with FfrxDadosProdutos do
     begin
        DataSet := cdsDadosProdutos;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'DadosProdutos';
     end;
     with cdsDadosProdutos do
     begin
        FieldDefs.Add('CProd'     , ftString, 60);
        FieldDefs.Add('cEAN'      , ftString, 60);
        FieldDefs.Add('XProd'     , ftString, 120);
        FieldDefs.Add('infAdProd' , ftString, 1000);
        FieldDefs.Add('NCM'       , ftString, 9);
        FieldDefs.Add('EXTIPI'    , ftString, 8);
        FieldDefs.Add('genero'    , ftString, 8);
        FieldDefs.Add('CFOP'      , ftString, 4);
        FieldDefs.Add('UCom'      , ftString, 6);
        FieldDefs.Add('QCom'      , ftFloat);
        FieldDefs.Add('VUnCom'    , ftFloat);
        FieldDefs.Add('VProd'     , ftString, 18);
        FieldDefs.Add('cEANTrib'  , ftString, 60);
        FieldDefs.Add('UTrib'     , ftString, 6);
        FieldDefs.Add('QTrib'     , ftFloat);
        FieldDefs.Add('vUnTrib'   , ftFloat);
        FieldDefs.Add('vFrete'    , ftString, 18);
        FieldDefs.Add('vOutro'    , ftString, 18);
        FieldDefs.Add('vSeg'      , ftString, 18);
        FieldDefs.Add('vDesc'     , ftString, 18);
        FieldDefs.Add('ORIGEM'    , ftString, 1);
        FieldDefs.Add('CST'       , ftString, 3);
        FieldDefs.Add('vBC'       , ftString, 18);
        FieldDefs.Add('pICMS'     , ftString, 18);
        FieldDefs.Add('vICMS'     , ftString, 18);
        FieldDefs.Add('vIPI'      , ftString, 18);
        FieldDefs.Add('pIPI'      , ftString, 18);
        FieldDefs.Add('VTotTrib'  , ftString, 18);
        FieldDefs.Add('ChaveNFe'  , ftString, 50);
        FieldDefs.Add('vISSQN'    , ftString, 18);
        FieldDefs.Add('vBcISSQN'  , ftString, 18);
        FieldDefs.Add('vBcST'     , ftString, 18);
        FieldDefs.Add('vICMSST'   , ftString, 18);
        FieldDefs.Add('nLote'     , ftString, 20);
        FieldDefs.Add('qLote'     , ftFloat);
        FieldDefs.Add('dFab'      , ftDateTime);
        FieldDefs.Add('dVal'      , ftDateTime);
        FieldDefs.Add('DescricaoProduto', ftString, 2000);
        FieldDefs.Add('Unidade'   , ftString, 14);
        FieldDefs.Add('Quantidade', ftString, 18);
        FieldDefs.Add('ValorUnitario'   , ftString, 18);
        FieldDefs.Add('Valorliquido'    , ftString, 18);
        FieldDefs.Add('ValorAcrescimos' , ftString, 18);
        CreateDataSet;
     end;
   end;

   // cdsParametros
   if not Assigned(cdsParametros) then
   begin
     cdsParametros  := TClientDataSet.Create(nil);
     FfrxParametros := TfrxDBDataset.Create(nil);
     with FfrxParametros do
     begin
        DataSet         := cdsParametros;
        OpenDataSource  := False;
        Enabled := False;
        UserName        := 'Parametros';
     end;
     with cdsParametros do
     begin
        FieldDefs.Add('poscanhoto', ftString, 1);
        FieldDefs.Add('ResumoCanhoto', ftString, 200);
        FieldDefs.Add('Mensagem0', ftString, 60);
        FieldDefs.Add('Imagem', ftString, 256);
        FieldDefs.Add('Sistema', ftString, 150);
        FieldDefs.Add('Usuario', ftString, 60);
        FieldDefs.Add('Fax', ftString, 60);
        FieldDefs.Add('Site', ftString, 60);
        FieldDefs.Add('Email', ftString, 60);
        FieldDefs.Add('Desconto', ftString, 60);
        FieldDefs.Add('TotalLiquido', ftString, 60);
        FieldDefs.Add('ChaveAcesso_Descricao', ftString, 90);
        FieldDefs.Add('Contingencia_ID', ftString, 36);
        FieldDefs.Add('Contingencia_Descricao', ftString, 60);
        FieldDefs.Add('Contingencia_Valor', ftString, 60);
        FieldDefs.Add('LinhasPorPagina', ftInteger);
        FieldDefs.Add('LogoExpandido', ftString, 1);
        FieldDefs.Add('DESCR_CST', ftString, 30);
        FieldDefs.Add('ConsultaAutenticidade', ftString, 300);
        FieldDefs.Add('sDisplayFormat', ftString, 25);
        FieldDefs.Add('iFormato', ftInteger);
        FieldDefs.Add('Casas_qCom', ftInteger);
        FieldDefs.Add('Casas_vUnCom', ftInteger);
        FieldDefs.Add('Mask_qCom', ftString, 30);
        FieldDefs.Add('Mask_vUnCom', ftString, 30);
        FieldDefs.Add('LogoCarregado', ftBlob);
        FieldDefs.Add('QrCodeCarregado', ftGraphic, 1000);
        FieldDefs.Add('DescricaoViaEstabelec', ftString, 30);
        FieldDefs.Add('QtdeItens', ftInteger);
        FieldDefs.Add('ExpandirDadosAdicionaisAuto', ftString, 1);
        FieldDefs.Add('ImprimeDescAcrescItem', ftInteger);
        FieldDefs.Add('nProt', ftString, 30);
        FieldDefs.Add('dhRecbto', ftDateTime);
        CreateDataSet;
     end;
   end;

   // cdsDuplicatas
   if not Assigned(cdsDuplicatas) then
   begin
     cdsDuplicatas := TClientDataSet.Create(nil);
     FfrxDuplicatas := TfrxDBDataset.Create(nil);
     with FfrxDuplicatas do
     begin
        DataSet := cdsDuplicatas;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'Duplicatas';
     end;
     with cdsDuplicatas do
     begin
        FieldDefs.Add('NDup', ftString, 60);
        FieldDefs.Add('DVenc', ftString, 10);
        FieldDefs.Add('VDup', ftFloat);
        FieldDefs.Add('ChaveNFe', ftString, 50);
        CreateDataSet;
     end;
   end;

   // cdsCalculoImposto
   if not Assigned(cdsCalculoImposto) then
   begin
     cdsCalculoImposto := TClientDataSet.Create(nil);
     FfrxCalculoImposto := TfrxDBDataset.Create(nil);
     with FfrxCalculoImposto do
     begin
        DataSet := cdsCalculoImposto;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'CalculoImposto';
     end;
     with cdsCalculoImposto do
     begin
        FieldDefs.Add('VBC'         , ftFloat);
        FieldDefs.Add('VICMS'       , ftFloat);
        FieldDefs.Add('VBCST'       , ftFloat);
        FieldDefs.Add('VST'         , ftFloat);
        FieldDefs.Add('VProd'       , ftFloat);
        FieldDefs.Add('VFrete'      , ftFloat);
        FieldDefs.Add('VSeg'        , ftFloat);
        FieldDefs.Add('VDesc'       , ftFloat);
        FieldDefs.Add('VII'         , ftFloat);
        FieldDefs.Add('VIPI'        , ftFloat);
        FieldDefs.Add('VPIS'        , ftFloat);
        FieldDefs.Add('VCOFINS'     , ftFloat);
        FieldDefs.Add('VOutro'      , ftFloat);
        FieldDefs.Add('VNF'         , ftFloat);
        FieldDefs.Add('VTotTrib'    , ftFloat);
        FieldDefs.Add('VTribPerc'   , ftFloat);
        FieldDefs.Add('VTribFonte'  , ftString, 100);
        FieldDefs.Add('vTotPago'    , ftFloat);
        FieldDefs.Add('vTroco'      , ftFloat);
        FieldDefs.Add('ValorApagar' , ftFloat);
        CreateDataSet;
     end;
   end;

   // cdsTransportador
   if not Assigned(cdsTransportador) then
   begin
     cdsTransportador := TClientDataSet.Create(nil);
     FfrxTransportador := TfrxDBDataset.Create(nil);
     with FfrxTransportador do
     begin
        DataSet := cdsTransportador;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'Transportador';
     end;
     with cdsTransportador do
     begin
        FieldDefs.Add('ModFrete', ftString, 14);
        FieldDefs.Add('CNPJCPF', ftString, 18);
        FieldDefs.Add('XNome', ftString, 60);
        FieldDefs.Add('IE', ftString, 15);
        FieldDefs.Add('XEnder', ftString, 60);
        FieldDefs.Add('XMun', ftString, 60);
        FieldDefs.Add('UF', ftString, 2);
        CreateDataSet;
     end;
   end;

   // cdsVeiculo
   if not Assigned(cdsVeiculo) then
   begin
     cdsVeiculo := TClientDataSet.Create(nil);
     FfrxVeiculo := TfrxDBDataset.Create(nil);
     with FfrxVeiculo do
     begin
        DataSet := cdsVeiculo;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'Veiculo';
     end;
     with cdsVeiculo do
     begin
        FieldDefs.Add('PLACA', ftString, 8);
        FieldDefs.Add('UF', ftString, 2);
        FieldDefs.Add('RNTC', ftString, 20);
        CreateDataSet;
     end;
   end;

   // cdsVolumes
   if not Assigned(cdsVolumes) then
   begin
     cdsVolumes := TClientDataSet.Create(nil);
     FfrxVolumes := TfrxDBDataset.Create(nil);
     with FfrxVolumes do
     begin
        DataSet := cdsVolumes;
        OpenDataSource := False;
        Enabled := False;
        UserName := 'Volumes';
     end;
     with cdsVolumes do
     begin
        FieldDefs.Add('QVol', ftFloat);
        FieldDefs.Add('Esp', ftString, 60);
        FieldDefs.Add('Marca', ftString, 60);
        FieldDefs.Add('NVol', ftString, 60);
        FieldDefs.Add('PesoL', ftFloat);
        FieldDefs.Add('PesoB', ftFloat);
        CreateDataSet;
     end;
   end;

   // csdEvento
   if not Assigned(cdsEventos) then
   begin
      cdsEventos := TClientDataSet.Create(nil);
      FfrxEventos := TfrxDBDataset.Create(nil);
      with FfrxEventos do
      begin
         DataSet := cdsEventos;
         OpenDataSource := False;
         Enabled := False;
         UserName := 'Eventos';
      end;
   end;

   // cdsISSQN
   if not Assigned(cdsISSQN) then
   begin
      cdsISSQN := TClientDataSet.Create(nil);
      FfrxISSQN := TfrxDBDataset.Create(nil);
      with FfrxISSQN do
      begin
         DataSet := cdsISSQN;
         OpenDataSource := False;
         Enabled := False;
         UserName := 'ISSQN';
      end;
      with cdsISSQN do
      begin
         FieldDefs.Add('vSERV', ftFloat);
         FieldDefs.Add('vBC', ftFloat);
         FieldDefs.Add('vISS', ftFloat);
         FieldDefs.Add('vDescIncond', ftFloat);
         CreateDataSet;
      end;
   end;

   // cdsFatura
   if not Assigned(cdsFatura) then
   begin
      cdsFatura   := TClientDataSet.Create(nil);
      FfrxFatura  := TfrxDBDataset.Create(nil);
      with FfrxFatura do
      begin
         DataSet        := cdsFatura;
         OpenDataSource := False;
         Enabled := False;
         UserName       := 'Fatura';
      end;
      with cdsFatura do
      begin
         FieldDefs.Add('iForma'   , ftInteger);
         FieldDefs.Add('Pagamento', ftString, 20);
         FieldDefs.Add('nFat'     , ftString, 60);
         FieldDefs.Add('vOrig'    , ftFloat);
         FieldDefs.Add('vDesc'    , ftFloat);
         FieldDefs.Add('vLiq'     , ftFloat);
         CreateDataSet;
      end;
   end;

   // cdsLocalRetirada
   if not Assigned(cdsLocalRetirada) then
   begin
      cdsLocalRetirada := TClientDataSet.Create(nil);
      FfrxLocalRetirada := TfrxDBDataset.Create(nil);
      with FfrxLocalRetirada do
      begin
         DataSet := cdsLocalRetirada;
         OpenDataSource := False;
         Enabled := False;
         UserName := 'LocalRetirada';
      end;
      with cdsLocalRetirada do
      begin
         FieldDefs.Add('CNPJ', ftString, 18);
         FieldDefs.Add('XLgr', ftString, 60);
         FieldDefs.Add('Nro', ftString, 60);
         FieldDefs.Add('XCpl', ftString, 60);
         FieldDefs.Add('XBairro', ftString, 60);
         FieldDefs.Add('CMun', ftString, 7);
         FieldDefs.Add('XMun', ftString, 60);
         FieldDefs.Add('UF', ftString, 2);
         CreateDataSet;
      end;
   end;

   // cdsLocalEntrega
   if not Assigned(cdsLocalEntrega) then
   begin
      cdsLocalEntrega := TClientDataSet.Create(nil);
      FfrxLocalEntrega := TfrxDBDataset.Create(nil);
      with FfrxLocalEntrega do
      begin
         DataSet := cdsLocalEntrega;
         OpenDataSource := False;
         Enabled := False;
         UserName := 'LocalEntrega';
      end;
      with cdsLocalEntrega do
      begin
         FieldDefs.Add('CNPJ', ftString, 18);
         FieldDefs.Add('XLgr', ftString, 60);
         FieldDefs.Add('Nro', ftString, 60);
         FieldDefs.Add('XCpl', ftString, 60);
         FieldDefs.Add('XBairro', ftString, 60);
         FieldDefs.Add('CMun', ftString, 7);
         FieldDefs.Add('XMun', ftString, 60);
         FieldDefs.Add('UF', ftString, 2);
         CreateDataSet;
      end;
   end;

   // cdsInformacoesAdicionais
   if not Assigned(cdsInformacoesAdicionais) then
   begin
      cdsInformacoesAdicionais := TClientDataSet.Create(nil);
      FfrxInformacoesAdicionais := TfrxDBDataset.Create(nil);
      with FfrxInformacoesAdicionais do
      begin
         DataSet := cdsInformacoesAdicionais;
         OpenDataSource := False;
         Enabled := False;
         UserName := 'InformacoesAdicionais';
      end;
      with cdsInformacoesAdicionais do
      begin
         FieldDefs.Add('OBS', ftString, 6900);
         FieldDefs.Add('LinhasOBS', ftInteger);
         CreateDataSet;
      end;
   end;

   // cdsPagamento
   if not Assigned(cdsPagamento) then
   begin
      cdsPagamento := TClientDataSet.Create(nil);
      FfrxPagamento := TfrxDBDataset.Create(nil);
      with FfrxPagamento do
      begin
         DataSet := cdsPagamento;
         OpenDataSource := False;
         Enabled := False;
         UserName := 'Pagamento';
      end;
      with cdsPagamento do
      begin
         FieldDefs.Add('tPag', ftString, 50);
         FieldDefs.Add('vPag', ftFloat);
         FieldDefs.Add('vTroco', ftFloat);
         FieldDefs.Add('CNPJ', ftString, 50);
         FieldDefs.Add('tBand', ftString, 50);
         FieldDefs.Add('cAut', ftString, 20);
         CreateDataSet;
      end;
   end;

   //cdsInutilização
   if not Assigned(cdsInutilizacao) then
   begin
      cdsInutilizacao := TClientDataSet.Create(nil);
      FfrxInutilizacao := TfrxDBDataset.Create(nil);
      with FfrxInutilizacao do
      begin
         DataSet := cdsInutilizacao;
         OpenDataSource := False;
         Enabled := False;
         UserName := 'Inutilizacao';
      end;
   end;
end;

destructor TACBrNFeFRClass.Destroy;
begin
    FfrxReport.Free;
    FfrxPDFExport.Free;
    FfrxBarCodeObject.Free;
    cdsIdentificacao.Free;
    FfrxIdentificacao.Free;
    cdsEmitente.Free;
    FfrxEmitente.Free;
    cdsDestinatario.Free;
    FfrxDestinatario.Free;
    cdsDadosProdutos.Free;
    FfrxDadosProdutos.Free;
    cdsParametros.Free;
    FfrxParametros.Free;
    cdsDuplicatas.Free;
    FfrxDuplicatas.Free;
    cdsCalculoImposto.Free;
    FfrxCalculoImposto.Free;
    cdsTransportador.Free;
    FfrxTransportador.Free;
    cdsVeiculo.Free;
    FfrxVeiculo.Free;
    cdsVolumes.Free;
    FfrxVolumes.Free;
    cdsEventos.Free;
    FfrxEventos.Free;
    cdsISSQN.Free;
    FfrxISSQN.Free;
    cdsFatura.Free;
    FfrxFatura.Free;
    cdsLocalRetirada.Free;
    FfrxLocalRetirada.Free;
    cdsLocalEntrega.Free;
    FfrxLocalEntrega.Free;
    cdsInformacoesAdicionais.Free;
    FfrxInformacoesAdicionais.Free;
    cdsPagamento.Free;
    FfrxPagamento.Free;
    cdsInutilizacao.Free;
    FfrxInutilizacao.Free;

  inherited;
//  FDANFEClassOwner := TACBrNFeDANFEClass(AOwner);
end;

procedure TACBrNFeFRClass.PintarQRCode(QRCodeData: String; APict: TPicture);
var
  QRCode: TDelphiZXingQRCode;
  QRCodeBitmap: TBitmap;
  Row, Column: Integer;
begin
  QRCode       := TDelphiZXingQRCode.Create;
  QRCodeBitmap := TBitmap.Create;
  try
    QRCode.Data      := QRCodeData;
    QRCode.Encoding  := qrUTF8NoBOM;
    QRCode.QuietZone := 1;

    //QRCodeBitmap.SetSize(QRCode.Rows, QRCode.Columns);
    QRCodeBitmap.Width  := QRCode.Columns;
    QRCodeBitmap.Height := QRCode.Rows;

    for Row := 0 to QRCode.Rows - 1 do
    begin
      for Column := 0 to QRCode.Columns - 1 do
      begin
        if (QRCode.IsBlack[Row, Column]) then
          QRCodeBitmap.Canvas.Pixels[Column, Row] := clBlack
        else
          QRCodeBitmap.Canvas.Pixels[Column, Row] := clWhite;
      end;
    end;

    APict.Assign(QRCodeBitmap);
  finally
    QRCode.Free;
    QRCodeBitmap.Free;
  end;
end;

procedure TACBrNFeFRClass.frxReportBeforePrint(Sender: TfrxReportComponent);
var
  qrcode: String;
  CpTituloReport, CpLogomarca, CpDescrProtocolo, CpTotTrib, CpContingencia1, CpContingencia2 : TfrxComponent;
begin

  qrCode := '';
  if Assigned(NFe) then
  begin
    case NFe.Ide.modelo of
      55 :  case FNFe.Ide.tpImp of
              tiSimplificado :
                begin
                  CpTituloReport := frxReport.FindObject('PageHeader1');
                  if Assigned(CpTituloReport) then
                    CpTituloReport.Visible  := ( cdsParametros.FieldByName('Imagem').AsString <> '' );

                  CpLogomarca := frxReport.FindObject('ImgLogo');
                  if Assigned(CpLogomarca) and Assigned(CpTituloReport) then
                    CpLogomarca.Visible := CpTituloReport.Visible;
                end;
            end;

      65 :  begin
              CpTituloReport := frxReport.FindObject('ReportTitle1');
              if Assigned(CpTituloReport) then
                CpTituloReport.Visible := cdsParametros.FieldByName('Imagem').AsString <> '';

              CpLogomarca := frxReport.FindObject('ImgLogo');
              if Assigned(CpLogomarca) and Assigned(CpTituloReport) then
                CpLogomarca.Visible := CpTituloReport.Visible;

              if EstaVazio(Trim(NFe.infNFeSupl.qrCode)) then
                qrcode := TACBrNFe(DANFEClassOwner.ACBrNFe).GetURLQRCode(
                       NFe.ide.cUF,
                       NFe.ide.tpAmb,
                       OnlyNumber(NFe.InfNFe.ID),
                       IfThen(NFe.Dest.idEstrangeiro <> '',NFe.Dest.idEstrangeiro, NFe.Dest.CNPJCPF),
                       NFe.ide.dEmi,
                       NFe.Total.ICMSTot.vNF,
                       NFe.Total.ICMSTot.vICMS,
                       NFe.signature.DigestValue,
                       NFe.infNFe.Versao)
              else
                qrcode := NFe.infNFeSupl.qrCode;

              if Assigned(Sender) and (Sender.Name = 'ImgQrCode') then
                PintarQRCode(qrcode, TfrxPictureView(Sender).Picture);

              CpDescrProtocolo := frxReport.FindObject('Memo25');
              if Assigned(CpDescrProtocolo) then
                CpDescrProtocolo.Visible := cdsParametros.FieldByName('Contingencia_Valor').AsString <> '';

              CpTotTrib := frxReport.FindObject('ValorTributos');
              if Assigned(CpTotTrib) then
                CpTotTrib.Visible := cdsCalculoImposto.FieldByName('VTotTrib').AsFloat > 0;

              // ajusta Informação de contingência no NFCe
              CpContingencia1 := frxReport.FindObject('ChildContingenciaCabecalho');
              if Assigned(CpContingencia1) then
                CpContingencia1.Visible := FNFe.Ide.tpEmis <> teNormal;

              CpContingencia2 := frxReport.FindObject('ChildContingenciaIdentificacao');
              if Assigned(CpContingencia2) then
                CpContingencia2.Visible := FNFe.Ide.tpEmis <> teNormal;
            end;
    end;
  end;
end;

Function TACBrNFeFRClass.ManterVprod( dVProd , dvDesc : Double ) : String;
Var
  dValor : Double;
begin
  if FDANFEClassOwner.ImprimirTotalLiquido then
    dValor := dVProd - dvDesc
  else
    dValor := dVProd;

  Result := FormatFloatBr( dValor,'###,###,##0.00');
end;

Function TACBrNFeFRClass.ManterdvTotTrib( dvTotTrib  : Double ) : String;
Var
  dValor : Double;
begin
  if ExibirTotalTributosItem then
    dValor := dvTotTrib
  else
    dValor := 0;
  Result := FormatFloatBr( dValor,'###,###,##0.00');
end;

Function TACBrNFeFRClass.ManterVDesc( dvDesc: Currency; dVUnCom , dQCom : double ) : Double;
begin
  if ( FDANFEClassOwner.ImprimirDescPorc ) then
  begin
    if ( ( dvDesc  > 0 ) and ( dVUnCom > 0 ) and ( dQCom   > 0 ) ) then
      Result := (( dvDesc*100 ) / (dVUnCom * dQCom) )
    else
      Result := 0;
  end
  else
    Result := dvDesc;
end;

Function TACBrNFeFRClass.ManterCst( dCRT: TpcnCRT;  dCSOSN: TpcnCSOSNIcms; dCST: TpcnCSTIcms ) : String;
begin
  if dCRT = crtSimplesNacional then
    Result := CSOSNIcmsToStr(dCSOSN)
  else
    Result := CSTICMSToStr(dCST);
end;

Function TACBrNFeFRClass.ManterArma( inItem:  integer  ) : String;
Var
  i : Integer;
begin
  Result := '';
  with FNFe.Det.Items[inItem].Prod do
  begin
    if (FImprimirDadosArma) and ( arma.Count > 0) then
    begin
      Result := sQuebraLinha;
      for i := 0 to arma.Count - 1 do
      begin
        Result := Result + ACBrStr('TIPO DE ARMA: ')   + ArmaTipoStr( arma.Items[i].tpArma ) + sQuebraLinha;
        Result := Result + ACBrStr('No. SÉRIE ARMA: ') + arma.Items[i].nSerie + sQuebraLinha;
        Result := Result + ACBrStr('No. SÉRIE CANO: ') + arma.Items[i].nCano + sQuebraLinha;
        Result := Result + ACBrStr('DESCRIÇÃO ARMA: ') + arma.Items[i].descr + ';';
       end;
    end;
  end;
end;

Function TACBrNFeFRClass.ManterMedicamentos( inItem:  integer  ) : String;
Var
  i : Integer;
begin
  Result := '';
  { detalhamento específico de medicamentos }
  with FNFe.Det.Items[inItem].Prod do
  begin
    if med.Count > 0 then
    begin
      Result := sQuebraLinha;
      for i := 0 to med.Count - 1 do
      begin
        if NFe.infNFe.Versao >= 4 then
          Result := Result + 'C.P. ANVISA '+ med.Items[i].cProdANVISA+ sQuebraLinha
        else
        begin
          Result := Result + 'LOTE: ' + med.Items[i].nLote+ sQuebraLinha;
          Result := Result + 'QTD: '  + FormatFloatBr(med.Items[i].qLote)+ sQuebraLinha;
          Result := Result + 'FAB: '  + FormatDateBr(med.Items[i].dFab)+ sQuebraLinha;
          Result := Result + 'VAL: '  + FormatDateBr(med.Items[i].dVal)+ sQuebraLinha;
        end;
        Result := Result + IfThen( med.Items[i].vPMC  > 0, 'PMC: ' + FormatFloatBr(med.Items[i].vPMC) + ';' , '');
      end;
    end;
  end;
end;

Function TACBrNFeFRClass.ManterRastro( inItem:  integer  ) : String;
Var
  i : Integer;
begin
  Result := '';
  { rastreabilidade do produto}
  with FNFe.Det.Items[inItem].Prod do
  begin
    if Rastro.Count > 0 then
    begin
      Result := sQuebraLinha;
      for i := 0 to Rastro.Count - 1 do
      begin
        Result := Result + 'LOTE: ' + rastro.Items[i].nLote+ sQuebraLinha;
        Result := Result + 'QTD: '  + FormatFloatBr(rastro.Items[i].qLote)+ sQuebraLinha;
        Result := Result + 'FAB: '  + FormatDateBr(rastro.Items[i].dFab)+ sQuebraLinha;
        Result := Result + 'VAL: '  + FormatDateBr(rastro.Items[i].dVal)+ sQuebraLinha;
        Result := Result + ACBrStr('C.AGREGAÇÃO: ' ) + rastro.Items[i].cAgreg+ ';';
      end;
    end;
  end;
end;

Function TACBrNFeFRClass.ManterVeiculos( inItem:  integer  ) : String;
begin
  Result := '';
{ detalhamento especifico de veículos }
  with FNFe.Det.Items[inItem].Prod do
  begin
    if veicProd.chassi > '' then
    begin
      Result := sQuebraLinha;
	    Result := Result + ACBrStr('TIPO DE OPERAÇÃO: ' + VeiculosTipoOperStr( veicProd.tpOP ) ) + sQuebraLinha;
	    Result := Result + ACBrStr('CHASSI: ' )+ veicProd.chassi + sQuebraLinha;
	    Result := Result + ACBrStr('CÓDIGO DA COR: ' )+ veicProd.cCor + sQuebraLinha;
	    Result := Result + ACBrStr('NOME DA COR: ') + veicProd.xCor + sQuebraLinha;
	    Result := Result + ACBrStr('POTÊNCIA DO MOTOR: ') + veicProd.pot + sQuebraLinha;
	    Result := Result + ACBrStr('CILINDRADAS: ') + veicProd.Cilin + sQuebraLinha;
	    Result := Result + ACBrStr('PESO LÍQUIDO: ') + veicProd.pesoL + sQuebraLinha;
	    Result := Result + ACBrStr('PESO BRUTO: ' )+ veicProd.pesoB + sQuebraLinha;
	    Result := Result + ACBrStr('NÚMERO DE SÉRIE: ') + veicProd.nSerie + sQuebraLinha;
	    Result := Result + ACBrStr('COMBUSTÍVEL: ' + VeiculosCombustivelStr( veicProd.tpComb ) ) + sQuebraLinha;
	    Result := Result + ACBrStr('NÚMERO DO MOTOR: ') + veicProd.nMotor + sQuebraLinha;
	    Result := Result + ACBrStr('CAP. MÁX. TRAÇÃO: ') + veicProd.CMT + sQuebraLinha;
	    Result := Result + ACBrStr('DISTÂNCIA ENTRE EIXOS: ') + veicProd.dist + sQuebraLinha;
	    Result := Result + ACBrStr('ANO DO MODELO: ' )+ IntToStr(veicProd.anoMod) + sQuebraLinha;
	    Result := Result + ACBrStr('ANO DE FABRICAÇÃO: ') + IntToStr(veicProd.anoFab) + sQuebraLinha;
	    Result := Result + ACBrStr('TIPO DE PINTURA: ') + veicProd.tpPint + sQuebraLinha;
	    Result := Result + ACBrStr('TIPO DE VEÍCULO: ' + VeiculosTipoStr( veicProd.tpVeic ) )+ sQuebraLinha;
	    Result := Result + ACBrStr('ESPÉCIE DO VEÍCULO: ' +VeiculosEspecieStr( veicProd.espVeic )) + sQuebraLinha;
	    Result := Result + ACBrStr('VIN (CHASSI): ' + VeiculosVinStr( veicProd.VIN ) )+ sQuebraLinha;
	    Result := Result + ACBrStr('CONDIÇÃO DO VEÍCULO: ' +VeiculosCondicaoStr( veicProd.condVeic)) + sQuebraLinha;
	    Result := Result + ACBrStr('CÓDIGO MARCA MODELO: ') + veicProd.cMod + sQuebraLinha;
	    Result := Result + ACBrStr('CÓDIGO COR DENATRAN: ' +VeiculosCorDENATRANSTr( veicProd.cCorDENATRAN )) + sQuebraLinha;
	    Result := Result + ACBrStr('CAP.MÁXIMA DE LOTAÇÃO: ') +IntToStr(veicProd.lota) + sQuebraLinha;
	    Result := Result + ACBrStr('RESTRIÇÃO: ' +VeiculosRestricaoStr( veicProd.tpRest ) )+ ';';
    end;
  end;
end;


Function TACBrNFeFRClass.ManterValAprox( inItem : Integer ): String;
begin
  Result := '';
  with FNFe.Det.Items[inItem] do
  begin
    if (Imposto.vTotTrib <> 0)  and (ExibirTotalTributosItem) then
    begin
      Result := '';
      with Imposto do
      begin
        Result := Result+'Val Aprox Tributos: '+ FloatToStrF(Imposto.vTotTrib,ffCurrency,15,2);
        if TributosPercentual = ptValorNF then
          Result := Result+' ('+FloatToStrF(((StringToFloatDef(FloatToStr(Imposto.vTotTrib),0)*100)/(StringToFloatDef(FloatToStr(Prod.VProd),0) +
                                StringToFloatDef(FloatToStr(Prod.vFrete),0)  +
                                StringToFloatDef(FloatToStr(Prod.vOutro),0)  +
                                StringToFloatDef(FloatToStr(Prod.vSeg),0)    +
                                StringToFloatDef(FloatToStr(IPI.vIPI), 0)    +
                                StringToFloatDef(FloatToStr(ICMS.vICMSST), 0))),ffNumber,15,2)+'%)'
        else
          Result := Result+' ('+FloatToStrF(((StringToFloatDef(FloatToStr(Imposto.vTotTrib),0)*100)/(StringToFloatDef(FloatToStr(Prod.VProd),0))),ffNumber,15,2)+'%)';
      end;
    end;
  end;
end;

Function TACBrNFeFRClass.ManterCombustivel( inItem:  integer  ) : String;
begin
  Result := '';
  with FNFe.Det.Items[inItem].Prod do
  begin
    if comb.cProdANP > 0 then
    begin
      Result := sQuebraLinha;
      Result := Result + ACBrStr( 'CÓD. PRODUTO ANP: ') + IntToStr(comb.cProdANP) + sQuebraLinha;
      Result := Result + IfThen( comb.CODIF > '', ACBrStr( 'AUTORIZAÇÃO/CODIF: ') + comb.CODIF + sQuebraLinha , '');
      Result := Result + IfThen( comb.qTemp > 0 , ACBrStr( 'QTD. FATURADA TEMP. AMBIENTE: ' )+ FormatFloat('###,##0.0000', comb.qTemp) + sQuebraLinha , '');
      Result := Result + ACBrStr('UF DE CONSUMO: ') + comb.UFcons + sQuebraLinha;
      if comb.CIDE.qBCProd > 0 then
      begin
        Result := Result + ACBrStr('BASE DE CÁLCULO CIDE: ') + FormatFloat('###,##0.0000', comb.CIDE.qBCProd) + sQuebraLinha;
        Result := Result + ACBrStr('ALÍQUOTA CIDE: ') + FormatFloat('###,##0.0000', comb.CIDE.vAliqProd) + sQuebraLinha;
        Result := Result + ACBrStr('VALOR CIDE: ') + FormatFloat('###,##0.00', comb.CIDE.vCIDE);
      end;
      if comb.encerrante.nBico > 0  then
      begin
        Result := Result + 'ENCERRANTE' + sQuebraLinha;
        Result := Result + 'BICO: ' +  IntToStr( comb.encerrante.nBico ) + sQuebraLinha;
        Result := Result + IfThen( comb.encerrante.nBomba > 0, 'BOMBA: ' + IntToStr(comb.encerrante.nBomba) + sQuebraLinha , '');
        Result := Result + 'TANQUE: ' + IntToStr(comb.encerrante.nTanque) + sQuebraLinha;
        Result := Result + ACBrStr('NO INÍCIO: ' ) + FormatFloat('###,##0.000', comb.encerrante.vEncIni) + sQuebraLinha;
        Result := Result + 'NO FINAL: ' + FormatFloat('###,##0.000', comb.encerrante.vEncFin) + sQuebraLinha;
      end;
    end;
  end;
end;

Function TACBrNFeFRClass.ManterInfAProd( inItem : Integer; sinfAdProd : String ) : String;
var
  Campos2     : TSplitResult;
  IndexCampo2 : Integer;
  vTemp2      : TStringList;
begin
  vTemp2  := TStringList.create;
  try
    Result  := sinfAdProd;
    Result  := Result + ManterValAprox( inItem );
    if FDANFEClassOwner.ImprimirDetalhamentoEspecifico then
    begin
      sQuebraLinha := QuebraLinha;
      Result := Result + ManterVeiculos( inItem  );
      Result := Result + ManterMedicamentos( inItem  );
      Result := Result + ManterRastro( inItem  );
      Result := Result + ManterArma( inItem  );
      Result := Result + ManterCombustivel( inItem );
    end;
    if Trim(Result) <> '' then
    begin
      Campos2 := Split(';', Result);

      for IndexCampo2 := 0 to Length(Campos2) - 1 do
        vTemp2.Add(Trim(Campos2[IndexCampo2]));

      Result  := #13 + Trim(vTemp2.Text);
    end;
  finally
    vTemp2.free;
  end;
end;

Function TACBrNFeFRClass.ManterDescricaoProduto( sXProd,sinfAdProd : String ) : String;
begin
  Result := trim( sXProd );
  if sinfAdProd <> '' then
    Result := Result +#13+ trim( sinfAdProd );
end;


Function TACBrNFeFRClass.ManterVTribPerc( dVTotTrib , dVProd ,dVNF : Double ) : Double;
begin
  Result := 0;
  case TributosPercentual of
    ptPersonalizado   : Result := TributosPercentualPersonalizado;
    ptValorProdutos   : if (dVProd > 0) then Result := dVTotTrib*100/dVProd;
    ptValorNF         : if (dVNF   > 0) then Result := dVTotTrib*100/dVNF;
  end;
end;

Function TACBrNFeFRClass.FormatQuantidade( dValor : Double ) : String;
begin
  With cdsParametros do
  begin
     case FieldByName('iFormato').AsInteger of
      0 : Result := FormatFloatBr( dValor , FloatMask( FieldByName('Casas_qCom').AsInteger));
      1 : Result := FormatFloatBr( dValor , FieldByName('Mask_qCom').AsString);
      else
        Result := FormatFloatBr( dValor , FloatMask( FieldByName('Casas_qCom').AsInteger));
    end;
  end;
end;


Function TACBrNFeFRClass.FormatValorUnitario( dValor : Double ) : String;
begin
  With cdsParametros do
  begin
    case FieldByName('iFormato').AsInteger of
      0 : Result := FormatFloatBr( dValor , FloatMask( FieldByName('Casas_vUnCom').AsInteger));
      1 : Result := FormatFloatBr( dValor , FieldByName('Mask_vUnCom').AsString);
      else
        Result := FormatFloatBr( dValor , FloatMask( FieldByName('Mask_vUnCom').AsInteger));
    end;
  end;
end;


Function TACBrNFeFRClass.QuebraLinha : String;
begin
  if fQuebraLinhaEmDetalhamentoEspecifico then
    Result := ';'
  else
    Result := ' - ';
end;


Function TACBrNFeFRClass.ManterInfAdi( swObs : String ) : String;
var
  i : Integer;
  TmpStr : String;
begin
  result := swObs;
  TmpStr := '';
  with FNFe.InfAdic do
  begin

    for i := 0 to ObsFisco.Count - 1 do
    begin
      with ObsFisco.Items[i] do
        TmpStr := TmpStr + XCampo + ': ' + XTexto + ';';
    end;

    //Fisco
    if Length(InfAdFisco) = 0 then InfAdFisco := '';

    result  := result + TmpStr + InfAdFisco;
    TmpStr  := '';
    for i := 0 to ObsCont.Count - 1 do
    begin
      with ObsCont.Items[i] do
        TmpStr := TmpStr + XCampo + ': ' + XTexto + ';';
    end;
    //Inf. Complementar
    if Length(InfCpl) = 0 then InfCpl := '';

    result  := result + TmpStr + InfCpl;
  end;
end;

Function TACBrNFeFRClass.ManterContingencia( swObs : String ) : String;
  //Contingencia
begin
  result := swObs;
  case FNFe.Ide.tpEmis of
    teNORMAL : result := result + '';
    teOffLine,
    teContingencia,
    teFSDA,
    teSCAN,
    teSVCAN,
    teSVCRS,
    teSVCSP : result := result + ACBrStr('DANFE EM CONTINGÊNCIA, IMPRESSO EM DECORRÊNCIA DE PROBLEMAS TÉCNICOS;');
    teDPEC  : begin
                result := result +
                  ACBrStr( 'DANFE IMPRESSO EM CONTINGÊNCIA - DPEC REGULARMENTE RECEBIDA PELA RECEITA FEDERAL DO BRASIL;')+
                  ACBrStr('DATA/HORA INÍCIO: ') + IfThen(FNFe.ide.dhCont = 0, ' ', DateTimeToStr(FNFe.ide.dhCont)) + ';'+
                  ACBrStr('MOTIVO CONTINGÊNCIA: ') + IfThen(EstaVazio(FNFe.ide.xJust), ' ', FNFe.ide.xJust)+';';
              end;
  end;
end;

end.
