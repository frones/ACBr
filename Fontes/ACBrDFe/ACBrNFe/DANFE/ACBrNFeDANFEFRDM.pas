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
******************************************************************************}
{$I ACBr.inc}

unit ACBrNFeDANFEFRDM;

interface

uses
  pcnEnvEventoNFe,
  SysUtils, Classes, ACBrNFeDANFEClass, pcnNFe, frxClass, frxExportPDF, DB,
  DBClient, frxDBSet, pcnConversao, ACBrUtil, frxBarcode, dialogs,
  ACBrDelphiZXingQrCode, Graphics;

type
  TdmACBrNFeFR = class(TDataModule)
    frxPDFExport: TfrxPDFExport;
    cdsIdentificacao: TClientDataSet;
    cdsIdentificacaoId: TStringField;
    cdsIdentificacaoChave: TStringField;
    cdsIdentificacaocUF: TStringField;
    cdsIdentificacaocNF: TStringField;
    cdsIdentificacaoNatOp: TStringField;
    cdsIdentificacaoIndPag: TStringField;
    cdsIdentificacaoMod_: TStringField;
    cdsIdentificacaoSerie: TStringField;
    cdsIdentificacaoNNF: TStringField;
    cdsIdentificacaoDEmi: TStringField;
    cdsIdentificacaoDSaiEnt: TStringField;
    cdsIdentificacaoTpNF: TStringField;
    cdsIdentificacaoCMunFG: TStringField;
    cdsIdentificacaoTpImp: TStringField;
    cdsIdentificacaoTpEmis: TStringField;
    cdsIdentificacaoCDV: TStringField;
    cdsIdentificacaoTpAmb: TStringField;
    cdsIdentificacaoFinNFe: TStringField;
    cdsIdentificacaoProcEmi: TStringField;
    cdsIdentificacaoVerProc: TStringField;
    cdsIdentificacaoHoraSaida: TStringField;
    cdsEmitente: TClientDataSet;
    cdsEmitenteCNPJ: TStringField;
    cdsEmitenteXNome: TStringField;
    cdsEmitenteXFant: TStringField;
    cdsEmitenteXLgr: TStringField;
    cdsEmitenteNro: TStringField;
    cdsEmitenteXCpl: TStringField;
    cdsEmitenteXBairro: TStringField;
    cdsEmitenteCMun: TStringField;
    cdsEmitenteXMun: TStringField;
    cdsEmitenteUF: TStringField;
    cdsEmitenteCEP: TStringField;
    cdsEmitenteCPais: TStringField;
    cdsEmitenteXPais: TStringField;
    cdsEmitenteFone: TStringField;
    cdsEmitenteIE: TStringField;
    cdsEmitenteIM: TStringField;
    cdsEmitenteIEST: TStringField;
    cdsEmitenteCRT: TStringField;
    cdsEmitenteDESCR_CST: TStringField;
    cdsEmitenteDADOS_ENDERECO: TStringField;
    cdsDestinatario: TClientDataSet;
    cdsDestinatarioCNPJCPF: TStringField;
    cdsDestinatarioXNome: TStringField;
    cdsDestinatarioXLgr: TStringField;
    cdsDestinatarioNro: TStringField;
    cdsDestinatarioXCpl: TStringField;
    cdsDestinatarioXBairro: TStringField;
    cdsDestinatarioCMun: TStringField;
    cdsDestinatarioXMun: TStringField;
    cdsDestinatarioUF: TStringField;
    cdsDestinatarioCEP: TStringField;
    cdsDestinatarioCPais: TStringField;
    cdsDestinatarioXPais: TStringField;
    cdsDestinatarioFone: TStringField;
    cdsDestinatarioIE: TStringField;
    cdsDadosProdutos: TClientDataSet;
    cdsDadosProdutosCProd: TStringField;
    cdsDadosProdutoscEAN: TStringField;
    cdsDadosProdutosXProd: TStringField;
    cdsDadosProdutosinfAdProd: TStringField;
    cdsDadosProdutosNCM: TStringField;
    cdsDadosProdutosEXTIPI: TStringField;
    cdsDadosProdutosgenero: TStringField;
    cdsDadosProdutosCFOP: TStringField;
    cdsDadosProdutosUCom: TStringField;
    cdsDadosProdutosQCom: TFloatField;
    cdsDadosProdutosVUnCom: TFloatField;
    cdsDadosProdutosVProd: TFloatField;
    cdsDadosProdutoscEANTrib: TStringField;
    cdsDadosProdutosUTrib: TStringField;
    cdsDadosProdutosQTrib: TFloatField;
    cdsDadosProdutosVUnTrib: TFloatField;
    cdsDadosProdutosvFrete: TFloatField;
    cdsDadosProdutosVOutro: TFloatField;
    cdsDadosProdutosvSeg: TFloatField;
    cdsDadosProdutosvDesc: TStringField;
    cdsDadosProdutosORIGEM: TStringField;
    cdsDadosProdutosCST: TStringField;
    cdsDadosProdutosVBC: TFloatField;
    cdsDadosProdutosPICMS: TFloatField;
    cdsDadosProdutosVICMS: TFloatField;
    cdsDadosProdutosVIPI: TFloatField;
    cdsDadosProdutosPIPI: TFloatField;
    cdsDadosProdutosDescricaoProduto: TStringField;
    cdsDadosProdutosVTotTrib: TFloatField;
    cdsDadosProdutosChaveNFe: TStringField;
    cdsParametros: TClientDataSet;
    cdsParametrosResumoCanhoto: TStringField;
    cdsParametrosMensagem0: TStringField;
    cdsParametrosImagem: TStringField;
    cdsParametrosSistema: TStringField;
    cdsParametrosUsuario: TStringField;
    cdsParametrosFax: TStringField;
    cdsParametrosSite: TStringField;
    cdsParametrosEmail: TStringField;
    cdsParametrosDesconto: TStringField;
    cdsParametrosTotalLiquido: TStringField;
    cdsParametrosChaveAcesso_Descricao: TStringField;
    cdsParametrosContingencia_ID: TStringField;
    cdsParametrosContingencia_Descricao: TStringField;
    cdsParametrosContingencia_Valor: TStringField;
    cdsParametrosLinhasPorPagina: TIntegerField;
    cdsParametrosLogoExpandido: TStringField;
    cdsParametrosDESCR_CST: TStringField;
    cdsParametrosConsultaAutenticidade: TStringField;
    cdsParametrosCasas_qCom: TIntegerField;
    cdsParametrosCasas_vUnCom: TIntegerField;
    cdsParametrosMask_qCom: TStringField;
    cdsParametrosMask_vUnCom: TStringField;
    cdsParametrosLogoCarregado: TBlobField;
    cdsInformacoesAdicionais: TClientDataSet;
    cdsInformacoesAdicionaisOBS: TStringField;
    cdsInformacoesAdicionaisLinhasOBS: TIntegerField;
    cdsDuplicatas: TClientDataSet;
    cdsDuplicatasNDup: TStringField;
    cdsDuplicatasDVenc: TStringField;
    cdsDuplicatasVDup: TFloatField;
    cdsDuplicatasChaveNFe: TStringField;
    cdsCalculoImposto: TClientDataSet;
    cdsCalculoImpostoVBC: TFloatField;
    cdsCalculoImpostoVICMS: TFloatField;
    cdsCalculoImpostoVBCST: TFloatField;
    cdsCalculoImpostoVST: TFloatField;
    cdsCalculoImpostoVProd: TFloatField;
    cdsCalculoImpostoVFrete: TFloatField;
    cdsCalculoImpostoVSeg: TFloatField;
    cdsCalculoImpostoVDesc: TFloatField;
    cdsCalculoImpostoVII: TFloatField;
    cdsCalculoImpostoVIPI: TFloatField;
    cdsCalculoImpostoVPIS: TFloatField;
    cdsCalculoImpostoVCOFINS: TFloatField;
    cdsCalculoImpostoVOutro: TFloatField;
    cdsCalculoImpostoVNF: TFloatField;
    cdsCalculoImpostoVTotTrib: TFloatField;
    cdsCalculoImpostoVTribPerc: TFloatField;
    cdsCalculoImpostoVTribFonte: TStringField;
    cdsTransportador: TClientDataSet;
    cdsTransportadorModFrete: TStringField;
    cdsTransportadorCNPJCPF: TStringField;
    cdsTransportadorXNome: TStringField;
    cdsTransportadorIE: TStringField;
    cdsTransportadorXEnder: TStringField;
    cdsTransportadorXMun: TStringField;
    cdsTransportadorUF: TStringField;
    cdsVeiculo: TClientDataSet;
    cdsVeiculoPLACA: TStringField;
    cdsVeiculoUF: TStringField;
    cdsVeiculoRNTC: TStringField;
    cdsVolumes: TClientDataSet;
    cdsVolumesQVol: TFloatField;
    cdsVolumesEsp: TStringField;
    cdsVolumesMarca: TStringField;
    cdsVolumesNVol: TStringField;
    cdsVolumesPesoL: TFloatField;
    cdsVolumesPesoB: TFloatField;
    cdsISSQN: TClientDataSet;
    cdsISSQNvSERV: TFloatField;
    cdsISSQNvBC: TFloatField;
    cdsISSQNvISS: TFloatField;
    cdsFatura: TClientDataSet;
    cdsFaturaPagamento: TStringField;
    cdsFaturanFat: TStringField;
    cdsFaturavOrig: TFloatField;
    cdsFaturavDesc: TFloatField;
    cdsFaturavLiq: TFloatField;
    cdsLocalRetirada: TClientDataSet;
    cdsLocalRetiradaCNPJ: TStringField;
    cdsLocalRetiradaXLgr: TStringField;
    cdsLocalRetiradaNro: TStringField;
    cdsLocalRetiradaXCpl: TStringField;
    cdsLocalRetiradaXBairro: TStringField;
    cdsLocalRetiradaCMun: TStringField;
    cdsLocalRetiradaXMun: TStringField;
    cdsLocalRetiradaUF: TStringField;
    cdsLocalEntrega: TClientDataSet;
    cdsLocalEntregaCNPJ: TStringField;
    cdsLocalEntregaXLgr: TStringField;
    cdsLocalEntregaNro: TStringField;
    cdsLocalEntregaXCpl: TStringField;
    cdsLocalEntregaXBairro: TStringField;
    cdsLocalEntregaCMun: TStringField;
    cdsLocalEntregaXMun: TStringField;
    cdsLocalEntregaUF: TStringField;
    frxIdentificacao: TfrxDBDataset;
    frxEmitente: TfrxDBDataset;
    frxDestinatario: TfrxDBDataset;
    frxDadosProdutos: TfrxDBDataset;
    frxParametros: TfrxDBDataset;
    frxDuplicatas: TfrxDBDataset;
    frxCalculoImposto: TfrxDBDataset;
    frxTransportador: TfrxDBDataset;
    frxVeiculo: TfrxDBDataset;
    frxVolumes: TfrxDBDataset;
    frxISSQN: TfrxDBDataset;
    frxFatura: TfrxDBDataset;
    frxLocalRetirada: TfrxDBDataset;
    frxLocalEntrega: TfrxDBDataset;
    frxInformacoesAdicionais: TfrxDBDataset;
    frxBarCodeObject: TfrxBarCodeObject;
    frxReport: TfrxReport;
    cdsEventos: TClientDataSet;
    frxEventos: TfrxDBDataset;
    cdsDadosProdutosvISSQN: TFloatField;
    cdsIdentificacaoMensagemFiscal: TStringField;
    cdsIdentificacaoURL: TStringField;
    cdsCalculoImpostovTotPago: TFloatField;
    cdsParametrosQrCodeCarregado: TBlobField;
    cdsParametrosDescricaoViaEstabelec: TStringField;
    cdsDestinatarioConsumidor: TStringField;
    cdsPagamento: TClientDataSet;
    cdsPagamentotPag: TStringField;
    cdsPagamentovPag: TFloatField;
    cdsPagamentoCNPJ: TStringField;
    cdsPagamentotBand: TStringField;
    cdsPagamentocAut: TStringField;
    frxPagamento: TfrxDBDataset;
    cdsParametrosQtdeItens: TIntegerField;
    cdsCalculoImpostovTroco: TFloatField;
    cdsDadosProdutosvBcISSQN: TFloatField;
    cdsDadosProdutosUTrib2: TStringField;
    cdsDadosProdutosQTrib2: TFloatField;
    cdsDadosProdutosVUnTrib2: TFloatField;
    cdsDadosProdutosvBcST: TFloatField;
    cdsDadosProdutosvICMSST: TFloatField;
    cdsDadosProdutosnLote: TStringField;
    cdsDadosProdutosqLote: TFloatField;
    cdsDadosProdutosdFab: TDateField;
    cdsDadosProdutosdVal: TDateField;
    cdsParametrosExpandirDadosAdicionaisAuto: TStringField;
    constructor Create(AOwner: TComponent); override;
    procedure frxReportBeforePrint(Sender: TfrxReportComponent);
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
    FImprimirUnQtVlComercial: boolean;
    FExpandirDadosAdicionaisAuto: boolean;
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
  public
    property NFe: TNFe read FNFe write FNFe;
    property Evento: TEventoNFe read FEvento write FEvento;
    property DANFEClassOwner: TACBrNFeDANFEClass read FDANFEClassOwner;
    property ExibirTotalTributosItem: Boolean read FExibirTotalTributosItem write FExibirTotalTributosItem default False;
    property ExibeCampoFatura: Boolean read FExibeCampoFatura write FExibeCampoFatura default True;
    property TributosFonte: string read FTributosFonte write FTributosFonte;
    property TributosPercentual: TpcnPercentualTributos read FTributosPercentual write FTributosPercentual;
    property TributosPercentualPersonalizado: double read FTributosPercentualPersonalizado write FTributosPercentualPersonalizado;
    property MarcaDaguaMSG: string read FMarcaDaguaMSG write FMarcaDaguaMSG;
    property ImprimirUnQtVlComercial: boolean read FImprimirUnQtVlComercial write FImprimirUnQtVlComercial;
    property ExpandirDadosAdicionaisAuto: boolean read FExpandirDadosAdicionaisAuto write FExpandirDadosAdicionaisAuto;
    property vTroco: Currency read FvTroco write FvTroco;
    property Detalhado: Boolean read FDetalhado write FDetalhado;
    property URLConsultaPublica:String read FURLConsultaPublica write FURLConsultaPublica;
    property DescricaoViaEstabelec: string read FDescricaoViaEstabelec write FDescricaoViaEstabelec;
    procedure CarregaDadosNFe;
    procedure CarregaDadosEventos;
    procedure PintarQRCode(QRCodeData: String; APict: TPicture);
  end;

var
  dmACBrNFeFR: TdmACBrNFeFR;

implementation

uses ACBrNFe, ACBrDFeUtil, StrUtils, Math, DateUtils;

{$R *.dfm}

type
  ArrOfStr = Array of String;
  TSplitResult = array of string;

{ TdmACBrNFeFR }

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
  Step: ^String;
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

function Explode(sPart, sInput: String): ArrOfStr;
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

function CollateBr(Str: String): String;
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

procedure TdmACBrNFeFR.CarregaCalculoImposto;
begin
  with cdsCalculoImposto do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.Total.ICMSTot do
    begin
      FieldByName('VBC').AsFloat := StringToFloatDef(FloatToStr(VBC), 0);
      FieldByName('VICMS').AsFloat := StringToFloatDef(FloatToStr(VICMS), 0);
      FieldByName('VBCST').AsFloat := StringToFloatDef(FloatToStr(VBCST), 0);
      FieldByName('VST').AsFloat := StringToFloatDef(FloatToStr(VST), 0);
      FieldByName('VProd').AsFloat := StringToFloatDef(FloatToStr(VProd), 0);
      FieldByName('VFrete').AsFloat := StringToFloatDef(FloatToStr(VFrete), 0);
      FieldByName('VSeg').AsFloat := StringToFloatDef(FloatToStr(VSeg), 0);
      FieldByName('VDesc').AsFloat := StringToFloatDef(FloatToStr(VDesc), 0);
      FieldByName('VII').AsFloat := StringToFloatDef(FloatToStr(VII), 0);
      FieldByName('VIPI').AsFloat := StringToFloatDef(FloatToStr(VIPI), 0);
      FieldByName('VPIS').AsFloat := StringToFloatDef(FloatToStr(VPIS), 0);
      FieldByName('VCOFINS').AsFloat := StringToFloatDef(FloatToStr(VCOFINS), 0);
      FieldByName('VOutro').AsFloat := StringToFloatDef(FloatToStr(VOutro), 0);
      FieldByName('VNF').AsFloat := StringToFloatDef(FloatToStr(VNF), 0);
      FieldByName('VTotTrib').AsFloat := StringToFloatDef(FloatToStr(VTotTrib), 0);
      if TributosPercentual = ptPersonalizado then
        FieldByName('VTribPerc').AsFloat := StringToFloatDef(FloatToStr(TributosPercentualPersonalizado), 0)
      else
      begin
        if (TributosPercentual = ptValorProdutos) and (VProd > 0) then
          FieldByName('VTribPerc').AsFloat := StringToFloatDef(FloatToStr(VTotTrib*100/VProd), 0)
        else if (TributosPercentual = ptValorNF) and (VNF > 0) then
          FieldByName('VTribPerc').AsFloat := StringToFloatDef(FloatToStr(VTotTrib*100/VNF), 0)
        else
          FieldByName('VTribPerc').AsFloat := 0;
      end;
      if NaoEstaVazio(TributosFonte) then
        FieldByName('VTribFonte').AsString := '(Fonte: '+TributosFonte+')';
    end;

    FieldByName('vTroco').AsCurrency := FvTroco;
    FieldByName('vTotPago').AsCurrency := FvTroco+FieldByName('VProd').AsFloat;

    Post;
  end;
end;

procedure TdmACBrNFeFR.CarregaDadosNFe;
begin
  CarregaIdentificacao;
  CarregaEmitente;
  CarregaDestinatario;
  CarregaDadosProdutos;
  CarregaParametros;
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

procedure TdmACBrNFeFR.CarregaDadosProdutos;
var
  i: Integer;
  vTemp2: TStringList;
  IndexCampo2: Integer;
  Campos2: TSplitResult;
  BufferXInfProd: String;
  TmpStr: String;
  j: Integer;
  wInfAdProd: String;
begin
  if not cdsParametros.Active then
    CarregaParametros;

  // verificar se e DANFE detalhado

  // dados dos produtos
  with cdsDadosProdutos do
  begin
    Close;
    CreateDataSet;

    if (NFe.Ide.modelo <> 65) or FDetalhado then
    begin
      for i := 0 to NFe.Det.Count - 1 do
      begin
        Append;

        with FNFe.Det.Items[i].Prod do
        begin
          FieldByName('ChaveNFe').AsString := FNFe.infNFe.ID;
          FieldByName('cProd').AsString := cProd;
          FieldByName('cEAN').AsString := cEAN;
          FieldByName('XProd').AsString := StringReplace(xProd, ';', #13, [rfReplaceAll]);

          if FDANFEClassOwner.ImprimirTotalLiquido then
            FieldByName('VProd').AsFloat := StringToFloatDef(FloatToStr(VProd - vDesc), 0)
          else
            FieldByName('VProd').AsFloat := StringToFloatDef(FloatToStr(VProd), 0);

          wInfAdProd := FNFe.Det.Items[i].infAdProd;

          if Self.ExibirTotalTributosItem then
            FieldByName('vTotTrib').AsFloat := StringToFloatDef(FloatToStr(FNFe.Det.Items[i].Imposto.vTotTrib), 0)
          else
            FieldByName('vTotTrib').AsFloat := 0;

          if FieldByName('vTotTrib').AsFloat <> 0 then
          begin
            wInfAdProd := wInfAdProd+#13+'Val Aprox Tributos: '+ FloatToStrF(FieldByName('vTotTrib').AsFloat,ffCurrency,15,2);
            wInfAdProd := wInfAdProd+' ('+FloatToStrF(((StringToFloatDef(FieldByName('vTotTrib').AsString,0)*100)/(StringToFloatDef(FieldByName('VProd').AsString,0) +
            StringToFloatDef(FieldByName('VOutro').AsString,0) -
            StringToFloatDef(FieldByName('VDesc').AsString,0)) ),ffNumber,15,2)+'%)';
          end;

          vTemp2 := TStringList.Create;
          try
            if FDANFEClassOwner.ImprimirDetalhamentoEspecifico then
            begin
              { detalhamento especifico de veículos }
              if Trim(veicProd.chassi) <> '' then
              begin
                vTemp2.Add(' CHASSI: ' + veicProd.chassi);
                vTemp2.Add(' COMBUSTÍVEL: ' + veicProd.tpComb);
                vTemp2.Add(' COR: ' + veicProd.xCor);
                vTemp2.Add(' FAB./MOD.: ' + IntToStr(veicProd.anoFab) + '/' + IntToStr(veicProd.anoMod));
                //vTemp2.Add(' RENAVAM: ' + veicProd.RENAVAM); // Essa propriedade foi comentada na class, por isso comentei aqui tambem.
                vTemp2.Add(' Nº DO MOTOR: ' + veicProd.nMotor);

                if Trim(wInfAdProd) <> '' then
                   wInfAdProd := wInfAdProd + ';'; //insere quebra de linha antes do detalhamento

                wInfAdProd := wInfAdProd + vTemp2.Text;
                vTemp2.Clear;
              end;

              { detalhamento específico de medicamentos }
              if med.Count > 0 then
              begin
                for j := 0 to med.Count - 1 do
                  begin
                    with med.Items[j] do
                      begin
                        vTemp2.Add('-LOTE: ' + nLote);
                        vTemp2.Add(' QTDADE: ' + FormatFloat(qLote));
                        vTemp2.Add(' FABR.: ' + FormatDate(DateToStr(dFab)));
                        vTemp2.Add(' VAL.: ' + FormatDate(DateToStr(dVal)));
                        vTemp2.Add(SeSenao(vPMC > 0, ' PMC: ' + FormatFloat(vPMC), ''));
                      end;
                  end;

                if (Trim(wInfAdProd) <> '') then
                   wInfAdProd := wInfAdProd + ';'; //insere quebra de linha antes do detalhamento

                wInfAdProd := wInfAdProd + vTemp2.Text;
                vTemp2.Clear;
              end;

              if arma.Count > 0 then
              begin
                for j := 0 to arma.Count - 1 do
                  begin
                    with arma.Items[j] do
                      begin
                        if tpArma=taUsoPermitido then
                          vTemp2.Add('-USO PERMITIDO')
                        else
                          vTemp2.Add('-USO RESTRITO');
                        vTemp2.Add(' N.SÉRIE: ' + nSerie);
                        vTemp2.Add(' N.SÉRIE CANO.: ' +nCano );
                        vTemp2.Add(' DESC.: ' + descr);
                      end;
                  end;

                if (Trim(wInfAdProd) <> '') then
                   wInfAdProd := wInfAdProd + ';'; //insere quebra de linha antes do detalhamento

                wInfAdProd := wInfAdProd + vTemp2.Text;
                vTemp2.Clear;
              end;

            end;

            if Trim(winfAdProd) <> '' then
            begin
              Campos2 := Split(';', winfAdProd);
              for IndexCampo2 := 0 to Length(Campos2) - 1 do
                vTemp2.Add(Trim(Campos2[IndexCampo2]));

              TmpStr := vTemp2.Text;

              //Inserir a quebra de linha para ficar abaixo da descrição do produto
              BufferXInfProd := #13 + TmPStr;
            end
            else
              BufferXInfProd := '';

              FieldByName('infAdProd').AsString := BufferXInfProd;
          finally
            vTemp2.Free;
          end;

          FieldByName('NCM').AsString := NCM;
          FieldByName('EXTIPI').AsString := EXTIPI;
          FieldByName('genero').AsString := '';
          FieldByName('CFOP').AsString := CFOP;
          FieldByName('Ucom').AsString := UCom;
          FieldByName('QCom').AsFloat := StringToFloatDef(FloatToStr(QCom), 0);
          FieldByName('VUnCom').AsFloat := StringToFloatDef(FloatToStr(VUnCom), 0);

          FieldByName('cEANTrib').AsString := cEANTrib;
          FieldByName('UTrib').AsString := uTrib;
          FieldByName('QTrib').AsFloat := StringToFloatDef(FloatToStr(qTrib), 0);
          FieldByName('VUnTrib').AsFloat := StringToFloatDef(FloatToStr(vUnTrib), 0);
          FieldByName('vFrete').AsFloat := StringToFloatDef(FloatToStr(vFrete), 0);
          FieldByName('vSeg').AsFloat := StringToFloatDef(FloatToStr(vSeg), 0);
          FieldByName('vOutro').AsFloat := StringToFloatDef(FloatToStr(vOutro), 0);

          //especifica tipo de impressão
          case FImprimirUnQtVlComercial of
            true:
            begin
              FieldByName('Unidade').AsString      := FieldByName('Ucom').AsString;
              FieldByName('Quantidade').AsFloat    := FieldByName('QCom').AsFloat;
              FieldByName('ValorUnitario').AsFloat := FieldByName('VUnCom').AsFloat;
            end;
            false:
            begin
              FieldByName('Unidade').AsString      := FieldByName('UTrib').AsString;
              FieldByName('Quantidade').AsFloat    := FieldByName('QTrib').AsFloat;
              FieldByName('ValorUnitario').AsFloat := FieldByName('VUnTrib').AsFloat;
            end;
          end;

          if FDANFEClassOwner.ImprimirDescPorc then
          begin
            if vDesc > 0 then
               FieldByName('vDesc').AsString := FormatFloat(((vDesc*100) / (VUnCom * QCom))) + '%'
               //FieldByName('vDesc').AsString := FormatFloat(RoundTo(100 - ((((VUnCom * QCom) - vDesc) / (VUnCom * QCom)) * 100), -1)) + '%'
            else
               FieldByName('vDesc').AsString := FormatFloat(vDesc);
          end
          else
            FieldByName('vDesc').AsString := FormatFloat(vDesc);

          with FNFe.Det.Items[i].Imposto.ISSQN do
          begin
            FieldByName('vISSQN').AsFloat := vISSQN;
            FieldByName('vBcISSQN').AsFloat := vBC;
          end;

          with FNFe.Det.Items[i].Imposto.ICMS do
          begin
            FieldByName('ORIGEM').AsString := OrigToStr(orig);

            case FNFe.Emit.CRT of
              crtSimplesNacional:
              begin
                case CSOSN of
                  csosn101, csosn102, csosn103, csosn201, csosn202, csosn203,
                  csosn300, csosn400, csosn500, csosn900:
                    begin
                      FieldByName('CST').AsString  := CSOSNIcmsToStr(CSOSN);
                      FieldByName('VBC').AsFloat   := vBC;
                      FieldByName('PICMS').AsFloat := pICMS;
                      FieldByName('VICMS').AsFloat := vICMS;
                    end;
                end;
              end;
            else
              begin
                if CST = cst00 then
                  begin
                    FieldByName('CST').AsString := CSTICMSToStr(cst00);
                    FieldByName('VBC').AsFloat := vBC;
                    FieldByName('PICMS').AsFloat := pICMS;
                    FieldByName('VICMS').AsFloat := vICMS;
                  end
                else if CST = cst10 then
                  begin
                    FieldByName('CST').AsString := CSTICMSToStr(cst10);
                    FieldByName('VBC').AsFloat := vBC;
                    FieldByName('PICMS').AsFloat := pICMS;
                    FieldByName('VICMS').AsFloat := vICMS;
                    FieldByName('VBCST').AsFloat := vBcST;
                    FieldByName('VICMSST').AsFloat := vICMSST;
                  end
                else if CST = cst20 then
                  begin
                    FieldByName('CST').AsString := CSTICMSToStr(cst20);
                    FieldByName('VBC').AsFloat := vBC;
                    FieldByName('PICMS').AsFloat := pICMS;
                    FieldByName('VICMS').AsFloat := vICMS;
                  end
                else if CST = cst30 then
                  begin
                    FieldByName('CST').AsString := CSTICMSToStr(cst30);
                    FieldByName('VBC').AsFloat := 0;
                    FieldByName('PICMS').AsFloat := 0;
                    FieldByName('VICMS').AsFloat := 0;
                  end
                else if (CST = cst40) or (CST = cst41) or (CST = cst50) or (CST = cstRep41) then
                  begin
                    if (CST = cst40) then
                      FieldByName('CST').AsString := CSTICMSToStr(cst40)
                    else if (CST = cst41) or (CST = cstRep41) then
                      FieldByName('CST').AsString := CSTICMSToStr(cst41)
                    else if (CST = cst50) then
                      FieldByName('CST').AsString := CSTICMSToStr(cst50);

                    FieldByName('VBC').AsFloat := 0;
                    FieldByName('PICMS').AsFloat := 0;
                    FieldByName('VICMS').AsFloat := 0;
                  end
                else if (CST = cst51) then
                  begin
                    FieldByName('CST').AsString := CSTICMSToStr(cst51);
                    FieldByName('VBC').AsFloat := vBC;
                    FieldByName('PICMS').AsFloat := pICMS;
                    FieldByName('VICMS').AsFloat := vICMS;
                  end
                else if (CST = cst60) then
                  begin
                    FieldByName('CST').AsString := CSTICMSToStr(cst60);
                    FieldByName('VBC').AsFloat := 0;
                    FieldByName('PICMS').AsFloat := 0;
                    FieldByName('VICMS').AsFloat := 0;
                  end
                else if (CST = cst70) then
                  begin
                    FieldByName('CST').AsString := CSTICMSToStr(cst70);
                    FieldByName('VBC').AsFloat := vBC;
                    FieldByName('PICMS').AsFloat := pICMS;
                    FieldByName('VICMS').AsFloat := vICMS;
                    FieldByName('VBCST').AsFloat := vBcST;
                    FieldByName('VICMSST').AsFloat := vICMSST;
                  end
                else if (CST = cst90) then
                  begin
                    FieldByName('CST').AsString := CSTICMSToStr(cst90);
                    FieldByName('VBC').AsFloat := vBC;
                    FieldByName('PICMS').AsFloat := pICMS;
                    FieldByName('VICMS').AsFloat := vICMS;
                  end
                else if (CST = cstPart10) then
                  begin
                    FieldByName('CST').AsString := CSTICMSToStr(cstPart10);
                    FieldByName('VBC').AsFloat := vBC;
                    FieldByName('PICMS').AsFloat := pICMS;
                    FieldByName('VICMS').AsFloat := vICMS;
                  end
                else if (CST = cstPart90) then
                  begin
                    FieldByName('CST').AsString := CSTICMSToStr(cstPart90);
                    FieldByName('VBC').AsFloat := vBC;
                    FieldByName('PICMS').AsFloat := pICMS;
                    FieldByName('VICMS').AsFloat := vICMS;
                  end
                else if (CST = cstVazio) then
                  begin
                    FieldByName('CST').AsString := ' ';
                    FieldByName('VBC').AsFloat := 0;
                    FieldByName('PICMS').AsFloat := 0;
                    FieldByName('VICMS').AsFloat := 0;
                  end;
              end;
            end;
          end;

          with FNFe.Det.Items[i].Imposto.IPI do
          begin
            if (CST = ipi00) or (CST = ipi49) or (CST = ipi50) or (CST = ipi99) then
              begin
                FieldByName('VIPI').AsFloat := StringToFloatDef(FloatToStr(VIPI), 0);
                FieldByName('PIPI').AsFloat := StringToFloatDef(FloatToStr(PIPI), 0);
              end
            else if (CST = ipi01) or (CST = ipi02) or (CST = ipi03) or (CST = ipi04) or (CST = ipi51) or (CST = ipi52) or (CST = ipi53) or (CST = ipi54) or (CST = ipi55) then
              begin
                FieldByName('VIPI').AsFloat := 0;
                FieldByName('PIPI').AsFloat := 0;
              end
            else
              begin
                FieldByName('VIPI').AsFloat := 0;
                FieldByName('PIPI').AsFloat := 0;
              end;
          end;
        end;

        cdsDadosProdutosDescricaoProduto.AsString := Trim(FieldByName('xProd').AsString);
        if Trim(FieldByName('InfAdProd').AsString) <> '' then
          cdsDadosProdutosDescricaoProduto.AsString := cdsDadosProdutosDescricaoProduto.AsString+#13+
                                                     trim(FieldByName('InfAdProd').AsString);

        Post;
      end;
    end;
  end;
end;

procedure TdmACBrNFeFR.CarregaDestinatario;
begin
  { destinatário }
  with cdsDestinatario do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.Dest do
    begin
      if NaoEstaVazio(CNPJCPF) then
       begin
         if Length(CNPJCPF) > 11 then
            FieldByName('CNPJCPF').AsString := FormatarCNPJ(CNPJCPF)
         else
            FieldByName('CNPJCPF').AsString := FormatarCPF(CNPJCPF);
       end
      else
         FieldByName('CNPJCPF').AsString := '';

      FieldByName('XNome').AsString := XNome;
      with EnderDest do
      begin
        FieldByName('XLgr').AsString := XLgr;
        FieldByName('Nro').AsString := Nro;
        FieldByName('XCpl').AsString := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString := IntToStr(CMun);
        FieldByName('XMun').AsString := CollateBr(XMun);
        FieldByName('UF').AsString := UF;
        FieldByName('CEP').AsString := FormatarCEP(Poem_Zeros(CEP, 8));
        FieldByName('CPais').AsString := IntToStr(CPais);
        FieldByName('XPais').AsString := XPais;
        FieldByName('Fone').AsString := FormatarFone(Fone);
      end;
      FieldByName('IE').AsString := IE;
      if (cdsIdentificacaoMod_.AsString = '65') then
      begin
        if (FieldByName('CNPJCPF').AsString = '') then
          FieldByName('Consumidor').AsString := 'CONSUMIDOR NÃO IDENTIFICADO'
        else
          FieldByName('Consumidor').AsString :=
             'CPF: ' + Trim(FieldByName('CNPJCPF').AsString) + ' ' + trim(FieldByName('XNome').AsString) + #13 +
             Trim(FieldByName('XLgr').AsString) + ', ' + Trim(FieldByName('Nro').AsString) + #13 +
             Trim(FieldByName('XBairro').AsString) + ' - ' +
             Trim(FieldByName('XMun').AsString) + ' - ' + Trim(FieldByName('UF').AsString);
      end
      else
        FieldByName('Consumidor').AsString := '';
    end;

    Post;
  end;
end;

procedure TdmACBrNFeFR.CarregaDuplicatas;
var
  i: Integer;
begin
  with cdsDuplicatas do
  begin
    Close;
    CreateDataSet;

    for i := 0 to NFe.Cobr.Dup.Count - 1 do
    begin
      Append;
      with FNFe.Cobr.Dup[i] do
      begin
        FieldByName('ChaveNFe').AsString := FNFe.infNFe.ID;
        FieldByName('NDup').AsString := NDup;
        FieldByName('DVenc').AsString := FormatDate(DateToStr(DVenc));
        FieldByName('VDup').AsFloat := StringToFloatDef(FloatToStr(VDup), 0);
      end;

      Post;
    end;
  end;
end;

procedure TdmACBrNFeFR.CarregaEmitente;
begin
  { emitente }
  with cdsEmitente do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.Emit do
    begin
      FieldByName('CNPJ').AsString := FormatarCNPJ(CNPJCPF);
      FieldByName('XNome').AsString := XNome;
      FieldByName('XFant').AsString := XFant;
      with EnderEmit do
      begin
        FieldByName('Xlgr').AsString := XLgr;
        FieldByName('Nro').AsString := Nro;
        FieldByName('XCpl').AsString := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString := IntToStr(CMun);
        FieldByName('XMun').AsString := CollateBr(XMun);
        FieldByName('UF').AsString := UF;
        FieldByName('CEP').AsString := FormatarCEP(Poem_Zeros(CEP, 8));
        FieldByName('CPais').AsString := IntToStr(CPais);
        FieldByName('XPais').AsString := XPais;
        FieldByName('Fone').AsString := FormatarFone(Fone);
      end;
      FieldByName('IE').AsString := IE;
      FieldByName('IM').AsString := IM;
      FieldByName('IEST').AsString := IEST;
      FieldByName('CRT').AsString := CRTToStr(CRT);

      if Trim(FieldByName('CRT').AsString) = '1' then
        FieldByName('DESCR_CST').AsString := 'CSOSN'
      else
        FieldByName('DESCR_CST').AsString  := 'CST';

      //Modificado por Fábio Gabriel - 10/06/2013 - Para evitar que fique truncado.
      cdsEmitenteDADOS_ENDERECO.AsString :=
        Trim(FieldByName('XLgr').AsString) + ', ' + trim(FieldByName('Nro').AsString);
	   if (trim(FieldByName('XCpl').AsString) <> '') then
          cdsEmitenteDADOS_ENDERECO.AsString := cdsEmitenteDADOS_ENDERECO.AsString + ', ' +
            Trim(FieldByName('XCpl').AsString);
       cdsEmitenteDADOS_ENDERECO.AsString := cdsEmitenteDADOS_ENDERECO.AsString + ' - ' +
											 Trim(FieldByName('XBairro').AsString) + ' - ' + Trim(FieldByName('XMun').AsString) + ' - ' + Trim(FieldByName('UF').AsString) + #13 +
											'Fone: ' + Trim(FieldByName('Fone').AsString) +
        SeSenao(trim(FDANFEClassOwner.Fax) <> '',
                        ' - FAX: ' + FormatarFone(trim(FDANFEClassOwner.Fax)),'')+
        ' - CEP: ' + Trim(FieldByName('CEP').AsString);
      if trim(FDANFEClassOwner.Site) <> '' then
        cdsEmitenteDADOS_ENDERECO.AsString := cdsEmitenteDADOS_ENDERECO.AsString + #13 +
        trim(FDANFEClassOwner.Site);
      if trim(FDANFEClassOwner.Email) <> '' then
        cdsEmitenteDADOS_ENDERECO.AsString := cdsEmitenteDADOS_ENDERECO.AsString + #13 +
        trim(FDANFEClassOwner.Email);
    end;

    Post;
  end;
end;

procedure TdmACBrNFeFR.CarregaFatura;
begin
  with cdsFatura do
  begin
    Close;
    CreateDataSet;

    if Self.ExibeCampoFatura then   //Incluido por Fábio Gabriel - 22/05/2013
    begin
      Append;

      if FNFe.Ide.indPag = ipVista then
        FieldByName('Pagamento').AsString := 'PAGAMENTO À VISTA'
      else if FNFe.Ide.indPag = ipPrazo then
        FieldByName('Pagamento').AsString := 'PAGAMENTO A PRAZO'
      else
        FieldByName('Pagamento').AsString := '';

      if NaoEstaVazio(FNFe.Cobr.Fat.nFat) then
      begin
        with FNFe.Cobr.Fat do
        begin
          FieldByName('nfat').AsString := nFat;
          FieldByName('vOrig').AsFloat := StringToFloatDef(FloatToStr(vOrig), 0);
          FieldByName('vDesc').AsFloat := StringToFloatDef(FloatToStr(vDesc), 0);
          FieldByName('vLiq').AsFloat := StringToFloatDef(FloatToStr(vLiq), 0);
        end;
      end;

      Post;
    end;
  end;
end;

procedure TdmACBrNFeFR.CarregaPagamento;
var
  i: Integer;
  bPag, wPag: String;
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
        case tPag of
          fpDinheiro:        wPag:='Dinheiro';
          fpCheque:          wPag:='Cheque';
          fpCartaoCredito:   wPag:='Cartão Crédito';
          fpCartaoDebito:    wPag:='Cartão Débito';
          fpCreditoLoja:     wPag:='Crédito Loja';
          fpValeAlimentacao: wPag:='Vale Alimentação';
          fpValeRefeicao:    wPag:='Vale Refeição';
          fpValePresente:    wPag:='Vale Presente';
          fpValeCombustivel: wPag:='Vale Combustível';
          fpOutro:           wPag:='Outros';
        end;
        FieldByName('tPag').AsString := wPag;
        FieldByName('vPag').AsFloat  := StringToFloatDef(FloatToStr(vPag), 0);

        with FNFe.Pag do
        begin
           FieldByName('CNPJ').AsString  := FormatarCNPJ(CNPJ);
           case tBand of
             bcVisa:            bPag := 'Visa';
             bcMasterCard:      bPag := 'MasterCard';
             bcAmericanExpress: bPag := 'AmericanExpress';
             bcSorocred:        bPag := 'Sorocred';
             bcOutros:          bPag := 'Outros'
           end;
           FieldByName('tBand').AsString := bPag;
           FieldByName('cAut').AsString  := cAut;
        end;
      end;

      Post;
    end;
  end;
end;

procedure TdmACBrNFeFR.CarregaIdentificacao;
begin
  with cdsIdentificacao do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.infNFe do
    begin
      //FieldByName('Versao').AsString := IntToStr(Versao);
      FieldByName('Id').AsString := LimpaNumero(Id);
      FieldByName('Chave').AsString := NotaUtil.FormatarChaveAcesso(Id);
    end;

    with FNFe.Ide do
    begin
      FieldByName('CUF').AsString := IntToStr(CUF);
      FieldByName('CNF').AsString := IntToStr(CNF);
      FieldByName('NatOp').AsString := NatOp;
      FieldByName('IndPag').AsString := SeSenao(IndPag = ipVista, '0', SeSenao(IndPag = ipPrazo, '1', '2'));
      FieldByName('Mod_').AsString := IntToStr(Modelo);
      FieldByName('Serie').AsString := IntToStr(Serie);
      FieldByName('NNF').AsString := FormatarNumeroDocumentoFiscal(IntToStr(NNF));
      if (IntToStr(Modelo) = '65') then
        FieldByName('DEmi').AsString := FormatDateTime(DateTimeToStr(DEmi))
      else
        FieldByName('DEmi').AsString := FormatDate(DateToStr(DEmi));
      FieldByName('DSaiEnt').AsString := IfThen(DSaiEnt <> 0, FormatDate(DateToStr(DSaiEnt)));
      FieldByName('TpNF').AsString := SeSenao(TpNF = tnEntrada, '0', '1');
      FieldByName('CMunFG').AsString := IntToStr(CMunFG);
      FieldByName('TpImp').AsString := SeSenao(TpImp = tiRetrato, '1', '2');
      FieldByName('TpEmis').AsString := SeSenao(TpEmis = teNormal, '1', '5');
      FieldByName('CDV').AsString := IntToStr(CDV);
      FieldByName('TpAmb').AsString := SeSenao(TpAmb = taHomologacao,'2','1');

      if (IntToStr(Modelo) = '65') then
      begin
        if TpAmb = taHomologacao then
          FieldByName('MensagemFiscal').AsString := 'EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL'
        else
        begin
          if tpEmis <> teNormal then
            FieldByName('MensagemFiscal').AsString := ACBrStr('EMITIDA EM CONTINGÊNCIA')
          else
            FieldByName('MensagemFiscal').AsString := ACBrStr('ÁREA DE MENSAGEM FISCAL');
        end;

        FieldByName('URL').AsString := NotaUtil.GetURLConsultaNFCe(cUF, tpAmb);
      end
      else
      begin
        FieldByName('MensagemFiscal').AsString := '';
        FieldByName('URL').AsString            := '';
      end;

      FieldByName('FinNFe').AsString := SeSenao(FinNFe = fnNormal, '1', SeSenao(FinNFe = fnComplementar, '2', '3'));
      FieldByName('ProcEmi').AsString := SeSenao(ProcEmi = peAplicativoContribuinte, '0', '');
      FieldByName('VerProc').AsString := VerProc;
    end;
    if FNFe.infNFe.versao = 2.00 then
      FieldByName('HoraSaida').AsString := ifthen(FNFe.ide.hSaiEnt = 0, '', TimeToStr(FNFe.ide.hSaiEnt))
    else
      FieldByName('HoraSaida').AsString := ifthen(TimeOf(FNFe.ide.dSaiEnt)=0, '', TimeToStr(FNFe.ide.dSaiEnt));

    Post;
  end;
end;

procedure TdmACBrNFeFR.CarregaInformacoesAdicionais;
var
  i: Integer;
  vTemp: TStringList;
  IndexCampo:Integer;
  Campos: TSplitResult;
  BufferInfCpl: String;
  TmpStr: String;
  wContingencia: string;
  wObs:string;
  wLinhasObs: integer;
begin
  with cdsInformacoesAdicionais do
  begin
    Close;
    CreateDataSet;
    Append;

    wLinhasObs := 0;
    with FNFe.InfAdic do
    begin
      TmpStr := '';
      //Fisco
      if Length(InfAdFisco) = 0 then
        InfAdFisco := '';

      for i := 0 to ObsFisco.Count - 1 do
      begin
        with ObsFisco.Items[i] do
          TmpStr := TmpStr + XCampo + ': ' + XTexto + ';';
      end;
      wObs := TmpStr + InfAdFisco;
      TmpStr := '';

      //Inf. Complementar
      if Length(InfCpl) = 0 then
        InfCpl := '';

      for i := 0 to ObsCont.Count - 1 do
      begin
        with ObsCont.Items[i] do
          TmpStr := TmpStr + XCampo + ': ' + XTexto + ';';
      end;
      if Length(wObs) > 0 then
        wObs := wObs + ';';
      wObs := wObs + TmpStr + InfCpl;
      TmpStr := '';

      //Contingencia
      if FNFe.Ide.tpEmis=teNORMAL then
        wContingencia := ''
      else
      begin
        case FNFe.Ide.tpEmis of
          teContingencia,
          teFSDA,
          teSCAN,
          teSVCAN,
          teSVCRS,
          teSVCSP:
            wContingencia := 'DANFE EM CONTINGÊNCIA, IMPRESSO EM DECORRÊNCIA DE PROBLEMAS TÉCNICOS';
        end;
      end;
      if Length(wObs) > 0 then
        wObs := wObs + ';';
      wObs := wObs + wContingencia;

      vTemp := TStringList.Create;
      try
        if Trim(wObs) <> '' then
        begin
          Campos := Split(';', wObs);
          for IndexCampo := 0 to Length(Campos) - 1 do
              vTemp.Add(Campos[IndexCampo]);
           wLinhasObs := 1; //TotalObS(vTemp.Text);
           TmpStr := vTemp.Text;

           BufferInfCpl := TmpStr;
        end
        else
           BufferInfCpl := '';

      finally
        vTemp.Free;
      end;
    end;

    FieldByName('OBS').AsString        := BufferInfCpl;
    FieldByName('LinhasOBS').AsInteger := wLinhasObs;

    Post;
  end;
end;

procedure TdmACBrNFeFR.CarregaISSQN;
begin
  with cdsISSQN do
  begin
    Close;
    CreateDataSet;
    Append;
    with FNFe.Total.ISSQNtot do
    begin
      FieldByName('vSERV').AsFloat := StringToFloatDef(FloatToStr(VServ), 0);
      FieldByName('vBC').AsFloat := StringToFloatDef(FloatToStr(VBC), 0);
      FieldByName('vISS').AsFloat := StringToFloatDef(FloatToStr(VISS), 0);
    end;
    Post;
  end;
end;

procedure TdmACBrNFeFR.CarregaLocalEntrega;
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
        if NaoEstaVazio(CNPJCPF) then
        begin
          if Length(CNPJCPF) > 11 then
             FieldByName('CNPJ').AsString := FormatarCNPJ(CNPJCPF)
          else
             FieldByName('CNPJ').AsString := FormatarCPF(CNPJCPF);
        end
        else
           FieldByName('CNPJ').AsString := FormatarCNPJ(Poem_Zeros(0, 18));

        FieldByName('Xlgr').AsString := XLgr;
        FieldByName('Nro').AsString := Nro;
        FieldByName('XCpl').AsString := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString := inttostr(CMun);
        FieldByName('XMun').AsString := CollateBr(XMun);
        FieldByName('UF').AsString := UF;
      end;
      Post;
    end;
  end;
end;

procedure TdmACBrNFeFR.CarregaLocalRetirada;
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
        if NaoEstaVazio(CNPJCPF) then
        begin
          if Length(CNPJCPF) > 11 then
             FieldByName('CNPJ').AsString := FormatarCNPJ(CNPJCPF)
          else
             FieldByName('CNPJ').AsString := FormatarCPF(CNPJCPF);
        end
        else
           FieldByName('CNPJ').AsString := FormatarCNPJ(Poem_Zeros(0, 18));

        FieldByName('Xlgr').AsString := XLgr;
        FieldByName('Nro').AsString := Nro;
        FieldByName('XCpl').AsString := XCpl;
        FieldByName('XBairro').AsString := XBairro;
        FieldByName('CMun').AsString := inttostr(CMun);
        FieldByName('XMun').AsString := CollateBr(XMun);
        FieldByName('UF').AsString := UF;
      end;
      Post;
    end;
  end;
end;

procedure TdmACBrNFeFR.CarregaParametros;
var
  vChave_Contingencia: String;
  vResumo: String;
  vStream: TMemoryStream;
  vStringStream: TStringStream;
begin
  { parâmetros }
  with cdsParametros do
  begin
    Close;
    CreateDataSet;
    Append;

    //Carregar Resumo Canhoto
    vResumo := '';
    if DANFEClassOwner.ExibirResumoCanhoto then
    begin
       if EstaVazio(DANFEClassOwner.ExibirResumoCanhoto_Texto) then
          vResumo := 'Emissão: ' + FormatDate(DateToStr(FNFe.Ide.DEmi)) + '  Dest/Reme: ' + FNFe.Dest.XNome + '  Valor Total: ' + FormatFloat(FNFe.Total.ICMSTot.VNF)
       else
          vResumo := DANFEClassOwner.ExibirResumoCanhoto_Texto;
    end;
    FieldByName('ResumoCanhoto').AsString := vResumo;

    if (FNFe.Ide.TpAmb = taHomologacao) then
    begin
      //Modificado em 22/05/2013 - Fábio Gabriel
      if (FNFe.Ide.tpEmis in [teContingencia, teFSDA, teSCAN, teSVCAN, teSVCRS, teSVCSP]) then
      begin
        if (FNFe.procNFe.cStat in [101, 151, 155]) then
          FieldByName('Mensagem0').AsString := 'NFe sem Valor Fiscal - HOMOLOGAÇÃO'+
            #10#13+'NFe em Contingência - CANCELADA'
        else
          FieldByName('Mensagem0').AsString := 'NFe sem Valor Fiscal - HOMOLOGAÇÃO'+
            #10#13+'NFe em Contingência';
      end
      else
        FieldByName('Mensagem0').AsString := 'NFe sem Valor Fiscal - HOMOLOGAÇÃO'
    end
    else
    begin
      if not (FNFe.Ide.tpEmis in [teContingencia, teFSDA, teSVCAN, teSVCRS, teSVCSP]) then
      begin
        //prioridade para opção NFeCancelada
        if (FDANFEClassOwner.NFeCancelada) or
           ((NaoEstaVazio(FNFe.procNFe.nProt)) and
            (FNFe.procNFe.cStat in [101,151,155])) then
          FieldByName('Mensagem0').AsString := 'NFe Cancelada'
        else if FNFe.procNFe.cStat = 110 then
          FieldByName('Mensagem0').AsString := 'NFe denegada pelo Fisco'
        else if ((EstaVazio(FDANFEClassOwner.ProtocoloNFe)) and
                 (EstaVazio(FNFe.procNFe.nProt))) then
          FieldByName('Mensagem0').AsString := 'NFe sem Autorização de Uso da SEFAZ'
        else if (FNFe.Ide.tpImp = tiSimplificado) then
           FieldByName('Mensagem0').AsString := 'EMISSÃO NORMAL'
        else
          FieldByName('Mensagem0').AsString := '';
      end
      else
        FieldByName('Mensagem0').AsString := '';
    end;

    //Marca Dagua
    FieldByName('Mensagem0').AsString := SeSenao(trim(FieldByName('Mensagem0').AsString) = '',
                                                         '',
                                                         FieldByName('Mensagem0').AsString+#10#13)+
                                         MarcaDaguaMSG;

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
        cdsParametrosLogoCarregado.LoadFromStream(vStream);
      finally
        vStream.Free;
      end;
    end;

    if DANFEClassOwner.ExpandirLogoMarca then
      FieldByName('LogoExpandido').AsString := '1'
    else
      FieldByName('LogoExpandido').AsString := '0';

    if FDANFEClassOwner.Sistema <> '' then
      FieldByName('Sistema').AsString := FDANFEClassOwner.Sistema
    else
      FieldByName('Sistema').AsString := 'Projeto ACBr - http://acbr.sf.net';

    if FDANFEClassOwner.Usuario <> '' then
      FieldByName('Usuario').AsString := ' - ' + FDANFEClassOwner.Usuario
    else
      FieldByName('Usuario').AsString := '';

    if FDANFEClassOwner.Fax <> '' then
      FieldByName('Fax').AsString := ' - FAX ' + FDANFEClassOwner.Fax
    else
      FieldByName('Fax').AsString := '';

    FieldByName('Site').AsString := FDANFEClassOwner.Site;
    FieldByName('Email').AsString := FDANFEClassOwner.Email;

    if FDANFEClassOwner.ImprimirDescPorc then
      FieldByName('Desconto').AsString := '%'
    else
      FieldByName('Desconto').AsString := 'VALOR';

    if FDANFEClassOwner.ImprimirTotalLiquido then
      FieldByName('TotalLiquido').AsString := 'LÍQUIDO'
    else
      FieldByName('TotalLiquido').AsString := 'TOTAL';

    FieldByName('Contingencia_ID').AsString := '';
    FieldByName('ConsultaAutenticidade').AsString := 'Consulta de autenticidade no portal nacional da NF-e'+#13+
                                                     'www.nfe.fazenda.gov.br/portal ou no site da Sefaz autorizadora';
    if ((FNFe.Ide.tpEmis in [teNormal,teSVCAN,teSCAN,teSVCRS,teSVCSP])) then
    begin
      FieldByName('ChaveAcesso_Descricao').AsString := 'CHAVE DE ACESSO';
      FieldByName('Contingencia_ID').AsString := '';

      if ((FDANFEClassOwner.NFeCancelada) or (FNFe.procNFe.cStat in [101,151,155])) then
        FieldByName('Contingencia_Descricao').AsString := 'PROTOCOLO DE HOMOLOGAÇÃO DO CANCELAMENTO'
      else
        FieldByName('Contingencia_Descricao').AsString := 'PROTOCOLO DE AUTORIZAÇÃO DE USO';

      if EstaVazio(FDANFEClassOwner.ProtocoloNFe) then
      begin
        if not (FNFe.Ide.tpEmis in [teContingencia, teFSDA]) and EstaVazio(FNFe.procNFe.nProt) then
          FieldByName('Contingencia_Valor').AsString := 'NFe sem Autorização de Uso da SEFAZ'
        else
          FieldByName('Contingencia_Valor').AsString := FNFe.procNFe.nProt + ' ' + SeSenao(FNFe.procNFe.dhRecbto <> 0, DateTimeToStr(FNFe.procNFe.dhRecbto), '');
      end
      else
        FieldByName('Contingencia_Valor').AsString := FDANFEClassOwner.ProtocoloNFe;
    end
    else
    begin
      if ((FNFe.Ide.tpEmis = teContingencia) or (FNFe.Ide.tpEmis = teFSDA)) then
      begin
        vChave_Contingencia := NotaUtil.GerarChaveContingencia(FNFe);
        FieldByName('ChaveAcesso_Descricao').AsString := 'CHAVE DE ACESSO';

        FieldByName('Contingencia_ID').AsString := vChave_Contingencia;
        FieldByName('Contingencia_Descricao').AsString := 'DADOS DA NF-E';
        FieldByName('Contingencia_Valor').AsString := NotaUtil.FormatarChaveContigencia(vChave_Contingencia);
        FieldByName('ConsultaAutenticidade').AsString := '';
      end
      else
      if (FNFe.Ide.tpEmis = teOffLine) then
      begin
        FieldByName('Contingencia_Valor').AsString := FNFe.procNFe.nProt + ' ' + SeSenao(FNFe.procNFe.dhRecbto <> 0, DateTimeToStr(FNFe.procNFe.dhRecbto), '');
      end;
    end;

    FieldByName('LinhasPorPagina').AsInteger := FDANFEClassOwner.ProdutosPorPagina;
    if ExpandirDadosAdicionaisAuto then
      FieldByName('ExpandirDadosAdicionaisAuto').AsString := 'S'
    else
      FieldByName('ExpandirDadosAdicionaisAuto').AsString := 'N';

    FieldByName('Mask_qCom').AsString     := FDANFEClassOwner.CasasDecimais._Mask_qCom;
    FieldByName('Mask_vUnCom').AsString   := FDANFEClassOwner.CasasDecimais._Mask_vUnCom;
    FieldByName('Casas_qCom').AsInteger   := FDANFEClassOwner.CasasDecimais._qCom;
    FieldByName('Casas_vUnCom').AsInteger := FDANFEClassOwner.CasasDecimais._vUnCom;

    FieldByName('QtdeItens').AsInteger    := NFe.Det.Count;
    FieldByName('DescricaoViaEstabelec').AsString := FDescricaoViaEstabelec;

    Post;
  end;
end;

procedure TdmACBrNFeFR.CarregaTransportador;
var
  wFrete: String;
begin
  with cdsTransportador do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.Transp do
    begin
      case ModFrete of
        mfContaEmitente: wFrete:='0-EMITENTE';
        mfContaDestinatario: wFrete:='1-DEST/REM';
        mfContaTerceiros: wFrete:='2-TERCEIROS';
        mfSemFrete: wFrete:='9-SEM FRETE';
      end;
      FieldByName('ModFrete').AsString := wFrete;

      with Transporta do
      begin
        if NaoEstaVazio(CNPJCPF) then
        begin
          if Length(CNPJCPF) > 11 then
            FieldByName('CNPJCPF').AsString := FormatarCNPJ(CNPJCPF)
          else
            FieldByName('CNPJCPF').AsString := FormatarCPF(CNPJCPF);
        end
        else
          FieldByName('CNPJCPF').AsString := '';

        FieldByName('XNome').AsString := XNome;
        FieldByName('IE').AsString := IE;
        FieldByName('XEnder').AsString := XEnder;
        FieldByName('XMun').AsString := CollateBr(XMun);
        FieldByName('UF').AsString := UF;
      end;
    end;

    Post;
  end;
end;

procedure TdmACBrNFeFR.CarregaVeiculo;
begin
  with cdsVeiculo do
  begin
    Close;
    CreateDataSet;
    Append;

    with FNFe.Transp.VeicTransp do
    begin
      FieldByName('PLACA').AsString := Placa;
      FieldByName('UF').AsString := UF;
      FieldByName('RNTC').AsString := RNTC;
    end;

    Post;
  end;
end;

procedure TdmACBrNFeFR.CarregaVolumes;
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
        FieldByName('QVol').AsFloat := StringToFloatDef(IntToStr(QVol), 0);
        FieldByName('Esp').AsString := Esp;
        FieldByName('Marca').AsString := Marca;
        FieldByName('NVol').AsString := NVol;
        FieldByName('PesoL').AsFloat := StringToFloatDef(FloatToStr(PesoL), 0);
        FieldByName('PesoB').AsFloat := StringToFloatDef(FloatToStr(PesoB), 0);
      end;
      Post;
    end;

  end;
end;

procedure TdmACBrNFeFR.CarregaDadosEventos;
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
        FieldByName('ChaveAcesso').AsString := NotaUtil.FormatarChaveAcesso(InfEvento.chNFe);

        // Carta de correção eletrônica
        FieldByName('cOrgao').AsInteger := InfEvento.cOrgao;

        case InfEvento.tpAmb of
          taProducao:    FieldByName('tpAmb').AsString := 'PRODUÇÃO';
          taHomologacao: FieldByName('tpAmb').AsString := 'HOMOLOGAÇÃO - SEM VALOR FISCAL';
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

constructor TdmACBrNFeFR.Create(AOwner: TComponent);
begin
  inherited;
  FDANFEClassOwner := TACBrNFeDANFEClass(AOwner);
end;

procedure TdmACBrNFeFR.PintarQRCode(QRCodeData: String; APict: TPicture);
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

procedure TdmACBrNFeFR.frxReportBeforePrint(Sender: TfrxReportComponent);
var
  qrcode: String;
begin
  qrCode := '';
  if Assigned(NFe) then
  begin
   if (NFe.Ide.modelo = 55) and (FNFe.Ide.tpImp = tiSimplificado) then
   begin
     if cdsParametros.FieldByName('Imagem').AsString = '' then
     begin
        frxReport.FindObject('PageHeader1').Visible := False;
        frxReport.FindObject('ImgLogo').Visible := False;
     end
     else
     begin
        frxReport.FindObject('PageHeader1').Visible := True;
        frxReport.FindObject('ImgLogo').Visible := True;
     end;
   end;

   if (NFe.Ide.modelo = 65) then
   begin
     if cdsParametros.FieldByName('Imagem').AsString = '' then
     begin
        frxReport.FindObject('ReportTitle1').Visible := False;
        frxReport.FindObject('ImgLogo').Visible := False;
     end
     else
     begin
        frxReport.FindObject('ReportTitle1').Visible := True;
        frxReport.FindObject('ImgLogo').Visible := True;
     end;

     qrcode := NotaUtil.GetURLQRCode( NFe.ide.cUF,
                                      NFe.ide.tpAmb,
                                      OnlyNumber(NFe.InfNFe.ID),
                                      SeSenao(NFe.Dest.idEstrangeiro <> '',NFe.Dest.idEstrangeiro, NFe.Dest.CNPJCPF),
                                      NFe.ide.dEmi,
                                      NFe.Total.ICMSTot.vNF,
                                      NFe.Total.ICMSTot.vICMS,
                                      NFe.signature.DigestValue,
                                      TACBrNFe( FDANFEClassOwner.ACBrNFe ).Configuracoes.Geral.IdToken,
                                      TACBrNFe( FDANFEClassOwner.ACBrNFe ).Configuracoes.Geral.Token );

      PintarQRCode( qrcode, TfrxPictureView(frxReport.FindObject('ImgQrCode')).Picture );
    end;
  end;
end;

end.

