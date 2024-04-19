{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Wemerson Souto, Daniel Simoes de Almeida        }
{							   André Ferreira de Moraes, Rafael Dias           }
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

unit ACBrNFeDANFEClass;

interface

uses
  SysUtils, Classes,
  ACBrBase, ACBrDFeDANFeReport,
  pcnNFe, pcnConversao, pcnConversaoNFe, StrUtilsEx, TypInfo;

type
  TDetVeiculo = (dv_tpOp, dv_chassi, dv_cCor, dv_xCor, dv_pot, dv_cilin,
    dv_pesoL, dv_pesoB, dv_nSerie, dv_tpComb, dv_nMotor, dv_CMT,
    dv_dist, dv_anoMod, dv_anoFab, dv_tpPint, dv_tpVeic,
    dv_espVeic, dv_VIN, dv_condVeic, dv_cMod, dv_cCorDENATRAN,
    dv_lota, dv_tpRest);
  TDetMedicamento = (dm_nLote, dm_qLote, dm_dFab, dm_dVal, dm_vPMC);
  TDetArmamento = (da_tpArma, da_nSerie, da_nCano, da_descr);
  TDetCombustivel = (dc_cProdANP, dc_CODIF, dc_qTemp, dc_UFCons, dc_CIDE,
    dc_qBCProd, dc_vAliqProd, dc_vCIDE);
  TDescricaoPagamento = (icaTipo, icaBandeira, icaAutorizacao);
  TDetRastro = (dr_nLote, dr_qLote, dr_dFab, dr_dVal, dr_cAgreg);
  TDetVeiculos = set of TDetVeiculo;
  TDetMedicamentos = set of TDetMedicamento;
  TDetArmamentos = set of TDetArmamento;
  TDetCombustiveis = set of TDetCombustivel;
  TDescricaoPagamentos = set of TDescricaoPagamento;
  TDetRastros = set of TDetRastro;

  { TACBrNFeDANFEClass }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNFeDANFEClass = class(TACBrDFeDANFeReport)
  private
    FImprimeDescPorPercentual: Boolean;
    FImprimeValor: TImprimirUnidQtdeValor;
    FImprimeDetalhamentoEspecifico: Boolean;
    FImprimeDescAcrescItem: TpcnImprimeDescAcrescItem;
    FPosCanhoto: TPosRecibo;
    FPosCanhotoLayout: TPosReciboLayout;
    FExibeResumoCanhoto: Boolean;
    FTextoResumoCanhoto: String;
    FExibeCampoFatura: Boolean;
    FExibeDadosISSQN: Boolean;
    FExibeDadosDocReferenciados: Boolean;
	  FExibeDadosInscricaoSuframa:Boolean;
    FDetVeiculos: TDetVeiculos;
    FDetMedicamentos: TDetMedicamentos;
    FDetArmamentos: TDetArmamentos;
    FDetCombustiveis: TDetCombustiveis;
    FDetRastros: TDetRastros;
    FTributosPercentual: TpcnPercentualTributos;
    FTributosPercentualPersonalizado: Double;
    FExpandirDadosAdicionaisAuto: boolean;
    FExibeCampoDePagamento: TpcnInformacoesDePagamento;
    FEtiqueta: Boolean;
    FFormatarNumeroDocumento : Boolean;

    procedure SetTributosPercentual(const AValue: TpcnPercentualTributos);
    procedure SetTributosPercentualPersonalizado(const AValue: Double);
  public
    constructor Create(AOwner: TComponent); override;

    function ManterinfAdProd(aNFE: TNFe; const inItem: Integer): String; override;
    function ManterVeiculos(aNFE: TNFe; inItem: Integer): String; virtual;
    function ManterMedicamentos(aNFE: TNFe; inItem: Integer): String; virtual;
    function ManterArma(aNFE: TNFe; inItem: Integer): String; virtual;
    function ManterCombustivel(aNFE: TNFe; inItem: Integer): String; virtual;
    function ManterRastro(aNFE: TNFe; inItem: Integer): String; virtual;
    function ManterVDesc(dvDesc: currency; dVUnCom, dQCom: Double): Double; virtual;
    function ManterDocreferenciados(aNFE: TNFe): String; virtual;
    function ManterContingencia(aNFE: TNFe): String; virtual;
    function ManterVTribPerc(dVTotTrib, dVProd, dVNF: Double): Double; virtual;
    function ManterValAprox(aNFE: TNFe; inItem: Integer): String; virtual;
    function ManterColunaDesconto( Value : Double): Boolean;
    function ManterProtocolo(aNFE: TNFe): String;
    function ManterSuframa(aNFE: TNFe): String;
    function ManterPagamentos(aNFE: TNFe): String;
    function ManterInformacoesDadosAdicionais(aNFE: TNFe): String;

  published
    property ImprimeValor: TImprimirUnidQtdeValor read FImprimeValor write FImprimeValor default iuComercial;
    property ImprimeDescPorPercentual: Boolean read FImprimeDescPorPercentual write FImprimeDescPorPercentual default False;
    property ImprimeDetalhamentoEspecifico: Boolean read FImprimeDetalhamentoEspecifico write FImprimeDetalhamentoEspecifico default True;
    property ImprimeDescAcrescItem: TpcnImprimeDescAcrescItem read FImprimeDescAcrescItem write FImprimeDescAcrescItem default idaiSempre;
    property PosCanhoto: TPosRecibo read FPosCanhoto write FPosCanhoto default prCabecalho;
    property PosCanhotoLayout: TPosReciboLayout read FPosCanhotoLayout write FPosCanhotoLayout default prlPadrao;
    property ExibeResumoCanhoto: Boolean read FExibeResumoCanhoto write FExibeResumoCanhoto default True;
    property TextoResumoCanhoto: String read FTextoResumoCanhoto write FTextoResumoCanhoto;
    property ExibeCampoFatura: Boolean read FExibeCampoFatura write FExibeCampoFatura default True;
    property ExibeDadosISSQN: Boolean read FExibeDadosISSQN write FExibeDadosISSQN default False;
    property ExibeDadosDocReferenciados: Boolean read FExibeDadosDocReferenciados write FExibeDadosDocReferenciados default True;
    property ExibeDadosInscricaoSuframa: Boolean read FExibeDadosInscricaoSuframa write FExibeDadosInscricaoSuframa default True;
    property DetVeiculos: TDetVeiculos read FDetVeiculos write FDetVeiculos default [dv_chassi, dv_xCor, dv_nSerie, dv_tpComb, dv_nMotor, dv_anoMod, dv_anoFab];
    property DetMedicamentos: TDetMedicamentos read FDetMedicamentos write FDetMedicamentos default [dm_nLote, dm_qLote, dm_dFab, dm_dVal, dm_vPMC];
    property DetArmamentos: TDetArmamentos read FDetArmamentos write FDetArmamentos default [da_tpArma, da_nSerie, da_nCano, da_descr];
    property DetCombustiveis: TDetCombustiveis read FDetCombustiveis write FDetCombustiveis default [dc_cProdANP, dc_CODIF, dc_qTemp, dc_UFCons, dc_CIDE, dc_qBCProd, dc_vAliqProd, dc_vCIDE];
    property DetRastros: TDetRastros read FDetRastros write FDetRastros default [dr_nLote, dr_qLote, dr_dFab, dr_dVal, dr_cAgreg];
    property TributosPercentual: TpcnPercentualTributos read FTributosPercentual write SetTributosPercentual default ptValorProdutos;
    property TributosPercentualPersonalizado: Double read FTributosPercentualPersonalizado write SetTributosPercentualPersonalizado;
    property ExpandirDadosAdicionaisAuto: boolean read FExpandirDadosAdicionaisAuto write FExpandirDadosAdicionaisAuto default False;
    property ExibeCampoDePagamento: TpcnInformacoesDePagamento read FExibeCampoDePagamento write FExibeCampoDePagamento default eipNunca;
    property FormularioContinuo;
    property Etiqueta: Boolean read FEtiqueta write FEtiqueta default False;
    property FormatarNumeroDocumento: Boolean read FFormatarNumeroDocumento write FFormatarNumeroDocumento default True;
  end;


  { TACBrNFeDANFCEClass }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNFeDANFCEClass = class(TACBrDFeDANFeReport)
  private
    FEspacoFinal: Integer;
    FLarguraBobina: Integer;
    FImprimeDescAcrescItem: Boolean;
    FImprimeItens: Boolean;
    FViaConsumidor: Boolean;
    FvTroco: currency;
    FImprimeQRCodeLateral: Boolean;
    FImprimeLogoLateral: Boolean;
    FDescricaoPagamentos: TDescricaoPagamentos;
    FImprimeEmUmaLinha: Boolean;
    FImprimeEmDuasLinhas: Boolean;
    FFormatarNumeroDocumento : Boolean;
    procedure setImprimeEmUmaLinha(const Value: Boolean);
    procedure setImprimeEmDuasLinhas(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;

    function ManterDescricaoPagamentos(aPagto: TpagCollectionItem): String; virtual;

    property vTroco: currency read FvTroco write FvTroco;
    property ViaConsumidor: Boolean read FViaConsumidor write FViaConsumidor;
  published
    property LarguraBobina: Integer read FLarguraBobina write FLarguraBobina default 302;
    property ImprimeDescAcrescItem: Boolean read FImprimeDescAcrescItem write FImprimeDescAcrescItem default True;
    property ImprimeItens: Boolean read FImprimeItens write FImprimeItens default True;
    property ImprimeQRCodeLateral: Boolean read FImprimeQRCodeLateral write FImprimeQRCodeLateral default False;
    property ImprimeLogoLateral: Boolean read FImprimeLogoLateral write FImprimeLogoLateral default False;
    property EspacoFinal: Integer read FEspacoFinal write FEspacoFinal default 38;
    property DescricaoPagamentos: TDescricaoPagamentos read FDescricaoPagamentos write FDescricaoPagamentos default [icaTipo, icaBandeira];
    property ImprimeEmUmaLinha: Boolean read FImprimeEmUmaLinha write setImprimeEmUmaLinha default False;
    property ImprimeEmDuasLinhas: Boolean read FImprimeEmDuasLinhas write setImprimeEmDuasLinhas default False;
    property FormularioContinuo;
    property FormatarNumeroDocumento: Boolean read FFormatarNumeroDocumento write FFormatarNumeroDocumento default True;
  end;

implementation

uses
  ACBrDFeUtil, ACBrValidador,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime,
  StrUtils;

{ TACBrNFeDANFEClass }

constructor TACBrNFeDANFEClass.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FImprimeValor                    := iuComercial;
  FImprimeDetalhamentoEspecifico   := True;
  FImprimeDescAcrescItem           := idaiSempre;
  FPosCanhoto                      := prCabecalho;
  FPosCanhotoLayout                := prlPadrao;
  FExibeResumoCanhoto              := True;
  FTextoResumoCanhoto              := '';
  FImprimeDescPorPercentual        := False;
  FExibeCampoFatura                := True;
  FExibeDadosISSQN                 := False;
  FExibeDadosDocReferenciados      := True;
  FExibeDadosInscricaoSuframa      := True;
  FDetVeiculos                     := [dv_chassi, dv_xCor, dv_nSerie, dv_tpComb, dv_nMotor, dv_anoMod, dv_anoFab];
  FDetMedicamentos                 := [dm_nLote, dm_qLote, dm_dFab, dm_dVal, dm_vPMC];
  FDetArmamentos                   := [da_tpArma, da_nSerie, da_nCano, da_descr];
  FDetCombustiveis                 := [dc_cProdANP, dc_CODIF, dc_qTemp, dc_UFCons, dc_CIDE, dc_qBCProd, dc_vAliqProd, dc_vCIDE];
  FDetRastros                      := [dr_nLote, dr_qLote, dr_dFab, dr_dVal, dr_cAgreg];
  FTributosPercentual              := ptValorProdutos;
  FTributosPercentualPersonalizado := 0;
  FExpandirDadosAdicionaisAuto     := False;
  FExibeCampoDePagamento           := eipNunca;   	
  FEtiqueta                        := False;
  FFormatarNumeroDocumento         := True;
end;

procedure TACBrNFeDANFEClass.SetTributosPercentual(const AValue: TpcnPercentualTributos);
begin
  FTributosPercentual := AValue;
  if (AValue <> ptPersonalizado) then
    FTributosPercentualPersonalizado := 0;
end;

procedure TACBrNFeDANFEClass.SetTributosPercentualPersonalizado(const AValue: Double);
begin
  if (FTributosPercentual = ptPersonalizado) then
    FTributosPercentualPersonalizado := AValue
  else
    FTributosPercentualPersonalizado := 0;
end;

function TACBrNFeDANFEClass.ManterinfAdProd(aNFE: TNFe; const inItem: Integer): String;
begin
  Result := inherited ManterinfAdProd(aNFE, inItem) + 
            ManterValAprox(aNFE, inItem);

  if FImprimeDetalhamentoEspecifico then
  begin
    Result := Result +
      ManterVeiculos(aNFE, inItem) +
      ManterMedicamentos(aNFE, inItem) +
      ManterRastro(aNFE, inItem) +
      ManterArma(aNFE, inItem) +
      ManterCombustivel(aNFE, inItem);
  end;
end;

function TACBrNFeDANFEClass.ManterValAprox(aNFE: TNFe; inItem: Integer): String;
var
  TotalProduto: currency;
begin
  Result := '';
  if (inItem < 0) or (inItem >= aNFE.Det.Count) then
    Exit;

  with aNFE.Det.Items[inItem] do
  begin
    if ExibeTotalTributosItem and NaoEstaZerado(Imposto.vTotTrib) then
    begin
      Result := SeparadorDetalhamentos +'Val Aprox Tributos: ' + FormatFloatBr(Imposto.vTotTrib);

      if (FTributosPercentual = ptValorNF) then
      begin
        TotalProduto := Prod.VProd + Prod.vFrete + Prod.vOutro + Prod.vSeg +
                        Imposto.IPI.vIPI + Imposto.ICMS.vICMSST;

        if NaoEstaZerado(TotalProduto) then
          Result := Result + ' (' + FormatFloatBr((Imposto.vTotTrib * 100) / TotalProduto) + '%)';
      end
      else
        if NaoEstaZerado(Prod.VProd) then
          Result := Result + ' (' + FormatFloatBr((Imposto.vTotTrib * 100) / Prod.VProd) + '%)';
    end;
  end;
end;

function TACBrNFeDANFEClass.ManterVeiculos(aNFE: TNFe; inItem: Integer): String;
var
  sQuebraLinha: String;
begin
  Result := '';
  if (inItem < 0) or (inItem >= aNFE.Det.Count) then
    Exit;

  with aNFE.Det.Items[inItem].Prod do
  begin
    if NaoEstaVazio(veicProd.chassi) then
    begin
      sQuebraLinha := SeparadorDetalhamentos;

      Result := sQuebraLinha;
      if (dv_tpOp in FDetVeiculos) then
        Result := Result + ACBrStr('TIPO DA OPERAÇÃO: ' + VeiculosTipoOperStr(veicProd.tpOP)) + sQuebraLinha;
      if (dv_Chassi in FDetVeiculos) then
        Result := Result + 'CHASSI: ' + veicProd.chassi + sQuebraLinha;
      if (dv_cCor in FDetVeiculos) then
        Result := Result + ACBrStr('CÓDIGO DA COR: ') + veicProd.cCor + sQuebraLinha;
      if (dv_xCor in FDetVeiculos) then
        Result := Result + 'NOME DA COR: ' + veicProd.xCor + sQuebraLinha;
      if (dv_pot in FDetVeiculos) then
        Result := Result + ACBrStr('POTÊNCIA DO MOTOR: ') + veicProd.pot + sQuebraLinha;
      if (dv_cilin in FDetVeiculos) then
        Result := Result + 'CILINDRADAS: ' + veicProd.Cilin + sQuebraLinha;
      if (dv_pesoL in FDetVeiculos) then
        Result := Result + ACBrStr('PESO LÍQUIDO: ') + veicProd.pesoL + sQuebraLinha;
      if (dv_pesoB in FDetVeiculos) then
        Result := Result + 'PESO BRUTO: ' + veicProd.pesoB + sQuebraLinha;
      if (dv_nSerie in FDetVeiculos) then
        Result := Result + ACBrStr('NÚMERO DE SÉRIE: ') + veicProd.nSerie + sQuebraLinha;
      if (dv_tpComb in FDetVeiculos) then
        Result := Result + ACBrStr('COMBUSTÍVEL: ' + VeiculosCombustivelStr(veicProd.tpComb)) + sQuebraLinha;
      if (dv_nMotor in FDetVeiculos) then
        Result := Result + ACBrStr('NÚMERO DO MOTOR: ') + veicProd.nMotor + sQuebraLinha;
      if (dv_CMT in FDetVeiculos) then
        Result := Result + ACBrStr('CAP. MÁX. TRAÇÃO: ') + veicProd.CMT + sQuebraLinha;
      if (dv_dist in FDetVeiculos) then
        Result := Result + ACBrStr('DISTÂNCIA ENTRE EIXOS: ') + veicProd.dist + sQuebraLinha;
      if (dv_anoMod in FDetVeiculos) then
        Result := Result + 'ANO DO MODELO: ' + IntToStr(veicProd.anoMod) + sQuebraLinha;
      if (dv_anoFab in FDetVeiculos) then
        Result := Result + ACBrStr('ANO DE FABRICAÇÃO: ') + IntToStr(veicProd.anoFab) + sQuebraLinha;
      if (dv_tpPint in FDetVeiculos) then
        Result := Result + 'TIPO DE PINTURA: ' + veicProd.tpPint + sQuebraLinha;
      if (dv_tpVeic in FDetVeiculos) then
        Result := Result + ACBrStr('TIPO DE VEÍCULO: ' + VeiculosTipoStr(veicProd.tpVeic)) + sQuebraLinha;
      if (dv_espVeic in FDetVeiculos) then
        Result := Result + ACBrStr('ESPÉCIE DO VEÍCULO: ' + VeiculosEspecieStr(veicProd.espVeic)) + sQuebraLinha;
      if (dv_VIN in FDetVeiculos) then
        Result := Result + ACBrStr('VIN (CHASSI): ' + VeiculosVinStr(veicProd.VIN)) + sQuebraLinha;
      if (dv_condVeic in FDetVeiculos) then
        Result := Result + ACBrStr('CONDIÇÃO DO VEÍCULO: ' + VeiculosCondicaoStr(veicProd.condVeic)) + sQuebraLinha;
      if (dv_cMod in FDetVeiculos) then
        Result := Result + ACBrStr('CÓDIGO MARCA MODELO: ') + veicProd.cMod + sQuebraLinha;
      if (dv_cCorDENATRAN in FDetVeiculos) then
        Result := Result + ACBrStr('CÓDIGO COR DENATRAN: ' + VeiculosCorDENATRANSTr(veicProd.cCorDENATRAN)) + sQuebraLinha;
      if (dv_lota in FDetVeiculos) then
        Result := Result + ACBrStr('CAPACIDADE MÁXIMA DE LOTAÇÃO: ') + IntToStr(veicProd.lota) + sQuebraLinha;
      if (dv_tpRest in FDetVeiculos) then
        Result := Result + ACBrStr('RESTRIÇÃO: ' + VeiculosRestricaoStr(veicProd.tpRest)) + sQuebraLinha;
    end;
  end;
end;

function TACBrNFeDANFEClass.ManterMedicamentos(aNFE: TNFe; inItem: Integer): String;
var
  i: Integer;
  sQuebraLinha: String;
begin
  Result := '';
  if (inItem < 0) or (inItem >= aNFE.Det.Count) then
    Exit;

  with aNFE.Det.Items[inItem].Prod do
  begin
    if (med.Count > 0) then
    begin
      sQuebraLinha := SeparadorDetalhamentos;

      Result := sQuebraLinha;
      for i := 0 to med.Count - 1 do
      begin
        if (aNFE.infNFe.Versao >= 4) then
          Result := Result + 'C.P. ANVISA ' + med.Items[i].cProdANVISA + sQuebraLinha
        else
        begin
          if (dm_nLote in FDetMedicamentos) then
            Result := Result + 'LOTE: ' + med.Items[i].nLote + sQuebraLinha;
          if (dm_qLote in FDetMedicamentos) then
            Result := Result + 'QTD: ' + FormatFloatBr(med.Items[i].qLote, FloatMask(3)) + sQuebraLinha;
          if (dm_dFab in FDetMedicamentos) then
            Result := Result + 'FAB: ' + DateToStr(med.Items[i].dFab) + sQuebraLinha;
          if (dm_dVal in FDetMedicamentos) then
            Result := Result + 'VAL: ' + DateToStr(med.Items[i].dVal) + sQuebraLinha;
        end;

        if (dm_vPMC in FDetMedicamentos) then
          if (med.Items[i].vPMC > 0) then
            Result := Result + 'PMC: R$' + FormatFloatBr(med.Items[i].vPMC) + sQuebraLinha;
      end;
    end;
  end;
end;

function TACBrNFeDANFEClass.ManterArma(aNFE: TNFe; inItem: Integer): String;
var
  i: Integer;
  sQuebraLinha: String;
begin
  Result := '';
  if (inItem < 0) or (inItem >= aNFE.Det.Count) then
    Exit;

  with aNFE.Det.Items[inItem].Prod do
  begin
    if (arma.Count > 0) then
    begin
      sQuebraLinha := SeparadorDetalhamentos;

      Result := sQuebraLinha;
      for i := 0 to arma.Count - 1 do
      begin
        if (da_tpArma in FDetArmamentos) then
          Result := Result + ACBrStr('TIPO DE ARMA: ' + ArmaTipoStr(arma.Items[i].tpArma)) + sQuebraLinha;
        if (da_nSerie in FDetArmamentos) then
          Result := Result + ACBrStr('No. SÉRIE ARMA: ') + arma.Items[i].nSerie + sQuebraLinha;
        if (da_nCano in FDetArmamentos) then
          Result := Result + ACBrStr('No. SÉRIE CANO: ') + arma.Items[i].nCano + sQuebraLinha;
        if (da_descr in FDetArmamentos) then
          Result := Result + ACBrStr('DESCRIÇÃO ARMA: ') + arma.Items[i].descr + sQuebraLinha;
      end;
    end;
  end;
end;

function TACBrNFeDANFEClass.ManterCombustivel(aNFE: TNFe; inItem: Integer): String;
var
  sQuebraLinha: String;
begin
  Result := '';
  if (inItem < 0) or (inItem >= aNFE.Det.Count) then
    Exit;

  with aNFE.Det.Items[inItem].Prod do
  begin
    if (comb.cProdANP > 0) then
    begin
      sQuebraLinha := SeparadorDetalhamentos;

      Result := sQuebraLinha;
      if (dc_cProdANP in FDetCombustiveis) then
        Result := Result + ACBrStr('CÓD. PRODUTO ANP: ') + IntToStr(comb.cProdANP) + sQuebraLinha;

      if (dc_CODIF in FDetCombustiveis) then
        if NaoEstaVazio(comb.CODIF) then
          Result := Result + ACBrStr('AUTORIZAÇÃO/CODIF: ') + comb.CODIF + sQuebraLinha;

      if (dc_qTemp in FDetCombustiveis) then
        if (comb.qTemp > 0) then
          Result := Result + ACBrStr('QTD. FATURADA TEMP. AMBIENTE: ') + FormatFloatBr(comb.qTemp, FloatMask(4)) + sQuebraLinha;

      if (dc_UFCons in FDetCombustiveis) then
        Result := Result + ACBrStr('UF DE CONSUMO: ') + comb.UFcons + sQuebraLinha;

      if (comb.CIDE.qBCProd > 0) then
      begin
        if (dc_qBCProd in FDetCombustiveis) then
          Result := Result + ACBrStr('BASE DE CÁLCULO CIDE: ') + FormatFloatBr(comb.CIDE.qBCProd, FloatMask(4)) + sQuebraLinha;
        if (dc_vAliqProd in FDetCombustiveis) then
          Result := Result + ACBrStr('ALÍQUOTA CIDE: ') + FormatFloatBr(comb.CIDE.vAliqProd, FloatMask(4)) + sQuebraLinha;
        if (dc_vCIDE in FDetCombustiveis) then
          Result := Result + ACBrStr('VALOR CIDE: ') + FormatFloatBr(comb.CIDE.vCIDE) + sQuebraLinha;
      end;

      if (comb.encerrante.nBico > 0) then
      begin
        Result := Result + 'ENCERRANTE' + sQuebraLinha;
        Result := Result + 'BICO: ' + IntToStr(comb.encerrante.nBico) + sQuebraLinha;
        if comb.encerrante.nBomba > 0 then
          Result := Result + 'BOMBA: ' + IntToStr(comb.encerrante.nBomba) + sQuebraLinha;
        Result := Result + 'TANQUE: ' + IntToStr(comb.encerrante.nTanque) + sQuebraLinha;
        Result := Result + ACBrStr('NO INÍCIO: ') + FormatFloatBr(comb.encerrante.vEncIni, FloatMask(3)) + sQuebraLinha;
        Result := Result + 'NO FINAL: ' + FormatFloatBr(comb.encerrante.vEncFin, FloatMask(3)) + sQuebraLinha;
      end;
    end;
  end;
end;

function TACBrNFeDANFEClass.ManterRastro(aNFE: TNFe; inItem: Integer): String;
var
  i: Integer;
  sQuebraLinha: String;
begin
  Result := '';
  if (inItem < 0) or (inItem >= aNFE.Det.Count) then
    Exit;

  if FDetRastros = [] then
    Exit;

  with aNFE.Det.Items[inItem].Prod do
  begin
    if (Rastro.Count > 0) then
    begin
      sQuebraLinha := SeparadorDetalhamentos;

      Result := sQuebraLinha;

      for i := 0 to Rastro.Count - 1 do
      begin
        if (dr_nLote in FDetRastros) then
          Result := Result + 'LOTE: ' + rastro.Items[i].nLote + sQuebraLinha;

        if (dr_qLote in FDetRastros) then
          Result := Result + 'QTD: ' + FormatFloatBr(rastro.Items[i].qLote) + sQuebraLinha;

        if (dr_dFab in FDetRastros) then
          Result := Result + 'FAB: ' + FormatDateBr(rastro.Items[i].dFab) + sQuebraLinha;

        if (dr_dVal in FDetRastros) then
          Result := Result + 'VAL: ' + FormatDateBr(rastro.Items[i].dVal) + sQuebraLinha;

        if (dr_cAgreg in FDetRastros) and NaoEstaVazio(rastro.Items[i].cAgreg) then
          Result := Result + ACBrStr('C.AGREGAÇÃO: ') + rastro.Items[i].cAgreg + sQuebraLinha;
      end;
    end;
  end;
end;

function TACBrNFeDANFEClass.ManterVDesc(dvDesc: currency; dVUnCom, dQCom: Double): Double;
begin
  if (FImprimeDescPorPercentual) then
  begin
    if (dvDesc > 0) and (dVUnCom > 0) and (dQCom > 0) then
      Result := ((dvDesc * 100) / (dVUnCom * dQCom))
    else
      Result := 0;
  end
  else
    Result := dvDesc;
end;

function TACBrNFeDANFEClass.ManterDocreferenciados(aNFE: TNFe): String;

  function DescrModeloNFe(chave: String): String;
  begin
    case StrToIntDef(Copy(chave, 21, 2), 0) of
      59: Result := 'CFe-SAT Ref.:';
      65: Result := 'NFCe Ref.:';
    else
      Result := 'NFe Ref.:';
    end;
  end;

  function MontaLadoALado(bExecuta: Boolean; sResult: String; sInicio: String; sString: String): String;
  begin
    if bExecuta then
    begin
      if sResult = '' then
        Result := sInicio
      else
      begin
        if pos(sInicio, sResult) = 0 then
          Result := sResult + ', ' + sInicio
        else
          Result := sResult + ', ';
      end;

      Result := Result + '(' + sString + ')';
    end
    else
      Result := sResult;
  end;

var
  i: Integer;
  sQuebraLinha: String;
begin
  Result := '';
  if (not ExibeDadosDocReferenciados) or (ANFe.Ide.NFref.Count < 1) then
    Exit;

  sQuebraLinha := SeparadorDetalhamentos;

  for i := 0 to (ANFe.ide.NFref.Count - 1) do
  begin
    Result := MontaLadoALado(NaoEstaVazio(ANFe.ide.NFref[i].refNFe),
      Result,
      DescrModeloNFe(ANFe.ide.NFref[i].refNFe),
      FormatarChaveAcesso(ANFe.ide.NFref[i].refNFe));

    Result := MontaLadoALado(NaoEstaVazio(ANFe.ide.NFref[i].refCTe),
      Result,
      'CTe Ref.:',
      FormatarChaveAcesso(ANFe.ide.NFref[i].refCTe));

    Result := MontaLadoALado((ANFe.ide.NFref[i].RefECF.modelo <> ECFModRefVazio),
      Result,
      'ECF Ref.:',
      'modelo: ' + ECFModRefToStr(ANFe.ide.NFref[i].RefECF.modelo) +
      ' ECF: ' + ANFe.ide.NFref[i].RefECF.nECF +
      ' COO: ' + ANFe.ide.NFref[i].RefECF.nCOO);

    Result := MontaLadoALado(NaoEstaVazio(ANFe.ide.NFref[i].RefNF.CNPJ),
      Result,
      'NF Ref.:',
      ACBrStr('série: ') + IntToStr(ANFe.ide.NFref[i].RefNF.serie) +
      ACBrStr(' número: ') + IntToStr(ANFe.ide.NFref[i].RefNF.nNF) +
      ' emit: ' + FormatarCNPJouCPF(ANFe.ide.NFref[i].RefNF.CNPJ) +
      ' modelo: ' + IntToStr(ANFe.ide.NFref[i].RefNF.modelo));

    Result := MontaLadoALado((ANFe.ide.NFref[i].RefNFP.nNF > 0),
      Result,
      'NFP Ref.:',
      ACBrStr('série: ') + IntToStr(ANFe.ide.NFref[i].RefNFP.serie) +
      ACBrStr(' número: ') + IntToStr(ANFe.ide.NFref[i].RefNFP.nNF) +
      ' modelo: ' + ANFe.ide.NFref[i].RefNFP.modelo +
      ' emit: ' + FormatarCNPJouCPF(ANFe.ide.NFref[i].RefNFP.CNPJCPF) +
      ' IE: ' + ANFe.ide.NFref[i].RefNFP.IE +
      ' UF: ' + CUFtoUF(ANFe.ide.NFref[i].RefNFP.cUF));
  end;

  if (Result <> '') then
    Result := Result + sQuebraLinha;
end;

function TACBrNFeDANFEClass.ManterContingencia(aNFE: TNFe): String;
begin
  Result := '';

  case aNFE.Ide.tpEmis of
    teOffLine, teContingencia, teFSDA, teSCAN, teSVCAN, teSVCRS, teSVCSP:
      Result := ACBrStr('DANFE EM CONTINGÊNCIA, IMPRESSO EM DECORRÊNCIA DE PROBLEMAS TÉCNICOS;');

    teDPEC:
      Result :=
        ACBrStr('DANFE IMPRESSO EM CONTINGÊNCIA - DPEC REGULARMENTE RECEBIDA PELA RECEITA FEDERAL DO BRASIL;') +
        ACBrStr('DATA/HORA INÍCIO: ') + IfThen(aNFE.ide.dhCont = 0, ' ', DateTimeToStr(aNFE.ide.dhCont)) + ';' +
        ACBrStr('MOTIVO CONTINGÊNCIA: ') + IfThen(EstaVazio(aNFE.ide.xJust), ' ', aNFE.ide.xJust) + ';';
  end;
end;

function TACBrNFeDANFEClass.ManterVTribPerc(dVTotTrib, dVProd, dVNF: Double): Double;
begin
  Result := 0;

  case FTributosPercentual of
    ptPersonalizado:
      Result := TributosPercentualPersonalizado;

    ptValorProdutos:
    begin
      if (dVProd > 0) then
        Result := dVTotTrib * 100 / dVProd;
    end;

    ptValorNF:
    begin
      if (dVNF > 0) then
        Result := dVTotTrib * 100 / dVNF;
    end;
  end;
end;

{ TACBrNFeDANFCEClass }

constructor TACBrNFeDANFCEClass.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLarguraBobina         := 302;
  FImprimeDescAcrescItem := True;
  FImprimeItens          := True;
  FViaConsumidor         := False;
  FvTroco                := 0;
  FImprimeQRCodeLateral  := False;
  FImprimeLogoLateral    := False;
  FEspacoFinal           := 38;
  FDescricaoPagamentos   := [icaTipo, icaBandeira];
  FImprimeEmUmaLinha     := False;
  FImprimeEmDuasLinhas   := False;
  FFormatarNumeroDocumento := True;
end;

function TACBrNFeDANFCEClass.ManterDescricaoPagamentos(aPagto: TpagCollectionItem
  ): String;
var
  LDescBandeira,
  LCodigoAutorizacao: String;
begin
  Result := '';

  try
    if (aPagto.tBand >= Low(TpcnBandeiraCartao)) and (aPagto.tBand <= High(TpcnBandeiraCartao)) then
      LDescBandeira:= BandeiraCartaoToDescStr(aPagto.tBand);

    if aPagto.cAut <>'' then
      LCodigoAutorizacao := '- Aut: ' + aPagto.cAut;

    if (icaTipo in FDescricaoPagamentos) then
      Result:= ACBrStr(FormaPagamentoToDescricao(aPagto.tPag, aPagto.xPag)) + Space(1);
    if (icaBandeira in FDescricaoPagamentos) then
      Result := Result + LDescBandeira + Space(1);
    if (icaAutorizacao in FDescricaoPagamentos) then
      Result := Result + LCodigoAutorizacao;
  except
    Result:= ACBrStr(FormaPagamentoToDescricao(aPagto.tPag, aPagto.xPag)) + Space(1);
  end;
end;

procedure TACBrNFeDANFCEClass.setImprimeEmDuasLinhas(const Value: Boolean);
begin
  if Value = FImprimeEmDuasLinhas then Exit;

  FImprimeEmDuasLinhas := Value;
  if Value then
  begin
    FImprimeEmUmaLinha := False;
  end;
end;

procedure TACBrNFeDANFCEClass.setImprimeEmUmaLinha(const Value: Boolean);
begin
  if Value = FImprimeEmUmaLinha then Exit;

  FImprimeEmUmaLinha := Value;
  if Value then
  begin
    FImprimeEmDuasLinhas := False;
  end;
end;


function TACBrNFeDANFEClass.ManterColunaDesconto(Value: Double): Boolean;
begin
  //Por padrão a configuração atual é idaiSempre.
  Result := True;
  // idaiSempre    => Sempre apresentar a coluna desconto
  // idaiNunca     => Nunca apresenta a coluna desconto
  // idaiComValor  => Apresentar a coluna desconto se value > 0 ( desconto )

  case fImprimeDescAcrescItem of
    idaiSempre    : Result := True;
    idaiNunca     : Result := False;
    idaiComValor  : Result := ( value > 0 );
  end;
end;
function TACBrNFeDANFEClass.ManterProtocolo(aNFE: TNFe): String;
begin
  // Protocolo de autorização, nos casos de emissão em contingência
  if (aNFe.Ide.tpEmis in [teContingencia, teFSDA]) and (aNFe.procNFe.cStat = 100) then
  begin
    Result := ACBrStr('PROTOCOLO DE AUTORIZAÇÃO DE USO: ') +
      aNFe.procNFe.nProt + ' ' + FormatDateTimeBr(aNFe.procNFe.dhRecbto) + ';';
  end
  else
    Result := '';
end;

function TACBrNFeDANFEClass.ManterSuframa(aNFE: TNFe): String;
begin
  // Inscrição Suframa
  if NaoEstaVazio(aNFe.Dest.ISUF) and (FExibeDadosInscricaoSuframa) then
  begin
    Result := ACBrStr('INSCRIÇÃO SUFRAMA: ') + aNFe.Dest.ISUF + ';';
  end
  else
    Result := '';
end;

function TACBrNFeDANFEClass.ManterInformacoesDadosAdicionais( aNFE: TNFe): String;
begin
  Result := ManterProtocolo( aNFE ) +
            ManterSuframa( aNFE ) +
            ManterPagamentos(aNFE) +
            ManterDocreferenciados(aNFE) +
            ManterInfAdFisco(aNFE) +
            ManterObsFisco(aNFE) +
            ManterProcreferenciado(aNFE) +
            ManterInfContr(aNFE) +
            ManterInfCompl(aNFE) +
            ManterContingencia(aNFE);

  Result := FastStringReplace(Result, ';', sLineBreak, [rfReplaceAll]);
end;

function TACBrNFeDANFEClass.ManterPagamentos(aNFE: TNFe): String;
  function MontaLadoALado(sResult: String; sInicio: String; sString: String): String;
  begin
    if sResult = '' then
      Result := sInicio
    else
    begin
      if pos(sInicio, sResult) = 0 then
      Result := sResult + ', ' + sInicio
      else
      Result := sResult + ', ';
    end;
    Result := Result + sString ;
  end;
var
  i: Integer;
  lTemp,
  lQuebraLinha: String;
begin
  Result := '';
  if (not ( FExibeCampoDePagamento = eipAdicionais) ) or (ANFe.pag.Count < 1) then
    Exit;

  lQuebraLinha := SeparadorDetalhamentos;

  for i := 0 to (ANFe.pag.Count - 1) do
  begin
    lTemp :=  ACBrStr(
                FormaPagamentoToDescricao( ANFe.pag.Items[i].tPag, ANFe.pag.Items[i].xPag)
                ) + ' R$'+
                FormatFloatBr( ANFe.pag.Items[i].vPag);

    Result := MontaLadoALado(Result, 'Pagamento(s): (', lTemp );
  end;

  if (Result <> '') then
    Result := Result+ ')' + lQuebraLinha;
end;

end.
