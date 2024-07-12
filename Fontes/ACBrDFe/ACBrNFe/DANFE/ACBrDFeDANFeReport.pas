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

unit ACBrDFeDANFeReport;

interface

uses
  Classes, SysUtils,
  ACBrBase, ACBrDFeReport,
  pcnNFe, pcnConversao;

type
  TpcnTributos = (trbNenhum, trbNormal, trbSeparadamente);
  TinfAdcProd = (infNenhum, infDescricao, infSeparadamente);
  TValorLiquido = (TVLFrete,TVLDesconto, TVLOutros, TVLSeguros);
  TValorLiquidoFlag = Set of TValorLiquido;

  { TACBrDFeDANFeReport }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}

  TACBrDFeDANFeReport = class(TACBrDFeReport)
  private
    FACBrNFe: TComponent;
    FImprimeTotalLiquido: Boolean;
    FProtocoloNFe: String;
    FNFeCancelada: Boolean;
    FImprimeCodigoEan: Boolean;
    FImprimeInfContr: Boolean;
    FExibeICMSDesoneradoComoDesconto: Boolean;
    FvTribFed: currency;
    FvTribEst: currency;
    FvTribMun: currency;
    FFonteTributos: String;
    FChaveTributos: String;
    FImprimeTributos: TpcnTributos;
    FQuebraLinhaEmDetalhamentos: Boolean;
    FExibeTotalTributosItem: Boolean;
    FExibeInforAdicProduto: TinfAdcProd;
    FImprimeNomeFantasia: Boolean;
    FTipoDANFE: TpcnTipoImpressao;
    FImprimeXPedNItemPed : Boolean;

    procedure SetACBrNFE(const AValue: TComponent);
    procedure ErroAbstract(const NomeProcedure: String);

  protected


    function GetSeparadorPathPDF(const aInitialPath: String): String; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure SetTipoDANFE(AValue: TpcnTipoImpressao); virtual;

  public
    FIndexImpressaoIndividual : Integer;
    constructor Create(AOwner: TComponent); override;

    procedure ImprimirDANFE(ANFe: TNFe = nil); virtual;
    procedure ImprimirDANFECancelado(ANFe: TNFe = nil); virtual;
    procedure ImprimirDANFEResumido(ANFe: TNFe = nil); virtual;
    procedure ImprimirDANFEPDF(ANFe: TNFe = nil); overload; virtual;
    procedure ImprimirDANFEPDF(AStream: TStream; ANFe: TNFe = nil); overload; virtual;
    procedure ImprimirDANFEResumidoPDF(ANFe: TNFe = nil); overload; virtual;
    procedure ImprimirDANFEResumidoPDF(AStream: TStream; ANFe: TNFe = nil); overload; virtual;
    procedure ImprimirEVENTO(ANFe: TNFe = nil); virtual;
    procedure ImprimirEVENTOPDF(ANFe: TNFe = nil); overload; virtual;
    procedure ImprimirEVENTOPDF(AStream: TStream; ANFe: TNFe = nil); overload; virtual;
    procedure ImprimirINUTILIZACAO(ANFe: TNFe = nil); virtual;
    procedure ImprimirINUTILIZACAOPDF(ANFe: TNFe = nil); overload; virtual;
    procedure ImprimirINUTILIZACAOPDF(AStream: TStream; ANFe: TNFe = nil); overload; virtual;

    function SeparadorDetalhamentos: String; virtual;
    function ManterCodigo(const scEAN, scProd: String): String; virtual;
    function ManterNomeImpresso(const sXNome, sXFant: String): String; virtual;
    function ManterXProd(aNFE: TNFe; const inItem: Integer): String;
    function ManterUnidades(const sUCom, sUTrib: String): String; virtual;
    function ManterQuantidades(dQCom, dQTrib: Double): String; virtual;
    function ManterValoresUnitarios(dVCom, dVTrib: Double): String; virtual;
    function ManterInfAdFisco(ANFe: TNFe): String; virtual;
    function TrataDocumento(const sCNPJCPF: String): String; virtual;
    function ManterinfAdProd(aNFE: TNFe; const inItem: Integer): String; virtual;
    function ManterXPedNItemPed(aNFE: TNFe; const inItem: Integer): String; virtual;
    function ManterInfCompl(ANFe: TNFe): String; virtual;
    function ManterInfContr(ANFe: TNFe): String; virtual;
    function ManterObsFisco(ANFe: TNFe): String; virtual;
    function ManterProcreferenciado(ANFe: TNFe): String; virtual;
    function ManterVprod(dVProd, dvDesc: Double): String; virtual;
    function ManterCst(dCRT: TpcnCRT; dCSOSN: TpcnCSOSNIcms; dCST: TpcnCSTIcms): String; virtual;
    function ManterdvTotTrib(dvTotTrib: Double): String; virtual;
    function CalcularValorLiquidoItem(const ANFE: TNFe;  const ANItem: Integer; ATipoCalculo: TValorLiquidoFlag): Double; overload;
    function CalcularValorLiquidoItem(const ANFE: TNFe; const ANItem: Integer):Double;overload;
    function CalcularValorDescontoItem(const ANFE: TNFe; const ANItem: Integer):Double;
    function CalcularValorDescontoTotal(const ANFE: TNFe):Double;
  public
    property Protocolo: String read FProtocoloNFe write FProtocoloNFe;
    property Cancelada: Boolean read FNFeCancelada write FNFeCancelada default False;
    property vTribFed: currency read FvTribFed write FvTribFed;
    property vTribEst: currency read FvTribEst write FvTribEst;
    property vTribMun: currency read FvTribMun write FvTribMun;
    property FonteTributos: String read FFonteTributos write FFonteTributos;
    property ChaveTributos: String read FChaveTributos write FChaveTributos;

  published
    property ACBrNFe: TComponent read FACBrNFe write SetACBrNFE;
    property TipoDANFE: TpcnTipoImpressao read FTipoDANFE write SetTipoDANFE default tiRetrato;
    property QuebraLinhaEmDetalhamentos: Boolean read FQuebraLinhaEmDetalhamentos write FQuebraLinhaEmDetalhamentos default True;
    property ImprimeTotalLiquido: Boolean read FImprimeTotalLiquido write FImprimeTotalLiquido default False;
    property ImprimeTributos: TpcnTributos read FImprimeTributos write FImprimeTributos default trbNormal;
    property ExibeICMSDesoneradoComoDesconto: Boolean Read FExibeICMSDesoneradoComoDesconto write FExibeICMSDesoneradoComoDesconto default False;
    property ExibeTotalTributosItem: Boolean read FExibeTotalTributosItem write FExibeTotalTributosItem default False;
    property ExibeInforAdicProduto: TinfAdcProd read FExibeInforAdicProduto write FExibeInforAdicProduto default infDescricao;
    property ImprimeCodigoEan: Boolean read FImprimeCodigoEan write FImprimeCodigoEan default False;
    property ImprimeInfContr: Boolean read FImprimeInfContr write FImprimeInfContr default True;
    property ImprimeNomeFantasia: Boolean read FImprimeNomeFantasia write FImprimeNomeFantasia default False;
    property ImprimeXPedNItemPed : Boolean read FImprimeXPedNItemPed write FImprimeXPedNItemPed default False;
  end;

implementation

uses
  ACBrNFe,
  ACBrUtil.Base, ACBrUtil.Strings,
  ACBrValidador,
  StrUtils;

{ TACBrDFeDANFeReport }

function TACBrDFeDANFeReport.CalcularValorLiquidoItem(const ANFE: TNFe;  const ANItem: Integer; ATipoCalculo: TValorLiquidoFlag): Double;
var LProd : TProd;
  LICMSDesonerado, LOutros, LFrete, LDesconto : Double;
begin
  Result := 0;
  if (ANItem < 0) or (ANItem >= ANFe.Det.Count) then
    Exit;
  LProd := ANFe.Det.Items[ ANItem ].Prod;

  LFrete := 0;
  LDesconto := 0;
  LOutros := 0;
  LICMSDesonerado := 0;

  if ExibeICMSDesoneradoComoDesconto then
    LICMSDesonerado := ANFe.Det.Items[ ANItem ].Imposto.ICMS.vICMSDeson;

  if (TVLFrete in ATipoCalculo) or (TVLSeguros in ATipoCalculo) then
      LFrete := LProd.vFrete + LProd.vSeg;

  if TVLDesconto in ATipoCalculo then
      LDesconto := LICMSDesonerado + LProd.vDesc;

  if TVLOutros in ATipoCalculo then
      LOutros := LProd.vOutro;

  Result := LProd.vProd - LDesconto + LOutros + LFrete;

end;

constructor TACBrDFeDANFeReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FACBrNFe                    := nil;
  FTipoDANFE                  := tiRetrato;
  FImprimeTotalLiquido        := False;
  FProtocoloNFe               := '';
  FNFeCancelada               := False;
  FvTribFed                   := 0.0;
  FvTribEst                   := 0.0;
  FvTribMun                   := 0.0;
  FFonteTributos              := '';
  FChaveTributos              := '';
  FImprimeTributos            := trbNormal;
  FExibeTotalTributosItem     := False;
  FImprimeCodigoEan           := False;
  FImprimeInfContr            := True;
  FImprimeNomeFantasia        := False;
  FExibeInforAdicProduto      := infDescricao;
  FQuebraLinhaEmDetalhamentos := True;
  FExibeICMSDesoneradoComoDesconto := False;
end;

procedure TACBrDFeDANFeReport.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FACBrNFe <> nil) and (AComponent is TACBrNFe) then
    FACBrNFe := nil;
end;

procedure TACBrDFeDANFeReport.SetACBrNFE(const AValue: TComponent);
var
  OldValue: TACBrNFe;
begin
  if AValue <> FACBrNFe then
  begin
    if AValue <> nil then
      if not (AValue is TACBrNFe) then
        raise EACBrNFeException.Create('ACBrDANFE.NFE deve ser do tipo TACBrNFe');

    if Assigned(FACBrNFe) then
      FACBrNFe.RemoveFreeNotification(Self);

    OldValue := TACBrNFe(FACBrNFe);   // Usa outra variavel para evitar Loop Infinito
    FACBrNFe := AValue;                // na remoção da associação dos componentes

    if Assigned(OldValue) then
      if Assigned(OldValue.DANFE) then
        OldValue.DANFE := nil;

    if AValue <> nil then
    begin
      AValue.FreeNotification(self);
      TACBrNFe(AValue).DANFE := self;
    end;
  end;
end;

procedure TACBrDFeDANFeReport.ErroAbstract(const NomeProcedure: String);
begin
  raise EACBrNFeException.Create(NomeProcedure + ' não implementado em: ' + ClassName);
end;

procedure TACBrDFeDANFeReport.SetTipoDANFE(AValue: TpcnTipoImpressao);
begin
  if (AValue <> FTipoDANFE) then
    FTipoDANFE := AValue;
end;

function TACBrDFeDANFeReport.GetSeparadorPathPDF(const aInitialPath: String): String;
var
  dhEmissao: TDateTime;
  wLiteral, DescricaoModelo: String;
  ANFe: TNFe;
begin
  Result := aInitialPath;

  if Assigned(ACBrNFe) then  // Se tem o componente ACBrNFe
  begin
    if TACBrNFe(ACBrNFe).NotasFiscais.Count > 0 then  // Se tem alguma Nota carregada
    begin
      if (TACBrNFe(ACBrNFe).DANFE.ClassName = 'TACBrNFeDANFCEFR') or
         (TACBrNFe(ACBrNFe).DANFE.ClassName = 'TACBrNFeDANFEFR') then
        ANFe := TACBrNFe(ACBrNFe).NotasFiscais[FIndexImpressaoIndividual - 1].NFe
      else
        ANFe := TACBrNFe(ACBrNFe).NotasFiscais[FIndexImpressaoIndividual].NFe;

      if TACBrNFe(ACBrNFe).Configuracoes.Arquivos.EmissaoPathNFe then
        dhEmissao := ANFe.Ide.dEmi
      else
        dhEmissao := Now;

      DescricaoModelo := '';
      case ANFe.Ide.modelo of
        0: DescricaoModelo := TACBrNFe(FACBrNFe).GetNomeModeloDFe;
        55: DescricaoModelo := 'NFe';
        65: DescricaoModelo := 'NFCe';
      end;

      wLiteral := '';
      if TACBrNFe(ACBrNFe).Configuracoes.Arquivos.AdicionarLiteral then
        wLiteral := DescricaoModelo;

      Result := TACBrNFe(FACBrNFe).Configuracoes.Arquivos.GetPath(Result,
        wLiteral, ANFe.Emit.CNPJCPF, ANFe.Emit.IE, dhEmissao, DescricaoModelo);
    end;
  end;
end;

procedure TACBrDFeDANFeReport.ImprimirDANFE(ANFe: TNFe = nil);
begin
  ErroAbstract('ImprimirDANFE');
end;

procedure TACBrDFeDANFeReport.ImprimirDANFECancelado(ANFe: TNFe);
begin
  Cancelada := True;
  try
    ImprimirDANFE(ANFe);
  finally
    Cancelada := False;
  end;
end;

procedure TACBrDFeDANFeReport.ImprimirDANFEResumido(ANFe: TNFe = nil);
begin
  ErroAbstract('ImprimirDANFEResumido');
end;

procedure TACBrDFeDANFeReport.ImprimirDANFEPDF(ANFe: TNFe = nil);
begin
  ErroAbstract('ImprimirDANFEPDF');
end;

procedure TACBrDFeDANFeReport.ImprimirDANFEPDF(AStream: TStream; ANFe: TNFe = nil);
begin
  ErroAbstract('ImprimirDANFEPDF');
end;

procedure TACBrDFeDANFeReport.ImprimirDANFEResumidoPDF(ANFe: TNFe);
begin
  ErroAbstract('ImprimirDANFEResumidoPDF');
end;

procedure TACBrDFeDANFeReport.ImprimirDANFEResumidoPDF(AStream: TStream; ANFe: TNFe);
begin
  ErroAbstract('ImprimirDANFEResumidoPDF');
end;

procedure TACBrDFeDANFeReport.ImprimirEVENTO(ANFe: TNFe);
begin
  ErroAbstract('ImprimirEVENTO');
end;

procedure TACBrDFeDANFeReport.ImprimirEVENTOPDF(ANFe: TNFe);
begin
  ErroAbstract('ImprimirEVENTOPDF');
end;

procedure TACBrDFeDANFeReport.ImprimirEVENTOPDF(AStream: TStream; ANFe: TNFe);
begin
  ErroAbstract('ImprimirEVENTOPDF');
end;

procedure TACBrDFeDANFeReport.ImprimirINUTILIZACAO(ANFe: TNFe);
begin
  ErroAbstract('ImprimirINUTILIZACAO');
end;

procedure TACBrDFeDANFeReport.ImprimirINUTILIZACAOPDF(ANFe: TNFe);
begin
  ErroAbstract('ImprimirINUTILIZACAOPDF');
end;

procedure TACBrDFeDANFeReport.ImprimirINUTILIZACAOPDF(AStream: TStream; ANFe: TNFe);
begin
  ErroAbstract('ImprimirINUTILIZACAOPDF');
end;

function TACBrDFeDANFeReport.SeparadorDetalhamentos: String;
begin
  if FQuebraLinhaEmDetalhamentos then
    Result := sLineBreak
  else
    Result := ' - ';
end;

function TACBrDFeDANFeReport.ManterCodigo(const scEAN, scProd: String): String;
begin

  Result := Trim(scEAN);

  if not  ( ( fImprimeCodigoEan )    and
            ( Result <> 'SEM GTIN' ) and
            ( Result <>  '' )
          ) then
      Result := Trim(scProd);
end;

function TACBrDFeDANFeReport.ManterNomeImpresso(const sXNome, sXFant: String): String;
begin
  if (FImprimeNomeFantasia) and (sXFant <> '') then
    Result := sXFant
  else
    Result := sXNome;
end;

function TACBrDFeDANFeReport.ManterQuantidades(dQCom, dQTrib: Double): String;
begin
  Result := FormatarQuantidade(dQCom);
  if dQTrib > 0 then
    Result := Result + sLineBreak + FormatarQuantidade(dQTrib);
end;

function TACBrDFeDANFeReport.ManterUnidades(const sUCom, sUTrib: String): String;
begin
  Result := Trim(sUCom);
  if Trim(sUTrib) <> '' then
    Result := Result + sLineBreak + Trim(sUTrib);
end;

function TACBrDFeDANFeReport.ManterValoresUnitarios(dVCom, dVTrib: Double): String;
begin
  Result := FormatarValorUnitario(dVCom);
  if dVTrib > 0 then
    Result := Result + sLineBreak + FormatarValorUnitario(dVTrib);
end;

function TACBrDFeDANFeReport.TrataDocumento(const sCNPJCPF: String): String;
begin
  Result := sCNPJCPF;
  if NaoEstaVazio(Result) then
  begin
    if Length(Result) = 14 then
      Result := ' CNPJ: '
    else
      Result := ' CPF: ';

    Result := Result + FormatarCNPJouCPF(sCNPJCPF);
  end;
end;

function TACBrDFeDANFeReport.ManterInfAdFisco(ANFe: TNFe): String;
  // Informações de interesse do fisco
var
  infAdFisco: string;
begin
  Result := '';
  infAdFisco := ANFe.InfAdic.infAdFisco;

  if infAdFisco > '' then
  begin
    if ANFe.InfAdic.infCpl > '' then
      Result := infAdFisco + IfThen(Copy(infAdFisco, Length(infAdFisco), 1) = ';', '', '; ')
    else
      Result := infAdFisco;
  end;
end;

function TACBrDFeDANFeReport.ManterinfAdProd(aNFE: TNFe; const inItem: Integer): String;
var
  sQuebraLinha: String;
begin
  Result := '';

  if (ExibeInforAdicProduto = infNenhum) or
     (inItem < 0) or
     (inItem >= aNFE.Det.Count) then
    Exit;

  if (ExibeInforAdicProduto = infDescricao)  then
  begin
    Result := Trim(aNFE.Det.Items[inItem].infAdProd);

    sQuebraLinha := SeparadorDetalhamentos;
    Result := StringReplace(Result, ';', sQuebraLinha, [rfReplaceAll, rfIgnoreCase]);
    if (Result <> '') then
      Result := sQuebraLinha + Result;
  end;
end;

function TACBrDFeDANFeReport.ManterInfCompl(ANFe: TNFe): String;
  // Informações de interesse do contribuinte
begin
  Result := '';
  if ANFe.InfAdic.infCpl > '' then
    Result := ANFe.InfAdic.infCpl;
end;

function TACBrDFeDANFeReport.ManterInfContr(ANFe: TNFe): String;
  // Informações de uso livre do contribuinte com "xCampo" e "xTexto"
var
  i: Integer;
begin
  Result := '';
  if FImprimeInfContr then
  begin
    with ANFe.InfAdic do
    begin
      if obsCont.Count > 0 then
      begin
        for i := 0 to (obsCont.Count - 1) do
        begin
          Result := Result +
            obsCont.Items[i].xCampo + ': ' +
            obsCont.Items[i].xTexto +
            IfThen((i = (obsCont.Count - 1)), '', ';');
        end;

        Result := Result + '; ';
      end;
    end;
  end;
end;

function TACBrDFeDANFeReport.ManterObsFisco(ANFe: TNFe): String;
  // Informações de uso livre do fisco com "xCampo" e "xTexto"
var
  i: Integer;
begin
  Result := '';
  with ANFe.InfAdic do
  begin
    if obsFisco.Count > 0 then
    begin
      for i := 0 to (obsFisco.Count - 1) do
      begin
        Result := Result +
          obsFisco.Items[i].xCampo + ': ' +
          obsFisco.Items[i].xTexto + IfThen((i = (obsFisco.Count - 1)), '', ';');
      end;

      Result := Result + '; ';
    end;
  end;
end;

function TACBrDFeDANFeReport.ManterProcreferenciado(ANFe: TNFe): String;
  // Informações do processo referenciado
var
  i: Integer;
begin
  Result := '';
  with ANFe.InfAdic do
  begin
    if procRef.Count > 0 then
    begin
      for i := 0 to (procRef.Count - 1) do
      begin
        if (i = (procRef.Count - 1)) then
          Result := Result +
            ACBrStr('PROCESSO OU ATO CONCESSÓRIO Nº: ') +
            procRef.Items[i].nProc + ' - ORIGEM: ' +
            indProcToDescrStr(procRef.Items[i].indProc) +
            ifthen((i = (procRef.Count - 1)), '', ';');
      end;

      Result := Result + '; ';
    end;
  end;
end;

function TACBrDFeDANFeReport.ManterVprod(dVProd, dvDesc: Double): String;
var
  dValor: Double;
begin
  if ImprimeTotalLiquido then
    dValor := dVProd - dvDesc
  else
    dValor := dVProd;

  Result := FormatFloatBr( dValor );
end;

function TACBrDFeDANFeReport.ManterXPedNItemPed(aNFE: TNFe;
  const inItem: Integer): String;
var LxPed, LnItemPed, sQuebraLinha : String;
begin
  LxPed := aNFE.Det.Items[inItem].Prod.xPed;
  LnItemPed := aNFE.Det.Items[inItem].Prod.nItemPed;

  if (ImprimeXPedNItemPed) and (NaoEstaVazio(LxPed) or NaoEstaVazio(LnItemPed)) then
  begin
    Result := ';N.Pedido = ' + LxPed + '  '+ 'N.Item = ' + LnItemPed;

    sQuebraLinha := SeparadorDetalhamentos;
    Result := StringReplace(Result, ';', sQuebraLinha, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function TACBrDFeDANFeReport.ManterXProd(aNFE: TNFe; const inItem: Integer): String;
begin
  Result := '';
  if (inItem < 0) or (inItem >= aNFE.Det.Count) then
    Exit;

  Result := aNFE.Det.Items[inItem].Prod.XProd
            + ManterinfAdProd(aNFE, inItem)
            + ManterXPedNItemPed(aNFE, inItem);
end;

function TACBrDFeDANFeReport.ManterCst(dCRT: TpcnCRT; dCSOSN: TpcnCSOSNIcms; dCST: TpcnCSTIcms): String;
begin
  if (dCRT in [crtSimplesNacional, crtMEI]) and not (dCST in [cst02, cst15, cst53, cst61]) then
    Result := CSOSNIcmsToStr(dCSOSN)
  else
    Result := CSTICMSToStr(dCST);
end;

function TACBrDFeDANFeReport.ManterdvTotTrib(dvTotTrib: Double): String;
var
  dValor: Double;
begin
  if FExibeTotalTributosItem then
    dValor := dvTotTrib
  else
    dValor := 0;

  Result := FormatFloatBr(dValor);
end;

function TACBrDFeDANFeReport.CalcularValorDescontoTotal(const ANFE: TNFe): Double;
var LICMSDesonerado : Double;
begin
  if ExibeICMSDesoneradoComoDesconto then
    LICMSDesonerado := ANFE.Total.ICMSTot.vICMSDeson
  else
    LICMSDesonerado := 0;

  Result := ANFE.Total.ICMSTot.vDesc + LICMSDesonerado;
end;

function TACBrDFeDANFeReport.CalcularValorLiquidoItem(const ANFE: TNFe; const ANItem: Integer): Double;
begin
  Result := CalcularValorLiquidoItem(ANFE, ANItem, [TVLFrete, TVLDesconto, TVLOutros, TVLSeguros]);
end;

function TACBrDFeDANFeReport.CalcularValorDescontoItem(const ANFE: TNFe; const ANItem: Integer): Double;
var LProd : TProd;
  LICMSDesonerado : Double;
begin
  Result := 0;
  if (ANItem < 0) or (ANItem >= ANFE.Det.Count) then
    Exit;
  LProd := ANFE.Det.Items[ANItem].Prod;

  if ExibeICMSDesoneradoComoDesconto then
    LICMSDesonerado := ANFE.Det.Items[ANItem].Imposto.ICMS.vICMSDeson
  else
    LICMSDesonerado := 0;

  Result := LProd.vDesc + LICMSDesonerado;
end;

end.
