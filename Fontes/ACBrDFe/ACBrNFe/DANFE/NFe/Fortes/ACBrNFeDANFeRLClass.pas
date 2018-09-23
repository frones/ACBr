{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }
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

{$I ACBr.inc}
unit ACBrNFeDANFeRLClass;

interface

uses SysUtils, Classes,
  {$IFDEF CLX}
  QForms, QDialogs,
  {$ELSE}
  Forms, Dialogs,
  {$ENDIF}
  Graphics, pcnNFe, ACBrNFeDANFEClass, pcnConversao;

type
  TNomeFonte = (nfTimesNewRoman, nfCourierNew, nfArial);
  TDetVeiculo = (dv_tpOp, dv_chassi, dv_cCor, dv_xCor, dv_pot, dv_cilin,
                 dv_pesoL, dv_pesoB, dv_nSerie, dv_tpComb, dv_nMotor, dv_CMT,
                 dv_dist, dv_anoMod, dv_anoFab, dv_tpPint, dv_tpVeic,
                 dv_espVeic, dv_VIN, dv_condVeic, dv_cMod, dv_cCorDENATRAN,
                 dv_lota, dv_tpRest);
  TDetMedicamento = (dm_nLote, dm_qLote, dm_dFab, dm_dVal, dm_vPMC);
  TDetArmamento = (da_tpArma, da_nSerie, da_nCano, da_descr);
  TDetCombustivel = (dc_cProdANP, dc_CODIF, dc_qTemp, dc_UFCons, dc_CIDE,
                     dc_qBCProd, dc_vAliqProd, dc_vCIDE);
  TDetVeiculos = set of TDetVeiculo;
  TDetMedicamentos = set of TDetMedicamento;
  TDetArmamentos = set of TDetArmamento;
  TDetCombustiveis = set of TDetCombustivel;
  TObjectArray = array of TObject;
  PObjectArray = ^TObjectArray;

	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TFonte = class(TComponent)
  protected
    FNome: TNomeFonte;
    FNegrito: Boolean;
    FTamanhoFonte_RazaoSocial: Integer;

  public
    constructor Create(AOwner: TComponent); override ;
    destructor Destroy; override;

  published
    property Nome: TNomeFonte read FNome write FNome default nfTimesNewRoman;
    property Negrito: Boolean read FNegrito write FNegrito default False;
    property TamanhoFonte_RazaoSocial: Integer read FTamanhoFonte_RazaoSocial
                                              write FTamanhoFonte_RazaoSocial
                                              default 8;
  end;
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TACBrNFeDANFeRL = class( TACBrNFeDANFEClass )
  private
    FMarcadagua: string;
    FLarguraCodProd: Integer;
    fExibeCampoFatura: Boolean;
    FFonte: TFonte;
    FExibirEAN: Boolean;
    FDetVeiculos: TDetVeiculos;
    FDetMedicamentos: TDetMedicamentos;
    FDetArmamentos: TDetArmamentos;
    FDetCombustiveis: TDetCombustiveis;
    fQuebraLinhaEmDetalhamentoEspecifico : Boolean;
    fMostraDadosISSQN: Boolean;
    fAltLinhaComun: Integer;
    fEspacoEntreProdutos: Integer;
    fAlternaCoresProdutos: Boolean;
    fCorDestaqueProdutos: TColor;
    fImprimirUnQtVlComercial: TImprimirUnidQtdeValor;
    fExibirBandInforAdicProduto : Boolean;
    FImprimirDadosDocReferenciados: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirDANFE(NFE : TNFe = nil); override ;
    procedure ImprimirDANFECancelado(NFE: TNFe = nil); override;
    procedure ImprimirDANFEPDF(NFE : TNFe = nil); override;
    procedure ImprimirEVENTO(NFE : TNFe = nil); override;
    procedure ImprimirEVENTOPDF(NFE : TNFe = nil); override;
    procedure ImprimirINUTILIZACAO(NFe: TNFe = nil); override;
    procedure ImprimirINUTILIZACAOPDF(NFe: TNFe = nil); override;
    procedure SetExibirEAN(Value: Boolean); virtual;
    procedure SetTipoDANFE(Value: TpcnTipoImpressao); virtual;
  published
    property MarcadAgua : String read FMarcadagua write FMarcadagua ;
    property LarguraCodProd: Integer read FLarguraCodProd write FLarguraCodProd;
    property Fonte: TFonte read FFonte;
    property ExibirEAN: Boolean read FExibirEAN write SetExibirEAN;
    property DetVeiculos: TDetVeiculos read FDetVeiculos write FDetVeiculos default
                          [dv_chassi, dv_xCor, dv_nSerie, dv_tpComb, dv_nMotor, dv_anoMod, dv_anoFab];
    property DetMedicamentos: TDetMedicamentos read FDetMedicamentos write FDetMedicamentos default
                              [dm_nLote, dm_qLote, dm_dFab, dm_dVal, dm_vPMC];
    property DetArmamentos: TDetArmamentos read FDetArmamentos write FDetArmamentos default
                            [da_tpArma, da_nSerie, da_nCano, da_descr];
    property DetCombustiveis: TDetCombustiveis read FDetCombustiveis write FDetCombustiveis default
                            [dc_cProdANP, dc_CODIF, dc_qTemp, dc_UFCons, dc_CIDE, dc_qBCProd, dc_vAliqProd, dc_vCIDE];
    property QuebraLinhaEmDetalhamentoEspecifico : Boolean  read fQuebraLinhaEmDetalhamentoEspecifico write fQuebraLinhaEmDetalhamentoEspecifico;
    property ExibeCampoFatura: Boolean        read fExibeCampoFatura      write fExibeCampoFatura;
    property MostraDadosISSQN: Boolean read FMostraDadosISSQN write FMostraDadosISSQN default False; // Oculta o campo ISSQN mesmo possuindo inscrição municipal
    property AltLinhaComun: Integer read FAltLinhaComun write FAltLinhaComun default 30; // Alturas das linhas mais comuns do Danfe
    property EspacoEntreProdutos: Integer read FEspacoEntreProdutos write FEspacoEntreProdutos default 7; // Altura dos espaços entre os produtos
    property AlternaCoresProdutos: Boolean read FAlternaCoresProdutos write FAlternaCoresProdutos default False; // Alterna as cores de fundo dos produtos para destaca-los
    property CorDestaqueProdutos: TColor read FCorDestaqueProdutos write FCorDestaqueProdutos default clWhite; // Cor usada para destacar produtos na lista alternando entre fundo coloridos e não colorido
    property ImprimirUnQtVlComercial: TImprimirUnidQtdeValor read fImprimirUnQtVlComercial write fImprimirUnQtVlComercial;
    property ExibirBandInforAdicProduto : Boolean  read fExibirBandInforAdicProduto write fExibirBandInforAdicProduto default False; // Exibir a banda de informação Adicionais do produto.
    property ImprimirDadosDocReferenciados : Boolean  read FImprimirDadosDocReferenciados write FImprimirDadosDocReferenciados;

  end;

implementation

uses ACBrNFe, ACBrUtil,
     ACBrNFeDANFeRL, ACBrNFeDANFeEventoRL,
     ACBrNFeDANFeRLRetrato, ACBrNFeDANFeRLPaisagem,
     ACBrNFeDANFeEventoRLRetrato, ACBrNFeDANFeRLSimplificado,
     ACBrNFeDAInutRL, ACBrNFeDAInutRLRetrato;

var
  frlDANFeRL: TfrlDANFeRL;
  frlDANFeEventoRL: TfrlDANFeEventoRL;
  frmNFeDAInutRL : TfrmNFeDAInutRL;

constructor TFonte.Create(AOwner: TComponent);
begin
  inherited create( AOwner );
  FNome := nfTimesNewRoman;
  FNegrito := False;
  FTamanhoFonte_RazaoSocial := 8;
end;

destructor TFonte.Destroy;
begin
  inherited Destroy;
end;

constructor TACBrNFeDANFeRL.Create(AOwner: TComponent);
begin
  inherited create( AOwner );
  FFonte := TFonte.Create(self);
  FFonte.Name := 'Fonte';
  FLarguraCodProd := 54;
  FMargemSuperior := 0.70;
  FMargemInferior := 0.70;
  FMargemEsquerda := 0.70;
  FMargemDireita := 0.70;
  fCasasDecimais.Tag := 0;
  FCasasDecimais._qCom := 4;
  FCasasDecimais._vUnCom := 4;
  FProdutosPorPagina := 0;
  FExibirEAN := False;
  FTipoDANFE := tiRetrato;
  FDetVeiculos := [dv_chassi, dv_xCor, dv_nSerie, dv_tpComb, dv_nMotor, dv_anoMod, dv_anoFab];
  FDetMedicamentos := [dm_nLote, dm_qLote, dm_dFab, dm_dVal, dm_vPMC];
  FDetArmamentos := [da_tpArma, da_nSerie, da_nCano, da_descr];
  FDetCombustiveis := [dc_cProdANP, dc_CODIF, dc_qTemp, dc_UFCons, dc_CIDE, dc_qBCProd, dc_vAliqProd, dc_vCIDE];
  fQuebraLinhaEmDetalhamentoEspecifico  := True;
  fExibeCampoFatura     := False;
  fMostraDadosISSQN     := False;
  fAltLinhaComun        := 30;
  fEspacoEntreProdutos  := 7;
  fAlternaCoresProdutos := False;
  fCorDestaqueProdutos  := clWhite;
  fImprimirUnQtVlComercial  := iuComercial;
  fExibirBandInforAdicProduto := False;    
  FImprimirDadosDocReferenciados := True;
end;

destructor TACBrNFeDANFeRL.Destroy;
begin
  FFonte.Destroy;
  inherited Destroy ;
end;

procedure TACBrNFeDANFeRL.ImprimirDANFE(NFE: TNFe = nil);
var
 i : Integer;
 ReportArray : TObjectArray; 
 c, ANumCopiasAgrupar: integer;
begin
try
  case TipoDANFE of
    tiRetrato      : frlDANFeRL := TfrlDANFeRLRetrato.Create(Self);
    tiPaisagem     : frlDANFeRL := TfrlDANFeRLPaisagem.Create(Self);
    tiSimplificado : frlDANFeRL := TfrlDANFeRLSimplificado.Create(Self);
  else
    frlDANFeRL := TfrlDANFeRLRetrato.Create(Self);
  end;

  if NFE = nil then
    begin
      // alteração para imprimir agrupando por notas quando tem mais de 1 cópia configurada
      ANumCopiasAgrupar := 1;
      if FAgruparNumCopias then
        begin
          ANumCopiasAgrupar := FNumCopias;
          FNumCopias := 1; // tem que considerar que foi configurado apenas 1 cópia e realizar um outro "for" para ir adicionando as cópias agrupadas por nota
        end;

      for i:= 0 to TACBrNFe(ACBrNFe).NotasFiscais.Count - 1 do
        begin
          for c:= 0 to ANumCopiasAgrupar - 1 do
            begin
              frlDANFeRL.Imprimir( Self, TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe,
              FLogo, FMarcaDagua, FLarguraCodProd, FEmail, FExibeResumoCanhoto, FFax,
              FNumCopias, FSistema, FSite, FUsuario, FPosCanhoto, FFormularioContinuo,
              FExpandirLogoMarca, FMostrarPreview, FMostrarStatus, FFonte.FNome,
              FFonte.FNegrito, FMargemSuperior, FMargemInferior, FMargemEsquerda,
              FMargemDireita, FCasasDecimais._qCom,
              FCasasDecimais._vUnCom, FProdutosPorPagina, FImpressora,
              FFonte.FTamanhoFonte_RazaoSocial, FExibirEAN, FProtocoloNFe,
              FExibeResumoCanhoto_Texto, FNFeCancelada,
              FImprimirDetalhamentoEspecifico, FImprimeDescPorc,FImprimeNomeFantasia,fImprimirTotalLiquido,
              FDetVeiculos, FDetMedicamentos, FDetArmamentos, FDetCombustiveis,
              fQuebraLinhaEmDetalhamentoEspecifico,
              Integer ( fCasasDecimais.Formato ),
              fCasasDecimais._Mask_qCom,
              fCasasDecimais._Mask_vUnCom,
              fExibeCampoFatura,
              fMostraDadosISSQN,
              fAltLinhaComun,
              fEspacoEntreProdutos,
              fAlternaCoresProdutos,
              fCorDestaqueProdutos,
              fImprimirDadosDocReferenciados,
              fTamanhoLogoHeight,
              fTamanhoLogoWidth,
              fRecuoEndereco,
              fRecuoEmpresa,
              fLogoEmCima,
              fTamanhoFonteEndereco,
              fRecuoLogo,
              (i = Pred(TACBrNFe(ACBrNFe).NotasFiscais.Count)) and (c = ANumCopiasAgrupar - 1),
              @ReportArray);
          end;
        end;
    end
  else
    begin
      frlDANFeRL.Imprimir( Self, NFE,
      FLogo, FMarcaDagua, FLarguraCodProd, FEmail, FExibeResumoCanhoto, FFax,
      FNumCopias, FSistema, FSite, FUsuario, FPosCanhoto, FFormularioContinuo,
      FExpandirLogoMarca, FMostrarPreview, FMostrarStatus, FFonte.FNome,
      FFonte.FNegrito, FMargemSuperior, FMargemInferior, FMargemEsquerda,
      FMargemDireita, FCasasDecimais._qCom,
      FCasasDecimais._vUnCom, FProdutosPorPagina, FImpressora,
      FFonte.FTamanhoFonte_RazaoSocial, FExibirEAN, FProtocoloNFe,
      FExibeResumoCanhoto_Texto, FNFeCancelada,
      FImprimirDetalhamentoEspecifico, FImprimeDescPorc,FImprimeNomeFantasia,fImprimirTotalLiquido,
      FDetVeiculos, FDetMedicamentos, FDetArmamentos, FDetCombustiveis,
      fQuebraLinhaEmDetalhamentoEspecifico,
      Integer ( fCasasDecimais.Formato ),
      fCasasDecimais._Mask_qCom,
      fCasasDecimais._Mask_vUnCom,
      fExibeCampoFatura,
      fMostraDadosISSQN,
      fAltLinhaComun,
      fEspacoEntreProdutos,
      fAlternaCoresProdutos,
      fCorDestaqueProdutos,
      fImprimirDadosDocReferenciados,
      fTamanhoLogoHeight,
      fTamanhoLogoWidth,
      fRecuoEndereco,
      fRecuoEmpresa,
      fLogoEmCima,
      fTamanhoFonteEndereco,
      fRecuoLogo,
      True,
      @ReportArray);
    end;

  finally
    FreeAndNil(frlDANFeRL);
  end;
end;

procedure TACBrNFeDANFeRL.ImprimirDANFECancelado(NFE: TNFe = nil);
begin
   NFeCancelada := True;
   ImprimirDANFE(NFE);
end;

procedure TACBrNFeDANFeRL.ImprimirDANFEPDF(NFE : TNFe = nil);
var
  i : Integer;
begin
  try
  case TipoDANFE of
    tiRetrato      : frlDANFeRL := TfrlDANFeRLRetrato.Create(Self);
    tiPaisagem     : frlDANFeRL := TfrlDANFeRLPaisagem.Create(Self);
    tiSimplificado : frlDANFeRL := TfrlDANFeRLSimplificado.Create(Self);
  else
    frlDANFeRL := TfrlDANFeRLRetrato.Create(Self);
  end;

  if NFE = nil then
    begin
      for i:= 0 to TACBrNFe(ACBrNFe).NotasFiscais.Count-1 do
        begin
//          sFile := TACBrNFe(ACBrNFe).DANFE.PathPDF +
//                   Copy(TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe.infNFe.ID,
//                   4, 44) + '-nfe.pdf';
          FPArquivoPDF := TACBrNFe(ACBrNFe).DANFE.PathPDF +
                          StringReplace(TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe.infNFe.ID,'NFe', '', [rfIgnoreCase]) +
                          '-nfe.pdf';

          frlDANFeRL.SavePDF(Self, TACBrNFe(ACBrNFe).NotasFiscais.Items[i].NFe,
          FLogo, FMarcaDagua, FLarguraCodProd, FEmail, FExibeResumoCanhoto, FFax,
          FNumCopias, FSistema, FSite, FUsuario, FPArquivoPDF, FPosCanhoto, FFormularioContinuo,
          FExpandirLogoMarca, FMostrarStatus, FFonte.FNome, FFonte.FNegrito, FMargemSuperior,
          FMargemInferior, FMargemEsquerda, FMargemDireita, FCasasDecimais._qCom,
          FCasasDecimais._vUnCom, FProdutosPorPagina, FImpressora,
          FFonte.FTamanhoFonte_RazaoSocial, FExibirEAN, FProtocoloNFe,
          FExibeResumoCanhoto_Texto, FNFeCancelada,
          FImprimirDetalhamentoEspecifico, FImprimeDescPorc,FImprimeNomeFantasia,fImprimirTotalLiquido,
          FDetVeiculos, FDetMedicamentos, FDetArmamentos, FDetCombustiveis,
          fQuebraLinhaEmDetalhamentoEspecifico,
          Integer ( fCasasDecimais.Formato ),
          fCasasDecimais._Mask_qCom,
          fCasasDecimais._Mask_vUnCom,
          fExibeCampoFatura,
          fMostraDadosISSQN,
          fAltLinhaComun,
          fEspacoEntreProdutos,
          fAlternaCoresProdutos,
          fCorDestaqueProdutos,
          fImprimirDadosDocReferenciados,
          FTamanhoLogoHeight,
          FTamanhoLogoWidth,
          FRecuoEndereco,
          FRecuoEmpresa,
          FLogoEmCima,
          FTamanhoFonteEndereco,
          FRecuoLogo);
        end;
    end
  else
    begin
//      sFile := Self.PathPDF + Copy(NFe.infNFe.ID, 4, 44) + '-nfe.pdf';
      FPArquivoPDF := Self.PathPDF +
                      StringReplace(NFe.infNFe.ID,'NFe', '', [rfIgnoreCase]) +
                      '-nfe.pdf';

      frlDANFeRL.SavePDF(Self, NFe,
      FLogo, FMarcaDagua, FLarguraCodProd, FEmail, FExibeResumoCanhoto, FFax,
      FNumCopias, FSistema, FSite, FUsuario, FPArquivoPDF, FPosCanhoto, FFormularioContinuo,
      FExpandirLogoMarca, FMostrarStatus, FFonte.FNome, FFonte.FNegrito, FMargemSuperior,
      FMargemInferior, FMargemEsquerda, FMargemDireita, FCasasDecimais._qCom,
      FCasasDecimais._vUnCom, FProdutosPorPagina, FImpressora,
      FFonte.FTamanhoFonte_RazaoSocial, FExibirEAN, FProtocoloNFe,
      FExibeResumoCanhoto_Texto, FNFeCancelada,
      FImprimirDetalhamentoEspecifico, FImprimeDescPorc,FImprimeNomeFantasia,fImprimirTotalLiquido,
      FDetVeiculos, FDetMedicamentos, FDetArmamentos, FDetCombustiveis,
      fQuebraLinhaEmDetalhamentoEspecifico,
      Integer ( fCasasDecimais.Formato ),
      fCasasDecimais._Mask_qCom,
      fCasasDecimais._Mask_vUnCom,
      fExibeCampoFatura,
      fMostraDadosISSQN,
      fAltLinhaComun,
      fEspacoEntreProdutos,
      fAlternaCoresProdutos,
      fCorDestaqueProdutos,
      fImprimirDadosDocReferenciados,
      FTamanhoLogoHeight,
      FTamanhoLogoWidth,
      FRecuoEndereco,
      FRecuoEmpresa,
      FLogoEmCima,
      FTamanhoFonteEndereco,
      FRecuoLogo);
    end;
 finally
   FreeAndNil(frlDANFeRL);
 end;
end;

procedure TACBrNFeDANFeRL.SetExibirEAN(Value: Boolean);
begin
  if FTipoDANFE = tiRetrato then
    FExibirEAN := False
  else
    FExibirEAN := Value;
end;

procedure TACBrNFeDANFeRL.SetTipoDANFE(Value: TpcnTipoImpressao);
begin
  if Value = tiRetrato then
    begin
      FExibirEAN := False;
    end;

  FTipoDANFE := Value;
end;

procedure TACBrNFeDANFeRL.ImprimirEVENTO(NFE: TNFe);
var Impresso: Boolean;
I, J : Integer;
begin
try
  case TipoDANFE of
    tiRetrato,
    tiPaisagem: frlDANFeEventoRL := TfrlDANFeEventoRLRetrato.Create(Self);
  else
    frlDANFeEventoRL := TfrlDANFeEventoRLRetrato.Create(Self);
  end;

  if TACBrNFe(ACBrNFe).NotasFiscais.Count > 0 then
    begin
      for i := 0 to (TACBrNFe(ACBrNFe).EventoNFe.Evento.Count - 1) do
        begin
          Impresso := False;
          for j := 0 to (TACBrNFe(ACBrNFe).NotasFiscais.Count - 1) do
            begin
//              if Copy(TACBrNFe(ACBrNFe).NotasFiscais.Items[j].NFe.infNFe.ID, 4, 44) = TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.chNFe then
              if StringReplace(TACBrNFe(ACBrNFe).NotasFiscais.Items[j].NFe.infNFe.ID,'NFe', '', [rfIgnoreCase]) = TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.chNFe then

                begin
                  frlDANFeEventoRL.Imprimir(Self, TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i],
                  FLogo, FMarcadagua, FNumCopias, FSistema, FUsuario, FMostrarPreview,
                  FFonte.FNome, FFonte.FNegrito, FMargemSuperior, FMargemInferior,
                  FMargemEsquerda, FMargemDireita, FImpressora,
                  TACBrNFe(ACBrNFe).NotasFiscais.Items[j].NFe);
                  Impresso := True;
                  Break;
                end; // if Copy(TACBrNFe(ACBrNFe).NotasFiscais.Items[j].NFe.infNFe.ID, 4, 44)
            end; // for j := 0 to (TACBrNFe(ACBrNFe).NotasFiscais.Count - 1)

          if Impresso = False then
            begin
              frlDANFeEventoRL.Imprimir(Self, TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i],
              FLogo, FMarcadagua, FNumCopias, FSistema, FUsuario, FMostrarPreview,
              FFonte.FNome, FFonte.FNegrito, FMargemSuperior, FMargemInferior,
              FMargemEsquerda, FMargemDireita, FImpressora);
            end;
        end; // for i := 0 to (TACBrNFe(ACBrNFe).EventoNFe.Evento.Count - 1)
    end // if TACBrNFe(ACBrNFe).NotasFiscais.Count > 0
  else
    begin
      for i := 0 to (TACBrNFe(ACBrNFe).EventoNFe.Evento.Count - 1) do
        begin
          frlDANFeEventoRL.Imprimir(Self, TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i],
          FLogo, FMarcadagua, FNumCopias, FSistema, FUsuario, FMostrarPreview,
          FFonte.FNome, FFonte.FNegrito, FMargemSuperior, FMargemInferior,
          FMargemEsquerda, FMargemDireita, FImpressora);
        end;
    end;

 finally
  FreeAndNil(frlDANFeEventoRL);
 end;
end;

procedure TACBrNFeDANFeRL.ImprimirEVENTOPDF(NFE: TNFe);
var
  Impresso: Boolean;
  I, J : Integer;
begin
try
  case TipoDANFE of
    tiRetrato,
    tiPaisagem: frlDANFeEventoRL := TfrlDANFeEventoRLRetrato.Create(Self);
  else
    frlDANFeEventoRL := TfrlDANFeEventoRLRetrato.Create(Self);
  end;

  if TACBrNFe(ACBrNFe).NotasFiscais.Count > 0 then
    begin
      for i := 0 to (TACBrNFe(ACBrNFe).EventoNFe.Evento.Count - 1) do
        begin
          FPArquivoPDF := TACBrNFe(ACBrNFe).DANFE.PathPDF +
                   StringReplace(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.id, 'ID', '', [rfIgnoreCase]) +
                   '-procEventoNFe.pdf';

//          Copy(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.id, 3, 52) + 'evento.pdf';
          Impresso := False;

          for j := 0 to (TACBrNFe(ACBrNFe).NotasFiscais.Count - 1) do
            begin
//              if Copy(TACBrNFe(ACBrNFe).NotasFiscais.Items[j].NFe.infNFe.ID, 4, 44) = TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.chNFe then
              if StringReplace(TACBrNFe(ACBrNFe).NotasFiscais.Items[j].NFe.infNFe.ID,'NFe', '', [rfIgnoreCase]) = TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.chNFe then
                begin
                  frlDANFeEventoRL.SavePDF(Self, TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i],
                  FLogo, FMarcadagua, FPArquivoPDF, FSistema, FUsuario, FFonte.FNome, FFonte.FNegrito,
                  FMargemSuperior, FMargemInferior, FMargemEsquerda, FMargemDireita,
                  TACBrNFe(ACBrNFe).NotasFiscais.Items[j].NFe);
                  Impresso := True;
                  Break;
                end; // if Copy(TACBrNFe(ACBrNFe).NotasFiscais.Items[j].NFe.infNFe.ID, 4, 44)
            end; // for j := 0 to (TACBrNFe(ACBrNFe).NotasFiscais.Count - 1)

          if Impresso = False then
            begin
              frlDANFeEventoRL.SavePDF(Self, TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i],
              FLogo, FMarcadagua, FPArquivoPDF, FSistema, FUsuario, FFonte.FNome, FFonte.FNegrito,
              FMargemSuperior, FMargemInferior, FMargemEsquerda, FMargemDireita);
            end;
        end; // for i := 0 to (TACBrNFe(ACBrNFe).EventoNFe.Evento.Count - 1)
    end // if TACBrNFe(ACBrNFe).NotasFiscais.Count > 0
  else
    begin
      for i := 0 to (TACBrNFe(ACBrNFe).EventoNFe.Evento.Count - 1) do
        begin
          FPArquivoPDF := TACBrNFe(ACBrNFe).DANFE.PathPDF +
                          StringReplace(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.id, 'ID', '', [rfIgnoreCase]) +
                          '-procEventoNFe.pdf';
//          Copy(TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i].InfEvento.id, 3, 52) + 'evento.pdf';

          frlDANFeEventoRL.SavePDF(Self, TACBrNFe(ACBrNFe).EventoNFe.Evento.Items[i],
          FLogo, FMarcadagua, FPArquivoPDF, FSistema, FUsuario, FFonte.FNome, FFonte.FNegrito,
          FMargemSuperior, FMargemInferior, FMargemEsquerda, FMargemDireita);
        end;
    end;
 finally
  FreeAndNil(frlDANFeEventoRL);
 end; 
end;

procedure TACBrNFeDANFErl.ImprimirINUTILIZACAO(NFe: TNFe);
begin
  frmNFeDAInutRL := TfrmNFeDAInutRLRetrato.Create(Self);

  frmNFeDAInutRL.Imprimir(TACBrNFe(ACBrNFe),
                          FLogo, FNumCopias, FSistema, FUsuario,
                          FMostrarPreview, FMargemSuperior,
                          FMargemInferior, FMargemEsquerda,
                          FMargemDireita, FImpressora);

  FreeAndNil(frmNFeDAInutRL);
end;

procedure TACBrNFeDANFErl.ImprimirINUTILIZACAOPDF(NFe: TNFe);
begin
  frmNFeDAInutRL := TfrmNFeDAInutRLRetrato.Create(Self);

  FPArquivoPDF := StringReplace(TACBrNFe(ACBrNFe).InutNFe.ID, 'ID', '', [rfIgnoreCase]);
  if FPArquivoPDF = '' then
    FPArquivoPDF := StringReplace(TACBrNFe(ACBrNFe).InutNFe.ID, 'ID', '', [rfIgnoreCase]);
  FPArquivoPDF := PathWithDelim(Self.PathPDF) + FPArquivoPDF + '-procInutNFe.pdf';

  frmNFeDAInutRL.SavePDF(TACBrNFe(ACBrNFe),
                         FLogo, FPArquivoPDF, FSistema, FUsuario,
                         FMargemSuperior, FMargemInferior,
                         FMargemEsquerda, FMargemDireita);

  FreeAndNil(frmNFeDAInutRL);
end;

end.
