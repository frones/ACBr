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

{******************************************************************************
|* Historico
|*
|* 16/12/2008: Wemerson Souto
|*  - Doação do componente para o Projeto ACBr
|* 20/08/2009: Caique Rodrigues
|*  - Doação units para geração do Danfe via QuickReport
|* 20/11/2009: Peterson de Cerqueira Matos
|*             E-mail: peterson161@yahoo.com - Tel: (11) 7197-1474 / 8059-4055
|*  - Componente e Units do QuickReport clonados
|*    e transformados em FORTES REPORT
|* 27/01/2010: Peterson de Cerqueira Matos
|*  - Acréscimo dos parâmetros "ALarguraCodProd" nas Class procedures
|*    "Imprimir" e "SavePDF"
|* 05/02/2010: Peterson de Cerqueira Matos
|*  - Tratamento das propriedades "Email", "ResumoCanhoto", "Fax", "NumCopias",
|*    "Sistema", "Site", "Usuario" em "ACBrNFeDANFeClass"
|*  - Acréscimo dos parâmetros "AEmail", "AResumoCanhoto", "AFax", "ANumCopias",
|*    "Asistema", "ASite", "AUsuario" nas Class procedures
|*    "Imprimir" e "SavePDF"
|* 13/02/2010: Peterson de Cerqueira Matos
|*  - Correção na exibição do 'Preview' para modo 'PREVIEWMODAL'
|* 15/03/2010: Felipe Feltes
|*  - Adequação na seção 'USES' para ser utilizado em CLX
|* 19/03/2010: Peterson de Cerqueira Matos
|*  - Tratamento das propriedades "FormularioContinuo", "ExpandirLogoMarca" e
|*    "MostrarPreview" de "ACBrNFeDANFeClass"
|*  - Acréscimo dos parâmetros "APosCanhoto", "AFormularioContinuo",
|*    "AExpandirLogoMarca" e "AMostrarPreview" nas Class procedures
|*    "Imprimir" e "SavePDF" (esta última sem o "AMostrarPreview")
|* 22/03/2010: Peterson de Cerqueira Matos
|*  - Tratamento das margens em "ACBrNFeDANFeClass"
|*  - Acréscimo dos parâmetros "AMargemSuperior", "AMargemInferior",
|*    "AMargemEsqurda", "AMargemDireita" e "AFonteDANFE"
|* 13/04/2010: Peterson de Cerqueira Matos
|*  - Adequação à NF-e 2.0, Manual de Integração do Contribuinte 4.0.1NT2009.006
|*  - Tratamento das casas decimais em "ACBrNFeDANFeClass"
|*  - Acréscimo dos parâmetros "ACasasDecimaisqCom" e "ACasasDecimaisvUnCom"
|* 06/07/2010: Peterson de Cerqueira Matos
|*  - Tratamento do formato de impressão e da quantidade de produtos por
|*  - página em "ACBrNFeDANFeClass"
|*  - Acréscimo dos parâmetros "ATipoDANFE" e "AProdutosPorPagina"
|* 20/07/2010: Peterson de Cerqueira Matos
|*  - Permite enviar o DANFe para a impressora informada em "Impressora"
|*  - Acréscimo do parâmetro "AImpressora"
|* 10/08/2010: Peterson de Cerqueira Matos
|*  - Acréscimo do parâmetro "ATamanhoFonte_RazaoSocial"
|* 25/11/2010: Peterson de Cerqueira Matos
|*  - Acréscimo do parâmetro "AExibirEAN"
|* 01/03/2011: Fernando Emiliano David Nunes
|*  - Acréscimo do parâmetro "AProtocoloNFe"
|* 20/05/2011: Peterson de Cerqueira Matos
|*  - Acréscimo do parâmetro "AResumoCanhoto_Texto"
|* 23/05/2011: Waldir Paim
|*  - Início da preparação para Lazarus: Somente utiliza TClientDataSet quando
|*    estiver no Delphi. Obrigatória a utilização da versão 3.70B ou superior
|*    do Fortes Report. Download disponível em
|*    http://sourceforge.net/projects/fortesreport/files/
|* 22/03/2013: Peterson de Cerqueira Matos
|*  - Impressão dos detalhamentos específicos e do desconto em percentual
******************************************************************************}

{$I ACBr.inc}
unit ACBrNFeDANFeRL;

interface

uses
  SysUtils, Variants, Classes, StrUtils,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, Qt,
  {$ELSE}
      Graphics, Controls, Forms, Dialogs, ExtCtrls,
  {$ENDIF}
  pcnNFe, pcnConversao, ACBrNFe, ACBrNFeDANFeRLClass, ACBrUtil,
  RLReport, RLFilters, RLPrinters, RLPDFFilter, RLConsts;

type

  { TfrlDANFeRL }

  TfrlDANFeRL = class(TForm)
    RLNFe: TRLReport;
    RLPDFFilter1: TRLPDFFilter;
    procedure FormCreate(Sender: TObject);
    function BuscaDireita(Busca, Text: String): Integer;
    procedure InsereLinhas(sTexto: String; iLimCaracteres: Integer; rMemo: TRLMemo);
    function QuebraLinha: String;
    function ManterDesPro(dvDesc, dvProd: Double): Double;
    function TrataDocumento(sCNPJCPF: String): String;
    function ManterInfAdFisco: String;
    function ManterInfCompl: String;
    function ManterInfContr: String;
    function ManterObsFisco: String;
    function ManterProcreferenciado: String;

  private
    { Private declarations }
  protected
    FNFe: TNFe;
    FLogo: String;
    FMarcaDagua: String;
    FLarguraCodProd: Integer;
    FEmail: String;
    FResumoCanhoto: Boolean;
    FFax: String;
    FNumCopias: Integer;
    FSistema: String;
    FSite: String;
    FUsuario: String;
    FPosCanhoto: TPosRecibo;
    FFormularioContinuo: Boolean;
    FExpandirLogoMarca: Boolean;
    FMostrarPreview: Boolean;
    FMostrarStatus: Boolean;
    FNomeFonte: TNomeFonte;
    FNegrito: Boolean;
    FMargemSuperior: Double;
    FMargemInferior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FCasasDecimaisqCom: Integer;
    FCasasDecimaisvUnCom: Integer;
    FProdutosPorPagina: Integer;
    FImpressora: String;
    FTamanhoFonte_RazaoSocial: Integer;
    FExibirEAN: Boolean;
    FProtocoloNFe : String;
    FResumoCanhoto_Texto: String;
    FNFeCancelada: Boolean;
    FImprimirDetalhamentoEspecifico: Boolean;
    iLimiteLinhas : Integer;
    iLinhasUtilizadas : Integer;
    iLimiteCaracteresLinha : Integer;
    iLimiteCaracteresContinuacao : Integer;
    q : Integer;
    iQuantItens : Integer;
    iItemAtual : Integer;
    sRetirada : String;
    sEntrega : String;
    FImprimirDescPorc: Boolean;
    fImprimeNomeFantasia: Boolean;
    fImprimirTotalLiquido : Boolean;
    FDetVeiculos: TDetVeiculos;
    FDetMedicamentos: TDetMedicamentos;
    FDetArmamentos: TDetArmamentos;
    FDetCombustiveis: TDetCombustiveis;
    fQuebraLinhaEmDetalhamentoEspecifico : Boolean;
    fFormato    : integer;
    fMask_qCom : String;
    fMask_vUnCom : String;
    fExibeCampoFatura: Boolean;

    FTamanhoLogoHeigth: Integer;
    FTamanhoLogoWidth: Integer;
    FRecuoEndereco: Integer;
    FRecuoEmpresa: Integer;
    FLogoemCima: Boolean;
    FTamanhoFonteEndereco: Integer;
    FRecuoLogo: Integer;

    fMostraDadosISSQN: Boolean;
    fAltLinhaComun: Integer;
    fEspacoEntreProdutos: Integer;
    fAlternaCoresProdutos: Boolean;
    fCorDestaqueProdutos: TColor;
    vAuxDiferencaPDF : integer;
    FImprimirDadosDocReferenciados: Boolean;
    procedure ConfigureVariavies(ATipoDANFE: TpcnTipoImpressao);
  public
    { Public declarations }
    class procedure Imprimir(AOwner: TComponent; ANFe: TNFe; ALogo: String = '';
                    AMarcaDagua: String = ''; ALarguraCodProd: Integer = 54;
                    AEmail: String = ''; AResumoCanhoto: Boolean = False;
                    AFax: String = ''; ANumCopias: Integer = 1;
                    ASistema: String = ''; ASite: String = '';
                    AUsuario: String = '';
                    APosCanhoto: TPosRecibo = prCabecalho;
                    AFormularioContinuo: Boolean = False;
                    AExpandirLogoMarca: Boolean = False;
                    AMostrarPreview: Boolean = True;
                    AMostrarStatus: Boolean = True;
                    ANomeFonte: TNomeFonte = nfTimesNewRoman;
                    ANegrito: Boolean = True;
                    AMargemSuperior: Double = 0.7;
                    AMargemInferior: Double = 0.7;
                    AMargemEsquerda: Double = 0.7;
                    AMargemDireita: Double = 0.7;
                    ACasasDecimaisqCom: Integer = 4;
                    ACasasDecimaisvUncCom: Integer = 4;
                    AProdutosPorPagina: Integer = 0;
                    AImpressora: String = '';
                    ATamanhoFonte_RazaoSocial: Integer = 8;
                    AExibirEAN: Boolean = False;
                    AProtocoloNFe: String = '';
                    AResumoCanhoto_Texto: String = '';
                    ANFECancelada: Boolean = False;
                    AImprimirDetalhamentoEspecifico: Boolean = True;
                    AImprimirDescPorc: Boolean = False;
                    AImprimeNomeFantasia: Boolean = False;
                    AImprimirTotalLiquido : Boolean = False;
                    ADetVeiculos: TDetVeiculos = [];
                    ADetMedicamentos: TDetMedicamentos = [];
                    ADetArmamentos: TDetArmamentos = [];
                    ADetCombustiveis: TDetCombustiveis = [];
                    AdQuebraLinhaEmDetalhamentoEspecifico : Boolean = True;
                    AdCasasDecimaisFormato : Integer = 0;
                    AdCasasDecimais_Mask_qCom : String = '###,###,###,##0.00';
                    AdCasasDecimais_Mask_vUnCom:String = '###,###,###,##0.00';
                    AdExibeCampoFatura: Boolean = False;
                    AMostraDadosISSQN: Boolean = False;
                    AAltLinhaComun: Integer = 30;
                    AEspacoEntreProdutos: Integer = 7;
                    AAlternaCoresProdutos: Boolean = False;
                    ACorDestaqueProdutos: TColor = clWhite;
                    AImprimirDadosDocReferenciados: Boolean = True;
                    ATamanhoLogoHeight: Integer = 200;
                    ATamanhoLogoWidth: Integer = 200;
                    ARecuoEndereco: Integer = 10;
                    ARecuoEmpresa: Integer = 10;
                    ALogoemCima: Boolean = False;
                    ATamanhoFonteEndereco: Integer = 10;
                    ARecuoLogo: Integer = 0;
                    AUltimaNFE : Boolean = True;
                    AArray : PObjectArray = nil);


    class procedure SavePDF(AOwner: TComponent; ANFe: TNFe; ALogo: String = '';
                    AMarcaDagua: String = ''; ALarguraCodProd: Integer = 54;
                    AEmail: String = ''; AResumoCanhoto: Boolean = False;
                    AFax: String = ''; ANumCopias: Integer = 1;
                    ASistema: String = ''; ASite: String = '';
                    AUsuario: String = ''; AFile: String = '';
                    APosCanhoto: TPosRecibo = prCabecalho;
                    AFormularioContinuo: Boolean = False;
                    AExpandirLogoMarca: Boolean = False;
                    AMostrarStatus: Boolean = False;
                    ANomeFonte: TNomeFonte = nfTimesNewRoman;
                    ANegrito: Boolean = True;
                    AMargemSuperior: Double = 0.7;
                    AMargemInferior: Double = 0.7;
                    AMargemEsquerda: Double = 0.7;
                    AMargemDireita: Double = 0.7;
                    ACasasDecimaisqCom: Integer = 4;
                    ACasasDecimaisvUncCom: Integer = 4;
                    AProdutosPorPagina: Integer = 0;
                    AImpressora: String = '';
                    ATamanhoFonte_RazaoSocial: Integer = 8;
                    AExibirEAN: Boolean = False;
                    AProtocoloNFe: String = '';
                    AResumoCanhoto_Texto: String = '';
                    ANFECancelada: Boolean = False;
                    AImprimirDetalhamentoEspecifico: Boolean = True;
                    AImprimirDescPorc: Boolean = False;
                    AImprimeNomeFantasia: Boolean = False;
                    AImprimirTotalLiquido : Boolean = False;
                    ADetVeiculos: TDetVeiculos = [];
                    ADetMedicamentos: TDetMedicamentos = [];
                    ADetArmamentos: TDetArmamentos = [];
                    ADetCombustiveis: TDetCombustiveis = [];
                    ADQuebraLinhaEmDetalhamentoEspecifico : Boolean = True;
                    AdCasasDecimaisFormato : Integer = 0;
                    AdCasasDecimais_Mask_qCom : String = '###,###,###,##0.00';
                    AdCasasDecimais_Mask_vUnCom:String = '###,###,###,##0.00';
                    AdExibeCampoFatura: Boolean = False;
                    AMostraDadosISSQN: Boolean = False;
                    AAltLinhaComun: Integer = 30;
                    AEspacoEntreProdutos: Integer = 7;
                    AAlternaCoresProdutos: Boolean = False;
                    ACorDestaqueProdutos: TColor = clWhite;
                    AImprimirDadosDocReferenciados: Boolean = True;
                    ATamanhoLogoHeight: Integer = 200;
                    ATamanhoLogoWidth: Integer = 200;
                    ARecuoEndereco: Integer = 10;
                    ARecuoEmpresa: Integer = 10;
                    ALogoemCima: Boolean = False;
                    ATamanhoFonteEndereco: Integer = 10;
                    ARecuoLogo: Integer = 0);


  end;


implementation

uses ACBrValidador;

{$R *.dfm}


class procedure TfrlDANFeRL.Imprimir(AOwner: TComponent;
                ANFe: TNFe;
                ALogo: String;
                AMarcaDagua: String;
                ALarguraCodProd: Integer;
                AEmail: String;
                AResumoCanhoto: Boolean;
                AFax: String;
                ANumCopias: Integer; ASistema: String;
                ASite: String;
                AUsuario: String;
                APosCanhoto: TPosRecibo;
                AFormularioContinuo: Boolean;
                AExpandirLogoMarca: Boolean;
                AMostrarPreview: Boolean;
                AMostrarStatus: Boolean;
                ANomeFonte: TNomeFonte;
                ANegrito: Boolean;
                AMargemSuperior: Double;
                AMargemInferior: Double;
                AMargemEsquerda: Double;
                AMargemDireita: Double;
                ACasasDecimaisqCom: Integer;
                ACasasDecimaisvUncCom: Integer;
                AProdutosPorPagina: Integer;
                AImpressora: String;
                ATamanhoFonte_RazaoSocial: Integer;
                AExibirEAN: Boolean;
                AProtocoloNFe: String;
                AResumoCanhoto_Texto: String;
                ANFECancelada: Boolean;
                AImprimirDetalhamentoEspecifico: Boolean;
                AImprimirDescPorc: Boolean;
                AImprimeNomeFantasia: Boolean;
                AImprimirTotalLiquido: Boolean;
                ADetVeiculos: TDetVeiculos;
                ADetMedicamentos: TDetMedicamentos;
                ADetArmamentos: TDetArmamentos;
                ADetCombustiveis: TDetCombustiveis;
                AdQuebraLinhaEmDetalhamentoEspecifico: Boolean;
                AdCasasDecimaisFormato: Integer;
                AdCasasDecimais_Mask_qCom: String;
                AdCasasDecimais_Mask_vUnCom: String;
                AdExibeCampoFatura: Boolean;
                AMostraDadosISSQN: Boolean;
                AAltLinhaComun: Integer;
                AEspacoEntreProdutos: Integer;
                AAlternaCoresProdutos: Boolean;
                ACorDestaqueProdutos: TColor;
                AImprimirDadosDocReferenciados: Boolean;
                ATamanhoLogoHeight: Integer;
                ATamanhoLogoWidth: Integer;
                ARecuoEndereco: Integer;
                ARecuoEmpresa: Integer;
                ALogoemCima: Boolean;
                ATamanhoFonteEndereco: Integer;
                ARecuoLogo: Integer;
                AUltimaNFE : Boolean;
                AArray : PObjectArray);

var Report, ReportNext : TRLCustomReport;
    i : Integer;
begin
  with Create ( AOwner ) do
    try
      FNFe := ANFe;
      FLogo := ALogo;
      FMarcaDagua := AMarcaDagua;
      FLarguraCodProd := ALarguraCodProd;
      FEmail := AEmail;
      FResumoCanhoto := AResumoCanhoto;
      FFax := AFax;
      FNumCopias := ANumCopias;
      FSistema := ASistema;
      FSite := ASite;
      FUsuario := AUsuario;
      FPosCanhoto := APosCanhoto;
      FFormularioContinuo := AFormularioContinuo;
      FExpandirLogoMarca := AExpandirLogoMarca;
      FMostrarPreview := AMostrarPreview;
      FMostrarStatus := AMostrarStatus;
      FNomeFonte := ANomeFonte;
      FNegrito := ANegrito;
      FMargemSuperior := AMargemSuperior;
      FMargemInferior := AMargemInferior;
      FMargemEsquerda := AMargemEsquerda;
      FMargemDireita := AMargemDireita;
      FCasasDecimaisqCom := ACasasDecimaisqCom;
      FCasasDecimaisvUnCom := ACasasDecimaisvUncCom;
      FProdutosPorPagina := AProdutosPorPagina;
      FImpressora := AImpressora;
      FTamanhoFonte_RazaoSocial := ATamanhoFonte_RazaoSocial;
      FExibirEAN := AExibirEAN;
      FProtocoloNFe := AProtocoloNFe;
      FResumoCanhoto_Texto := AResumoCanhoto_Texto;
      FNFeCancelada := ANFeCancelada;
      FImprimirDetalhamentoEspecifico := AImprimirDetalhamentoEspecifico;
      FImprimirDescPorc := AImprimirDescPorc;
      fImprimeNomeFantasia  := AImprimeNomeFantasia;
      fImprimirTotalLiquido := AImprimirTotalLiquido;
      FDetVeiculos          := ADetVeiculos;
      FDetMedicamentos      := ADetMedicamentos;
      FDetArmamentos        := ADetArmamentos;
      FDetCombustiveis      := ADetCombustiveis;
      FQuebraLinhaEmDetalhamentoEspecifico := ADQuebraLinhaEmDetalhamentoEspecifico;
      fFormato      := AdCasasDecimaisFormato;
      fMask_qCom   := AdCasasDecimais_Mask_qCom;
      fMask_vUnCom := AdCasasDecimais_Mask_vUnCom;
      fExibeCampoFatura := AdExibeCampoFatura;
      FTamanhoLogoHeigth := ATamanhoLogoHeight;
      FTamanhoLogoWidth := ATamanhoLogoWidth;
      FRecuoEndereco := ARecuoEndereco;
      FRecuoEmpresa := ARecuoEmpresa;
      FLogoemCima := ALogoemCima;
      FTamanhoFonteEndereco := ATamanhoFonteEndereco;
      FRecuoLogo := ARecuoLogo;
      fMostraDadosISSQN := AMostraDadosISSQN;
      fAltLinhaComun := AAltLinhaComun;
      fEspacoEntreProdutos := AEspacoEntreProdutos;
      fAlternaCoresProdutos := AAlternaCoresProdutos;
      fCorDestaqueProdutos := ACorDestaqueProdutos;
      fImprimirDadosDocReferenciados := AImprimirDadosDocReferenciados;

      if not EstaVazio(FImpressora) then
        RLPrinter.PrinterName := FImpressora;

      if FNumCopias > 0 then
        RLPrinter.Copies := FNumCopias
      else
        RLPrinter.Copies := 1;

      vAuxDiferencaPDF := 0;
      RLNFe.ShowProgress := FMostrarStatus;
      RLNFe.PrintDialog := not(FMostrarPreview) and (EstaVazio(FImpressora));
      RLNFe.CompositeOptions.ResetPageNumber := True;

      SetLength(AArray^,Length(AArray^) + 1);
      AArray^[Length(AArray^) - 1] := RLNFe.Owner;

      if AUltimaNFE then
      begin
        Report := TfrlDANFeRL(AArray^[0]).RLNFe;
        for i := 1 to High(AArray^) do
        begin
          if Report.NextReport = nil then
            Report.NextReport := TfrlDANFeRL(AArray^[i]).RLNFe
          else
          begin
            ReportNext := Report.NextReport;
            repeat
              if ReportNext.NextReport <> nil then
                ReportNext := ReportNext.NextReport;
            until ReportNext.NextReport = nil;
            ReportNext.NextReport := TfrlDANFeRL(AArray^[i]).RLNFe;
          end;
        end;
        if FMostrarPreview then
          Report.PreviewModal
        else
          Report.Print;
      end;
    finally
      if AUltimaNFE then
      begin
        for i := Low(AArray^) to High(AArray^) do
          AArray^[i].Free;
      end;
    end ;
end;

class procedure TfrlDANFeRL.SavePDF(AOwner: TComponent;
                ANFe: TNFe;
                ALogo: String;
                AMarcaDagua: String;
                ALarguraCodProd: Integer;
                AEmail: String;
                AResumoCanhoto: Boolean;
                AFax: String;
                ANumCopias: Integer;
                ASistema: String;
                ASite: String;
                AUsuario: String;
                AFile: String;
                APosCanhoto: TPosRecibo;
                AFormularioContinuo: Boolean;
                AExpandirLogoMarca: Boolean;
                AMostrarStatus: Boolean;
                ANomeFonte: TNomeFonte;
                ANegrito: Boolean;
                AMargemSuperior: Double;
                AMargemInferior: Double;
                AMargemEsquerda: Double;
                AMargemDireita: Double;
                ACasasDecimaisqCom: Integer;
                ACasasDecimaisvUncCom: Integer;
                AProdutosPorPagina: Integer;
                AImpressora: String;
                ATamanhoFonte_RazaoSocial: Integer;
                AExibirEAN: Boolean;
                AProtocoloNFe: String;
                AResumoCanhoto_Texto: String;
                ANFECancelada: Boolean;
                AImprimirDetalhamentoEspecifico: Boolean;
                AImprimirDescPorc: Boolean;
                AImprimeNomeFantasia: Boolean;
                AImprimirTotalLiquido: Boolean;
                ADetVeiculos: TDetVeiculos;
                ADetMedicamentos: TDetMedicamentos;
                ADetArmamentos: TDetArmamentos;
                ADetCombustiveis: TDetCombustiveis;
                ADQuebraLinhaEmDetalhamentoEspecifico: Boolean;
                AdCasasDecimaisFormato: Integer;
                AdCasasDecimais_Mask_qCom: String;
                AdCasasDecimais_Mask_vUnCom: String;
                AdExibeCampoFatura: Boolean;
                AMostraDadosISSQN: Boolean;
                AAltLinhaComun: Integer;
                AEspacoEntreProdutos: Integer;
                AAlternaCoresProdutos: Boolean;
                ACorDestaqueProdutos: TColor;
                AImprimirDadosDocReferenciados: Boolean;
                ATamanhoLogoHeight: Integer;
                ATamanhoLogoWidth: Integer;
                ARecuoEndereco: Integer;
                ARecuoEmpresa: Integer;
                ALogoemCima: Boolean;
                ATamanhoFonteEndereco: Integer;
                ARecuoLogo: Integer);

var
  ADir: String;
begin
  with Create ( AOwner ) do
    try
      FNFe := ANFe;
      FLogo := ALogo;
      FMarcaDagua := AMarcaDagua;
      FLarguraCodProd := ALarguraCodProd;
      FEmail := AEmail;
      FResumoCanhoto := AResumoCanhoto;
      FFax := AFax;
      FNumCopias := ANumCopias;
      FSistema := ASistema;
      FSite := ASite;
      FUsuario := AUsuario;
      FPosCanhoto := APosCanhoto;
      FFormularioContinuo := AFormularioContinuo;
      FExpandirLogoMarca := AExpandirLogoMarca;
      FMostrarStatus := AMostrarStatus;
      FNomeFonte := ANomeFonte;
      FNegrito := ANegrito;
      FMargemSuperior := AMargemSuperior;
      FMargemInferior := AMargemInferior;
      FMargemEsquerda := AMargemEsquerda;
      FMargemDireita := AMargemDireita;
      FCasasDecimaisqCom := ACasasDecimaisqCom;
      FCasasDecimaisvUnCom := ACasasDecimaisvUncCom;
      FProdutosPorPagina := AProdutosPorPagina;
      FImpressora := AImpressora;
      FTamanhoFonte_RazaoSocial := ATamanhoFonte_RazaoSocial;
      FExibirEAN := AExibirEAN;
      FProtocoloNFe := AProtocoloNFe;
      FResumoCanhoto_Texto := AResumoCanhoto_Texto;
      FNFeCancelada := ANFeCancelada;
      FImprimirDetalhamentoEspecifico := AImprimirDetalhamentoEspecifico;
      FImprimirDescPorc := AImprimirDescPorc;
      fImprimeNomeFantasia  := AImprimeNomeFantasia;
      fImprimirTotalLiquido := AImprimirTotalLiquido;
      FDetVeiculos := ADetVeiculos;
      FDetMedicamentos := ADetMedicamentos;
      FDetArmamentos := ADetArmamentos;
      FDetCombustiveis := ADetCombustiveis;
      FQuebraLinhaEmDetalhamentoEspecifico := ADQuebraLinhaEmDetalhamentoEspecifico;
      fFormato := AdCasasDecimaisFormato;
      fMask_qCom := AdCasasDecimais_Mask_qCom;
      fMask_vUnCom := AdCasasDecimais_Mask_vUnCom;
      fExibeCampoFatura := AdExibeCampoFatura;
      FTamanhoLogoHeigth := ATamanhoLogoHeight;
      FTamanhoLogoWidth := ATamanhoLogoWidth;
      FRecuoEndereco := ARecuoEndereco;
      FRecuoEmpresa := ARecuoEmpresa;
      FLogoemCima := ALogoemCima;
      FTamanhoFonteEndereco := ATamanhoFonteEndereco;
      FRecuoLogo := ARecuoLogo;
      fMostraDadosISSQN := AMostraDadosISSQN;
      fAltLinhaComun := AAltLinhaComun;
      fEspacoEntreProdutos := AEspacoEntreProdutos;
      fAlternaCoresProdutos := AAlternaCoresProdutos;
      fCorDestaqueProdutos := ACorDestaqueProdutos;
      FImprimirDadosDocReferenciados := AImprimirDadosDocReferenciados;

      if FImpressora > '' then
        RLPrinter.PrinterName := FImpressora;

      if Trim(AFile) = '' then
        raise EACBrNFeException.Create('Erro ao gerar PDF. Arquivo não informado');

      ADir := ExtractFilePath(AFile);
      if EstaVazio(ADir) then
        AFile := ApplicationPath + ExtractFileName(AFile)
      else
      begin
        if not DirectoryExists(ADir) then
          ForceDirectories(ADir);

        if not DirectoryExists(ADir) then
          raise EACBrNFeException.Create('Erro ao gerar PDF. Diretório: '+ADir+' não pode ser criado');
      end;

      with RLPDFFilter1.DocumentInfo do
      begin
        Title := ACBrStr('DANFE - Nota fiscal nº ' +
                                    FormatFloat('000,000,000', FNFe.Ide.nNF));
        KeyWords := ACBrStr('Número:' + FormatFloat('000,000,000', FNFe.Ide.nNF) +
                    '; Data de emissão: ' + FormatDateTime('dd/mm/yyyy', FNFe.Ide.dEmi) +
                    '; Destinatário: ' + FNFe.Dest.xNome +
                    '; CNPJ: ' + FNFe.Dest.CNPJCPF +
                    '; Valor total: ' + FormatFloat('###,###,###,###,##0.00', FNFe.Total.ICMSTot.vNF));
      end;
      vAuxDiferencaPDF := 10;
      RLPDFFilter1.ShowProgress := FMostrarStatus;
      RLPDFFilter1.FileName := AFile;
      RLNFe.ShowProgress := FMostrarStatus;
      RLNFe.Prepare;
      RLPDFFilter1.FilterPages(RLNFe.Pages);

    finally
     Destroy;
    end ;
end;

procedure TfrlDANFeRL.FormCreate(Sender: TObject);
begin
  {$IfNDef FPC}
   Self.Scaled := false;
  {$EndIf}
end;

procedure TfrlDANFeRL.ConfigureVariavies(ATipoDANFE: TpcnTipoImpressao);
begin
   case ATipoDANFE of
   tiRetrato    :  begin
                    iLimiteLinhas                := 10;
                    iLimiteCaracteresLinha       := 81;
                    iLimiteCaracteresContinuacao := 129;
                   end;
   tiPaisagem   :  begin
                    iLimiteLinhas                := 12;
                    iLimiteCaracteresLinha       := 142;
                    iLimiteCaracteresContinuacao := 204;
                   end;
  end;

end;

function TfrlDANFeRL.BuscaDireita(Busca, Text: String): Integer;
{Pesquisa um caractere à direita da string, retornando sua posição}
var n, retorno: integer;
begin
  retorno := 0;
    for n := length(Text) downto 1 do
      begin
        if Copy(Text, n, 1) = Busca then
          begin
            retorno := n;
            break;
         end;
      end;
  Result := retorno;
end;

procedure TfrlDANFeRL.InsereLinhas(sTexto: String; iLimCaracteres: Integer;
                                                                 rMemo: TRLMemo);
var iTotalLinhas, iUltimoEspacoLinha, iPosAtual, iQuantCaracteres, i: Integer;
    sLinhaProvisoria, sLinha: String;
begin
  iPosAtual := 1;
  iQuantCaracteres := Length(sTexto);
  if iQuantCaracteres <= iLimiteLinhas then
    iTotalLinhas := 1
  else
  begin
    if (iQuantCaracteres mod iLimCaracteres) > 0 then
      iTotalLinhas := (iQuantCaracteres div iLimCaracteres) + 1
    else
      iTotalLinhas := iQuantCaracteres div iLimCaracteres;
  end;

  //
  // Define o numero de linhas em complemento
  // iTotalLinhas + 20 = 30 linhas
  //

  for i := 1 to (iTotalLinhas + 20) do
  begin
    sLinhaProvisoria    := Copy(sTexto, iPosAtual, iLimCaracteres);
    iUltimoEspacoLinha  := BuscaDireita(' ', sLinhaProvisoria);

    if iUltimoEspacoLinha = 0 then
        iUltimoEspacoLinha := iQuantCaracteres;

    if Pos(';', sLinhaProvisoria) = 0 then
    begin
      if (BuscaDireita(' ', sLinhaProvisoria) = iLimCaracteres)  or
         (BuscaDireita(' ', sLinhaProvisoria) = (iLimCaracteres + 1)) then
        sLinha := sLinhaProvisoria
      else
      begin
        if (iQuantCaracteres - iPosAtual) > iLimCaracteres then
          sLinha := Copy(sLinhaProvisoria, 1, iUltimoEspacoLinha)
        else
          sLinha := sLinhaProvisoria;
      end;
      iPosAtual := iPosAtual + Length(sLinha);
    end // if Pos(';', sLinhaProvisoria) = 0
    else
    begin
      sLinha := Copy(sLinhaProvisoria, 1, Pos(';', sLinhaProvisoria));
      iPosAtual := iPosAtual + (Length(sLinha));
    end;

    if sLinha > '' then
    begin
      if LeftStr(sLinha, 1) = ' ' then
        sLinha := Copy(sLinha, 2, (Length(sLinha) - 1))
      else
        sLinha := sLinha;

      rMemo.Lines.Add(sLinha);
    end;
  end;
end;

function TfrlDANFeRL.QuebraLinha: String;
begin
  if fQuebraLinhaEmDetalhamentoEspecifico then
    Result := #13#10
  else
    Result := ' - ';
end;

function TfrlDANFeRL.ManterDesPro(dvDesc, dvProd: Double): Double;
begin
  if ( fImprimirDescPorc ) then
  begin
    if (dvDesc > 0) and ( dvProd > 0 ) then
      Result := (dvDesc * 100) / dvProd
    else
      Result := 0;
  end
  else
    Result := dvDesc;
end;


function TfrlDANFeRL.TrataDocumento(sCNPJCPF: String): String;
begin
  Result := sCNPJCPF;
  if NaoEstaVazio( Result ) then
  begin
    if Length( Result ) = 14 then
      Result := ' CNPJ: '
    else
      Result := ' CPF: ';

    Result := Result + FormatarCNPJouCPF( sCNPJCPF );
  end;
end;

function TfrlDANFeRL.ManterInfAdFisco : String;
// Informações de interesse do fisco
begin
  Result := '';
  if FNFe.InfAdic.infAdFisco > '' then
  begin
    if FNFe.InfAdic.infCpl > '' then
      Result := FNFe.InfAdic.infAdFisco + '; '
    else
      Result := FNFe.InfAdic.infAdFisco;
  end
end;

function TfrlDANFeRL.ManterInfCompl : String;
 // Informações de interesse do contribuinte
begin
  Result := '';
  if FNFe.InfAdic.infCpl > '' then
    Result := FNFe.InfAdic.infCpl
end;


function TfrlDANFeRL.ManterInfContr : String;
 // Informações de uso livre do contribuinte com "xCampo" e "xTexto"
var
  i : Integer;
begin
  Result := '';
  with FNFe.InfAdic do
  begin
    if obsCont.Count > 0 then
    begin
      for i := 0 to ( obsCont.Count - 1) do
      begin
        Result := Result + obsCont.Items[i].xCampo + ': ' + obsCont.Items[i].xTexto+
                  ifthen( ( obsCont.Items[i].Index = ( obsCont.Count - 1) ) ,'', ';');
      end;
      Result := Result + '; ';
    end;
  end;
end;

function TfrlDANFeRL.ManterObsFisco : String;
 // Informações de uso livre do fisco com "xCampo" e "xTexto"
var
  i : Integer;
begin
  Result := '';
  with FNFe.InfAdic do
  begin
    if obsFisco.Count > 0 then
    begin
      for i := 0 to ( obsFisco.Count - 1 ) do
      begin
        Result := Result + obsFisco.Items[i].xCampo +': ' + obsFisco.Items[i].xTexto +
                  ifthen( ( obsFisco.Items[i].Index = (obsFisco.Count - 1) ) ,'', ';');
      end;
      Result := Result + '; ';
    end;
  end;
end;

function TfrlDANFeRL.ManterProcreferenciado : String;
 // Informações do processo referenciado
var
  i : Integer;
begin
  Result := '';
  with FNFe.InfAdic do
  begin
    if procRef.Count > 0 then
    begin
      for i := 0 to (procRef.Count - 1) do
      begin
        if procRef.Items[i].Index = (procRef.Count - 1) then
          Result := Result + ACBrStr( 'PROCESSO OU ATO CONCESSÓRIO Nº: ' +
                             procRef.Items[i].nProc +
                             ' - ORIGEM: ' +
                             indProcToDescrStr(procRef.Items[i].indProc ))+
                  ifthen( ( procRef.Items[i].Index = (procRef.Count - 1) ) ,'', ';');
      end;
      Result := Result + '; ';
    end;
  end;
end;

end.

