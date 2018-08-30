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
|* 10/10/2013: Juliomar Marchetti
|*  - Compatibilização para Lazarus da DANFSe em Fortes Report
******************************************************************************}
{$I ACBr.inc}

unit ACBrNFSeDANFSeRL;

interface

uses
  SysUtils, Variants, Classes, StrUtils,
  {$IFDEF CLX}
   QGraphics, QControls, QForms, QDialogs, QExtCtrls, Qt,
  {$ELSE}
   Graphics, Controls, Forms, Dialogs, ExtCtrls,
  {$ENDIF}
  pnfsNFSe, ACBrNFSe, Printers,
  RLReport, RLFilters, RLPrinters, RLPDFFilter, RLConsts,
  {$IFDEF BORLAND} DBClient, {$ELSE} BufDataset, {$ENDIF} DB;
  
type

  { TfrlDANFSeRL }

  TfrlDANFSeRL = class(TForm)
    RLNFSe: TRLReport;
    RLPDFFilter1: TRLPDFFilter;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RLNFSeNeedData(Sender: TObject; var MoreData: Boolean);
  private
    { Private declarations }
    FMoreData: Boolean;
  protected    
    FACBrNFSe       : TACBrNFSe;
    FNFSe           : TNFSe;
    FLogo           : String;
    FEmail          : String;
    FFax            : String;
    FNumCopias      : Integer;
    FSistema        : String;
    FSite           : String;
    FUsuario        : String;
    AfterPreview    : Boolean;
    ChangedPos      : Boolean;
    FSemValorFiscal : Boolean;
    FMargemSuperior : double;
    FMargemInferior : double;
    FMargemEsquerda : double;
    FMargemDireita  : double;
    FImpressora     : String;
    FPrestLogo      : String;
    FPrefeitura     : String;
    FRazaoSocial    : String;
    FUF             : String;
    FEndereco       : String;
    FComplemento    : String;
    FFone           : String;
    FMunicipio      : String;
    FOutrasInformacaoesImp : String;
    FInscMunicipal  : String;
    FT_InscEstadual : String;
    FT_InscMunicipal : String;
    FEMail_Prestador : String;
    FAtividade       : String;
    FT_Fone          : String;
    FT_Endereco      : String;
    FT_Complemento   : String;
    FT_Email         : String;
    FImprimeCanhoto  : Boolean;
    FDetalharServico : Boolean;

		cdsItens:  {$IFDEF BORLAND} TClientDataSet {$ELSE} TBufDataset{$ENDIF};
	
    procedure frlSemValorFiscalPrint(sender: TObject; var Value: String);    
  public
    { Public declarations }
    class procedure Imprimir(AOwner: TComponent;
                             ANFSe                  : TNFSe;
                             ALogo                  : String  = '';
                             AEmail                 : String  = '';
                             AFax                   : String  = '';
                             ANumCopias             : Integer = 1;
                             ASistema               : String  = '';
                             ASite                  : String  = '';
                             AUsuario               : String  = '' ;
                             APreview               : Boolean = True;
                             AMargemSuperior        : Double  = 0.8;
                             AMargemInferior        : Double  = 0.8;
                             AMargemEsquerda        : Double  = 0.6;
                             AMargemDireita         : Double  = 0.51;
                             AImpressora            : String  = '';
                             APrestLogo             : String  = '';
                             APrefeitura            : String  = '';
                             ARazaoSocial           : String  = '';
                             AEndereco              : String  = '';
                             AComplemento           : String  = '';
                             AFone                  : String  = '';
                             AMunicipio             : String  = '';
                             AInscMunicipal         : String  = '';
                             AEMail_Prestador       : String = '';
                             AUF                    : String = '';
                             AT_InscEstadual        : String = '';
                             AT_InscMunicipal       : String = '';
                             AOutrasInformacaoesImp : String = '';
                             AAtividade             : String = '';
                             AT_Fone                : String = '';
                             AT_Endereco            : String = '';
                             AT_Complemento         : String = '';
                             AT_Email               : String = '';
                             APrintDialog           : Boolean = True;
                             AImprimeCanhoto        : Boolean = True;
                             ADetalharServico       : Boolean = False);

    class procedure SavePDF(AOwner: TComponent;
                            AFile                  : String;
                            ANFSe                  : TNFSe;
                            ALogo                  : String  = '';
                            AEmail                 : String  = '';
                            AFax                   : String  = '';
                            ANumCopias             : Integer = 1;
                            ASistema               : String  = '';
                            ASite                  : String  = '';
                            AUsuario               : String  = '';
                            AMargemSuperior        : Double  = 0.8;
                            AMargemInferior        : Double  = 0.8;
                            AMargemEsquerda        : Double  = 0.6;
                            AMargemDireita         : Double  = 0.51;
                            APrestLogo             : String  = '';
                            APrefeitura            : String  = '';
                            ARazaoSocial           : String  = '';
                            AEndereco              : String  = '';
                            AComplemento           : String  = '';
                            AFone                  : String  = '';
                            AMunicipio             : String  = '';
                            AInscMunicipal         : String  = '';
                            AEMail_Prestador       : String  = '';
                            AUF                    : String  = '';
                            AT_InscEstadual        : String = '';
                            AT_InscMunicipal       : String = '';
                            AOutrasInformacaoesImp : String = '';
                            AAtividade             : String = '';
                            AT_Fone                : String = '';
                            AT_Endereco            : String = '';
                            AT_Complemento         : String = '';
                            AT_Email               : String = '';
                            AImprimeCanhoto        : Boolean = True;
                            ADetalharServico       : Boolean = False);
  end;

var
  frlDANFSeRL: TfrlDANFSeRL;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TfrlDANFSeRL.FormCreate(Sender: TObject);
begin
  FMoreData := True;
end;

procedure TfrlDANFSeRL.FormDestroy(Sender: TObject);
begin
  FreeAndNil( cdsItens );
end;

procedure TfrlDANFSeRL.frlSemValorFiscalPrint(sender: TObject;
  var Value: String);
begin
  if FSemValorFiscal then
    Value := '';
end;

class procedure TfrlDANFSeRL.Imprimir(AOwner: TComponent; ANFSe: TNFSe; ALogo, AEmail, AFax: String;
  ANumCopias: Integer; ASistema, ASite, AUsuario: String; APreview: Boolean;
  AMargemSuperior, AMargemInferior, AMargemEsquerda, AMargemDireita: Double;
  AImpressora, APrestLogo, APrefeitura, ARazaoSocial, AEndereco,
  AComplemento, AFone, AMunicipio, AInscMunicipal, AEMail_Prestador, AUF,
  AT_InscEstadual, AT_InscMunicipal, AOutrasInformacaoesImp, AAtividade, AT_Fone,
  AT_Endereco, AT_Complemento, AT_Email : String; APrintDialog, AImprimeCanhoto, ADetalharServico: Boolean);
begin
  with Create ( AOwner ) do
    try
      FNFSe                  := ANFSe;
      FLogo                  := ALogo;
      FEmail                 := AEmail;
      FFax                   := AFax;
      FNumCopias             := ANumCopias;
      FSistema               := ASistema;
      FSite                  := ASite;
      FUsuario               := AUsuario;
      FMargemSuperior        := AMargemSuperior;
      FMargemInferior        := AMargemInferior;
      FMargemEsquerda        := AMargemEsquerda;
      FMargemDireita         := AMargemDireita;
      FImpressora            := AImpressora;
      FPrestLogo             := APrestLogo;
      FPrefeitura            := APrefeitura;
      FRazaoSocial           := ARazaoSocial;
      FUF                    := AUF;
      FEndereco              := AEndereco;
      FComplemento           := AComplemento;
      FFone                  := AFone;
      FMunicipio             := AMunicipio;
      FOutrasInformacaoesImp := AOutrasInformacaoesImp;
      FInscMunicipal         := AInscMunicipal;
      FEMail_Prestador       := AEMail_Prestador;
      FT_InscEstadual        := AT_InscEstadual;
      FT_InscMunicipal       := AT_InscMunicipal;
      FAtividade             := AAtividade;
      FT_Fone                := AT_Fone;
      FT_Endereco            := AT_Endereco;
      FT_Complemento         := AT_Complemento;
      FT_Email               := AT_Email;
      FImprimeCanhoto        := AImprimeCanhoto;
      FDetalharServico       := ADetalharServico;

      if FImpressora > '' then
        RLPrinter.PrinterName := FImpressora;

      if FNumCopias > 0 then
        RLPrinter.Copies := FNumCopias
      else
        RLPrinter.Copies := 1;

      RLNFSe.PrintDialog := APrintDialog;
      if APreview = True then
        RLNFSe.PreviewModal
      else
        RLNFSe.Print;
    finally
      Free;
    end;
end;

procedure TfrlDANFSeRL.RLNFSeNeedData(Sender: TObject; var MoreData: Boolean);
begin
  MoreData := FMoreData;
  FMoreData := False;
end;

class procedure TfrlDANFSeRL.SavePDF(AOwner: TComponent; AFile: String; ANFSe: TNFSe; ALogo, AEmail,
  AFax: String; ANumCopias: Integer; ASistema, ASite, AUsuario: String;
  AMargemSuperior, AMargemInferior, AMargemEsquerda, AMargemDireita: Double;
  APrestLogo, APrefeitura, ARazaoSocial, AEndereco, AComplemento, AFone, AMunicipio,
  AInscMunicipal, AEMail_Prestador, AUF, AT_InscEstadual, AT_InscMunicipal,
  AOutrasInformacaoesImp, AAtividade, AT_Fone,
  AT_Endereco, AT_Complemento, AT_Email : String; AImprimeCanhoto, ADetalharServico: Boolean);
begin
  with Create ( AOwner ) do
    try
      FNFSe           := ANFSe;
      FLogo           := ALogo;
      FEmail          := AEmail;
      FFax            := AFax;
      FNumCopias      := ANumCopias;
      FSistema        := ASistema;
      FSite           := ASite;
      FUsuario        := AUsuario;
      FMargemSuperior := AMargemSuperior;
      FMargemInferior := AMargemInferior;
      FMargemEsquerda := AMargemEsquerda;
      FMargemDireita  := AMargemDireita;
      FPrestLogo      := APrestLogo;
      FPrefeitura     := APrefeitura;
      FRazaoSocial    := ARazaoSocial;
      FUF             := AUF;
      FEndereco       := AEndereco;
      FComplemento    := AComplemento;
      FFone           := AFone;
      FMunicipio      := AMunicipio;
      FOutrasInformacaoesImp := AOutrasInformacaoesImp;
      FInscMunicipal         := AInscMunicipal;
      FEMail_Prestador       := AEMail_Prestador;
      FT_InscEstadual        := AT_InscEstadual;
      FT_InscMunicipal       := AT_InscMunicipal;
      FAtividade             := AAtividade;
      FT_Fone                := AT_Fone;
      FT_Endereco            := AT_Endereco;
      FT_Complemento         := AT_Complemento;
      FT_Email               := AT_Email;
      FImprimeCanhoto := AImprimeCanhoto;
      FDetalharServico:= ADetalharServico;

      with RLPDFFilter1.DocumentInfo do
      begin
        Title := 'NFSe - ' + FNFSe.Numero;
        KeyWords := 'Número:' + FNFSe.Numero +
                    '; Data de emissão: ' + FormatDateTime('dd/mm/yyyy', FNFSe.DataEmissao) +
                    '; Tomador: ' + FNFSe.Tomador.RazaoSocial +
                    '; CNPJ: ' + FNFSe.Tomador.IdentificacaoTomador.CpfCnpj +
                    '; Valor total: ' + FormatFloat('###,###,###,###,##0.00', FNFse.Servico.Valores.ValorServicos);
      end;

      RLNFSe.SaveToFile(AFile);
    finally
     Free;
    end;
end;

end.
