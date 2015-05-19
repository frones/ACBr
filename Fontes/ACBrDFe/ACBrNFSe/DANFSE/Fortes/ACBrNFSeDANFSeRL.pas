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
    {$IFDEF MSWINDOWS}Windows, Messages, {$ENDIF}
      Graphics, Controls, Forms, Dialogs, ExtCtrls,
  {$ENDIF}
  MaskUtils, pnfsNFSe, ACBrNFSe, Printers,
  RLReport, RLFilters, RLPrinters, RLPDFFilter, RLConsts,
  {$IFDEF BORLAND} DBClient, {$ELSE} BufDataset, {$ENDIF} DB;
  
type

  { TfrlDANFSeRL }

  TfrlDANFSeRL = class(TForm)
    dsItens: TDatasource;
    RLNFSe: TRLReport;
    RLPDFFilter1: TRLPDFFilter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  protected
    //BarCode : TBarCode128c ;
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

	cdsItens:  {$IFDEF BORLAND} TClientDataSet {$ELSE} TBufDataset{$ENDIF};
	procedure ConfigDataSet;
	
    procedure frlSemValorFiscalPrint(sender: TObject; var Value: String);
    procedure SetBarCodeImage ( ACode : String ; RLImage : TRLImage ) ;
  public
    { Public declarations }
    class procedure Imprimir(ANFSe           : TNFSe;
                             ALogo           : String  = '';
                             AEmail          : String  = '';
                             AFax            : String  = '';
                             ANumCopias      : Integer = 1;
                             ASistema        : String  = '';
                             ASite           : String  = '';
                             AUsuario        : String  = '' ;
                             APreview        : Boolean = True;
                             AMargemSuperior : Double  = 0.8;
                             AMargemInferior : Double  = 0.8;
                             AMargemEsquerda : Double  = 0.6;
                             AMargemDireita  : Double  = 0.51;
                             AImpressora     : String  = '';
                             APrestLogo      : String  = '';
                             APrefeitura     : String  = '';
                             ARazaoSocial    : String  = '';
                             AEndereco       : String  = '';
                             AComplemento    : String  = '';
                             AFone           : String  = '';
                             AMunicipio      : String  = '';
                             AInscMunicipal  : String  = '';
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

                             // Augusto Fontana
                             APrintDialog    : Boolean = True);

    class procedure SavePDF(AFile           : String;
                            ANFSe           : TNFSe;
                            ALogo           : String  = '';
                            AEmail          : String  = '';
                            AFax            : String  = '';
                            ANumCopias      : Integer = 1;
                            ASistema        : String  = '';
                            ASite           : String  = '';
                            AUsuario        : String  = '';
                            AMargemSuperior : Double  = 0.8;
                            AMargemInferior : Double  = 0.8;
                            AMargemEsquerda : Double  = 0.6;
                            AMargemDireita  : Double  = 0.51;
                            APrestLogo      : String  = '';
                            APrefeitura     : String  = '';
                            ARazaoSocial    : String  = '';
                            AEndereco       : String  = '';
                            AComplemento    : String  = '';
                            AFone           : String  = '';
                            AMunicipio      : String  = '';
                            AInscMunicipal  : String  = '';
                            AEMail_Prestador : String  = '';
                            AUF              : String  = '';
                            AT_InscEstadual : String = '';
                            AT_InscMunicipal : String = '';
                            AOutrasInformacaoesImp : String = '';
                            AAtividade             : String = '';
                            AT_Fone                : String = '';
                            AT_Endereco            : String = '';
                            AT_Complemento         : String = '';
                            AT_Email               : String = '');
  end;

var
  frlDANFSeRL: TfrlDANFSeRL;

implementation

{$R *.dfm}

procedure TfrlDANFSeRL.FormCreate(Sender: TObject);
begin
 ConfigDataSet;
end;

procedure TfrlDANFSeRL.FormDestroy(Sender: TObject);
begin
 FreeAndNil( cdsItens );
end;

procedure TfrlDANFSeRL.ConfigDataSet;
begin
 if not Assigned( cdsItens ) then
 cdsItens:=  {$IFDEF BORLAND}  TClientDataSet.create(nil)  {$ELSE}  TBufDataset.create(nil) {$ENDIF};

  if cdsItens.Active then
 begin
 {$IFDEF BORLAND}
  if cdsItens is TClientDataSet then
  TClientDataSet(cdsItens).EmptyDataSet;
 {$ENDIF}
  cdsItens.Active := False;
 end;

 {$IFDEF BORLAND}
 if cdsItens is TClientDataSet then
  begin
  TClientDataSet(cdsItens).StoreDefs := False;
  TClientDataSet(cdsItens).IndexDefs.Clear;
  TClientDataSet(cdsItens).IndexFieldNames := '';
  TClientDataSet(cdsItens).IndexName := '';
  TClientDataSet(cdsItens).Aggregates.Clear;
  TClientDataSet(cdsItens).AggFields.Clear;
  end;
 {$ELSE}
 if cdsItens is TBufDataset then
  begin
  TBufDataset(cdsItens).IndexDefs.Clear;
  TBufDataset(cdsItens).IndexFieldNames:='';
  TBufDataset(cdsItens).IndexName:='';
  end;
 {$ENDIF}

 with cdsItens do
  if FieldCount = 0 then
  begin
    FieldDefs.Clear;
    Fields.Clear;
    FieldDefs.Add('DISCRIMINACAO',ftString,60);

   {$IFDEF BORLAND}
    if cdsItens is TClientDataSet then
    TClientDataSet(cdsItens).CreateDataSet;
   {$ELSE}
    if cdsItens is TBufDataset then
    TBufDataset(cdsItens).CreateDataSet;
   {$ENDIF}
   end;

 {$IFDEF BORLAND}
  if cdsItens is TClientDataSet then
  TClientDataSet(cdsItens).StoreDefs := False;
 {$ENDIF}

   if not cdsItens.Active then
   cdsItens.Active := True;

  {$IFDEF BORLAND}
   if cdsItens is TClientDataSet then
   if cdsItens.Active then
   TClientDataSet(cdsItens).LogChanges := False;
 {$ENDIF}

 dsItens.dataset := cdsItens;

end;

procedure TfrlDANFSeRL.frlSemValorFiscalPrint(sender: TObject;
  var Value: String);
begin
 if FSemValorFiscal
  then Value := '';
end;

class procedure TfrlDANFSeRL.Imprimir(ANFSe: TNFSe; ALogo, AEmail, AFax: String;
  ANumCopias: Integer; ASistema, ASite, AUsuario: String; APreview: Boolean;
  AMargemSuperior, AMargemInferior, AMargemEsquerda, AMargemDireita: Double;
  AImpressora, APrestLogo, APrefeitura, ARazaoSocial, AEndereco,
  AComplemento, AFone, AMunicipio, AInscMunicipal, AEMail_Prestador, AUF,
  AT_InscEstadual, AT_InscMunicipal, AOutrasInformacaoesImp, AAtividade, AT_Fone,
  AT_Endereco, AT_Complemento, AT_Email : String; APrintDialog: Boolean);
begin
 with Create ( nil ) do
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

   if FImpressora > '' then
     RLPrinter.PrinterName := FImpressora;

   if FNumCopias > 0 then
     RLPrinter.Copies := FNumCopias
   else
     RLPrinter.Copies := 1;

   // Augusto Fontana
   RLNFSe.PrintDialog := APrintDialog;
   if APreview = True then
     RLNFSe.PreviewModal
   else
     RLNFSe.Print;

  finally
   Free ;
  end ;
end;

class procedure TfrlDANFSeRL.SavePDF(AFile: String; ANFSe: TNFSe; ALogo, AEmail,
  AFax: String; ANumCopias: Integer; ASistema, ASite, AUsuario: String;
  AMargemSuperior, AMargemInferior, AMargemEsquerda, AMargemDireita: Double;
  APrestLogo, APrefeitura, ARazaoSocial, AEndereco, AComplemento, AFone, AMunicipio,
  AInscMunicipal, AEMail_Prestador, AUF, AT_InscEstadual, AT_InscMunicipal,
  AOutrasInformacaoesImp, AAtividade, AT_Fone,
  AT_Endereco, AT_Complemento, AT_Email : String);
begin
  with Create ( nil ) do
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
    FInscMunicipal  := AInscMunicipal;
    FEMail_Prestador := AEMail_Prestador;
    FT_InscEstadual        := AT_InscEstadual;
    FT_InscMunicipal       := AT_InscMunicipal;
    FAtividade             := AAtividade;
    FT_Fone                := AT_Fone;

    FT_Endereco            := AT_Endereco;
    FT_Complemento         := AT_Complemento;
    FT_Email               := AT_Email;

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

procedure TfrlDANFSeRL.SetBarCodeImage(ACode: String; RLImage : TRLImage);
begin

end;

// Descomentar este comando quando aparecer a mensagem do Fortes sobre
// versão diferente

{initialization
RLConsts.SetVersion(3,71,'B');}

end.
