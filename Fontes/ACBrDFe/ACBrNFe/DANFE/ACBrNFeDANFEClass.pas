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

{*******************************************************************************
|* Historico
|*
|* 16/12/2008: Wemerson Souto
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrNFeDANFEClass;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(VisualCLX)}
     QForms,
  {$ELSEIF DEFINED(FMX)}
     FMX.Forms,
  {$ELSE}
     Forms,
  {$IFEND}
  pcnNFe, pcnConversao;

type

  TCasasDecimais = class(TComponent)
  private
    FqCom: Integer;
    FvUnCom: Integer;
    FMask_qCom: String;
    FMask_vUnCom: String;

    procedure Set_qCom(AValue: Integer);
    procedure Set_vUnCom(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property _qCom: Integer       read FQCom        write Set_qCom;
    property _vUnCom: Integer     read FvUnCom      write Set_vUnCom;
    property _Mask_qCom: String   read FMask_qCom   write FMask_qCom;
    property _Mask_vUnCom: String read FMask_vUnCom write FMask_vUnCom;
  end;

  { TACBrNFeDANFEClass }

  TACBrNFeDANFEClass = class( TComponent )
   private
    procedure SetNFE(const Value: TComponent);
    procedure ErroAbstract(NomeProcedure: String);
    function GetPathArquivos: String;
    procedure SetPathArquivos(const Value: String);
  protected
    FACBrNFe: TComponent;
    FLogo: String;
    FSistema: String;
    FUsuario: String;
    FPathArquivos: String;
    FImpressora: String;
    FImprimirTotalLiquido: Boolean;
    FMostrarPreview: Boolean;
    FMostrarStatus: Boolean;
    FTipoDANFE: TpcnTipoImpressao;
    FNumCopias: Integer;
    FExpandirLogoMarca: Boolean;
    FFax: String;
    FSite: String;
    FEmail: String;
    FImprimeDescPorc: Boolean;
    FProtocoloNFe: String;
    FMargemInferior: Double;
    FMargemSuperior: Double;
    FMargemEsquerda: Double;
    FMargemDireita: Double;
    FCasasDecimais: TCasasDecimais;
    FExibeResumoCanhoto: Boolean;
    FExibeResumoCanhoto_Texto: String;
    FFormularioContinuo: Boolean;
    FTamanhoFonte_DemaisCampos: Integer;
    FProdutosPorPagina: Integer;
    FImprimirDetalhamentoEspecifico: Boolean;
    FNFeCancelada: Boolean;
    FLocalImpCanhoto: Integer;
    // Incluido por Italo em 27/03/2014
    // Destinado exclusivamente ao DANFE da NFC-e
    FImprimeItens: Boolean;
    
    // Incluido por Edilson Alves de Oliveira em 10/10/2014
    // Destinado exclusivamente ao DANFE da NFC-e
    FViaConsumidor : Boolean;
    FvTroco: Currency;

    // Incluido por Leandro da Silva Alves em 17/04/2015
    FTributosSeparadamente : Boolean; //informação dos tributos separadamente
    FvTribFed: Currency; //total tributos federais
    FvTribEst: Currency; //total tributos estaduais
    FvTribMun: Currency; //total tributos municipais
    FFonteTributos: String;
    FChaveTributos: String;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ImprimirDANFE(NFE: TNFe = nil); virtual;
    procedure ImprimirDANFEResumido(NFE: TNFe = nil); virtual;
    procedure ImprimirDANFEPDF(NFE: TNFe = nil); virtual;
    procedure ImprimirDANFEResumidoPDF(NFE: TNFe = nil); virtual;
    procedure ImprimirEVENTO(NFE: TNFe = nil); virtual;
    procedure ImprimirEVENTOPDF(NFE: TNFe = nil); virtual;
    procedure ImprimirINUTILIZACAO(NFE: TNFe = nil); virtual;
    procedure ImprimirINUTILIZACAOPDF(NFE: TNFe = nil); virtual;
  published
    property ACBrNFe: TComponent                     read FACBrNFe                        write SetNFE;
    property Logo: String                            read FLogo                           write FLogo;
    property Sistema: String                         read FSistema                        write FSistema;
    property Usuario: String                         read FUsuario                        write FUsuario;
    property PathPDF: String                         read GetPathArquivos                 write SetPathArquivos;
    property Impressora: String                      read FImpressora                     write FImpressora;
    property MostrarPreview: Boolean                 read FMostrarPreview                 write FMostrarPreview;
    property MostrarStatus: Boolean                  read FMostrarStatus                  write FMostrarStatus;
    property TipoDANFE: TpcnTipoImpressao            read FTipoDANFE                      write FTipoDANFE;
    property NumCopias: Integer                      read FNumCopias                      write FNumCopias;
    property Fax: String                             read FFax                            write FFax;
    property Site: String                            read FSite                           write FSite;
    property Email: String                           read FEmail                          write FEmail;
    property ImprimirDescPorc: Boolean               read FImprimeDescPorc                write FImprimeDescPorc;
    property ImprimirTotalLiquido: Boolean           read FImprimirTotalLiquido           write FImprimirTotalLiquido;
    property ProtocoloNFe: String                    read FProtocoloNFe                   write FProtocoloNFe;
    property MargemInferior: Double                  read FMargemInferior                 write FMargemInferior;
    property MargemSuperior: Double                  read FMargemSuperior                 write FMargemSuperior;
    property MargemEsquerda: Double                  read FMargemEsquerda                 write FMargemEsquerda;
    property MargemDireita: Double                   read FMargemDireita                  write FMargemDireita;
    property CasasDecimais: TCasasDecimais           read FCasasDecimais;
    property ExibirResumoCanhoto: Boolean            read FExibeResumoCanhoto             write FExibeResumoCanhoto;
    property ExibirResumoCanhoto_Texto: String       read FExibeResumoCanhoto_Texto       write FExibeResumoCanhoto_Texto;
    property FormularioContinuo: Boolean             read FFormularioContinuo             write FFormularioContinuo;
    property ExpandirLogoMarca: Boolean              read FExpandirLogoMarca              write FExpandirLogoMarca default false;
    property TamanhoFonte_DemaisCampos: Integer      read FTamanhoFonte_DemaisCampos      write FTamanhoFonte_DemaisCampos;
    property ProdutosPorPagina: Integer              read FProdutosPorPagina              write FProdutosPorPagina;
    property ImprimirDetalhamentoEspecifico: Boolean read FImprimirDetalhamentoEspecifico write FImprimirDetalhamentoEspecifico;
    property NFeCancelada: Boolean                   read FNFeCancelada                   write FNFeCancelada;
    property LocalImpCanhoto: Integer                read FLocalImpCanhoto                write FLocalImpCanhoto;
    // Incluido por Italo em 27/03/2014
    // Destinado exclusivamente ao DANFE da NFC-e
    property ImprimeItens: Boolean                   read FImprimeItens                   write FImprimeItens;
    property vTroco: Currency                        read FvTroco                         write FvTroco;
    property ViaConsumidor : Boolean                 read FViaConsumidor                  write FViaConsumidor;

    // Incluido por Leandro da Silva Alves em 17/04/2015
    property TributosSeparadamente: Boolean          read FTributosSeparadamente          write FTributosSeparadamente;
    property vTribFed: Currency                      read FvTribFed                       write FvTribFed;
    property vTribEst: Currency                      read FvTribEst                       write FvTribEst;
    property vTribMun: Currency                      read FvTribMun                       write FvTribMun;
    property FonteTributos: String                   read FFonteTributos                  write FFonteTributos;
    property ChaveTributos: String                   read FChaveTributos                  write FChaveTributos;

  end;

implementation

uses
  ACBrNFe, ACBrUtil, ACBrDFeUtil;

//Casas Decimais
constructor TCasasDecimais.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  FQCom := 2;
  FvUnCom := 2;

end;

destructor TCasasDecimais.Destroy;
begin

  inherited Destroy;
end;

procedure TCasasDecimais.Set_qCom(AValue: Integer);
begin
  if ((AValue >= 0) and
      (AValue <= 4))  then
    FqCom := AValue
  else
    FqCom := 2;
end;

procedure TCasasDecimais.Set_vUnCom(AValue: Integer);
begin
  if ((AValue >= 0) and
      (AValue <= 10))  then
    FvUnCom := AValue
  else
    FvUnCom := 2;
end;

//DANFE CLASS
constructor TACBrNFeDANFEClass.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  FACBrNFe      := nil;
  FLogo         := '';
  FSistema      := '';
  FUsuario      := '';
  FPathArquivos := '';
  FImpressora   := '';
  FImprimirTotalLiquido := False;
  FMostrarPreview       := True;
  FMostrarStatus        := True;
  FNumCopias := 1;
  FFax       := '';
  FSite      := '';
  FEmail     := '';
  FImprimeDescPorc := False;
  FProtocoloNFe    := '';
  FMargemInferior  := 0.8;
  FMargemSuperior  := 0.8;
  FMargemEsquerda  := 0.6;
  FMargemDireita   := 0.51;
  FExibeResumoCanhoto := false;
  FExibeResumoCanhoto_Texto := '';
  FFormularioContinuo := false;
  FTamanhoFonte_DemaisCampos := 10;
  FProdutosPorPagina := 0;
  FImprimirDetalhamentoEspecifico := true;
  FImprimirTotalLiquido:= True;
  FNFeCancelada := False;
  FLocalImpCanhoto := 0;
  FCasasDecimais := TCasasDecimais.Create(self);
  FCasasDecimais.Name:= 'CasasDecimais';

  FImprimeItens := True;
  FViaConsumidor:= True;
  FvTroco       := 0.0;

  FTributosSeparadamente:= False;
  FvTribFed:= 0.0;
  FvTribEst:= 0.0;
  FvTribMun:= 0.0;

  {$IFDEF COMPILER6_UP}
      FCasasDecimais.SetSubComponent( true );{ para gravar no DFM/XFM }
  {$ENDIF}
end;

destructor TACBrNFeDANFEClass.Destroy;
begin

  inherited Destroy;
end;

procedure TACBrNFeDANFEClass.ImprimirDANFE(NFE : TNFe = nil);
begin
  ErroAbstract('Imprimir');
end;

procedure TACBrNFeDANFEClass.ImprimirDANFEResumido(NFE : TNFe = nil);
begin
  ErroAbstract('ImprimirResumido');
end;

procedure TACBrNFeDANFEClass.ImprimirDANFEPDF(NFE : TNFe = nil);
begin
  ErroAbstract('ImprimirPDF');
end;

procedure TACBrNFeDANFEClass.ImprimirDANFEResumidoPDF(NFE: TNFe);
begin
  ErroAbstract('ImprimirDANFEResumidoPDF');
end;

procedure TACBrNFeDANFEClass.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FACBrNFe <> nil) and (AComponent is TACBrNFe) then
     FACBrNFe := nil;
end;

procedure TACBrNFeDANFEClass.SetNFE(const Value: TComponent);
  Var OldValue : TACBrNFe;
begin
  if Value <> FACBrNFe then
  begin
     if Value <> nil then
        if not (Value is TACBrNFe) then
           raise EACBrNFeException.Create('ACBrDANFE.NFE deve ser do tipo TACBrNFe');

     if Assigned(FACBrNFe) then
        FACBrNFe.RemoveFreeNotification(Self);

     OldValue := TACBrNFe(FACBrNFe);   // Usa outra variavel para evitar Loop Infinito
     FACBrNFe := Value;                 // na remoção da associação dos componentes

     if Assigned(OldValue) then
        if Assigned(OldValue.DANFE) then
           OldValue.DANFE := nil;

     if Value <> nil then
     begin
        Value.FreeNotification(self);
        TACBrNFe(Value).DANFE := self;
     end;
  end;
end;

procedure TACBrNFeDANFEClass.ErroAbstract(NomeProcedure: String);
begin
  raise EACBrNFeException.Create( NomeProcedure );
end;

function TACBrNFeDANFEClass.GetPathArquivos: String;
begin
  if EstaVazio(FPathArquivos) then
     if Assigned(FACBrNFe) then
        FPathArquivos := TACBrNFe(FACBrNFe).Configuracoes.Arquivos.PathSalvar;

  if NaoEstaVazio(FPathArquivos) then
     if not DirectoryExists(FPathArquivos) then
        ForceDirectories(FPathArquivos);

  Result := PathWithDelim(FPathArquivos);
end;

procedure TACBrNFeDANFEClass.SetPathArquivos(const Value: String);
begin
  FPathArquivos := Value;
end;

procedure TACBrNFeDANFEClass.ImprimirEVENTO(NFE: TNFe);
begin
  ErroAbstract('Imprimir');
end;

procedure TACBrNFeDANFEClass.ImprimirEVENTOPDF(NFE: TNFe);
begin
  ErroAbstract('ImprimirPDF');
end;

procedure TACBrNFeDANFEClass.ImprimirINUTILIZACAO(NFE: TNFe);
begin
  ErroAbstract('Imprimir');
end;

procedure TACBrNFeDANFEClass.ImprimirINUTILIZACAOPDF(NFE: TNFe);
begin
  ErroAbstract('ImprimirPDF');
end;

end.
